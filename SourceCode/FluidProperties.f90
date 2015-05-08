MODULE FluidProperties

        ! MODULE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees  (May, June 2002)
        !                      Rick Strand (June 2004)
        !                      Linda Lawrie (March 2008)
        !       RE-ENGINEERED  Rick Strand (April 2000, May 2000)

        ! PURPOSE OF THIS MODULE:
        ! This module contains subroutines which determine and return properties
        ! of materials including enthalpy, quality, specific heat, and density.
        ! The module uses InputProcessor to read the material type and the
        ! associated charts from IN.IDF.  The module is only as powerful as the
        ! amount of data loaded into this file.

        ! METHODOLOGY EMPLOYED:
        ! The module will first check if the current refrigerant has been read
        ! in yet.  If not, it will get the data from IN.IDF and "store" it into
        ! a set of variables.  Any future iterations with that refrigerant will
        ! simply retrieve the data from storage instead of reading from the .IDF
        ! file again.  After the data is made available, the module uses input
        ! temperatures, pressures, and either quality or enthalpy to locate the
        ! state point and choose the proper routine.  Finally, it performs a
        ! double interpolation between temperatures and pressures or qualities
        ! which surround the point on a chart specified by the input conditions.
        ! The program is designed to work on either side of or under the vapor
        ! dome.  This data can be added as needed.
        !
        ! Where properties are invalid at particular pressure/temperature points
        ! in the data input file, zeros have to be inserted. This is necessary
        ! as the data structures are rectangular. The zero values are used to detect
        ! bounds of the data and issue appropriate warnings.
        !
        ! Properties of liquids (e.g. water) can be specified as glycol properties by
        ! supplying the same data for concentrations of 0.0 and 1.0 only.
        !
        ! Temperature data has to be supplied in ascending order only.

        ! REFERENCES:

        ! USE STATEMENTS
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, WarmupFlag, OutputFileDebug
USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowRecurringWarningErrorAtEnd, &
                       ShowRecurringSevereErrorAtEnd, ShowContinueError, ShowContinueErrorTimeStamp, ShowMessage

IMPLICIT NONE                           ! Enforce explicit typing of all variables
PRIVATE
        ! MODULE PARAMETER DEFINITIONS
CHARACTER(len=11), PARAMETER :: Refrig       = "REFRIGERANT"
CHARACTER(len=6),  PARAMETER :: Glycol       = "GLYCOL"
CHARACTER(len=8),  PARAMETER :: Pressure     = "PRESSURE"
CHARACTER(len=8),  PARAMETER :: Enthalpy     = "ENTHALPY"
CHARACTER(len=7),  PARAMETER :: Density      = "DENSITY"
CHARACTER(len=12), PARAMETER :: SpecificHeat = "SPECIFICHEAT"
CHARACTER(len=12), PARAMETER :: Conductivity = "CONDUCTIVITY"
CHARACTER(len=9),  PARAMETER :: Viscosity    = "VISCOSITY"
CHARACTER(len=5),  PARAMETER :: Fluid        = "FLUID"
CHARACTER(len=8),  PARAMETER :: GasFluid     = "FLUIDGAS"
CHARACTER(len=5),  PARAMETER :: Water           = 'Water'
CHARACTER(len=5),  PARAMETER :: Steam           = 'Steam'
CHARACTER(len=14), PARAMETER :: EthyleneGlycol  = 'EthyleneGlycol'
CHARACTER(len=15), PARAMETER :: PropyleneGlycol = 'PropyleneGlycol'
INTEGER,           PARAMETER :: EthyleneGlycolIndex  = -2
INTEGER,           PARAMETER :: PropyleneGlycolIndex = -1
INTEGER,           PARAMETER :: iRefrig      = 1
INTEGER,           PARAMETER :: iGlycol      = 1

        ! DERIVED TYPE DEFINITIONS
TYPE FluidPropsRefrigerantData
  CHARACTER(len=MaxNameLength) :: Name = ' '    ! Name of the refrigerant
  INTEGER   :: NumPsPoints       = 0              ! Number of saturation pressure
  REAL(r64) :: PsLowTempValue  = 0.0     ! Low Temperature Value for Ps (>0.0)
  REAL(r64) :: PsHighTempValue = 0.0     ! High Temperature Value for Ps (max in tables)
  INTEGER   :: PsLowTempIndex  = 0       ! Low Temperature Min Index for Ps (>0.0)
  INTEGER   :: PsHighTempIndex = 0       ! High Temperature Max Index for Ps (>0.0)
  REAL(r64) :: PsLowPresValue  = 0.0     ! Low Pressure Value for Ps (>0.0)
  REAL(r64) :: PsHighPresValue = 0.0     ! High Pressure Value for Ps (max in tables)
  INTEGER   :: PsLowPresIndex  = 0       ! Low Pressure Min Index for Ps (>0.0)
  INTEGER   :: PsHighPresIndex = 0       ! High Pressure Max Index for Ps (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: PsTemps       ! Temperatures for saturation pressures
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: PsValues      ! Saturation pressures at PsTemps
  INTEGER   :: NumHPoints        = 0              ! Number of enthalpy points
  REAL(r64) :: HfLowTempValue  = 0.0     ! Low Temperature Value for Hf (>0.0)
  REAL(r64) :: HfHighTempValue = 0.0     ! High Temperature Value for Hf (max in tables)
  INTEGER   :: HfLowTempIndex  = 0       ! Low Temperature Min Index for Hf (>0.0)
  INTEGER   :: HfHighTempIndex = 0       ! High Temperature Max Index for Hf (>0.0)
  REAL(r64) :: HfgLowTempValue  = 0.0    ! Low Temperature Value for Hfg (>0.0)
  REAL(r64) :: HfgHighTempValue = 0.0    ! High Temperature Value for Hfg (max in tables)
  INTEGER   :: HfgLowTempIndex  = 0      ! Low Temperature Min Index for Hfg (>0.0)
  INTEGER   :: HfgHighTempIndex = 0      ! High Temperature Max Index for Hfg (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HTemps        ! Temperatures for enthalpy points
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HfValues      ! Enthalpy of saturated fluid at HTemps
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HfgValues     ! Enthalpy of saturated fluid/gas at HTemps
  INTEGER   :: NumCpPoints       = 0              ! Number of specific heat of fluid points
  REAL(r64) :: CpfLowTempValue  = 0.0     ! Low Temperature Value for Cpf (>0.0)
  REAL(r64) :: CpfHighTempValue = 0.0     ! High Temperature Value for Cpf (max in tables)
  INTEGER   :: CpfLowTempIndex  = 0       ! Low Temperature Min Index for Cpf (>0.0)
  INTEGER   :: CpfHighTempIndex = 0       ! High Temperature Max Index for Cpf (>0.0)
  REAL(r64) :: CpfgLowTempValue  = 0.0    ! Low Temperature Value for Cpfg (>0.0)
  REAL(r64) :: CpfgHighTempValue = 0.0    ! High Temperature Value for Cpfg (max in tables)
  INTEGER   :: CpfgLowTempIndex  = 0      ! Low Temperature Min Index for Cpfg (>0.0)
  INTEGER   :: CpfgHighTempIndex = 0      ! High Temperature Max Index for Cpfg (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CpTemps       ! Temperatures for specific heat points
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CpfValues     ! Specific heat of saturated fluid at CpTemps
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CpfgValues    ! Specific heat of saturated fluid/gas at CpTemps
  INTEGER   :: NumRhoPoints      = 0              ! Number of density of fluid points
  REAL(r64) :: RhofLowTempValue  = 0.0     ! Low Temperature Value for Rhof (>0.0)
  REAL(r64) :: RhofHighTempValue = 0.0     ! High Temperature Value for Rhof (max in tables)
  INTEGER   :: RhofLowTempIndex  = 0       ! Low Temperature Min Index for Rhof (>0.0)
  INTEGER   :: RhofHighTempIndex = 0       ! High Temperature Max Index for Rhof (>0.0)
  REAL(r64) :: RhofgLowTempValue  = 0.0    ! Low Temperature Value for Rhofg (>0.0)
  REAL(r64) :: RhofgHighTempValue = 0.0    ! High Temperature Value for Rhofg (max in tables)
  INTEGER   :: RhofgLowTempIndex  = 0      ! Low Temperature Min Index for Rhofg (>0.0)
  INTEGER   :: RhofgHighTempIndex = 0      ! High Temperature Max Index for Rhofg (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: RhoTemps      ! Temperatures for density of fluid points
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: RhofValues    ! Density of saturated fluid at RhoTemps
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: RhofgValues   ! Density of saturated fluid/gas at RhoTemps
  INTEGER   :: NumSuperTempPts   = 0              ! Number of temperature points for superheated enthalpy
  INTEGER   :: NumSuperPressPts  = 0              ! Number of pressure points for superheated enthalpy
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SHTemps       ! Temperatures for superheated gas
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SHPress       ! Pressures for superheated gas
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: HshValues    ! Enthalpy of superheated gas at HshTemps, HshPress
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: RhoshValues  ! Density of superheated gas at HshTemps, HshPress
END TYPE

TYPE FluidPropsGlycolRawData
  CHARACTER(len=MaxNameLength)  :: Name = ' '  ! Name of the glycol
  LOGICAL :: CpDataPresent      = .FALSE.      ! Flag set when specific heat data is available
  INTEGER :: NumCpTempPts       = 0            ! Number of temperature points for specific heat
  INTEGER :: NumCpConcPts       = 0            ! Number of concentration points for specific heat
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CpTemps     ! Temperatures for specific heat of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CpConcs     ! Concentration for specific heat of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: CpValues    ! Specific heat data values
  LOGICAL :: RhoDataPresent     = .FALSE.      ! Flag set when density data is available
  INTEGER :: NumRhoTempPts      = 0            ! Number of temperature points for density
  INTEGER :: NumRhoConcPts      = 0            ! Number of concentration points for density
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: RhoTemps    ! Temperatures for density of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: RhoConcs    ! Concentration for density of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: RhoValues   ! Density data values
  LOGICAL :: CondDataPresent    = .FALSE.      ! Flag set when conductivity data is available
  INTEGER :: NumCondTempPts     = 0            ! Number of temperature points for conductivity
  INTEGER :: NumCondConcPts     = 0            ! Number of concentration points for conductivity
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CondTemps   ! Temperatures for conductivity of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CondConcs   ! Concentration for conductivity of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: CondValues  ! conductivity values
  LOGICAL :: ViscDataPresent    = .FALSE.      ! Flag set when viscosity data is available
  INTEGER :: NumViscTempPts     = 0            ! Number of temperature points for viscosity
  INTEGER :: NumViscConcPts     = 0            ! Number of concentration points for viscosity
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ViscTemps   ! Temperatures for viscosity of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ViscConcs   ! Concentration for viscosity of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: ViscValues  ! viscosity values
END TYPE

TYPE FluidPropsGlycolData
  CHARACTER(len=MaxNameLength) :: Name = ' '                ! Name of the glycol mixture (used by other parts of code)
  CHARACTER(len=MaxNameLength) :: GlycolName = ' '          ! Name of non-water fluid that is part of this mixture
                                                            ! (refers to ethylene glycol, propylene glycol, or user fluid)
  INTEGER                      :: GlycolIndex = 0           ! Index in user defined glycol data (>0 = index in raw data,
                                                            ! -1=propylene glycol, -2=ethylene glycol)
  REAL(r64)                    :: Concentration = 1.0       ! Concentration (if applicable)
  LOGICAL                      :: CpDataPresent = .FALSE.   ! Flag set when specific heat data is available
  REAL(r64)                    :: CpLowTempValue  = 0.0     ! Low Temperature Value for Cp (>0.0)
  REAL(r64)                    :: CpHighTempValue = 0.0     ! High Temperature Value for Cp (max in tables)
  INTEGER                      :: CpLowTempIndex  = 0       ! Low Temperature Min Index for Cp (>0.0)
  INTEGER                      :: CpHighTempIndex = 0       ! High Temperature Max Index for Cp (>0.0)
  INTEGER                      :: NumCpTempPts       = 0    ! Number of temperature points for specific heat
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CpTemps                   ! Temperatures for specific heat of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CpValues                  ! Specific heat data values (J/kg-K)
  LOGICAL                      :: RhoDataPresent = .FALSE.  ! Flag set when density data is available
  INTEGER                      :: NumRhoTempPts      = 0    ! Number of temperature points for density
  REAL(r64)                    :: RhoLowTempValue  = 0.0     ! Low Temperature Value for Rho (>0.0)
  REAL(r64)                    :: RhoHighTempValue = 0.0     ! High Temperature Value for Rho (max in tables)
  INTEGER                      :: RhoLowTempIndex  = 0       ! Low Temperature Min Index for Rho (>0.0)
  INTEGER                      :: RhoHighTempIndex = 0       ! High Temperature Max Index for Rho (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: RhoTemps                  ! Temperatures for density of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: RhoValues                 ! Density data values (kg/m3)
  LOGICAL                      :: CondDataPresent = .FALSE. ! Flag set when conductivity data is available
  INTEGER                      :: NumCondTempPts     = 0    ! Number of temperature points for conductivity
  REAL(r64)                    :: CondLowTempValue  = 0.0     ! Low Temperature Value for Cond (>0.0)
  REAL(r64)                    :: CondHighTempValue = 0.0     ! High Temperature Value for Cond (max in tables)
  INTEGER                      :: CondLowTempIndex  = 0       ! Low Temperature Min Index for Cond (>0.0)
  INTEGER                      :: CondHighTempIndex = 0       ! High Temperature Max Index for Cond (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CondTemps                 ! Temperatures for conductivity of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CondValues                ! conductivity values (W/m-K)
  LOGICAL                      :: ViscDataPresent = .FALSE. ! Flag set when viscosity data is available
  INTEGER                      :: NumViscTempPts     = 0    ! Number of temperature points for viscosity
  REAL(r64)                    :: ViscLowTempValue  = 0.0     ! Low Temperature Value for Visc (>0.0)
  REAL(r64)                    :: ViscHighTempValue = 0.0     ! High Temperature Value for Visc (max in tables)
  INTEGER                      :: ViscLowTempIndex  = 0       ! Low Temperature Min Index for Visc (>0.0)
  INTEGER                      :: ViscHighTempIndex = 0       ! High Temperature Max Index for Visc (>0.0)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: ViscTemps                 ! Temperatures for viscosity of glycol
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: ViscValues                ! viscosity values (mPa-s)
END TYPE

TYPE (FluidPropsRefrigerantData), ALLOCATABLE, DIMENSION(:) :: RefrigData
TYPE (FluidPropsGlycolRawData), ALLOCATABLE, DIMENSION(:)   :: GlyRawData
TYPE (FluidPropsGlycolData), ALLOCATABLE, DIMENSION(:)      :: GlycolData

        ! INTERFACE BLOCK SPECIFICATIONS
        ! na

        ! MODULE VARIABLE DECLARATIONS
LOGICAL :: GetInput = .TRUE.     ! Used to get the input once only
INTEGER :: NumOfRefrigerants = 0 ! Total number of refrigerants input by user
INTEGER :: NumOfGlycols = 0      ! Total number of glycols input by user
LOGICAL :: DebugReportGlycols=.false.
LOGICAL :: DebugReportRefrigerants=.false.
INTEGER :: GlycolErrorLimitTest = 1    ! how many times error is printed with details before recurring called
INTEGER :: RefrigerantErrorLimitTest = 1    ! how many times error is printed with details before recurring called
LOGICAL, ALLOCATABLE, DIMENSION(:) :: RefrigUsed
LOGICAL, ALLOCATABLE, DIMENSION(:) :: GlycolUsed
INTEGER,PUBLIC :: FluidIndex_Water = 0
INTEGER,PUBLIC :: FluidIndex_EthyleneGlycol = 0
INTEGER,PUBLIC :: FluidIndex_PropoleneGlycol = 0

        ! ACCESSIBLE SPECIFICATIONS OF MODULE SUBROUTINES OR FUNCTONS:
PRIVATE GetFluidPropertiesData
PRIVATE InterpDefValuesForGlycolConc
PRIVATE InterpValuesForGlycolConc
PRIVATE InitializeGlycolTempLimits
PRIVATE InitializeRefrigerantLimits
PRIVATE ReportAndTestGlycols
PRIVATE ReportAndTestRefrigerants
PUBLIC  GetSatPressureRefrig
PUBLIC  GetSatTemperatureRefrig
PUBLIC  GetSatEnthalpyRefrig
PUBLIC  GetSatDensityRefrig
PUBLIC  GetSatSpecificHeatRefrig
PUBLIC  GetSupHeatEnthalpyRefrig
PUBLIC  GetSupHeatPressureRefrig
PUBLIC  GetSupHeatDensityRefrig
PUBLIC  GetSpecificHeatGlycol
PUBLIC  GetConductivityGlycol
PUBLIC  GetDensityGlycol
PUBLIC  GetViscosityGlycol
PRIVATE GetInterpValue
PUBLIC  GetQualityRefrig
PUBLIC  CheckFluidPropertyName
PUBLIC  ReportOrphanFluids
PUBLIC  FindRefrigerant
PUBLIC  FindGlycol
PUBLIC  GetGlycolNameByIndex
PUBLIC  FindArrayIndex
PRIVATE GetInterpolatedSatProp

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE GetFluidPropertiesData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   April 2000
          !       MODIFIED       May 2002 Simon Rees (Added saturated pressure data retreaval)
          !                      June 2004 Rick Strand (Added glycol defaults and modified glycol data structure)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to read in all of the fluid
          ! property data contained in the user input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.  Derived type portions are
          ! allocated as necessary as the data is read into the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: TempToler = 0.1d0    ! Some reasonable value for comparisons
  REAL(r64), PARAMETER :: PressToler = 1.0d0   ! Some reasonable value for comparisons
  INTEGER, PARAMETER :: DefaultNumGlyTemps = 33 ! Temperature dimension of default glycol data
  INTEGER, PARAMETER :: DefaultNumGlyConcs = 10 ! Concentration dimension of default glycol data
  INTEGER, PARAMETER :: DefaultNumSteamTemps = 111 ! Temperature dimension of default steam data.
  INTEGER, PARAMETER :: DefaultNumSteamSuperheatedTemps = 114 ! Temperature dimension of default steam data.
  INTEGER, PARAMETER :: DefaultNumSteamSuperheatedPressure = 114 ! Temperature dimension of default steam data.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
  TYPE FluidTempData
    CHARACTER(len=MaxNameLength) :: Name =' '      ! Name of the temperature list
    INTEGER :: NumOfTemps                =0      ! Number of temperatures in a particular arry
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Temps ! Temperature values (degrees C)
  END TYPE

  TYPE PressureSequence
    REAL(r64) :: Pressure =0.0
    INTEGER   :: InPtr    =0
  END TYPE

  TYPE FluidData
    CHARACTER(Len=MaxNameLength) :: Name=' '
    LOGICAL ::                      IsGlycol=.false.
  END TYPE

  TYPE(FluidTempData), ALLOCATABLE, DIMENSION(:) :: FluidTemps
  TYPE(PressureSequence), ALLOCATABLE, DIMENSION(:) :: PressurePtr
  TYPE(FluidData), ALLOCATABLE, DIMENSION(:) :: FluidNames

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: Alphas ! Reads string value from input file
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames ! field names for alpha fields
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames ! field names for numeric fields
  INTEGER :: Loop                    ! DO loop counter (various uses)
  INTEGER :: NumAlphas               ! States which alpha value to read from a "Number" line
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers    ! brings in data from IP
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks    ! logical for blank alpha fields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks  ! logical for blank numeric fields
  INTEGER :: NumNumbers              ! States which number value to read from a "Numbers" line
  INTEGER :: MaxAlphas               ! maximum number of alphas
  INTEGER :: MaxNumbers              ! maximum number of numbers
  INTEGER :: Status                  ! Either 1 "object found" or -1 "not found" (also used as temp)
  INTEGER :: InData
  INTEGER :: TempLoop
  INTEGER :: NumOfFluidTempArrays
  INTEGER :: NumOfSatFluidPropArrays
  INTEGER :: NumOfSHFluidPropArrays
  INTEGER :: NumOfGlyFluidPropArrays
  CHARACTER(len=MaxNameLength) :: TempsName
  LOGICAL :: FirstSHMatch
  INTEGER :: NumOfPressPts
  INTEGER :: NumOfConcPts
  LOGICAL :: ErrorsFound=.false.
  INTEGER :: Index
  REAL(r64), DIMENSION(DefaultNumGlyTemps) :: DefaultGlycolTemps
  REAL(r64), DIMENSION(DefaultNumGlyConcs) :: DefaultGlycolConcs
  REAL(r64), DIMENSION(DefaultNumGlyTemps) :: DefaultWaterCpData
  REAL(r64), DIMENSION(DefaultNumGlyTemps) :: DefaultWaterViscData
  REAL(r64), DIMENSION(DefaultNumGlyTemps) :: DefaultWaterRhoData
  REAL(r64), DIMENSION(DefaultNumGlyTemps) :: DefaultWaterCondData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyCpData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyViscData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyRhoData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyCondData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyCpData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyViscData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyRhoData
  REAL(r64), DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyCondData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamTemps
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamPressData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamEnthalpyFluidData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamEnthalpyGasFluidData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamCpFluidData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamCpGasFluidData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamDensityFluidData
  REAL(r64), DIMENSION(DefaultNumSteamTemps) :: DefaultSteamDensityGasFluidData
  REAL(r64), DIMENSION(DefaultNumSteamSuperheatedTemps) :: DefaultSteamSuperheatedTemps
  REAL(r64), DIMENSION(DefaultNumSteamSuperheatedTemps) :: DefaultSteamSuperheatedPressData
  REAL(r64), DIMENSION(DefaultNumSteamSuperheatedTemps,DefaultNumSteamSuperheatedPressure) :: DefaultSteamSuperheatedEnthalpyData
  REAL(r64), DIMENSION(DefaultNumSteamSuperheatedTemps,DefaultNumSteamSuperheatedPressure) :: DefaultSteamSuperheatedDensityData

  INTEGER :: I
  INTEGER :: NumOfGlyConcs
  LOGICAL :: GlycolFound
  INTEGER :: NumOfOptionalInput
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
  REAL(r64) :: pTemp
  INTEGER :: iTemp
  INTEGER :: j
  LOGICAL ::  ErrorInName
  LOGICAL ::  IsBlank
  INTEGER :: FluidNum

          ! SUBROUTINE LOCAL DATA:
          ! For default "glycol" fluids of Water, Ethylene Glycol, and Propylene Glycol
  DATA DefaultGlycolTemps /-35.0d0,-30.0d0,-25.0d0,-20.0d0,-15.0d0,-10.0d0,-5.0d0,0.0d0,5.0d0,10.0d0,15.0d0,20.0d0,25.0d0,30.0d0, &
                            35.0d0,40.0d0,45.0d0,50.0d0,55.0d0,60.0d0,65.0d0,70.0d0,75.0d0,80.0d0,85.0d0, 90.0d0, 95.0d0,100.0d0, &
                            105.0d0,110.0d0,115.0d0,120.0d0,125.0d0/ ! 33 total temperature points
  DATA DefaultGlycolConcs /0.0d0,0.1d0,0.2d0,0.3d0,0.4d0,0.5d0,0.6d0,0.7d0,0.8d0,0.9d0/ ! 10 total concentration points

  DATA DefaultWaterCpData   /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,4217.0d0,4198.0d0,4191.0d0, &
                             4185.0d0,4181.0d0,4179.0d0,4180.0d0,4180.0d0,4180.0d0,4180.0d0,4181.0d0,4183.0d0,4185.0d0, &
                             4188.0d0,4192.0d0,4196.0d0,4200.0d0,4203.0d0,4208.0d0,4213.0d0,4218.0d0,4223.0d0,4228.0d0, &
                             4233.0d0,4238.0d0,4243.0d0/  ! in J/kg-K
  DATA DefaultWaterViscData /0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.7912d0, 1.5183d0, 1.306d0, &
                             1.1376d0, 1.0016d0, 0.8901d0, 0.7974d0, 0.7193d0, 0.653d0, 0.5961d0, 0.5468d0, 0.504d0, 0.4664d0, &
                             0.4332d0, 0.4039d0, 0.3777d0, 0.3543d0, 0.3333d0, 0.3144d0, 0.2973d0, 0.2817d0, 0.0d0, 0.0d0, &
                             0.0d0, 0.0d0, 0.0d0/ ! in mPa-s
  DATA DefaultWaterRhoData  /0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 999.8d0, 999.9d0, 999.7d0, &
                             999.1d0, 998.2d0, 997.0d0, 995.6d0, 994.0d0, 992.2d0, 990.2d0, 988.0d0, 985.7d0, 983.2d0, &
                             980.5d0, 977.7d0, 974.8d0, 971.8d0, 968.6d0, 965.3d0, 961.9d0, 958.3d0, 0.0d0, 0.0d0, &
                             0.0d0, 0.0d0, 0.0d0/ ! in kg/m3
  DATA DefaultWaterCondData /0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.561d0, 0.5705d0, 0.58d0, &
                             0.5893d0, 0.5984d0, 0.6072d0, 0.6155d0, 0.6233d0, 0.6306d0, 0.6373d0, 0.6436d0, 0.6492d0, 0.6543d0, &
                             0.659d0, 0.6631d0, 0.6668d0, 0.67d0, 0.6728d0, 0.6753d0, 0.6773d0, 0.6791d0, 0.0d0, 0.0d0, &
                             0.0d0, 0.0d0, 0.0d0/ ! in W/mK

          ! Ethylene Glycol Data: Specific Heat in J/(kg-k)
  DATA (DefaultEthGlyCpData(2,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3937.0d0,3946.0d0,3954.0d0, &
                            3963.0d0,3972.0d0,3981.0d0,3989.0d0,3998.0d0,4007.0d0,4015.0d0,4024.0d0,4033.0d0,4042.0d0, &
                            4050.0d0,4059.0d0,4068.0d0,4077.0d0,4085.0d0,4094.0d0,4103.0d0,4112.0d0,4120.0d0,4129.0d0, &
                            4138.0d0,4147.0d0,4155.0d0/ ! Conc=0.1
  DATA (DefaultEthGlyCpData(3,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3757.0d0,3769.0d0,3780.0d0,3792.0d0, &
                            3803.0d0,3815.0d0,3826.0d0,3838.0d0,3849.0d0,3861.0d0,3872.0d0,3884.0d0,3895.0d0,3907.0d0, &
                            3918.0d0,3930.0d0,3941.0d0,3953.0d0,3964.0d0,3976.0d0,3987.0d0,3999.0d0,4010.0d0,4022.0d0, &
                            4033.0d0,4045.0d0,4056.0d0/ ! Conc=0.2
  DATA (DefaultEthGlyCpData(4,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3560.0d0,3574.0d0,3589.0d0,3603.0d0,3617.0d0, &
                            3631.0d0,3645.0d0,3660.0d0,3674.0d0,3688.0d0,3702.0d0,3716.0d0,3730.0d0,3745.0d0,3759.0d0, &
                            3773.0d0,3787.0d0,3801.0d0,3816.0d0,3830.0d0,3844.0d0,3858.0d0,3872.0d0,3886.0d0,3901.0d0, &
                            3915.0d0,3929.0d0,3943.0d0/ ! Conc=0.3
  DATA (DefaultEthGlyCpData(5,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,3334.0d0,3351.0d0,3367.0d0,3384.0d0,3401.0d0,3418.0d0,3435.0d0, &
                            3451.0d0,3468.0d0,3485.0d0,3502.0d0,3518.0d0,3535.0d0,3552.0d0,3569.0d0,3585.0d0,3602.0d0, &
                            3619.0d0,3636.0d0,3653.0d0,3669.0d0,3686.0d0,3703.0d0,3720.0d0,3736.0d0,3753.0d0,3770.0d0, &
                            3787.0d0,3804.0d0,3820.0d0/ ! Conc=0.4
  DATA (DefaultEthGlyCpData(6,I),I=1,DefaultNumGlyTemps) &
                           /3068.0d0,3088.0d0,3107.0d0,3126.0d0,3145.0d0,3165.0d0,3184.0d0,3203.0d0,3223.0d0,3242.0d0, &
                            3261.0d0,3281.0d0,3300.0d0,3319.0d0,3339.0d0,3358.0d0,3377.0d0,3396.0d0,3416.0d0,3435.0d0, &
                            3454.0d0,3474.0d0,3493.0d0,3512.0d0,3532.0d0,3551.0d0,3570.0d0,3590.0d0,3609.0d0,3628.0d0, &
                            3647.0d0,3667.0d0,3686.0d0/ ! Conc=0.5
  DATA (DefaultEthGlyCpData(7,I),I=1,DefaultNumGlyTemps) &
                           /2844.0d0,2866.0d0,2888.0d0,2909.0d0,2931.0d0,2953.0d0,2975.0d0,2997.0d0,3018.0d0,3040.0d0, &
                            3062.0d0,3084.0d0,3106.0d0,3127.0d0,3149.0d0,3171.0d0,3193.0d0,3215.0d0,3236.0d0,3258.0d0, &
                            3280.0d0,3302.0d0,3324.0d0,3345.0d0,3367.0d0,3389.0d0,3411.0d0,3433.0d0,3454.0d0,3476.0d0, &
                            3498.0d0,3520.0d0,3542.0d0/ ! Conc=0.6
  DATA (DefaultEthGlyCpData(8,I),I=1,DefaultNumGlyTemps) &
                           /2612.0d0,2636.0d0,2660.0d0,2685.0d0,2709.0d0,2733.0d0,2757.0d0,2782.0d0,2806.0d0,2830.0d0, &
                            2854.0d0,2878.0d0,2903.0d0,2927.0d0,2951.0d0,2975.0d0,3000.0d0,3024.0d0,3048.0d0,3072.0d0, &
                            3097.0d0,3121.0d0,3145.0d0,3169.0d0,3193.0d0,3218.0d0,3242.0d0,3266.0d0,3290.0d0,3315.0d0, &
                            3339.0d0,3363.0d0,3387.0d0/ ! Conc=0.7
  DATA (DefaultEthGlyCpData(9,I),I=1,DefaultNumGlyTemps) &
                           /2370.0d0,2397.0d0,2423.0d0,2450.0d0,2477.0d0,2503.0d0,2530.0d0,2556.0d0,2583.0d0,2610.0d0, &
                            2636.0d0,2663.0d0,2690.0d0,2716.0d0,2743.0d0,2770.0d0,2796.0d0,2823.0d0,2850.0d0,2876.0d0, &
                            2903.0d0,2929.0d0,2956.0d0,2983.0d0,3009.0d0,3036.0d0,3063.0d0,3089.0d0,3116.0d0,3143.0d0, &
                            3169.0d0,3196.0d0,3223.0d0/ ! Conc=0.8
  DATA (DefaultEthGlyCpData(10,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,2177.0d0,2206.0d0,2235.0d0,2264.0d0,2293.0d0,2322.0d0,2351.0d0,2380.0d0, &
                            2409.0d0,2438.0d0,2467.0d0,2496.0d0,2525.0d0,2554.0d0,2583.0d0,2612.0d0,2641.0d0,2670.0d0, &
                            2699.0d0,2728.0d0,2757.0d0,2786.0d0,2815.0d0,2844.0d0,2873.0d0,2902.0d0,2931.0d0,2960.0d0, &
                            2989.0d0,3018.0d0,3047.0d0/ ! Conc=0.9

          ! Ethylene Glycol Data: Viscosity in mPa-s
  DATA (DefaultEthGlyViscData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 2.08d0, 1.79d0, 1.56d0, &
                              1.37d0, 1.21d0, 1.08d0, 0.97d0, 0.88d0, 0.80d0, 0.73d0, 0.67d0, 0.62d0, 0.57d0, &
                              0.53d0, 0.50d0, 0.47d0, 0.44d0, 0.41d0, 0.39d0, 0.37d0, 0.35d0, 0.33d0, 0.32d0, &
                              0.30d0, 0.29d0, 0.28d0/ ! Conc=0.1
  DATA (DefaultEthGlyViscData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 3.65d0, 3.02d0, 2.54d0, 2.18d0, &
                              1.89d0, 1.65d0, 1.46d0, 1.30d0, 1.17d0, 1.06d0, 0.96d0, 0.88d0, 0.81d0, 0.74d0, &
                              0.69d0, 0.64d0, 0.59d0, 0.55d0, 0.52d0, 0.49d0, 0.46d0, 0.43d0, 0.40d0, 0.38d0, &
                              0.36d0, 0.34d0, 0.33d0/ ! Conc=0.2
  DATA (DefaultEthGlyViscData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 6.19d0, 5.03d0, 4.15d0, 3.48d0, 2.95d0, &
                              2.53d0, 2.20d0, 1.92d0, 1.69d0, 1.50d0, 1.34d0, 1.21d0, 1.09d0, 0.99d0, 0.90d0, &
                              0.83d0, 0.76d0, 0.70d0, 0.65d0, 0.60d0, 0.56d0, 0.52d0, 0.49d0, 0.46d0, 0.43d0, &
                              0.41d0, 0.38d0, 0.36d0/ ! Conc=0.3
  DATA (DefaultEthGlyViscData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 15.75d0, 11.74d0, 9.06d0, 7.18d0, 5.83d0, 4.82d0, 4.04d0, &
                              3.44d0, 2.96d0, 2.57d0, 2.26d0, 1.99d0, 1.77d0, 1.59d0, 1.43d0, 1.29d0, 1.17d0, &
                              1.06d0, 0.97d0, 0.89d0, 0.82d0, 0.76d0, 0.70d0, 0.65d0, 0.60d0, 0.56d0, 0.53d0, &
                              0.49d0, 0.46d0, 0.43d0/ ! Conc=0.4
  DATA (DefaultEthGlyViscData(6,I),I=1,DefaultNumGlyTemps) &
                             /66.93d0, 43.98d0, 30.5d0, 22.07d0, 16.53d0, 12.74d0, 10.05d0, 8.09d0, 6.63d0, 5.50d0, &
                              4.63d0, 3.94d0, 3.39d0, 2.94d0, 2.56d0, 2.26d0, 2.00d0, 1.78d0, 1.59d0, 1.43d0, &
                              1.29d0, 1.17d0, 1.07d0, 0.98d0, 0.89d0, 0.82d0, 0.76d0, 0.70d0, 0.65d0, 0.60d0, &
                              0.56d0, 0.53d0, 0.49d0/ ! Conc=0.5
  DATA (DefaultEthGlyViscData(7,I),I=1,DefaultNumGlyTemps) &
                             /93.44d0, 65.25d0, 46.75d0, 34.28d0, 25.69d0, 19.62d0, 15.25d0, 12.05d0, 9.66d0, 7.85d0, &
                              6.46d0, 5.38d0, 4.52d0, 3.84d0, 3.29d0, 2.84d0, 2.47d0, 2.16d0, 1.91d0, 1.69d0, &
                              1.51d0, 1.35d0, 1.22d0, 1.10d0, 1.00d0, 0.92d0, 0.84d0, 0.77d0, 0.71d0, 0.66d0, &
                              0.61d0, 0.57d0, 0.53d0/ ! Conc=0.6
  DATA (DefaultEthGlyViscData(8,I),I=1,DefaultNumGlyTemps) &
                             /133.53d0, 96.57d0, 70.38d0, 51.94d0, 38.88d0, 29.53d0, 22.76d0, 17.79d0, 14.09d0, 11.31d0, &
                              9.18d0, 7.53d0, 6.24d0, 5.23d0, 4.42d0, 3.76d0, 3.23d0, 2.80d0, 2.43d0, 2.13d0, &
                              1.88d0, 1.67d0, 1.49d0, 1.33d0, 1.20d0, 1.09d0, 0.99d0, 0.90d0, 0.82d0, 0.76d0, &
                              0.70d0, 0.64d0, 0.60d0/ ! Conc=0.7
  DATA (DefaultEthGlyViscData(9,I),I=1,DefaultNumGlyTemps) &
                             /191.09d0, 141.02d0, 102.21d0, 74.53d0, 55.09d0, 41.36d0, 31.56d0, 24.44d0, 19.2d0, 15.29d0, &
                              12.33d0, 10.05d0, 8.29d0, 6.90d0, 5.79d0, 4.91d0, 4.19d0, 3.61d0, 3.12d0, 2.72d0, &
                              2.39d0, 2.11d0, 1.87d0, 1.66d0, 1.49d0, 1.34d0, 1.21d0, 1.10d0, 1.00d0, 0.91d0, &
                              0.83d0, 0.77d0, 0.71d0/ ! Conc=0.8
  DATA (DefaultEthGlyViscData(10,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 196.87d0, 128.43d0, 87.52d0, 61.85d0, 45.08d0, 33.74d0, 25.84d0, 20.18d0, &
                              16.04d0, 12.95d0, 10.59d0, 8.77d0, 7.34d0, 6.21d0, 5.30d0, 4.56d0, 3.95d0, 3.45d0, &
                              3.03d0, 2.67d0, 2.37d0, 2.12d0, 1.90d0, 1.71d0, 1.54d0, 1.40d0, 1.27d0, 1.16d0, &
                              1.07d0, 0.98d0, 0.90d0/ ! Conc=0.9

          ! Ethylene Glycol Data: Density in kg/m3
  DATA (DefaultEthGlyRhoData(2,I),I=1,DefaultNumGlyTemps) &
                    /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 1018.73d0, 1017.57d0, 1016.28d0, &
                  1014.87d0, 1013.34d0, 1011.69d0, 1009.92d0, 1008.02d0, 1006.01d0, 1003.87d0, 1001.61d0, 999.23d0, 996.72d0, &
                   994.10d0, 991.35d0, 988.49d0, 985.50d0, 982.39d0, 979.15d0, 975.80d0, 972.32d0, 968.73d0, 965.01d0, &
                   961.17d0, 957.21d0, 953.12d0/ ! Conc=0.1
  DATA (DefaultEthGlyRhoData(3,I),I=1,DefaultNumGlyTemps) &
                    /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 1036.85d0, 1035.67d0, 1034.36d0, 1032.94d0, &
                  1031.39d0, 1029.72d0, 1027.93d0, 1026.02d0, 1023.99d0, 1021.83d0, 1019.55d0, 1017.16d0, 1014.64d0, 1011.99d0, &
                  1009.23d0, 1006.35d0, 1003.34d0, 1000.21d0, 996.96d0, 993.59d0, 990.10d0, 986.48d0, 982.75d0, 978.89d0, &
                   974.91d0, 970.81d0, 966.59d0/ ! Conc=0.2
  DATA (DefaultEthGlyRhoData(4,I),I=1,DefaultNumGlyTemps) &
                    /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 1054.31d0, 1053.11d0, 1051.78d0, 1050.33d0, 1048.76d0, &
                  1047.07d0, 1045.25d0, 1043.32d0, 1041.26d0, 1039.08d0, 1036.78d0, 1034.36d0, 1031.36d0, 1029.15d0, 1026.36d0, &
                  1023.45d0, 1020.42d0, 1017.27d0, 1014.00d0, 1010.60d0, 1007.09d0, 1003.45d0, 999.69d0, 995.81d0, 991.81d0, &
                   987.68d0, 983.43d0, 979.07d0/ ! Conc=0.3
  DATA (DefaultEthGlyRhoData(5,I),I=1,DefaultNumGlyTemps) &
                    /0.00d0, 0.00d0, 0.00d0, 1071.98d0, 1070.87d0, 1069.63d0, 1068.28d0, 1066.80d0, 1065.21d0, 1063.49d0, &
                  1061.65d0, 1059.68d0, 1057.60d0, 1055.39d0, 1053.07d0, 1050.62d0, 1048.05d0, 1045.35d0, 1042.54d0, 1039.61d0, &
                  1036.55d0, 1033.37d0, 1030.07d0, 1026.65d0, 1023.10d0, 1019.44d0, 1015.65d0, 1011.74d0, 1007.70d0, 1003.56d0, &
                   999.29d0, 994.90d0, 990.38d0/ ! Conc=0.4
  DATA (DefaultEthGlyRhoData(6,I),I=1,DefaultNumGlyTemps) &
                 /1089.94d0, 1089.04d0, 1088.01d0, 1086.87d0, 1085.61d0, 1084.22d0, 1082.71d0, 1081.08d0, 1079.33d0, 1077.46d0, &
                  1075.46d0, 1073.35d0, 1071.11d0, 1068.75d0, 1066.27d0, 1063.66d0, 1060.94d0, 1058.09d0, 1055.13d0, 1052.04d0, &
                  1048.83d0, 1045.49d0, 1042.04d0, 1038.46d0, 1034.77d0, 1030.95d0, 1027.01d0, 1022.95d0, 1018.76d0, 1014.46d0, &
                  1010.03d0, 1005.48d0, 1000.81d0/ ! Conc=0.5
  DATA (DefaultEthGlyRhoData(7,I),I=1,DefaultNumGlyTemps) &
                 /1104.60d0, 1103.54d0, 1102.36d0, 1101.06d0, 1099.64d0, 1098.09d0, 1096.43d0, 1094.64d0, 1092.73d0, 1090.70d0, &
                  1088.54d0, 1086.27d0, 1083.87d0, 1081.35d0, 1078.71d0, 1075.95d0, 1073.07d0, 1070.06d0, 1066.94d0, 1063.69d0, &
                  1060.32d0, 1056.83d0, 1053.22d0, 1049.48d0, 1045.63d0, 1041.65d0, 1037.55d0, 1033.33d0, 1028.99d0, 1024.52d0, &
                  1019.94d0, 1015.23d0, 1010.40d0/ ! Conc=0.6
  DATA (DefaultEthGlyRhoData(8,I),I=1,DefaultNumGlyTemps) &
                 /1118.61d0, 1117.38d0, 1116.04d0, 1114.58d0, 1112.99d0, 1111.28d0, 1109.45d0, 1107.50d0, 1105.43d0, 1103.23d0, &
                  1100.92d0, 1098.48d0, 1095.92d0, 1093.24d0, 1090.43d0, 1087.51d0, 1084.46d0, 1081.30d0, 1078.01d0, 1074.60d0, &
                  1071.06d0, 1067.41d0, 1063.64d0, 1059.74d0, 1055.72d0, 1051.58d0, 1047.32d0, 1042.93d0, 1038.43d0, 1033.80d0, &
                  1029.05d0, 1024.18d0, 1019.19d0/ ! Conc=0.7
  DATA (DefaultEthGlyRhoData(9,I),I=1,DefaultNumGlyTemps) &
                 /1132.11d0, 1130.72d0, 1129.21d0, 1127.57d0, 1125.82d0, 1123.94d0, 1121.94d0, 1119.82d0, 1117.58d0, 1115.22d0, &
                  1112.73d0, 1110.13d0, 1107.40d0, 1104.55d0, 1101.58d0, 1098.48d0, 1095.27d0, 1091.93d0, 1088.48d0, 1084.90d0, &
                  1081.20d0, 1077.37d0, 1073.43d0, 1069.36d0, 1065.18d0, 1060.87d0, 1056.44d0, 1051.88d0, 1047.21d0, 1042.41d0, &
                  1037.50d0, 1032.46d0, 1027.30d0/ ! Conc=0.8
  DATA (DefaultEthGlyRhoData(10,I),I=1,DefaultNumGlyTemps) &
                 /0.00d0, 0.00d0, 1141.87d0, 1140.07d0, 1138.14d0, 1136.09d0, 1133.91d0, 1131.62d0, 1129.20d0, 1126.67d0, &
                  1124.01d0, 1121.23d0, 1118.32d0, 1115.30d0, 1112.15d0, 1108.89d0, 1105.50d0, 1101.99d0, 1098.36d0, 1094.60d0, &
                  1090.73d0, 1086.73d0, 1082.61d0, 1078.37d0, 1074.01d0, 1069.53d0, 1064.92d0, 1060.20d0, 1055.35d0, 1050.38d0, &
                  1045.29d0, 1040.08d0, 1034.74d0/ ! Conc=0.9

          ! Ethylene Glycol Data: Conductivity in W/(m-K)
  DATA (DefaultEthGlyCondData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.511d0, 0.520d0, 0.528d0, &
                              0.537d0, 0.545d0, 0.552d0, 0.559d0, 0.566d0, 0.572d0, 0.577d0, 0.583d0, 0.588d0, 0.592d0, &
                              0.596d0, 0.600d0, 0.603d0, 0.606d0, 0.608d0, 0.610d0, 0.612d0, 0.613d0, 0.614d0, 0.614d0, &
                              0.614d0, 0.613d0, 0.612d0/ ! Conc=0.1
  DATA (DefaultEthGlyCondData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.460d0, 0.468d0, 0.476d0, 0.483d0, &
                              0.490d0, 0.497d0, 0.503d0, 0.509d0, 0.515d0, 0.520d0, 0.525d0, 0.529d0, 0.534d0, 0.538d0, &
                              0.541d0, 0.544d0, 0.547d0, 0.549d0, 0.551d0, 0.553d0, 0.555d0, 0.556d0, 0.556d0, 0.557d0, &
                              0.557d0, 0.556d0, 0.555d0/ ! Conc=0.2
  DATA (DefaultEthGlyCondData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.415d0, 0.422d0, 0.429d0, 0.436d0, 0.442d0, &
                              0.448d0, 0.453d0, 0.459d0, 0.464d0, 0.469d0, 0.473d0, 0.477d0, 0.481d0, 0.485d0, 0.488d0, &
                              0.491d0, 0.494d0, 0.496d0, 0.498d0, 0.500d0, 0.501d0, 0.503d0, 0.504d0, 0.504d0, 0.505d0, &
                              0.505d0, 0.504d0, 0.504d0/ ! Conc=0.3
  DATA (DefaultEthGlyCondData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.371d0, 0.377d0, 0.383d0, 0.389d0, 0.395d0, 0.400d0, 0.405d0, &
                              0.410d0, 0.415d0, 0.419d0, 0.424d0, 0.428d0, 0.431d0, 0.435d0, 0.438d0, 0.441d0, 0.444d0, &
                              0.446d0, 0.449d0, 0.451d0, 0.452d0, 0.454d0, 0.455d0, 0.456d0, 0.457d0, 0.458d0, 0.458d0, &
                              0.458d0, 0.458d0, 0.458d0/ ! Conc=0.4
  DATA (DefaultEthGlyCondData(6,I),I=1,DefaultNumGlyTemps) &
                             /0.328d0, 0.333d0, 0.339d0, 0.344d0, 0.349d0, 0.354d0, 0.359d0, 0.364d0, 0.368d0, 0.373d0, &
                              0.377d0, 0.380d0, 0.384d0, 0.387d0, 0.391d0, 0.394d0, 0.397d0, 0.399d0, 0.402d0, 0.404d0, &
                              0.406d0, 0.408d0, 0.410d0, 0.411d0, 0.413d0, 0.414d0, 0.415d0, 0.416d0, 0.416d0, 0.417d0, &
                              0.417d0, 0.417d0, 0.417d0/ ! Conc=0.5
  DATA (DefaultEthGlyCondData(7,I),I=1,DefaultNumGlyTemps) &
                             /0.307d0, 0.312d0, 0.316d0, 0.321d0, 0.325d0, 0.329d0, 0.333d0, 0.336d0, 0.340d0, 0.343d0, &
                              0.346d0, 0.349d0, 0.352d0, 0.355d0, 0.358d0, 0.360d0, 0.363d0, 0.365d0, 0.367d0, 0.369d0, &
                              0.371d0, 0.372d0, 0.374d0, 0.375d0, 0.376d0, 0.377d0, 0.378d0, 0.379d0, 0.379d0, 0.380d0, &
                              0.380d0, 0.380d0, 0.380d0/ ! Conc=0.6
  DATA (DefaultEthGlyCondData(8,I),I=1,DefaultNumGlyTemps) &
                             /0.289d0, 0.293d0, 0.296d0, 0.300d0, 0.303d0, 0.306d0, 0.309d0, 0.312d0, 0.314d0, 0.317d0, &
                              0.320d0, 0.322d0, 0.324d0, 0.327d0, 0.329d0, 0.331d0, 0.332d0, 0.334d0, 0.336d0, 0.337d0, &
                              0.339d0, 0.340d0, 0.341d0, 0.342d0, 0.343d0, 0.344d0, 0.345d0, 0.346d0, 0.346d0, 0.347d0, &
                              0.347d0, 0.347d0, 0.347d0/ ! Conc=0.7
  DATA (DefaultEthGlyCondData(9,I),I=1,DefaultNumGlyTemps) &
                             /0.274, 0.276, 0.279, 0.281, 0.283, 0.286, 0.288, 0.290, 0.292, 0.294, &
                              0.296, 0.298, 0.299, 0.301, 0.303, 0.304, 0.306, 0.307, 0.308, 0.310, &
                              0.311, 0.312, 0.313, 0.314, 0.314, 0.315, 0.316, 0.316, 0.317, 0.317, &
                              0.318, 0.318, 0.318/ ! Conc=0.8
  DATA (DefaultEthGlyCondData(10,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.263d0, 0.265d0, 0.266d0, 0.268d0, 0.269d0, 0.271d0, 0.272d0, 0.274d0, &
                              0.275d0, 0.276d0, 0.278d0, 0.279d0, 0.280d0, 0.281d0, 0.282d0, 0.283d0, 0.284d0, 0.285d0, &
                              0.286d0, 0.287d0, 0.288d0, 0.288d0, 0.289d0, 0.290d0, 0.290d0, 0.291d0, 0.291d0, 0.292d0, &
                              0.292d0, 0.293d0, 0.293d0/ ! Conc=0.9

          ! Propylene Glycol Data: Specific Heat in J/(kg-k)
  DATA (DefaultPropGlyCpData(2,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,4042.0d0,4050.0d0,4058.0d0, &
                            4067.0d0,4075.0d0,4083.0d0,4091.0d0,4099.0d0,4107.0d0,4115.0d0,4123.0d0,4131.0d0,4139.0d0, &
                            4147.0d0,4155.0d0,4163.0d0,4171.0d0,4179.0d0,4187.0d0,4195.0d0,4203.0d0,4211.0d0,4219.0d0, &
                            4227.0d0,4235.0d0,4243.0d0/ ! Conc=0.1
  DATA (DefaultPropGlyCpData(3,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3918.0d0,3929.0d0,3940.0d0,3951.0d0, &
                            3962.0d0,3973.0d0,3983.0d0,3994.0d0,4005.0d0,4016.0d0,4027.0d0,4038.0d0,4049.0d0,4060.0d0, &
                            4071.0d0,4082.0d0,4093.0d0,4104.0d0,4115.0d0,4126.0d0,4136.0d0,4147.0d0,4158.0d0,4169.0d0, &
                            4180.0d0,4191.0d0,4202.0d0/ ! Conc=0.2
  DATA (DefaultPropGlyCpData(4,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3765.0d0,3779.0d0,3793.0d0,3807.0d0,3820.0d0, &
                            3834.0d0,3848.0d0,3862.0d0,3875.0d0,3889.0d0,3903.0d0,3917.0d0,3930.0d0,3944.0d0,3958.0d0, &
                            3972.0d0,3985.0d0,3999.0d0,4013.0d0,4027.0d0,4040.0d0,4054.0d0,4068.0d0,4082.0d0,4095.0d0, &
                            4109.0d0,4123.0d0,4137.0d0/ ! Conc=0.3
  DATA (DefaultPropGlyCpData(5,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,0.0d0,0.0d0,3586.0d0,3603.0d0,3619.0d0,3636.0d0,3652.0d0,3669.0d0, &
                            3685.0d0,3702.0d0,3718.0d0,3735.0d0,3751.0d0,3768.0d0,3784.0d0,3801.0d0,3817.0d0,3834.0d0, &
                            3850.0d0,3867.0d0,3883.0d0,3900.0d0,3916.0d0,3933.0d0,3949.0d0,3966.0d0,3982.0d0,3999.0d0, &
                            4015.0d0,4032.0d0,4049.0d0/ ! Conc=0.4
  DATA (DefaultPropGlyCpData(6,I),I=1,DefaultNumGlyTemps) &
                           /0.0d0,0.0d0,3358.0d0,3378.0d0,3397.0d0,3416.0d0,3435.0d0,3455.0d0,3474.0d0,3493.0d0, &
                            3513.0d0,3532.0d0,3551.0d0,3570.0d0,3590.0d0,3609.0d0,3628.0d0,3648.0d0,3667.0d0,3686.0d0, &
                            3706.0d0,3725.0d0,3744.0d0,3763.0d0,3783.0d0,3802.0d0,3821.0d0,3841.0d0,3860.0d0,3879.0d0, &
                            3898.0d0,3918.0d0,3937.0d0/ ! Conc=0.5
  DATA (DefaultPropGlyCpData(7,I),I=1,DefaultNumGlyTemps) &
                           /3096.0d0,3118.0d0,3140.0d0,3162.0d0,3184.0d0,3206.0d0,3228.0d0,3250.0d0,3272.0d0,3295.0d0, &
                            3317.0d0,3339.0d0,3361.0d0,3383.0d0,3405.0d0,3427.0d0,3449.0d0,3471.0d0,3493.0d0,3515.0d0, &
                            3537.0d0,3559.0d0,3581.0d0,3603.0d0,3625.0d0,3647.0d0,3670.0d0,3692.0d0,3714.0d0,3736.0d0, &
                            3758.0d0,3780.0d0,3802.0d0/ ! Conc=0.6
  DATA (DefaultPropGlyCpData(8,I),I=1,DefaultNumGlyTemps) &
                           /2843.0d0,2868.0d0,2893.0d0,2918.0d0,2943.0d0,2968.0d0,2993.0d0,3018.0d0,3042.0d0,3067.0d0, &
                            3092.0d0,3117.0d0,3142.0d0,3167.0d0,3192.0d0,3217.0d0,3242.0d0,3266.0d0,3291.0d0,3316.0d0, &
                            3341.0d0,3366.0d0,3391.0d0,3416.0d0,3441.0d0,3465.0d0,3490.0d0,3515.0d0,3540.0d0,3565.0d0, &
                            3590.0d0,3615.0d0,3640.0d0/ ! Conc=0.7
  DATA (DefaultPropGlyCpData(9,I),I=1,DefaultNumGlyTemps) &
                           /2572.0d0,2600.0d0,2627.0d0,2655.0d0,2683.0d0,2710.0d0,2738.0d0,2766.0d0,2793.0d0,2821.0d0, &
                            2849.0d0,2876.0d0,2904.0d0,2931.0d0,2959.0d0,2987.0d0,3014.0d0,3042.0d0,3070.0d0,3097.0d0, &
                            3125.0d0,3153.0d0,3180.0d0,3208.0d0,3236.0d0,3263.0d0,3291.0d0,3319.0d0,3346.0d0,3374.0d0, &
                            3402.0d0,3429.0d0,3457.0d0/ ! Conc=0.8
  DATA (DefaultPropGlyCpData(10,I),I=1,DefaultNumGlyTemps) &
                           /2264.0d0,2295.0d0,2326.0d0,2356.0d0,2387.0d0,2417.0d0,2448.0d0,2478.0d0,2509.0d0,2539.0d0, &
                            2570.0d0,2600.0d0,2631.0d0,2661.0d0,2692.0d0,2723.0d0,2753.0d0,2784.0d0,2814.0d0,2845.0d0, &
                            2875.0d0,2906.0d0,2936.0d0,2967.0d0,2997.0d0,3028.0d0,3058.0d0,3089.0d0,3119.0d0,3150.0d0, &
                            3181.0d0,3211.0d0,3242.0d0/ ! Conc=0.9

          ! Propylene Glycol Data: Viscosity in mPa-s
  DATA (DefaultPropGlyViscData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 2.68d0, 2.23d0, 1.89d0, &
                              1.63d0, 1.42d0, 1.25d0, 1.11d0, 0.99d0, 0.89d0, 0.81d0, 0.73d0, 0.67d0, 0.62d0, &
                              0.57d0, 0.53d0, 0.49d0, 0.46d0, 0.43d0, 0.40d0, 0.38d0, 0.35d0, 0.33d0, 0.32d0, &
                              0.30d0, 0.28d0, 0.27d0/ ! Conc=0.1
  DATA (DefaultPropGlyViscData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 4.98d0, 4.05d0, 3.34d0, 2.79d0, &
                              2.36d0, 2.02d0, 1.74d0, 1.52d0, 1.34d0, 1.18d0, 1.06d0, 0.95d0, 0.86d0, 0.78d0, &
                              0.71d0, 0.66d0, 0.60d0, 0.56d0, 0.52d0, 0.49d0, 0.45d0, 0.43d0, 0.40d0, 0.38d0, &
                              0.36d0, 0.34d0, 0.32d0/ ! Conc=0.2
  DATA (DefaultPropGlyViscData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 11.87d0, 9.08d0, 7.08d0, 5.61d0, 4.52d0, &
                              3.69d0, 3.06d0, 2.57d0, 2.18d0, 1.88d0, 1.63d0, 1.43d0, 1.26d0, 1.13d0, 1.01d0, &
                              0.91d0, 0.83d0, 0.76d0, 0.70d0, 0.65d0, 0.61d0, 0.57d0, 0.53d0, 0.50d0, 0.47d0, &
                              0.45d0, 0.43d0, 0.41d0/ ! Conc=0.3
  DATA (DefaultPropGlyViscData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 0.00d0, 0.00d0, 33.22d0, 23.27d0, 16.75d0, 12.37d0, 9.35d0, 7.22d0, &
                              5.69d0, 4.57d0, 3.73d0, 3.09d0, 2.60d0, 2.21d0, 1.91d0, 1.66d0, 1.47d0, 1.30d0, &
                              1.17d0, 1.06d0, 0.96d0, 0.88d0, 0.81d0, 0.75d0, 0.70d0, 0.66d0, 0.62d0, 0.59d0, &
                              0.56d0, 0.53d0, 0.51d0/ ! Conc=0.4
  DATA (DefaultPropGlyViscData(6,I),I=1,DefaultNumGlyTemps) &
                             /0.00d0, 0.00d0, 110.59d0, 73.03d0, 49.7d0, 34.78d0, 24.99d0, 18.4d0, 13.85d0, 10.65d0, &
                              8.34d0, 6.65d0, 5.39d0, 4.43d0, 3.69d0, 3.11d0, 2.65d0, 2.29d0, 1.99d0, 1.75d0, &
                              1.55d0, 1.38d0, 1.24d0, 1.12d0, 1.02d0, 0.93d0, 0.86d0, 0.79d0, 0.74d0, 0.69d0, &
                              0.64d0, 0.6d0, 0.57d0/ ! Conc=0.5
  DATA (DefaultPropGlyViscData(7,I),I=1,DefaultNumGlyTemps) &
                             /524.01d0, 330.39d0, 211.43d0, 137.96d0, 92.00d0, 62.78d0, 43.84d0, 31.32d0, 22.87d0, 17.05d0, &
                              12.96d0, 10.04d0, 7.91d0, 6.34d0, 5.15d0, 4.25d0, 3.55d0, 3.00d0, 2.57d0, 2.22d0, &
                              1.93d0, 1.70d0, 1.51d0, 1.35d0, 1.22d0, 1.10d0, 1.01d0, 0.92d0, 0.85d0, 0.79d0, &
                              0.74d0, 0.69d0, 0.65d0/ ! Conc=0.6
  DATA (DefaultPropGlyViscData(8,I),I=1,DefaultNumGlyTemps) &
                             /916.18d0, 551.12d0, 340.09d0, 215.67d0, 140.62d0, 94.23d0, 64.83d0, 45.74d0, 33.04d0, 24.41d0, &
                              18.41d0, 14.15d0, 11.08d0, 8.81d0, 7.12d0, 5.84d0, 4.85d0, 4.08d0, 3.46d0, 2.98d0, &
                              2.58d0, 2.26d0, 1.99d0, 1.77d0, 1.59d0, 1.43d0, 1.30d0, 1.18d0, 1.08d0, 1.00d0, &
                              0.93d0, 0.86d0, 0.80d0/ ! Conc=0.7
  DATA (DefaultPropGlyViscData(9,I),I=1,DefaultNumGlyTemps) &
                             /1434.22d0, 908.47d0, 575.92d0, 368.77d0, 239.86d0, 159.02d0, 107.64d0, 74.45d0, 52.63d0, 37.99d0, &
                              28.00d0, 21.04d0, 16.10d0, 12.55d0, 9.94d0, 7.99d0, 6.52d0, 5.39d0, 4.51d0, 3.82d0, &
                              3.28d0, 2.83d0, 2.47d0, 2.18d0, 1.94d0, 1.73d0, 1.56d0, 1.42d0, 1.29d0, 1.19d0, &
                              1.09d0, 1.02d0, 0.95d0/ ! Conc=0.8
  DATA (DefaultPropGlyViscData(10,I),I=1,DefaultNumGlyTemps) &
                             /3813.29d0, 2071.34d0, 1176.09d0, 696.09d0, 428.19d0, 272.94d0, 179.78d0, 122.03d0, 85.15d0, 60.93d0, &
                              44.62d0, 33.38d0, 25.45d0, 19.76d0, 15.60d0, 12.49d0, 10.15d0, 8.35d0, 6.95d0, 5.85d0, &
                              4.97d0, 4.26d0, 3.69d0, 3.22d0, 2.83d0, 2.50d0, 2.23d0, 2.00d0, 1.80d0, 1.63d0, &
                              1.48d0, 1.35d0, 1.24d0/ ! Conc=0.9

          ! Propylene Glycol Data: Density in kg/m3
  DATA (DefaultPropGlyRhoData(2,I),I=1,DefaultNumGlyTemps) &
                  /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 1013.85d0, 1012.61d0, 1011.24d0, &
                   1009.75d0, 1008.13d0, 1006.40d0, 1004.54d0, 1002.56d0, 1000.46d0, 998.23d0, 995.88d0, 993.41d0, 990.82d0, &
                    988.11d0, 985.27d0, 982.31d0, 979.23d0, 976.03d0, 972.70d0, 969.25d0, 965.68d0, 961.99d0, 958.17d0, &
                    954.24d0, 950.18d0, 945.99d0/ ! Conc=0.1
  DATA (DefaultPropGlyRhoData(3,I),I=1,DefaultNumGlyTemps) &
                  /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 1027.24d0, 1025.84d0, 1024.32d0, 1022.68d0, &
                   1020.91d0, 1019.01d0, 1016.99d0, 1014.84d0, 1012.56d0, 1010.16d0, 1007.64d0, 1004.99d0, 1002.21d0, 999.31d0, &
                    996.28d0, 993.12d0, 989.85d0, 986.44d0, 982.91d0, 979.25d0, 975.47d0, 971.56d0, 967.53d0, 963.37d0, &
                    959.09d0, 954.67d0, 950.14d0/ ! Conc=0.2
  DATA (DefaultPropGlyRhoData(4,I),I=1,DefaultNumGlyTemps) &
                  /0.00d0, 0.00d0, 0.00d0, 0.00d0, 0.00d0, 1039.42d0, 1037.89d0, 1036.24d0, 1034.46d0, 1032.55d0, &
                   1030.51d0, 1028.35d0, 1026.06d0, 1023.64d0, 1021.09d0, 1018.42d0, 1015.62d0, 1012.69d0, 1009.63d0, 1006.44d0, &
                   1003.13d0, 999.69d0, 996.12d0, 992.42d0, 988.60d0, 984.65d0, 980.57d0, 976.36d0, 972.03d0, 967.56d0, &
                    962.97d0, 958.26d0, 953.41d0/ ! Conc=0.3
  DATA (DefaultPropGlyRhoData(5,I),I=1,DefaultNumGlyTemps) &
                  /0.00d0, 0.00d0, 0.00d0, 0.00d0, 1050.43d0, 1048.79d0, 1047.02d0, 1045.12d0, 1043.09d0, 1040.94d0, &
                   1038.65d0, 1036.24d0, 1033.70d0, 1031.03d0, 1028.23d0, 1025.30d0, 1022.24d0, 1019.06d0, 1015.75d0, 1012.30d0, &
                   1008.73d0, 1005.03d0, 1001.21d0, 997.25d0, 993.17d0, 988.95d0, 984.61d0, 980.14d0, 975.54d0, 970.81d0, &
                    965.95d0, 960.97d0, 955.86d0/ ! Conc=0.4
  DATA (DefaultPropGlyRhoData(6,I),I=1,DefaultNumGlyTemps) &
                  /0.00d0, 0.00d0, 1062.11d0, 1060.49d0, 1058.73d0, 1056.85d0, 1054.84d0, 1052.71d0, 1050.44d0, 1048.04d0, &
                   1045.52d0, 1042.87d0, 1040.09d0, 1037.18d0, 1034.15d0, 1030.98d0, 1027.69d0, 1024.27d0, 1020.72d0, 1017.04d0, &
                   1013.23d0, 1009.30d0, 1005.24d0, 1001.05d0, 996.73d0, 992.28d0, 987.70d0, 983.00d0, 978.16d0, 973.20d0, &
                    968.11d0, 962.89d0, 957.55d0/ ! Conc=0.5
  DATA (DefaultPropGlyRhoData(7,I),I=1,DefaultNumGlyTemps) &
                  /1072.92d0, 1071.31d0, 1069.58d0, 1067.72d0, 1065.73d0, 1063.61d0, 1061.37d0, 1059.00d0, 1056.50d0, 1053.88d0, &
                   1051.13d0, 1048.25d0, 1045.24d0, 1042.11d0, 1038.85d0, 1035.47d0, 1031.95d0, 1028.32d0, 1024.55d0, 1020.66d0, &
                   1016.63d0, 1012.49d0, 1008.21d0, 1003.81d0, 999.28d0, 994.63d0, 989.85d0, 984.94d0, 979.90d0, 974.74d0, &
                    969.45d0, 964.03d0, 958.49d0/ ! Conc=0.6
  DATA (DefaultPropGlyRhoData(8,I),I=1,DefaultNumGlyTemps) &
                  /1079.67d0, 1077.82d0, 1075.84d0, 1073.74d0, 1071.51d0, 1069.16d0, 1066.69d0, 1064.09d0, 1061.36d0, 1058.51d0, &
                   1055.54d0, 1052.44d0, 1049.22d0, 1045.87d0, 1042.40d0, 1038.81d0, 1035.09d0, 1031.25d0, 1027.28d0, 1023.19d0, &
                   1018.97d0, 1014.63d0, 1010.16d0, 1005.57d0, 1000.86d0, 996.02d0, 991.06d0, 985.97d0, 980.76d0, 975.42d0, &
                    969.96d0, 964.38d0, 958.67d0/ ! Conc=0.7
  DATA (DefaultPropGlyRhoData(9,I),I=1,DefaultNumGlyTemps) &
                  /1094.50d0, 1090.85d0, 1087.18d0, 1083.49d0, 1079.77d0, 1076.04d0, 1072.27d0, 1068.49d0, 1064.68d0, 1060.85d0, &
                   1057.00d0, 1053.12d0, 1049.22d0, 1045.30d0, 1041.35d0, 1037.38d0, 1033.39d0, 1029.37d0, 1025.33d0, 1021.27d0, &
                   1017.19d0, 1013.08d0, 1008.95d0, 1004.79d0, 1000.62d0, 996.41d0, 992.19d0, 987.94d0, 983.68d0, 979.38d0, &
                    975.07d0, 970.73d0, 966.37d0/ ! Conc=0.8
  DATA (DefaultPropGlyRhoData(10,I),I=1,DefaultNumGlyTemps) &
                  /1092.46d0, 1088.82d0, 1085.15d0, 1081.46d0, 1077.74d0, 1074.00d0, 1070.24d0, 1066.46d0, 1062.65d0, 1058.82d0, &
                   1054.96d0, 1051.09d0, 1047.19d0, 1043.26d0, 1039.32d0, 1035.35d0, 1031.35d0, 1027.34d0, 1023.30d0, 1019.24d0, &
                   1015.15d0, 1011.04d0, 1006.91d0, 1002.76d0, 998.58d0, 994.38d0, 990.16d0, 985.91d0, 981.64d0, 977.35d0, &
                    973.03d0, 968.69d0, 964.33d0/ ! Conc=0.9

          ! Propylene Glycol Data: Conductivity in W/(m-K)
  DATA (DefaultPropGlyCondData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.510d0, 0.518d0, 0.527d0, &
                              0.535d0, 0.543d0, 0.550d0, 0.557d0, 0.563d0, 0.569d0, 0.575d0, 0.580d0, 0.585d0, 0.589d0, &
                              0.593d0, 0.596d0, 0.599d0, 0.602d0, 0.604d0, 0.606d0, 0.607d0, 0.608d0, 0.609d0, 0.609d0, &
                              0.608d0, 0.608d0, 0.606d0/ ! Conc=0.1
  DATA (DefaultPropGlyCondData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.456d0, 0.464d0, 0.472d0, 0.479d0, &
                              0.485d0, 0.492d0, 0.498d0, 0.503d0, 0.508d0, 0.513d0, 0.518d0, 0.522d0, 0.526d0, 0.529d0, &
                              0.532d0, 0.535d0, 0.538d0, 0.540d0, 0.541d0, 0.543d0, 0.544d0, 0.544d0, 0.544d0, 0.544d0, &
                              0.544d0, 0.543d0, 0.542d0/ ! Conc=0.2
  DATA (DefaultPropGlyCondData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.410d0, 0.416d0, 0.423d0, 0.429d0, 0.434d0, &
                              0.440d0, 0.445d0, 0.449d0, 0.454d0, 0.458d0, 0.462d0, 0.466d0, 0.469d0, 0.472d0, 0.475d0, &
                              0.477d0, 0.479d0, 0.481d0, 0.482d0, 0.484d0, 0.484d0, 0.485d0, 0.485d0, 0.485d0, 0.485d0, &
                              0.485d0, 0.484d0, 0.482d0/ ! Conc=0.3
  DATA (DefaultPropGlyCondData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.000d0, 0.000d0, 0.369d0, 0.375d0, 0.380d0, 0.385d0, 0.389d0, 0.394d0, &
                              0.398d0, 0.402d0, 0.406d0, 0.409d0, 0.412d0, 0.415d0, 0.418d0, 0.420d0, 0.423d0, 0.425d0, &
                              0.426d0, 0.428d0, 0.429d0, 0.430d0, 0.431d0, 0.431d0, 0.432d0, 0.432d0, 0.432d0, 0.431d0, &
                              0.430d0, 0.429d0, 0.428d0/ ! Conc=0.4
  DATA (DefaultPropGlyCondData(6,I),I=1,DefaultNumGlyTemps) &
                             /0.000d0, 0.000d0, 0.329d0, 0.334d0, 0.338d0, 0.342d0, 0.346d0, 0.349d0, 0.353d0, 0.356d0, &
                              0.359d0, 0.362d0, 0.365d0, 0.367d0, 0.370d0, 0.372d0, 0.374d0, 0.375d0, 0.377d0, 0.378d0, &
                              0.379d0, 0.380d0, 0.381d0, 0.382d0, 0.382d0, 0.382d0, 0.382d0, 0.382d0, 0.382d0, 0.381d0, &
                              0.380d0, 0.379d0, 0.378d0/ ! Conc=0.5
  DATA (DefaultPropGlyCondData(7,I),I=1,DefaultNumGlyTemps) &
                             /0.296d0, 0.300d0, 0.303d0, 0.306d0, 0.309d0, 0.312d0, 0.314d0, 0.317d0, 0.319d0, 0.321d0, &
                              0.323d0, 0.325d0, 0.327d0, 0.329d0, 0.330d0, 0.331d0, 0.333d0, 0.334d0, 0.335d0, 0.335d0, &
                              0.336d0, 0.336d0, 0.337d0, 0.337d0, 0.337d0, 0.337d0, 0.336d0, 0.336d0, 0.335d0, 0.335d0, &
                              0.334d0, 0.333d0, 0.332d0/ ! Conc=0.6
  DATA (DefaultPropGlyCondData(8,I),I=1,DefaultNumGlyTemps) &
                             /0.275d0, 0.277d0, 0.278d0, 0.280d0, 0.282d0, 0.284d0, 0.285d0, 0.286d0, 0.289d0, 0.290d0, &
                              0.291d0, 0.292d0, 0.293d0, 0.293d0, 0.294d0, 0.294d0, 0.295d0, 0.295d0, 0.295d0, 0.295d0, &
                              0.295d0, 0.295d0, 0.295d0, 0.295d0, 0.295d0, 0.294d0, 0.294d0, 0.293d0, 0.292d0, 0.292d0, &
                              0.291d0, 0.290d0, 0.288d0/ ! Conc=0.7
  DATA (DefaultPropGlyCondData(9,I),I=1,DefaultNumGlyTemps) &
                             /0.255d0, 0.256d0, 0.257d0, 0.257d0, 0.258d0, 0.259d0, 0.259d0, 0.259d0, 0.260d0, 0.260d0, &
                              0.260d0, 0.261d0, 0.261d0, 0.261d0, 0.261d0, 0.261d0, 0.260d0, 0.260d0, 0.260d0, 0.260d0, &
                              0.259d0, 0.259d0, 0.258d0, 0.258d0, 0.257d0, 0.256d0, 0.256d0, 0.255d0, 0.254d0, 0.253d0, &
                              0.252d0, 0.251d0, 0.250d0/ ! Conc=0.8
  DATA (DefaultPropGlyCondData(10,I),I=1,DefaultNumGlyTemps) &
                             /0.237d0, 0.237d0, 0.236d0, 0.236d0, 0.236d0, 0.235d0, 0.235d0, 0.234d0, 0.234d0, 0.233d0, &
                              0.233d0, 0.232d0, 0.233d0, 0.231d0, 0.230d0, 0.229d0, 0.229d0, 0.228d0, 0.227d0, 0.227d0, &
                              0.226d0, 0.225d0, 0.224d0, 0.223d0, 0.222d0, 0.221d0, 0.220d0, 0.219d0, 0.218d0, 0.217d0, &
                              0.216d0, 0.215d0, 0.214d0/ ! Conc=0.9

                  ! Steam Refrigerant Data
  DATA (DefaultSteamTemps(I),I=1,DefaultNumSteamTemps)  &
     /1.00d-002,1.0d0,5.0d0,10.0d0,15.0d0,20.0d0,25.0d0,30.0d0,35.0d0,40.0d0,45.0d0,50.0d0,55.0d0,60.0d0,65.0d0,70.0d0,     &
      72.0d0,74.0d0,76.0d0,78.0d0,80.0d0,82.0d0,84.0d0,86.0d0,88.0d0,90.0d0,92.0d0,94.0d0,96.0d0,98.0d0,99.0d0,100.0d0,     &
      101.0d0,102.0d0,103.0d0,104.0d0,105.0d0,106.0d0,107.0d0,108.0d0,109.0d0,110.0d0,111.0d0,112.0d0,113.0d0,114.0d0,      &
      115.0d0,116.0d0,117.0d0,118.0d0,119.0d0,120.0d0,121.0d0,122.0d0,123.0d0,124.0d0,125.0d0,126.0d0,127.0d0,128.0d0,      &
      129.0d0,130.0d0,132.0d0,134.0d0,136.0d0,138.0d0,140.0d0,142.0d0,144.0d0,146.0d0,148.0d0,150.0d0,152.0d0,154.0d0,      &
      156.0d0,158.0d0,160.0d0,162.0d0,164.0d0,166.0d0,168.0d0,170.0d0,172.0d0,174.0d0,176.0d0,178.0d0,180.0d0,185.0d0,      &
      190.0d0,195.0d0,200.0d0,205.0d0,210.0d0,215.0d0,220.0d0,225.0d0,230.0d0,240.0d0,250.0d0,260.0d0,270.0d0,280.0d0,      &
      290.0d0,300.0d0,310.0d0,320.0d0,330.0d0,340.0d0,350.0d0,360.0d0,370.0d0/

  DATA (DefaultSteamPressData(I),I=1,DefaultNumSteamTemps)  &
     /611.7d0,657.1d0,872.6d0,1228.0d0,1706.0d0,2339.0d0,3170.0d0,4247.0d0,5629.0d0,7385.0d0,9595.0d0,12350.0d0,15760.0d0,  &
      19950.0d0,25040.0d0,31200.0d0,34000.0d0,37010.0d0,40240.0d0,43700.0d0,47410.0d0,51390.0d0,55640.0d0,60170.0d0,        &
      65020.0d0,70180.0d0,75680.0d0,81540.0d0,87770.0d0,94390.0d0,97850.0d0,101400.0d0,105100.0d0,108900.0d0,112800.0d0,    &
      116800.0d0,120900.0d0,125100.0d0,129500.0d0,134000.0d0,138600.0d0,143400.0d0,148300.0d0,153300.0d0,158400.0d0,        &
      163700.0d0,169200.0d0,174800.0d0,180500.0d0,186400.0d0,192500.0d0,198700.0d0,205000.0d0,211600.0d0,218300.0d0,        &
      225200.0d0,232200.0d0,239500.0d0,246900.0d0,254500.0d0,262300.0d0,270300.0d0,286800.0d0,304200.0d0,322400.0d0,        &
      341500.0d0,361500.0d0,382500.0d0,404400.0d0,427300.0d0,451200.0d0,476200.0d0,502200.0d0,529500.0d0,557800.0d0,        &
      587400.0d0,618200.0d0,650300.0d0,683700.0d0,718500.0d0,754600.0d0,792200.0d0,831200.0d0,871800.0d0,913800.0d0,        &
      957500.0d0,1003000.0d0,1123000.0d0,1255000.0d0,1399000.0d0,1555000.0d0,1724000.0d0,1908000.0d0,2106000.0d0,           &
      2320000.0d0,2550000.0d0,2797000.0d0,3347000.0d0,3976000.0d0,4692000.0d0,5503000.0d0,6417000.0d0,7442000.0d0,          &
      8588000.0d0,9865000.0d0,11280000.0d0,12860000.0d0,14600000.0d0,16530000.0d0,18670000.0d0,21040000.0d0/

  DATA (DefaultSteamEnthalpyFluidData(I),I=1,DefaultNumSteamTemps)  &
     /0.59d0,4177.0d0,21020.0d0,42020.0d0,62980.0d0,83910.0d0,104800.0d0,125700.0d0,146600.0d0,167500.0d0,188400.0d0,       &
      209300.0d0,230300.0d0,251200.0d0,272100.0d0,293100.0d0,301400.0d0,309800.0d0,318200.0d0,326600.0d0,335000.0d0,        &
      343400.0d0,351800.0d0,360200.0d0,368600.0d0,377000.0d0,385500.0d0,393900.0d0,402300.0d0,410700.0d0,414900.0d0,        &
      419200.0d0,423400.0d0,427600.0d0,431800.0d0,436000.0d0,440300.0d0,444500.0d0,448700.0d0,453000.0d0,457200.0d0,        &
      461400.0d0,465600.0d0,469900.0d0,474100.0d0,478400.0d0,482600.0d0,486800.0d0,491100.0d0,495300.0d0,499600.0d0,        &
      503800.0d0,508100.0d0,512300.0d0,516600.0d0,520800.0d0,525100.0d0,529300.0d0,533600.0d0,537900.0d0,542100.0d0,        &
      546400.0d0,554900.0d0,563500.0d0,572000.0d0,580600.0d0,589200.0d0,597700.0d0,606300.0d0,614900.0d0,623600.0d0,        &
      632200.0d0,640800.0d0,649500.0d0,658100.0d0,666800.0d0,675500.0d0,684200.0d0,692900.0d0,701600.0d0,710300.0d0,        &
      719100.0d0,727800.0d0,736600.0d0,745400.0d0,754200.0d0,763100.0d0,785200.0d0,807400.0d0,829800.0d0,852300.0d0,        &
      874900.0d0,897600.0d0,920500.0d0,943600.0d0,966800.0d0,990200.0d0,1038000.0d0,1086000.0d0,1135000.0d0,1185000.0d0,    &
      1237000.0d0,1290000.0d0,1345000.0d0,1402000.0d0,1462000.0d0,1526000.0d0,1595000.0d0,1671000.0d0,1762000.0d0,1891000.0d0/

  DATA (DefaultSteamEnthalpyGasFluidData(I),I=1,DefaultNumSteamTemps)  &
     /2501000.0d0,2503000.0d0,2510000.0d0,2519000.0d0,2528000.0d0,2537000.0d0,2547000.0d0,2556000.0d0,2565000.0d0,          &
      2574000.0d0,2582000.0d0,2591000.0d0,2600000.0d0,2609000.0d0,2618000.0d0,2626000.0d0,2630000.0d0,2633000.0d0,          &
      2636000.0d0,2640000.0d0,2643000.0d0,2646000.0d0,2650000.0d0,2653000.0d0,2656000.0d0,2660000.0d0,2663000.0d0,          &
      2666000.0d0,2669000.0d0,2672000.0d0,2674000.0d0,2676000.0d0,2677000.0d0,2679000.0d0,2680000.0d0,2682000.0d0,          &
      2683000.0d0,2685000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2691000.0d0,2693000.0d0,2694000.0d0,2696000.0d0,          &
      2697000.0d0,2699000.0d0,2700000.0d0,2702000.0d0,2703000.0d0,2704000.0d0,2706000.0d0,2707000.0d0,2709000.0d0,          &
      2710000.0d0,2712000.0d0,2713000.0d0,2715000.0d0,2716000.0d0,2717000.0d0,2719000.0d0,2720000.0d0,2723000.0d0,          &
      2726000.0d0,2728000.0d0,2731000.0d0,2733000.0d0,2736000.0d0,2739000.0d0,2741000.0d0,2744000.0d0,2746000.0d0,          &
      2748000.0d0,2751000.0d0,2753000.0d0,2755000.0d0,2757000.0d0,2760000.0d0,2762000.0d0,2764000.0d0,2766000.0d0,          &
      2768000.0d0,2770000.0d0,2772000.0d0,2774000.0d0,2775000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,          &
      2792000.0d0,2795000.0d0,2797000.0d0,2799000.0d0,2801000.0d0,2802000.0d0,2803000.0d0,2803000.0d0,2801000.0d0,          &
      2797000.0d0,2790000.0d0,2780000.0d0,2767000.0d0,2750000.0d0,2728000.0d0,2701000.0d0,2666000.0d0,2622000.0d0,          &
      2564000.0d0,2481000.0d0,2335000.0d0/

  DATA (DefaultSteamCpFluidData(I),I=1,DefaultNumSteamTemps)  &
     /4220.0d0,4217.0d0,4205.0d0,4196.0d0,4189.0d0,4184.0d0,4182.0d0,4180.0d0,4180.0d0,4180.0d0,4180.0d0,4182.0d0,          &
      4183.0d0,4185.0d0,4187.0d0,4190.0d0,4191.0d0,4193.0d0,4194.0d0,4195.0d0,4197.0d0,4198.0d0,4200.0d0,4202.0d0,          &
      4203.0d0,4205.0d0,4207.0d0,4209.0d0,4211.0d0,4213.0d0,4215.0d0,4216.0d0,4217.0d0,4218.0d0,4219.0d0,4220.0d0,          &
      4222.0d0,4223.0d0,4224.0d0,4226.0d0,4227.0d0,4228.0d0,4230.0d0,4231.0d0,4233.0d0,4234.0d0,4236.0d0,4237.0d0,          &
      4239.0d0,4240.0d0,4242.0d0,4244.0d0,4245.0d0,4247.0d0,4249.0d0,4250.0d0,4252.0d0,4254.0d0,4256.0d0,4258.0d0,          &
      4260.0d0,4261.0d0,4265.0d0,4270.0d0,4274.0d0,4278.0d0,4283.0d0,4287.0d0,4292.0d0,4297.0d0,4302.0d0,4307.0d0,          &
      4312.0d0,4318.0d0,4324.0d0,4329.0d0,4335.0d0,4341.0d0,4348.0d0,4354.0d0,4361.0d0,4368.0d0,4375.0d0,4382.0d0,          &
      4390.0d0,4397.0d0,4405.0d0,4425.0d0,4447.0d0,4471.0d0,4496.0d0,4523.0d0,4551.0d0,4582.0d0,4615.0d0,4650.0d0,          &
      4688.0d0,4772.0d0,4870.0d0,4986.0d0,5123.0d0,5289.0d0,5493.0d0,5750.0d0,6085.0d0,6537.0d0,7186.0d0,8208.0d0,          &
      10120.0d0,15000.0d0,45160.0d0/

  DATA (DefaultSteamCpGasFluidData(I),I=1,DefaultNumSteamTemps)  &
     /1884.0d0,1885.0d0,1889.0d0,1895.0d0,1900.0d0,1906.0d0,1912.0d0,1918.0d0,1925.0d0,1931.0d0,1939.0d0,1947.0d0,          &
      1955.0d0,1965.0d0,1975.0d0,1986.0d0,1991.0d0,1996.0d0,2001.0d0,2006.0d0,2012.0d0,2018.0d0,2024.0d0,2030.0d0,          &
      2036.0d0,2043.0d0,2050.0d0,2057.0d0,2064.0d0,2072.0d0,2076.0d0,2080.0d0,2084.0d0,2088.0d0,2093.0d0,2097.0d0,          &
      2101.0d0,2106.0d0,2110.0d0,2115.0d0,2120.0d0,2124.0d0,2129.0d0,2134.0d0,2139.0d0,2144.0d0,2150.0d0,2155.0d0,          &
      2160.0d0,2166.0d0,2171.0d0,2177.0d0,2183.0d0,2189.0d0,2195.0d0,2201.0d0,2207.0d0,2213.0d0,2219.0d0,2226.0d0,          &
      2232.0d0,2239.0d0,2252.0d0,2266.0d0,2281.0d0,2296.0d0,2311.0d0,2327.0d0,2343.0d0,2359.0d0,2376.0d0,2394.0d0,          &
      2412.0d0,2430.0d0,2449.0d0,2468.0d0,2488.0d0,2509.0d0,2529.0d0,2551.0d0,2572.0d0,2594.0d0,2617.0d0,2640.0d0,          &
      2664.0d0,2688.0d0,2713.0d0,2777.0d0,2844.0d0,2915.0d0,2990.0d0,3068.0d0,3150.0d0,3237.0d0,3329.0d0,3426.0d0,          &
      3528.0d0,3754.0d0,4011.0d0,4308.0d0,4656.0d0,5073.0d0,5582.0d0,6220.0d0,7045.0d0,8159.0d0,9753.0d0,12240.0d0,         &
      16690.0d0,27360.0d0,96600.0d0/

  DATA (DefaultSteamDensityFluidData(I),I=1,DefaultNumSteamTemps)  &
     /999.8d0,999.9d0,999.9d0,999.7d0,999.1d0,998.2d0,997.0d0,995.6d0,994.0d0,992.2d0,990.2d0,988.0d0,985.7d0,983.2d0,      &
      980.5d0,977.7d0,976.6d0,975.4d0,974.2d0,973.0d0,971.8d0,970.5d0,969.2d0,967.9d0,966.6d0,965.3d0,963.9d0,962.6d0,      &
      961.2d0,959.8d0,959.1d0,958.3d0,957.6d0,956.9d0,956.2d0,955.4d0,954.7d0,954.0d0,953.2d0,952.5d0,951.7d0,950.9d0,      &
      950.2d0,949.4d0,948.6d0,947.9d0,947.1d0,946.3d0,945.5d0,944.7d0,943.9d0,943.1d0,942.3d0,941.5d0,940.7d0,939.8d0,      &
      939.0d0,938.2d0,937.4d0,936.5d0,935.7d0,934.8d0,933.1d0,931.4d0,929.7d0,927.9d0,926.1d0,924.3d0,922.5d0,920.7d0,      &
      918.9d0,917.0d0,915.1d0,913.2d0,911.3d0,909.4d0,907.4d0,905.5d0,903.5d0,901.5d0,899.5d0,897.5d0,895.4d0,893.3d0,      &
      891.2d0,889.1d0,887.0d0,881.6d0,876.1d0,870.4d0,864.7d0,858.8d0,852.7d0,846.5d0,840.2d0,833.7d0,827.1d0,813.4d0,      &
      798.9d0,783.6d0,767.5d0,750.3d0,731.9d0,712.1d0,690.7d0,667.1d0,640.8d0,610.7d0,574.7d0,527.6d0,451.4d0/

  DATA (DefaultSteamDensityGasFluidData(I),I=1,DefaultNumSteamTemps)  &
     /4.86d-003,5.20d-003,6.80d-003,9.41d-003,1.28d-002,1.73d-002,2.31d-002,3.04d-002,3.97d-002,5.12d-002,6.56d-002,        &
      8.32d-002,0.10d0,0.13d0,0.16d0,0.20d0,0.22d0,0.23d0,0.25d0,0.27d0,0.29d0,0.32d0,0.34d0,0.37d0,0.39d0,0.42d0,0.45d0,   &
      0.49d0,0.52d0,0.56d0,0.58d0,0.60d0,0.62d0,0.64d0,0.66d0,0.68d0,0.71d0,0.73d0,0.75d0,0.78d0,0.80d0,0.83d0,0.85d0,      &
      0.88d0,0.91d0,0.94d0,0.97d0,1.00d0,1.03d0,1.06d0,1.09d0,1.12d0,1.16d0,1.19d0,1.23d0,1.26d0,1.30d0,1.34d0,1.38d0,      &
      1.42d0,1.46d0,1.50d0,1.58d0,1.67d0,1.77d0,1.86d0,1.97d0,2.07d0,2.19d0,2.30d0,2.42d0,2.55d0,2.68d0,2.82d0,2.96d0,      &
      3.11d0,3.26d0,3.42d0,3.59d0,3.76d0,3.94d0,4.12d0,4.32d0,4.52d0,4.72d0,4.94d0,5.16d0,5.75d0,6.40d0,7.10d0,7.86d0,      &
      8.69d0,9.59d0,10.56d0,11.62d0,12.75d0,13.99d0,16.75d0,19.97d0,23.71d0,28.07d0,33.16d0,39.13d0,46.17d0,54.54d0,        &
      64.64d0,77.05d0,92.76d0,113.60d0,143.90d0,201.80d0/

  DATA (DefaultSteamSuperheatedTemps(i),i=1,DefaultNumSteamSuperheatedTemps) &
     /1.00d-002,1.0d0,5.0d0,10.0d0,15.0d0,20.0d0,25.0d0,30.0d0,35.0d0,40.0d0,45.0d0,50.0d0,55.0d0,60.0d0,      &
     65.0d0,70.0d0,72.0d0,74.0d0,76.0d0,78.0d0,80.0d0,82.0d0,84.0d0,86.0d0,88.0d0,90.0d0,92.0d0,94.0d0,        &
     96.0d0,98.0d0,99.0d0,100.0d0,101.0d0,102.0d0,103.0d0,104.0d0,105.0d0,106.0d0,107.0d0,108.0d0,109.0d0,     &
     110.0d0,111.0d0,112.0d0,113.0d0,114.0d0,115.0d0,116.0d0,117.0d0,118.0d0,119.0d0,120.0d0,121.0d0,122.0d0,  &
     123.0d0,124.0d0,125.0d0,126.0d0,127.0d0,128.0d0,129.0d0,130.0d0,132.0d0,134.0d0,136.0d0,138.0d0,140.0d0,  &
     142.0d0,144.0d0,146.0d0,148.0d0,150.0d0,152.0d0,154.0d0,156.0d0,158.0d0,160.0d0,162.0d0,164.0d0,166.0d0,  &
     168.0d0,170.0d0,172.0d0,174.0d0,176.0d0,178.0d0,180.0d0,185.0d0,190.0d0,195.0d0,200.0d0,205.0d0,210.0d0,  &
     215.0d0,220.0d0,225.0d0,230.0d0,240.0d0,250.0d0,260.0d0,270.0d0,280.0d0,290.0d0,300.0d0,310.0d0,320.0d0,  &
     330.0d0,340.0d0,350.0d0,360.0d0,370.0d0,400.0d0,450.0d0,500.0d0/

  DATA (DefaultSteamSuperheatedPressData(i),i=1,DefaultNumSteamSuperheatedTemps) &
     /611.70d0,657.10d0,872.60d0,1228.0d0,1706.0d0,2339.0d0,3170.0d0,4247.0d0,5629.0d0,7385.0d0,9595.0d0,12350.0d0,    &
     15760.0d0,19950.0d0,25040.0d0,31200.0d0,34000.0d0,37010.0d0,40240.0d0,43700.0d0,47410.0d0,51390.0d0,55640.0d0,    &
     60170.0d0,65020.0d0,70180.0d0,75680.0d0,81540.0d0,87770.0d0,94390.0d0,97850.0d0,101400.0d0,105100.0d0,108900.0d0, &
     112800.0d0,116800.0d0,120900.0d0,125100.0d0,129500.0d0,134000.0d0,138600.0d0,143400.0d0,148300.0d0,153300.0d0,    &
     158400.0d0,163700.0d0,169200.0d0,174800.0d0,180500.0d0,186400.0d0,192500.0d0,198700.0d0,205000.0d0,211600.0d0,    &
     218300.0d0,225200.0d0,232200.0d0,239500.0d0,246900.0d0,254500.0d0,262300.0d0,270300.0d0,286800.0d0,304200.0d0,    &
     322400.0d0,341500.0d0,361500.0d0,382500.0d0,404400.0d0,427300.0d0,451200.0d0,476200.0d0,502200.0d0,529500.0d0,    &
     557800.0d0,587400.0d0,618200.0d0,650300.0d0,683700.0d0,718500.0d0,754600.0d0,792200.0d0,831200.0d0,871800.0d0,    &
     913800.0d0,957500.0d0,1003000.0d0,1123000.0d0,1255000.0d0,1399000.0d0,1555000.0d0,1724000.0d0,1908000.0d0,        &
     2106000.0d0,2320000.0d0,2550000.0d0,2797000.0d0,3347000.0d0,3976000.0d0,4692000.0d0,5503000.0d0,6417000.0d0,      &
     7442000.0d0,8588000.0d0,9865000.0d0,11280000.0d0,12860000.0d0,14600000.0d0,16530000.0d0,18670000.0d0,             &
     21040000.0d0,30000000.0d0,35000000.0d0,40000000.0d0/

  DATA (DefaultSteamSuperheatedEnthalpyData(i,1),i=1,DefaultNumSteamSuperheatedTemps)  &
    /2501000.0d0,2503000.0d0,2510000.0d0,2520000.0d0,2529000.0d0,2538000.0d0,2548000.0d0,2557000.0d0,2566000.0d0,2576000.0d0,  &
     2585000.0d0,2595000.0d0,2604000.0d0,2613000.0d0,2623000.0d0,2632000.0d0,2636000.0d0,2640000.0d0,2643000.0d0,  &
     2647000.0d0,2651000.0d0,2655000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2673000.0d0,2677000.0d0,  &
     2681000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,  &
     2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,  &
     2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,  &
     2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2744000.0d0,2746000.0d0,2749000.0d0,2753000.0d0,  &
     2757000.0d0,2761000.0d0,2765000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,  &
     2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,  &
     2826000.0d0,2830000.0d0,2834000.0d0,2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,  &
     2890000.0d0,2899000.0d0,2909000.0d0,2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,  &
     3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,  &
     3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,2),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,2503000.0d0,2510000.0d0,2520000.0d0,2529000.0d0,2538000.0d0,2548000.0d0,2557000.0d0,2566000.0d0,2576000.0d0,  &
     2585000.0d0,2595000.0d0,2604000.0d0,2613000.0d0,2623000.0d0,2632000.0d0,2636000.0d0,2640000.0d0,2643000.0d0,  &
     2647000.0d0,2651000.0d0,2655000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2673000.0d0,2677000.0d0,  &
     2681000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,  &
     2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,  &
     2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,  &
     2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2744000.0d0,2746000.0d0,2749000.0d0,2753000.0d0,  &
     2757000.0d0,2761000.0d0,2765000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,  &
     2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,  &
     2826000.0d0,2830000.0d0,2834000.0d0,2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,  &
     2890000.0d0,2899000.0d0,2909000.0d0,2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,  &
     3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,  &
     3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,3),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,2510000.0d0,2519000.0d0,2529000.0d0,2538000.0d0,2548000.0d0,2557000.0d0,2566000.0d0,2576000.0d0,2585000.0d0,  &
     2594000.0d0,2604000.0d0,2613000.0d0,2623000.0d0,2632000.0d0,2636000.0d0,2640000.0d0,2643000.0d0,2647000.0d0,  &
     2651000.0d0,2655000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,  &
     2685000.0d0,2687000.0d0,2689000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,  &
     2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,  &
     2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,  &
     2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2744000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,  &
     2761000.0d0,2765000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2791000.0d0,  &
     2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,  &
     2830000.0d0,2834000.0d0,2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,  &
     2899000.0d0,2909000.0d0,2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,  &
     3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,  &
     3218000.0d0,3280000.0d0,3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,4),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,2519000.0d0,2529000.0d0,2538000.0d0,2547000.0d0,2557000.0d0,2566000.0d0,2576000.0d0,2585000.0d0,  &
     2594000.0d0,2604000.0d0,2613000.0d0,2623000.0d0,2632000.0d0,2636000.0d0,2639000.0d0,2643000.0d0,2647000.0d0,  &
     2651000.0d0,2655000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,  &
     2685000.0d0,2687000.0d0,2689000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,  &
     2702000.0d0,2704000.0d0,2706000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,  &
     2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,  &
     2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2744000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,  &
     2761000.0d0,2765000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2787000.0d0,2791000.0d0,  &
     2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,  &
     2830000.0d0,2834000.0d0,2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,  &
     2899000.0d0,2909000.0d0,2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,  &
     3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,  &
     3218000.0d0,3280000.0d0,3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,5),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,2528000.0d0,2538000.0d0,2547000.0d0,2557000.0d0,2566000.0d0,2575000.0d0,2585000.0d0,2594000.0d0,  &
     2604000.0d0,2613000.0d0,2622000.0d0,2632000.0d0,2636000.0d0,2639000.0d0,2643000.0d0,2647000.0d0,2651000.0d0,  &
     2654000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,2685000.0d0,  &
     2687000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,  &
     2704000.0d0,2706000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,  &
     2721000.0d0,2723000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,  &
     2738000.0d0,2740000.0d0,2742000.0d0,2744000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,  &
     2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,  &
     2799000.0d0,2803000.0d0,2807000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,  &
     2834000.0d0,2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,2899000.0d0,  &
     2909000.0d0,2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,  &
     3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,  &
     3280000.0d0,3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,6),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2537000.0d0,2547000.0d0,2556000.0d0,2566000.0d0,2575000.0d0,2585000.0d0,2594000.0d0,  &
     2603000.0d0,2613000.0d0,2622000.0d0,2632000.0d0,2635000.0d0,2639000.0d0,2643000.0d0,2647000.0d0,2651000.0d0,  &
     2654000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2669000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,2685000.0d0,  &
     2687000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,  &
     2704000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,  &
     2721000.0d0,2723000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,  &
     2738000.0d0,2740000.0d0,2742000.0d0,2743000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,  &
     2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,  &
     2799000.0d0,2803000.0d0,2807000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,  &
     2834000.0d0,2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,2899000.0d0,  &
     2909000.0d0,2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,  &
     3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,  &
     3280000.0d0,3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,7),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2547000.0d0,2556000.0d0,2566000.0d0,2575000.0d0,2584000.0d0,2594000.0d0,2603000.0d0,  &
     2613000.0d0,2622000.0d0,2632000.0d0,2635000.0d0,2639000.0d0,2643000.0d0,2647000.0d0,2650000.0d0,2654000.0d0,  &
     2658000.0d0,2662000.0d0,2666000.0d0,2669000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,2685000.0d0,2686000.0d0,  &
     2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2703000.0d0,  &
     2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,  &
     2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,  &
     2740000.0d0,2741000.0d0,2743000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2764000.0d0,  &
     2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,  &
     2803000.0d0,2807000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,2833000.0d0,  &
     2837000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,2899000.0d0,2909000.0d0,  &
     2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,  &
     3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,  &
     3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,8),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2556000.0d0,2565000.0d0,2575000.0d0,2584000.0d0,2594000.0d0,2603000.0d0,  &
     2612000.0d0,2622000.0d0,2631000.0d0,2635000.0d0,2639000.0d0,2643000.0d0,2646000.0d0,2650000.0d0,2654000.0d0,  &
     2658000.0d0,2662000.0d0,2665000.0d0,2669000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,2684000.0d0,2686000.0d0,  &
     2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2701000.0d0,2703000.0d0,  &
     2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2720000.0d0,  &
     2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,  &
     2739000.0d0,2741000.0d0,2743000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2760000.0d0,2764000.0d0,  &
     2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,  &
     2803000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,2833000.0d0,  &
     2837000.0d0,2841000.0d0,2851000.0d0,2860000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,2899000.0d0,2909000.0d0,  &
     2919000.0d0,2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,  &
     3077000.0d0,3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,  &
     3384000.0d0,3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,9),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2565000.0d0,2574000.0d0,2584000.0d0,2593000.0d0,2603000.0d0,2612000.0d0,  &
     2622000.0d0,2631000.0d0,2635000.0d0,2639000.0d0,2642000.0d0,2646000.0d0,2650000.0d0,2654000.0d0,2658000.0d0,  &
     2661000.0d0,2665000.0d0,2669000.0d0,2673000.0d0,2677000.0d0,2680000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,  &
     2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,  &
     2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,  &
     2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2737000.0d0,2739000.0d0,  &
     2741000.0d0,2743000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,  &
     2772000.0d0,2776000.0d0,2780000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,  &
     2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,  &
     2841000.0d0,2851000.0d0,2860000.0d0,2870000.0d0,2880000.0d0,2890000.0d0,2899000.0d0,2909000.0d0,2919000.0d0,  &
     2929000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,  &
     3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,10),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2574000.0d0,2583000.0d0,2593000.0d0,2602000.0d0,2612000.0d0,  &
     2621000.0d0,2631000.0d0,2635000.0d0,2638000.0d0,2642000.0d0,2646000.0d0,2650000.0d0,2654000.0d0,2657000.0d0,  &
     2661000.0d0,2665000.0d0,2669000.0d0,2673000.0d0,2676000.0d0,2680000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,  &
     2690000.0d0,2692000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,  &
     2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,  &
     2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,  &
     2741000.0d0,2743000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,  &
     2772000.0d0,2776000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2802000.0d0,  &
     2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,  &
     2841000.0d0,2851000.0d0,2860000.0d0,2870000.0d0,2880000.0d0,2889000.0d0,2899000.0d0,2909000.0d0,2919000.0d0,  &
     2928000.0d0,2938000.0d0,2958000.0d0,2978000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,  &
     3097000.0d0,3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,11),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2582000.0d0,2592000.0d0,2602000.0d0,2611000.0d0,2621000.0d0,  &
     2630000.0d0,2634000.0d0,2638000.0d0,2642000.0d0,2646000.0d0,2649000.0d0,2653000.0d0,2657000.0d0,2661000.0d0,  &
     2665000.0d0,2668000.0d0,2672000.0d0,2676000.0d0,2680000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2689000.0d0,  &
     2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,  &
     2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,  &
     2726000.0d0,2728000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,2741000.0d0,  &
     2743000.0d0,2745000.0d0,2749000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,  &
     2775000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,  &
     2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,2841000.0d0,  &
     2851000.0d0,2860000.0d0,2870000.0d0,2880000.0d0,2889000.0d0,2899000.0d0,2909000.0d0,2919000.0d0,2928000.0d0,  &
     2938000.0d0,2958000.0d0,2977000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,  &
     3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,12),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2591000.0d0,2601000.0d0,2611000.0d0,2620000.0d0,  &
     2630000.0d0,2634000.0d0,2637000.0d0,2641000.0d0,2645000.0d0,2649000.0d0,2653000.0d0,2657000.0d0,2660000.0d0,  &
     2664000.0d0,2668000.0d0,2672000.0d0,2676000.0d0,2680000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,  &
     2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,  &
     2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2723000.0d0,  &
     2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,2741000.0d0,  &
     2743000.0d0,2745000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2771000.0d0,  &
     2775000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,  &
     2810000.0d0,2814000.0d0,2818000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,2841000.0d0,  &
     2850000.0d0,2860000.0d0,2870000.0d0,2879000.0d0,2889000.0d0,2899000.0d0,2909000.0d0,2918000.0d0,2928000.0d0,  &
     2938000.0d0,2958000.0d0,2977000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,  &
     3117000.0d0,3137000.0d0,3157000.0d0,3178000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,13),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2600000.0d0,2610000.0d0,2620000.0d0,2629000.0d0,  &
     2633000.0d0,2637000.0d0,2641000.0d0,2645000.0d0,2648000.0d0,2652000.0d0,2656000.0d0,2660000.0d0,2664000.0d0,  &
     2668000.0d0,2671000.0d0,2675000.0d0,2679000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,  &
     2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,  &
     2710000.0d0,2712000.0d0,2714000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,  &
     2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,  &
     2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,  &
     2779000.0d0,2783000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,  &
     2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,2841000.0d0,2850000.0d0,  &
     2860000.0d0,2870000.0d0,2879000.0d0,2889000.0d0,2899000.0d0,2909000.0d0,2918000.0d0,2928000.0d0,2938000.0d0,  &
     2958000.0d0,2977000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,3077000.0d0,3097000.0d0,3117000.0d0,  &
     3137000.0d0,3157000.0d0,3177000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,14),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2609000.0d0,2619000.0d0,2628000.0d0,  &
     2632000.0d0,2636000.0d0,2640000.0d0,2644000.0d0,2648000.0d0,2652000.0d0,2655000.0d0,2659000.0d0,2663000.0d0,  &
     2667000.0d0,2671000.0d0,2675000.0d0,2679000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,  &
     2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2705000.0d0,2707000.0d0,  &
     2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,  &
     2727000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,  &
     2744000.0d0,2748000.0d0,2752000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,  &
     2778000.0d0,2782000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2805000.0d0,2809000.0d0,  &
     2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,2836000.0d0,2840000.0d0,2850000.0d0,  &
     2860000.0d0,2869000.0d0,2879000.0d0,2889000.0d0,2899000.0d0,2908000.0d0,2918000.0d0,2928000.0d0,2938000.0d0,  &
     2957000.0d0,2977000.0d0,2997000.0d0,3017000.0d0,3037000.0d0,3057000.0d0,3076000.0d0,3097000.0d0,3117000.0d0,  &
     3137000.0d0,3157000.0d0,3177000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,15),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2618000.0d0,2627000.0d0,2631000.0d0,  &
     2635000.0d0,2639000.0d0,2643000.0d0,2647000.0d0,2651000.0d0,2655000.0d0,2659000.0d0,2662000.0d0,2666000.0d0,  &
     2670000.0d0,2674000.0d0,2678000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2689000.0d0,2691000.0d0,  &
     2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,  &
     2711000.0d0,2713000.0d0,2715000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,  &
     2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2741000.0d0,2743000.0d0,  &
     2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,2767000.0d0,2770000.0d0,2774000.0d0,2778000.0d0,  &
     2782000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,  &
     2817000.0d0,2821000.0d0,2825000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2840000.0d0,2850000.0d0,2859000.0d0,  &
     2869000.0d0,2879000.0d0,2889000.0d0,2898000.0d0,2908000.0d0,2918000.0d0,2928000.0d0,2938000.0d0,2957000.0d0,  &
     2977000.0d0,2997000.0d0,3017000.0d0,3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3117000.0d0,3137000.0d0,  &
     3157000.0d0,3177000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3490000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,16),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2626000.0d0,2630000.0d0,  &
     2634000.0d0,2638000.0d0,2642000.0d0,2646000.0d0,2650000.0d0,2654000.0d0,2658000.0d0,2661000.0d0,2665000.0d0,  &
     2669000.0d0,2673000.0d0,2677000.0d0,2681000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,  &
     2693000.0d0,2695000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,  &
     2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2725000.0d0,  &
     2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,2741000.0d0,2743000.0d0,  &
     2747000.0d0,2751000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2774000.0d0,2778000.0d0,  &
     2782000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,  &
     2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2840000.0d0,2849000.0d0,2859000.0d0,  &
     2869000.0d0,2879000.0d0,2888000.0d0,2898000.0d0,2908000.0d0,2918000.0d0,2928000.0d0,2937000.0d0,2957000.0d0,  &
     2977000.0d0,2997000.0d0,3016000.0d0,3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3137000.0d0,  &
     3157000.0d0,3177000.0d0,3198000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,17),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2630000.0d0,2633000.0d0,  &
     2637000.0d0,2641000.0d0,2645000.0d0,2649000.0d0,2653000.0d0,2657000.0d0,2661000.0d0,2665000.0d0,2669000.0d0,  &
     2673000.0d0,2677000.0d0,2681000.0d0,2683000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,  &
     2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,  &
     2712000.0d0,2714000.0d0,2716000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,  &
     2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,2741000.0d0,2743000.0d0,2747000.0d0,  &
     2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2774000.0d0,2777000.0d0,2781000.0d0,  &
     2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2805000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,  &
     2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2840000.0d0,2849000.0d0,2859000.0d0,2869000.0d0,  &
     2879000.0d0,2888000.0d0,2898000.0d0,2908000.0d0,2918000.0d0,2927000.0d0,2937000.0d0,2957000.0d0,2977000.0d0,  &
     2996000.0d0,3016000.0d0,3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3137000.0d0,3157000.0d0,  &
     3177000.0d0,3197000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,  &
     3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,18),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2633000.0d0,  &
     2637000.0d0,2641000.0d0,2645000.0d0,2649000.0d0,2653000.0d0,2657000.0d0,2661000.0d0,2665000.0d0,2668000.0d0,  &
     2672000.0d0,2676000.0d0,2680000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,  &
     2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2707000.0d0,2709000.0d0,  &
     2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,  &
     2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2746000.0d0,  &
     2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,  &
     2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,  &
     2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2835000.0d0,2839000.0d0,2849000.0d0,2859000.0d0,2869000.0d0,  &
     2878000.0d0,2888000.0d0,2898000.0d0,2908000.0d0,2918000.0d0,2927000.0d0,2937000.0d0,2957000.0d0,2977000.0d0,  &
     2996000.0d0,3016000.0d0,3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3157000.0d0,  &
     3177000.0d0,3197000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,19),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2636000.0d0,  &
     2640000.0d0,2644000.0d0,2648000.0d0,2652000.0d0,2656000.0d0,2660000.0d0,2664000.0d0,2668000.0d0,2672000.0d0,  &
     2676000.0d0,2680000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2691000.0d0,2693000.0d0,  &
     2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,  &
     2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2728000.0d0,  &
     2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,  &
     2754000.0d0,2758000.0d0,2762000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,  &
     2789000.0d0,2793000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,  &
     2824000.0d0,2828000.0d0,2831000.0d0,2835000.0d0,2839000.0d0,2849000.0d0,2859000.0d0,2868000.0d0,2878000.0d0,  &
     2888000.0d0,2898000.0d0,2908000.0d0,2917000.0d0,2927000.0d0,2937000.0d0,2957000.0d0,2976000.0d0,2996000.0d0,  &
     3016000.0d0,3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3157000.0d0,3177000.0d0,  &
     3197000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,20),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2640000.0d0,2644000.0d0,2648000.0d0,2652000.0d0,2656000.0d0,2660000.0d0,2664000.0d0,2667000.0d0,2671000.0d0,  &
     2675000.0d0,2679000.0d0,2681000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,  &
     2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,  &
     2713000.0d0,2715000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,  &
     2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,  &
     2753000.0d0,2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,  &
     2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,  &
     2823000.0d0,2827000.0d0,2831000.0d0,2835000.0d0,2839000.0d0,2849000.0d0,2859000.0d0,2868000.0d0,2878000.0d0,  &
     2888000.0d0,2898000.0d0,2907000.0d0,2917000.0d0,2927000.0d0,2937000.0d0,2957000.0d0,2976000.0d0,2996000.0d0,  &
     3016000.0d0,3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3157000.0d0,3177000.0d0,  &
     3197000.0d0,3218000.0d0,3280000.0d0,3384000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,21),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2643000.0d0,2647000.0d0,2651000.0d0,2655000.0d0,2659000.0d0,2663000.0d0,2667000.0d0,2671000.0d0,2675000.0d0,  &
     2679000.0d0,2681000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,  &
     2697000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,  &
     2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,  &
     2732000.0d0,2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,  &
     2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,  &
     2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,  &
     2827000.0d0,2831000.0d0,2835000.0d0,2839000.0d0,2849000.0d0,2858000.0d0,2868000.0d0,2878000.0d0,2888000.0d0,  &
     2897000.0d0,2907000.0d0,2917000.0d0,2927000.0d0,2937000.0d0,2956000.0d0,2976000.0d0,2996000.0d0,3016000.0d0,  &
     3036000.0d0,3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3157000.0d0,3177000.0d0,3197000.0d0,  &
     3218000.0d0,3280000.0d0,3384000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,22),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,2646000.0d0,2650000.0d0,2654000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2674000.0d0,2678000.0d0,  &
     2680000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,  &
     2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,  &
     2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,  &
     2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,  &
     2761000.0d0,2765000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,  &
     2796000.0d0,2800000.0d0,2804000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,2827000.0d0,  &
     2831000.0d0,2835000.0d0,2839000.0d0,2848000.0d0,2858000.0d0,2868000.0d0,2878000.0d0,2887000.0d0,2897000.0d0,  &
     2907000.0d0,2917000.0d0,2927000.0d0,2937000.0d0,2956000.0d0,2976000.0d0,2996000.0d0,3016000.0d0,3036000.0d0,  &
     3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3156000.0d0,3177000.0d0,3197000.0d0,3218000.0d0,  &
     3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,23),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,2650000.0d0,2654000.0d0,2658000.0d0,2662000.0d0,2666000.0d0,2670000.0d0,2674000.0d0,2678000.0d0,  &
     2680000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,  &
     2698000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,  &
     2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,  &
     2733000.0d0,2735000.0d0,2737000.0d0,2739000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2752000.0d0,2756000.0d0,  &
     2760000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,  &
     2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,2827000.0d0,  &
     2831000.0d0,2834000.0d0,2838000.0d0,2848000.0d0,2858000.0d0,2868000.0d0,2878000.0d0,2887000.0d0,2897000.0d0,  &
     2907000.0d0,2917000.0d0,2927000.0d0,2936000.0d0,2956000.0d0,2976000.0d0,2996000.0d0,3016000.0d0,3036000.0d0,  &
     3056000.0d0,3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3156000.0d0,3177000.0d0,3197000.0d0,3218000.0d0,  &
     3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,24),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,2653000.0d0,2657000.0d0,2661000.0d0,2665000.0d0,2669000.0d0,2673000.0d0,2677000.0d0,2679000.0d0,  &
     2681000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,  &
     2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,  &
     2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2732000.0d0,  &
     2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,  &
     2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,  &
     2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,  &
     2834000.0d0,2838000.0d0,2848000.0d0,2858000.0d0,2867000.0d0,2877000.0d0,2887000.0d0,2897000.0d0,2907000.0d0,  &
     2917000.0d0,2926000.0d0,2936000.0d0,2956000.0d0,2976000.0d0,2996000.0d0,3016000.0d0,3035000.0d0,3055000.0d0,  &
     3076000.0d0,3096000.0d0,3116000.0d0,3136000.0d0,3156000.0d0,3177000.0d0,3197000.0d0,3217000.0d0,3280000.0d0,  &
     3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,25),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,2656000.0d0,2660000.0d0,2664000.0d0,2668000.0d0,2672000.0d0,2676000.0d0,2678000.0d0,  &
     2680000.0d0,2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,  &
     2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,  &
     2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,  &
     2734000.0d0,2736000.0d0,2738000.0d0,2740000.0d0,2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,  &
     2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,  &
     2799000.0d0,2803000.0d0,2807000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,  &
     2834000.0d0,2838000.0d0,2848000.0d0,2857000.0d0,2867000.0d0,2877000.0d0,2887000.0d0,2897000.0d0,2907000.0d0,  &
     2916000.0d0,2926000.0d0,2936000.0d0,2956000.0d0,2976000.0d0,2996000.0d0,3015000.0d0,3035000.0d0,3055000.0d0,  &
     3075000.0d0,3095000.0d0,3116000.0d0,3136000.0d0,3156000.0d0,3176000.0d0,3197000.0d0,3217000.0d0,3280000.0d0,  &
     3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,26),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2660000.0d0,2664000.0d0,2668000.0d0,2672000.0d0,2676000.0d0,2678000.0d0,2680000.0d0,  &
     2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,  &
     2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,  &
     2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,  &
     2735000.0d0,2737000.0d0,2739000.0d0,2743000.0d0,2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,  &
     2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2794000.0d0,2798000.0d0,  &
     2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,2834000.0d0,  &
     2838000.0d0,2847000.0d0,2857000.0d0,2867000.0d0,2877000.0d0,2887000.0d0,2896000.0d0,2906000.0d0,2916000.0d0,  &
     2926000.0d0,2936000.0d0,2956000.0d0,2975000.0d0,2995000.0d0,3015000.0d0,3035000.0d0,3055000.0d0,3075000.0d0,  &
     3095000.0d0,3116000.0d0,3136000.0d0,3156000.0d0,3176000.0d0,3197000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,  &
     3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,27),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2663000.0d0,2667000.0d0,2671000.0d0,2675000.0d0,2677000.0d0,2679000.0d0,  &
     2681000.0d0,2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,  &
     2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,  &
     2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,  &
     2735000.0d0,2737000.0d0,2739000.0d0,2743000.0d0,2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,  &
     2767000.0d0,2771000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,  &
     2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2829000.0d0,2833000.0d0,  &
     2837000.0d0,2847000.0d0,2857000.0d0,2867000.0d0,2877000.0d0,2886000.0d0,2896000.0d0,2906000.0d0,2916000.0d0,  &
     2926000.0d0,2936000.0d0,2955000.0d0,2975000.0d0,2995000.0d0,3015000.0d0,3035000.0d0,3055000.0d0,3075000.0d0,  &
     3095000.0d0,3115000.0d0,3136000.0d0,3156000.0d0,3176000.0d0,3197000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,  &
     3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,28),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2666000.0d0,2670000.0d0,2674000.0d0,2676000.0d0,2678000.0d0,2680000.0d0,  &
     2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2699000.0d0,  &
     2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,  &
     2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,  &
     2736000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,  &
     2770000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,  &
     2806000.0d0,2809000.0d0,2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,  &
     2847000.0d0,2857000.0d0,2866000.0d0,2876000.0d0,2886000.0d0,2896000.0d0,2906000.0d0,2916000.0d0,2926000.0d0,  &
     2935000.0d0,2955000.0d0,2975000.0d0,2995000.0d0,3015000.0d0,3035000.0d0,3055000.0d0,3075000.0d0,3095000.0d0,  &
     3115000.0d0,3136000.0d0,3156000.0d0,3176000.0d0,3197000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,29),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2669000.0d0,2673000.0d0,2675000.0d0,2677000.0d0,2679000.0d0,  &
     2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,  &
     2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,  &
     2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,  &
     2736000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,  &
     2770000.0d0,2774000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,  &
     2805000.0d0,2809000.0d0,2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,2837000.0d0,  &
     2846000.0d0,2856000.0d0,2866000.0d0,2876000.0d0,2886000.0d0,2896000.0d0,2906000.0d0,2915000.0d0,2925000.0d0,  &
     2935000.0d0,2955000.0d0,2975000.0d0,2995000.0d0,3015000.0d0,3035000.0d0,3055000.0d0,3075000.0d0,3095000.0d0,  &
     3115000.0d0,3135000.0d0,3156000.0d0,3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,30),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2672000.0d0,2674000.0d0,2677000.0d0,2679000.0d0,2681000.0d0,  &
     2683000.0d0,2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,  &
     2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,  &
     2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,  &
     2737000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,  &
     2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2805000.0d0,  &
     2809000.0d0,2813000.0d0,2817000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2846000.0d0,  &
     2856000.0d0,2866000.0d0,2876000.0d0,2886000.0d0,2895000.0d0,2905000.0d0,2915000.0d0,2925000.0d0,2935000.0d0,  &
     2955000.0d0,2975000.0d0,2995000.0d0,3015000.0d0,3035000.0d0,3055000.0d0,3075000.0d0,3095000.0d0,3115000.0d0,  &
     3135000.0d0,3156000.0d0,3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,31),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2674000.0d0,2676000.0d0,2678000.0d0,2680000.0d0,  &
     2682000.0d0,2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,  &
     2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,  &
     2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,  &
     2737000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,  &
     2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2804000.0d0,  &
     2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2846000.0d0,  &
     2856000.0d0,2866000.0d0,2876000.0d0,2885000.0d0,2895000.0d0,2905000.0d0,2915000.0d0,2925000.0d0,2935000.0d0,  &
     2955000.0d0,2975000.0d0,2994000.0d0,3014000.0d0,3034000.0d0,3054000.0d0,3075000.0d0,3095000.0d0,3115000.0d0,  &
     3135000.0d0,3156000.0d0,3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,32),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2676000.0d0,2678000.0d0,2680000.0d0,2682000.0d0,  &
     2684000.0d0,2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,  &
     2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2719000.0d0,  &
     2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2737000.0d0,  &
     2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,  &
     2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,  &
     2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2846000.0d0,2856000.0d0,  &
     2866000.0d0,2875000.0d0,2885000.0d0,2895000.0d0,2905000.0d0,2915000.0d0,2925000.0d0,2935000.0d0,2955000.0d0,  &
     2974000.0d0,2994000.0d0,3014000.0d0,3034000.0d0,3054000.0d0,3074000.0d0,3095000.0d0,3115000.0d0,3135000.0d0,  &
     3155000.0d0,3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,33),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2677000.0d0,2679000.0d0,2681000.0d0,  &
     2683000.0d0,2685000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,  &
     2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,  &
     2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,  &
     2740000.0d0,2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,  &
     2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,  &
     2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2836000.0d0,2846000.0d0,2855000.0d0,  &
     2865000.0d0,2875000.0d0,2885000.0d0,2895000.0d0,2905000.0d0,2915000.0d0,2925000.0d0,2935000.0d0,2954000.0d0,  &
     2974000.0d0,2994000.0d0,3014000.0d0,3034000.0d0,3054000.0d0,3074000.0d0,3095000.0d0,3115000.0d0,3135000.0d0,  &
     3155000.0d0,3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,34),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2679000.0d0,2681000.0d0,2683000.0d0,  &
     2685000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,  &
     2703000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,  &
     2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2740000.0d0,  &
     2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,  &
     2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,  &
     2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2835000.0d0,2845000.0d0,2855000.0d0,2865000.0d0,  &
     2875000.0d0,2885000.0d0,2895000.0d0,2905000.0d0,2915000.0d0,2925000.0d0,2934000.0d0,2954000.0d0,2974000.0d0,  &
     2994000.0d0,3014000.0d0,3034000.0d0,3054000.0d0,3074000.0d0,3094000.0d0,3115000.0d0,3135000.0d0,3155000.0d0,  &
     3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,35),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2680000.0d0,2682000.0d0,  &
     2684000.0d0,2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,  &
     2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,  &
     2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2736000.0d0,2740000.0d0,  &
     2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,  &
     2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2807000.0d0,2811000.0d0,  &
     2815000.0d0,2819000.0d0,2823000.0d0,2827000.0d0,2831000.0d0,2835000.0d0,2845000.0d0,2855000.0d0,2865000.0d0,  &
     2875000.0d0,2885000.0d0,2895000.0d0,2905000.0d0,2914000.0d0,2924000.0d0,2934000.0d0,2954000.0d0,2974000.0d0,  &
     2994000.0d0,3014000.0d0,3034000.0d0,3054000.0d0,3074000.0d0,3094000.0d0,3115000.0d0,3135000.0d0,3155000.0d0,  &
     3176000.0d0,3196000.0d0,3217000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,36),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2682000.0d0,2684000.0d0,  &
     2686000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2701000.0d0,2703000.0d0,  &
     2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,  &
     2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2739000.0d0,2743000.0d0,  &
     2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,  &
     2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,  &
     2819000.0d0,2823000.0d0,2827000.0d0,2831000.0d0,2835000.0d0,2845000.0d0,2855000.0d0,2865000.0d0,2875000.0d0,  &
     2885000.0d0,2894000.0d0,2904000.0d0,2914000.0d0,2924000.0d0,2934000.0d0,2954000.0d0,2974000.0d0,2994000.0d0,  &
     3014000.0d0,3034000.0d0,3054000.0d0,3074000.0d0,3094000.0d0,3115000.0d0,3135000.0d0,3155000.0d0,3176000.0d0,  &
     3196000.0d0,3216000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,37),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2683000.0d0,  &
     2685000.0d0,2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,  &
     2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,  &
     2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2735000.0d0,2739000.0d0,2743000.0d0,  &
     2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,  &
     2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,  &
     2819000.0d0,2823000.0d0,2827000.0d0,2831000.0d0,2835000.0d0,2845000.0d0,2855000.0d0,2865000.0d0,2874000.0d0,  &
     2884000.0d0,2894000.0d0,2904000.0d0,2914000.0d0,2924000.0d0,2934000.0d0,2954000.0d0,2974000.0d0,2994000.0d0,  &
     3014000.0d0,3034000.0d0,3054000.0d0,3074000.0d0,3094000.0d0,3114000.0d0,3135000.0d0,3155000.0d0,3175000.0d0,  &
     3196000.0d0,3216000.0d0,3280000.0d0,3383000.0d0,3489000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,38),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2685000.0d0,  &
     2687000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,  &
     2706000.0d0,2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,  &
     2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2738000.0d0,2743000.0d0,2747000.0d0,  &
     2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,2783000.0d0,  &
     2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,  &
     2823000.0d0,2827000.0d0,2831000.0d0,2835000.0d0,2844000.0d0,2854000.0d0,2864000.0d0,2874000.0d0,2884000.0d0,  &
     2894000.0d0,2904000.0d0,2914000.0d0,2924000.0d0,2934000.0d0,2954000.0d0,2974000.0d0,2994000.0d0,3014000.0d0,  &
     3034000.0d0,3054000.0d0,3074000.0d0,3094000.0d0,3114000.0d0,3135000.0d0,3155000.0d0,3175000.0d0,3196000.0d0,  &
     3216000.0d0,3280000.0d0,3383000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,39),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2686000.0d0,2689000.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,  &
     2705000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,  &
     2724000.0d0,2726000.0d0,2728000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,  &
     2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,  &
     2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,  &
     2822000.0d0,2826000.0d0,2830000.0d0,2834000.0d0,2844000.0d0,2854000.0d0,2864000.0d0,2874000.0d0,2884000.0d0,  &
     2894000.0d0,2904000.0d0,2914000.0d0,2924000.0d0,2934000.0d0,2954000.0d0,2974000.0d0,2994000.0d0,3014000.0d0,  &
     3034000.0d0,3054000.0d0,3074000.0d0,3094000.0d0,3114000.0d0,3135000.0d0,3155000.0d0,3175000.0d0,3196000.0d0,  &
     3216000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,40),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2688000.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,  &
     2707000.0d0,2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,  &
     2725000.0d0,2727000.0d0,2730000.0d0,2732000.0d0,2734000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,  &
     2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2786000.0d0,  &
     2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,  &
     2826000.0d0,2830000.0d0,2834000.0d0,2844000.0d0,2854000.0d0,2864000.0d0,2874000.0d0,2884000.0d0,2894000.0d0,  &
     2904000.0d0,2914000.0d0,2924000.0d0,2934000.0d0,2953000.0d0,2973000.0d0,2993000.0d0,3013000.0d0,3033000.0d0,  &
     3054000.0d0,3074000.0d0,3094000.0d0,3114000.0d0,3134000.0d0,3155000.0d0,3175000.0d0,3196000.0d0,3216000.0d0,  &
     3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,41),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,2690000.0d0,2692000.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,  &
     2708000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,  &
     2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2737000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2754000.0d0,  &
     2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2786000.0d0,2790000.0d0,  &
     2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,  &
     2830000.0d0,2834000.0d0,2844000.0d0,2854000.0d0,2864000.0d0,2874000.0d0,2884000.0d0,2894000.0d0,2903000.0d0,  &
     2913000.0d0,2923000.0d0,2933000.0d0,2953000.0d0,2973000.0d0,2993000.0d0,3013000.0d0,3033000.0d0,3053000.0d0,  &
     3074000.0d0,3094000.0d0,3114000.0d0,3134000.0d0,3155000.0d0,3175000.0d0,3196000.0d0,3216000.0d0,3280000.0d0,  &
     3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,42),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,2691000.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,  &
     2708000.0d0,2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,  &
     2727000.0d0,2729000.0d0,2731000.0d0,2733000.0d0,2737000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,  &
     2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2790000.0d0,  &
     2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,  &
     2830000.0d0,2834000.0d0,2844000.0d0,2853000.0d0,2863000.0d0,2873000.0d0,2883000.0d0,2893000.0d0,2903000.0d0,  &
     2913000.0d0,2923000.0d0,2933000.0d0,2953000.0d0,2973000.0d0,2993000.0d0,3013000.0d0,3033000.0d0,3053000.0d0,  &
     3073000.0d0,3094000.0d0,3114000.0d0,3134000.0d0,3155000.0d0,3175000.0d0,3195000.0d0,3216000.0d0,3280000.0d0,  &
     3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,43),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,2693000.0d0,2695000.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,  &
     2709000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,  &
     2728000.0d0,2730000.0d0,2732000.0d0,2736000.0d0,2740000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,  &
     2761000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,  &
     2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,  &
     2833000.0d0,2843000.0d0,2853000.0d0,2863000.0d0,2873000.0d0,2883000.0d0,2893000.0d0,2903000.0d0,2913000.0d0,  &
     2923000.0d0,2933000.0d0,2953000.0d0,2973000.0d0,2993000.0d0,3013000.0d0,3033000.0d0,3053000.0d0,3073000.0d0,  &
     3094000.0d0,3114000.0d0,3134000.0d0,3154000.0d0,3175000.0d0,3195000.0d0,3216000.0d0,3280000.0d0,3382000.0d0,  &
     3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,44),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,2694000.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,  &
     2709000.0d0,2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2724000.0d0,2726000.0d0,  &
     2728000.0d0,2730000.0d0,2732000.0d0,2736000.0d0,2740000.0d0,2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,  &
     2760000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,  &
     2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,  &
     2833000.0d0,2843000.0d0,2853000.0d0,2863000.0d0,2873000.0d0,2883000.0d0,2893000.0d0,2903000.0d0,2913000.0d0,  &
     2923000.0d0,2933000.0d0,2953000.0d0,2973000.0d0,2993000.0d0,3013000.0d0,3033000.0d0,3053000.0d0,3073000.0d0,  &
     3093000.0d0,3114000.0d0,3134000.0d0,3154000.0d0,3175000.0d0,3195000.0d0,3216000.0d0,3280000.0d0,3382000.0d0,  &
     3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,45),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2696000.0d0,2698000.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,  &
     2710000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,  &
     2729000.0d0,2731000.0d0,2735000.0d0,2740000.0d0,2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,  &
     2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2793000.0d0,2797000.0d0,  &
     2801000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2833000.0d0,  &
     2843000.0d0,2853000.0d0,2863000.0d0,2873000.0d0,2883000.0d0,2893000.0d0,2903000.0d0,2913000.0d0,2923000.0d0,  &
     2933000.0d0,2953000.0d0,2973000.0d0,2993000.0d0,3013000.0d0,3033000.0d0,3053000.0d0,3073000.0d0,3093000.0d0,  &
     3114000.0d0,3134000.0d0,3154000.0d0,3175000.0d0,3195000.0d0,3216000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,46),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2697000.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2706000.0d0,2708000.0d0,  &
     2710000.0d0,2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2725000.0d0,2727000.0d0,  &
     2729000.0d0,2731000.0d0,2735000.0d0,2739000.0d0,2743000.0d0,2747000.0d0,2751000.0d0,2756000.0d0,2760000.0d0,  &
     2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,  &
     2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,  &
     2842000.0d0,2852000.0d0,2862000.0d0,2872000.0d0,2882000.0d0,2892000.0d0,2902000.0d0,2912000.0d0,2922000.0d0,  &
     2932000.0d0,2952000.0d0,2972000.0d0,2992000.0d0,3013000.0d0,3033000.0d0,3053000.0d0,3073000.0d0,3093000.0d0,  &
     3113000.0d0,3134000.0d0,3154000.0d0,3175000.0d0,3195000.0d0,3216000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,47),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2699000.0d0,2701000.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,  &
     2711000.0d0,2713000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,  &
     2730000.0d0,2734000.0d0,2739000.0d0,2743000.0d0,2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,  &
     2767000.0d0,2771000.0d0,2776000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,  &
     2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2842000.0d0,  &
     2852000.0d0,2862000.0d0,2872000.0d0,2882000.0d0,2892000.0d0,2902000.0d0,2912000.0d0,2922000.0d0,2932000.0d0,  &
     2952000.0d0,2972000.0d0,2992000.0d0,3012000.0d0,3032000.0d0,3053000.0d0,3073000.0d0,3093000.0d0,3113000.0d0,  &
     3134000.0d0,3154000.0d0,3174000.0d0,3195000.0d0,3216000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,48),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2700000.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2709000.0d0,  &
     2711000.0d0,2713000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2726000.0d0,2728000.0d0,  &
     2730000.0d0,2734000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,  &
     2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2800000.0d0,  &
     2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2828000.0d0,2832000.0d0,2842000.0d0,  &
     2852000.0d0,2862000.0d0,2872000.0d0,2882000.0d0,2892000.0d0,2902000.0d0,2912000.0d0,2922000.0d0,2932000.0d0,  &
     2952000.0d0,2972000.0d0,2992000.0d0,3012000.0d0,3032000.0d0,3052000.0d0,3073000.0d0,3093000.0d0,3113000.0d0,  &
     3134000.0d0,3154000.0d0,3174000.0d0,3195000.0d0,3215000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,49),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2702000.0d0,2704000.0d0,2706000.0d0,2708000.0d0,2710000.0d0,  &
     2712000.0d0,2714000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2729000.0d0,  &
     2733000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,  &
     2771000.0d0,2775000.0d0,2779000.0d0,2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,  &
     2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,2827000.0d0,2831000.0d0,2842000.0d0,2852000.0d0,  &
     2862000.0d0,2872000.0d0,2882000.0d0,2892000.0d0,2902000.0d0,2912000.0d0,2922000.0d0,2932000.0d0,2952000.0d0,  &
     2972000.0d0,2992000.0d0,3012000.0d0,3032000.0d0,3052000.0d0,3073000.0d0,3093000.0d0,3113000.0d0,3133000.0d0,  &
     3154000.0d0,3174000.0d0,3195000.0d0,3215000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,50),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2703000.0d0,2705000.0d0,2707000.0d0,2709000.0d0,  &
     2712000.0d0,2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2729000.0d0,  &
     2733000.0d0,2737000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,  &
     2770000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,  &
     2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,2827000.0d0,2831000.0d0,2841000.0d0,2851000.0d0,  &
     2861000.0d0,2871000.0d0,2881000.0d0,2891000.0d0,2901000.0d0,2911000.0d0,2922000.0d0,2932000.0d0,2952000.0d0,  &
     2972000.0d0,2992000.0d0,3012000.0d0,3032000.0d0,3052000.0d0,3072000.0d0,3093000.0d0,3113000.0d0,3133000.0d0,  &
     3154000.0d0,3174000.0d0,3195000.0d0,3215000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,51),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2704000.0d0,2707000.0d0,2709000.0d0,2711000.0d0,  &
     2713000.0d0,2715000.0d0,2717000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2728000.0d0,2732000.0d0,  &
     2736000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2766000.0d0,2770000.0d0,  &
     2774000.0d0,2778000.0d0,2782000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,  &
     2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,2827000.0d0,2831000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,  &
     2871000.0d0,2881000.0d0,2891000.0d0,2901000.0d0,2911000.0d0,2921000.0d0,2931000.0d0,2951000.0d0,2971000.0d0,  &
     2992000.0d0,3012000.0d0,3032000.0d0,3052000.0d0,3072000.0d0,3093000.0d0,3113000.0d0,3133000.0d0,3154000.0d0,  &
     3174000.0d0,3195000.0d0,3215000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,52),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2706000.0d0,2708000.0d0,2710000.0d0,  &
     2712000.0d0,2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2727000.0d0,2732000.0d0,  &
     2736000.0d0,2740000.0d0,2744000.0d0,2748000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,  &
     2773000.0d0,2777000.0d0,2782000.0d0,2786000.0d0,2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,  &
     2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,2841000.0d0,2851000.0d0,2861000.0d0,  &
     2871000.0d0,2881000.0d0,2891000.0d0,2901000.0d0,2911000.0d0,2921000.0d0,2931000.0d0,2951000.0d0,2971000.0d0,  &
     2991000.0d0,3012000.0d0,3032000.0d0,3052000.0d0,3072000.0d0,3092000.0d0,3113000.0d0,3133000.0d0,3153000.0d0,  &
     3174000.0d0,3194000.0d0,3215000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,53),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2707000.0d0,2710000.0d0,2712000.0d0,  &
     2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2725000.0d0,2727000.0d0,2731000.0d0,2735000.0d0,  &
     2739000.0d0,2744000.0d0,2748000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,  &
     2777000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,  &
     2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,2840000.0d0,2850000.0d0,2860000.0d0,2870000.0d0,  &
     2881000.0d0,2891000.0d0,2901000.0d0,2911000.0d0,2921000.0d0,2931000.0d0,2951000.0d0,2971000.0d0,2991000.0d0,  &
     3011000.0d0,3031000.0d0,3052000.0d0,3072000.0d0,3092000.0d0,3113000.0d0,3133000.0d0,3153000.0d0,3174000.0d0,  &
     3194000.0d0,3215000.0d0,3280000.0d0,3382000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,54),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2709000.0d0,2711000.0d0,  &
     2713000.0d0,2715000.0d0,2718000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2726000.0d0,2730000.0d0,2735000.0d0,  &
     2739000.0d0,2743000.0d0,2747000.0d0,2752000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,  &
     2776000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,  &
     2813000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,2830000.0d0,2840000.0d0,2850000.0d0,2860000.0d0,2870000.0d0,  &
     2880000.0d0,2890000.0d0,2900000.0d0,2910000.0d0,2921000.0d0,2931000.0d0,2951000.0d0,2971000.0d0,2991000.0d0,  &
     3011000.0d0,3031000.0d0,3052000.0d0,3072000.0d0,3092000.0d0,3112000.0d0,3133000.0d0,3153000.0d0,3174000.0d0,  &
     3194000.0d0,3215000.0d0,3280000.0d0,3381000.0d0,3488000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,55),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2710000.0d0,2712000.0d0,  &
     2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2725000.0d0,2730000.0d0,2734000.0d0,2738000.0d0,  &
     2743000.0d0,2747000.0d0,2751000.0d0,2755000.0d0,2759000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,  &
     2780000.0d0,2784000.0d0,2788000.0d0,2793000.0d0,2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,  &
     2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2839000.0d0,2850000.0d0,2860000.0d0,2870000.0d0,2880000.0d0,  &
     2890000.0d0,2900000.0d0,2910000.0d0,2920000.0d0,2930000.0d0,2950000.0d0,2971000.0d0,2991000.0d0,3011000.0d0,  &
     3031000.0d0,3051000.0d0,3072000.0d0,3092000.0d0,3112000.0d0,3133000.0d0,3153000.0d0,3174000.0d0,3194000.0d0,  &
     3215000.0d0,3280000.0d0,3381000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,56),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2712000.0d0,  &
     2714000.0d0,2716000.0d0,2718000.0d0,2720000.0d0,2723000.0d0,2725000.0d0,2729000.0d0,2733000.0d0,2738000.0d0,  &
     2742000.0d0,2746000.0d0,2750000.0d0,2755000.0d0,2759000.0d0,2763000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,  &
     2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2813000.0d0,  &
     2817000.0d0,2821000.0d0,2825000.0d0,2829000.0d0,2839000.0d0,2849000.0d0,2859000.0d0,2870000.0d0,2880000.0d0,  &
     2890000.0d0,2900000.0d0,2910000.0d0,2920000.0d0,2930000.0d0,2950000.0d0,2970000.0d0,2991000.0d0,3011000.0d0,  &
     3031000.0d0,3051000.0d0,3071000.0d0,3092000.0d0,3112000.0d0,3132000.0d0,3153000.0d0,3173000.0d0,3194000.0d0,  &
     3215000.0d0,3280000.0d0,3381000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,57),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2713000.0d0,  &
     2715000.0d0,2717000.0d0,2720000.0d0,2722000.0d0,2724000.0d0,2728000.0d0,2733000.0d0,2737000.0d0,2741000.0d0,  &
     2746000.0d0,2750000.0d0,2754000.0d0,2758000.0d0,2762000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,  &
     2783000.0d0,2787000.0d0,2792000.0d0,2796000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,  &
     2820000.0d0,2824000.0d0,2829000.0d0,2839000.0d0,2849000.0d0,2859000.0d0,2869000.0d0,2879000.0d0,2889000.0d0,  &
     2900000.0d0,2910000.0d0,2920000.0d0,2930000.0d0,2950000.0d0,2970000.0d0,2990000.0d0,3011000.0d0,3031000.0d0,  &
     3051000.0d0,3071000.0d0,3092000.0d0,3112000.0d0,3132000.0d0,3153000.0d0,3173000.0d0,3194000.0d0,3214000.0d0,  &
     3280000.0d0,3381000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,58),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2715000.0d0,2717000.0d0,2719000.0d0,2721000.0d0,2723000.0d0,2728000.0d0,2732000.0d0,2736000.0d0,2741000.0d0,  &
     2745000.0d0,2749000.0d0,2753000.0d0,2758000.0d0,2762000.0d0,2766000.0d0,2770000.0d0,2774000.0d0,2779000.0d0,  &
     2783000.0d0,2787000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,  &
     2820000.0d0,2824000.0d0,2828000.0d0,2838000.0d0,2849000.0d0,2859000.0d0,2869000.0d0,2879000.0d0,2889000.0d0,  &
     2899000.0d0,2909000.0d0,2919000.0d0,2930000.0d0,2950000.0d0,2970000.0d0,2990000.0d0,3010000.0d0,3031000.0d0,  &
     3051000.0d0,3071000.0d0,3091000.0d0,3112000.0d0,3132000.0d0,3153000.0d0,3173000.0d0,3194000.0d0,3214000.0d0,  &
     3280000.0d0,3381000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,59),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2716000.0d0,2718000.0d0,2720000.0d0,2723000.0d0,2727000.0d0,2731000.0d0,2736000.0d0,2740000.0d0,2744000.0d0,  &
     2748000.0d0,2753000.0d0,2757000.0d0,2761000.0d0,2765000.0d0,2770000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,  &
     2786000.0d0,2791000.0d0,2795000.0d0,2799000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,  &
     2824000.0d0,2828000.0d0,2838000.0d0,2848000.0d0,2858000.0d0,2868000.0d0,2879000.0d0,2889000.0d0,2899000.0d0,  &
     2909000.0d0,2919000.0d0,2929000.0d0,2949000.0d0,2970000.0d0,2990000.0d0,3010000.0d0,3030000.0d0,3051000.0d0,  &
     3071000.0d0,3091000.0d0,3112000.0d0,3132000.0d0,3152000.0d0,3173000.0d0,3194000.0d0,3214000.0d0,3280000.0d0,  &
     3381000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,60),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,2717000.0d0,2720000.0d0,2722000.0d0,2726000.0d0,2731000.0d0,2735000.0d0,2739000.0d0,2744000.0d0,2748000.0d0,  &
     2752000.0d0,2756000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2773000.0d0,2777000.0d0,2782000.0d0,2786000.0d0,  &
     2790000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,  &
     2827000.0d0,2837000.0d0,2848000.0d0,2858000.0d0,2868000.0d0,2878000.0d0,2888000.0d0,2899000.0d0,2909000.0d0,  &
     2919000.0d0,2929000.0d0,2949000.0d0,2969000.0d0,2990000.0d0,3010000.0d0,3030000.0d0,3050000.0d0,3071000.0d0,  &
     3091000.0d0,3111000.0d0,3132000.0d0,3152000.0d0,3173000.0d0,3193000.0d0,3214000.0d0,3280000.0d0,3381000.0d0,  &
     3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,61),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,2719000.0d0,2721000.0d0,2725000.0d0,2730000.0d0,2734000.0d0,2738000.0d0,2743000.0d0,2747000.0d0,  &
     2751000.0d0,2756000.0d0,2760000.0d0,2764000.0d0,2768000.0d0,2773000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,  &
     2789000.0d0,2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2819000.0d0,2823000.0d0,  &
     2827000.0d0,2837000.0d0,2847000.0d0,2858000.0d0,2868000.0d0,2878000.0d0,2888000.0d0,2898000.0d0,2908000.0d0,  &
     2919000.0d0,2929000.0d0,2949000.0d0,2969000.0d0,2989000.0d0,3010000.0d0,3030000.0d0,3050000.0d0,3071000.0d0,  &
     3091000.0d0,3111000.0d0,3132000.0d0,3152000.0d0,3173000.0d0,3193000.0d0,3214000.0d0,3280000.0d0,3381000.0d0,  &
     3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,62),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,2720000.0d0,2725000.0d0,2729000.0d0,2733000.0d0,2738000.0d0,2742000.0d0,2746000.0d0,2751000.0d0,  &
     2755000.0d0,2759000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,2776000.0d0,2781000.0d0,2785000.0d0,2789000.0d0,  &
     2793000.0d0,2797000.0d0,2801000.0d0,2806000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2826000.0d0,  &
     2837000.0d0,2847000.0d0,2857000.0d0,2867000.0d0,2878000.0d0,2888000.0d0,2898000.0d0,2908000.0d0,2918000.0d0,  &
     2928000.0d0,2949000.0d0,2969000.0d0,2989000.0d0,3009000.0d0,3030000.0d0,3050000.0d0,3070000.0d0,3091000.0d0,  &
     3111000.0d0,3132000.0d0,3152000.0d0,3173000.0d0,3193000.0d0,3214000.0d0,3280000.0d0,3381000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,63),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,2723000.0d0,2727000.0d0,2732000.0d0,2736000.0d0,2741000.0d0,2745000.0d0,2749000.0d0,  &
     2754000.0d0,2758000.0d0,2762000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2779000.0d0,2784000.0d0,2788000.0d0,  &
     2792000.0d0,2796000.0d0,2800000.0d0,2805000.0d0,2809000.0d0,2813000.0d0,2817000.0d0,2821000.0d0,2825000.0d0,  &
     2836000.0d0,2846000.0d0,2856000.0d0,2867000.0d0,2877000.0d0,2887000.0d0,2897000.0d0,2907000.0d0,2918000.0d0,  &
     2928000.0d0,2948000.0d0,2968000.0d0,2989000.0d0,3009000.0d0,3029000.0d0,3050000.0d0,3070000.0d0,3090000.0d0,  &
     3111000.0d0,3131000.0d0,3152000.0d0,3172000.0d0,3193000.0d0,3213000.0d0,3280000.0d0,3380000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,64),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2726000.0d0,2730000.0d0,2735000.0d0,2739000.0d0,2743000.0d0,2748000.0d0,2752000.0d0,  &
     2757000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2787000.0d0,2791000.0d0,  &
     2795000.0d0,2799000.0d0,2803000.0d0,2808000.0d0,2812000.0d0,2816000.0d0,2820000.0d0,2824000.0d0,2835000.0d0,  &
     2845000.0d0,2855000.0d0,2866000.0d0,2876000.0d0,2886000.0d0,2896000.0d0,2907000.0d0,2917000.0d0,2927000.0d0,  &
     2947000.0d0,2968000.0d0,2988000.0d0,3008000.0d0,3029000.0d0,3049000.0d0,3069000.0d0,3090000.0d0,3110000.0d0,  &
     3131000.0d0,3151000.0d0,3172000.0d0,3192000.0d0,3213000.0d0,3280000.0d0,3380000.0d0,3487000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,65),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2728000.0d0,2733000.0d0,2737000.0d0,2742000.0d0,2746000.0d0,2751000.0d0,  &
     2755000.0d0,2759000.0d0,2764000.0d0,2768000.0d0,2772000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2790000.0d0,  &
     2794000.0d0,2798000.0d0,2802000.0d0,2806000.0d0,2811000.0d0,2815000.0d0,2819000.0d0,2823000.0d0,2834000.0d0,  &
     2844000.0d0,2854000.0d0,2865000.0d0,2875000.0d0,2885000.0d0,2896000.0d0,2906000.0d0,2916000.0d0,2926000.0d0,  &
     2947000.0d0,2967000.0d0,2987000.0d0,3008000.0d0,3028000.0d0,3049000.0d0,3069000.0d0,3089000.0d0,3110000.0d0,  &
     3130000.0d0,3151000.0d0,3172000.0d0,3192000.0d0,3213000.0d0,3280000.0d0,3380000.0d0,3486000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,66),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2731000.0d0,2735000.0d0,2740000.0d0,2744000.0d0,2749000.0d0,2753000.0d0,  &
     2758000.0d0,2762000.0d0,2767000.0d0,2771000.0d0,2775000.0d0,2780000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,  &
     2797000.0d0,2801000.0d0,2805000.0d0,2809000.0d0,2814000.0d0,2818000.0d0,2822000.0d0,2833000.0d0,2843000.0d0,  &
     2853000.0d0,2864000.0d0,2874000.0d0,2885000.0d0,2895000.0d0,2905000.0d0,2915000.0d0,2926000.0d0,2946000.0d0,  &
     2966000.0d0,2987000.0d0,3007000.0d0,3028000.0d0,3048000.0d0,3069000.0d0,3089000.0d0,3109000.0d0,3130000.0d0,  &
     3151000.0d0,3171000.0d0,3192000.0d0,3212000.0d0,3280000.0d0,3380000.0d0,3486000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,67),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2733000.0d0,2738000.0d0,2743000.0d0,2747000.0d0,2752000.0d0,  &
     2756000.0d0,2761000.0d0,2765000.0d0,2769000.0d0,2774000.0d0,2778000.0d0,2782000.0d0,2787000.0d0,2791000.0d0,  &
     2795000.0d0,2800000.0d0,2804000.0d0,2808000.0d0,2812000.0d0,2817000.0d0,2821000.0d0,2831000.0d0,2842000.0d0,  &
     2852000.0d0,2863000.0d0,2873000.0d0,2884000.0d0,2894000.0d0,2904000.0d0,2915000.0d0,2925000.0d0,2945000.0d0,  &
     2966000.0d0,2986000.0d0,3007000.0d0,3027000.0d0,3048000.0d0,3068000.0d0,3089000.0d0,3109000.0d0,3130000.0d0,  &
     3150000.0d0,3171000.0d0,3191000.0d0,3212000.0d0,3280000.0d0,3380000.0d0,3486000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,68),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2736000.0d0,2741000.0d0,2745000.0d0,2750000.0d0,2754000.0d0,  &
     2759000.0d0,2763000.0d0,2768000.0d0,2772000.0d0,2777000.0d0,2781000.0d0,2785000.0d0,2790000.0d0,2794000.0d0,  &
     2798000.0d0,2803000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2820000.0d0,2830000.0d0,2841000.0d0,2851000.0d0,  &
     2862000.0d0,2872000.0d0,2883000.0d0,2893000.0d0,2903000.0d0,2914000.0d0,2924000.0d0,2945000.0d0,2965000.0d0,  &
     2986000.0d0,3006000.0d0,3027000.0d0,3047000.0d0,3068000.0d0,3088000.0d0,3109000.0d0,3129000.0d0,3150000.0d0,  &
     3170000.0d0,3191000.0d0,3212000.0d0,3280000.0d0,3379000.0d0,3486000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,69),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2739000.0d0,2743000.0d0,2748000.0d0,2752000.0d0,  &
     2757000.0d0,2761000.0d0,2766000.0d0,2770000.0d0,2775000.0d0,2779000.0d0,2784000.0d0,2788000.0d0,2792000.0d0,  &
     2797000.0d0,2801000.0d0,2805000.0d0,2810000.0d0,2814000.0d0,2818000.0d0,2829000.0d0,2840000.0d0,2850000.0d0,  &
     2861000.0d0,2871000.0d0,2882000.0d0,2892000.0d0,2902000.0d0,2913000.0d0,2923000.0d0,2944000.0d0,2964000.0d0,  &
     2985000.0d0,3005000.0d0,3026000.0d0,3046000.0d0,3067000.0d0,3088000.0d0,3108000.0d0,3129000.0d0,3149000.0d0,  &
     3170000.0d0,3191000.0d0,3211000.0d0,3280000.0d0,3379000.0d0,3485000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,70),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2741000.0d0,2746000.0d0,2750000.0d0,2755000.0d0,  &
     2760000.0d0,2764000.0d0,2769000.0d0,2773000.0d0,2778000.0d0,2782000.0d0,2786000.0d0,2791000.0d0,2795000.0d0,  &
     2800000.0d0,2804000.0d0,2808000.0d0,2813000.0d0,2817000.0d0,2828000.0d0,2838000.0d0,2849000.0d0,2860000.0d0,  &
     2870000.0d0,2881000.0d0,2891000.0d0,2901000.0d0,2912000.0d0,2922000.0d0,2943000.0d0,2964000.0d0,2984000.0d0,  &
     3005000.0d0,3025000.0d0,3046000.0d0,3066000.0d0,3087000.0d0,3108000.0d0,3128000.0d0,3149000.0d0,3170000.0d0,  &
     3190000.0d0,3211000.0d0,3280000.0d0,3379000.0d0,3485000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,71),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2744000.0d0,2748000.0d0,2753000.0d0,  &
     2758000.0d0,2762000.0d0,2767000.0d0,2771000.0d0,2776000.0d0,2780000.0d0,2785000.0d0,2789000.0d0,2794000.0d0,  &
     2798000.0d0,2802000.0d0,2807000.0d0,2811000.0d0,2815000.0d0,2826000.0d0,2837000.0d0,2848000.0d0,2858000.0d0,  &
     2869000.0d0,2879000.0d0,2890000.0d0,2900000.0d0,2911000.0d0,2921000.0d0,2942000.0d0,2963000.0d0,2983000.0d0,  &
     3004000.0d0,3025000.0d0,3045000.0d0,3066000.0d0,3086000.0d0,3107000.0d0,3128000.0d0,3148000.0d0,3169000.0d0,  &
     3190000.0d0,3211000.0d0,3280000.0d0,3378000.0d0,3485000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,72),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2746000.0d0,2751000.0d0,2755000.0d0,  &
     2760000.0d0,2765000.0d0,2769000.0d0,2774000.0d0,2778000.0d0,2783000.0d0,2787000.0d0,2792000.0d0,2796000.0d0,  &
     2801000.0d0,2805000.0d0,2810000.0d0,2814000.0d0,2825000.0d0,2836000.0d0,2846000.0d0,2857000.0d0,2868000.0d0,  &
     2878000.0d0,2889000.0d0,2899000.0d0,2910000.0d0,2920000.0d0,2941000.0d0,2962000.0d0,2983000.0d0,3003000.0d0,  &
     3024000.0d0,3045000.0d0,3065000.0d0,3086000.0d0,3106000.0d0,3127000.0d0,3148000.0d0,3169000.0d0,3189000.0d0,  &
     3210000.0d0,3280000.0d0,3378000.0d0,3485000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,73),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2748000.0d0,2753000.0d0,  &
     2758000.0d0,2763000.0d0,2767000.0d0,2772000.0d0,2776000.0d0,2781000.0d0,2786000.0d0,2790000.0d0,2795000.0d0,  &
     2799000.0d0,2803000.0d0,2808000.0d0,2812000.0d0,2823000.0d0,2834000.0d0,2845000.0d0,2856000.0d0,2866000.0d0,  &
     2877000.0d0,2888000.0d0,2898000.0d0,2909000.0d0,2919000.0d0,2940000.0d0,2961000.0d0,2982000.0d0,3002000.0d0,  &
     3023000.0d0,3044000.0d0,3064000.0d0,3085000.0d0,3106000.0d0,3127000.0d0,3147000.0d0,3168000.0d0,3189000.0d0,  &
     3210000.0d0,3280000.0d0,3378000.0d0,3484000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,74),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2751000.0d0,2755000.0d0,  &
     2760000.0d0,2765000.0d0,2770000.0d0,2774000.0d0,2779000.0d0,2784000.0d0,2788000.0d0,2793000.0d0,2797000.0d0,  &
     2802000.0d0,2806000.0d0,2811000.0d0,2822000.0d0,2833000.0d0,2843000.0d0,2854000.0d0,2865000.0d0,2876000.0d0,  &
     2886000.0d0,2897000.0d0,2908000.0d0,2918000.0d0,2939000.0d0,2960000.0d0,2981000.0d0,3002000.0d0,3022000.0d0,  &
     3043000.0d0,3064000.0d0,3085000.0d0,3105000.0d0,3126000.0d0,3147000.0d0,3168000.0d0,3188000.0d0,3209000.0d0,  &
     3280000.0d0,3377000.0d0,3484000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,75),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2753000.0d0,  &
     2758000.0d0,2763000.0d0,2767000.0d0,2772000.0d0,2777000.0d0,2781000.0d0,2786000.0d0,2791000.0d0,2795000.0d0,  &
     2800000.0d0,2804000.0d0,2809000.0d0,2820000.0d0,2831000.0d0,2842000.0d0,2853000.0d0,2864000.0d0,2874000.0d0,  &
     2885000.0d0,2896000.0d0,2906000.0d0,2917000.0d0,2938000.0d0,2959000.0d0,2980000.0d0,3001000.0d0,3022000.0d0,  &
     3042000.0d0,3063000.0d0,3084000.0d0,3105000.0d0,3125000.0d0,3146000.0d0,3167000.0d0,3188000.0d0,3209000.0d0,  &
     3280000.0d0,3377000.0d0,3484000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,76),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2755000.0d0,  &
     2760000.0d0,2765000.0d0,2770000.0d0,2775000.0d0,2779000.0d0,2784000.0d0,2789000.0d0,2793000.0d0,2798000.0d0,  &
     2802000.0d0,2807000.0d0,2818000.0d0,2829000.0d0,2840000.0d0,2851000.0d0,2862000.0d0,2873000.0d0,2884000.0d0,  &
     2894000.0d0,2905000.0d0,2916000.0d0,2937000.0d0,2958000.0d0,2979000.0d0,3000000.0d0,3021000.0d0,3042000.0d0,  &
     3062000.0d0,3083000.0d0,3104000.0d0,3125000.0d0,3146000.0d0,3166000.0d0,3187000.0d0,3208000.0d0,3280000.0d0,  &
     3377000.0d0,3484000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,77),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2757000.0d0,2762000.0d0,2767000.0d0,2772000.0d0,2777000.0d0,2782000.0d0,2786000.0d0,2791000.0d0,2796000.0d0,  &
     2800000.0d0,2805000.0d0,2816000.0d0,2827000.0d0,2839000.0d0,2850000.0d0,2861000.0d0,2872000.0d0,2882000.0d0,  &
     2893000.0d0,2904000.0d0,2915000.0d0,2936000.0d0,2957000.0d0,2978000.0d0,2999000.0d0,3020000.0d0,3041000.0d0,  &
     3062000.0d0,3082000.0d0,3103000.0d0,3124000.0d0,3145000.0d0,3166000.0d0,3187000.0d0,3208000.0d0,3280000.0d0,  &
     3376000.0d0,3483000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,78),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2760000.0d0,2765000.0d0,2770000.0d0,2774000.0d0,2779000.0d0,2784000.0d0,2789000.0d0,2793000.0d0,2798000.0d0,  &
     2803000.0d0,2814000.0d0,2826000.0d0,2837000.0d0,2848000.0d0,2859000.0d0,2870000.0d0,2881000.0d0,2892000.0d0,  &
     2902000.0d0,2913000.0d0,2935000.0d0,2956000.0d0,2977000.0d0,2998000.0d0,3019000.0d0,3040000.0d0,3061000.0d0,  &
     3082000.0d0,3102000.0d0,3123000.0d0,3144000.0d0,3165000.0d0,3186000.0d0,3207000.0d0,3280000.0d0,3376000.0d0,  &
     3483000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,79),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,2762000.0d0,2767000.0d0,2772000.0d0,2777000.0d0,2781000.0d0,2786000.0d0,2791000.0d0,2796000.0d0,2800000.0d0,  &
     2812000.0d0,2824000.0d0,2835000.0d0,2846000.0d0,2857000.0d0,2868000.0d0,2879000.0d0,2890000.0d0,2901000.0d0,  &
     2912000.0d0,2933000.0d0,2955000.0d0,2976000.0d0,2997000.0d0,3018000.0d0,3039000.0d0,3060000.0d0,3081000.0d0,  &
     3102000.0d0,3123000.0d0,3144000.0d0,3164000.0d0,3185000.0d0,3206000.0d0,3280000.0d0,3375000.0d0,3483000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,80),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,2764000.0d0,2769000.0d0,2774000.0d0,2779000.0d0,2784000.0d0,2789000.0d0,2793000.0d0,2798000.0d0,  &
     2810000.0d0,2821000.0d0,2833000.0d0,2844000.0d0,2855000.0d0,2867000.0d0,2878000.0d0,2889000.0d0,2900000.0d0,  &
     2910000.0d0,2932000.0d0,2953000.0d0,2975000.0d0,2996000.0d0,3017000.0d0,3038000.0d0,3059000.0d0,3080000.0d0,  &
     3101000.0d0,3122000.0d0,3143000.0d0,3164000.0d0,3185000.0d0,3206000.0d0,3280000.0d0,3375000.0d0,3482000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,81),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,2766000.0d0,2771000.0d0,2776000.0d0,2781000.0d0,2786000.0d0,2791000.0d0,2796000.0d0,2808000.0d0,  &
     2819000.0d0,2831000.0d0,2842000.0d0,2854000.0d0,2865000.0d0,2876000.0d0,2887000.0d0,2898000.0d0,2909000.0d0,  &
     2931000.0d0,2952000.0d0,2973000.0d0,2995000.0d0,3016000.0d0,3037000.0d0,3058000.0d0,3079000.0d0,3100000.0d0,  &
     3121000.0d0,3142000.0d0,3163000.0d0,3184000.0d0,3205000.0d0,3280000.0d0,3374000.0d0,3482000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,82),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,2768000.0d0,2773000.0d0,2778000.0d0,2783000.0d0,2788000.0d0,2793000.0d0,2805000.0d0,  &
     2817000.0d0,2829000.0d0,2840000.0d0,2852000.0d0,2863000.0d0,2874000.0d0,2885000.0d0,2896000.0d0,2907000.0d0,  &
     2929000.0d0,2951000.0d0,2972000.0d0,2994000.0d0,3015000.0d0,3036000.0d0,3057000.0d0,3078000.0d0,3099000.0d0,  &
     3120000.0d0,3141000.0d0,3162000.0d0,3183000.0d0,3204000.0d0,3280000.0d0,3374000.0d0,3481000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,83),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2770000.0d0,2775000.0d0,2780000.0d0,2785000.0d0,2790000.0d0,2802000.0d0,2814000.0d0,  &
     2826000.0d0,2838000.0d0,2850000.0d0,2861000.0d0,2872000.0d0,2883000.0d0,2895000.0d0,2906000.0d0,2928000.0d0,  &
     2949000.0d0,2971000.0d0,2992000.0d0,3014000.0d0,3035000.0d0,3056000.0d0,3077000.0d0,3098000.0d0,3119000.0d0,  &
     3140000.0d0,3162000.0d0,3183000.0d0,3204000.0d0,3280000.0d0,3373000.0d0,3481000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,84),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2772000.0d0,2777000.0d0,2782000.0d0,2787000.0d0,2800000.0d0,2812000.0d0,  &
     2824000.0d0,2836000.0d0,2847000.0d0,2859000.0d0,2870000.0d0,2882000.0d0,2893000.0d0,2904000.0d0,2926000.0d0,  &
     2948000.0d0,2969000.0d0,2991000.0d0,3012000.0d0,3034000.0d0,3055000.0d0,3076000.0d0,3097000.0d0,3118000.0d0,  &
     3140000.0d0,3161000.0d0,3182000.0d0,3203000.0d0,3280000.0d0,3373000.0d0,3480000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,85),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2774000.0d0,2779000.0d0,2784000.0d0,2797000.0d0,2809000.0d0,2821000.0d0,  &
     2833000.0d0,2845000.0d0,2857000.0d0,2868000.0d0,2880000.0d0,2891000.0d0,2902000.0d0,2924000.0d0,2946000.0d0,  &
     2968000.0d0,2990000.0d0,3011000.0d0,3033000.0d0,3054000.0d0,3075000.0d0,3096000.0d0,3118000.0d0,3139000.0d0,  &
     3160000.0d0,3181000.0d0,3202000.0d0,3280000.0d0,3372000.0d0,3480000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,86),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2775000.0d0,2781000.0d0,2794000.0d0,2806000.0d0,2819000.0d0,  &
     2831000.0d0,2843000.0d0,2854000.0d0,2866000.0d0,2878000.0d0,2889000.0d0,2900000.0d0,2923000.0d0,2945000.0d0,  &
     2967000.0d0,2988000.0d0,3010000.0d0,3031000.0d0,3053000.0d0,3074000.0d0,3095000.0d0,3117000.0d0,3138000.0d0,  &
     3159000.0d0,3180000.0d0,3201000.0d0,3280000.0d0,3372000.0d0,3480000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,87),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2777000.0d0,2790000.0d0,2803000.0d0,2816000.0d0,2828000.0d0,  &
     2840000.0d0,2852000.0d0,2864000.0d0,2875000.0d0,2887000.0d0,2898000.0d0,2921000.0d0,2943000.0d0,2965000.0d0,  &
     2987000.0d0,3009000.0d0,3030000.0d0,3052000.0d0,3073000.0d0,3094000.0d0,3116000.0d0,3137000.0d0,3158000.0d0,  &
     3179000.0d0,3201000.0d0,3280000.0d0,3371000.0d0,3479000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,88),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2781000.0d0,2795000.0d0,2808000.0d0,2821000.0d0,  &
     2833000.0d0,2846000.0d0,2858000.0d0,2870000.0d0,2881000.0d0,2893000.0d0,2916000.0d0,2939000.0d0,2961000.0d0,  &
     2983000.0d0,3005000.0d0,3027000.0d0,3048000.0d0,3070000.0d0,3091000.0d0,3113000.0d0,3134000.0d0,3156000.0d0,  &
     3177000.0d0,3198000.0d0,3280000.0d0,3370000.0d0,3478000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,89),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2785000.0d0,2799000.0d0,2813000.0d0,2826000.0d0,  &
     2838000.0d0,2851000.0d0,2863000.0d0,2875000.0d0,2887000.0d0,2910000.0d0,2933000.0d0,2956000.0d0,2979000.0d0,  &
     3001000.0d0,3023000.0d0,3045000.0d0,3067000.0d0,3088000.0d0,3110000.0d0,3132000.0d0,3153000.0d0,3175000.0d0,  &
     3196000.0d0,3280000.0d0,3368000.0d0,3476000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,90),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2789000.0d0,2803000.0d0,2817000.0d0,  &
     2830000.0d0,2843000.0d0,2856000.0d0,2868000.0d0,2880000.0d0,2904000.0d0,2928000.0d0,2951000.0d0,2974000.0d0,  &
     2996000.0d0,3019000.0d0,3041000.0d0,3063000.0d0,3085000.0d0,3107000.0d0,3128000.0d0,3150000.0d0,3172000.0d0,  &
     3193000.0d0,3280000.0d0,3366000.0d0,3475000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,91),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2792000.0d0,2807000.0d0,2821000.0d0,  &
     2834000.0d0,2847000.0d0,2860000.0d0,2873000.0d0,2898000.0d0,2922000.0d0,2945000.0d0,2969000.0d0,2992000.0d0,  &
     3014000.0d0,3037000.0d0,3059000.0d0,3081000.0d0,3103000.0d0,3125000.0d0,3147000.0d0,3169000.0d0,3190000.0d0,  &
     3280000.0d0,3364000.0d0,3473000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,92),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2795000.0d0,2810000.0d0,  &
     2824000.0d0,2838000.0d0,2851000.0d0,2864000.0d0,2890000.0d0,2915000.0d0,2939000.0d0,2963000.0d0,2986000.0d0,  &
     3009000.0d0,3032000.0d0,3055000.0d0,3077000.0d0,3099000.0d0,3121000.0d0,3143000.0d0,3165000.0d0,3187000.0d0,  &
     3280000.0d0,3362000.0d0,3471000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,93),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2797000.0d0,2813000.0d0,  &
     2827000.0d0,2841000.0d0,2855000.0d0,2882000.0d0,2907000.0d0,2932000.0d0,2956000.0d0,2980000.0d0,3004000.0d0,  &
     3027000.0d0,3050000.0d0,3072000.0d0,3095000.0d0,3117000.0d0,3140000.0d0,3162000.0d0,3184000.0d0,3280000.0d0,  &
     3359000.0d0,3469000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,94),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2799000.0d0,  &
     2815000.0d0,2830000.0d0,2844000.0d0,2872000.0d0,2899000.0d0,2924000.0d0,2949000.0d0,2974000.0d0,2998000.0d0,  &
     3021000.0d0,3044000.0d0,3067000.0d0,3090000.0d0,3113000.0d0,3135000.0d0,3158000.0d0,3180000.0d0,3280000.0d0,  &
     3357000.0d0,3467000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,95),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2801000.0d0,  &
     2817000.0d0,2832000.0d0,2862000.0d0,2889000.0d0,2916000.0d0,2941000.0d0,2966000.0d0,2991000.0d0,3015000.0d0,  &
     3039000.0d0,3062000.0d0,3085000.0d0,3108000.0d0,3131000.0d0,3154000.0d0,3176000.0d0,3280000.0d0,3354000.0d0,  &
     3465000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,96),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2802000.0d0,2819000.0d0,2850000.0d0,2879000.0d0,2906000.0d0,2933000.0d0,2958000.0d0,2984000.0d0,3008000.0d0,  &
     3032000.0d0,3056000.0d0,3080000.0d0,3103000.0d0,3126000.0d0,3149000.0d0,3172000.0d0,3280000.0d0,3351000.0d0,  &
     3462000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,97),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     2803000.0d0,2836000.0d0,2867000.0d0,2895000.0d0,2923000.0d0,2950000.0d0,2975000.0d0,3001000.0d0,3025000.0d0,  &
     3050000.0d0,3073000.0d0,3097000.0d0,3121000.0d0,3144000.0d0,3167000.0d0,3280000.0d0,3348000.0d0,3459000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,98),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,2803000.0d0,2838000.0d0,2870000.0d0,2900000.0d0,2929000.0d0,2957000.0d0,2983000.0d0,3009000.0d0,3035000.0d0,  &
     3060000.0d0,3084000.0d0,3108000.0d0,3132000.0d0,3156000.0d0,3280000.0d0,3340000.0d0,3453000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,99),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,2801000.0d0,2838000.0d0,2872000.0d0,2904000.0d0,2934000.0d0,2963000.0d0,2990000.0d0,3017000.0d0,  &
     3043000.0d0,3069000.0d0,3094000.0d0,3119000.0d0,3143000.0d0,3280000.0d0,3332000.0d0,3446000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,100),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,2797000.0d0,2837000.0d0,2873000.0d0,2906000.0d0,2937000.0d0,2967000.0d0,2996000.0d0,3023000.0d0,  &
     3050000.0d0,3077000.0d0,3103000.0d0,3128000.0d0,3280000.0d0,3322000.0d0,3438000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,101),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,2790000.0d0,2833000.0d0,2871000.0d0,2906000.0d0,2939000.0d0,2970000.0d0,3000000.0d0,  &
     3029000.0d0,3057000.0d0,3084000.0d0,3110000.0d0,3280000.0d0,3310000.0d0,3429000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,102),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2780000.0d0,2826000.0d0,2867000.0d0,2905000.0d0,2939000.0d0,2972000.0d0,3003000.0d0,  &
     3033000.0d0,3062000.0d0,3090000.0d0,3280000.0d0,3297000.0d0,3418000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,103),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2767000.0d0,2817000.0d0,2861000.0d0,2901000.0d0,2938000.0d0,2972000.0d0,  &
     3004000.0d0,3036000.0d0,3066000.0d0,3280000.0d0,3282000.0d0,3406000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,104),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2750000.0d0,2806000.0d0,2853000.0d0,2895000.0d0,2934000.0d0,2970000.0d0,  &
     3004000.0d0,3037000.0d0,3280000.0d0,3264000.0d0,3392000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,105),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2728000.0d0,2790000.0d0,2842000.0d0,2887000.0d0,2929000.0d0,  &
     2967000.0d0,3003000.0d0,3280000.0d0,3244000.0d0,3377000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,106),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2701000.0d0,2771000.0d0,2828000.0d0,2877000.0d0,2921000.0d0,  &
     2961000.0d0,3280000.0d0,3222000.0d0,3359000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,107),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2666000.0d0,2747000.0d0,2810000.0d0,2864000.0d0,  &
     2911000.0d0,3280000.0d0,3195000.0d0,3339000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,108),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2622000.0d0,2718000.0d0,2789000.0d0,2847000.0d0,  &
     3280000.0d0,3165000.0d0,3316000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,109),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2564000.0d0,2683000.0d0,2763000.0d0,  &
     3280000.0d0,3130000.0d0,3290000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,110),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2481000.0d0,2641000.0d0,3280000.0d0,  &
     3089000.0d0,3260000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,111),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2335000.0d0,3280000.0d0,  &
     3040000.0d0,3226000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,112),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3280000.0d0,2821000.0d0,  &
     3085000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,113),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3280000.0d0,2671000.0d0,  &
     2998000.0d0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,114),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3280000.0d0,2512000.0d0,  &
     2906000.0d0/
  DATA (DefaultSteamSuperheatedDensityData(i,1),i=1,DefaultNumSteamSuperheatedTemps)  &
    /4.855d-003,4.837d-003,4.767d-003,4.683d-003,4.601d-003,4.522d-003,4.446d-003,4.373d-003,4.302d-003,4.233d-003,  &
     4.167d-003,4.102d-003,4.039d-003,3.979d-003,3.920d-003,3.863d-003,3.840d-003,3.818d-003,3.796d-003,3.775d-003,  &
     3.753d-003,3.732d-003,3.711d-003,3.691d-003,3.670d-003,3.650d-003,3.630d-003,3.610d-003,3.591d-003,3.571d-003,  &
     3.562d-003,3.552d-003,3.543d-003,3.533d-003,3.524d-003,3.514d-003,3.505d-003,3.496d-003,3.487d-003,3.477d-003,  &
     3.468d-003,3.459d-003,3.450d-003,3.441d-003,3.432d-003,3.424d-003,3.415d-003,3.406d-003,3.397d-003,3.388d-003,  &
     3.380d-003,3.371d-003,3.363d-003,3.354d-003,3.346d-003,3.337d-003,3.329d-003,3.321d-003,3.312d-003,3.304d-003,  &
     3.296d-003,3.288d-003,3.271d-003,3.255d-003,3.239d-003,3.224d-003,3.208d-003,3.193d-003,3.177d-003,3.162d-003,  &
     3.147d-003,3.132d-003,3.117d-003,3.103d-003,3.088d-003,3.074d-003,3.060d-003,3.046d-003,3.032d-003,3.018d-003,  &
     3.004d-003,2.991d-003,2.977d-003,2.964d-003,2.951d-003,2.938d-003,2.925d-003,2.893d-003,2.862d-003,2.831d-003,  &
     2.801d-003,2.772d-003,2.743d-003,2.715d-003,2.688d-003,2.661d-003,2.634d-003,2.583d-003,2.533d-003,2.486d-003,  &
     2.440d-003,2.396d-003,2.353d-003,2.312d-003,2.273d-003,2.234d-003,2.197d-003,2.162d-003,2.127d-003,2.093d-003,  &
     2.061d-003,3.542d-005,1.833d-003,1.714d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,2),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,5.196d-003,5.121d-003,5.031d-003,4.943d-003,4.859d-003,4.777d-003,4.698d-003,4.622d-003,4.548d-003,4.476d-003,  &
     4.407d-003,4.340d-003,4.274d-003,4.211d-003,4.150d-003,4.126d-003,4.102d-003,4.078d-003,4.055d-003,4.032d-003,  &
     4.009d-003,3.987d-003,3.965d-003,3.943d-003,3.921d-003,3.899d-003,3.878d-003,3.857d-003,3.836d-003,3.826d-003,  &
     3.816d-003,3.806d-003,3.795d-003,3.785d-003,3.775d-003,3.765d-003,3.755d-003,3.746d-003,3.736d-003,3.726d-003,  &
     3.716d-003,3.707d-003,3.697d-003,3.687d-003,3.678d-003,3.668d-003,3.659d-003,3.650d-003,3.640d-003,3.631d-003,  &
     3.622d-003,3.612d-003,3.603d-003,3.594d-003,3.585d-003,3.576d-003,3.567d-003,3.558d-003,3.549d-003,3.541d-003,  &
     3.532d-003,3.514d-003,3.497d-003,3.480d-003,3.463d-003,3.446d-003,3.430d-003,3.413d-003,3.397d-003,3.381d-003,  &
     3.365d-003,3.349d-003,3.333d-003,3.318d-003,3.302d-003,3.287d-003,3.272d-003,3.257d-003,3.242d-003,3.228d-003,  &
     3.213d-003,3.198d-003,3.184d-003,3.170d-003,3.156d-003,3.142d-003,3.108d-003,3.074d-003,3.041d-003,3.009d-003,  &
     2.978d-003,2.947d-003,2.917d-003,2.887d-003,2.858d-003,2.830d-003,2.775d-003,2.722d-003,2.671d-003,2.621d-003,  &
     2.574d-003,2.528d-003,2.484d-003,2.442d-003,2.400d-003,2.361d-003,2.322d-003,2.285d-003,2.249d-003,2.214d-003,  &
     3.542d-005,1.969d-003,1.841d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,3),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,6.802d-003,6.681d-003,6.565d-003,6.453d-003,6.344d-003,6.239d-003,6.138d-003,6.040d-003,5.944d-003,  &
     5.852d-003,5.763d-003,5.676d-003,5.592d-003,5.511d-003,5.479d-003,5.447d-003,5.416d-003,5.385d-003,5.355d-003,  &
     5.324d-003,5.295d-003,5.265d-003,5.236d-003,5.207d-003,5.178d-003,5.150d-003,5.122d-003,5.095d-003,5.081d-003,  &
     5.067d-003,5.054d-003,5.040d-003,5.027d-003,5.014d-003,5.000d-003,4.987d-003,4.974d-003,4.961d-003,4.948d-003,  &
     4.935d-003,4.922d-003,4.909d-003,4.897d-003,4.884d-003,4.871d-003,4.859d-003,4.846d-003,4.834d-003,4.822d-003,  &
     4.809d-003,4.797d-003,4.785d-003,4.773d-003,4.761d-003,4.749d-003,4.737d-003,4.725d-003,4.714d-003,4.702d-003,  &
     4.690d-003,4.667d-003,4.644d-003,4.621d-003,4.599d-003,4.577d-003,4.555d-003,4.533d-003,4.511d-003,4.490d-003,  &
     4.468d-003,4.447d-003,4.427d-003,4.406d-003,4.385d-003,4.365d-003,4.345d-003,4.325d-003,4.306d-003,4.286d-003,  &
     4.267d-003,4.247d-003,4.228d-003,4.210d-003,4.191d-003,4.172d-003,4.127d-003,4.082d-003,4.039d-003,3.996d-003,  &
     3.954d-003,3.913d-003,3.873d-003,3.834d-003,3.796d-003,3.758d-003,3.685d-003,3.614d-003,3.546d-003,3.481d-003,  &
     3.418d-003,3.357d-003,3.299d-003,3.242d-003,3.188d-003,3.135d-003,3.084d-003,3.034d-003,2.986d-003,2.940d-003,  &
     3.542d-005,2.615d-003,2.445d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,4),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,9.407d-003,9.243d-003,9.084d-003,8.931d-003,8.783d-003,8.640d-003,8.502d-003,8.368d-003,8.238d-003,  &
     8.113d-003,7.991d-003,7.872d-003,7.757d-003,7.712d-003,7.668d-003,7.624d-003,7.580d-003,7.537d-003,7.495d-003,  &
     7.453d-003,7.411d-003,7.370d-003,7.330d-003,7.289d-003,7.250d-003,7.210d-003,7.172d-003,7.152d-003,7.133d-003,  &
     7.114d-003,7.095d-003,7.076d-003,7.057d-003,7.039d-003,7.020d-003,7.002d-003,6.983d-003,6.965d-003,6.947d-003,  &
     6.929d-003,6.911d-003,6.893d-003,6.875d-003,6.857d-003,6.840d-003,6.822d-003,6.805d-003,6.787d-003,6.770d-003,  &
     6.753d-003,6.736d-003,6.719d-003,6.702d-003,6.685d-003,6.668d-003,6.651d-003,6.635d-003,6.618d-003,6.602d-003,  &
     6.569d-003,6.537d-003,6.505d-003,6.473d-003,6.442d-003,6.411d-003,6.380d-003,6.350d-003,6.320d-003,6.290d-003,  &
     6.260d-003,6.231d-003,6.202d-003,6.173d-003,6.144d-003,6.116d-003,6.088d-003,6.060d-003,6.033d-003,6.006d-003,  &
     5.979d-003,5.952d-003,5.925d-003,5.899d-003,5.873d-003,5.809d-003,5.746d-003,5.685d-003,5.625d-003,5.566d-003,  &
     5.508d-003,5.452d-003,5.397d-003,5.342d-003,5.289d-003,5.186d-003,5.087d-003,4.992d-003,4.900d-003,4.811d-003,  &
     4.726d-003,4.643d-003,4.564d-003,4.487d-003,4.412d-003,4.340d-003,4.271d-003,4.203d-003,4.138d-003,3.542d-005,  &
     3.680d-003,3.442d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,5),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,1.284d-002,1.262d-002,1.241d-002,1.220d-002,1.200d-002,1.181d-002,1.162d-002,1.144d-002,  &
     1.127d-002,1.110d-002,1.093d-002,1.078d-002,1.071d-002,1.065d-002,1.059d-002,1.053d-002,1.047d-002,1.041d-002,  &
     1.035d-002,1.029d-002,1.024d-002,1.018d-002,1.012d-002,1.007d-002,1.001d-002,9.961d-003,9.934d-003,9.907d-003,  &
     9.881d-003,9.855d-003,9.828d-003,9.802d-003,9.776d-003,9.750d-003,9.725d-003,9.699d-003,9.674d-003,9.649d-003,  &
     9.623d-003,9.598d-003,9.574d-003,9.549d-003,9.524d-003,9.500d-003,9.475d-003,9.451d-003,9.427d-003,9.403d-003,  &
     9.379d-003,9.355d-003,9.332d-003,9.308d-003,9.285d-003,9.261d-003,9.238d-003,9.215d-003,9.192d-003,9.170d-003,  &
     9.124d-003,9.079d-003,9.035d-003,8.991d-003,8.947d-003,8.904d-003,8.862d-003,8.819d-003,8.777d-003,8.736d-003,  &
     8.695d-003,8.654d-003,8.614d-003,8.574d-003,8.534d-003,8.495d-003,8.456d-003,8.417d-003,8.379d-003,8.341d-003,  &
     8.304d-003,8.267d-003,8.230d-003,8.193d-003,8.157d-003,8.068d-003,7.981d-003,7.896d-003,7.812d-003,7.731d-003,  &
     7.651d-003,7.572d-003,7.495d-003,7.420d-003,7.346d-003,7.203d-003,7.065d-003,6.933d-003,6.805d-003,6.682d-003,  &
     6.563d-003,6.449d-003,6.338d-003,6.231d-003,6.128d-003,6.028d-003,5.931d-003,5.838d-003,5.747d-003,3.542d-005,  &
     5.111d-003,4.781d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,6),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.731d-002,1.702d-002,1.674d-002,1.646d-002,1.620d-002,1.594d-002,1.570d-002,1.546d-002,  &
     1.522d-002,1.500d-002,1.478d-002,1.469d-002,1.461d-002,1.452d-002,1.444d-002,1.436d-002,1.428d-002,1.420d-002,  &
     1.412d-002,1.404d-002,1.396d-002,1.389d-002,1.381d-002,1.374d-002,1.366d-002,1.362d-002,1.359d-002,1.355d-002,  &
     1.352d-002,1.348d-002,1.344d-002,1.341d-002,1.337d-002,1.334d-002,1.330d-002,1.327d-002,1.323d-002,1.320d-002,  &
     1.316d-002,1.313d-002,1.310d-002,1.306d-002,1.303d-002,1.300d-002,1.296d-002,1.293d-002,1.290d-002,1.286d-002,  &
     1.283d-002,1.280d-002,1.277d-002,1.273d-002,1.270d-002,1.267d-002,1.264d-002,1.261d-002,1.258d-002,1.251d-002,  &
     1.245d-002,1.239d-002,1.233d-002,1.227d-002,1.221d-002,1.215d-002,1.210d-002,1.204d-002,1.198d-002,1.192d-002,  &
     1.187d-002,1.181d-002,1.176d-002,1.170d-002,1.165d-002,1.160d-002,1.154d-002,1.149d-002,1.144d-002,1.139d-002,  &
     1.134d-002,1.129d-002,1.124d-002,1.119d-002,1.107d-002,1.095d-002,1.083d-002,1.071d-002,1.060d-002,1.049d-002,  &
     1.038d-002,1.028d-002,1.018d-002,1.007d-002,9.879d-003,9.690d-003,9.508d-003,9.333d-003,9.164d-003,9.001d-003,  &
     8.844d-003,8.692d-003,8.546d-003,8.404d-003,8.267d-003,8.134d-003,8.006d-003,7.881d-003,3.542d-005,7.009d-003,  &
     6.556d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,7),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.307d-002,2.269d-002,2.232d-002,2.196d-002,2.161d-002,2.128d-002,2.095d-002,  &
     2.063d-002,2.033d-002,2.003d-002,1.991d-002,1.980d-002,1.968d-002,1.957d-002,1.946d-002,1.935d-002,1.924d-002,  &
     1.913d-002,1.903d-002,1.892d-002,1.882d-002,1.872d-002,1.862d-002,1.851d-002,1.846d-002,1.842d-002,1.837d-002,  &
     1.832d-002,1.827d-002,1.822d-002,1.817d-002,1.812d-002,1.808d-002,1.803d-002,1.798d-002,1.793d-002,1.789d-002,  &
     1.784d-002,1.779d-002,1.775d-002,1.770d-002,1.766d-002,1.761d-002,1.757d-002,1.752d-002,1.748d-002,1.743d-002,  &
     1.739d-002,1.734d-002,1.730d-002,1.726d-002,1.721d-002,1.717d-002,1.713d-002,1.708d-002,1.704d-002,1.696d-002,  &
     1.687d-002,1.679d-002,1.671d-002,1.663d-002,1.655d-002,1.647d-002,1.639d-002,1.631d-002,1.624d-002,1.616d-002,  &
     1.608d-002,1.601d-002,1.593d-002,1.586d-002,1.579d-002,1.572d-002,1.564d-002,1.557d-002,1.550d-002,1.543d-002,  &
     1.536d-002,1.530d-002,1.523d-002,1.516d-002,1.499d-002,1.483d-002,1.467d-002,1.452d-002,1.437d-002,1.422d-002,  &
     1.407d-002,1.393d-002,1.379d-002,1.365d-002,1.339d-002,1.313d-002,1.288d-002,1.265d-002,1.242d-002,1.220d-002,  &
     1.198d-002,1.178d-002,1.158d-002,1.139d-002,1.120d-002,1.102d-002,1.085d-002,1.068d-002,3.542d-005,9.498d-003,  &
     8.884d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,8),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.042d-002,2.992d-002,2.943d-002,2.897d-002,2.851d-002,2.808d-002,2.765d-002,  &
     2.724d-002,2.684d-002,2.669d-002,2.653d-002,2.638d-002,2.623d-002,2.608d-002,2.593d-002,2.579d-002,2.564d-002,  &
     2.550d-002,2.536d-002,2.522d-002,2.508d-002,2.494d-002,2.481d-002,2.474d-002,2.468d-002,2.461d-002,2.454d-002,  &
     2.448d-002,2.441d-002,2.435d-002,2.428d-002,2.422d-002,2.416d-002,2.409d-002,2.403d-002,2.397d-002,2.391d-002,  &
     2.384d-002,2.378d-002,2.372d-002,2.366d-002,2.360d-002,2.354d-002,2.348d-002,2.342d-002,2.336d-002,2.330d-002,  &
     2.324d-002,2.318d-002,2.312d-002,2.306d-002,2.301d-002,2.295d-002,2.289d-002,2.284d-002,2.272d-002,2.261d-002,  &
     2.250d-002,2.239d-002,2.228d-002,2.217d-002,2.207d-002,2.196d-002,2.186d-002,2.175d-002,2.165d-002,2.155d-002,  &
     2.145d-002,2.135d-002,2.125d-002,2.115d-002,2.106d-002,2.096d-002,2.087d-002,2.077d-002,2.068d-002,2.059d-002,  &
     2.049d-002,2.040d-002,2.031d-002,2.009d-002,1.987d-002,1.966d-002,1.945d-002,1.925d-002,1.905d-002,1.885d-002,  &
     1.866d-002,1.848d-002,1.829d-002,1.794d-002,1.759d-002,1.726d-002,1.694d-002,1.664d-002,1.634d-002,1.606d-002,  &
     1.578d-002,1.552d-002,1.526d-002,1.501d-002,1.477d-002,1.453d-002,1.431d-002,3.542d-005,1.273d-002,1.190d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,9),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.967d-002,3.903d-002,3.841d-002,3.781d-002,3.723d-002,3.666d-002,  &
     3.612d-002,3.559d-002,3.538d-002,3.518d-002,3.497d-002,3.477d-002,3.457d-002,3.438d-002,3.419d-002,3.399d-002,  &
     3.380d-002,3.362d-002,3.343d-002,3.325d-002,3.307d-002,3.289d-002,3.280d-002,3.271d-002,3.262d-002,3.254d-002,  &
     3.245d-002,3.236d-002,3.228d-002,3.219d-002,3.211d-002,3.202d-002,3.194d-002,3.186d-002,3.177d-002,3.169d-002,  &
     3.161d-002,3.153d-002,3.144d-002,3.136d-002,3.128d-002,3.120d-002,3.112d-002,3.104d-002,3.096d-002,3.089d-002,  &
     3.081d-002,3.073d-002,3.065d-002,3.058d-002,3.050d-002,3.042d-002,3.035d-002,3.027d-002,3.012d-002,2.997d-002,  &
     2.983d-002,2.968d-002,2.954d-002,2.939d-002,2.925d-002,2.911d-002,2.897d-002,2.884d-002,2.870d-002,2.857d-002,  &
     2.843d-002,2.830d-002,2.817d-002,2.804d-002,2.791d-002,2.778d-002,2.766d-002,2.753d-002,2.741d-002,2.729d-002,  &
     2.716d-002,2.704d-002,2.692d-002,2.663d-002,2.634d-002,2.606d-002,2.579d-002,2.552d-002,2.525d-002,2.499d-002,  &
     2.474d-002,2.449d-002,2.425d-002,2.377d-002,2.332d-002,2.288d-002,2.246d-002,2.205d-002,2.166d-002,2.128d-002,  &
     2.092d-002,2.057d-002,2.022d-002,1.989d-002,1.957d-002,1.927d-002,1.897d-002,3.542d-005,1.687d-002,1.578d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,10),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,5.124d-002,5.042d-002,4.963d-002,4.887d-002,4.812d-002,4.741d-002,  &
     4.671d-002,4.644d-002,4.617d-002,4.590d-002,4.564d-002,4.537d-002,4.512d-002,4.486d-002,4.461d-002,4.436d-002,  &
     4.412d-002,4.387d-002,4.363d-002,4.340d-002,4.316d-002,4.304d-002,4.293d-002,4.281d-002,4.270d-002,4.258d-002,  &
     4.247d-002,4.236d-002,4.225d-002,4.213d-002,4.202d-002,4.191d-002,4.180d-002,4.169d-002,4.158d-002,4.148d-002,  &
     4.137d-002,4.126d-002,4.116d-002,4.105d-002,4.094d-002,4.084d-002,4.073d-002,4.063d-002,4.053d-002,4.043d-002,  &
     4.032d-002,4.022d-002,4.012d-002,4.002d-002,3.992d-002,3.982d-002,3.972d-002,3.952d-002,3.933d-002,3.914d-002,  &
     3.895d-002,3.876d-002,3.857d-002,3.838d-002,3.820d-002,3.802d-002,3.784d-002,3.766d-002,3.748d-002,3.731d-002,  &
     3.713d-002,3.696d-002,3.679d-002,3.662d-002,3.646d-002,3.629d-002,3.613d-002,3.596d-002,3.580d-002,3.564d-002,  &
     3.548d-002,3.533d-002,3.494d-002,3.456d-002,3.419d-002,3.383d-002,3.348d-002,3.313d-002,3.279d-002,3.246d-002,  &
     3.213d-002,3.181d-002,3.119d-002,3.059d-002,3.002d-002,2.947d-002,2.893d-002,2.842d-002,2.792d-002,2.744d-002,  &
     2.698d-002,2.653d-002,2.610d-002,2.568d-002,2.528d-002,2.488d-002,3.542d-005,2.213d-002,2.070d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,11),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,6.556d-002,6.453d-002,6.353d-002,6.256d-002,6.163d-002,  &
     6.072d-002,6.036d-002,6.001d-002,5.966d-002,5.932d-002,5.898d-002,5.864d-002,5.831d-002,5.799d-002,5.766d-002,  &
     5.734d-002,5.702d-002,5.671d-002,5.640d-002,5.610d-002,5.594d-002,5.579d-002,5.564d-002,5.549d-002,5.535d-002,  &
     5.520d-002,5.505d-002,5.490d-002,5.476d-002,5.461d-002,5.447d-002,5.433d-002,5.419d-002,5.404d-002,5.390d-002,  &
     5.376d-002,5.362d-002,5.349d-002,5.335d-002,5.321d-002,5.307d-002,5.294d-002,5.280d-002,5.267d-002,5.254d-002,  &
     5.240d-002,5.227d-002,5.214d-002,5.201d-002,5.188d-002,5.175d-002,5.162d-002,5.136d-002,5.111d-002,5.086d-002,  &
     5.061d-002,5.036d-002,5.012d-002,4.988d-002,4.964d-002,4.940d-002,4.917d-002,4.894d-002,4.871d-002,4.848d-002,  &
     4.825d-002,4.803d-002,4.781d-002,4.759d-002,4.737d-002,4.716d-002,4.694d-002,4.673d-002,4.652d-002,4.632d-002,  &
     4.611d-002,4.591d-002,4.540d-002,4.491d-002,4.443d-002,4.396d-002,4.350d-002,4.305d-002,4.261d-002,4.218d-002,  &
     4.175d-002,4.134d-002,4.053d-002,3.975d-002,3.901d-002,3.829d-002,3.759d-002,3.693d-002,3.628d-002,3.566d-002,  &
     3.506d-002,3.448d-002,3.391d-002,3.337d-002,3.284d-002,3.233d-002,3.542d-005,2.875d-002,2.689d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,12),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,8.315d-002,8.185d-002,8.060d-002,7.939d-002,  &
     7.821d-002,7.775d-002,7.730d-002,7.685d-002,7.641d-002,7.597d-002,7.553d-002,7.511d-002,7.468d-002,7.426d-002,  &
     7.385d-002,7.344d-002,7.304d-002,7.264d-002,7.224d-002,7.205d-002,7.185d-002,7.166d-002,7.147d-002,7.128d-002,  &
     7.108d-002,7.090d-002,7.071d-002,7.052d-002,7.033d-002,7.015d-002,6.996d-002,6.978d-002,6.960d-002,6.942d-002,  &
     6.923d-002,6.906d-002,6.888d-002,6.870d-002,6.852d-002,6.835d-002,6.817d-002,6.800d-002,6.782d-002,6.765d-002,  &
     6.748d-002,6.731d-002,6.714d-002,6.697d-002,6.680d-002,6.664d-002,6.647d-002,6.614d-002,6.581d-002,6.549d-002,  &
     6.517d-002,6.485d-002,6.454d-002,6.423d-002,6.392d-002,6.361d-002,6.331d-002,6.301d-002,6.272d-002,6.242d-002,  &
     6.213d-002,6.185d-002,6.156d-002,6.128d-002,6.100d-002,6.072d-002,6.044d-002,6.017d-002,5.990d-002,5.963d-002,  &
     5.937d-002,5.911d-002,5.846d-002,5.783d-002,5.721d-002,5.660d-002,5.601d-002,5.543d-002,5.486d-002,5.430d-002,  &
     5.375d-002,5.322d-002,5.218d-002,5.118d-002,5.022d-002,4.929d-002,4.840d-002,4.754d-002,4.671d-002,4.591d-002,  &
     4.513d-002,4.438d-002,4.366d-002,4.296d-002,4.228d-002,4.162d-002,3.542d-005,3.701d-002,3.462d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,13),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.10460,0.10290,0.10140,9.988d-002,9.929d-002,  &
     9.871d-002,9.813d-002,9.757d-002,9.700d-002,9.645d-002,9.590d-002,9.536d-002,9.482d-002,9.430d-002,9.377d-002,  &
     9.325d-002,9.274d-002,9.224d-002,9.199d-002,9.174d-002,9.149d-002,9.124d-002,9.100d-002,9.075d-002,9.051d-002,  &
     9.027d-002,9.003d-002,8.979d-002,8.955d-002,8.932d-002,8.908d-002,8.885d-002,8.862d-002,8.839d-002,8.816d-002,  &
     8.793d-002,8.770d-002,8.747d-002,8.725d-002,8.703d-002,8.680d-002,8.658d-002,8.636d-002,8.614d-002,8.592d-002,  &
     8.571d-002,8.549d-002,8.528d-002,8.506d-002,8.485d-002,8.443d-002,8.401d-002,8.360d-002,8.319d-002,8.278d-002,  &
     8.238d-002,8.198d-002,8.159d-002,8.120d-002,8.081d-002,8.043d-002,8.005d-002,7.968d-002,7.931d-002,7.894d-002,  &
     7.857d-002,7.821d-002,7.786d-002,7.750d-002,7.715d-002,7.680d-002,7.646d-002,7.611d-002,7.578d-002,7.544d-002,  &
     7.461d-002,7.380d-002,7.301d-002,7.224d-002,7.148d-002,7.074d-002,7.001d-002,6.930d-002,6.860d-002,6.792d-002,  &
     6.659d-002,6.532d-002,6.409d-002,6.291d-002,6.177d-002,6.067d-002,5.961d-002,5.859d-002,5.760d-002,5.664d-002,  &
     5.572d-002,5.482d-002,5.395d-002,5.312d-002,3.542d-005,4.724d-002,4.418d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,14),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.13040,0.12840,0.12650,0.12580,0.125d0,  &
     0.12430,0.12360,0.12290,0.12220,0.12150,0.12080,0.12010,0.11940,0.11870,0.11810,0.11740,0.11680,0.11650,0.11620,  &
     0.11580,0.11550,0.11520,0.11490,0.11460,0.11430,0.114d0,0.11370,0.11340,0.11310,0.11280,0.11250,0.11220,0.11190,  &
     0.11160,0.11130,0.111d0,0.11080,0.11050,0.11020,0.10990,0.10960,0.10930,0.10910,0.10880,0.10850,0.10820,0.108d0,  &
     0.10770,0.10740,0.10690,0.10640,0.10580,0.10530,0.10480,0.10430,0.10380,0.10330,0.10280,0.10230,0.10180,0.10130,  &
     0.10090,0.10040,9.993d-002,9.946d-002,9.901d-002,9.855d-002,9.810d-002,9.766d-002,9.722d-002,9.678d-002,9.635d-002,  &
     9.592d-002,9.549d-002,9.444d-002,9.342d-002,9.242d-002,9.144d-002,9.048d-002,8.954d-002,8.862d-002,8.771d-002,  &
     8.683d-002,8.597d-002,8.429d-002,8.267d-002,8.112d-002,7.962d-002,7.818d-002,7.678d-002,7.544d-002,7.415d-002,  &
     7.289d-002,7.168d-002,7.051d-002,6.938d-002,6.828d-002,6.722d-002,3.542d-005,5.978d-002,5.591d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,15),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.16150,0.159d0,0.15810,0.15710,  &
     0.15620,0.15530,0.15440,0.15350,0.15260,0.15180,0.15090,0.15d0,0.14920,0.14840,0.14760,0.14670,0.14630,0.14590,  &
     0.14550,0.14520,0.14480,0.14440,0.144d0,0.14360,0.14320,0.14280,0.14250,0.14210,0.14170,0.14130,0.141d0,0.14060,  &
     0.14020,0.13990,0.13950,0.13910,0.13880,0.13840,0.13810,0.13770,0.13730,0.137d0,0.13660,0.13630,0.136d0,0.13560,  &
     0.13530,0.13490,0.13430,0.13360,0.13290,0.13230,0.13160,0.131d0,0.13040,0.12970,0.12910,0.12850,0.12790,0.12730,  &
     0.12670,0.12610,0.12550,0.12490,0.12430,0.12380,0.12320,0.12260,0.12210,0.12150,0.121d0,0.12050,0.11990,0.11860,  &
     0.11730,0.11610,0.11480,0.11360,0.11240,0.11130,0.11010,0.109d0,0.10790,0.10580,0.10380,0.10190,9.997d-002,9.816d-002,  &
     9.641d-002,9.473d-002,9.310d-002,9.152d-002,9.000d-002,8.853d-002,8.711d-002,8.573d-002,8.440d-002,3.542d-005,  &
     7.505d-002,7.019d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,16),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.19840,0.19720,0.19610,  &
     0.19490,0.19370,0.19260,0.19150,0.19040,0.18930,0.18820,0.18720,0.18610,0.18510,0.184d0,0.183d0,0.18250,0.182d0,  &
     0.18150,0.181d0,0.18050,0.18d0,0.17960,0.17910,0.17860,0.17810,0.17760,0.17720,0.17670,0.17620,0.17580,0.17530,  &
     0.17480,0.17440,0.17390,0.17350,0.173d0,0.17260,0.17210,0.17170,0.17120,0.17080,0.17040,0.16990,0.16950,0.16910,  &
     0.16870,0.16820,0.16740,0.16660,0.16570,0.16490,0.16410,0.16330,0.16250,0.16170,0.16090,0.16020,0.15940,0.15870,  &
     0.15790,0.15720,0.15640,0.15570,0.155d0,0.15430,0.15360,0.15290,0.15220,0.15150,0.15080,0.15010,0.14950,0.14780,  &
     0.14620,0.14460,0.14310,0.14160,0.14010,0.13870,0.13730,0.13590,0.13450,0.13190,0.12940,0.12690,0.12460,0.12230,  &
     0.12010,0.118d0,0.116d0,0.11410,0.11220,0.11030,0.10850,0.10680,0.10520,3.542d-005,9.352d-002,8.746d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,17),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.21510,0.21380,0.21250,  &
     0.21130,0.21d0,0.20880,0.20760,0.20640,0.20520,0.204d0,0.20290,0.20180,0.20060,0.19950,0.199d0,0.19840,0.19790,  &
     0.19730,0.19680,0.19630,0.19570,0.19520,0.19470,0.19420,0.19360,0.19310,0.19260,0.19210,0.19160,0.19110,0.19060,  &
     0.19010,0.18960,0.18910,0.18860,0.18810,0.18760,0.18720,0.18670,0.18620,0.18570,0.18520,0.18480,0.18430,0.18380,  &
     0.18340,0.18250,0.18150,0.18060,0.17980,0.17890,0.178d0,0.17710,0.17630,0.17540,0.17460,0.17380,0.17290,0.17210,  &
     0.17130,0.17050,0.16970,0.16890,0.16820,0.16740,0.16660,0.16590,0.16510,0.16440,0.16360,0.16290,0.16110,0.15940,  &
     0.15770,0.156d0,0.15430,0.15270,0.15110,0.14960,0.14810,0.14660,0.14370,0.141d0,0.13830,0.13580,0.13330,0.13090,  &
     0.12860,0.12640,0.12430,0.12220,0.12020,0.11830,0.11640,0.11460,3.542d-005,0.10190,9.531d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,18),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.23290,0.23150,  &
     0.23010,0.22870,0.22740,0.22610,0.22480,0.22350,0.22220,0.221d0,0.21970,0.21850,0.21730,0.21670,0.21610,0.21550,  &
     0.21490,0.21430,0.21370,0.21310,0.21260,0.212d0,0.21140,0.21090,0.21030,0.20970,0.20920,0.20860,0.20810,0.20750,  &
     0.207d0,0.20640,0.20590,0.20540,0.20480,0.20430,0.20380,0.20330,0.20270,0.20220,0.20170,0.20120,0.20070,0.20020,  &
     0.19970,0.19870,0.19770,0.19670,0.19570,0.19480,0.19380,0.19290,0.19190,0.191d0,0.19010,0.18920,0.18830,0.18740,  &
     0.18650,0.18560,0.18480,0.18390,0.18310,0.18220,0.18140,0.18060,0.17980,0.179d0,0.17820,0.17740,0.17540,0.17350,  &
     0.17160,0.16980,0.168d0,0.16630,0.16450,0.16290,0.16120,0.15960,0.15650,0.15350,0.15060,0.14780,0.14510,0.14250,  &
     0.14d0,0.13760,0.13530,0.133d0,0.13090,0.12880,0.12670,0.12480,3.542d-005,0.11090,0.1037d0/
  DATA (DefaultSteamSuperheatedDensityData(i,19),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.25180,  &
     0.25030,0.24890,0.24740,0.246d0,0.24450,0.24310,0.24170,0.24040,0.239d0,0.23770,0.23640,0.23570,0.23510,0.23440,  &
     0.23380,0.23310,0.23250,0.23190,0.23120,0.23060,0.23d0,0.22940,0.22880,0.22810,0.22750,0.22690,0.22630,0.22570,  &
     0.22510,0.22460,0.224d0,0.22340,0.22280,0.22220,0.22160,0.22110,0.22050,0.21990,0.21940,0.21880,0.21830,0.21770,  &
     0.21720,0.21610,0.215d0,0.21390,0.21290,0.21180,0.21080,0.20970,0.20870,0.20770,0.20670,0.20570,0.20480,0.20380,  &
     0.20280,0.20190,0.201d0,0.2d0,0.19910,0.19820,0.19730,0.19640,0.19550,0.19460,0.19370,0.19290,0.19080,0.18870,  &
     0.18660,0.18470,0.18270,0.18080,0.17890,0.17710,0.17530,0.17360,0.17020,0.16690,0.16370,0.16070,0.15780,0.155d0,  &
     0.15230,0.14960,0.14710,0.14470,0.14230,0.14d0,0.13780,0.13560,3.542d-005,0.12060,0.1128d0/
  DATA (DefaultSteamSuperheatedDensityData(i,20),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.27210,0.27050,0.26890,0.26730,0.26580,0.26420,0.26270,0.26120,0.25970,0.25830,0.25680,0.25610,0.25540,0.25470,  &
     0.254d0,0.25330,0.25260,0.25190,0.25130,0.25060,0.24990,0.24920,0.24860,0.24790,0.24720,0.24660,0.24590,0.24530,  &
     0.24460,0.244d0,0.24330,0.24270,0.24210,0.24140,0.24080,0.24020,0.23960,0.239d0,0.23840,0.23770,0.23710,0.23650,  &
     0.23590,0.23480,0.23360,0.23240,0.23130,0.23010,0.229d0,0.22790,0.22680,0.22570,0.22460,0.22350,0.22250,0.22140,  &
     0.22040,0.21930,0.21830,0.21730,0.21630,0.21530,0.21430,0.21330,0.21240,0.21140,0.21050,0.20950,0.20720,0.205d0,  &
     0.20270,0.20060,0.19850,0.19640,0.19440,0.19240,0.19040,0.18850,0.18480,0.18130,0.17790,0.17460,0.17140,0.16830,  &
     0.16540,0.16250,0.15980,0.15710,0.15460,0.15210,0.14970,0.14730,3.542d-005,0.131d0,0.1225d0/
  DATA (DefaultSteamSuperheatedDensityData(i,21),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.29370,0.29190,0.29020,0.28850,0.28690,0.28520,0.28360,0.282d0,0.28040,0.27880,0.278d0,0.27730,0.27650,0.27570,  &
     0.275d0,0.27420,0.27350,0.27270,0.272d0,0.27130,0.27050,0.26980,0.26910,0.26840,0.26760,0.26690,0.26620,0.26550,  &
     0.26480,0.26410,0.26340,0.26280,0.26210,0.26140,0.26070,0.26d0,0.25940,0.25870,0.258d0,0.25740,0.25670,0.25610,  &
     0.25480,0.25350,0.25220,0.251d0,0.24980,0.24850,0.24730,0.24610,0.24490,0.24370,0.24260,0.24140,0.24030,0.23910,  &
     0.238d0,0.23690,0.23580,0.23470,0.23360,0.23260,0.23150,0.23050,0.22940,0.22840,0.22740,0.22490,0.22240,0.22d0,  &
     0.21770,0.21540,0.21310,0.21090,0.20880,0.20660,0.20460,0.20060,0.19670,0.193d0,0.18940,0.186d0,0.18270,0.17950,  &
     0.17640,0.17340,0.17050,0.16770,0.165d0,0.16240,0.15990,3.542d-005,0.14210,  &
     0.1329d0/
  DATA (DefaultSteamSuperheatedDensityData(i,22),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.31660,0.31480,0.31290,0.31110,0.30930,0.30760,0.30580,0.30410,0.30240,0.30150,0.30070,0.29990,0.299d0,0.29820,  &
     0.29740,0.29660,0.29580,0.295d0,0.29420,0.29340,0.29260,0.29180,0.291d0,0.29020,0.28940,0.28870,0.28790,0.28720,  &
     0.28640,0.28560,0.28490,0.28420,0.28340,0.28270,0.282d0,0.28120,0.28050,0.27980,0.27910,0.27840,0.27760,0.27620,  &
     0.27490,0.27350,0.27210,0.27080,0.26940,0.26810,0.26680,0.26550,0.26430,0.263d0,0.26170,0.26050,0.25930,0.258d0,  &
     0.25680,0.25560,0.25450,0.25330,0.25210,0.251d0,0.24980,0.24870,0.24760,0.24650,0.24380,0.24110,0.23850,0.23590,  &
     0.23350,0.231d0,0.22860,0.22630,0.224d0,0.22170,0.21740,0.21320,0.20920,0.20530,0.20160,0.198d0,0.19450,0.19120,  &
     0.18790,0.18480,0.18180,0.17880,0.176d0,0.17330,3.542d-005,0.154d0,  &
     0.1441d0/
  DATA (DefaultSteamSuperheatedDensityData(i,23),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.34110,0.33910,0.33710,0.33520,0.33320,0.33130,0.32940,0.32760,0.32670,0.32580,0.32490,0.324d0,0.32310,  &
     0.32220,0.32130,0.32040,0.31950,0.31870,0.31780,0.31690,0.31610,0.31520,0.31440,0.31350,0.31270,0.31190,0.31110,  &
     0.31020,0.30940,0.30860,0.30780,0.307d0,0.30620,0.30540,0.30460,0.30380,0.30310,0.30230,0.30150,0.30070,0.29920,  &
     0.29770,0.29620,0.29470,0.29330,0.29180,0.29040,0.289d0,0.28760,0.28620,0.28480,0.28350,0.28210,0.28080,0.27950,  &
     0.27820,0.27690,0.27560,0.27430,0.27310,0.27180,0.27060,0.26930,0.26810,0.26690,0.264d0,0.26110,0.25830,0.25550,  &
     0.25280,0.25020,0.24760,0.245d0,0.24260,0.24010,0.23540,0.23090,0.22650,0.22230,0.21830,0.21440,0.21060,0.207d0,  &
     0.20350,0.20010,0.19680,0.19360,0.19060,0.18760,3.542d-005,0.16680,  &
     0.156d0/
  DATA (DefaultSteamSuperheatedDensityData(i,24),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.36710,0.36490,0.36280,0.36070,0.35860,0.35660,0.35460,0.35360,0.35260,0.35160,0.35060,0.34960,  &
     0.34870,0.34770,0.34680,0.34580,0.34490,0.34390,0.343d0,0.34210,0.34110,0.34020,0.33930,0.33840,0.33750,0.33660,  &
     0.33570,0.33480,0.334d0,0.33310,0.33220,0.33130,0.33050,0.32960,0.32880,0.32790,0.32710,0.32630,0.32540,0.32380,  &
     0.32210,0.32050,0.31890,0.31730,0.31580,0.31420,0.31270,0.31120,0.30970,0.30820,0.30670,0.30520,0.30380,0.30240,  &
     0.30090,0.29950,0.29820,0.29680,0.29540,0.29410,0.29270,0.29140,0.29010,0.28880,0.28560,0.28250,0.27940,0.27640,  &
     0.27350,0.27060,0.26780,0.26510,0.26240,0.25980,0.25460,0.24970,0.245d0,0.24050,0.23610,0.23190,0.22780,0.22390,  &
     0.22010,0.21640,0.21290,0.20940,0.20610,0.20290,3.542d-005,0.18040,  &
     0.1687d0/
  DATA (DefaultSteamSuperheatedDensityData(i,25),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.39460,0.39230,0.39010,0.38780,0.38560,0.38340,0.38230,0.38120,0.38020,0.37910,0.37810,  &
     0.377d0,0.376d0,0.37490,0.37390,0.37290,0.37190,0.37080,0.36980,0.36880,0.36780,0.36690,0.36590,0.36490,0.36390,  &
     0.363d0,0.362d0,0.361d0,0.36010,0.35920,0.35820,0.35730,0.35640,0.35540,0.35450,0.35360,0.35270,0.35180,0.35d0,  &
     0.34820,0.34650,0.34470,0.343d0,0.34130,0.33970,0.338d0,0.33640,0.33470,0.33310,0.33150,0.32990,0.32840,0.32680,  &
     0.32530,0.32380,0.32230,0.32080,0.31930,0.31780,0.31640,0.315d0,0.31350,0.31210,0.30870,0.30530,0.302d0,0.29870,  &
     0.29560,0.29250,0.28940,0.28650,0.28360,0.28070,0.27520,0.26990,0.26480,0.25990,0.25510,0.25060,0.24620,0.24190,  &
     0.23780,0.23390,0.23d0,0.22630,0.22270,0.21930,3.542d-005,0.19490,  &
     0.1823d0/
  DATA (DefaultSteamSuperheatedDensityData(i,26),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.42390,0.42140,0.419d0,0.41660,0.41420,0.413d0,0.41190,0.41070,0.40960,0.40840,0.40730,  &
     0.40610,0.405d0,0.40390,0.40280,0.40170,0.40060,0.39950,0.39840,0.39730,0.39630,0.39520,0.39410,0.39310,0.392d0,  &
     0.391d0,0.39d0,0.38890,0.38790,0.38690,0.38590,0.38490,0.38390,0.38290,0.38190,0.38090,0.37990,0.378d0,0.37610,  &
     0.37420,0.37230,0.37050,0.36860,0.36680,0.365d0,0.36320,0.36150,0.35970,0.358d0,0.35630,0.35460,0.35290,0.35130,  &
     0.34960,0.348d0,0.34640,0.34480,0.34320,0.34160,0.34010,0.33860,0.337d0,0.33330,0.32960,0.32610,0.32260,0.31910,  &
     0.31580,0.31250,0.30930,0.30620,0.30310,0.29710,0.29140,0.28590,0.28060,0.27540,0.27050,0.26580,0.26120,0.25680,  &
     0.25250,0.24830,0.24430,0.24050,0.23670,3.542d-005,0.21040,  &
     0.1968d0/
  DATA (DefaultSteamSuperheatedDensityData(i,27),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.45490,0.45230,0.44970,0.44710,0.44580,0.44450,0.44330,0.442d0,0.44080,0.43960,  &
     0.43830,0.43710,0.43590,0.43470,0.43350,0.43230,0.43110,0.43d0,0.42880,0.42760,0.42650,0.42530,0.42420,0.42310,  &
     0.42190,0.42080,0.41970,0.41860,0.41750,0.41640,0.41530,0.41420,0.41320,0.41210,0.411d0,0.41d0,0.40790,0.40580,  &
     0.40380,0.40170,0.39970,0.39770,0.39580,0.39380,0.39190,0.39d0,0.38810,0.38620,0.38440,0.38260,0.38080,0.379d0,  &
     0.37720,0.37540,0.37370,0.372d0,0.37030,0.36860,0.36690,0.36520,0.36360,0.35950,0.35560,0.35170,0.34790,0.34420,  &
     0.34060,0.33710,0.33360,0.33020,0.32690,0.32050,0.31430,0.30830,0.30260,0.29710,0.29180,0.28660,0.28170,0.27690,  &
     0.27230,0.26780,0.26350,0.25930,0.25530,3.542d-005,0.22690,  &
     0.2122d0/
  DATA (DefaultSteamSuperheatedDensityData(i,28),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.48780,0.48490,0.48210,0.48080,0.47940,0.478d0,0.47670,0.47530,0.474d0,  &
     0.47270,0.47130,0.47d0,0.46870,0.46740,0.46620,0.46490,0.46360,0.46230,0.46110,0.45980,0.45860,0.45740,0.45610,  &
     0.45490,0.45370,0.45250,0.45130,0.45010,0.44890,0.44780,0.44660,0.44540,0.44430,0.44310,0.442d0,0.43970,0.43750,  &
     0.43530,0.43310,0.43090,0.42870,0.42660,0.42450,0.42240,0.42040,0.41830,0.41630,0.41430,0.41240,0.41040,0.40850,  &
     0.40650,0.40460,0.40280,0.40090,0.39910,0.39720,0.39540,0.39360,0.39190,0.38750,0.38320,0.37910,0.375d0,0.371d0,  &
     0.36710,0.36330,0.35950,0.35590,0.35230,0.34530,0.33870,0.33230,0.32610,0.32010,0.31440,0.30890,0.30350,0.29840,  &
     0.29340,0.28860,0.28390,0.27940,0.27510,3.542d-005,0.24450,  &
     0.2287d0/
  DATA (DefaultSteamSuperheatedDensityData(i,29),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.52250,0.51950,0.518d0,0.51650,0.51510,0.51360,0.51210,0.51070,  &
     0.50920,0.50780,0.50640,0.505d0,0.50360,0.50220,0.50080,0.49940,0.49810,0.49670,0.49540,0.494d0,0.49270,0.49140,  &
     0.49010,0.48870,0.48740,0.48610,0.48490,0.48360,0.48230,0.481d0,0.47980,0.47850,0.47730,0.47610,0.47360,0.47120,  &
     0.46880,0.46640,0.46410,0.46180,0.45950,0.45720,0.455d0,0.45270,0.45050,0.44840,0.44620,0.44410,0.442d0,0.43990,  &
     0.43780,0.43580,0.43370,0.43170,0.42970,0.42780,0.42580,0.42390,0.422d0,0.41730,0.41270,0.40820,0.40380,0.39950,  &
     0.39530,0.39110,0.38710,0.38320,0.37930,0.37180,0.36460,0.35770,0.35110,0.34460,0.33850,0.33250,0.32680,0.32120,  &
     0.31590,0.31070,0.30570,0.30080,0.29610,3.542d-005,0.26320,  &
     0.2461d0/
  DATA (DefaultSteamSuperheatedDensityData(i,30),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.55930,0.55770,0.55610,0.55450,0.55290,0.55130,0.54980,0.54820,  &
     0.54670,0.54510,0.54360,0.54210,0.54060,0.53910,0.53760,0.53610,0.53460,0.53320,0.53170,0.53030,0.52890,0.52740,  &
     0.526d0,0.52460,0.52320,0.52180,0.52050,0.51910,0.51770,0.51640,0.515d0,0.51370,0.51230,0.50970,0.50710,0.50450,  &
     0.50190,0.49940,0.49690,0.49440,0.492d0,0.48960,0.48720,0.48480,0.48240,0.48010,0.47780,0.47550,0.47330,0.47110,  &
     0.46880,0.46670,0.46450,0.46230,0.46020,0.45810,0.456d0,0.454d0,0.44890,0.44390,0.43910,0.43440,0.42970,0.42520,  &
     0.42080,0.41640,0.41220,0.408d0,0.4d0,0.39220,0.38480,0.37760,0.37070,0.36410,0.35760,0.35150,0.34550,0.33970,  &
     0.33410,0.32870,0.32350,0.31850,3.542d-005,0.28310,  &
     0.2647d0/
  DATA (DefaultSteamSuperheatedDensityData(i,31),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.57850,0.57680,0.57510,0.57350,0.57180,0.57020,0.56860,  &
     0.567d0,0.56540,0.56380,0.56220,0.56070,0.55910,0.55760,0.556d0,0.55450,0.553d0,0.55150,0.55d0,0.54850,0.547d0,  &
     0.54550,0.54410,0.54260,0.54120,0.53980,0.53830,0.53690,0.53550,0.53410,0.53270,0.53130,0.52860,0.52590,0.52320,  &
     0.52050,0.51790,0.51530,0.51270,0.51020,0.50770,0.50520,0.50270,0.50030,0.49790,0.49550,0.49310,0.49080,0.48850,  &
     0.48620,0.48390,0.48160,0.47940,0.47720,0.475d0,0.47290,0.47070,0.46550,0.46030,0.45530,0.45040,0.44560,0.44090,  &
     0.43630,0.43180,0.42740,0.423d0,0.41470,0.40660,0.39890,0.39150,0.38430,0.37740,0.37080,0.36440,0.35820,0.35220,  &
     0.34640,0.34080,0.33540,0.33020,3.542d-005,0.29350,  &
     0.2744d0/
  DATA (DefaultSteamSuperheatedDensityData(i,32),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.59820,0.59640,0.59470,0.593d0,0.59130,0.58960,  &
     0.588d0,0.58630,0.58470,0.583d0,0.58140,0.57980,0.57820,0.57660,0.575d0,0.57340,0.57180,0.57030,0.56870,0.56720,  &
     0.56570,0.56420,0.56270,0.56120,0.55970,0.55820,0.55670,0.55520,0.55380,0.55230,0.55090,0.548d0,0.54520,0.54240,  &
     0.53970,0.53690,0.53420,0.53160,0.52890,0.52630,0.52370,0.52120,0.51870,0.51620,0.51370,0.51120,0.50880,0.50640,  &
     0.504d0,0.50170,0.49930,0.497d0,0.49470,0.49250,0.49020,0.488d0,0.48250,0.47720,0.472d0,0.46690,0.46190,0.457d0,  &
     0.45220,0.44760,0.443d0,0.43850,0.42980,0.42150,0.41350,0.40580,0.39840,0.39120,0.38430,0.37770,0.37130,0.36510,  &
     0.35910,0.35330,0.34760,0.34220,3.542d-005,0.30420,  &
     0.2844d0/
  DATA (DefaultSteamSuperheatedDensityData(i,33),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.61840,0.61660,0.61480,0.61310,0.61130,  &
     0.60960,0.60790,0.60620,0.60450,0.60280,0.60110,0.59940,0.59780,0.59610,0.59450,0.59280,0.59120,0.58960,0.588d0,  &
     0.58640,0.58490,0.58330,0.58170,0.58020,0.57860,0.57710,0.57560,0.57410,0.57260,0.57110,0.56810,0.56520,0.56230,  &
     0.55940,0.55660,0.55380,0.551d0,0.54830,0.54560,0.54290,0.54020,0.53760,0.535d0,0.53240,0.52990,0.52740,0.52490,  &
     0.52240,0.52d0,0.51750,0.51510,0.51280,0.51040,0.50810,0.50580,0.50010,0.49460,0.48920,0.48390,0.47870,0.47360,  &
     0.46870,0.46390,0.45910,0.45450,0.44550,0.43680,0.42850,0.42050,0.41290,0.40540,0.39830,0.39140,0.38470,0.37830,  &
     0.37210,0.36610,0.36030,0.35460,3.542d-005,0.31520,  &
     0.2948d0/
  DATA (DefaultSteamSuperheatedDensityData(i,34),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.63920,0.63740,0.63550,0.63370,0.63190,  &
     0.63010,0.62830,0.62660,0.62480,0.623d0,0.62130,0.61960,0.61790,0.61620,0.61450,0.61280,0.61110,0.60950,0.60780,  &
     0.60620,0.60460,0.60290,0.60130,0.59970,0.59810,0.59660,0.595d0,0.59340,0.59190,0.58880,0.58580,0.58270,0.57980,  &
     0.57680,0.57390,0.571d0,0.56820,0.56540,0.56260,0.55990,0.55710,0.55440,0.55180,0.54910,0.54650,0.54390,0.54140,  &
     0.53880,0.53630,0.53380,0.53140,0.52890,0.52650,0.52410,0.51820,0.51250,0.50690,0.50140,0.496d0,0.49080,0.48570,  &
     0.48060,0.47570,0.47090,0.46160,0.45260,0.444d0,0.43570,0.42780,0.42010,0.41270,0.40550,0.39860,0.392d0,0.38550,  &
     0.37930,0.37330,0.36740,3.542d-005,0.32660,  &
     0.3054d0/
  DATA (DefaultSteamSuperheatedDensityData(i,35),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.66060,0.65870,0.65680,0.65490,  &
     0.653d0,0.65120,0.64930,0.64750,0.64570,0.64390,0.64210,0.64030,0.63850,0.63680,0.635d0,0.63330,0.63160,0.62990,  &
     0.62820,0.62650,0.62480,0.62310,0.62150,0.61980,0.61820,0.61650,0.61490,0.61330,0.61010,0.607d0,0.60380,0.60070,  &
     0.59770,0.59470,0.59170,0.58870,0.58580,0.58290,0.58010,0.57720,0.57440,0.57170,0.56890,0.56620,0.56350,0.56090,  &
     0.55820,0.55560,0.55310,0.55050,0.548d0,0.54550,0.543d0,0.53690,0.53090,0.52510,0.51940,0.51390,0.50840,0.50310,  &
     0.49790,0.49280,0.48780,0.47820,0.46890,0.46d0,0.45140,0.44310,0.43510,0.42750,0.42010,0.41290,0.406d0,0.39930,  &
     0.39290,0.38660,0.38060,3.542d-005,0.33830,  &
     0.3163d0/
  DATA (DefaultSteamSuperheatedDensityData(i,36),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.68250,0.68050,0.67860,  &
     0.67660,0.67470,0.67280,0.67090,0.669d0,0.66710,0.66530,0.66340,0.66160,0.65980,0.658d0,0.65620,0.65440,0.65260,  &
     0.65080,0.64910,0.64730,0.64560,0.64390,0.64210,0.64040,0.63870,0.63710,0.63540,0.63210,0.62880,0.62550,0.62230,  &
     0.61920,0.616d0,0.61290,0.60990,0.60690,0.60390,0.60090,0.598d0,0.59510,0.59220,0.58930,0.58650,0.58370,0.581d0,  &
     0.57830,0.57560,0.57290,0.57020,0.56760,0.565d0,0.56240,0.55610,0.54990,0.54390,0.538d0,0.53230,0.52660,0.52110,  &
     0.51570,0.51040,0.50530,0.49520,0.48560,0.47640,0.46750,0.45890,0.45070,0.44270,0.435d0,0.42760,0.42050,0.41360,  &
     0.40690,0.40040,0.39410,3.542d-005,0.35030,  &
     0.3276d0/
  DATA (DefaultSteamSuperheatedDensityData(i,37),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.705d0,0.703d0,  &
     0.701d0,0.699d0,0.697d0,0.695d0,0.69310,0.69110,0.68920,0.68730,0.68530,0.68350,0.68160,0.67970,0.67780,0.676d0,  &
     0.67420,0.67230,0.67050,0.66870,0.66690,0.66510,0.66340,0.66160,0.65990,0.65810,0.65470,0.65130,0.64790,0.64460,  &
     0.64130,0.63810,0.63480,0.63170,0.62850,0.62540,0.62230,0.61930,0.61630,0.61330,0.61040,0.60740,0.60460,0.60170,  &
     0.59890,0.59610,0.59330,0.59050,0.58780,0.58510,0.58250,0.57590,0.56950,0.56330,0.55710,0.55120,0.54530,0.53960,  &
     0.534d0,0.52860,0.52320,0.51280,0.50280,0.49330,0.484d0,0.47520,0.46660,0.45840,0.45050,0.44280,0.43540,0.42820,  &
     0.42130,0.41460,0.40810,3.542d-005,0.36270,  &
     0.3391d0/
  DATA (DefaultSteamSuperheatedDensityData(i,38),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.72820,0.72610,  &
     0.724d0,0.72190,0.71990,0.71780,0.71580,0.71380,0.71180,0.70980,0.70790,0.70590,0.704d0,0.702d0,0.70010,0.69820,  &
     0.69630,0.69440,0.69250,0.69070,0.68880,0.687d0,0.68520,0.68340,0.68160,0.678d0,0.67450,0.671d0,0.66750,0.66410,  &
     0.66070,0.65740,0.65410,0.65080,0.64760,0.64440,0.64130,0.63810,0.63510,0.632d0,0.629d0,0.626d0,0.623d0,0.62010,  &
     0.61720,0.61430,0.61150,0.60860,0.60580,0.60310,0.59630,0.58960,0.58320,0.57680,0.57060,0.56460,0.55870,0.55290,  &
     0.54720,0.54170,0.53090,0.52060,0.51060,0.50110,0.49190,0.48310,0.47450,0.46630,0.45840,0.45070,0.44330,0.43610,  &
     0.42920,0.42240,3.542d-005,0.37540,  &
     0.3511d0/
  DATA (DefaultSteamSuperheatedDensityData(i,39),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.75190,  &
     0.74970,0.74760,0.74550,0.74330,0.74120,0.73920,0.73710,0.735d0,0.733d0,0.73090,0.72890,0.72690,0.72490,0.723d0,  &
     0.721d0,0.719d0,0.71710,0.71520,0.71320,0.71130,0.70940,0.70760,0.70570,0.702d0,0.69830,0.69470,0.69110,0.68760,  &
     0.68410,0.68060,0.67720,0.67380,0.67050,0.66720,0.66390,0.66060,0.65740,0.65430,0.65110,0.648d0,0.645d0,0.64190,  &
     0.63890,0.63590,0.633d0,0.63010,0.62720,0.62430,0.61730,0.61040,0.60370,0.59710,0.59070,0.58440,0.57830,0.57230,  &
     0.56640,0.56070,0.54950,0.53880,0.52850,0.51870,0.50910,0.5d0,0.49120,0.48260,0.47440,0.46650,0.45880,0.45140,  &
     0.44420,0.43720,3.542d-005,0.38860,  &
     0.3633d0/
  DATA (DefaultSteamSuperheatedDensityData(i,40),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.77630,0.774d0,0.77180,0.76960,0.76740,0.76530,0.76310,0.761d0,0.75890,0.75670,0.75470,0.75260,0.75050,0.74840,  &
     0.74640,0.74440,0.74240,0.74040,0.73840,0.73640,0.73440,0.73250,0.73050,0.72670,0.72290,0.71910,0.71540,0.71170,  &
     0.70810,0.70450,0.701d0,0.69750,0.694d0,0.69060,0.68720,0.68380,0.68050,0.67720,0.674d0,0.67070,0.66760,0.66440,  &
     0.66130,0.65820,0.65510,0.65210,0.64910,0.64610,0.63880,0.63170,0.62480,0.618d0,0.61130,0.60480,0.59850,0.59230,  &
     0.58620,0.58020,0.56870,0.55760,0.547d0,0.53670,0.52690,0.51740,0.50820,0.49940,0.49090,0.48270,0.47470,0.46710,  &
     0.45960,0.45240,3.542d-005,0.40210,  &
     0.3759d0/
  DATA (DefaultSteamSuperheatedDensityData(i,41),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.80130,0.799d0,0.79670,0.79440,0.79220,0.78990,0.78770,0.78550,0.78330,0.78110,0.779d0,0.77680,0.77470,0.77260,  &
     0.77050,0.76840,0.76630,0.76420,0.76220,0.76010,0.75810,0.75610,0.75210,0.74820,0.74430,0.74040,0.73660,0.73280,  &
     0.72910,0.72540,0.72180,0.71820,0.71470,0.71110,0.70770,0.70420,0.70080,0.69740,0.69410,0.69080,0.68750,0.68430,  &
     0.68110,0.67790,0.67480,0.67170,0.66860,0.661d0,0.65370,0.64650,0.63940,0.63250,0.62580,0.61920,0.61280,0.60650,  &
     0.60030,0.58840,0.57690,0.56590,0.55530,0.54510,0.53530,0.52580,0.51670,0.50790,0.49940,0.49110,0.48320,0.47550,  &
     0.468d0,3.542d-005,0.41590,  &
     0.3889d0/
  DATA (DefaultSteamSuperheatedDensityData(i,42),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.82690,0.82460,0.82220,0.81990,0.81750,0.81520,0.81290,0.81070,0.80840,0.80620,0.80390,0.80170,0.79950,  &
     0.79730,0.79520,0.793d0,0.79090,0.78870,0.78660,0.78450,0.78240,0.77830,0.77420,0.77010,0.76610,0.76220,0.75830,  &
     0.75440,0.75060,0.74690,0.74310,0.73940,0.73580,0.73220,0.72860,0.72510,0.72160,0.71810,0.71470,0.71130,0.708d0,  &
     0.70470,0.70140,0.69810,0.69490,0.69170,0.68390,0.67630,0.66880,0.66150,0.65440,0.64740,0.64060,0.63390,0.62740,  &
     0.621d0,0.60870,0.59680,0.58540,0.57440,0.56390,0.55370,0.54390,0.53450,0.52530,0.51650,0.508d0,0.49980,0.49180,  &
     0.48410,3.542d-005,0.43020,  &
     0.4023d0/
  DATA (DefaultSteamSuperheatedDensityData(i,43),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.85320,0.85080,0.84840,0.846d0,0.84360,0.84120,0.83880,0.83650,0.83410,0.83180,0.82950,0.82730,  &
     0.825d0,0.82270,0.82050,0.81830,0.81610,0.81390,0.81170,0.80950,0.80520,0.801d0,0.79680,0.79260,0.78850,0.78450,  &
     0.78050,0.77650,0.77260,0.76880,0.76490,0.76120,0.75740,0.75370,0.75010,0.74650,0.74290,0.73930,0.73580,0.73240,  &
     0.72890,0.72550,0.72210,0.71880,0.71550,0.70740,0.69950,0.69180,0.68420,0.67680,0.66960,0.66260,0.65570,0.64890,  &
     0.64230,0.62950,0.61720,0.60540,0.59410,0.58310,0.57260,0.56250,0.55270,0.54330,0.53420,0.52540,0.51690,0.50860,  &
     0.50060,3.542d-005,0.44490,  &
     0.416d0/
  DATA (DefaultSteamSuperheatedDensityData(i,44),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.88020,0.87770,0.87520,0.87270,0.87030,0.86780,0.86540,0.86290,0.86050,0.85820,0.85580,  &
     0.85340,0.85110,0.84880,0.84650,0.84420,0.84190,0.83960,0.83740,0.83290,0.82850,0.82420,0.81990,0.81560,0.81140,  &
     0.80730,0.80320,0.79920,0.79510,0.79120,0.78730,0.78340,0.77960,0.77580,0.772d0,0.76830,0.76460,0.761d0,0.75740,  &
     0.75390,0.75030,0.74680,0.74340,0.74d0,0.73160,0.72340,0.71540,0.70760,0.69990,0.69240,0.68510,0.678d0,0.671d0,  &
     0.66420,0.65090,0.63820,0.626d0,0.61430,0.603d0,0.59210,0.58160,0.57150,0.56170,0.55230,0.54320,0.53440,0.52590,  &
     0.51760,3.542d-005,0.46d0,  &
     0.4301d0/
  DATA (DefaultSteamSuperheatedDensityData(i,45),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.90790,0.90530,0.90270,0.90020,0.89760,0.89510,0.89260,0.89010,0.88760,0.88520,0.88270,  &
     0.88030,0.87790,0.87550,0.87310,0.87070,0.86840,0.86610,0.86140,0.85690,0.85240,0.84790,0.84350,0.83920,0.83490,  &
     0.83060,0.82640,0.82230,0.81820,0.81410,0.81010,0.80610,0.80220,0.79830,0.79450,0.79070,0.78690,0.78320,0.77950,  &
     0.77590,0.77220,0.76870,0.76510,0.75640,0.74790,0.73970,0.73160,0.72370,0.71590,0.70840,0.701d0,0.69380,0.68670,  &
     0.673d0,0.65980,0.64720,0.635d0,0.62340,0.61210,0.60130,0.59080,0.58070,0.571d0,0.56150,0.55240,0.54360,0.53510,  &
     3.542d-005,0.47550,  &
     0.4446d0/
  DATA (DefaultSteamSuperheatedDensityData(i,46),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.93630,0.93360,0.931d0,0.92830,0.92570,0.92310,0.92050,0.91790,0.91540,0.91280,  &
     0.91030,0.90780,0.90530,0.90290,0.90040,0.898d0,0.89560,0.89080,0.886d0,0.88140,0.87680,0.87220,0.86770,0.86320,  &
     0.85880,0.85450,0.85020,0.84590,0.84170,0.83760,0.83340,0.82940,0.82540,0.82140,0.81740,0.81350,0.80970,0.80590,  &
     0.80210,0.79840,0.79460,0.791d0,0.782d0,0.77320,0.76460,0.75620,0.74810,0.74010,0.73220,0.72460,0.71710,0.70980,  &
     0.69560,0.682d0,0.66890,0.65640,0.64430,0.63270,0.62150,0.61060,0.60020,0.59010,0.58040,0.571d0,0.56190,0.553d0,  &
     3.542d-005,0.49140,  &
     0.4594d0/
  DATA (DefaultSteamSuperheatedDensityData(i,47),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.96540,0.96260,0.95990,0.95720,0.95450,0.95180,0.94910,0.94650,0.94380,  &
     0.94120,0.93860,0.93610,0.93350,0.93090,0.92840,0.92590,0.92090,0.916d0,0.91120,0.90640,0.90170,0.897d0,0.89240,  &
     0.88780,0.88330,0.87890,0.87450,0.87010,0.86580,0.86150,0.85730,0.85320,0.849d0,0.845d0,0.84090,0.83690,0.833d0,  &
     0.82910,0.82520,0.82140,0.81760,0.80830,0.79920,0.79030,0.78160,0.77310,0.76490,0.75680,0.74890,0.74110,0.73360,  &
     0.71890,0.70480,0.69130,0.67830,0.66580,0.65380,0.64220,0.631d0,0.62020,0.60980,0.59970,0.59d0,0.58060,0.57150,  &
     3.542d-005,0.50780,  &
     0.4747d0/
  DATA (DefaultSteamSuperheatedDensityData(i,48),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.99520,0.99240,0.98950,0.98670,0.984d0,0.98120,0.97840,0.97570,  &
     0.973d0,0.97030,0.96760,0.965d0,0.96230,0.95970,0.95710,0.952d0,0.94690,0.94190,0.93690,0.932d0,0.92720,0.92240,  &
     0.91770,0.913d0,0.90840,0.90380,0.89930,0.89480,0.89040,0.88610,0.88170,0.87750,0.87320,0.86910,0.86490,0.86080,  &
     0.85680,0.85280,0.84880,0.84490,0.83520,0.82580,0.81670,0.80770,0.79890,0.79040,0.782d0,0.77380,0.76580,0.758d0,  &
     0.74280,0.72830,0.71430,0.70090,0.68790,0.67550,0.66350,0.652d0,0.64080,0.63d0,0.61960,0.60960,0.59980,0.59040,  &
     3.542d-005,0.52460,  &
     0.4905d0/
  DATA (DefaultSteamSuperheatedDensityData(i,49),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.026d0,1.023d0,1.02d0,1.017d0,1.014d0,1.011d0,1.008d0,1.006d0,  &
     1.003d0,1.0d0,0.99740,0.99460,0.99190,0.98920,0.98390,0.97860,0.97340,0.96830,0.96320,0.95820,0.95320,0.94830,0.94350,  &
     0.93870,0.934d0,0.92930,0.92470,0.92010,0.91560,0.91110,0.90670,0.90230,0.898d0,0.89370,0.88950,0.88530,0.88110,  &
     0.877d0,0.873d0,0.863d0,0.85330,0.84380,0.83450,0.82540,0.81660,0.80790,0.79940,0.79120,0.78310,0.76740,0.75230,  &
     0.73790,0.724d0,0.71060,0.69780,0.68540,0.67350,0.66190,0.65080,0.64010,0.62970,0.61960,0.60990,3.542d-005,0.54180,  &
     0.5066d0/
  DATA (DefaultSteamSuperheatedDensityData(i,50),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.057d0,1.054d0,1.051d0,1.048d0,1.045d0,1.042d0,1.039d0,  &
     1.036d0,1.034d0,1.031d0,1.028d0,1.025d0,1.022d0,1.017d0,1.011d0,1.006d0,1.0d0,0.99520,0.99d0,0.98490,0.97980,0.97480,  &
     0.96990,0.965d0,0.96010,0.95530,0.95060,0.94590,0.94130,0.93670,0.93220,0.92770,0.92330,0.91890,0.91460,0.91030,  &
     0.906d0,0.90180,0.89150,0.88140,0.87160,0.862d0,0.85260,0.84350,0.83450,0.82580,0.81720,0.80880,0.79260,0.77710,  &
     0.76210,0.74780,0.734d0,0.72070,0.70790,0.69550,0.68360,0.67210,0.661d0,0.65030,0.63990,0.62980,3.542d-005,0.55960,  &
     0.5232d0/
  DATA (DefaultSteamSuperheatedDensityData(i,51),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.089d0,1.086d0,1.083d0,1.08d0,1.077d0,1.074d0,  &
     1.071d0,1.068d0,1.065d0,1.062d0,1.059d0,1.056d0,1.05d0,1.045d0,1.039d0,1.034d0,1.028d0,1.023d0,1.017d0,1.012d0,  &
     1.007d0,1.002d0,0.99680,0.99180,0.98680,0.982d0,0.97710,0.97230,0.96760,0.96290,0.95830,0.95370,0.94910,0.94470,  &
     0.94020,0.93580,0.93150,0.92080,0.91040,0.90020,0.89030,0.88060,0.87110,0.86190,0.85280,0.844d0,0.83530,0.81850,  &
     0.80250,0.787d0,0.77220,0.75790,0.74420,0.731d0,0.71820,0.70590,0.694d0,0.68260,0.67150,0.66070,0.65030,3.542d-005,  &
     0.57780,  &
     0.5402d0/
  DATA (DefaultSteamSuperheatedDensityData(i,52),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.122d0,1.119d0,1.116d0,1.113d0,1.109d0,  &
     1.106d0,1.103d0,1.1d0,1.097d0,1.094d0,1.091d0,1.085d0,1.079d0,1.073d0,1.068d0,1.062d0,1.056d0,1.051d0,1.045d0,  &
     1.04d0,1.035d0,1.03d0,1.024d0,1.019d0,1.014d0,1.009d0,1.004d0,0.99930,0.99440,0.98960,0.98490,0.98020,0.97560,  &
     0.971d0,0.96640,0.96190,0.95090,0.94010,0.92960,0.91930,0.90930,0.89950,0.88990,0.88060,0.87140,0.86250,0.84510,  &
     0.82850,0.81260,0.79730,0.78250,0.76830,0.75470,0.74150,0.72880,0.71650,0.70470,0.69320,0.68210,0.67140,3.542d-005,  &
     0.59640,  &
     0.5576d0/
  DATA (DefaultSteamSuperheatedDensityData(i,53),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.156d0,1.152d0,1.149d0,1.146d0,1.143d0,  &
     1.139d0,1.136d0,1.133d0,1.13d0,1.127d0,1.121d0,1.115d0,1.109d0,1.103d0,1.097d0,1.091d0,1.085d0,1.08d0,1.074d0,  &
     1.069d0,1.063d0,1.058d0,1.052d0,1.047d0,1.042d0,1.037d0,1.032d0,1.027d0,1.022d0,1.017d0,1.012d0,1.007d0,1.003d0,  &
     0.99790,0.99320,0.98180,0.97060,0.95970,0.94910,0.93880,0.92860,0.91880,0.90910,0.89960,0.89040,0.87250,0.85530,  &
     0.83880,0.823d0,0.80780,0.79310,0.779d0,0.76540,0.75230,0.73960,0.72740,0.71550,0.70410,0.693d0,3.542d-005,0.61560,  &
     0.5755d0/
  DATA (DefaultSteamSuperheatedDensityData(i,54),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.19d0,1.187d0,1.183d0,1.18d0,  &
     1.177d0,1.173d0,1.17d0,1.167d0,1.164d0,1.157d0,1.151d0,1.145d0,1.139d0,1.133d0,1.127d0,1.121d0,1.115d0,1.109d0,  &
     1.103d0,1.098d0,1.092d0,1.087d0,1.081d0,1.076d0,1.071d0,1.065d0,1.06d0,1.055d0,1.05d0,1.045d0,1.04d0,1.035d0,  &
     1.03d0,1.025d0,1.013d0,1.002d0,0.99070,0.97970,0.969d0,0.95860,0.94840,0.93840,0.92860,0.919d0,0.90050,0.88280,  &
     0.86580,0.84940,0.83370,0.81860,0.804d0,0.78990,0.77640,0.76330,0.75070,0.73840,0.72660,0.71520,3.542d-005,0.63530,  &
     0.5939d0/
  DATA (DefaultSteamSuperheatedDensityData(i,55),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.226d0,1.222d0,1.219d0,  &
     1.215d0,1.212d0,1.208d0,1.205d0,1.202d0,1.195d0,1.188d0,1.182d0,1.176d0,1.169d0,1.163d0,1.157d0,1.151d0,1.145d0,  &
     1.139d0,1.133d0,1.127d0,1.122d0,1.116d0,1.111d0,1.105d0,1.1d0,1.094d0,1.089d0,1.084d0,1.079d0,1.073d0,1.068d0,  &
     1.063d0,1.058d0,1.046d0,1.034d0,1.023d0,1.011d0,1.0d0,0.98930,0.97870,0.96840,0.95830,0.94840,0.92930,0.911d0,0.89340,  &
     0.87650,0.86030,0.84470,0.82960,0.81510,0.80110,0.78760,0.77460,0.76190,0.74970,0.73790,3.542d-005,0.65550,  &
     0.6128d0/
  DATA (DefaultSteamSuperheatedDensityData(i,56),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.262d0,1.258d0,  &
     1.254d0,1.251d0,1.247d0,1.244d0,1.24d0,1.234d0,1.227d0,1.22d0,1.213d0,1.207d0,1.201d0,1.194d0,1.188d0,1.182d0,  &
     1.176d0,1.17d0,1.164d0,1.158d0,1.152d0,1.146d0,1.141d0,1.135d0,1.129d0,1.124d0,1.118d0,1.113d0,1.108d0,1.102d0,  &
     1.097d0,1.092d0,1.08d0,1.067d0,1.055d0,1.043d0,1.032d0,1.021d0,1.01d0,0.99920,0.98880,0.97860,0.95890,0.93990,  &
     0.92180,0.90440,0.88760,0.87150,0.85590,0.84090,0.82650,0.81260,0.79910,0.78610,0.77350,0.76130,3.542d-005,0.67620,  &
     0.6321d0/
  DATA (DefaultSteamSuperheatedDensityData(i,57),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.299d0,1.295d0,  &
     1.291d0,1.288d0,1.284d0,1.28d0,1.273d0,1.266d0,1.259d0,1.252d0,1.246d0,1.239d0,1.232d0,1.226d0,1.22d0,1.213d0,  &
     1.207d0,1.201d0,1.195d0,1.189d0,1.183d0,1.177d0,1.171d0,1.165d0,1.16d0,1.154d0,1.149d0,1.143d0,1.138d0,1.132d0,  &
     1.127d0,1.114d0,1.101d0,1.089d0,1.077d0,1.065d0,1.053d0,1.042d0,1.031d0,1.02d0,1.01d0,0.98920,0.96960,0.95090,  &
     0.93290,0.91560,0.89890,0.88290,0.86740,0.85250,0.83810,0.82420,0.81080,0.79780,0.78520,3.542d-005,0.69740,  &
     0.652d0/
  DATA (DefaultSteamSuperheatedDensityData(i,58),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.337d0,  &
     1.333d0,1.329d0,1.325d0,1.321d0,1.314d0,1.307d0,1.3d0,1.292d0,1.285d0,1.279d0,1.272d0,1.265d0,1.258d0,1.252d0,  &
     1.245d0,1.239d0,1.233d0,1.227d0,1.22d0,1.214d0,1.208d0,1.202d0,1.196d0,1.191d0,1.185d0,1.179d0,1.174d0,1.168d0,  &
     1.163d0,1.149d0,1.136d0,1.123d0,1.111d0,1.098d0,1.086d0,1.075d0,1.063d0,1.052d0,1.041d0,1.02d0,1.0d0,0.98080,0.96220,  &
     0.94430,0.92710,0.91060,0.89460,0.87920,0.86440,0.85d0,0.83620,0.82280,0.80980,3.542d-005,0.7192d0,0.6723d0/
  DATA (DefaultSteamSuperheatedDensityData(i,59),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     1.375d0,1.371d0,1.367d0,1.364d0,1.356d0,1.348d0,1.341d0,1.334d0,1.326d0,1.319d0,1.312d0,1.305d0,1.298d0,1.292d0,  &
     1.285d0,1.278d0,1.272d0,1.265d0,1.259d0,1.253d0,1.246d0,1.24d0,1.234d0,1.228d0,1.222d0,1.216d0,1.211d0,1.205d0,  &
     1.199d0,1.185d0,1.172d0,1.158d0,1.145d0,1.133d0,1.12d0,1.108d0,1.097d0,1.085d0,1.074d0,1.052d0,1.031d0,1.011d0,  &
     0.99220,0.97380,0.956d0,0.939d0,0.92250,0.90660,0.89130,0.87650,0.86220,0.84840,0.835d0,3.542d-005,0.7416d0,0.6932d0/
  DATA (DefaultSteamSuperheatedDensityData(i,60),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,1.415d0,1.411d0,1.407d0,1.399d0,1.391d0,1.383d0,1.376d0,1.368d0,1.361d0,1.354d0,1.346d0,1.339d0,1.332d0,1.325d0,  &
     1.319d0,1.312d0,1.305d0,1.299d0,1.292d0,1.286d0,1.279d0,1.273d0,1.267d0,1.261d0,1.255d0,1.249d0,1.243d0,1.237d0,  &
     1.222d0,1.208d0,1.195d0,1.181d0,1.168d0,1.155d0,1.143d0,1.131d0,1.119d0,1.107d0,1.085d0,1.063d0,1.043d0,1.023d0,  &
     1.004d0,0.98570,0.96810,0.95110,0.93470,0.91890,0.90360,0.88890,0.87460,0.86080,3.542d-005,0.7645d0,0.7146d0/
  DATA (DefaultSteamSuperheatedDensityData(i,61),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,1.455d0,1.451d0,1.443d0,1.435d0,1.427d0,1.419d0,1.411d0,1.404d0,1.396d0,1.389d0,1.381d0,1.374d0,1.367d0,  &
     1.36d0,1.353d0,1.346d0,1.339d0,1.332d0,1.326d0,1.319d0,1.313d0,1.306d0,1.3d0,1.294d0,1.287d0,1.281d0,1.275d0,  &
     1.26d0,1.246d0,1.232d0,1.218d0,1.204d0,1.191d0,1.178d0,1.166d0,1.154d0,1.142d0,1.118d0,1.096d0,1.075d0,1.055d0,  &
     1.035d0,1.016d0,0.99790,0.98040,0.96350,0.94720,0.93140,0.91620,0.90150,0.88730,3.542d-005,0.7879d0,0.7365d0/
  DATA (DefaultSteamSuperheatedDensityData(i,62),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,1.497d0,1.488d0,1.48d0,1.472d0,1.464d0,1.456d0,1.448d0,1.44d0,1.432d0,1.425d0,1.417d0,1.41d0,  &
     1.402d0,1.395d0,1.388d0,1.381d0,1.374d0,1.367d0,1.36d0,1.354d0,1.347d0,1.34d0,1.334d0,1.327d0,1.321d0,1.315d0,  &
     1.299d0,1.284d0,1.27d0,1.255d0,1.242d0,1.228d0,1.215d0,1.202d0,1.189d0,1.177d0,1.153d0,1.13d0,1.108d0,1.087d0,  &
     1.067d0,1.047d0,1.028d0,1.01d0,0.993d0,0.97620,0.95990,0.94420,0.92910,0.91440,3.542d-005,0.812d0,0.759d0/
  DATA (DefaultSteamSuperheatedDensityData(i,63),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,1.583d0,1.574d0,1.565d0,1.556d0,1.548d0,1.539d0,1.531d0,1.522d0,1.514d0,1.506d0,1.498d0,  &
     1.49d0,1.483d0,1.475d0,1.468d0,1.46d0,1.453d0,1.445d0,1.438d0,1.431d0,1.424d0,1.417d0,1.41d0,1.404d0,1.397d0,  &
     1.38d0,1.364d0,1.349d0,1.334d0,1.319d0,1.304d0,1.29d0,1.276d0,1.263d0,1.25d0,1.224d0,1.2d0,1.177d0,1.154d0,  &
     1.133d0,1.112d0,1.092d0,1.073d0,1.054d0,1.036d0,1.019d0,1.002d0,0.98630,0.97070,3.542d-005,0.8619d0,0.8056d0/
  DATA (DefaultSteamSuperheatedDensityData(i,64),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.673d0,1.663d0,1.654d0,1.644d0,1.635d0,1.626d0,1.617d0,1.609d0,1.6d0,1.592d0,1.583d0,  &
     1.575d0,1.567d0,1.559d0,1.551d0,1.543d0,1.535d0,1.527d0,1.52d0,1.512d0,1.505d0,1.498d0,1.49d0,1.483d0,1.466d0,  &
     1.449d0,1.432d0,1.416d0,1.4d0,1.385d0,1.37d0,1.355d0,1.341d0,1.327d0,1.299d0,1.273d0,1.249d0,1.225d0,1.202d0,  &
     1.18d0,1.159d0,1.138d0,1.119d0,1.1d0,1.081d0,1.063d0,1.046d0,1.03d0,3.542d-005,0.9143d0,0.8546d0/
  DATA (DefaultSteamSuperheatedDensityData(i,65),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.766d0,1.756d0,1.746d0,1.737d0,1.727d0,1.717d0,1.708d0,1.699d0,1.69d0,1.681d0,  &
     1.672d0,1.663d0,1.655d0,1.646d0,1.638d0,1.629d0,1.621d0,1.613d0,1.605d0,1.597d0,1.589d0,1.582d0,1.574d0,1.555d0,  &
     1.537d0,1.519d0,1.502d0,1.485d0,1.469d0,1.453d0,1.437d0,1.422d0,1.407d0,1.378d0,1.351d0,1.324d0,1.299d0,1.274d0,  &
     1.251d0,1.229d0,1.207d0,1.186d0,1.166d0,1.146d0,1.128d0,1.109d0,1.092d0,3.542d-005,0.9692d0,0.9059d0/
  DATA (DefaultSteamSuperheatedDensityData(i,66),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.864d0,1.854d0,1.843d0,1.833d0,1.823d0,1.813d0,1.803d0,1.793d0,1.784d0,  &
     1.774d0,1.765d0,1.755d0,1.746d0,1.737d0,1.729d0,1.72d0,1.711d0,1.703d0,1.694d0,1.686d0,1.678d0,1.669d0,1.649d0,  &
     1.63d0,1.611d0,1.593d0,1.575d0,1.557d0,1.54d0,1.524d0,1.507d0,1.492d0,1.461d0,1.432d0,1.403d0,1.377d0,1.351d0,  &
     1.326d0,1.302d0,1.279d0,1.257d0,1.235d0,1.215d0,1.195d0,1.175d0,1.157d0,3.542d-005,1.027d0,0.9597d0/
  DATA (DefaultSteamSuperheatedDensityData(i,67),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.967d0,1.955d0,1.944d0,1.933d0,1.923d0,1.912d0,1.902d0,1.891d0,  &
     1.881d0,1.871d0,1.861d0,1.852d0,1.842d0,1.833d0,1.823d0,1.814d0,1.805d0,1.796d0,1.787d0,1.778d0,1.77d0,1.748d0,  &
     1.728d0,1.707d0,1.688d0,1.669d0,1.65d0,1.632d0,1.614d0,1.597d0,1.58d0,1.548d0,1.516d0,1.487d0,1.458d0,1.431d0,  &
     1.404d0,1.379d0,1.354d0,1.331d0,1.308d0,1.286d0,1.265d0,1.245d0,1.225d0,3.542d-005,1.087d0,1.016d0/
  DATA (DefaultSteamSuperheatedDensityData(i,68),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.074d0,2.062d0,2.05d0,2.038d0,2.027d0,2.016d0,2.005d0,1.994d0,  &
     1.983d0,1.973d0,1.962d0,1.952d0,1.942d0,1.932d0,1.922d0,1.912d0,1.903d0,1.893d0,1.884d0,1.875d0,1.852d0,1.83d0,  &
     1.809d0,1.788d0,1.767d0,1.748d0,1.728d0,1.709d0,1.691d0,1.673d0,1.639d0,1.605d0,1.574d0,1.543d0,1.514d0,1.486d0,  &
     1.459d0,1.434d0,1.409d0,1.384d0,1.361d0,1.339d0,1.317d0,1.296d0,3.542d-005,1.15d0,1.075d0/
  DATA (DefaultSteamSuperheatedDensityData(i,69),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.185d0,2.172d0,2.16d0,2.148d0,2.136d0,2.124d0,2.112d0,  &
     2.101d0,2.09d0,2.079d0,2.068d0,2.057d0,2.046d0,2.036d0,2.025d0,2.015d0,2.005d0,1.995d0,1.985d0,1.961d0,1.937d0,  &
     1.915d0,1.892d0,1.871d0,1.85d0,1.829d0,1.809d0,1.79d0,1.771d0,1.734d0,1.699d0,1.665d0,1.633d0,1.602d0,1.572d0,  &
     1.544d0,1.516d0,1.49d0,1.464d0,1.44d0,1.416d0,1.393d0,1.371d0,3.542d-005,1.216d0,1.137d0/
  DATA (DefaultSteamSuperheatedDensityData(i,70),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.301d0,2.288d0,2.275d0,2.262d0,2.249d0,2.237d0,  &
     2.225d0,2.213d0,2.201d0,2.189d0,2.177d0,2.166d0,2.155d0,2.144d0,2.133d0,2.122d0,2.111d0,2.101d0,2.075d0,2.05d0,  &
     2.026d0,2.002d0,1.979d0,1.957d0,1.935d0,1.914d0,1.893d0,1.873d0,1.834d0,1.796d0,1.761d0,1.727d0,1.694d0,1.662d0,  &
     1.632d0,1.603d0,1.575d0,1.548d0,1.522d0,1.497d0,1.473d0,1.449d0,3.542d-005,1.286d0,1.201d0/
  DATA (DefaultSteamSuperheatedDensityData(i,71),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.422d0,2.408d0,2.394d0,2.381d0,2.367d0,  &
     2.354d0,2.341d0,2.329d0,2.316d0,2.304d0,2.292d0,2.28d0,2.268d0,2.256d0,2.245d0,2.233d0,2.222d0,2.195d0,2.168d0,  &
     2.142d0,2.117d0,2.093d0,2.069d0,2.046d0,2.023d0,2.001d0,1.98d0,1.938d0,1.899d0,1.861d0,1.825d0,1.79d0,1.757d0,  &
     1.725d0,1.694d0,1.664d0,1.635d0,1.608d0,1.581d0,1.556d0,1.531d0,3.542d-005,1.358d0,1.269d0/
  DATA (DefaultSteamSuperheatedDensityData(i,72),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.548d0,2.533d0,2.519d0,2.505d0,2.491d0,  &
     2.477d0,2.463d0,2.45d0,2.437d0,2.424d0,2.411d0,2.398d0,2.386d0,2.373d0,2.361d0,2.349d0,2.32d0,2.292d0,2.264d0,  &
     2.238d0,2.212d0,2.186d0,2.162d0,2.138d0,2.114d0,2.091d0,2.048d0,2.006d0,1.965d0,1.927d0,1.89d0,1.855d0,1.821d0,  &
     1.789d0,1.757d0,1.727d0,1.698d0,1.67d0,1.642d0,1.616d0,3.542d-005,1.433d0,1.339d0/
  DATA (DefaultSteamSuperheatedDensityData(i,73),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.679d0,2.664d0,2.648d0,2.633d0,  &
     2.619d0,2.604d0,2.59d0,2.576d0,2.562d0,2.548d0,2.535d0,2.522d0,2.508d0,2.495d0,2.483d0,2.452d0,2.421d0,2.392d0,  &
     2.364d0,2.336d0,2.309d0,2.283d0,2.258d0,2.233d0,2.209d0,2.162d0,2.117d0,2.075d0,2.034d0,1.995d0,1.958d0,1.922d0,  &
     1.888d0,1.854d0,1.822d0,1.792d0,1.762d0,1.733d0,1.705d0,3.542d-005,1.512d0,1.413d0/
  DATA (DefaultSteamSuperheatedDensityData(i,74),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.816d0,2.8d0,2.783d0,  &
     2.768d0,2.752d0,2.737d0,2.722d0,2.707d0,2.692d0,2.678d0,2.664d0,2.65d0,2.636d0,2.622d0,2.589d0,2.557d0,2.526d0,  &
     2.496d0,2.466d0,2.438d0,2.41d0,2.383d0,2.357d0,2.331d0,2.282d0,2.234d0,2.189d0,2.146d0,2.105d0,2.066d0,2.028d0,  &
     1.991d0,1.956d0,1.922d0,1.89d0,1.858d0,1.828d0,1.799d0,3.542d-005,1.595d0,1.490d0/
  DATA (DefaultSteamSuperheatedDensityData(i,75),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,2.958d0,2.941d0,  &
     2.924d0,2.907d0,2.891d0,2.875d0,2.859d0,2.843d0,2.828d0,2.813d0,2.798d0,2.783d0,2.769d0,2.733d0,2.699d0,2.666d0,  &
     2.634d0,2.603d0,2.572d0,2.543d0,2.514d0,2.486d0,2.459d0,2.407d0,2.357d0,2.309d0,2.263d0,2.22d0,2.178d0,2.138d0,  &
     2.099d0,2.062d0,2.026d0,1.992d0,1.959d0,1.927d0,1.896d0,3.542d-005,1.681d0,1.570d0/
  DATA (DefaultSteamSuperheatedDensityData(i,76),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.106d0,3.088d0,  &
     3.07d0,3.052d0,3.035d0,3.018d0,3.001d0,2.985d0,2.969d0,2.953d0,2.937d0,2.922d0,2.884d0,2.848d0,2.812d0,2.778d0,  &
     2.745d0,2.713d0,2.682d0,2.651d0,2.622d0,2.593d0,2.537d0,2.484d0,2.434d0,2.386d0,2.34d0,2.295d0,2.253d0,2.212d0,  &
     2.173d0,2.135d0,2.099d0,2.064d0,2.03d0,1.997d0,3.542d-005,1.77d0,1.654d0/
  DATA (DefaultSteamSuperheatedDensityData(i,77),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.26d0,  &
     3.24d0,3.222d0,3.203d0,3.185d0,3.167d0,3.15d0,3.132d0,3.115d0,3.099d0,3.082d0,3.042d0,3.003d0,2.966d0,2.929d0,  &
     2.894d0,2.86d0,2.827d0,2.794d0,2.763d0,2.732d0,2.674d0,2.618d0,2.564d0,2.513d0,2.465d0,2.418d0,2.373d0,2.33d0,  &
     2.289d0,2.249d0,2.21d0,2.173d0,2.138d0,2.103d0,3.542d-005,1.864d0,1.741d0/
  DATA (DefaultSteamSuperheatedDensityData(i,78),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     3.419d0,3.399d0,3.379d0,3.36d0,3.341d0,3.322d0,3.304d0,3.286d0,3.268d0,3.25d0,3.207d0,3.166d0,3.126d0,3.087d0,  &
     3.05d0,3.014d0,2.978d0,2.944d0,2.911d0,2.878d0,2.816d0,2.757d0,2.7d0,2.646d0,2.595d0,2.546d0,2.498d0,2.453d0,  &
     2.409d0,2.367d0,2.326d0,2.287d0,2.25d0,2.213d0,3.542d-005,1.961d0,1.832d0/
  DATA (DefaultSteamSuperheatedDensityData(i,79),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,3.585d0,3.564d0,3.543d0,3.523d0,3.503d0,3.483d0,3.464d0,3.445d0,3.426d0,3.38d0,3.336d0,3.294d0,3.253d0,3.213d0,  &
     3.174d0,3.137d0,3.1d0,3.065d0,3.031d0,2.965d0,2.902d0,2.842d0,2.785d0,2.731d0,2.679d0,2.629d0,2.581d0,2.535d0,  &
     2.49d0,2.448d0,2.406d0,2.367d0,2.328d0,3.542d-005,2.063d0,1.926d0/
  DATA (DefaultSteamSuperheatedDensityData(i,80),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,3.758d0,3.735d0,3.713d0,3.692d0,3.671d0,3.65d0,3.63d0,3.61d0,3.561d0,3.514d0,3.469d0,3.425d0,3.383d0,  &
     3.342d0,3.302d0,3.264d0,3.226d0,3.19d0,3.12d0,3.054d0,2.99d0,2.93d0,2.873d0,2.818d0,2.765d0,2.714d0,2.665d0,  &
     2.619d0,2.574d0,2.53d0,2.488d0,2.448d0,3.542d-005,2.168d0,2.025d0/
  DATA (DefaultSteamSuperheatedDensityData(i,81),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,3.937d0,3.913d0,3.89d0,3.867d0,3.845d0,3.823d0,3.802d0,3.75d0,3.7d0,3.652d0,3.605d0,3.561d0,  &
     3.517d0,3.475d0,3.434d0,3.394d0,3.356d0,3.282d0,3.212d0,3.145d0,3.081d0,3.02d0,2.962d0,2.907d0,2.853d0,2.802d0,  &
     2.752d0,2.705d0,2.659d0,2.615d0,2.573d0,3.542d-005,2.278d0,2.127d0/
  DATA (DefaultSteamSuperheatedDensityData(i,82),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,4.122d0,4.097d0,4.073d0,4.049d0,4.026d0,4.003d0,3.948d0,3.895d0,3.843d0,3.794d0,3.746d0,  &
     3.7d0,3.655d0,3.612d0,3.57d0,3.529d0,3.451d0,3.376d0,3.306d0,3.238d0,3.174d0,3.113d0,3.054d0,2.998d0,2.944d0,  &
     2.892d0,2.842d0,2.794d0,2.747d0,2.702d0,3.542d-005,2.392d0,2.234d0/
  DATA (DefaultSteamSuperheatedDensityData(i,83),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,4.315d0,4.289d0,4.263d0,4.238d0,4.214d0,4.155d0,4.098d0,4.043d0,3.991d0,3.94d0,3.891d0,  &
     3.843d0,3.797d0,3.753d0,3.709d0,3.627d0,3.548d0,3.473d0,3.402d0,3.335d0,3.27d0,3.208d0,3.148d0,3.091d0,3.037d0,  &
     2.984d0,2.933d0,2.884d0,2.837d0,3.542d-005,2.511d0,2.344d0/
  DATA (DefaultSteamSuperheatedDensityData(i,84),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,4.515d0,4.487d0,4.46d0,4.434d0,4.371d0,4.31d0,4.252d0,4.196d0,4.142d0,4.09d0,  &
     4.04d0,3.991d0,3.944d0,3.898d0,3.81d0,3.727d0,3.648d0,3.573d0,3.501d0,3.433d0,3.368d0,3.305d0,3.245d0,3.187d0,  &
     3.132d0,3.079d0,3.027d0,2.977d0,3.542d-005,2.635d0,2.459d0/
  DATA (DefaultSteamSuperheatedDensityData(i,85),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,4.722d0,4.693d0,4.665d0,4.597d0,4.532d0,4.47d0,4.411d0,4.353d0,4.298d0,  &
     4.244d0,4.193d0,4.143d0,4.094d0,4.001d0,3.913d0,3.83d0,3.751d0,3.675d0,3.603d0,3.534d0,3.468d0,3.405d0,3.344d0,  &
     3.286d0,3.23d0,3.176d0,3.123d0,3.542d-005,2.763d0,2.579d0/
  DATA (DefaultSteamSuperheatedDensityData(i,86),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,4.936d0,4.906d0,4.833d0,4.764d0,4.698d0,4.635d0,4.574d0,4.515d0,  &
     4.458d0,4.403d0,4.35d0,4.298d0,4.2d0,4.107d0,4.019d0,3.935d0,3.856d0,3.78d0,3.707d0,3.638d0,3.571d0,3.507d0,  &
     3.446d0,3.387d0,3.33d0,3.275d0,3.542d-005,2.896d0,2.703d0/
  DATA (DefaultSteamSuperheatedDensityData(i,87),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,5.159d0,5.081d0,5.007d0,4.936d0,4.868d0,4.803d0,4.741d0,4.681d0,  &
     4.622d0,4.566d0,4.512d0,4.407d0,4.309d0,4.216d0,4.128d0,4.044d0,3.964d0,3.887d0,3.814d0,3.744d0,3.677d0,3.612d0,  &
     3.55d0,3.49d0,3.432d0,3.542d-005,3.035d0,2.832d0/
  DATA (DefaultSteamSuperheatedDensityData(i,88),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,5.75d0,5.662d0,5.579d0,5.499d0,5.423d0,5.35d0,5.28d0,  &
     5.212d0,5.147d0,5.084d0,4.964d0,4.851d0,4.744d0,4.643d0,4.547d0,4.456d0,4.369d0,4.286d0,4.206d0,4.13d0,4.056d0,  &
     3.986d0,3.918d0,3.853d0,3.542d-005,3.404d0,3.176d0/
  DATA (DefaultSteamSuperheatedDensityData(i,89),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,6.395d0,6.296d0,6.202d0,6.112d0,6.027d0,5.945d0,  &
     5.866d0,5.79d0,5.717d0,5.579d0,5.449d0,5.327d0,5.211d0,5.102d0,4.998d0,4.898d0,4.804d0,4.714d0,4.627d0,4.544d0,  &
     4.464d0,4.388d0,4.314d0,3.542d-005,3.808d0,3.552d0/
  DATA (DefaultSteamSuperheatedDensityData(i,90),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,7.098d0,6.985d0,6.879d0,6.779d0,6.683d0,  &
     6.591d0,6.503d0,6.418d0,6.258d0,6.108d0,5.968d0,5.836d0,5.711d0,5.593d0,5.48d0,5.373d0,5.27d0,5.172d0,5.078d0,  &
     4.988d0,4.902d0,4.819d0,3.542d-005,4.25d0,3.962d0/
  DATA (DefaultSteamSuperheatedDensityData(i,91),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,7.861d0,7.734d0,7.615d0,7.502d0,7.395d0,  &
     7.292d0,7.193d0,7.008d0,6.835d0,6.674d0,6.523d0,6.38d0,6.245d0,6.118d0,5.996d0,5.88d0,5.769d0,5.663d0,5.561d0,  &
     5.464d0,5.37d0,3.542d-005,4.732d0,4.410d0/
  DATA (DefaultSteamSuperheatedDensityData(i,92),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,8.69d0,8.547d0,8.413d0,8.286d0,  &
     8.166d0,8.051d0,7.835d0,7.636d0,7.451d0,7.278d0,7.115d0,6.961d0,6.816d0,6.678d0,6.547d0,6.421d0,6.302d0,6.187d0,  &
     6.078d0,5.972d0,3.542d-005,5.257d0,4.897d0/
  DATA (DefaultSteamSuperheatedDensityData(i,93),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,9.588d0,9.428d0,9.277d0,  &
     9.135d0,9.0d0,8.749d0,8.519d0,8.305d0,8.106d0,7.92d0,7.745d0,7.58d0,7.423d0,7.275d0,7.133d0,6.998d0,6.87d0,6.746d0,  &
     6.628d0,3.542d-005,5.827d0,5.425d0/
  DATA (DefaultSteamSuperheatedDensityData(i,94),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,10.56d0,10.38d0,  &
     10.21d0,10.05d0,9.759d0,9.491d0,9.244d0,9.016d0,8.803d0,8.603d0,8.415d0,8.238d0,8.069d0,7.91d0,7.758d0,7.613d0,  &
     7.474d0,7.341d0,3.542d-005,6.445d0,5.998d0/
  DATA (DefaultSteamSuperheatedDensityData(i,95),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,11.62d0,  &
     11.41d0,11.22d0,10.88d0,10.56d0,10.28d0,10.01d0,9.769d0,9.541d0,9.328d0,9.126d0,8.936d0,8.756d0,8.584d0,8.421d0,  &
     8.265d0,8.116d0,3.542d-005,7.115d0,6.618d0/
  DATA (DefaultSteamSuperheatedDensityData(i,96),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,12.75d0,  &
     12.53d0,12.11d0,11.75d0,11.41d0,11.11d0,10.83d0,10.57d0,10.32d0,10.1d0,9.88d0,9.676d0,9.483d0,9.299d0,  &
     9.124d0,8.957d0,3.542d-005,7.84d0,7.288d0/
  DATA (DefaultSteamSuperheatedDensityData(i,97),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     13.99d0,13.49d0,13.05d0,12.67d0,12.31d0,11.99d0,11.69d0,11.41d0,11.15d0,10.91d0,10.68d0,10.46d0,10.25d0,  &
     10.06d0,9.869d0,3.542d-005,8.623d0,8.011d0/
  DATA (DefaultSteamSuperheatedDensityData(i,98),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,16.75d0,16.12d0,15.58d0,15.1d0,14.66d0,14.26d0,13.9d0,13.56d0,13.25d0,12.95d0,12.67d0,12.41d0,  &
     12.16d0,11.93d0,3.542d-005,10.38d0,9.628d0/
  DATA (DefaultSteamSuperheatedDensityData(i,99),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,19.97d0,19.17d0,18.49d0,17.89d0,17.36d0,16.87d0,16.43d0,16.02d0,15.64d0,15.28d0,14.95d0,  &
     14.63d0,14.34d0,3.542d-005,12.42d0,11.5d0/
  DATA (DefaultSteamSuperheatedDensityData(i,100),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,23.71d0,22.7d0,21.85d0,21.1d0,20.45d0,19.85d0,19.31d0,18.81d0,18.35d0,17.93d0,17.53d0,  &
     17.15d0,3.542d-005,14.77d0,13.65d0/
  DATA (DefaultSteamSuperheatedDensityData(i,101),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,28.07d0,26.78d0,25.71d0,24.79d0,23.97d0,23.25d0,22.59d0,21.99d0,21.44d0,20.93d0,  &
     20.45d0,3.542d-005,17.48d0,16.12d0/
  DATA (DefaultSteamSuperheatedDensityData(i,102),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,33.16d0,31.5d0,30.15d0,29.0d0,28.0d0,27.11d0,26.31d0,25.59d0,24.92d0,24.31d0,  &
     3.542d-005,20.6d0,18.94d0/
  DATA (DefaultSteamSuperheatedDensityData(i,103),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,39.13d0,36.97d0,35.25d0,33.82d0,32.58d0,31.5d0,30.53d0,29.65d0,28.86d0,  &
     3.542d-005,24.19d0,22.16d0/
  DATA (DefaultSteamSuperheatedDensityData(i,104),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,46.17d0,43.33d0,41.13d0,39.33d0,37.8d0,36.47d0,35.29d0,34.24d0,  &
     3.542d-005,28.31d0,25.84d0/
  DATA (DefaultSteamSuperheatedDensityData(i,105),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,54.54d0,50.75d0,47.92d0,45.65d0,43.75d0,42.11d0,40.68d0,  &
     3.542d-005,33.07d0,30.03d0/
  DATA (DefaultSteamSuperheatedDensityData(i,106),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,64.64d0,59.47d0,55.78d0,52.9d0,50.53d0,48.51d0,3.542d-005,  &
     38.55d0,34.81d0/
  DATA (DefaultSteamSuperheatedDensityData(i,107),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,77.05d0,69.8d0,64.93d0,61.24d0,58.27d0,3.542d-005,  &
     44.92d0,40.28d0/
  DATA (DefaultSteamSuperheatedDensityData(i,108),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,92.76d0,82.18d0,75.63d0,70.87d0,3.542d-005,  &
     52.35d0,46.54d0/
  DATA (DefaultSteamSuperheatedDensityData(i,109),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,113.6d0,97.22d0,88.27d0,3.542d-005,  &
     61.12d0,53.76d0/
  DATA (DefaultSteamSuperheatedDensityData(i,110),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,143.9d0,115.8d0,3.542d-005,71.6d0,  &
     62.15d0/
  DATA (DefaultSteamSuperheatedDensityData(i,111),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,201.8d0,3.542d-005,84.38d0,  &
     71.99d0/
  DATA (DefaultSteamSuperheatedDensityData(i,112),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.542d-005,148.4d0,  &
     115.1d0/
  DATA (DefaultSteamSuperheatedDensityData(i,113),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.542d-005,201.7d0,  &
     144.2d0/
  DATA (DefaultSteamSuperheatedDensityData(i,114),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,3.542d-005,270.9d0,  &
     177.8d0/

          ! Okay, the following is no longer strictly a DATA statement, but it is still data transfer for 0% concentration
          ! Convert mPa-s viscosity data to Pa-s
  DefaultWaterViscData = DefaultWaterViscData/1000.0d0
  DefaultEthGlyViscData = DefaultEthGlyViscData/1000.0d0
  DefaultPropGlyViscData = DefaultPropGlyViscData/1000.0d0

          ! Set zero concentration data
  DefaultEthGlyCpData(1,:)    = DefaultWaterCpData
  DefaultEthGlyViscData(1,:)  = DefaultWaterViscData
  DefaultEthGlyRhoData(1,:)   = DefaultWaterRhoData
  DefaultEthGlyCondData(1,:)  = DefaultWaterCondData
  DefaultPropGlyCpData(1,:)   = DefaultWaterCpData
  DefaultPropGlyViscData(1,:) = DefaultWaterViscData
  DefaultPropGlyRhoData(1,:)  = DefaultWaterRhoData
  DefaultPropGlyCondData(1,:) = DefaultWaterCondData

          ! FLOW:
  MaxAlphas=0
  MaxNumbers=0
  IF (GetNumObjectsFound('FluidProperties:Name') > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Name',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  IF (GetNumObjectsFound('FluidProperties:GlycolConcentration') > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:GlycolConcentration',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfFluidTempArrays    = GetNumObjectsFound('FluidProperties:Temperatures')
  IF (NumOfFluidTempArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Temperatures',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfSatFluidPropArrays = GetNumObjectsFound('FluidProperties:Saturated')
  IF (NumOfSatFluidPropArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Saturated',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfSHFluidPropArrays  = GetNumObjectsFound('FluidProperties:Superheated')
  IF (NumOfSHFluidPropArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Superheated',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfGlyFluidPropArrays = GetNumObjectsFound('FluidProperties:Concentration')
  IF (NumOfGlyFluidPropArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Concentration',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF

  ALLOCATE(Alphas(MaxAlphas))
  ALLOCATE(cAlphaFieldNames(MaxAlphas))
  ALLOCATE(lAlphaFieldBlanks(MaxAlphas))

  Alphas=' '
  cAlphaFieldNames=' '
  lAlphaFIeldBlanks=.false.

  ALLOCATE(Numbers(MaxNumbers))
  ALLOCATE(cNumericFieldNames(MaxNumbers))
  ALLOCATE(lNumericFieldBlanks(MaxNumbers))

  Numbers=0.0
  cNumericFieldNames=' '
  lNumericFieldBlanks=.false.

          ! Check to see if there is any FluidName input.  If not, this is okay as
          ! long as the user only desires to simulate loops with water.  More than
          ! one FluidName input is not allowed.
  CurrentModuleObject = 'FluidProperties:Name'
  NumOfOptionalInput = GetNumObjectsFound(TRIM(CurrentModuleObject))

  ALLOCATE(FluidNames(NumOfOptionalInput))

          ! Get a count on the number of refrigerants and the number of glycols entered
          ! so that the main derived types can be allocated
  FluidNum=0
  DO Loop = 1, NumOfOptionalInput
    CALL GetObjectItem(TRIM(CurrentModuleObject),Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),FluidNames%Name,FluidNum,ErrorInName,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...Fluid names must be unique regardless of subtype.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    FluidNum=FluidNum+1
    FluidNames(FluidNum)%Name=Alphas(1)
    IF (SameString(Alphas(2),Refrig)) THEN
      NumOfRefrigerants = NumOfRefrigerants + 1
      FluidNames(FluidNum)%IsGlycol=.false.
    ELSEIF (SameString(Alphas(2),Glycol)) THEN
      NumOfGlycols = NumOfGlycols + 1
      FluidNames(FluidNum)%IsGlycol=.true.
    ELSE
      CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid type')
      CALL ShowContinueError('...entered value="'//TRIM(Alphas(2))//', Only REFRIGERANT or GLYCOL allowed as '//  &
         trim(cAlphaFieldNames(2)))
      ErrorsFound=.true.
    END IF
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetFluidPropertiesData: Previous errors in input cause program termination.')
  ENDIF

  IF (NumOfRefrigerants+1 > 0) THEN
    ALLOCATE(RefrigData(NumOfRefrigerants+1))
    ALLOCATE(RefrigUsed(NumOfRefrigerants+1))
    RefrigUsed=.false.
  ENDIF
  IF (NumOfGlycols > 0) THEN
    ALLOCATE(GlyRawData(NumOfGlycols))
  ENDIF

          ! Take the fluid names and assign them to the appropriate derived type
  NumOfRefrigerants = 1
  NumOfGlycols      = 0
  RefrigData(1)%Name = 'STEAM'
  RefrigUsed(1)=.true.
  DO Loop = 1, FluidNum
    IF (.not. FluidNames(Loop)%IsGlycol) THEN
      NumOfRefrigerants = NumOfRefrigerants + 1
      RefrigData(NumOfRefrigerants)%Name = FluidNames(Loop)%Name
    ELSEIF (FluidNames(Loop)%IsGlycol) THEN
      NumOfGlycols = NumOfGlycols + 1
      GlyRawData(NumOfGlycols)%Name = FluidNames(Loop)%Name
    END IF
  END DO

  DEALLOCATE(FluidNames)

  RefrigData(1)%NumPsPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%PsTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%PsValues(DefaultNumSteamTemps))
  RefrigData(1)%NumHPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%HTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%HfValues(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%HfgValues(DefaultNumSteamTemps))
  RefrigData(1)%NumCpPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%CpTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%CpfValues(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%CpfgValues(DefaultNumSteamTemps))
  RefrigData(1)%NumRhoPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%RhoTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%RhofValues(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%RhofgValues(DefaultNumSteamTemps))

  RefrigData(1)%PsTemps=    DefaultSteamTemps
  RefrigData(1)%PsValues=   DefaultSteamPressData
  RefrigData(1)%HTemps=     DefaultSteamTemps
  RefrigData(1)%HfValues=   DefaultSteamEnthalpyFluidData
  RefrigData(1)%HfgValues=  DefaultSteamEnthalpyGasFluidData
  RefrigData(1)%CpTemps=    DefaultSteamTemps
  RefrigData(1)%CpfValues=  DefaultSteamCpFluidData
  RefrigData(1)%CpfgValues= DefaultSteamCpGasFluidData
  RefrigData(1)%RhoTemps=   DefaultSteamTemps
  RefrigData(1)%RhofValues= DefaultSteamDensityFluidData
  RefrigData(1)%RhofgValues=DefaultSteamDensityGasFluidData

  RefrigData(1)%NumSuperTempPts=DefaultNumSteamSuperheatedTemps
  RefrigData(1)%NumSuperPressPts=DefaultNumSteamSuperheatedPressure
  ALLOCATE(RefrigData(1)%SHTemps(RefrigData(1)%NumSuperTempPts))
  ALLOCATE(RefrigData(1)%SHPress(RefrigData(1)%NumSuperPressPts))
  ALLOCATE(RefrigData(1)%HshValues(RefrigData(1)%NumSuperTempPts,RefrigData(1)%NumSuperPressPts))
  ALLOCATE(RefrigData(1)%RhoshValues(RefrigData(1)%NumSuperTempPts,RefrigData(1)%NumSuperPressPts))
  RefrigData(1)%SHTemps=DefaultSteamSuperheatedTemps
  RefrigData(1)%SHPress=DefaultSteamSuperheatedPressData
  RefrigData(1)%HshValues=DefaultSteamSuperheatedEnthalpyData
  RefrigData(1)%RhoshValues=DefaultSteamSuperheatedDensityData


          ! Read in all of the temperature arrays in the input file
  ALLOCATE(FluidTemps(NumOfFluidTempArrays))

  CurrentModuleObject = 'FluidProperties:Temperatures'

  DO Loop = 1, NumOfFluidTempArrays

    CALL GetObjectItem(TRIM(CurrentModuleObject),Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    FluidTemps(Loop)%Name       = Alphas(1)
    FluidTemps(Loop)%NumOfTemps = NumNumbers

    ALLOCATE(FluidTemps(Loop)%Temps(FluidTemps(Loop)%NumOfTemps))
    FluidTemps(Loop)%Temps = Numbers(1:NumNumbers)

    DO TempLoop = 2, FluidTemps(Loop)%NumOfTemps
      IF (FluidTemps(Loop)%Temps(TempLoop) <= FluidTemps(Loop)%Temps(TempLoop-1)) THEN
        CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//' lists must have data in ascending order')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' name='//TRIM(FluidTemps(Loop)%Name))
        CALL ShowContinueError('First Occurance at Temp('//TRIM(RoundSigDigits(TempLoop-1))//  &
                 ') {'//TRIM(RoundSigDigits(FluidTemps(Loop)%Temps(TempLoop-1),3))//'} >= Temp('//  &
                 TRIM(RoundSigDigits(TempLoop))//') {'//TRIM(RoundSigDigits(FluidTemps(Loop)%Temps(TempLoop),3))//'}')
        ErrorsFound=.true.
        EXIT
      ENDIF
    END DO

  END DO

          ! *************** REFRIGERANTS ***************
          ! Go through each refrigerant found in the fluid names statement and read in the data
          ! Note that every valid fluid must have ALL of the necessary data or a fatal error will
          ! be produced.
  DO Loop = 2, NumOfRefrigerants

          ! For each property, cycle through all the valid input until the proper match is found.

          ! **********    SATURATED DATA SECTION    **********

          ! Get: ***** Saturation Pressure temperatures and data (fluidgas only) *****
          ! This section added by S.J.Rees May 2002.
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = ' '
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Pressure) )              .AND. &
           (SameString(Alphas(3),GasFluid ) )           ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumPsPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%PsTemps(RefrigData(Loop)%NumPsPoints))
            ALLOCATE(RefrigData(Loop)%PsValues(RefrigData(Loop)%NumPsPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumPsPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and fluid saturation pressure '// &
                                                            'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumPsPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%PsTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%PsValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: '//  &
               'Found saturated fluid gas/fluid pressure input but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match,
          ! then no sat press data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Gas/Fluid Saturation Pressure found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Pressure" and '//trim(cAlphaFieldNames(3))//  &
           '="FluidGas".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturation pressure for this refrigerant



          ! Get: ***** ENTHALPY of SATURATED LIQUID *****
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = " "
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Enthalpy) )              .AND. &
           (SameString(Alphas(3),Fluid ) )                ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumHPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%HTemps(RefrigData(Loop)%NumHPoints))
            ALLOCATE(RefrigData(Loop)%HfValues(RefrigData(Loop)%NumHPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumHPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated fluid enthalpy '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumHPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%HTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%HfValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated fluid enthalpy input but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid enthalpy data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Saturated Fluid Enthalpy found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object:')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Enthalpy" and '//trim(cAlphaFieldNames(3))//  &
           '="Fluid".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated fluid enthalpy for this refrigerant

          ! Get: ***** ENTHALPY of SATURATED LIQUID/VAPOR ***** (difference between Hf and Hg, i.e. Hfg)
    CurrentModuleObject = 'FluidProperties:Saturated'
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Enthalpy) )              .AND. &
           (SameString(Alphas(3),GasFluid ) )             ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            IF (.NOT.SameString(FluidTemps(TempLoop)%Name,TempsName)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperatures for enthalpy fluid and '// &
                                   'gas/fluid points are not the same')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('Name='//TRIM(Alphas(4))//' => '//TRIM(FluidTemps(TempLoop)%Name)//' /= '//TRIM(TempsName))
              ErrorsFound=.true.
              EXIT
            ENDIF
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            ALLOCATE(RefrigData(Loop)%HfgValues(RefrigData(Loop)%NumHPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumHPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated gas/fluid '// &
                                   'enthalpy array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumHPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%HfgValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated gas/fluid enthalpy input '// &
                                 'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g enthalpy data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Saturated Gas/Fluid Enthalpy found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Enthalpy" and '//trim(cAlphaFieldNames(3))//  &
           '="FluidGas".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated gas/fluid enthalpy for this refrigerant

          ! Get: ***** SPECIFIC HEAT of SATURATED LIQUID *****
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = " "
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),SpecificHeat) )          .AND. &
           (SameString(Alphas(3),Fluid ) )                ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumCpPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%CpTemps(RefrigData(Loop)%NumCpPoints))
            ALLOCATE(RefrigData(Loop)%CpfValues(RefrigData(Loop)%NumCpPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumCpPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated fluid Cp '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumCpPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%CpTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%CpfValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated fluid specific heat (Cp) input '//  &
               'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid Cp data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Saturated Fluid Specific Heat found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="SpecificHeat" and '//trim(cAlphaFieldNames(3))//  &
           '="Fluid".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated fluid Cp for this refrigerant

          ! Get: ***** SPECIFIC HEAT of SATURATED LIQUID/VAPOR ***** (difference between Cpf and Cpg, i.e. Cpfg)
    CurrentModuleObject = 'FluidProperties:Saturated'
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),SpecificHeat) )          .AND. &
           (SameString(Alphas(3),GasFluid ) )             ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            IF (.NOT.SameString(FluidTemps(TempLoop)%Name,TempsName)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperatures for specific heat fluid and '// &
                                   'gas/fluid points are not the same')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('Name='//TRIM(Alphas(4))//' => '//TRIM(FluidTemps(TempLoop)%Name)//' /= '//TRIM(TempsName))
              ErrorsFound=.true.
              EXIT
            ENDIF
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            ALLOCATE(RefrigData(Loop)%CpfgValues(RefrigData(Loop)%NumCpPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumCpPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated gas/fluid Cp '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumCpPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%CpfgValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated gas/fluid specific heat (Cp) input '//  &
               'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g Cp data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Saturated Gas/Fluid Specific Heat found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="SpecificHeat" and '//trim(cAlphaFieldNames(3))//  &
           '="FluidGas".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated gas/fluid Cp for this refrigerant

          ! Get: ***** DENSITY of SATURATED LIQUID *****
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = " "
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Density) )               .AND. &
           (SameString(Alphas(3),Fluid ) )                ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumRhoPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%RhoTemps(RefrigData(Loop)%NumRhoPoints))
            ALLOCATE(RefrigData(Loop)%RhofValues(RefrigData(Loop)%NumRhoPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumRhoPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated fluid density '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//  &
                                      TRIM(RoundSigDigits(RefrigData(Loop)%NumRhoPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%RhoTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%RhofValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated fluid density input but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid density data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Saturated Fluid Density found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Density" and '//trim(cAlphaFieldNames(3))//  &
           '="Fluid".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated fluid enthalpy for this refrigerant

          ! Get: ***** DENSITY of SATURATED LIQUID/VAPOR ***** (difference between Rhof and Rhog, i.e. Rhofg)
    CurrentModuleObject = 'FluidProperties:Saturated'
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Density) )               .AND. &
           (SameString(Alphas(3),GasFluid ) )             ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            IF (.NOT.SameString(FluidTemps(TempLoop)%Name,TempsName)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperatures for density fluid and '// &
                                   'gas/fluid points are not the same')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('Name='//TRIM(Alphas(4))//' => '//TRIM(FluidTemps(TempLoop)%Name)//' /= '//TRIM(TempsName))
              ErrorsFound=.true.
              EXIT
            ENDIF
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            ALLOCATE(RefrigData(Loop)%RhofgValues(RefrigData(Loop)%NumRhoPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumRhoPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated gas/fluid density '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//  &
                                      TRIM(RoundSigDigits(RefrigData(Loop)%NumRhoPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%RhofgValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowFatalError('Found saturated gas/fluid density input but no matching temperature array')
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated gas/fluid density input '// &
                                 'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g density data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No Saturated Gas/Fluid Density found')
        CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Density" and '//trim(cAlphaFieldNames(3))//  &
           '="FluidGas".')
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated gas/fluid density for this refrigerant

          ! Check: TEMPERATURES for saturated density (must all be the same)
!    IF (RefrigData(Loop)%NumCpPoints /= RefrigData(Loop)%NumCpPoints) THEN
    !!!!  Error -- can never happen, does this mean NumCp vs. NumRho?
!      CALL ShowFatalError('GetFluidPropertiesData: Number of specific heat fluid and gas/fluid points are not the same')
!    ELSE
!      DO TempLoop = 1, RefrigData(Loop)%NumCpPoints
        !!!! Error -- something else that can never happen
!        IF (ABS(RefrigData(Loop)%CpTemps(TempLoop)-RefrigData(Loop)%CpTemps(TempLoop)) > TempToler) THEN
!          CALL ShowSevereError('GetFluidPropertiesData: Temperatures for specific heat fluid and '// &
!                               'gas/fluid points are not the same')
!          CALL ShowContinueError('Error occurs in Refrigerant Data Name='//TRIM(RefrigData(Loop)%Name))
!          WRITE(String1,*) TempLoop
!          String1=ADJUSTL(String1)
!          String2=TrimSigDigits(RefrigData(Loop)%CpTemps(TempLoop),3)
!          String2=ADJUSTL(String2)
!          String4=TrimSigDigits(RefrigData(Loop)%CpTemps(TempLoop),3)
!          String4=ADJUSTL(String4)
!          CALL ShowContinueError('First Occurance at CpTemp('//TRIM(String1)//') {'//TRIM(String2)//'} /= {'//TRIM(String4)//'}')
!          ErrorsFound=.true.
!          EXIT
!        ENDIF
!      END DO
!    END IF

          ! **********   SUPERHEATED DATA SECTION   **********
          ! Get: ***** ENTHALPY of SUPERHEATED GAS  *****
          ! First find the number of pressure value syntax lines have been entered and
          ! make sure that all of the pressure input is linked to the same temperature list
    CurrentModuleObject = 'FluidProperties:Superheated'
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfPressPts = 0
    DO InData = 1, NumOfSHFluidPropArrays
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ((SameString(Alphas(1),RefrigData(Loop)%Name)).AND.(SameString(Alphas(2),Enthalpy))) THEN
        NumOfPressPts = NumOfPressPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All superheated data for the same property must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            CALL ShowContinueError('Expected Temperature name='//TRIM(TempsName)//', Input had name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          ENDIF
        END IF
      END IF
    END DO
    IF (NumOfPressPts == 0) THEN
      CALL ShowSevereError('GetFluidPropertiesData: No pressure data found for superheated enthalpy')
      CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
      ErrorsFound=.true.
    ENDIF

          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
    DO TempLoop = 1, NumOfFluidTempArrays
      IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
        RefrigData(Loop)%NumSuperTempPts = FluidTemps(TempLoop)%NumOfTemps
        ALLOCATE(RefrigData(Loop)%SHTemps(RefrigData(Loop)%NumSuperTempPts))
        RefrigData(Loop)%SHTemps = FluidTemps(TempLoop)%Temps
        EXIT ! the TempLoop DO loop
      END IF
      IF (TempLoop == NumOfFluidTempArrays) THEN
        CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with '// &
                             'superheated enthalpy data')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      ENDIF
    END DO

          ! Next, allocate the pressure related arrays
    RefrigData(Loop)%NumSuperPressPts = NumOfPressPts
    ALLOCATE(RefrigData(Loop)%SHPress(RefrigData(Loop)%NumSuperPressPts))
    ALLOCATE(RefrigData(Loop)%HshValues(RefrigData(Loop)%NumSuperTempPts,RefrigData(Loop)%NumSuperPressPts))

          ! Finally, get the pressure and enthalpy values from the user input
    CurrentModuleObject = 'FluidProperties:Superheated'
    NumOfPressPts = 0
    ALLOCATE(PressurePtr(NumOfSHFluidPropArrays))
    DO InData = 1, NumOfSHFluidPropArrays
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ((SameString(Alphas(1),RefrigData(Loop)%Name)).AND.(SameString(Alphas(2),Enthalpy))) THEN
        NumOfPressPts = NumOfPressPts + 1
        IF (Numbers(1) <= 0.0) THEN
          CALL ShowSevereError('GetFluidPropertiesData: Negative pressures not allowed in fluid property input data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
          CALL ShowContinueError('...Value =['//trim(RoundSigDigits(Numbers(1),3))//'].')
          ErrorsFound=.true.
        ENDIF
        PressurePtr(NumOfPressPts)%Pressure = Numbers(1)
        PressurePtr(NumOfPressPts)%InPtr = InData
      END IF
    ENDDO

    ! Sort Pressure list
    ! insertionSort
    do InData=2,NumOfPressPts
      pTemp = PressurePtr(InData)%Pressure
      iTemp = PressurePtr(InData)%InPtr
      j = InData-1
      do while (j >= 1 .and. PressurePtr(j)%Pressure > pTemp)
        PressurePtr(j+1)%Pressure = PressurePtr(j)%Pressure
        PressurePtr(j+1)%InPtr = PressurePtr(j)%InPtr
        j = j-1
        if (j == 0) exit
      enddo
      PressurePtr(j+1)%Pressure = pTemp
      PressurePtr(j+1)%InPtr = iTemp
    end do

    DO InData = 1, NumOfPressPts
      CALL GetObjectItem(TRIM(CurrentModuleObject),PressurePtr(InData)%InPtr,Alphas,NumAlphas, &
                          Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      RefrigData(Loop)%SHPress(InData) = Numbers(1)
      ! a little error trapping
      IF (InData > 1) THEN
        IF (RefrigData(Loop)%SHPress(InData) <= RefrigData(Loop)%SHPress(Indata-1)) THEN
          CALL ShowSevereError('GetFluidPropertiesData: Pressures must be entered in ascending order for fluid property data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
          CALL ShowContinueError('First Occurance at Pressure('//TRIM(RoundSigDigits(Indata-1))//  &
                                 ') {'//TRIM(RoundSigDigits(RefrigData(Loop)%SHPress(Indata-1),3))//  &
                                 '} >= Pressure('//TRIM(RoundSigDigits(Indata))//  &
                                 ') {'//TRIM(RoundSigDigits(RefrigData(Loop)%SHPress(Indata),3))//'}')
          ErrorsFound=.true.
          EXIT
        ENDIF
      END IF
      IF ((NumNumbers-1) == RefrigData(Loop)%NumSuperTempPts) THEN
        RefrigData(Loop)%HshValues(1:RefrigData(Loop)%NumSuperTempPts,Indata) = Numbers(2:NumNumbers)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Number of superheated enthalpy data points '// &
                             'not equal to number of temperature points')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
    END DO

    DEALLOCATE(PressurePtr)

          ! Get: ***** DENSITY of SUPERHEATED GAS  *****
          ! First find the number of pressure value syntax lines have been entered and
          ! make sure that all of the pressure input is linked to the same temperature list
          ! Then allocate the arrays and read the data into the proper place
    ALLOCATE(RefrigData(Loop)%RhoshValues(RefrigData(Loop)%NumSuperTempPts,RefrigData(Loop)%NumSuperPressPts))
    CurrentModuleObject = 'FluidProperties:Superheated'
    NumOfPressPts = 0
    ALLOCATE(PressurePtr(NumOfSHFluidPropArrays))
    DO InData = 1, NumOfSHFluidPropArrays
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas, &
                          Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),RefrigData(Loop)%Name)).AND.(SameString(Alphas(2),Density))) THEN
        NumOfPressPts = NumOfPressPts + 1
        IF (Numbers(1) <= 0.0) THEN
          CALL ShowSevereError('GetFluidPropertiesData: Negative pressures not allowed in fluid property input data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
          CALL ShowContinueError('...Value =['//trim(RoundSigDigits(Numbers(1),3))//'].')
          ErrorsFound=.true.
        ENDIF
        PressurePtr(NumOfPressPts)%Pressure = Numbers(1)
        PressurePtr(NumOfPressPts)%InPtr = InData
      END IF
    ENDDO

    ! Sort Pressure list
    ! insertionSort
    do InData=2,NumOfPressPts
      pTemp = PressurePtr(InData)%Pressure
      iTemp = PressurePtr(InData)%InPtr
      j = InData-1
      do while (j >= 1 .and. PressurePtr(j)%Pressure > pTemp)
        PressurePtr(j+1)%Pressure = PressurePtr(j)%Pressure
        PressurePtr(j+1)%InPtr = PressurePtr(j)%InPtr
        j = j-1
        if (j == 0) exit
      enddo
      PressurePtr(j+1)%Pressure = pTemp
      PressurePtr(j+1)%InPtr = iTemp
    end do

    DO InData = 1, NumOfPressPts
      CALL GetObjectItem(TRIM(CurrentModuleObject),PressurePtr(InData)%InPtr,Alphas,NumAlphas, &
                          Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF (ABS(Numbers(1)-RefrigData(Loop)%SHPress(Indata)) > PressToler) THEN
        CALL ShowSevereError('GetFluidPropertiesData: All superheated data for the same refrigerant must '// &
                             'use the same pressure data')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
      IF (.NOT.SameString(TempsName,Alphas(3))) THEN
        CALL ShowSevereError('GetFluidPropertiesData: All superheated data for the same property must use '// &
                             'the same temperature list')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
      IF ((NumNumbers-1) == RefrigData(Loop)%NumSuperTempPts) THEN
        RefrigData(Loop)%RhoshValues(1:RefrigData(Loop)%NumSuperTempPts,InData) = Numbers(2:NumNumbers)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Number of superheated density data points not equal to '// &
                             'number of temperature points')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
    END DO

    DEALLOCATE(PressurePtr)

    IF (NumOfPressPts == 0) THEN
      CALL ShowSevereError('GetFluidPropertiesData: No pressure data found for superheated density')
      CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
      ErrorsFound=.true.
    END IF
    IF (NumOfPressPts /= RefrigData(Loop)%NumSuperPressPts) THEN
      CALL ShowSevereError('GetFluidPropertiesData: Number of pressure points for superheated data '// &
                           'different for enthalpy and density')
      CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
      ErrorsFound=.true.
    END IF

  END DO    ! ...end of DO loop through all of the refrigerants

          ! *************** GLYCOLS ***************
          ! Go through each glycol found in the fluid names statement and read in the data
          ! Note that every valid fluid must have ALL of the necessary data or a fatal error will
          ! be produced.
  CurrentModuleObject = 'FluidProperties:Concentration'
  DO Loop = 1, NumOfGlycols

          ! Get: ***** SPECIFIC HEAT of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%CpDataPresent = .FALSE.
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for specific heat are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),SpecificHeat))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol specific heat data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      GlyRawData(Loop)%CpDataPresent = .TRUE.
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumCpTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%CpTemps(GlyRawData(Loop)%NumCpTempPts))
          GlyRawData(Loop)%CpTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the specific heat related arrays
      GlyRawData(Loop)%NumCpConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%CpConcs(GlyRawData(Loop)%NumCpConcPts))
      ALLOCATE(GlyRawData(Loop)%CpValues(GlyRawData(Loop)%NumCpTempPts,GlyRawData(Loop)%NumCpConcPts))

            ! Finally, get the specific heat and concentration values from the user input
      CurrentModuleObject = 'FluidProperties:Concentration'
      NumOfConcPts = 0
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),SpecificHeat))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%CpConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%CpConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%CpConcs(NumOfConcPts) <= GlyRawData(Loop)%CpConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumCpTempPts) THEN
            GlyRawData(Loop)%CpValues(1:GlyRawData(Loop)%NumCpTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of specific heat data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
          ! Get: ***** DENSITY of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%RhoDataPresent = .FALSE.
    CurrentModuleObject = 'FluidProperties:Concentration'
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for density are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Density))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol density data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      GlyRawData(Loop)%RhoDataPresent = .TRUE.
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumRhoTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%RhoTemps(GlyRawData(Loop)%NumRhoTempPts))
          GlyRawData(Loop)%RhoTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the density related arrays
      GlyRawData(Loop)%NumRhoConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%RhoConcs(GlyRawData(Loop)%NumRhoConcPts))
      ALLOCATE(GlyRawData(Loop)%RhoValues(GlyRawData(Loop)%NumRhoTempPts,GlyRawData(Loop)%NumRhoConcPts))

            ! Finally, get the density and concentration values from the user input
      NumOfConcPts = 0
      CurrentModuleObject = 'FluidProperties:Concentration'
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Density))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%RhoConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%RhoConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%RhoConcs(NumOfConcPts) <= GlyRawData(Loop)%RhoConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumRhoTempPts) THEN
            GlyRawData(Loop)%RhoValues(1:GlyRawData(Loop)%NumRhoTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of density data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
          ! Get: ***** CONDUCTIVITY of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%CondDataPresent = .FALSE.
    CurrentModuleObject = 'FluidProperties:Concentration'
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for conductivity are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Conductivity))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol conductivity data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      GlyRawData(Loop)%CondDataPresent = .TRUE.
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumCondTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%CondTemps(GlyRawData(Loop)%NumCondTempPts))
          GlyRawData(Loop)%CondTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the conductivity related arrays
      GlyRawData(Loop)%NumCondConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%CondConcs(GlyRawData(Loop)%NumCondConcPts))
      ALLOCATE(GlyRawData(Loop)%CondValues(GlyRawData(Loop)%NumCondTempPts,GlyRawData(Loop)%NumCondConcPts))

            ! Finally, get the conductivity and concentration values from the user input
      NumOfConcPts = 0
      CurrentModuleObject = 'FluidProperties:Concentration'
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Conductivity))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%CondConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%CondConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%CondConcs(NumOfConcPts) <= GlyRawData(Loop)%CondConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumCondTempPts) THEN
            GlyRawData(Loop)%CondValues(1:GlyRawData(Loop)%NumCondTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of conductivity data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
          ! Get: ***** VISCOSITY of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%ViscDataPresent = .FALSE.
    CurrentModuleObject = 'FluidProperties:Concentration'
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for viscosity are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Viscosity))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol viscosity data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
      GlyRawData(Loop)%ViscDataPresent = .TRUE.
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumViscTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%ViscTemps(GlyRawData(Loop)%NumViscTempPts))
          GlyRawData(Loop)%ViscTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the viscosity related arrays
      GlyRawData(Loop)%NumViscConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%ViscConcs(GlyRawData(Loop)%NumViscConcPts))
      ALLOCATE(GlyRawData(Loop)%ViscValues(GlyRawData(Loop)%NumViscTempPts,GlyRawData(Loop)%NumViscConcPts))

            ! Finally, get the viscosity and concentration values from the user input
      NumOfConcPts = 0
      CurrentModuleObject = 'FluidProperties:Concentration'
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Viscosity))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%ViscConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%ViscConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%ViscConcs(NumOfConcPts) <= GlyRawData(Loop)%ViscConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumViscTempPts) THEN
            GlyRawData(Loop)%ViscValues(1:GlyRawData(Loop)%NumViscTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of viscosity data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
  END DO  ! glycol loop

          ! Get: ***** GLYCOL CONCENTRATIONS *****
          ! Read in the GlycolConcentrations input and then set the property data accordingly
          ! Input Syntax:
          ! FluidProperties:GlycolConcentration,
          !       \memo glycol and what concentration it is
          !  A1,  \field Name
          !       \type alpha
          !       \required-field
          !       \reference GlycolConcentrations
          !  A2,  \field Glycol Type
          !       \required-field
          !       \type choice
          !       \key EthyleneGlycol
          !       \key PropyleneGlycol
          !       \key UserDefinedGlycolType
          !       \note or UserDefined Fluid (must show up as a glycol in FluidProperties:Name object)
          !  A3,  \field User Defined Glycol Name
          !       \type object-list
          !       \object-list FluidAndGlycolNames
          !  N1;  \field Glycol Concentration
          !       \type real
          !       \minimum 0.0
          !       \maximum 1.0

          ! Check to see if there is any GlycolConcentrations input.  If not, this
          ! is okay as long as the user only desires to simulate loops with water.
          ! More than one GlycolConcentrations input is not allowed.

  CurrentModuleObject = 'FluidProperties:GlycolConcentration'
  NumOfOptionalInput = GetNumObjectsFound(TRIM(CurrentModuleObject))

  NumOfGlyConcs=NumOfOptionalInput+1
  ALLOCATE(GlycolData(NumOfGlyConcs))
  ALLOCATE(GlycolUsed(NumOfGlyConcs))
  GlycolUsed=.false.
  GlycolUsed(1)=.true.  ! mark Water as always used

          ! First "glycol" is always pure water.  Load data from default arrays
  GlycolData(1)%Name = 'WATER'
  GlycolData(1)%GlycolName      = 'WATER'
  GlycolData(1)%GlycolIndex     = 0
  GlycolData(1)%Concentration   = 1.0
  GlycolData(1)%CpDataPresent   = .TRUE.
  GlycolData(1)%NumCpTempPts    = DefaultNumGlyTemps
  GlycolData(1)%RhoDataPresent  = .TRUE.
  GlycolData(1)%NumRhoTempPts   = DefaultNumGlyTemps
  GlycolData(1)%CondDataPresent = .TRUE.
  GlycolData(1)%NumCondTempPts  = DefaultNumGlyTemps
  GlycolData(1)%ViscDataPresent = .TRUE.
  GlycolData(1)%NumViscTempPts  = DefaultNumGlyTemps
  ALLOCATE(GlycolData(1)%CpTemps(GlycolData(1)%NumCpTempPts))
  ALLOCATE(GlycolData(1)%CpValues(GlycolData(1)%NumCpTempPts))
  ALLOCATE(GlycolData(1)%RhoTemps(GlycolData(1)%NumRhoTempPts))
  ALLOCATE(GlycolData(1)%RhoValues(GlycolData(1)%NumRhoTempPts))
  ALLOCATE(GlycolData(1)%CondTemps(GlycolData(1)%NumCondTempPts))
  ALLOCATE(GlycolData(1)%CondValues(GlycolData(1)%NumCondTempPts))
  ALLOCATE(GlycolData(1)%ViscTemps(GlycolData(1)%NumViscTempPts))
  ALLOCATE(GlycolData(1)%ViscValues(GlycolData(1)%NumViscTempPts))
  GlycolData(1)%CpTemps         = DefaultGlycolTemps
  GlycolData(1)%CpValues        = DefaultWaterCpData
  GlycolData(1)%RhoTemps        = DefaultGlycolTemps
  GlycolData(1)%RhoValues       = DefaultWaterRhoData
  GlycolData(1)%CondTemps       = DefaultGlycolTemps
  GlycolData(1)%CondValues      = DefaultWaterCondData
  GlycolData(1)%ViscTemps       = DefaultGlycolTemps
  GlycolData(1)%ViscValues      = DefaultWaterViscData

  NumOfGlyConcs = 1  ! Water is always available, everything else must be specified

  DO Loop = 1, NumOfOptionalInput
    CALL GetObjectItem(TRIM(CurrentModuleObject),Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
          ! Check to see if glycol name is one of the defaults or is listed in the Fluid Name list
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),GlycolData%Name,NumOfGlyConcs,ErrorInName,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...Fluid names must be unique regardless of subtype.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    GlycolFound = .FALSE.
    IF (SameString(Alphas(2),EthyleneGlycol)) THEN
      GlycolFound = .TRUE.
      NumOfGlyConcs=NumOfGlyConcs+1
      GlycolData(NumOfGlyConcs)%Name = Alphas(1)
      GlycolData(NumOfGlyConcs)%GlycolName = Alphas(2)
    ELSE IF (SameString(Alphas(2),PropyleneGlycol)) THEN
      GlycolFound = .TRUE.
      NumOfGlyConcs=NumOfGlyConcs+1
      GlycolData(NumOfGlyConcs)%Name = Alphas(1)
      GlycolData(NumOfGlyConcs)%GlycolName = Alphas(2)
    ELSEIF (SameString(Alphas(2),'UserDefinedGlycolType')) THEN
      DO InData = 1, NumOfGlycols
        IF (SameString(Alphas(3),GlyRawData(InData)%Name)) THEN
          GlycolFound = .TRUE.
          EXIT ! DO LOOP through user defined glycols
        END IF
      END DO
      IF (GlycolFound) THEN
        NumOfGlyConcs = NumOfGlyConcs + 1
        GlycolData(NumOfGlyConcs)%Name = Alphas(1)
        GlycolData(NumOfGlyConcs)%GlycolName = Alphas(3)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid reference')
        CALL ShowContinueError('... not found in the FluidProperties:Name list: "'//TRIM(Alphas(3))//'".')
        ErrorsFound = .TRUE.
      END IF
    ELSE
      CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid field')
      CALL ShowContinueError('...'//trim(cAlphaFieldNames(2))//'="'//trim(Alphas(2))//'".')
      CALL ShowContinueError('... Legal values are PropoleneGlycol, EthyleneGlycol or UserDefinedGlycolType.')
      ErrorsFound=.true.
    END IF
    IF (.not. GlycolFound) CYCLE
    GlycolData(NumOfGlyConcs)%Concentration = Numbers(1)
  END DO


          ! Now initialize the rest of the data for the glycols
  DO Loop = 2, NumOfGlyConcs
        ! Check to see if glycol name is one of the defaults or is listed in the Fluid Name list
    IF (SameString(GlycolData(Loop)%GlycolName,EthyleneGlycol)) THEN
      GlycolData(Loop)%GlycolIndex = EthyleneGlycolIndex
    ELSE IF (SameString(GlycolData(Loop)%GlycolName,PropyleneGlycol)) THEN
      GlycolData(Loop)%GlycolIndex = PropyleneGlycolIndex
    ELSE
      DO InData = 1, NumOfGlycols
        IF (SameString(GlycolData(Loop)%GlycolName,GlyRawData(InData)%Name)) THEN
          GlycolData(Loop)%GlycolIndex = InData
          EXIT ! DO LOOP through user defined glycols
        END IF
      END DO
    END IF

          ! Set the rest of the parameters...
    IF ( (GlycolData(Loop)%GlycolIndex == EthyleneGlycolIndex) .OR. &
         (GlycolData(Loop)%GlycolIndex == PropyleneGlycolIndex) ) THEN

      GlycolData(Loop)%CpDataPresent   = .TRUE.
      GlycolData(Loop)%NumCpTempPts    = DefaultNumGlyTemps
      GlycolData(Loop)%RhoDataPresent  = .TRUE.
      GlycolData(Loop)%NumRhoTempPts   = DefaultNumGlyTemps
      GlycolData(Loop)%CondDataPresent = .TRUE.
      GlycolData(Loop)%NumCondTempPts  = DefaultNumGlyTemps
      GlycolData(Loop)%ViscDataPresent = .TRUE.
      GlycolData(Loop)%NumViscTempPts  = DefaultNumGlyTemps
      ALLOCATE(GlycolData(Loop)%CpTemps(GlycolData(Loop)%NumCpTempPts))
      ALLOCATE(GlycolData(Loop)%CpValues(GlycolData(Loop)%NumCpTempPts))
      ALLOCATE(GlycolData(Loop)%RhoTemps(GlycolData(Loop)%NumRhoTempPts))
      ALLOCATE(GlycolData(Loop)%RhoValues(GlycolData(Loop)%NumRhoTempPts))
      ALLOCATE(GlycolData(Loop)%CondTemps(GlycolData(Loop)%NumCondTempPts))
      ALLOCATE(GlycolData(Loop)%CondValues(GlycolData(Loop)%NumCondTempPts))
      ALLOCATE(GlycolData(Loop)%ViscTemps(GlycolData(Loop)%NumViscTempPts))
      ALLOCATE(GlycolData(Loop)%ViscValues(GlycolData(Loop)%NumViscTempPts))
      GlycolData(Loop)%CpTemps         = DefaultGlycolTemps
      GlycolData(Loop)%RhoTemps        = DefaultGlycolTemps
      GlycolData(Loop)%CondTemps       = DefaultGlycolTemps
      GlycolData(Loop)%ViscTemps       = DefaultGlycolTemps

      IF (GlycolData(Loop)%GlycolIndex == EthyleneGlycolIndex) THEN
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyCpData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CpValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyRhoData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%RhoValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyCondData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CondValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyViscData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%ViscValues)
      ELSE    ! == PropyleneGlycolIndex
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyCpData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CpValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyRhoData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%RhoValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyCondData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CondValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyViscData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%ViscValues)
      END IF

    ELSE  ! User-defined fluid

      Index = GlycolData(Loop)%GlycolIndex

          ! Specific heat data:
      IF (GlyRawData(Index)%CpDataPresent) THEN
        GlycolData(Loop)%CpDataPresent = .TRUE.
        GlycolData(Loop)%NumCpTempPts  = GlyRawData(Index)%NumCpTempPts
        ALLOCATE(GlycolData(Loop)%CpTemps(GlycolData(Loop)%NumCpTempPts))
        ALLOCATE(GlycolData(Loop)%CpValues(GlycolData(Loop)%NumCpTempPts))
        GlycolData(Loop)%CpTemps = GlyRawData(Index)%CpTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumCpConcPts,GlyRawData(Index)%NumCpTempPts, &
                                       GlyRawData(Index)%CpConcs,GlyRawData(Index)%CpValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%CpValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Specific heat data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

          ! Density data:
      IF (GlyRawData(Index)%CpDataPresent) THEN
        GlycolData(Loop)%RhoDataPresent = .TRUE.
        GlycolData(Loop)%NumRhoTempPts  = GlyRawData(Index)%NumRhoTempPts
        ALLOCATE(GlycolData(Loop)%RhoTemps(GlycolData(Loop)%NumRhoTempPts))
        ALLOCATE(GlycolData(Loop)%RhoValues(GlycolData(Loop)%NumRhoTempPts))
        GlycolData(Loop)%RhoTemps = GlyRawData(Index)%RhoTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumRhoConcPts,GlyRawData(Index)%NumRhoTempPts, &
                                       GlyRawData(Index)%RhoConcs,GlyRawData(Index)%RhoValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%RhoValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Density data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

          ! Conductivity data:
      IF (GlyRawData(Index)%CondDataPresent) THEN
        GlycolData(Loop)%CondDataPresent = .TRUE.
        GlycolData(Loop)%NumCondTempPts  = GlyRawData(Index)%NumCondTempPts
        ALLOCATE(GlycolData(Loop)%CondTemps(GlycolData(Loop)%NumCondTempPts))
        ALLOCATE(GlycolData(Loop)%CondValues(GlycolData(Loop)%NumCondTempPts))
        GlycolData(Loop)%CondTemps = GlyRawData(Index)%CondTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumCondConcPts,GlyRawData(Index)%NumCondTempPts, &
                                       GlyRawData(Index)%CondConcs,GlyRawData(Index)%CondValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%CondValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Conductivity data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

          ! Viscosity data:
      IF (GlyRawData(Index)%ViscDataPresent) THEN
        GlycolData(Loop)%ViscDataPresent = .TRUE.
        GlycolData(Loop)%NumViscTempPts  = GlyRawData(Index)%NumViscTempPts
        ALLOCATE(GlycolData(Loop)%ViscTemps(GlycolData(Loop)%NumViscTempPts))
        ALLOCATE(GlycolData(Loop)%ViscValues(GlycolData(Loop)%NumViscTempPts))
        GlycolData(Loop)%ViscTemps = GlyRawData(Index)%ViscTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumViscConcPts,GlyRawData(Index)%NumViscTempPts, &
                                       GlyRawData(Index)%ViscConcs,GlyRawData(Index)%ViscValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%ViscValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Viscosity data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

    END IF

  END DO

  NumOfGlycols = NumOfGlyConcs  ! Reset number of glycols to actual number

  IF (.not. ErrorsFound) CALL InitializeGlycolTempLimits(ErrorsFound)   ! Initialize the Temp limits for the glycols

  IF (.not. ErrorsFound) CALL InitializeRefrigerantLimits(ErrorsFound) ! Initialize the limits for the refrigerants

  DEALLOCATE(FluidTemps)

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(Numbers)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(lNumericFieldBlanks)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetFluidPropertiesData: Previous errors in input cause program termination.')
  ENDIF

  IF (GetNumSectionsFound(MakeUPPERCase('ReportGlycols')) > 0) DebugReportGlycols=.true.
  IF (GetNumSectionsFound(MakeUPPERCase('ReportRefrigerants')) > 0) DebugReportRefrigerants=.true.
  IF (GetNumSectionsFound(MakeUPPERCase('IncreaseGlycolErrorLimit')) > 0)   &
                    GlycolErrorLimitTest=GlycolErrorLimitTest+10
  IF (GetNumSectionsFound(MakeUPPERCase('IncreaseRefrigerantErrorLimit')) > 0)   &
                    RefrigerantErrorLimitTest=RefrigerantErrorLimitTest+10

  IF (DebugReportGlycols) CALL ReportAndTestGlycols
  IF (DebugReportRefrigerants) CALL ReportAndTestRefrigerants

  RETURN

END SUBROUTINE GetFluidPropertiesData

!*****************************************************************************

SUBROUTINE InterpDefValuesForGlycolConc(NumOfConcs,NumOfTemps,RawConcData,RawPropData,Concentration,InterpData)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to find the values for the property
          ! data at a particular concentration from default data that is at "generic"
          ! concentrations.  This is then returned to the main get routine and
          ! then used later in the program to find values at various temperatures.
          ! The ultimate purpose of this is to avoid double interpolation during
          ! the simulation.  Since concentration does not change during the simulation,
          ! there is no reason to do a double interpolation every time a property
          ! value is needed.

          ! METHODOLOGY EMPLOYED:
          ! Fairly straight forward--find the two concentrations between which
          ! the actual concentration falls and then interpolate the property
          ! data using standard linear interpolation.  Note that data is stored
          ! in the format: 2dArray(Concentration,Temperature)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,              INTENT(IN)  :: NumOfConcs       ! number of concentrations (dimension of raw data)
  INTEGER,              INTENT(IN)  :: NumOfTemps       ! number of temperatures (dimension of raw data)
  REAL(r64), DIMENSION(:),   INTENT(IN)  :: RawConcData      ! concentrations for raw data
  REAL(r64), DIMENSION(:,:), INTENT(IN)  :: RawPropData      ! raw property data (concentration, temperature)
  REAL(r64),                 INTENT(IN)  :: Concentration    ! concentration of actual fluid mix
  REAL(r64), DIMENSION(:),   INTENT(OUT) :: InterpData       ! interpolated output data at proper concentration

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ConcToler = 0.0001d0    ! Some reasonable value for comparisons

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: HiIndex      ! index on the high side of the concentration
  REAL(r64)         :: InterpFrac   ! intermediate value for interpolations
  INTEGER           :: LoopC        ! loop counter for concentration
  INTEGER           :: LoopT        ! loop counter for temperature

          ! FLOW:
          ! First, find where the actual concentration falls between the concentration data.
          ! Then, interpolate if necessary.
  IF (Concentration < RawConcData(1)) THEN  ! Concentration too low
    CALL ShowWarningError('Glycol concentration out of range for data (too low), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for lowest concentration entered')
    InterpData = RawPropData(1,:)
  ELSE IF (Concentration > RawConcData(NumOfConcs)) THEN    ! Concentration too high
    CALL ShowWarningError('Glycol concentration out of range for data (too high), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for highest concentration entered')
    InterpData = RawPropData(NumOfConcs,:)
  ELSE  ! Concentration somewhere between lowest and highest point--interpolate
    HiIndex = NumOfConcs    ! Default to highest concentration
    DO LoopC = 2, NumOfConcs-1
      IF (Concentration <= RawConcData(LoopC)) THEN
        HiIndex = LoopC
        EXIT ! LoopC DO loop
      END IF
    END DO
    IF ( ABS(RawConcData(HiIndex)-RawConcData(HiIndex-1)) >= ConcToler ) THEN
      InterpFrac = ( RawConcData(HiIndex) - Concentration ) / ( RawConcData(HiIndex) - RawConcData(HiIndex-1) )
      DO LoopT = 1, NumOfTemps
        IF ( (RawPropData(HiIndex,LoopT) < ConcToler) .OR. (RawPropData(HiIndex-1,LoopT) < ConcToler) ) THEN
          ! One of the two values is zero--so we cannot interpolate for this point (assign to zero)
          InterpData(LoopT) = 0.0
        ELSE
          InterpData(LoopT) = RawPropData(HiIndex,LoopT) &
                             -( InterpFrac * (RawPropData(HiIndex,LoopT)-RawPropData(HiIndex-1,LoopT)) )
        END IF
      END DO
    ELSE    ! user has input data for concentrations that are too close or repeated, this must be fixed
      CALL ShowFatalError('InterpDefValuesForGlycolConc: concentration values too close or data repeated, ' &
                          //'check your fluid property input data')
    END IF
  END IF

  RETURN

END SUBROUTINE InterpDefValuesForGlycolConc

!*****************************************************************************

SUBROUTINE InterpValuesForGlycolConc(NumOfConcs,NumOfTemps,RawConcData,RawPropData,Concentration,InterpData)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to find the values for the property
          ! data at a particular concentration from default data that is at "generic"
          ! concentrations.  This is then returned to the main get routine and
          ! then used later in the program to find values at various temperatures.
          ! The ultimate purpose of this is to avoid double interpolation during
          ! the simulation.  Since concentration does not change during the simulation,
          ! there is no reason to do a double interpolation every time a property
          ! value is needed.

          ! METHODOLOGY EMPLOYED:
          ! Fairly straight forward--find the two concentrations between which
          ! the actual concentration falls and then interpolate the property
          ! data using standard linear interpolation.  Note that data is stored
          ! in the format: 2dArray(Temperature,Concentration).  Temperature
          ! data is not needed here since we are only interpolating to eliminate
          ! the concentration as a variable (it really isn't one during the
          ! simulation).

          ! REFERENCES:
          ! GetFluidPropertiesData--subroutine forces user to input data in
          ! order of increasing concentration.  This is assumed in this subroutine.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,              INTENT(IN)  :: NumOfConcs       ! number of concentrations (dimension of raw data)
  INTEGER,              INTENT(IN)  :: NumOfTemps       ! number of temperatures (dimension of raw data)
  REAL(r64), DIMENSION(:),   INTENT(IN)  :: RawConcData      ! concentrations for raw data
  REAL(r64), DIMENSION(:,:), INTENT(IN)  :: RawPropData      ! raw property data (temperature,concentration)
  REAL(r64),                 INTENT(IN)  :: Concentration    ! concentration of actual fluid mix
  REAL(r64), DIMENSION(:),   INTENT(OUT) :: InterpData       ! interpolated output data at proper concentration

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ConcToler = 0.0001d0    ! Some reasonable value for comparisons

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: HiIndex      ! index on the high side of the concentration
  REAL(r64)         :: InterpFrac   ! intermediate value for interpolations
  INTEGER           :: LoopC        ! loop counter for concentration
  INTEGER           :: LoopT        ! loop counter for temperature

          ! FLOW:
          ! First, find where the actual concentration falls between the concentration data.
          ! Then, interpolate if necessary.
  IF (Concentration < RawConcData(1)) THEN  ! Concentration too low
    CALL ShowWarningError('Glycol concentration out of range for data (too low), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for lowest concentration entered')
    InterpData = RawPropData(:,1)
  ELSE IF (Concentration > RawConcData(NumOfConcs)) THEN    ! Concentration too high
    CALL ShowWarningError('Glycol concentration out of range for data (too high), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for highest concentration entered')
    InterpData = RawPropData(:,NumOfConcs)
  ELSE  ! Concentration somewhere between lowest and highest point--interpolate
    HiIndex = NumOfConcs    ! Default to highest concentration
    DO LoopC = 2, NumOfConcs-1
      IF (Concentration <= RawConcData(LoopC)) THEN
        HiIndex = LoopC
        EXIT ! LoopC DO loop
      END IF
    END DO
    IF ( ABS(RawConcData(HiIndex)-RawConcData(HiIndex-1)) >= ConcToler ) THEN
      InterpFrac = ( RawConcData(HiIndex) - Concentration ) / ( RawConcData(HiIndex) - RawConcData(HiIndex-1) )
      DO LoopT = 1, NumOfTemps
        IF ( (RawPropData(LoopT,HiIndex) < ConcToler) .OR. (RawPropData(LoopT,HiIndex-1) < ConcToler) ) THEN
          InterpData(LoopT) = 0.0
        ELSE
          InterpData(LoopT) = RawPropData(LoopT,HiIndex) &
                             -( InterpFrac * (RawPropData(LoopT,HiIndex)-RawPropData(LoopT,HiIndex-1)) )
        END IF
      END DO
    ELSE    ! user has input data for concentrations that are too close or repeated, this must be fixed
      CALL ShowFatalError('InterpValuesForGlycolConc: concentration values too close or data repeated, check ' &
                          //'your fluid property input data')
    END IF
  END IF

  RETURN

END SUBROUTINE InterpValuesForGlycolConc

!*****************************************************************************

SUBROUTINE InitializeGlycolTempLimits(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine sets up the min/max temperature limits for the glycol properties.
          ! Most properties requested (e.g., Specific Heat) must be > 0 but the tables may
          ! be set up for symmetry and not be limited to just valid values.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! set to true if errors found here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: GlycolNum
  INTEGER :: IndexNum
  LOGICAL :: Failure

  DO GlycolNum=1,NumOfGlycols
    IF (GlycolData(GlyColNum)%CPDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumCpTempPts
        IF (GlycolData(GlycolNum)%CpValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CpLowTempIndex = IndexNum
        GlycolData(GlycolNum)%CpLowTempValue = GlycolData(GlycolNum)%CpTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumCpTempPts, 1, -1
        IF (GlycolData(GlycolNum)%CpValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CpHighTempIndex = IndexNum
        GlycolData(GlycolNum)%CpHighTempValue = GlycolData(GlycolNum)%CpTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumRhoTempPts
        IF (GlycolData(GlycolNum)%RhoValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%RhoLowTempIndex = IndexNum
        GlycolData(GlycolNum)%RhoLowTempValue = GlycolData(GlycolNum)%RhoTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumRhoTempPts, 1, -1
        IF (GlycolData(GlycolNum)%RhoValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%RhoHighTempIndex = IndexNum
        GlycolData(GlycolNum)%RhoHighTempValue = GlycolData(GlycolNum)%RhoTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumCondTempPts
        IF (GlycolData(GlycolNum)%CondValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CondLowTempIndex = IndexNum
        GlycolData(GlycolNum)%CondLowTempValue = GlycolData(GlycolNum)%CondTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumCondTempPts, 1, -1
        IF (GlycolData(GlycolNum)%CondValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CondHighTempIndex = IndexNum
        GlycolData(GlycolNum)%CondHighTempValue = GlycolData(GlycolNum)%CondTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumViscTempPts
        IF (GlycolData(GlycolNum)%ViscValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%ViscLowTempIndex = IndexNum
        GlycolData(GlycolNum)%ViscLowTempValue = GlycolData(GlycolNum)%ViscTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumViscTempPts, 1, -1
        IF (GlycolData(GlycolNum)%ViscValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%ViscHighTempIndex = IndexNum
        GlycolData(GlycolNum)%ViscHighTempValue = GlycolData(GlycolNum)%ViscTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    Failure=.false.
    ! Check to see that all are set to non-zero
    IF (GlycolData(GlyColNum)%CpDataPresent) THEN
      IF (GlycolData(GlycolNum)%CpLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%CpHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      IF (GlycolData(GlycolNum)%RhoLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%RhoHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      IF (GlycolData(GlycolNum)%CondLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%CondHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      IF (GlycolData(GlycolNum)%ViscLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%ViscHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (Failure) THEN
      CALL ShowSevereError('InitializeGlycolTempLimits: Required values for Glycol='//TRIM(GlycolData(GlycolNum)%Name)//  &
          ' are all zeroes for some data types.')
      ErrorsFound=.true.
    ENDIF
  ENDDO
  RETURN

END SUBROUTINE InitializeGlycolTempLimits

!*****************************************************************************

SUBROUTINE InitializeRefrigerantLimits(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine sets up the min/max limits (usually temperature and/or pressure)
          ! for the refrigerant properties.
          ! Most properties requested (e.g., Specific Heat) must be > 0 but the tables may
          ! be set up for symmetry and not be limited to just valid values.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! set to true if errors found here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum
  INTEGER :: IndexNum
  LOGICAL :: Failure

  DO RefrigNum=1,NumOfRefrigerants
    DO IndexNum=1,RefrigData(RefrigNum)%NumPsPoints
      IF (RefrigData(RefrigNum)%PsValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%PsLowPresIndex=IndexNum
      RefrigData(RefrigNum)%PsLowPresValue=RefrigData(RefrigNum)%PsValues(IndexNum)
      RefrigData(RefrigNum)%PsLowTempValue=RefrigData(RefrigNum)%PsTemps(IndexNum)
      RefrigData(RefrigNum)%PsLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumPsPoints,1,-1
      IF (RefrigData(RefrigNum)%PsValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%PsHighPresIndex=IndexNum
      RefrigData(RefrigNum)%PsHighPresValue=RefrigData(RefrigNum)%PsValues(IndexNum)
      RefrigData(RefrigNum)%PsHighTempValue=RefrigData(RefrigNum)%PsTemps(IndexNum)
      RefrigData(RefrigNum)%PsHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumHPoints
      IF (RefrigData(RefrigNum)%HfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfLowTempValue=RefrigData(RefrigNum)%HfValues(IndexNum)
      RefrigData(RefrigNum)%HfLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumHPoints,1,-1
      IF (RefrigData(RefrigNum)%HfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfHighTempValue=RefrigData(RefrigNum)%HfValues(IndexNum)
      RefrigData(RefrigNum)%HfHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumHPoints
      IF (RefrigData(RefrigNum)%HfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfgLowTempValue=RefrigData(RefrigNum)%HfgValues(IndexNum)
      RefrigData(RefrigNum)%HfgLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumHPoints,1,-1
      IF (RefrigData(RefrigNum)%HfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfgHighTempValue=RefrigData(RefrigNum)%HfgValues(IndexNum)
      RefrigData(RefrigNum)%HfgHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumCpPoints
      IF (RefrigData(RefrigNum)%CpfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfLowTempValue=RefrigData(RefrigNum)%CpfValues(IndexNum)
      RefrigData(RefrigNum)%CpfLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumCpPoints,1,-1
      IF (RefrigData(RefrigNum)%CpfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfHighTempValue=RefrigData(RefrigNum)%CpfValues(IndexNum)
      RefrigData(RefrigNum)%CpfHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumCpPoints
      IF (RefrigData(RefrigNum)%CpfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfgLowTempValue=RefrigData(RefrigNum)%CpfgValues(IndexNum)
      RefrigData(RefrigNum)%CpfgLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumCpPoints,1,-1
      IF (RefrigData(RefrigNum)%CpfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfgHighTempValue=RefrigData(RefrigNum)%CpfgValues(IndexNum)
      RefrigData(RefrigNum)%CpfgHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumRhoPoints
      IF (RefrigData(RefrigNum)%RhofValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofLowTempValue=RefrigData(RefrigNum)%RhofValues(IndexNum)
      RefrigData(RefrigNum)%RhofLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumRhoPoints,1,-1
      IF (RefrigData(RefrigNum)%RhofValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofHighTempValue=RefrigData(RefrigNum)%RhofValues(IndexNum)
      RefrigData(RefrigNum)%RhofHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumRhoPoints
      IF (RefrigData(RefrigNum)%RhofgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofgLowTempValue=RefrigData(RefrigNum)%RhofgValues(IndexNum)
      RefrigData(RefrigNum)%RhofgLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumRhoPoints,1,-1
      IF (RefrigData(RefrigNum)%RhofgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofgHighTempValue=RefrigData(RefrigNum)%RhofgValues(IndexNum)
      RefrigData(RefrigNum)%RhofgHighTempIndex=IndexNum
      EXIT
    ENDDO
    Failure=.false.
    ! Check to see that all are set to non-zero
    IF (RefrigData(RefrigNum)%NumPsPoints > 0) THEN
      IF (RefrigData(RefrigNum)%PsLowPresIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%PsLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%PsHighPresIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%PsHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (RefrigData(RefrigNum)%NumHPoints > 0) THEN
      IF (RefrigData(RefrigNum)%HfLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%HfgLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%HfHighTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%HfgHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (RefrigData(RefrigNum)%NumCpPoints > 0) THEN
      IF (RefrigData(RefrigNum)%CpfLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%CpfgLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%CpfHighTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%CpfgHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (RefrigData(RefrigNum)%NumRhoPoints > 0) THEN
      IF (RefrigData(RefrigNum)%RhofLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%RhofgLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%RhofHighTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%RhofgHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (Failure) THEN
      CALL ShowSevereError('InitializeRefrigerantLimits: Required values for Refrigerant='//  &
           TRIM(RefrigData(RefrigNum)%Name)//  &
          ' are all zeroes for some data types.')
      ErrorsFound=.true.
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE InitializeRefrigerantLimits

!*****************************************************************************

SUBROUTINE ReportAndTestGlycols

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is written to report and test glycols through their range
          ! of temperatures and make sure that proper values will be returned.

          ! METHODOLOGY EMPLOYED:
          ! Use internal structure as the temperature limits. Write output to the
          ! debug output file.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  REAL(r64), PARAMETER        :: incr=10.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: GlycolNum        ! Loop Counter
  REAL(r64) :: Temperature    ! Temperature to drive values
  REAL(r64) :: ReturnValue    ! Values returned from glycol functions
  INTEGER :: Loop             ! Loop Counter
  INTEGER :: GlycolIndex      ! index used in routine / function calls, value is returned on first use (when index=0)

  GetInput = .FALSE.  ! input has already been gotten

  DO GlycolNum=1,NumOfGlycols
    GlycolIndex=0     ! used in routine calls -- value is returned when first 0
    ! Lay out the basic values:
    IF (GlycolData(GlycolNum)%GlycolName /= ' ') THEN
      write(OutputFileDebug,fmta) 'Glycol='//TRIM(GlycolData(GlycolNum)%Name)//  &
            ', Mixture fluid='//TRIM(GlycolData(GlycolNum)%GlycolName)
    ELSE
      write(OutputFileDebug,fmta) 'Glycol='//TRIM(GlycolData(GlycolNum)%Name)
    ENDIF
    write(OutputFileDebug,fmta) 'Concentration:,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%Concentration,2))
    IF (GlycolData(GlyColNum)%CPDataPresent) THEN
      write(OutputFileDebug,fmta) 'Specific Heat Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat:'
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpValues(GlycolData(GlycolNum)%NumCpTempPts),2))
    ENDIF
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      write(OutputFileDebug,fmta) 'Density Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Density:'
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoValues(GlycolData(GlycolNum)%NumRhoTempPts),2))
    ENDIF
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      write(OutputFileDebug,fmta) 'Conductivity Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Conductivity:'
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondValues(GlycolData(GlycolNum)%NumCondTempPts),2))
    ENDIF
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      write(OutputFileDebug,fmta) 'Viscosity Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Viscosity:'
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscValues(GlycolData(GlycolNum)%NumViscTempPts),2))
    ENDIF
! ============================================
! Glycol Results, using out of bounds to out of bounds values in calling
! ============================================

! ========= Specific Heat from Temperatures
    write(OutputFileDebug,fmta) 'Glycol='//TRIM(GlycolData(GlycolNum)%Name)//' **** Results ****'
    IF (GlycolData(GlyColNum)%CPDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%CpTemps(Loop) +   &
           (GlycolData(GlycolNum)%CpTemps(Loop+1)-GlycolData(GlycolNum)%CpTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat:'
      Temperature=GlycolData(GlycolNum)%CpTemps(1)-incr
      ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        Temperature=GlycolData(GlycolNum)%CpTemps(Loop)
        ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=GlycolData(GlycolNum)%CpTemps(Loop) +   &
           (GlycolData(GlycolNum)%CpTemps(Loop+1)-GlycolData(GlycolNum)%CpTemps(Loop))/2.0
        ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts)
      ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts)+incr
      ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Density from Temperatures
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Density Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%RhoTemps(Loop) +   &
           (GlycolData(GlycolNum)%RhoTemps(Loop+1)-GlycolData(GlycolNum)%RhoTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Density:'
      Temperature=GlycolData(GlycolNum)%RhoTemps(1)-incr
      ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        Temperature=GlycolData(GlycolNum)%RhoTemps(Loop)
        ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
        Temperature=GlycolData(GlycolNum)%RhoTemps(Loop) +   &
           (GlycolData(GlycolNum)%RhoTemps(Loop+1)-GlycolData(GlycolNum)%RhoTemps(Loop))/2.0
        ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      enddo
      Temperature=GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts)
      ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      Temperature=GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts)+incr
      ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,3))
    ENDIF

! ========= Conductivity from Temperatures
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Conductivity Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%CondTemps(Loop) +   &
           (GlycolData(GlycolNum)%CondTemps(Loop+1)-GlycolData(GlycolNum)%CondTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Conductivity:'
      Temperature=GlycolData(GlycolNum)%CondTemps(1)-incr
      ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        Temperature=GlycolData(GlycolNum)%CondTemps(Loop)
        ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
        Temperature=GlycolData(GlycolNum)%CondTemps(Loop) +   &
           (GlycolData(GlycolNum)%CondTemps(Loop+1)-GlycolData(GlycolNum)%CondTemps(Loop))/2.0
        ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      enddo
      Temperature=GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts)
      ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      Temperature=GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts)+incr
      ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,3))
    ENDIF

! ========= Viscosity from Temperatures
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Viscosity Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%ViscTemps(Loop) +   &
           (GlycolData(GlycolNum)%ViscTemps(Loop+1)-GlycolData(GlycolNum)%ViscTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Viscosity:'
      Temperature=GlycolData(GlycolNum)%ViscTemps(1)-incr
      ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        Temperature=GlycolData(GlycolNum)%ViscTemps(Loop)
        ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
        Temperature=GlycolData(GlycolNum)%ViscTemps(Loop) +   &
           (GlycolData(GlycolNum)%ViscTemps(Loop+1)-GlycolData(GlycolNum)%ViscTemps(Loop))/2.0
        ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
      enddo
      Temperature=GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts)
      ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
      Temperature=GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts)+incr
      ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,4))
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportAndTestGlycols

!*****************************************************************************

SUBROUTINE ReportAndTestRefrigerants

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008; only stub provided to satisfy calling programs.
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is written to report and test refrigerants through their range
          ! of inputs (temperatures?) and make sure that proper values will be returned.

          ! METHODOLOGY EMPLOYED:
          ! Use internal structure as the range limits. Write output to the
          ! debug output file.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  REAL(r64), PARAMETER        :: incr=10.0d0
  REAL(r64), PARAMETER        :: Quality=1.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum        ! Loop Counter
  REAL(r64) :: Temperature    ! Temperature to drive values
!  REAL(r64) :: Pressure       ! Pressure to drive values
  REAL(r64) :: ReturnValue    ! Values returned from refrigerant functions
  INTEGER :: Loop             ! Loop Counter
  INTEGER :: Loop1            ! Loop Counter
  INTEGER :: RefrigIndex      !

  GetInput = .FALSE.  ! input has already been gotten

  DO RefrigNum=1,NumOfRefrigerants
    RefrigIndex=0     ! used in routine calls -- value is returned when first 0
    ! Lay out the basic values:
    IF (RefrigData(RefrigNum)%Name /= ' ') THEN
      write(OutputFileDebug,fmta) 'Refrigerant='//TRIM(RefrigData(RefrigNum)%Name)
    ENDIF
    IF (RefrigData(RefrigNum)%NumPsPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Saturation Pressures Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Saturation Pressure:'
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsValues(RefrigData(RefrigNum)%NumPsPoints),2))
    ENDIF
    IF (RefrigData(RefrigNum)%NumHPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Enthalpy Saturated Fluid Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Enthalpy Saturated Fluid:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfValues(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta) 'Enthalpy Saturated Fluid/Gas Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Enthalpy Saturated Fluid/Gas:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgValues(RefrigData(RefrigNum)%NumHPoints),2))
    ENDIF
    IF (RefrigData(RefrigNum)%NumCpPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Specific Heat Saturated Fluid Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Saturated Fluid:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfValues(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta) 'Specific Heat Saturated Fluid/Gas Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Saturated Fluid/Gas:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgValues(RefrigData(RefrigNum)%NumCpPoints),2))
    ENDIF
    IF (RefrigData(RefrigNum)%NumRhoPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Density Saturated Fluid Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Density Saturated Fluid:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta) 'Density Saturated Fluid/Gas Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Density Saturated Fluid/Gas:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgValues(RefrigData(RefrigNum)%NumRhoPoints),2))
    ENDIF

    IF (RefrigData(RefrigNum)%NumSuperTempPts > 0 .and. RefrigData(RefrigNum)%NumSuperPressPts > 0) THEN
      write(OutputFileDebug,fmta) 'Superheated Gas Fluid Data points:,NumTemperaturePoints=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%NumSuperTempPts))//',NumPressurePoints=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%NumSuperPressPts))
      write(OutputFileDebug,fmta,advance='No') 'Superheated Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumSuperTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(Loop),3))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(RefrigData(RefrigNum)%NumSuperTempPts),3))
      write(OutputFileDebug,fmta,advance='No') 'Superheated Pressures:'
      do Loop=1,RefrigData(RefrigNum)%NumSuperPressPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHPress(Loop),3))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHPress(RefrigData(RefrigNum)%NumSuperPressPts),3))
      do Loop=1,RefrigData(RefrigNum)%NumSuperPressPts
        write(OutputFileDebug,fmta) 'Superheated Pressure:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHPress(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Enthalpy Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperTempPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(Loop1,Loop),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(RefrigData(RefrigNum)%NumSuperTempPts,Loop),3))
      enddo
      do Loop=1,RefrigData(RefrigNum)%NumSuperPressPts
        write(OutputFileDebug,fmta) 'Superheated Pressure:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHPress(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Density Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperTempPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(Loop1,Loop),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(RefrigData(RefrigNum)%NumSuperTempPts,Loop),3))
      enddo
      do Loop=1,RefrigData(RefrigNum)%NumSuperTempPts
        write(OutputFileDebug,fmta) 'Superheated Temperature:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Enthalpy Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperPressPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(Loop,Loop1),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(Loop,RefrigData(RefrigNum)%NumSuperPressPts),3))
      enddo
      do Loop=1,RefrigData(RefrigNum)%NumSuperTempPts
        write(OutputFileDebug,fmta) 'Superheated Temperature:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Density Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperPressPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(Loop,Loop1),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(Loop,RefrigData(RefrigNum)%NumSuperPressPts),3))
      enddo
    ENDIF

! ============================================
! Refrigeration Results, using out of bounds to out of bounds values in calling
! ============================================

! ========= Pressure from Temperatures
    write(OutputFileDebug,fmta) 'Refrigerant='//TRIM(RefrigData(RefrigNum)%Name)//' **** Results ****'
    IF (RefrigData(RefrigNum)%NumPsPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Pressure Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%PsTemps(Loop) +   &
           (RefrigData(RefrigNum)%PsTemps(Loop+1)-RefrigData(RefrigNum)%PsTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Pressures:'
      Temperature=RefrigData(RefrigNum)%PsTemps(1)-incr
      ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        Temperature=RefrigData(RefrigNum)%PsTemps(Loop)
        ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%PsTemps(Loop) +   &
           (RefrigData(RefrigNum)%PsTemps(Loop+1)-RefrigData(RefrigNum)%PsTemps(Loop))/2.0
        ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints)
      ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints)+incr
      ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Enthalpy from Temperatures
    IF (RefrigData(RefrigNum)%NumHPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Enthalpy Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%HTemps(Loop) +   &
           (RefrigData(RefrigNum)%HTemps(Loop+1)-RefrigData(RefrigNum)%HTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Enthalpy:'
      Temperature=RefrigData(RefrigNum)%HTemps(1)-incr
      ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        Temperature=RefrigData(RefrigNum)%HTemps(Loop)
        ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%HTemps(Loop) +   &
           (RefrigData(RefrigNum)%HTemps(Loop+1)-RefrigData(RefrigNum)%HTemps(Loop))/2.0
        ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints)
      ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints)+incr
      ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Specific Heat from Temperatures
    IF (RefrigData(RefrigNum)%NumCpPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%CpTemps(Loop) +   &
           (RefrigData(RefrigNum)%CpTemps(Loop+1)-RefrigData(RefrigNum)%CpTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Specific Heat:'
      Temperature=RefrigData(RefrigNum)%CpTemps(1)-incr
      ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        Temperature=RefrigData(RefrigNum)%CpTemps(Loop)
        ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%CpTemps(Loop) +   &
           (RefrigData(RefrigNum)%CpTemps(Loop+1)-RefrigData(RefrigNum)%CpTemps(Loop))/2.0
        ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints)
      ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints)+incr
      ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Density from Temperatures
    IF (RefrigData(RefrigNum)%NumRhoPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Density Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%RhoTemps(Loop) +   &
           (RefrigData(RefrigNum)%RhoTemps(Loop+1)-RefrigData(RefrigNum)%RhoTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Density:'
      Temperature=RefrigData(RefrigNum)%RhoTemps(1)-incr
      ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        Temperature=RefrigData(RefrigNum)%RhoTemps(Loop)
        ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%RhoTemps(Loop) +   &
           (RefrigData(RefrigNum)%RhoTemps(Loop+1)-RefrigData(RefrigNum)%RhoTemps(Loop))/2.0
        ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints)
      ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints)+incr
      ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportAndTestRefrigerants

!*****************************************************************************

FUNCTION GetSatPressureRefrig(Refrigerant,Temperature,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Simon Rees
        !       DATE WRITTEN   24 May 2002
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! This finds the saturation pressure for given temperature.

        ! METHODOLOGY EMPLOYED:
        ! Calls FindArrayIndex to find indices either side of requested temperature
        ! and linearly interpolates the corresponding saturation pressure values.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN)  :: Temperature ! actual temperature given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HiTempIndex                ! index value of next highest Temperature from table
  INTEGER :: LoTempIndex                ! index value of next lowest Temperature from table
  INTEGER :: RefrigNum                  ! index for refrigerant under consideration
  REAL(r64)    :: TempInterpRatio            ! ratio to interpolate in temperature domain
 ! error counters and dummy string
  LOGICAL :: ErrorFlag                  ! error flag for current call
  INTEGER,SAVE :: TempRangeErrCount=0   ! cumulative error counter
  INTEGER, SAVE :: TempRangeErrIndex=0

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatPressureRefrig','properties',calledfrom)
  ENDIF

  ErrorFlag = .False.

  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatPressureRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! determine array indices for
  LoTempIndex = FindArrayIndex(Temperature, RefrigData(RefrigNum)%PsTemps,  &
                                            RefrigData(RefrigNum)%PsLowTempIndex,RefrigData(RefrigNum)%PsHighTempIndex)
  HiTempIndex = LoTempIndex + 1

  ! check for out of data bounds problems
  IF (LoTempIndex == 0) THEN
    ReturnValue = RefrigData(RefrigNum)%PsValues(RefrigData(RefrigNum)%PsLowTempIndex)
    ErrorFlag = .True.
  ELSE IF(HiTempIndex > RefrigData(RefrigNum)%PsHighTempIndex) THEN
    ReturnValue = RefrigData(RefrigNum)%PsValues(RefrigData(RefrigNum)%PsHighTempIndex)
    ErrorFlag = .True.
  ELSE
   ! find interpolation ratio w.r.t temperature
    TempInterpRatio = (Temperature - RefrigData(RefrigNum)%PsTemps(LoTempIndex)) / &
                         (RefrigData(RefrigNum)%PsTemps(HiTempIndex) - RefrigData(RefrigNum)%PsTemps(LoTempIndex))

    ! apply final linear interpolation
    ReturnValue = RefrigData(RefrigNum)%PsValues(LoTempIndex) + TempInterpRatio * &
                              (RefrigData(RefrigNum)%PsValues(HiTempIndex) - RefrigData(RefrigNum)%PsValues(LoTempIndex))
  ENDIF

  IF (.not. WarmupFlag .and. ErrorFlag) THEN
     TempRangeErrCount = TempRangeErrCount + 1
    ! send warning
    IF (TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('GetSatPressureRefrig: Saturation temperature requested is out of range for supplied data: **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('..Refrigerant Temperature='//TRIM(RoundSigDigits(Temperature,2))//  &
                          ' Returned saturated pressure value = '//TRIM(RoundSigDigits(ReturnValue,0)))
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('GetSatPressureRefrig: Refrigerant Saturation temperature out of range error',  &
                                             TempRangeErrIndex,ReportMaxOf=Temperature,ReportMinOf=Temperature,  &
                                             ReportMaxUnits='{C}',ReportMinUnits='{C}')
    ENDIF
  END IF

  RETURN

END FUNCTION GetSatPressureRefrig

!*****************************************************************************

FUNCTION GetSatTemperatureRefrig(Refrigerant, Pressure, RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Simon Rees
        !       DATE WRITTEN   24 May 2002
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! This finds the saturation temperature for given pressure.

        ! METHODOLOGY EMPLOYED:
        ! Calls FindArrayIndex to find indices either side of requested pressure
        ! and linearly interpolates the corresponding saturation temperature values.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant    ! carries in substance name
  REAL(r64), INTENT(IN)         :: Pressure       ! actual temperature given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HiPresIndex        ! index value of next highest Temperature from table
  INTEGER :: LoPresIndex        ! index value of next lowest Temperature from table
  INTEGER :: RefrigNum          ! index for refrigerant under consideration
  REAL(r64)    :: PresInterpRatio    ! ratio to interpolate in temperature domain
 ! error counters and dummy string
  LOGICAL :: ErrorFlag                  ! error flag for current call
  INTEGER,SAVE :: PresRangeErrCount=0   ! cumulative error counter
  INTEGER,SAVE :: PresRangeErrIndex=0

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatTemperatureRefrig','properties',calledfrom)
  ENDIF

  ErrorFlag = .False.

  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatTemperatureRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! get the array indices
  LoPresIndex = FindArrayIndex(Pressure, RefrigData(RefrigNum)%PsValues,  &
                                RefrigData(RefrigNum)%PsLowPresIndex,RefrigData(RefrigNum)%PsHighPresIndex)
  HiPresIndex = LoPresIndex + 1

  ! check for out of data bounds problems
  IF (LoPresIndex == 0) THEN
    ReturnValue = RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%PsLowPresIndex)
    ErrorFlag = .True.
  ELSE IF(HiPresIndex > RefrigData(RefrigNum)%PsHighPresIndex) THEN
    ReturnValue = RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%PsHighPresIndex)
    ErrorFlag = .True.
  ELSE
  ! find interpolation ratio w.r.t temperature
    PresInterpRatio = (Pressure - RefrigData(RefrigNum)%PsValues(LoPresIndex)) / &
                         (RefrigData(RefrigNum)%PsValues(HiPresIndex) - RefrigData(RefrigNum)%PsValues(LoPresIndex))

    ! apply final linear interpolation
    ReturnValue = RefrigData(RefrigNum)%PsTemps(LoPresIndex) + PresInterpRatio * &
                              (RefrigData(RefrigNum)%PsTemps(HiPresIndex) - &
                               RefrigData(RefrigNum)%PsTemps(LoPresIndex))
  ENDIF

  IF(.NOT. WarmupFlag .and. ErrorFlag)THEN
     PresRangeErrCount = PresRangeErrCount + 1
   ! send warning
    IF (PresRangeErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('GetSatTemperatureRefrig: Saturation pressure requested is out of range for supplied data: **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('..Refrigerant Pressure='//TRIM(RoundSigDigits(Pressure,2))//  &
                          ' Returned saturated temperature value ='//TRIM(RoundSigDigits(ReturnValue,0)))
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('GetSatTemperatureRefrig: Refrigerant saturation pressure out of range error',  &
                                             PresRangeErrIndex,ReportMinOf=Pressure,ReportMaxOf=Pressure,  &
                                             ReportMinUnits='{Pa}',ReportMaxUnits='{Pa}')
    ENDIF
  END IF
  RETURN

END FUNCTION GetSatTemperatureRefrig

!*****************************************************************************

FUNCTION GetSatEnthalpyRefrig(Refrigerant,Temperature,Quality,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees (May 2002)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! This finds enthalpy for given temperature and a quality under the vapor dome.
        ! This fucntion is only called with a valid refrigerant and quality between 0 and 1.

        ! METHODOLOGY EMPLOYED:
        ! Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        ! liquid  and vapour enthalpies according to the given quality.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN)  :: Quality     ! actual quality given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum    ! index for refrigerant under consideration

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatEnthalpyRefrig','properties',calledfrom)
  ENDIF

  IF ((Quality < 0.0) .OR. (Quality > 1.0)) THEN
    CALL ShowSevereError('GetSatEnthalpyRefrig: Refrigerant "'//TRIM(Refrigerant)//  &
         '", invalid quality, called from '//calledfrom)
    CALL ShowContinueError('Saturated refrigerant quality must be between 0 and 1, entered value=['//  &
      trim(RoundSigDigits(Quality,4))//'].')
    CALL ShowFatalError('Program terminates due to preceding condition.')
  ENDIF

  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatEnthalpyRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! Apply linear interpolation function
  ReturnValue = GetInterpolatedSatProp(Temperature, RefrigData(RefrigNum)%HTemps, RefrigData(RefrigNum)%HfValues,  &
                                           RefrigData(RefrigNum)%HfgValues, Quality, calledfrom,        &
                                           RefrigData(RefrigNum)%HfLowTempIndex,RefrigData(RefrigNum)%HfHighTempIndex)

  RETURN

END FUNCTION GetSatEnthalpyRefrig

!*****************************************************************************

FUNCTION GetSatDensityRefrig(Refrigerant,Temperature,Quality,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees (May 2002); Kenneth Tang (Jan 2004)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        ! This finds density for given temperature and a quality under the vapor dome.
        ! This function is only called with a valid refrigerant and quality between 0 and 1.

        ! METHODOLOGY EMPLOYED:
        ! Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        ! liquid  and vapour densities according to the given quality.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN)  :: Quality     ! actual quality given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER :: RefrigNum                ! index for refrigerant under consideration
  INTEGER :: HiTempIndex              ! array index for temp above input temp
  INTEGER :: LoTempIndex              ! array index for temp below input temp
  REAL(r64)    :: LoSatProp                ! Sat. prop. at lower temp & given quality
  REAL(r64)    :: HiSatProp                ! Sat. prop. at higher temp & given quality
  REAL(r64)    :: TempInterpRatio          ! ratio to interpolate in temperature domain
  LOGICAL :: ErrorFlag                ! error flag for current call

  ! error counters and dummy string
  INTEGER,SAVE :: TempRangeErrCount=0   ! cumulative error counter
  INTEGER,SAVE :: TempRangeErrIndex=0   ! cumulative error counter

  ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatDensityRefrig','properties',calledfrom)
  ENDIF

  IF ((Quality < 0.0) .OR. (Quality > 1.0)) THEN
    CALL ShowSevereError('GetSatDensityRefrig: Refrigerant "'//TRIM(Refrigerant)//  &
         '", invalid quality, called from '//TRIM(calledfrom))
    CALL ShowContinueError('Saturated density quality must be between 0 and 1, entered value=['//  &
      trim(RoundSigDigits(Quality,4))//'].')
    CALL ShowFatalError('Program terminates due to preceding condition.')
  ENDIF

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatDensityRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ErrorFlag = .False.

  LoTempIndex = FindArrayIndex(Temperature, RefrigData(RefrigNum)%RhoTemps,  &
                                  RefrigData(RefrigNum)%RhofLowTempIndex,RefrigData(RefrigNum)%RhofHighTempIndex)
  HiTempIndex = LoTempIndex + 1

  !Error check to make sure the temperature is not out of bounds
  IF (LoTempIndex == 0) THEN
    !Give the lowest density value if the temperature is below than the minimum
    !temperature in the refrigerant table
    ReturnValue = 1.0d0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofLowTempIndex) +                  &
                       Quality*(1.0d0/RefrigData(RefrigNum)%RhofgValues(RefrigData(RefrigNum)%RhofLowTempIndex) -   &
                       1.0d0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofLowTempIndex))
    ReturnValue=1.0d0/ReturnValue
    ErrorFlag = .True.
  ELSE IF(HiTempIndex > RefrigData(RefrigNum)%RhofHighTempIndex) THEN
    !Give the highest density value if the temperature is higher than the maximum
    !temperature in the refrigerant table
    ReturnValue = 1.0d0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofHighTempIndex) +        &
               Quality*(1.0d0/RefrigData(RefrigNum)%RhofgValues(RefrigData(RefrigNum)%RhofHighTempIndex) -   &
               1.0d0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofHighTempIndex))
    ReturnValue=1.0d0/ReturnValue
    ErrorFlag = .True.
  ELSE    ! Okay

    !Calculate the specific volume for the lower temperature index based on linear
    !interpolation of the quality
    LoSatProp = 1.0d0/RefrigData(RefrigNum)%RhofValues(LoTempIndex) + &
                  Quality*(1.0d0/RefrigData(RefrigNum)%RhofgValues(LoTempIndex) -   &
                    1.0d0/RefrigData(RefrigNum)%RhofValues(LoTempIndex))

    !Calculate the specific volume for the higher temperature index based on linear
    !interpolation of the quality
    HiSatProp = 1.0d0/RefrigData(RefrigNum)%RhofValues(HiTempIndex) + &
                  Quality*(1.0d0/RefrigData(RefrigNum)%RhofgValues(HiTempIndex) -   &
                    1.0d0/RefrigData(RefrigNum)%RhofValues(HiTempIndex))

    !Find interpolation ratio in temperature direction
    TempInterpRatio = (Temperature - RefrigData(RefrigNum)%RhoTemps(LoTempIndex)) / &
                      (RefrigData(RefrigNum)%RhoTemps(HiTempIndex) - RefrigData(RefrigNum)%RhoTemps(LoTempIndex))

    !Apply final linear interpolation to find the specific volume
    ReturnValue = LoSatProp + TempInterpRatio*(HiSatProp - LoSatProp)
    !Convert the specific volume to density
    ReturnValue = 1.0d0/ReturnValue
  ENDIF

  IF (.not. WarmupFlag .and. ErrorFlag) THEN
    TempRangeErrCount = TempRangeErrCount + 1
   ! send warning
    IF (TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('GetInterpolatedSatProp: Saturation temperature for interpolation is out of range '// &
                           'for supplied data: **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('..Refrigerant Temperature='//TRIM(RoundSigDigits(Temperature,2))//  &
                          ' Returned saturated property value = '//TRIM(RoundSigDigits(ReturnValue,0)))
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('GetInterpolatedSatProp: Saturation temperature out of range error',     &
                                               TempRangeErrIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
                                               ReportMinUnits='{C}',ReportMaxUnits='{C}')
    ENDIF
  END IF
  RETURN

END FUNCTION GetSatDensityRefrig

!*****************************************************************************

FUNCTION GetSatSpecificHeatRefrig(Refrigerant,Temperature,Quality,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees (May 2002)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        ! This finds specific heat for given temperature and a quality under the vapor dome.
        ! This fucntion is only called with a valid refrigerant and quality between 0 and 1.

        ! METHODOLOGY EMPLOYED:
        ! Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        ! liquid  and vapour specific heats according to the given quality.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN)  :: Quality     ! actual quality given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum    ! index for refrigerant under consideration

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatSpecificHeatRefrig','properties',calledfrom)
  ENDIF

  IF ((Quality < 0.0) .OR. (Quality > 1.0)) THEN
    CALL ShowSevereError('GetSatSpecificHeatRefrig: Refrigerant "'//TRIM(Refrigerant)//  &
         '", invalid quality, called from '//TRIM(calledfrom))
    CALL ShowContinueError('Saturated density quality must be between 0 and 1, entered value=['//  &
      trim(RoundSigDigits(Quality,4))//'].')
    CALL ShowFatalError('Program terminates due to preceding condition.')
  ENDIF

        ! Find which refrigerant (index) is being requested and then determine
        ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatSpecificHeatRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! Apply linear interpolation function
  ReturnValue = GetInterpolatedSatProp(Temperature, RefrigData(RefrigNum)%CpTemps, RefrigData(RefrigNum)%CpfValues,     &
                                                 RefrigData(RefrigNum)%CpfgValues, Quality, calledfrom, &
                                                 RefrigData(RefrigNum)%CpfLowTempIndex,RefrigData(RefrigNum)%CpfHighTempIndex)

  RETURN

END FUNCTION GetSatSpecificHeatRefrig

!*****************************************************************************

FUNCTION GetSupHeatEnthalpyRefrig(Refrigerant,Temperature,Pressure,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !       MODIFIED       Simon Rees (May 2002)
        !       RE-ENGINEERED  N/A

        ! PURPOSE OF THIS SUBROUTINE:
        ! Performs linear interpolation between pressures and temperatures and
        ! returns enthalpy values.  Works only in superheated region.

        ! METHODOLOGY EMPLOYED:
        ! Double linear interpolation is used with enthalpy values at four
        ! pressure/temperature input points surrounding the given temperature
        ! and pressure argument values.
        !
        ! With enthalpy data it is assumed that zero values in the data are in
        ! the saturated region. Hence, values near the saturation line are
        ! approximated using the saturation value instead of the zero data value.
        ! points completely in the saturation region are given the saturation value
        ! at the given temperature. Points at the upper limits of pressure/temperature
        ! have the pressure/temperature capped. Warnings are given if the point
        ! is not clearly in the bounds of the superheated data.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN) :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN) :: Pressure    ! actual pressure given as input
  INTEGER,      INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in) :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                    :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: PressInterpRatio   ! Interpolation factor w.r.t pressure
  REAL(r64) :: TempInterpRatio    ! Interpolation factor w.r.t temperature
  REAL(r64) :: EnthalpyHigh              ! Enthalpy value at interpolated pressure and high temperature
  REAL(r64) :: EnthalpyLow               ! Enthalpy value at interpolated pressure and low temperature
  REAL(r64) :: LoTempLoEnthalpy          ! Enthalpy value at low pressure and low temperature
  REAL(r64) :: LoTempHiEnthalpy          ! Enthalpy value at high pressure and low temperature
  REAL(r64) :: HiTempLoEnthalpy          ! Enthalpy value at low pressure and high temperature
  REAL(r64) :: HiTempHiEnthalpy          ! Enthalpy value at high pressure and high temperature

  INTEGER :: HiTempIndex            ! high temperature index value
  INTEGER :: HiPressIndex           ! high pressure index value
  INTEGER :: LoPressIndex           ! low index value of Pressure from table
  INTEGER :: RefrigNum              ! index for refrigerant under consideration
  INTEGER :: TempIndex              ! low index value of Temperature from table

  ! error counters and dummy string
  INTEGER :: ErrCount               ! error counter for current call
  INTEGER :: CurTempRangeErrCount   ! error counter for current call
  INTEGER :: CurPresRangeErrCount   ! error counter for current call
  INTEGER,SAVE :: TempRangeErrCount=0
  INTEGER,SAVE :: TempRangeErrIndex=0
  INTEGER,SAVE :: PresRangeErrCount=0
  INTEGER,SAVE :: PresRangeErrIndex=0
  INTEGER,SAVE :: SatErrCount=0
  INTEGER,SAVE :: SatErrIndex=0

  ! see if data is there
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSupHeatEnthalpyRefrig','properties',calledfrom)
  ENDIF

  ErrCount = 0
  CurTempRangeErrCount = 0
  CurPresRangeErrCount = 0

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature and pressure are within the temperature and
  ! pressure arrays, respectively
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSupHeatEnthalpyRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  TempIndex  = FindArrayIndex(Temperature,RefrigData(RefrigNum)%SHTemps,1,RefrigData(RefrigNum)%NumSuperTempPts)
  LoPressIndex = FindArrayIndex(Pressure,RefrigData(RefrigNum)%SHPress,1,RefrigData(RefrigNum)%NumSuperPressPts)

  ! check temperature data range and attempt to cap if necessary
  IF((TempIndex > 0) .AND. (TempIndex < RefrigData(RefrigNum)%NumSuperTempPts) )THEN ! in range
    HiTempIndex   = TempIndex + 1
    TempInterpRatio  = (Temperature - RefrigData(RefrigNum)%SHTemps(TempIndex)) / &
                              (RefrigData(RefrigNum)%SHTemps(HiTempIndex) - RefrigData(RefrigNum)%SHTemps(TempIndex))
  ELSE IF(TempIndex <1)THEN
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    TempIndex = 1
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  ELSE  ! out of range
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  END IF

  ! check pressure data range and attempt to cap if necessary
  IF((LoPressIndex > 0) .AND. (LoPressIndex < RefrigData(RefrigNum)%NumSuperPressPts) ) THEN ! in range
    HiPressIndex = LoPressIndex + 1
    PressInterpRatio = (Pressure - RefrigData(RefrigNum)%SHPress(LoPressIndex)) / &
                              (RefrigData(RefrigNum)%SHPress(HiPressIndex) - RefrigData(RefrigNum)%SHPress(LoPressIndex))
  ELSE IF(LoPressIndex < 1)THEN
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    LoPressIndex = 1
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  ELSE  ! out of range
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  END IF

  ! get interpolation point values
  LoTempLoEnthalpy = RefrigData(RefrigNum)%HshValues(TempIndex,LoPressIndex)
  LoTempHiEnthalpy = RefrigData(RefrigNum)%HshValues(TempIndex,HiPressIndex)
  HiTempLoEnthalpy = RefrigData(RefrigNum)%HshValues(HiTempIndex,LoPressIndex)
  HiTempHiEnthalpy = RefrigData(RefrigNum)%HshValues(HiTempIndex,HiPressIndex)

  ! to give reasonable interpolation near saturation reset any point with zero value
  ! in table to saturation value
  IF(LoTempLoEnthalpy <= 0.0) THEN
    LoTempLoEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF
  IF(LoTempHiEnthalpy <= 0.0) THEN
    LoTempHiEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF
  IF(HiTempLoEnthalpy <= 0.0) THEN
    HiTempLoEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF
  IF(HiTempHiEnthalpy <= 0.0) THEN
    HiTempHiEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF

  ! interpolate w.r.t. pressure
  EnthalpyLow = PressInterpRatio*LoTempHiEnthalpy + (1.0-PressInterpRatio)*LoTempLoEnthalpy

  EnthalpyHigh = PressInterpRatio*HiTempHiEnthalpy + (1.0-PressInterpRatio)*HiTempLoEnthalpy

  ! interpolate w.r.t. temperature
  ReturnValue = TempInterpRatio*EnthalpyHigh + (1.0-TempInterpRatio)*EnthalpyLow

  ! Check to see if all data is at zero. In this case we are completely
  ! inside the saturation dome. Best thing we can do is return saturation value
  IF((RefrigData(RefrigNum)%HshValues(TempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%HshValues(TempIndex,HiPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%HshValues(HiTempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%HshValues(HiTempIndex,HiPressIndex) <= 0.0) ) THEN
    SatErrCount = SatErrCount +1
    ! set return value
    ReturnValue = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0d0,   &
        RefrigNum,'GetSupHeatEnthalpyRefrig:'//trim(calledfrom))
    ! send warning
    IF (.not. WarmupFlag) THEN
      IF (SatErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowSevereError('GetSupHeatEnthalpyRefrig: Refrigerant is saturated at the given conditions: **')
        CALL ShowContinueError('saturated enthalpy at given temperature returned.    **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Refrigerant temperature = '//TRIM(RoundSigDigits(Temperature,2)))
        CALL ShowContinueError('Refrigerant pressure = '//TRIM(RoundSigDigits(Pressure,0)))
        CALL ShowContinueError('Returned Enthalpy value = '//TRIM(RoundSigDigits(ReturnValue,3)))
      ELSEIF (SatErrCount>RefrigerantErrorLimitTest) THEN
        CALL ShowRecurringSevereErrorAtEnd('GetSupHeatEnthalpyRefrig: Refrigerant saturated at the given conditions error',  &
                          SatErrIndex, ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')
      ENDIF
    ENDIF
    RETURN
  ENDIF

  IF (.not. WarmupFlag) THEN
      ! some checks...
    IF(ErrCount > 0)THEN
      ! send temp range error if flagged
      TempRangeErrCount=TempRangeErrCount + CurTempRangeErrCount
      IF (TempRangeErrCount > 1 .AND. TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatEnthalpyRefrig: Temperature is out of range for superheated refrigerant '// &
                              'enthalpy: values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (TempRangeErrCount>1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
           'GetSupHeatEnthalpyRefrig: Temperature out of range for superheated refrigerant enthalpy',TempRangeErrIndex,   &
              ReportMaxOf=Temperature,ReportMinOf=Temperature,ReportMaxUnits='{C}',ReportMinUnits='{C}')
      ENDIF

      ! send pressure range error if flagged
      PresRangeErrCount=PresRangeErrCount + CurPresRangeErrCount
      IF (PresRangeErrCount > 1 .AND. PresRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatEnthalpyRefrig: Pressure is out of range for superheated refrigerant enthalpy: '// &
                              'values capped **')
      ELSEIF (PresRangeErrCount>1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
           'GetSupHeatEnthalpyRefrig: Pressure out of range for superheated refrigerant enthalpy',PresRangeErrIndex, &
              ReportMaxOf=Pressure,ReportMinOf=Pressure,ReportMaxUnits='{Pa}',ReportMinUnits='{Pa}')
      ENDIF
    END IF ! end error checking
  ENDIF


  RETURN

END FUNCTION GetSupHeatEnthalpyRefrig

!*****************************************************************************

FUNCTION GetSupHeatPressureRefrig(Refrigerant,Temperature,Enthalpy,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Rick Strand
        !       DATE WRITTEN   May 2000
        !       MODIFIED       Simon Rees (May 2002)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        ! Performs linear interpolation between enthalpy and temperatures and
        ! returns pressure values.  Works only in superheated region.

        ! METHODOLOGY EMPLOYED:
        ! Double linear interpolation is used with pressure values at four
        ! enthalpy/temperature input points surrounding the given temperature
        ! and enthalpy argument values.
        !
        ! All enthalpies have to be calculated at the given temperature before a
        ! search is made for the data adjacent to the given enthalpy. Linear interpolation
        ! using the enthalpy data is used to interpolate the correspondng pressures.
        ! Temperatures and enthalpies outside the bounds of the available data are capped
        ! and warnings given. For enthlpys lower than the saturated vapour value at the
        ! given temperature result in the saturation pressure being returned (calls to
        ! GetSatEnthalpy and GetSatPressure are made.)

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
        ! na

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN) :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN) :: Enthalpy    ! actual enthalpy given as input
  INTEGER,      INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in) :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                    :: ReturnValue

        ! FUNCTION PARAMETERS:
  REAL(r64), PARAMETER :: EnthalpyDiff = 0.01d0    ! Allows a 1% difference in the enthalpy input and
                                            ! the enthalpy calculated from the pressure found

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: EnthalpyCheck      ! recalculates enthalpy based on calculated pressure
  REAL(r64)    :: EnthalpyHigh       ! Enthalpy value at interpolated pressure and high temperature
  REAL(r64)    :: EnthalpyLow        ! Enthalpy value at interpolated pressure and low temperature
  REAL(r64)    :: EnthalpyMax        ! Enthalpy value at interpolated pressure and high temperature
  REAL(r64)    :: EnthalpyMin        ! Enthalpy value at interpolated pressure and low temperature
  REAL(r64)    :: SatEnthalpy        ! Saturated vapour enthalpy
  REAL(r64)    :: TempInterpRatio    ! Interpolation ratio w.r.t temperature
  REAL(r64)    :: EnthInterpRatio    ! Interpolation ratio w.r.t enthalpy

  INTEGER :: finish             ! index of high end of enthalpy values
  INTEGER :: start              ! index of high end of enthalpy values
  INTEGER :: Loop               ! DO loop counter
  INTEGER :: middle             ! mid-point for interval halving

  INTEGER :: RefrigNum          ! index for refrigerant under consideration
  INTEGER :: LoTempStart        ! lower non-zero index of enthalpy values at lower temp.
  INTEGER :: LoTempFinish       ! upper non-zero index of enthalpy values at lower temp.
  INTEGER :: HiTempStart        ! lower non-zero index of enthalpy values at higher temp.
  INTEGER :: HiTempFinish       ! upper non-zero index of enthalpy values at higher temp.
  INTEGER :: TempStart          ! corrected lower non-zero index of enthalpy values
  INTEGER :: TempFinish         ! corrected upper non-zero index of enthalpy values


  INTEGER :: LoTempIndex        ! Index value of lower temperature from data
  INTEGER :: HiTempIndex        ! Index value of higher temperature from data
  INTEGER :: LoEnthalpyIndex    ! Index value of lower enthalpy from data
  INTEGER :: HiEnthalpyIndex    ! Index value of higher enthalpy from data

  ! error counters and dummy string
  INTEGER,SAVE :: TempRangeErrCount=0
  INTEGER,SAVE :: EnthalpyRangeErrCount=0
  INTEGER,SAVE :: SatErrCount=0
  INTEGER,SAVE :: TempRangeErrIndex=0
  INTEGER,SAVE :: EnthalpyRangeErrIndex=0
  INTEGER,SAVE :: SatErrIndex=0
  INTEGER :: ErrCount                  ! error counter for current call
  INTEGER :: CurTempRangeErrCount      ! error counter for current call
  INTEGER :: CurEnthalpyRangeErrCount  ! error counter for current call
  INTEGER :: CurSatErrCount            ! error counter for current call
          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSupHeatPressureRefrig','properties',calledfrom)
  ENDIF

  ErrCount = 0
  CurTempRangeErrCount = 0
  CurEnthalpyRangeErrCount = 0
  CurSatErrCount = 0

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSupHeatPressureRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  LoTempIndex = FindArrayIndex(Temperature,RefrigData(RefrigNum)%SHTemps,1,RefrigData(RefrigNum)%NumSuperTempPts)
  HiTempIndex = LoTempIndex + 1

  ! check temperature data range and attempt to cap if necessary
  IF((LoTempIndex > 0) .AND. (LoTempIndex < RefrigData(RefrigNum)%NumSuperTempPts) )THEN ! in range
    HiTempIndex  = LoTempIndex + 1
  ELSE IF (LoTempIndex<1)THEN ! below lower bound
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    LoTempIndex = 1
    HiTempIndex = LoTempIndex
  ELSE  ! out of range
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    HiTempIndex = LoTempIndex
  END IF

  ! check for lowest non-zero value in lower temp data
  LoTempStart=RefrigData(RefrigNum)%NumSuperPressPts
  DO Loop = 1, RefrigData(RefrigNum)%NumSuperPressPts
    IF (RefrigData(RefrigNum)%HshValues(LoTempIndex,Loop) > 0.0) THEN
      LoTempStart = Loop
      EXIT
    END IF
  END DO
  ! check for highest non-zero value in lower temp data
  LoTempFinish=1
  DO Loop = RefrigData(RefrigNum)%NumSuperPressPts, 1, -1
    IF (RefrigData(RefrigNum)%HshValues(LoTempIndex,Loop) <= 0.0) THEN
      LoTempFinish = Loop
      !EXIT
    END IF
  END DO
  ! check for lowest non-zero value in high temp data
  HiTempStart=RefrigData(RefrigNum)%NumSuperPressPts
  DO Loop = 1, RefrigData(RefrigNum)%NumSuperPressPts
    IF (RefrigData(RefrigNum)%HshValues(HiTempIndex,Loop) > 0.0) THEN
      HiTempStart = Loop
      EXIT
    END IF
  END DO

  ! check for highest non-zero value in high temp data
  HiTempFinish=1
  DO Loop = RefrigData(RefrigNum)%NumSuperPressPts, 1, -1
    IF (RefrigData(RefrigNum)%HshValues(HiTempIndex,Loop) <= 0.0) THEN
      HiTempFinish = Loop
    END IF
  END DO

  ! find bounds of both hi and lo temp data
  TempStart = MAX(LoTempStart, HiTempStart)
  TempFinish = MIN(LoTempFinish, HiTempFinish)
  ! calculate interpolation ratio w.r.t temperature
  ! This ratio is used to find enthalpies at the given temperature
  TempInterpRatio = (Temperature - RefrigData(RefrigNum)%SHTemps(LoTempIndex))/ &
                    (RefrigData(RefrigNum)%SHTemps(HiTempIndex) - &
                     RefrigData(RefrigNum)%SHTemps(LoTempIndex) )

  ! search for array index by bisection
  start = TempStart     ! set the bounds
  finish = TempFinish

  ! find the bounds of the enthalpy data available
  EnthalpyMax = MAX(RefrigData(RefrigNum)%HshValues(LoTempIndex,TempStart), &
                    RefrigData(RefrigNum)%HshValues(HiTempIndex,TempStart))
  EnthalpyMin = MIN(RefrigData(RefrigNum)%HshValues(LoTempIndex,TempFinish), &
                    RefrigData(RefrigNum)%HshValues(HiTempIndex,TempFinish))
  ! get saturated enthalpy for checking
  SatEnthalpy = GetSatEnthalpyRefrig(Refrigerant, Temperature, 1.0d0, &
           RefrigNum,'GetSupHeatPressureRefrig:'//trim(calledfrom))

  ! make some checks on the data before interpolating
  IF(Enthalpy < SatEnthalpy)THEN
    ! flag error
    CurSatErrCount = CurSatErrCount + 1
    ErrCount = ErrCount + 1
    ! return sat pressure at this temperature
    ReturnValue = GetSatPressureRefrig(Refrigerant, Temperature,   &
         RefrigNum,'GetSupHeatPressureRefrig:'//trim(calledfrom))

  ELSE IF (EnthalpyMax < Enthalpy .OR. EnthalpyMin > Enthalpy) THEN
    ! out of range error
    CurEnthalpyRangeErrCount = CurEnthalpyRangeErrCount +1
    ErrCount = ErrCount + 1
    IF(Enthalpy > EnthalpyMax)THEN
      ! return min pressure
      ReturnValue = RefrigData(RefrigNum)%SHPress(HiTempStart)
    ELSE
      ! return max pressure
      ReturnValue = RefrigData(RefrigNum)%SHPress(LoTempFinish)
    END IF
  ELSE
    ! go ahead and search
    DO WHILE ((finish - start) > 1)
      middle = (finish + start) / 2

      ! calc enthalpy at middle index for given temperature
      EnthalpyCheck = RefrigData(RefrigNum)%HshValues(LoTempIndex,middle) + &
                      TempInterpRatio * (RefrigData(RefrigNum)%HshValues(HiTempIndex,middle) - &
                      RefrigData(RefrigNum)%HshValues(LoTempIndex,middle) )

      IF (Enthalpy < EnthalpyCheck) THEN
        start = middle
      ELSE
        finish = middle
      END IF
    END DO
    LoEnthalpyIndex  = start
    HiEnthalpyIndex = start + 1

    ! calculate enthalpies adjacent specified enthalpy at given temperature
    EnthalpyLow = RefrigData(RefrigNum)%HshValues(LoTempIndex,LoEnthalpyIndex) + &
                  TempInterpRatio * (RefrigData(RefrigNum)%HshValues(HiTempIndex,LoEnthalpyIndex) - &
                  RefrigData(RefrigNum)%HshValues(LoTempIndex,LoEnthalpyIndex) )

    EnthalpyHigh =  RefrigData(RefrigNum)%HshValues(LoTempIndex,HiEnthalpyIndex) + &
                    TempInterpRatio * (RefrigData(RefrigNum)%HshValues(HiTempIndex,HiEnthalpyIndex) - &
                    RefrigData(RefrigNum)%HshValues(LoTempIndex,HiEnthalpyIndex) )
    ! calculate an interpolation ratio
    EnthInterpRatio = (Enthalpy - EnthalpyLow) / (EnthalpyHigh - EnthalpyLow)
    ! apply this interpolation ratio to find the final pressure
    ReturnValue = RefrigData(RefrigNum)%SHPress(LoEnthalpyIndex) + &
                               EnthInterpRatio * (RefrigData(RefrigNum)%SHPress(HiEnthalpyIndex) - &
                               RefrigData(RefrigNum)%SHPress(LoEnthalpyIndex))
  END IF

  IF (.not. WarmupFlag) THEN
    ! ** make error checks **
    IF(ErrCount > 0) THEN
      ! send near saturation warning if flagged
      SatErrCount=SatErrCount+CurSatErrCount
      IF (SatErrCount > 1 .AND. SatErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatPressureRefrig: Refrigerant is saturated at given enthalpy and temperature: '// &
                               'saturation pressure returned **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (SatErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
             'GetSupHeatPressureRefrig: Refrigerant near saturation error',SatErrIndex)
      ENDIF

      ! send temp range error if flagged
      TempRangeErrCount=TempRangeErrCount+CurTempRangeErrCount
      IF (TempRangeErrCount > 1 .AND. TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatPressureRefrig: Temperature is out of range for superheated refrigerant '// &
                               'pressure: values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (TempRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
                'GetSupHeatPressureRefrig: Temperature out of range for superheated refrigerant pressure',TempRangeErrIndex)
      ENDIF
      ! send enthalpy range error if flagged
      EnthalpyRangeErrCount=EnthalpyRangeErrCount+CurEnthalpyRangeErrCount
      IF (EnthalpyRangeErrCount > 1 .AND. EnthalpyRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatPressureRefrig: Enthlalpy is out of range for superheated refrigerant pressure: '// &
                               'values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (EnthalpyRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
                'GetSupHeatPressureRefrig: Enthlalpy out of range for superheated refrigerant pressure',EnthalpyRangeErrIndex)
      ENDIF
    END IF ! end error checking
  ENDIF

  RETURN

END FUNCTION GetSupHeatPressureRefrig

!*****************************************************************************

FUNCTION GetSupHeatDensityRefrig(Refrigerant,Temperature,Pressure,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !       MODIFIED       Simon Rees (May 2002)
        !       RE-ENGINEERED  N/A

        ! PURPOSE OF THIS SUBROUTINE:
        ! Performs linear interpolation between pressures and temperatures and
        ! returns Density values.  Works only in superheated region.

        ! METHODOLOGY EMPLOYED:
        ! Double linear interpolation is used with Density values at four
        ! pressure/temperature input points surrounding the given temperature
        ! and pressure arguments.
        !
        ! With Density data it is assumed that zero values in the data are in
        ! the saturated region. Hence, values near the saturation line are
        ! approximated using the saturation value instead of the zero data value.
        ! points completely in the saturation region are given the saturation value
        ! at the given temperature. Points at the upper limits of pressure/temperature
        ! have the pressure/temperature capped. Warnings are given if the point
        ! is not clearly in the bounds of the superheated data.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN) :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN) :: Pressure    ! actual pressure given as input
  INTEGER,      INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in) :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                    :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: TempInterpRatio     ! Interpolation ratio w.r.t temperature
  REAL(r64)    :: PressInterpRatio    ! Interpolation ratio w.r.t pressures
  REAL(r64)    :: DensityHigh         ! Density value at interpolated pressure and high temperature
  REAL(r64)    :: DensityLow          ! Density value at interpolated pressure and low temperature
  REAL(r64)    :: LoTempLoDensity     ! Density value at low pressure and low temperature
  REAL(r64)    :: LoTempHiDensity     ! Density value at high pressure and low temperature
  REAL(r64)    :: HiTempLoDensity     ! Density value at low pressure and high temperature
  REAL(r64)    :: HiTempHiDensity     ! Density value at high pressure and high temperature

  INTEGER :: HiTempIndex         ! high temperature index value
  INTEGER :: HiPressIndex        ! high pressure index value
  INTEGER :: LoPressIndex        ! low index value of Pressure from table
  INTEGER :: RefrigNum           ! index for refrigerant under consideration
  INTEGER :: TempIndex           ! low index value of Temperature from table
  ! error counters and dummy string
  INTEGER,SAVE :: TempRangeErrCount=0
  INTEGER,SAVE :: PresRangeErrCount=0
  INTEGER,SAVE :: SatErrCount=0
  INTEGER,SAVE :: TempRangeErrIndex=0
  INTEGER,SAVE :: PresRangeErrIndex=0
  INTEGER,SAVE :: SatErrIndex=0
  INTEGER :: ErrCount               ! error counter for current call
  INTEGER :: CurTempRangeErrCount   ! error counter for current call
  INTEGER :: CurPresRangeErrCount   ! error counter for current call

  ! see if data is there
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSupHeatDensityRefrig','properties',calledfrom)
  ENDIF

  ErrCount = 0  ! initialize for this call
  CurTempRangeErrCount = 0
  CurPresRangeErrCount = 0

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature and pressure are within the temperature and
  ! pressure arrays, respectively
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSupHeatDensityRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  TempIndex  = FindArrayIndex(Temperature,RefrigData(RefrigNum)%SHTemps,1,RefrigData(RefrigNum)%NumSuperTempPts)
  LoPressIndex = FindArrayIndex(Pressure,RefrigData(RefrigNum)%SHPress,1,RefrigData(RefrigNum)%NumSuperPressPts)

  ! check temperature data range and attempt to cap if necessary
  IF((TempIndex > 0) .AND. (TempIndex < RefrigData(RefrigNum)%NumSuperTempPts) )THEN ! in range
    HiTempIndex   = TempIndex + 1
    TempInterpRatio  = (Temperature - RefrigData(RefrigNum)%SHTemps(TempIndex)) &
                 /(RefrigData(RefrigNum)%SHTemps(HiTempIndex) &
                 - RefrigData(RefrigNum)%SHTemps(TempIndex))
  ELSE IF(TempIndex <1)THEN
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    TempIndex = 1
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  ELSE  ! out of range
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  END IF

  ! check pressure data range and attempt to cap if necessary
  IF((LoPressIndex > 0) .AND. (LoPressIndex < RefrigData(RefrigNum)%NumSuperPressPts) ) THEN ! in range
    HiPressIndex = LoPressIndex + 1
    PressInterpRatio = (Pressure - RefrigData(RefrigNum)%SHPress(LoPressIndex)) &
                 /(RefrigData(RefrigNum)%SHPress(HiPressIndex) &
                 - RefrigData(RefrigNum)%SHPress(LoPressIndex))
  ELSE IF(LoPressIndex<1)THEN
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    LoPressIndex=1
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  ELSE  ! out of range
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  END IF

  ! get interpolation point values
  LoTempLoDensity = RefrigData(RefrigNum)%RhoshValues(TempIndex,LoPressIndex)
  LoTempHiDensity = RefrigData(RefrigNum)%RhoshValues(TempIndex,HiPressIndex)
  HiTempLoDensity = RefrigData(RefrigNum)%RhoshValues(HiTempIndex,LoPressIndex)
  HiTempHiDensity = RefrigData(RefrigNum)%RhoshValues(HiTempIndex,HiPressIndex)

  ! to give reasonable interpolation near saturation reset any point with zero value
  ! in table to saturation value
  IF(LoTempLoDensity <= 0.0) THEN
    LoTempLoDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF
  IF(LoTempHiDensity <= 0.0) THEN
    LoTempHiDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF
  IF(HiTempLoDensity <= 0.0) THEN
    HiTempLoDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF
  IF(HiTempHiDensity <= 0.0) THEN
    HiTempHiDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF

  ! interpolate w.r.t. pressure
  DensityLow = PressInterpRatio*LoTempHiDensity + (1.0-PressInterpRatio)*LoTempLoDensity

  DensityHigh = PressInterpRatio*HiTempHiDensity + (1.0-PressInterpRatio)*HiTempLoDensity

  ! interpolate w.r.t. temperature
  ReturnValue = TempInterpRatio*DensityHigh + (1.0-TempInterpRatio)*DensityLow

  ! some checks...
  ! Check to see if all data is at zero. In this case we are completely
  ! inside the saturation dome. Best thing we can do is return saturation value
  IF((RefrigData(RefrigNum)%RhoshValues(TempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%RhoshValues(TempIndex,HiPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%RhoshValues(HiTempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%RhoshValues(HiTempIndex,HiPressIndex) <= 0.0) ) THEN
    SatErrCount = SatErrCount +1
    ! set return value
    ReturnValue = GetSatDensityRefrig(Refrigerant,Temperature, 1.0d0, RefrigNum, ' GetSupHeatDensityRefrig')
    ! send warning
    IF (SatErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('Refrigerant is saturated at the given conditions: **')
      CALL ShowContinueError('saturated density at given temperature returned.    **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('Refrigerant temperature = '//TRIM(RoundSigDigits(Temperature,2)))
      CALL ShowContinueError('Refrigerant pressure = '//TRIM(RoundSigDigits(Pressure,0)))
      CALL ShowContinueError('Returned Density value = '//TRIM(RoundSigDigits(ReturnValue,3)))
    ELSEIF (SatErrCount>1) THEN
      CALL ShowRecurringSevereErrorAtEnd('Refrigerant saturated at the given conditions error',SatErrIndex)
    ENDIF
    RETURN
  ENDIF

  IF (.not. WarmupFlag) THEN
      ! some checks...
    IF(ErrCount > 0)THEN
      ! send temp range error if flagged
      TempRangeErrCount=TempRangeErrCount+CurTempRangeErrCount
      IF (TempRangeErrCount > 1 .AND. TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatDensityRefrig: Temperature is out of range for superheated refrigerant '// &
                               'density: values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (TempRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
            'GetSupHeatDensityRefrig: Temperature out of range for superheated refrigerant density',TempRangeErrIndex,  &
               ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')
      ENDIF
      ! send pressure range error if flagged
      PresRangeErrCount=PresRangeErrCount+CurPresRangeErrCount
      IF (PresRangeErrCount > 1 .AND. PresRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatDensityRefrig: Pressure is out of range for superheated refrigerant density: '// &
                               'values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (PresRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
            'GetSupHeatDensityRefrig: Pressure out of range for superheated refrigerant density',PresRangeErrIndex,  &
               ReportMinOf=Pressure,ReportMaxOf=Pressure,ReportMinUnits='{Pa}',ReportMaxUnits='{Pa}')
      ENDIF
    END IF ! end error checking
  ENDIF

  RETURN

END FUNCTION GetSupHeatDensityRefrig

!*****************************************************************************

FUNCTION GetSpecificHeatGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds specific heats for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find specific heat values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL(r64),        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                       :: ReturnValue    ! Value for function

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .FALSE.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetSpecificHeatGlycol','specific heat',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which glycol (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetSpecificHeatGlycol','specific heat',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%CpDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%CpDataPresent,Glycol,  &
       'GetSpecificHeatGlycol','specific heat',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%CpLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CpValues(GlycolData(GlycolIndex)%CpLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%CpHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CpValues(GlycolData(GlycolIndex)%CpHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ! make sure there is a return value
    ReturnValue = GlycolData(GlycolIndex)%CpValues(GlycolData(GlycolIndex)%CpLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%CpLowTempIndex+1, GlycolData(GlycolIndex)%CpHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%CpTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%CpTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%CpTemps(Loop),    &
                                   GlycolData(GlycolIndex)%CpValues(Loop-1), &
                                   GlycolData(GlycolIndex)%CpValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetSpecificHeatGlycol: Temperature is out of range (too low) for fluid specific heat **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Fluid name ='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetSpecificHeatGlycol: Temperature out of range (too low) for fluid specific heat', &
            LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetSpecificHeatGlycol: Temperature is out of range (too high) for fluid specific heat **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Fluid name ='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetSpecificHeatGlycol: Temperature out of range (too high) for fluid specific heat', &
            HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,   &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF


  RETURN

END FUNCTION GetSpecificHeatGlycol

!*****************************************************************************

FUNCTION GetDensityGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the density for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find density values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL(r64),        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                       :: ReturnValue

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .FALSE.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetDensityGlycol','density',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which refrigerant (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetDensityGlycol','density',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%RhoDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%RhoDataPresent,Glycol,  &
       'GetDensityGlycol','density',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%RhoLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%RhoValues(GlycolData(GlycolIndex)%RhoLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%RhoHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%RhoValues(GlycolData(GlycolIndex)%RhoHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ReturnValue = GlycolData(GlycolIndex)%RhoValues(GlycolData(GlycolIndex)%RhoLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%RhoLowTempIndex+1, GlycolData(GlycolIndex)%RhoHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%RhoTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%RhoTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%RhoTemps(Loop),    &
                                   GlycolData(GlycolIndex)%RhoValues(Loop-1), &
                                   GlycolData(GlycolIndex)%RhoValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetDensityGlycol: Temperature is out of range (too low) for fluid density **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetDensityGlycol: Temperature is out of range (too low) for fluid density',  &
         LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetDensityGlycol: Temperature is out of range (too high) for fluid density **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Fluid name ='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetDensityGlycol: Temperature out of range (too high) for fluid density',   &
            HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,   &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF

  RETURN

END FUNCTION GetDensityGlycol

!*****************************************************************************

FUNCTION GetConductivityGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the conductivity for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find conductivity values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL(r64),        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                       :: ReturnValue

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .FALSE.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetConductivityGlycol','conductivity',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which refrigerant (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetConductivityGlycol','conductivity',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%CondDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%CondDataPresent,Glycol,  &
       'GetConductivityGlycol','conductivity',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%CondLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CondValues(GlycolData(GlycolIndex)%CondLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%CondHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CondValues(GlycolData(GlycolIndex)%CondHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ReturnValue = GlycolData(GlycolIndex)%CondValues(GlycolData(GlycolIndex)%CondLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%CondLowTempIndex+1, GlycolData(GlycolIndex)%CondHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%CondTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%CondTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%CondTemps(Loop),    &
                                   GlycolData(GlycolIndex)%CondValues(Loop-1), &
                                   GlycolData(GlycolIndex)%CondValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetConductivityGlycol: Temperature is out of range (too low) for glycol conductivity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetConductivityGlycol: Temperature is out of range (too low) for glycol conductivity',  &
            LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetConductivityGlycol: Temperature is out of range (too high) for glycol conductivity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetConductivityGlycol: Temperature is out of range (too high) for glycol conductivity', &
            HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,   &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF

  RETURN

END FUNCTION GetConductivityGlycol

!*****************************************************************************

FUNCTION GetViscosityGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the viscosity for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find viscosity values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL(r64),        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                       :: ReturnValue    ! Value for function

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .false.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetViscosityGlycol','viscosity',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which refrigerant (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetViscosityGlycol','viscosity',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%ViscDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%ViscDataPresent,Glycol,  &
       'GetViscosityGlycol','viscosity',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%ViscLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%ViscValues(GlycolData(GlycolIndex)%ViscLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%ViscHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%ViscValues(GlycolData(GlycolIndex)%ViscHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ReturnValue = GlycolData(GlycolIndex)%ViscValues(GlycolData(GlycolIndex)%ViscLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%ViscLowTempIndex+1, GlycolData(GlycolIndex)%ViscHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%ViscTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%ViscTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%ViscTemps(Loop),    &
                                   GlycolData(GlycolIndex)%ViscValues(Loop-1), &
                                   GlycolData(GlycolIndex)%ViscValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetViscosityGlycol: Temperature is out of range (too low) for glycol viscosity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetViscosityGlycol: Temperature is out of range (too low) for glycol viscosity',  &
         LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
         ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetViscosityGlycol: Temperature is out of range (too high) for glycol viscosity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetViscosityGlycol: Temperature is out of range (too high) for glycol viscosity',   &
         HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
         ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF


  RETURN

END FUNCTION GetViscosityGlycol

!*****************************************************************************

FUNCTION GetInterpValue(Tact,Tlo,Thi,Xlo,Xhi) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine does a simple linear interpolation.

          ! METHODOLOGY EMPLOYED:
          ! No mysteries here...just plain-old linear interpolation.

          ! REFERENCES:
          ! Any basic engineering mathematic text.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: Tact  ! actual temperature at which we want the property of interest
  REAL(r64), INTENT(IN) :: Tlo   ! temperature below Tact for which we have property data
  REAL(r64), INTENT(IN) :: Thi   ! temperature above Tact for which we have property data
  REAL(r64), INTENT(IN) :: Xlo   ! value of property at Tlo
  REAL(r64), INTENT(IN) :: Xhi   ! value of property at Thi
  REAL(r64)             :: ReturnValue

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: TempToler = 0.001d0    ! Some reasonable value for comparisons

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  IF ( ABS(Thi-Tlo) > TempToler) THEN
    ReturnValue = Xhi - ( ( (Thi-Tact)/(Thi-Tlo) ) * (Xhi-Xlo) )
  ELSE
    CALL ShowFatalError('GetInterpValue: Temperatures for fluid property data too close together, division by zero')
    ReturnValue = 0.0
  END IF

  RETURN

END FUNCTION GetInterpValue

!*****************************************************************************

FUNCTION GetQualityRefrig(Refrigerant,Temperature,Enthalpy,RefrigIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (May 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function determines the quality of a refrigerant in the saturate
          ! region based on its temperature and enthalpy

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the refrigerant name coming in can
          ! be found in the refrigerant derived type.  If so, the "reverse" of the
          ! GetSatEnthalpyRefrig function is performed.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL(r64),        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL(r64),        INTENT(IN)  :: Enthalpy    ! actual enthalpy given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: SatVapEnthalpy ! value of enthalpy at hi index value for given Quality
  REAL(r64)    :: SatLiqEnthalpy  ! value of enthalpy at TempIndex index value for given Quality
  INTEGER :: RefrigNum    ! index for refrigerant under consideration
  INTEGER :: HiTempIndex              ! array index for temp above input temp
  INTEGER :: LoTempIndex              ! array index for temp below input temp
  REAL(r64)    :: TempInterpRatio            ! ratio to interpolate in temperature domain
  INTEGER,SAVE :: TempLoRangeErrIndex=0
  INTEGER,SAVE :: TempHiRangeErrIndex=0

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,'GetQualityRefrig','enthalpy',calledfrom)
  ENDIF

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,'GetQualityRefrig','enthalpy',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  LoTempIndex = FindArrayIndex(Temperature,RefrigData(RefrigNum)%HTemps,  &
                           RefrigData(RefrigNum)%HfLowTempIndex,RefrigData(RefrigNum)%HfHighTempIndex)
  HiTempIndex = LoTempIndex + 1

  ! check on the data bounds and adjust indices to give clamped return value
  IF (LoTempIndex == 0) THEN
    SatLiqEnthalpy = RefrigData(RefrigNum)%HfValues(RefrigData(RefrigNum)%HfLowTempIndex)
    SatVapEnthalpy = RefrigData(RefrigNum)%HfgValues(RefrigData(RefrigNum)%HfLowTempIndex)
    IF (.not. WarmupFlag)   &
      ! Temperature supplied is out of bounds--produce an error message...
      CALL ShowRecurringWarningErrorAtEnd(  &
         'GetQualityRefrig: ** Temperature for requested quality is below the range of data supplied **',TempLoRangeErrIndex,  &
          ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')

  ELSE IF(HiTempIndex > RefrigData(RefrigNum)%NumHPoints) THEN
    SatLiqEnthalpy = RefrigData(RefrigNum)%HfValues(RefrigData(RefrigNum)%HfHighTempIndex)
    SatVapEnthalpy = RefrigData(RefrigNum)%HfgValues(RefrigData(RefrigNum)%HfHighTempIndex)
    IF (.not. WarmupFlag)   &
     ! Temperature supplied is out of bounds--produce an error message...
      CALL ShowRecurringWarningErrorAtEnd(  &
          'GetQualityRefrig: ** Temperature requested quality is above the range of data supplied **',TempHiRangeErrIndex,  &
          ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')

  ELSE  ! in normal range work out interpolated liq and gas enthalpies
    TempInterpRatio = (Temperature - RefrigData(RefrigNum)%HTemps(LoTempIndex)) &
            /(RefrigData(RefrigNum)%HTemps(HiTempIndex) - RefrigData(RefrigNum)%HTemps(LoTempIndex))
    SatLiqEnthalpy  = TempInterpRatio*RefrigData(RefrigNum)%HfValues(HiTempIndex) &
                  +(1.0-TempInterpRatio)*RefrigData(RefrigNum)%HfValues(LoTempIndex)
    SatVapEnthalpy = TempInterpRatio*RefrigData(RefrigNum)%HfgValues(HiTempIndex) &
                  +(1.0-TempInterpRatio)*RefrigData(RefrigNum)%HfgValues(LoTempIndex)
  END IF

  ! calculate final quality value from enthalpy ratio
  ReturnValue = (Enthalpy-SatLiqEnthalpy)/(SatVapEnthalpy - SatLiqEnthalpy)

  ! final check to bound returned quality value
  IF (ReturnValue < 0.0) THEN
!    CALL ShowRecurringWarningErrorAtEnd('GetQualityRefrig: ** '//  &
!                   'Quality is less than zero in GetQualityRefrig; Quality reset to 0.0 **')
    ReturnValue = 0.0
  ELSE IF (ReturnValue > 1.0) THEN
!    CALL ShowRecurringWarningErrorAtEnd('GetQualityRefrig: ** '//  &
!                   'Quality is greater than one in GetQualityRefrig; refrigerant is superheated **')
    ReturnValue = 2.0
  END IF

  RETURN

END FUNCTION GetQualityRefrig

!*****************************************************************************

INTEGER FUNCTION FindRefrigerant(Refrigerant)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (June 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function simply determines the index of the refrigerant named
          ! in the input variable to this routine within the derived type.

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the refrigerant name coming in can
          ! be found in the refrigerant derived type.  If so, the function is set
          ! to the index within the derived type.  If the input has not been read
          ! yet for some reason, that must be done.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found   ! Indicator for found item

          ! FLOW:
          ! Make sure we have already read in the input
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! Check to see if this glycol shows up in the glycol data
  Found=FindItemInList(MakeUPPERCase(Refrigerant),RefrigData%Name,NumOfRefrigerants)

  IF (Found > 0) THEN
    FindRefrigerant = Found
    RefrigUsed(Found)=.true.
  ELSE ! not found - errors handled in calling proceedure
    FindRefrigerant = 0
  ENDIF

  RETURN

END FUNCTION FindRefrigerant

!*****************************************************************************

INTEGER FUNCTION FindGlycol(Glycol)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (June 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function simply determines the index of the glycol named
          ! in the input variable to this routine within the derived type.

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the glycol name coming in can
          ! be found in the glycol derived type.  If so, the function is set
          ! to the index within the derived type.  If the input has not been read
          ! yet for some reason, that must be done.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList,MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Glycol ! carries in substance name

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found   ! Indicator for found item

          ! FLOW:
          ! Make sure we have already read in the input
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! Check to see if this glycol shows up in the glycol data
  Found=FindItemInList(MakeUPPERCase(Glycol),GlycolData%Name,NumOfGlycols)

  IF (Found > 0) THEN
    FindGlycol=Found
    GlycolUsed(Found)=.true.
  ELSE        ! return zero - error checking in calling proceedure
    FindGlycol = 0
  ENDIF

  RETURN

END FUNCTION FindGlycol

!*****************************************************************************

CHARACTER(Len=MaxNameLength) FUNCTION GetGlycolNameByIndex(Index)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function simply returns the glycol name by index from the
          ! GlycolData data structure.  This is needed to expose the name
          ! as the data structure is private.
          ! This is used by plant equipment to pass in both the proper index
          ! and the proper name when calling glycol routines.  Thus, the index
          ! is already known, and the input is assumed to be found.

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the glycol index is valid
          ! and if so, the function returns the name.  If not, it returns ' '

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Index ! carries in substance index

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

          ! Check to see if this glycol shows up in the glycol data
!  ArrayLength = SIZE(GlycolData)

  IF (Index <= NumOfGlycols) THEN
    GetGlycolNameByIndex = GlycolData(Index)%Name
  ELSE        ! return blank - error checking in calling proceedure
    GetGlycolNameByIndex = ' '
  ENDIF

  RETURN

END FUNCTION GetGlycolNameByIndex

!*****************************************************************************

INTEGER FUNCTION FindArrayIndex(Value,Array,LowBound,UpperBound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (May 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This generic function simply finds the points in an array between
          ! which a single value is found.  The returned value is the index of
          ! the low point.

          ! METHODOLOGY EMPLOYED:
          ! Straight interval halving. It is assumed that the values in the array
          ! appear in ascending order. If the value is below that in the supplied
          ! data array a zero index is returned. If the value is above that in the
          ! supplied data array, the max index is returned. This allows some error
          ! checking in the calling routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)               :: Value      ! Value to be placed/found within the array of values
  REAL(r64), INTENT(IN), DIMENSION(:) :: Array      ! Array of values in ascending order
  INTEGER, INTENT(IN), OPTIONAL       :: LowBound   ! Valid values lower bound (set by calling program)
  INTEGER, INTENT(IN), OPTIONAL       :: UpperBound   ! Valid values upper bound (set by calling program)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: start                   ! sets low index value
  INTEGER :: finish                  ! sets high index value
  INTEGER :: middle                  ! difference of finish & start

          ! FLOW:
  IF (PRESENT(LowBound) .and. PRESENT(UpperBound)) THEN
    start  = LowBound
    finish = UpperBound
  ELSEIF (PRESENT(LowBound)) THEN
    start = LowBound
    finish = SIZE(Array)
  ELSEIF (PRESENT(UpperBound)) THEN
    start  = 1
    finish = UpperBound
  ELSE
    start  = 1
    finish = SIZE(Array)
  ENDIF

  ! check bounds of data and set limiting values of the index
  IF(Value < Array(start)) THEN
    FindArrayIndex = 0
  ELSE IF(Value >Array(finish)) THEN
    FindArrayIndex = finish
  ELSE  ! start searching by bisection method
    DO WHILE ((finish - start) > 1)
      middle = (finish + start) / 2
      IF (Value > Array(middle)) THEN
        start = middle
      ELSE
        finish = middle
      END IF
    END DO
    FindArrayIndex = start
  END IF

  RETURN

END FUNCTION FindArrayIndex

!*****************************************************************************

FUNCTION GetInterpolatedSatProp(Temperature, PropTemps, LiqProp, VapProp, Quality, calledfrom, LowBound, UpperBound)   &
                                     RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   May 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This generic function performs an interpolation on the supplied saturated
          ! liquid and vapor data to find the saturated property value at a given
          ! temperature and quality. This function is used by all the functions that
          ! get saturated property values.

          ! METHODOLOGY EMPLOYED:
          ! Index of arrays either side of given temperature is found using FindArrayIndex.
          ! Double linear interpolation is used to first find property values at the given
          ! quality bounding the required temperature. These values are interpolated in the
          ! temperature domain to find the final value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)               :: Temperature   ! Saturation Temp.
  REAL(r64), INTENT(IN), DIMENSION(:) :: PropTemps   ! Array of temperature at which props are available
  REAL(r64), INTENT(IN), DIMENSION(:) :: LiqProp     ! Array of saturated liquid properties
  REAL(r64), INTENT(IN), DIMENSION(:) :: VapProp     ! Array of saturatedvapour properties
  REAL(r64), INTENT(IN)               :: Quality     ! Quality
  character(len=*), intent(in)        :: calledfrom  ! routine this function was called from (error messages)
  INTEGER, INTENT(IN)                 :: LowBound    ! Valid values lower bound (set by calling program)
  INTEGER, INTENT(IN)                 :: UpperBound  ! Valid values upper bound (set by calling program)
  REAL(r64)                           :: ReturnValue

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER :: HiTempIndex              ! array index for temp above input temp
  INTEGER :: LoTempIndex              ! array index for temp below input temp
  REAL(r64)    :: LoSatProp                  ! Sat. prop. at lower temp & given quality
  REAL(r64)    :: HiSatProp                ! Sat. prop. at higher temp & given quality
  REAL(r64)    :: TempInterpRatio            ! ratio to interpolate in temperature domain
 ! error counters and dummy string
  LOGICAL :: ErrorFlag                  ! error flag for current call
  INTEGER,SAVE :: TempRangeErrCount=0   ! cumulative error counter
  INTEGER,SAVE :: TempRangeErrIndex=0

  ErrorFlag = .False.

  LoTempIndex = FindArrayIndex(Temperature, PropTemps, LowBound, UpperBound)
  HiTempIndex = LoTempIndex + 1

  IF (LoTempIndex == 0) THEN
    LoTempIndex = LowBound ! MAX(1, LoTempIndex)
    ReturnValue = LiqProp(LoTempIndex) + &
               Quality*(VapProp(LoTempIndex) - LiqProp(LoTempIndex))
    ErrorFlag = .True.
  ELSE IF(HiTempIndex > UpperBound) THEN
    HiTempIndex = UpperBound
    ReturnValue = LiqProp(HiTempIndex) + &
               Quality*(VapProp(HiTempIndex) - LiqProp(HiTempIndex))
    ErrorFlag = .True.
  ELSE
    ! find adjacent property values at the given quality
    LoSatProp = LiqProp(LoTempIndex) + &
                 Quality*(VapProp(LoTempIndex) - LiqProp(LoTempIndex))

    HiSatProp = LiqProp(HiTempIndex) + &
                 Quality*(VapProp(HiTempIndex) - LiqProp(HiTempIndex))

    ! find interpolation ratio in temperature direction
    TempInterpRatio = (Temperature - PropTemps(LoTempIndex)) / &
                      (PropTemps(HiTempIndex) - PropTemps(LoTempIndex))

    ! apply final linear interpolation
    ReturnValue = LoSatProp + TempInterpRatio*(HiSatProp - LoSatProp)
  ENDIF

  IF(ErrorFlag .and. .not. calledfrom == 'ReportAndTestRefrigerants' )THEN
      TempRangeErrCount = TempRangeErrCount + 1
     ! send warning
      IF (TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowSevereError('GetInterpolatedSatProp: Saturation temperature for interpolation is out of range '// &
                             'of data supplied: **')
        CALL ShowContinueErrorTimeStamp(' Called from:'//trim(calledfrom))
        CALL ShowContinueError('Refrigerant temperature = '//TRIM(RoundSigDigits(Temperature,2)))
        CALL ShowContinueError('Returned saturated property value = '//TRIM(RoundSigDigits(ReturnValue,3)))
      ELSE
        CALL ShowRecurringSevereErrorAtEnd(  &
             'GetInterpolatedSatProp: Refrigerant temperature for interpolation out of range error',TempRangeErrIndex,  &
                ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')
      ENDIF
  END IF

  RETURN

END FUNCTION GetInterpolatedSatProp

!*****************************************************************************

INTEGER FUNCTION CheckFluidPropertyName(NameToCheck)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks on an input fluid property to make sure it is valid.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: NameToCheck  ! Name from input(?) to be checked against valid FluidPropertyNames

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Found

  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  ! Item must be either in Refrigerant or Glycol list
  Found = 0
  IF (NumOfRefrigerants > 0) THEN
    Found=FindItemInList(NameToCheck,RefrigData%Name,NumOfRefrigerants)
  ENDIF
  IF (Found == 0) THEN
    IF (NumOfGlycols > 0) THEN
      Found=FindItemInlist(NameToCheck,GlycolData%Name,NumOfGlycols)
    ENDIF
  ENDIF

  CheckFluidPropertyName=Found

  RETURN

END FUNCTION CheckFluidPropertyName

SUBROUTINE ReportOrphanFluids

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! In response to CR8008, report orphan (unused) fluid items.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: DisplayUnusedObjects
  USE General, ONLY: RoundSigDigits
  USE InputProcessor, ONLY: SameString

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
  LOGICAL :: NeedOrphanMessage
  INTEGER :: Item
  INTEGER :: NumUnusedRefrig
  INTEGER :: NumUnusedGlycol

  NeedOrphanMessage=.true.
  NumUnusedRefrig=0

  DO Item=1,NumOfRefrigerants
    IF (RefrigUsed(Item)) CYCLE
    IF (SameString(RefrigData(Item)%Name,Steam)) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedObjects) THEN
      CALL ShowWarningError('The following fluid names are "Unused Fluids".  These fluids are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedObjects) THEN
      CALL ShowMessage('Refrigerant='//TRIM(RefrigData(Item)%Name))
    ELSE
      NumUnusedRefrig=NumUnusedRefrig+1
    ENDIF
  ENDDO

  NumUnusedGlycol=0

  DO Item=1,NumOfGlycols
    IF (GlycolUsed(Item)) CYCLE
    IF (SameString(GlycolData(Item)%Name,Water)) CYCLE
    IF (SameString(GlycolData(Item)%Name,EthyleneGlycol)) CYCLE
    IF (SameString(GlycolData(Item)%Name,PropyleneGlycol)) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedObjects) THEN
      CALL ShowWarningError('The following fluid names are "Unused Fluids".  These fluids are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedObjects) THEN
      CALL ShowMessage('Glycol='//TRIM(GlycolData(Item)%Name))
    ELSE
      NumUnusedGlycol=NumUnusedGlycol+1
    ENDIF
  ENDDO

  IF (NumUnusedRefrig > 0 .or. NumUnusedGlycol > 0) THEN
    IF (NumUnusedRefrig > 0)  &
       CALL ShowMessage('There are '//trim(RoundSigDigits(NumUnusedRefrig))//' unused refrigerants in input.')
    IF (NumUnusedGlycol > 0)  &
       CALL ShowMessage('There are '//trim(RoundSigDigits(NumUnusedGlycol))//' unused glycols in input.')
    CALL ShowMessage('Use Output:Diagnostics,DisplayUnusedObjects; to see them.')
  ENDIF

  RETURN

END SUBROUTINE ReportOrphanFluids

SUBROUTINE ReportFatalGlycolErrors(NumGlycols,GlycolNum,DataPresent,GlycolName,RoutineName,Property,calledfrom)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Consolidate fatal error reporting for glycols.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: NumGlycols  ! Number of Glycols in input/data
  INTEGER :: GlycolNum   ! Glycol Index
  LOGICAL :: DataPresent ! data is present for this fluid.
  CHARACTER(len=*) :: GlycolName  ! Name being reported
  CHARACTER(len=*) :: RoutineName ! Routine name to show
  CHARACTER(len=*) :: Property    ! Property being requested
  CHARACTER(len=*) :: calledfrom  ! original called from (external to fluid properties)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNo

  ! check and see if it might be a refrigerant
  RefrigNo=FindRefrigerant(GlycolName)

  IF (NumGlycols == 0) THEN
    CALL ShowSevereError(RoutineName//': no glycols found -- cannot evaluate fluid '//Property//  &
      ' for "'//trim(GlycolName)//'", called from: '//calledfrom)
  ELSEIF (GlycolNum == 0) THEN
    CALL ShowSevereError(RoutineName//': data not found in input for requested glycol "'//   &
      trim(GlycolName)//'", called from: '//calledfrom)
  ELSEIF (.not. DataPresent) THEN
    CALL ShowSevereError(RoutineName//': '//Property//' data not found in input for requested glycol "'//   &
      trim(GlycolName)//'", called from: '//calledfrom)
  ENDIF
  IF (RefrigNo > 0)   &
      CALL ShowContinueError('Note: that fluid is listed as a Refrigerant from input.')

  CALL ShowFatalError('Program terminates due to preceding condition.')


  RETURN

END SUBROUTINE ReportFatalGlycolErrors

SUBROUTINE ReportFatalRefrigerantErrors(NumRefrigerants,RefrigerantNum,DataPresent,RefrigerantName,RoutineName,Property,calledfrom)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Consolidate fatal error reporting for refrigerants.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: NumRefrigerants  ! Number of Refrigerants in input/data
  INTEGER :: RefrigerantNum   ! Refrigerant Index
  LOGICAL :: DataPresent ! data is present for this fluid.
  CHARACTER(len=*) :: RefrigerantName  ! Name being reported
  CHARACTER(len=*) :: RoutineName ! Routine name to show
  CHARACTER(len=*) :: Property    ! Property being requested
  CHARACTER(len=*) :: calledfrom  ! original called from (external to fluid properties)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: GlycolNo

  ! check and see if it might be a refrigerant
  GlycolNo=FindGlycol(RefrigerantName)

  IF (NumRefrigerants == 0) THEN
    CALL ShowSevereError(RoutineName//': no refrigerants found -- cannot evaluate fluid '//Property//  &
      ' for "'//trim(RefrigerantName)//'", called from: '//calledfrom)
  ELSEIF (RefrigerantNum == 0) THEN
    CALL ShowSevereError(RoutineName//': data not found in input for requested refrigerant "'//   &
      trim(RefrigerantName)//'", called from: '//calledfrom)
  ELSEIF (.not. DataPresent) THEN
    CALL ShowSevereError(RoutineName//': '//Property//' data not found in input for requested refrigerant "'//   &
      trim(RefrigerantName)//'", called from: '//calledfrom)
  ENDIF
  IF (GlycolNo > 0)   &
      CALL ShowContinueError('Note: that fluid is listed as a Glycol from input.')

  CALL ShowFatalError('Program terminates due to preceding condition.')


  RETURN

END SUBROUTINE ReportFatalRefrigerantErrors


!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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

END MODULE FluidProperties

