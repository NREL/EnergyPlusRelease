MODULE DataComplexFenestration
          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contains data necessary for complex fenestration calculations

          ! USE STATEMENTS:
USE DataGlobals, ONLY: MaxNameLength
USE DataPrecisionGlobals

IMPLICIT NONE

PUBLIC

! Parameters for complex shade
INTEGER, PARAMETER :: csVenetian          = 1
INTEGER, PARAMETER :: csWoven              = 2
INTEGER, PARAMETER :: csPerforated        = 3
INTEGER, PARAMETER :: csOtherShadingType  = 4
INTEGER, PARAMETER :: csBSDF              = 5

! Parameters for gas definitions
INTEGER, PARAMETER :: GasCoeffsCustom = 0
INTEGER, PARAMETER :: GasCoeffsAir = 1
INTEGER, PARAMETER :: GasCoeffsArgon = 2
INTEGER, PARAMETER :: GasCoeffsKrypton = 3
INTEGER, PARAMETER :: GasCoeffsXenon = 4

! Parameters for Thermal Algorithm
!INTEGER, PARAMETER :: taTarcog = 0
!INTEGER, PARAMETER :: taWinkelmann = 1

! Parameters for calculation standard
INTEGER, PARAMETER :: csISO15099      = 1
INTEGER, PARAMETER :: csEN673Declared  = 2
INTEGER, PARAMETER :: csEN673Design    = 3

! Parameters for thermal model
INTEGER, PARAMETER :: tmISO15099 = 0
INTEGER, PARAMETER :: tmScaledCavityWidth = 1
INTEGER, PARAMETER :: tmConvectiveScalarModel_NoSDThickness = 2
INTEGER, PARAMETER :: tmConvectiveScalarmodel_WithSDThickness = 3

! Parameters for deflection model
INTEGER, PARAMETER :: dmNoDeflection = 0
INTEGER, PARAMETER :: dmTemperatureAndPressureInput = 1
INTEGER, PARAMETER :: dmMeasuredDeflection = 2

TYPE GapSupportPillar
  CHARACTER(len=MaxNameLength) :: Name = ' ' ! Name of support pillar
  REAL(r64) :: Spacing = 0.0D0  !Spacing between centers of support pillars (m)
  REAL(r64) :: Radius = 0.0D0    !Support pillar radius (m)
END TYPE GapSupportPillar

TYPE GapDeflectionState
  CHARACTER(len=MaxNameLength) :: Name = ' ' ! Name of deflection state
  REAL(r64) :: DeflectedThickness = 0.0D0
END TYPE GapDeflectionState

TYPE WindowComplexShade
  CHARACTER(len=MaxNameLength) :: Name = ' '  ! Name for complex shade
  INTEGER    :: LayerType                = -1         !Layer type (OtherShadingType, Venetian, Woven, Perforated)
  REAL(r64) :: Thickness                 = 0.0d0      ! Layer thickness (m)
  REAL(r64) :: Conductivity              = 0.0d0      ! Layer conductivity (W/m2K)
  REAL(r64) :: IRTransmittance           = 0.0d0      ! IR Transmittance
  REAL(r64) :: FrontEmissivity           = 0.0d0      ! Emissivity of front suraface
  REAL(r64) :: BackEmissivity            = 0.0d0      ! Emissivity of back surface
  REAL(r64) :: TopOpeningMultiplier      = 0.0d0      ! Coverage percent for top opening (%)
  REAL(r64) :: BottomOpeningMultiplier   = 0.0d0      ! Coverage percent for bottom opening (%)
  REAL(r64) :: LeftOpeningMultiplier     = 0.0d0      ! Coverage percent for left opening (%)
  REAL(r64) :: RightOpeningMultiplier    = 0.0d0      ! Coverage percent for right opening (%)
  REAL(r64) :: FrontOpeningMultiplier    = 0.0d0      ! Coverage percent for front opening (%)
  REAL(r64) :: SlatWidth                 = 0.0d0      ! Slat width (m)
  REAL(r64) :: SlatSpacing               = 0.0d0      ! Slat spacing (m)
  REAL(r64) :: SlatThickness             = 0.0d0      ! Slat thickness (m)
  REAL(r64) :: SlatAngle                 = 0.0d0      ! Slat angle (deg)
  REAL(r64) :: SlatConductivity          = 0.0d0      ! Slat conductivity (W/m2K)
  REAL(r64) :: SlatCurve                 = 0.0d0      ! Curvature radius of slat (if =0 then flat) (m)
END Type WindowComplexShade

TYPE WindowThermalModelParams
  CHARACTER(len=MaxNameLength) :: Name = ' '  ! Window thermal model name
  INTEGER :: CalculationStandard = -1          ! Tarcog calculation standard
  INTEGER :: ThermalModel = -1                ! Tarcog thermal model
  REAL(r64) :: SDScalar = 0.0d0                ! SDScalar coefficient
  INTEGER :: DeflectionModel = -1              ! Deflection model
  REAL(r64) :: VacuumPressureLimit = 0.0d0    ! Pressure limit at which it will be considered vacuum gas state
  REAL(r64) :: InitialTemperature = 0.0d0      ! Window(s) temperature in time of fabrication
  REAL(r64) :: InitialPressure = 0.0d0        ! Window(s) pressure in time of fabrication
END TYPE WindowThermalModelParams

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

END MODULE DataComplexFenestration