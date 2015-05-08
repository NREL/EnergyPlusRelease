MODULE DataBranchAirLoopPlant

          ! Module containing the routines dealing with the <module_name>

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Certain data needs to be shared from Branch to Airloop to Plant and this module should
          ! alleviate cyclic dependencies.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Data module -- everything public

          ! MODULE PARAMETER DEFINITIONS:
! Parameters for tolerance
REAL(r64), PARAMETER :: MassFlowTolerance  = 0.000000001d0  ! minimum significant mass flow rate (kg/s)

! Pressure Curve Type: None, pressure, or generic curve (if generic it will be a postive value which is the curve manager index)
INTEGER,          PARAMETER    :: PressureCurve_Error       = -1
INTEGER,          PARAMETER    :: PressureCurve_None        = 0
INTEGER,          PARAMETER    :: PressureCurve_Pressure    = 1
INTEGER,          PARAMETER    :: PressureCurve_Generic     = 2

! Parameters for flow Control Types for branch flow resolution inside splitter/mixers
INTEGER, PARAMETER,PUBLIC :: ControlType_Unknown      = 0
INTEGER, PARAMETER,PUBLIC :: ControlType_Active       = 1      ! 'Active'
INTEGER, PARAMETER,PUBLIC :: ControlType_Passive      = 2      ! 'Passive'
INTEGER, PARAMETER,PUBLIC :: ControlType_SeriesActive = 3      ! 'SeriesActive'
INTEGER, PARAMETER,PUBLIC :: ControlType_Bypass       = 4      ! 'Bypass
CHARACTER(len=*), PARAMETER, DIMENSION(0:4), PUBLIC :: cControlType=  &
                                    (/'Unknown     ',  &
                                      'Active      ',  &
                                      'Passive     ',  &
                                      'SeriesActive',  &
                                      'Bypass      '/)

          ! DERIVED TYPE DEFINITIONS:
TYPE PlantPressureCurveData
  CHARACTER(len=MaxNameLength) :: Name                    = ' '
  REAL(r64)                    :: EquivDiameter           = 0.0d0   !- An effective diameter for calculation of Re & e/D [m]
  REAL(r64)                    :: MinorLossCoeff          = 0.0d0   !- K factor                                          [-]
  REAL(r64)                    :: EquivLength             = 0.0d0   !- An effective length to apply friction calculation [m]
  REAL(r64)                    :: EquivRoughness          = 0.0d0   !- An effective roughness (e) to calculate e/D       [m]
  LOGICAL                      :: ConstantFpresent        = .FALSE. !- Signal for if a constant value of f was entered
  REAL(r64)                    :: ConstantF               = 0.0d0   !- Constant value of f (if applicable)               [-]
  LOGICAL                      :: EMSOverrideOn = .FALSE. ! if TRUE, then EMS is calling to override curve value
  REAL(r64)                    :: EMSOverrideCurveValue = 0.0D0 ! Value of curve result EMS is directing to use
  !  report variables.
  REAL(r64)                    :: CurveOutput             = 0.0d0
  REAL(r64)                    :: CurveInput1             = 0.0d0   !- MassFlow                                         [kg/s]
  REAL(r64)                    :: CurveInput2             = 0.0d0   !- Density                                          [kg/m3]
  REAL(r64)                    :: CurveInput3             = 0.0d0   !- Velocity                                         [m/s]
END TYPE PlantPressureCurveData

          ! MODULE VARIABLE DECLARATIONS:
TYPE(PlantPressureCurveData), ALLOCATABLE, DIMENSION(:) :: PressureCurve
INTEGER :: NumPressureCurves=0

          ! SUBROUTINE SPECIFICATIONS FOR MODULE

!=================================================================================================!

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

END MODULE DataBranchAirLoopPlant

