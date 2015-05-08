MODULE DataConvergParams      ! EnergyPlus Data-Only Module

          ! PURPOSE OF THIS MODULE:
          ! This data-only module sets the parameters that control the convergence
          ! of the HVAC simulation.

USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:

          ! Note: Unless otherwise noted, the tolerance parameters listed below were chosen
          ! to represent educated guesses at what the tolerances for individual physical
          ! parameters should be.
  REAL(r64), PARAMETER :: HVACEnthalpyToler    = 260.d0     ! Tolerance for enthalpy comparisons (in kJ/kgK)
  REAL(r64), PARAMETER :: HVACFlowRateToler    = 0.01d0     ! Tolerance for mass flow rate convergence (in kg/s) [~20 CFM]
  REAL(r64), PARAMETER :: HVACHumRatToler      = 0.0001d0   ! Tolerance for humidity ratio comparisons (in kg water/kg air)
  REAL(r64), PARAMETER :: HVACQualityToler     = 0.01d0     ! Tolerance for fluid quality comparisons (dimensionless)
  REAL(r64), PARAMETER :: HVACPressToler       = 10.0d0     ! Tolerance for pressure comparisons (in Pascals)
  REAL(r64), PARAMETER :: HVACTemperatureToler = 0.01d0     ! Tolerance for temperature comparisons (in degrees C or K)
  REAL(r64), PARAMETER :: HVACEnergyToler      = 10.0d0     ! Tolerance for Energy comparisons (in Watts W)

  REAL(r64), PARAMETER :: HVACCpApprox         = 1004.844d0 ! Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
                                                     ! Only used to scale the answer for a more intuitive answer for comparison

  REAL(r64), PARAMETER :: PlantEnthalpyToler    = 0.10d0     ! Tolerance for enthalpy comparisons (in kJ/kgK)
  REAL(r64), PARAMETER :: PlantFlowRateToler    = 0.001d0    ! Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
  REAL(r64), PARAMETER :: PlantHumRatToler      = 0.0001d0   ! Tolerance for humidity ratio comparisons (in kg water/kg air)
  REAL(r64), PARAMETER :: PlantQualityToler     = 0.01d0     ! Tolerance for fluid quality comparisons (dimensionless)
  REAL(r64), PARAMETER :: PlantPressToler       = 10.0d0     ! Tolerance for pressure comparisons (in Pascals)
  REAL(r64), PARAMETER :: PlantTemperatureToler = 0.01d0     ! Tolerance for temperature comparisons (in degrees C or K)
  REAL(r64), PARAMETER :: PlantEnergyToler      = 10.0d0     ! Tolerance for Energy comparisons (in Watts W)

  REAL(r64), PARAMETER :: PlantCpApprox         = 4180.0d0   ! Approximate Cp used in Interface manager for
                                                             ! Energy Tolerance Calculation, used to scale the answer
                                                             ! for a more intuitive answer for comparison
  REAL(r64), PARAMETER :: PlantFlowFlowRateToler= 0.01d0     ! Tolerance for mass flow rate convergence (in kg/s)

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: HVACEnthTolFlag = 0         ! Flag to show enthalpy convergence (0) or failure (1)
  INTEGER :: HVACFlowTolFlag = 0         ! Flag to show mass flow convergence (0) or failure (1)
  INTEGER :: HVACHumTolFlag = 0          ! Flag to show humidity ratio convergence (0) or failure (1)
  INTEGER :: HVACPressTolFlag = 0        ! Flag to show pressure convergence (0) or failure (1)
  INTEGER :: HVACQualTolFlag = 0         ! Flag to show fluid quality convergence (0) or failure (1)
  INTEGER :: HVACEnergyTolFlag = 0       ! Flag to show energy convergence (0) or failure (1)
  INTEGER :: HVACTempTolFlag = 0         ! Flag to show temperature convergence (0) or failure (1)
  REAL(r64)    :: HVACEnthTolValue(5)  =0.0   ! Queue of convergence "results"
  REAL(r64)    :: HVACFlowTolValue(5)  =0.0   ! Queue of convergence "results"
  REAL(r64)    :: HVACHumTolValue(5)   =0.0   ! Queue of convergence "results"
  REAL(r64)    :: HVACPressTolValue(5) =0.0   ! Queue of convergence "results"
  REAL(r64)    :: HVACQualTolValue(5)  =0.0   ! Queue of convergence "results"
  REAL(r64)    :: HVACEnergyTolValue(5)=0.0   ! Queue of convergence "results"
  REAL(r64)    :: HVACTempTolValue(5)  =0.0   ! Queue of convergence "results"
  INTEGER, DIMENSION(5) :: HVACOutletNodeTolOut = 0 ! Queue of outlet node index to identify with tol values
  INTEGER :: HVACQuePtr           =0     ! Pointer to "head" of queue

  INTEGER :: PlantEnthTolFlag = 0        ! Flag to show enthalpy convergence (0) or failure (1)
  INTEGER :: PlantFlowTolFlag = 0        ! Flag to show mass flow convergence (0) or failure (1)
  INTEGER :: PlantHumTolFlag = 0         ! Flag to show humidity ratio convergence (0) or failure (1)
  INTEGER :: PlantPressTolFlag = 0       ! Flag to show pressure convergence (0) or failure (1)
  INTEGER :: PlantQualTolFlag = 0        ! Flag to show fluid quality convergence (0) or failure (1)
  INTEGER :: PlantEnergyTolFlag = 0      ! Flag to show energy convergence (0) or failure (1)
  INTEGER :: PlantTempTolFlag = 0        ! Flag to show temperature convergence (0) or failure (1)
  REAL(r64)    :: PlantEnthTolValue(5)  =0.0  ! Queue of convergence "results"
  REAL(r64)    :: PlantFlowTolValue(5)  =0.0  ! Queue of convergence "results"
  REAL(r64)    :: PlantHumTolValue(5)   =0.0  ! Queue of convergence "results"
  REAL(r64)    :: PlantPressTolValue(5) =0.0  ! Queue of convergence "results"
  REAL(r64)    :: PlantQualTolValue(5)  =0.0  ! Queue of convergence "results"
  REAL(r64)    :: PlantEnergyTolValue(5)=0.0  ! Queue of convergence "results"
  REAL(r64)    :: PlantTempTolValue(5)  =0.0  ! Queue of convergence "results"
  INTEGER, DIMENSION(5) :: PlantOutletNodeTolOut = 0 ! Queue of outlet node index to identify with tol values
  INTEGER :: PlantQuePtr           =0

!  INTEGER :: PlantFlowEnthTolFlag = 0    ! Flag to show enthalpy convergence (0) or failure (1)
  INTEGER :: PlantFlowFlowTolFlag = 0    ! Flag to show mass flow convergence (0) or failure (1)
!  INTEGER :: PlantFlowHumTolFlag = 0     ! Flag to show humidity ratio convergence (0) or failure (1)
!  INTEGER :: PlantFlowPressTolFlag = 0   ! Flag to show pressure convergence (0) or failure (1)
!  INTEGER :: PlantFlowQualTolFlag = 0    ! Flag to show fluid quality convergence (0) or failure (1)
!  INTEGER :: PlantFlowEnergyTolFlag = 0  ! Flag to show energy convergence (0) or failure (1)
!  INTEGER :: PlantFlowTempTolFlag = 0    ! Flag to show temperature convergence (0) or failure (1)
  REAL(r64)    :: PlantFlowFlowTolValue(5) = 0.0  ! Queue of convergence "results"
  INTEGER :: PlantFlowQuePtr          = 0

  INTEGER :: AirLoopConvergFail = 0

  REAL(r64) :: MinTimeStepSys      =(1.d0/60.d0)  ! =1 minute
  REAL(r64) :: MinTimeStepTol      =1.0d-4    != min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
  REAL(r64) :: MaxZoneTempDiff     = 0.3d0      !0.3 C = (1% OF 300 C) = max allowable difference between
                                                                   !   zone air temp at Time=T and Time=T-1
  REAL(r64) :: MinSysTimeRemaining =(1.0d0/3600.d0) != 1 second
  INTEGER          :: MaxIter             =20          !maximum number of iterations allowed

  INTEGER   :: MaxPlantSubIterations  = 8  ! Iteration Max for Plant Simulation sub iterations
  INTEGER   :: MinPlantSubIterations  = 2  ! Iteration Min for Plant Simulation sub iterations

!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
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

END MODULE DataConvergParams
