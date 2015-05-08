MODULE DataConvergParams      ! EnergyPlus Data-Only Module

          ! PURPOSE OF THIS MODULE:
          ! This data-only module sets the parameters that control the convergence
          ! of the HVAC simulation.

USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

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
  REAL(r64), PARAMETER :: HVACFlowRateSlopeToler = 0.001d0  ! Slope tolerance for mass flow, kg/s/iteration
  REAL(r64), PARAMETER :: HVACFlowRateOscillationToler = 0.0000001d0 !tolerance for detecting duplicate flow rate in stack
  REAL(r64), PARAMETER :: HVACHumRatToler      = 0.0001d0   ! Tolerance for humidity ratio comparisons (kg water/kg dryair)
  REAL(r64), PARAMETER :: HVACHumRatSlopeToler = 0.00001d0  ! Slope tolerance for humidity ratio, kg water/kg-dryair/iteration
  REAL(r64), PARAMETER :: HVACHumRatOscillationToler = 0.00000001d0 ! tolerance for detecting duplicate humidity ratio in stack
  REAL(r64), PARAMETER :: HVACQualityToler     = 0.01d0     ! Tolerance for fluid quality comparisons (dimensionless)
  REAL(r64), PARAMETER :: HVACPressToler       = 10.0d0     ! Tolerance for pressure comparisons (in Pascals)
  REAL(r64), PARAMETER :: HVACTemperatureToler = 0.01d0     ! Tolerance for temperature comparisons (in degrees C or K)
  REAL(r64), PARAMETER :: HVACTemperatureSlopeToler = 0.001d0 ! Slope tolerance for Temperature, Deg C/iteration
  REAL(r64), PARAMETER :: HVACTemperatureOscillationToler = 0.000001d0 !tolerance for detecting duplicate temps in stack
  REAL(r64), PARAMETER :: HVACEnergyToler      = 10.0d0     ! Tolerance for Energy comparisons (in Watts W)
                                                            ! to be consistent, should be 20.d0 (BG Aug 2012)

  REAL(r64), PARAMETER :: HVACCpApprox         = 1004.844d0 ! Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
                                                     ! Only used to scale the answer for a more intuitive answer for comparison

  REAL(r64), PARAMETER :: PlantEnthalpyToler    = 0.10d0     ! Tolerance for enthalpy comparisons (in kJ/kgK)
  REAL(r64), PARAMETER :: PlantFlowRateToler    = 0.001d0    ! Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
  REAL(r64), PARAMETER :: PlantFlowRateOscillationToler = 0.0000001d0
  REAL(r64), PARAMETER :: PlantFlowRateSlopeToler = 0.0001d0  ! Slope tolerance for mass flow, kg/s/iteration

  REAL(r64), PARAMETER :: PlantPressToler       = 10.0d0     ! Tolerance for pressure comparisons (in Pascals)
  REAL(r64), PARAMETER :: PlantTemperatureToler = 0.01d0     ! Tolerance for temperature comparisons (in degrees C or K)
  REAL(r64), PARAMETER :: PlantTemperatureSlopeToler = 0.001d0 ! Slope tolerance for Temperature, Deg C/iteration
  REAL(r64), PARAMETER :: PlantTemperatureOscillationToler = 0.000001d0 !tolerance for detecting duplicate temps in stack

  REAL(r64), PARAMETER :: PlantEnergyToler      = 10.0d0     ! Tolerance for Energy comparisons (in Watts W)

  REAL(r64), PARAMETER :: PlantCpApprox         = 4180.0d0   ! Approximate Cp used in Interface manager for
                                                             ! Energy Tolerance Calculation, used to scale the answer
                                                             ! for a more intuitive answer for comparison
  REAL(r64), PARAMETER :: PlantFlowFlowRateToler= 0.01d0     ! Tolerance for mass flow rate convergence (in kg/s)


  INTEGER, PARAMETER   :: ConvergLogStackDepth   = 10
  REAL(r64), PARAMETER, DIMENSION(ConvergLogStackDepth) :: ConvergLogStackARR = &
               (/0.d0, -1.d0, -2.d0, -3.d0, - 4.d0, -5.d0, -6.d0, -7.d0, -8.d0, -9.d0 /)


  INTEGER, PARAMETER   :: CalledFromAirSystemDemandSide = 100
  INTEGER, PARAMETER   :: CalledFromAirSystemSupplySideDeck1 = 101
  INTEGER, PARAMETER   :: CalledFromAirSystemSupplySideDeck2 = 102
          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  TYPE HVACNodeConvergLogStruct
    INTEGER :: NodeNum
    LOGICAL :: NotConvergedHumRate
    LOGICAL :: NotConvergedMassFlow
    LOGICAL :: NotConvergedTemp
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HumidityRatio
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: MassFlowRate
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: Temperature
  END TYPE HVACNodeConvergLogStruct

  TYPE HVACZoneInletConvergenceStruct
    CHARACTER(len=MaxNameLength) :: ZoneName
    INTEGER  :: NumInletNodes = 0 ! number of inlet nodes for zone
    TYPE(HVACNodeConvergLogStruct), DIMENSION(:), ALLOCATABLE :: InletNode
  END TYPE HVACZoneInletConvergenceStruct

  TYPE (HVACZoneInletConvergenceStruct), DIMENSION(:), ALLOCATABLE :: ZoneInletConvergence

  TYPE HVACAirLoopIterationConvergenceStruct
    LOGICAL :: HVACMassFlowNotConverged = .FALSE.         ! Flag to show mass flow convergence
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACFlowDemandToSupplyTolValue   = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACFlowSupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACFlowSupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

    LOGICAL :: HVACHumRatNotConverged = .FALSE.          ! Flag to show humidity ratio convergence   or failure
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACHumDemandToSupplyTolValue   = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACHumSupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACHumSupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

    LOGICAL :: HVACTempNotConverged = .FALSE.         ! Flag to show temperature convergence  or failure
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACTempDemandToSupplyTolValue   = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACTempSupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACTempSupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

    LOGICAL :: HVACEnergyNotConverged = .FALSE.         ! Flag to show energy convergence   or failure
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACEnergyDemandToSupplyTolValue   = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACEnergySupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACEnergySupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

    LOGICAL :: HVACEnthalpyNotConverged = .FALSE.         ! Flag to show energy convergence   or failure
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACEnthalpyDemandToSupplyTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACEnthalpySupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACEnthalpySupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

    LOGICAL :: HVACPressureNotConverged = .FALSE.         ! Flag to show energy convergence   or failure
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACPressureDemandToSupplyTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACPressureSupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACPressueSupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

    LOGICAL :: HVACQualityNotConverged = .FALSE.         ! Flag to show energy convergence   or failure
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACQualityDemandToSupplyTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACQualitSupplyDeck1ToDemandTolValue = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: HVACQualitySupplyDeck2ToDemandTolValue = 0.d0   ! Queue of convergence "results"

  END TYPE HVACAirLoopIterationConvergenceStruct

  TYPE(HVACAirLoopIterationConvergenceStruct), DIMENSION(:), ALLOCATABLE :: AirLoopConvergence

  TYPE PlantIterationConvergenceStruct

    LOGICAL :: PlantMassFlowNotConverged = .FALSE.         ! Flag to show mass flow convergence
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: PlantFlowDemandToSupplyTolValue   = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: PlantFlowSupplyToDemandTolValue   = 0.d0   ! Queue of convergence "results"

    LOGICAL :: PlantTempNotConverged = .FALSE.         ! Flag to show temperature convergence (0) or failure (1)
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: PlantTempDemandToSupplyTolValue   = 0.d0   ! Queue of convergence "results"
    REAL(r64), DIMENSION(ConvergLogStackDepth) :: PlantTempSupplyToDemandTolValue   = 0.d0   ! Queue of convergence "results"

  END TYPE PlantIterationConvergenceStruct

  TYPE(PlantIterationConvergenceStruct) , DIMENSION(:), ALLOCATABLE :: PlantConvergence

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

END MODULE DataConvergParams
