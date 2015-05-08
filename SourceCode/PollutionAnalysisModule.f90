Module PollutionModule
          ! Module containing the pollution calculation routines

          ! MODULE INFORMATION:
          !       AUTHOR         Richard J. Liesen (RJL)
          !       DATE WRITTEN   August 2002
          !       MODIFIED       January 17, 2004 - J Glazer - Added source energy support including schedules for source energy
          !                      January 2008 - L Lawrie - implementing schedule fields for all emission factors.
          !       RE-ENGINEERED  December 2003 RJL

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms required to
          ! calculate the pollution, and carbon eqiuvalent for the Energy consumed

          ! METHODOLOGY EMPLOYED:
          ! The methodology employed is to calculate the
          ! source pollution from building energy consumption.
          !    PURPOSE:= Takes the Energy from the various sources and
          !               calculates the Environmental Impact Factors.
          !         STEP 1:  We begin with the output expressing the energy
          !
          !         STEP 2:  The energy used by types: must be converted back
          !         to source fuel types (fossil or electricity) via User Input.
          !
          !         STEP 3:  All energy numbers have been converted to units of MJ's or 1x10^6 Joules.
          !
          !         STEP 4:  Environmental Impact Factors are calculated from Coefficients

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: MaxNameLength,HourOfDay,TimeStep,TimeStepZone,  &
                           OutputFileMeters,StdMeterRecordCount,EndHourFlag,EndDayFlag,EndEnvrnFlag
    USE DataInterfaces, ONLY: SetupOutputVariable
    USE DataEnvironment, ONLY: Month,DayOfMonth,EndMonthFlag

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: ElecPollFactor=1
INTEGER, PARAMETER :: NatGasPollFactor=2
INTEGER, PARAMETER :: FuelOil1PollFactor=3
INTEGER, PARAMETER :: FuelOil2PollFactor=4
INTEGER, PARAMETER :: CoalPollFactor=5
INTEGER, PARAMETER :: GasolinePollFactor=6
INTEGER, PARAMETER :: PropanePollFactor=7
INTEGER, PARAMETER :: DieselPollFactor=8
INTEGER, PARAMETER :: OtherFuel1PollFactor=9
INTEGER, PARAMETER :: OtherFuel2PollFactor=10
INTEGER, PARAMETER :: PollFactorNumTypes=10

          ! DERIVED TYPE DEFINITIONS:
TYPE ComponentProps
  INTEGER :: FuelFactorType=0
  REAL(r64)    :: Source = 0.0d0
  REAL(r64)    :: CO2Pollution = 0.0d0
  REAL(r64)    :: COPollution = 0.0d0
  REAL(r64)    :: CH4Pollution = 0.0d0
  REAL(r64)    :: NOxPollution = 0.0d0
  REAL(r64)    :: N2OPollution = 0.0d0
  REAL(r64)    :: SO2Pollution = 0.0d0
  REAL(r64)    :: PMPollution = 0.0d0
  REAL(r64)    :: PM10Pollution = 0.0d0
  REAL(r64)    :: PM25Pollution = 0.0d0
  REAL(r64)    :: NH3Pollution = 0.0d0
  REAL(r64)    :: NMVOCPollution = 0.0d0
  REAL(r64)    :: HgPollution = 0.0d0
  REAL(r64)    :: PbPollution = 0.0d0
  REAL(r64)    :: WaterPollution = 0.0d0
  REAL(r64)    :: NucHiPollution = 0.0d0
  REAL(r64)    :: NucLoPollution = 0.0d0
END TYPE

TYPE CoefficientProps
  INTEGER :: FuelFactorType=0
  LOGICAL :: FuelFactorUsed=.false.
  REAL(r64)    :: Source = 0.0d0
  REAL(r64)    :: CO2 = 0.0d0
  REAL(r64)    :: CO = 0.0d0
  REAL(r64)    :: CH4 = 0.0d0
  REAL(r64)    :: NOx = 0.0d0
  REAL(r64)    :: N2O = 0.0d0
  REAL(r64)    :: SO2 = 0.0d0
  REAL(r64)    :: PM = 0.0d0
  REAL(r64)    :: PM10 = 0.0d0
  REAL(r64)    :: PM25 = 0.0d0
  REAL(r64)    :: NH3 = 0.0d0
  REAL(r64)    :: NMVOC = 0.0d0
  REAL(r64)    :: Hg = 0.0d0
  REAL(r64)    :: Pb = 0.0d0
  REAL(r64)    :: Water = 0.0d0
  REAL(r64)    :: NucHi = 0.0d0
  REAL(r64)    :: NucLo = 0.0d0
  INTEGER :: SourceSched = 0
  INTEGER :: CO2Sched = 0
  INTEGER :: COSched = 0
  INTEGER :: CH4Sched = 0
  INTEGER :: NOxSched = 0
  INTEGER :: N2OSched = 0
  INTEGER :: SO2Sched = 0
  INTEGER :: PMSched = 0
  INTEGER :: PM10Sched = 0
  INTEGER :: PM25Sched = 0
  INTEGER :: NH3Sched = 0
  INTEGER :: NMVOCSched = 0
  INTEGER :: HgSched = 0
  INTEGER :: PbSched = 0
  INTEGER :: WaterSched = 0
  INTEGER :: NucHiSched = 0
  INTEGER :: NucLoSched = 0
END TYPE

TYPE PollutionProps
  !Components
  TYPE (ComponentProps) :: ElecComp
  TYPE (ComponentProps) :: ElecPurchComp
  TYPE (ComponentProps) :: ElecSurplusSoldComp
  TYPE (ComponentProps) :: NatGasComp
  TYPE (ComponentProps) :: FuelOil1Comp
  TYPE (ComponentProps) :: FuelOil2Comp
  TYPE (ComponentProps) :: CoalComp
  TYPE (ComponentProps) :: GasolineComp
  TYPE (ComponentProps) :: PropaneComp
  TYPE (ComponentProps) :: DieselComp
  TYPE (ComponentProps) :: OtherFuel1Comp
  TYPE (ComponentProps) :: OtherFuel2Comp

  !Total for all of the Pollutants
  REAL(r64)    :: N2OPollutTotal = 0.0d0
  REAL(r64)    :: CH4PollutTotal = 0.0d0
  REAL(r64)    :: CO2PollutTotal = 0.0d0

  !Total Carbon Equivalent Components
  REAL(r64)    :: TotCarbonEquivFromN2O = 0.0d0
  REAL(r64)    :: TotCarbonEquivFromCH4 = 0.0d0
  REAL(r64)    :: TotCarbonEquivFromCO2 = 0.0d0

  !Fuel Type Coefficients
  TYPE (CoefficientProps) :: ElecCoef
  TYPE (CoefficientProps) :: NatGasCoef
  TYPE (CoefficientProps) :: FuelOil1Coef
  TYPE (CoefficientProps) :: FuelOil2Coef
  TYPE (CoefficientProps) :: CoalCoef
  TYPE (CoefficientProps) :: GasolineCoef
  TYPE (CoefficientProps) :: PropaneCoef
  TYPE (CoefficientProps) :: DieselCoef
  TYPE (CoefficientProps) :: OtherFuel1Coef
  TYPE (CoefficientProps) :: OtherFuel2Coef

  !Total Carbon Equivalent Coeffs
  REAL(r64)    :: CarbonEquivN2O = 0.0d0
  REAL(r64)    :: CarbonEquivCH4 = 0.0d0
  REAL(r64)    :: CarbonEquivCO2 = 0.0d0

  REAL(r64)    :: PurchHeatEffic = 0.0d0
  REAL(r64)    :: PurchCoolCOP   = 0.0d0
  REAL(r64)    :: SteamConvEffic = 0.0d0
END TYPE PollutionProps

TYPE FuelTypeProps
!FuelType Names
  CHARACTER(len=MaxNameLength), DIMENSION(1:PollFactorNumTypes) :: FuelTypeNames=' '
!Fuel Types used with the Pollution Factors
  REAL(r64)    :: Elec = 0.0d0
  REAL(r64)    :: NatGas = 0.0d0
  REAL(r64)    :: FuelOil1 = 0.0d0
  REAL(r64)    :: FuelOil2 = 0.0d0
  REAL(r64)    :: Coal = 0.0d0
  REAL(r64)    :: Gasoline = 0.0d0
  REAL(r64)    :: Propane = 0.0d0
  REAL(r64)    :: Diesel = 0.0d0
  REAL(r64)    :: OtherFuel1 = 0.0d0
  REAL(r64)    :: OtherFuel2 = 0.0d0
  REAL(r64)    :: ElecPurch = 0.0D0
  REAL(r64)    :: ElecSold  = 0.0D0
!Facility Meter Indexes
  Integer :: ElecFacilityIndex=0
  Integer :: DieselFacilityIndex=0
  Integer :: PurchCoolFacilityIndex=0
  Integer :: PurchHeatFacilityIndex=0
  Integer :: NatGasFacilityIndex=0
  Integer :: GasolineFacilityIndex=0
  Integer :: CoalFacilityIndex=0
  Integer :: FuelOil1FacilityIndex=0
  Integer :: FuelOil2FacilityIndex=0
  Integer :: PropaneFacilityIndex=0
  Integer :: OtherFuel1FacilityIndex=0
  Integer :: OtherFuel2FacilityIndex=0
  Integer :: ElecProducedFacilityIndex=0
  Integer :: SteamFacilityIndex=0
  INTEGER :: ElecPurchasedFacilityIndex=0
  INTEGER :: ElecSurplusSoldFacilityIndex=0
!Facility Meter Values used in Pollution Calcs
  REAL(r64)    :: ElecFacility=0.0d0
  REAL(r64)    :: DieselFacility=0.0d0
  REAL(r64)    :: PurchCoolFacility=0.0d0
  REAL(r64)    :: PurchHeatFacility=0.0d0
  REAL(r64)    :: NatGasFacility=0.0d0
  REAL(r64)    :: GasolineFacility=0.0d0
  REAL(r64)    :: CoalFacility=0.0d0
  REAL(r64)    :: FuelOil1Facility=0.0d0
  REAL(r64)    :: FuelOil2Facility=0.0d0
  REAL(r64)    :: PropaneFacility=0.0d0
  REAL(r64)    :: OtherFuel1Facility=0.0d0
  REAL(r64)    :: OtherFuel2Facility=0.0d0
  REAL(r64)    :: ElecProducedFacility=0.0d0
  REAL(r64)    :: SteamFacility=0.0d0
  REAL(r64)    :: ElecPurchasedFacility=0.0D0
  REAL(r64)    :: ElecSurplusSoldFacility=0.0D0
END TYPE FuelTypeProps

          ! MODULE VARIABLE DECLARATIONS:
TYPE (PollutionProps), SAVE :: Pollution=PollutionProps(                       &
  ComponentProps(ElecPollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),     &
  ComponentProps(ElecPollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),     &
  ComponentProps(ElecPollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),     &
  ComponentProps(NatGasPollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),   &
  ComponentProps(FuelOil1PollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0), &
  ComponentProps(FuelOil2PollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0), &
  ComponentProps(CoalPollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),     &
  ComponentProps(GasolinePollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0), &
  ComponentProps(PropanePollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),  &
  ComponentProps(DieselPollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),   &
  ComponentProps(OtherFuel1PollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),   &
  ComponentProps(OtherFuel2PollFactor,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0),   &
  !Total for all of the Pollutants
  0.0d0,0.0d0,0.0d0,                                                          &
  !Total Carbon Equivalent Components
  0.0d0,0.0d0,0.0d0,                                                          &
!
!  !Fuel Types
   CoefficientProps(ElecPollFactor,.false., &
   3.167d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(NatGasPollFactor,.false., &
   1.084d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(FuelOil1PollFactor,.false., &
    1.05d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(FuelOil2PollFactor,.false., &
    1.05d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(CoalPollFactor,.false., &
    1.05d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(GasolinePollFactor,.false., &
    1.05d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(PropanePollFactor,.false., &
    1.05d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(DieselPollFactor,.false., &
    1.05d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(OtherFuel1PollFactor,.false., &
    1.0d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
   CoefficientProps(OtherFUel2PollFactor,.false., &
    1.0d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
      0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ), &
  !Total Carbon Equivalent Coeffs
  0.d0,0.d0,0.d0,                                                                      &
  ! Purchased Efficiencies
  0.d0,0.d0,0.d0)

TYPE (FuelTypeProps), SAVE :: FuelType =FuelTypeProps(' ',                   &
!Fuel Types used with the Pollution Factors
              0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,   &
!Facility Meter Indexes
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                                 &
!Facility Meter Values used in Pollution Calcs
              0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0)

LOGICAL :: PollutionReportSetup = .False.
LOGICAL :: GetInputFlagPollution = .True.
INTEGER :: NumEnvImpactFactors=0
INTEGER :: NumFuelFactors=0


!         Subroutine Specifications for the Module
Public CalculatePollution
Public SetupPollutionCalculations
Public SetupPollutionMeterReporting
Public CheckPollutionMeterReporting
Public GetFuelFactorInfo
Public GetEnvironmentalImpactFactorInfo

Private ReadEnergyMeters
Private CalcPollution
Private CheckFFSchedule

Contains

! MODULE SUBROUTINES:
!*************************************************************************
Subroutine CalculatePollution

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  December 2003 RJL

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver for the pollution calculation

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

    IF (.Not. PollutionReportSetup) Return

!   Call the Routine to Read the Energy Values from the EnergyPlus Meters
      Call ReadEnergyMeters

!   Call the routine that takes the fuel data and calculates the
!     Pollution for each fuel type.
      Call CalcPollution

 Return
 End Subroutine CalculatePollution

! Get Input Section of the Module
!******************************************************************************
SUBROUTINE SetupPollutionCalculations

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  December 2003 RJL; August 2008 LKL - more standard getinput

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataIPShortCuts
    USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,MakeUPPERCase
    USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowFatalError
    USE ScheduleManager, ONLY: GetScheduleIndex

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
    INTEGER :: NumPolluteRpt
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: Loop
    INTEGER :: IOSTAT

    !First determine if the Pollution reporting has been triggered, and is not exit.
    cCurrentModuleObject='Output:EnvironmentalImpactFactors'
    NumPolluteRpt = GetNumObjectsFound(cCurrentModuleObject)
    PollutionReportSetup = .true.

    Do Loop = 1,NumPolluteRpt

      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      !Call this routine in the Output Processor to setup the correct Facility energy meters that are
      !  necessary to make sure that the Meter file is opened and written to by the OP so that time stamps
      !  and the like are happening as expected.
      If(.not. lAlphaFieldBlanks(1)) Then
        CALL InitPollutionMeterReporting(cAlphaArgs(1))
      Else
        CALL InitPollutionMeterReporting('RunPeriod')
      End If
    End Do


  RETURN

END SUBROUTINE SetupPollutionCalculations

SUBROUTINE GetPollutionFactorInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! SetupPollutionCalculation must be called after meters are initialized.  This caused a problem
          ! in runs so have added this routine to allow central get for most inputs.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataIPShortCuts
    USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,MakeUPPERCase
    USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowFatalError

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
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: Loop
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound = .False.
    INTEGER, external :: GetMeterIndex

    IF (.not. GetInputFlagPollution) RETURN   ! Input already gotten

    GetInputFlagPollution=.false.

    cCurrentModuleObject='EnvironmentalImpactFactors'
    NumEnvImpactFactors=GetNumObjectsFound(cCurrentModuleObject)
    IF (NumEnvImpactFactors > 0) THEN
      ! Now find and load all of the user inputs and factors.
      CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ELSE
      IF (PollutionReportSetup)  &
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': not entered.  Values will be defaulted.')
    ENDIF

    Pollution%PurchHeatEffic = 0.3d0
    Pollution%PurchCoolCOP =  3.0d0
    Pollution%SteamConvEffic = 0.25d0
    Pollution%CarbonEquivN2O = 0.0d0
    Pollution%CarbonEquivCH4 = 0.0d0
    Pollution%CarbonEquivCO2 = 0.0d0

    IF (NumEnvImpactFactors > 0) THEN
      !If Heating Efficiency defined by the User is negative or zero then a default of 30% will be assigned.
      If(rNumericArgs(1) > 0.0d0) Then
        Pollution%PurchHeatEffic = rNumericArgs(1)
      End If

      !If COP defined by the User is negative or zero then a default of 3.0 will be assigned.
      If(rNumericArgs(2) > 0.0d0) Then
        Pollution%PurchCoolCOP =  rNumericArgs(2)
      End If

      !If Steam Conversion Efficiency defined by the User is negative or zero then a default of 25% will be assigned.
      If(rNumericArgs(1) > 0.0d0) Then
        Pollution%SteamConvEffic = rNumericArgs(3)
      End If

      !Load the Total Carbon Equivalent Pollution Factor coefficients
      Pollution%CarbonEquivN2O = rNumericArgs(4)
      Pollution%CarbonEquivCH4 = rNumericArgs(5)
      Pollution%CarbonEquivCO2 = rNumericArgs(6)
    End If


    !Compare all of the Fuel Factors and compare to PollutionCalculationFactors List
    cCurrentModuleObject='FuelFactors'
    NumFuelFactors = GetNumObjectsFound(cCurrentModuleObject)

    Do Loop = 1,NumFuelFactors
      ! Now find and load all of the user inputs and factors.
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      FuelType%FuelTypeNames(Loop) = Trim(cAlphaArgs(1))

      SELECT CASE (MakeUPPERCase(FuelType%FuelTypeNames(Loop)))
        CASE ('NATURALGAS','NATURAL GAS','GAS')
          IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%NatGasCoef%FuelFactorUsed = .True.
         !Natural Gas Coeffs
          Pollution%NatGasCoef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%NatGasCoef%SourceSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%NatGasCoef%CO2Sched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%NatGasCoef%COSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%NatGasCoef%CH4Sched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%NatGasCoef%NOxSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%NatGasCoef%N2OSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%NatGasCoef%SO2Sched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%NatGasCoef%PMSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%NatGasCoef%PM10Sched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%NatGasCoef%PM25Sched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%NatGasCoef%NH3Sched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%NatGasCoef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%NatGasCoef%HgSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%NatGasCoef%PbSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%NatGasCoef%WaterSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%NatGasCoef%NucHiSched,ErrorsFound)
          END IF
          Pollution%NatGasCoef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Natural Gas',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%NatGasCoef%NucLoSched,ErrorsFound)
          END IF

        CASE ('RESIDUALOIL','RESIDUAL OIL','FUEL OIL #2','FUELOIL#2')
          IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%FuelOil2Coef%FuelFactorUsed = .True.
         !FuelOil#2 Coeffs
          Pollution%FuelOil2Coef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%FuelOil2Coef%SourceSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%FuelOil2Coef%CO2Sched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%FuelOil2Coef%COSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%FuelOil2Coef%CH4Sched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%FuelOil2Coef%NOxSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%FuelOil2Coef%N2OSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%FuelOil2Coef%SO2Sched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%FuelOil2Coef%PMSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%FuelOil2Coef%PM10Sched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%FuelOil2Coef%PM25Sched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%FuelOil2Coef%NH3Sched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%FuelOil2Coef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%FuelOil2Coef%HgSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%FuelOil2Coef%PbSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%FuelOil2Coef%WaterSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%FuelOil2Coef%NucHiSched,ErrorsFound)
          END IF
          Pollution%FuelOil2Coef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#2',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%FuelOil2Coef%NucLoSched,ErrorsFound)
          END IF

        CASE ('DISTILLATEOIL','DISTILLATE OIL','FUEL OIL #1','FUELOIL#1','FUEL OIL')
          IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%FuelOil1Coef%FuelFactorUsed = .True.
         !FuelOil#1 Coeffs
          Pollution%FuelOil1Coef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%FuelOil1Coef%SourceSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%FuelOil1Coef%CO2Sched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%FuelOil1Coef%COSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%FuelOil1Coef%CH4Sched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%FuelOil1Coef%NOxSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%FuelOil1Coef%N2OSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%FuelOil1Coef%SO2Sched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%FuelOil1Coef%PMSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%FuelOil1Coef%PM10Sched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%FuelOil1Coef%PM25Sched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%FuelOil1Coef%NH3Sched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%FuelOil1Coef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%FuelOil1Coef%HgSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%FuelOil1Coef%PbSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%FuelOil1Coef%WaterSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%FuelOil1Coef%NucHiSched,ErrorsFound)
          END IF
          Pollution%FuelOil1Coef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Fuel Oil#1',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%FuelOil1Coef%NucLoSched,ErrorsFound)
          END IF

        CASE ('COAL')
          IF (Pollution%CoalCoef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%CoalCoef%FuelFactorUsed = .True.
        ! Coal
          Pollution%CoalCoef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%CoalCoef%SourceSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%CoalCoef%CO2Sched,ErrorsFound)
          END IF
          Pollution%CoalCoef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%CoalCoef%COSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%CoalCoef%CH4Sched,ErrorsFound)
          END IF
          Pollution%CoalCoef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%CoalCoef%NOxSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%CoalCoef%N2OSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%CoalCoef%SO2Sched,ErrorsFound)
          END IF
          Pollution%CoalCoef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%CoalCoef%PMSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%CoalCoef%PM10Sched,ErrorsFound)
          END IF
          Pollution%CoalCoef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%CoalCoef%PM25Sched,ErrorsFound)
          END IF
          Pollution%CoalCoef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%CoalCoef%NH3Sched,ErrorsFound)
          END IF
          Pollution%CoalCoef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%CoalCoef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%CoalCoef%HgSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%CoalCoef%PbSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%CoalCoef%WaterSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%CoalCoef%NucHiSched,ErrorsFound)
          END IF
          Pollution%CoalCoef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Coal',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%CoalCoef%NucLoSched,ErrorsFound)
          END IF

        CASE ('ELECTRICITY','ELECTRIC','ELEC')
          IF (Pollution%ElecCoef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%ElecCoef%FuelFactorUsed = .True.
         !Electric Coeffs
          Pollution%ElecCoef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%ElecCoef%SourceSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%ElecCoef%CO2Sched,ErrorsFound)
          END IF
          Pollution%ElecCoef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%ElecCoef%COSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%ElecCoef%CH4Sched,ErrorsFound)
          END IF
          Pollution%ElecCoef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%ElecCoef%NOxSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%ElecCoef%N2OSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%ElecCoef%SO2Sched,ErrorsFound)
          END IF
          Pollution%ElecCoef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%ElecCoef%PMSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%ElecCoef%PM10Sched,ErrorsFound)
          END IF
          Pollution%ElecCoef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%ElecCoef%PM25Sched,ErrorsFound)
          END IF
          Pollution%ElecCoef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%ElecCoef%NH3Sched,ErrorsFound)
          END IF
          Pollution%ElecCoef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%ElecCoef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%ElecCoef%HgSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%ElecCoef%PbSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%ElecCoef%WaterSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%ElecCoef%NucHiSched,ErrorsFound)
          END IF
          Pollution%ElecCoef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Electricity',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%ElecCoef%NucLoSched,ErrorsFound)
          END IF

        CASE ('GASOLINE')
          IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%GasolineCoef%FuelFactorUsed = .True.
         !Gasoline Coeffs
          Pollution%GasolineCoef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%GasolineCoef%SourceSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%GasolineCoef%CO2Sched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%GasolineCoef%COSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%GasolineCoef%CH4Sched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%GasolineCoef%NOxSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%GasolineCoef%N2OSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%GasolineCoef%SO2Sched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%GasolineCoef%PMSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%GasolineCoef%PM10Sched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%GasolineCoef%PM25Sched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%GasolineCoef%NH3Sched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%GasolineCoef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%GasolineCoef%HgSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%GasolineCoef%PbSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%GasolineCoef%WaterSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%GasolineCoef%NucHiSched,ErrorsFound)
          END IF
          Pollution%GasolineCoef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Gasoline',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%GasolineCoef%NucLoSched,ErrorsFound)
          END IF

        CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
          IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%PropaneCoef%FuelFactorUsed = .True.
         !Propane Coeffs
          Pollution%PropaneCoef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%PropaneCoef%SourceSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%PropaneCoef%CO2Sched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%PropaneCoef%COSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%PropaneCoef%CH4Sched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%PropaneCoef%NOxSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%PropaneCoef%N2OSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%PropaneCoef%SO2Sched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%PropaneCoef%PMSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%PropaneCoef%PM10Sched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%PropaneCoef%PM25Sched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%PropaneCoef%NH3Sched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%PropaneCoef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%PropaneCoef%HgSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%PropaneCoef%PbSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%PropaneCoef%WaterSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%PropaneCoef%NucHiSched,ErrorsFound)
          END IF
          Pollution%PropaneCoef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Propane',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%PropaneCoef%NucLoSched,ErrorsFound)
          END IF

        CASE ('DIESEL')
          IF (Pollution%DieselCoef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%DieselCoef%FuelFactorUsed = .True.
         !Diesel Coeffs
          Pollution%DieselCoef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%DieselCoef%SourceSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%DieselCoef%CO2Sched,ErrorsFound)
          END IF
          Pollution%DieselCoef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%DieselCoef%COSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%DieselCoef%CH4Sched,ErrorsFound)
          END IF
          Pollution%DieselCoef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%DieselCoef%NOxSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%DieselCoef%N2OSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%DieselCoef%SO2Sched,ErrorsFound)
          END IF
          Pollution%DieselCoef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%DieselCoef%PMSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%DieselCoef%PM10Sched,ErrorsFound)
          END IF
          Pollution%DieselCoef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%DieselCoef%PM25Sched,ErrorsFound)
          END IF
          Pollution%DieselCoef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%DieselCoef%NH3Sched,ErrorsFound)
          END IF
          Pollution%DieselCoef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%DieselCoef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%DieselCoef%HgSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%DieselCoef%PbSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%DieselCoef%WaterSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%DieselCoef%NucHiSched,ErrorsFound)
          END IF
          Pollution%DieselCoef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'Diesel',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%DieselCoef%NucLoSched,ErrorsFound)
          END IF

        CASE ('OTHERFUEL1')
          IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%OtherFuel1Coef%FuelFactorUsed = .True.
         !OtherFuel1 Coeffs
          Pollution%OtherFuel1Coef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%OtherFuel1Coef%SourceSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%OtherFuel1Coef%CO2Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%OtherFuel1Coef%COSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%OtherFuel1Coef%CH4Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%OtherFuel1Coef%NOxSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%OtherFuel1Coef%N2OSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%OtherFuel1Coef%SO2Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%OtherFuel1Coef%PMSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%OtherFuel1Coef%PM10Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%OtherFuel1Coef%PM25Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%OtherFuel1Coef%NH3Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%OtherFuel1Coef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%OtherFuel1Coef%HgSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%OtherFuel1Coef%PbSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%OtherFuel1Coef%WaterSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%OtherFuel1Coef%NucHiSched,ErrorsFound)
          END IF
          Pollution%OtherFuel1Coef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel1',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%OtherFuel1Coef%NucLoSched,ErrorsFound)
          END IF

        CASE ('OTHERFUEL2')
          IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': '//TRIM(FuelType%FuelTypeNames(Loop))//' already entered.'//  &
              ' Previous entry will be used.')
            CYCLE
          ENDIF
          Pollution%OtherFuel2Coef%FuelFactorUsed = .True.
         !OtherFuel2 Coeffs
          Pollution%OtherFuel2Coef%Source = rNumericArgs(2)
          IF (.not. lAlphaFieldBlanks(3)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(3)),trim(cAlphaArgs(3)),  &
                                      Pollution%OtherFuel2Coef%SourceSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%CO2    = rNumericArgs(3)
          IF (.not. lAlphaFieldBlanks(4)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(4)),trim(cAlphaArgs(4)),  &
                                      Pollution%OtherFuel2Coef%CO2Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%CO     = rNumericArgs(4)
          IF (.not. lAlphaFieldBlanks(5)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(5)),trim(cAlphaArgs(5)),  &
                                      Pollution%OtherFuel2Coef%COSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%CH4    = rNumericArgs(5)
          IF (.not. lAlphaFieldBlanks(6)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(6)),trim(cAlphaArgs(6)),  &
                                      Pollution%OtherFuel2Coef%CH4Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%NOx    = rNumericArgs(6)
          IF (.not. lAlphaFieldBlanks(7)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(7)),trim(cAlphaArgs(7)),  &
                                      Pollution%OtherFuel2Coef%NOxSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%N2O    = rNumericArgs(7)
          IF (.not. lAlphaFieldBlanks(8)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(8)),trim(cAlphaArgs(8)),  &
                                      Pollution%OtherFuel2Coef%N2OSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%SO2    = rNumericArgs(8)
          IF (.not. lAlphaFieldBlanks(9)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(9)),trim(cAlphaArgs(9)),  &
                                      Pollution%OtherFuel2Coef%SO2Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%PM     = rNumericArgs(9)
          IF (.not. lAlphaFieldBlanks(10)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(10)),trim(cAlphaArgs(10)),  &
                                      Pollution%OtherFuel2Coef%PMSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%PM10   = rNumericArgs(10)
          IF (.not. lAlphaFieldBlanks(11)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(11)),trim(cAlphaArgs(11)),  &
                                      Pollution%OtherFuel2Coef%PM10Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%PM25   = rNumericArgs(11)
          IF (.not. lAlphaFieldBlanks(12)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(12)),trim(cAlphaArgs(12)),  &
                                      Pollution%OtherFuel2Coef%PM25Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%NH3    = rNumericArgs(12)
          IF (.not. lAlphaFieldBlanks(13)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(13)),trim(cAlphaArgs(13)),  &
                                      Pollution%OtherFuel2Coef%NH3Sched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%NMVOC  = rNumericArgs(13)
          IF (.not. lAlphaFieldBlanks(14)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(14)),trim(cAlphaArgs(14)),  &
                                      Pollution%OtherFuel2Coef%NMVOCSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%Hg     = rNumericArgs(14)
          IF (.not. lAlphaFieldBlanks(15)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(15)),trim(cAlphaArgs(15)),  &
                                      Pollution%OtherFuel2Coef%HgSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%Pb     = rNumericArgs(15)
          IF (.not. lAlphaFieldBlanks(16)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(16)),trim(cAlphaArgs(16)),  &
                                      Pollution%OtherFuel2Coef%PbSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%Water  = rNumericArgs(16)
          IF (.not. lAlphaFieldBlanks(17)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(17)),trim(cAlphaArgs(17)),  &
                                      Pollution%OtherFuel2Coef%WaterSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%NucHi  = rNumericArgs(17)
          IF (.not. lAlphaFieldBlanks(18)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(18)),trim(cAlphaArgs(18)),  &
                                      Pollution%OtherFuel2Coef%NucHiSched,ErrorsFound)
          END IF
          Pollution%OtherFuel2Coef%NucLo  = rNumericArgs(18)
          IF (.not. lAlphaFieldBlanks(19)) THEN
            CALL CheckFFSchedule(trim(cCurrentModuleObject),'OtherFuel2',trim(cAlphaFieldNames(19)),trim(cAlphaArgs(19)),  &
                                      Pollution%OtherFuel2Coef%NucLoSched,ErrorsFound)
          END IF

        CASE DEFAULT
          CALL ShowSevereError('Illegal FuelType for Pollution Calc Entered='//TRIM(FuelType%FuelTypeNames(Loop)))
          ErrorsFound=.true.

      END SELECT

    End Do ! End of the NumEnergyTypes Do Loop


    FuelType%ElecFacilityIndex     = GetMeterIndex('Electricity:Facility')
    FuelType%DieselFacilityIndex   = GetMeterIndex('Diesel:Facility')
    FuelType%PurchCoolFacilityIndex = GetMeterIndex('DistrictCooling:Facility')
    FuelType%PurchHeatFacilityIndex = GetMeterIndex('DistrictHeating:Facility')
    FuelType%NatGasFacilityIndex   = GetMeterIndex('Gas:Facility')
    FuelType%GasolineFacilityIndex = GetMeterIndex('Gasoline:Facility')
    FuelType%CoalFacilityIndex     = GetMeterIndex('Coal:Facility')
    FuelType%FuelOil1FacilityIndex = GetMeterIndex('FuelOil#1:Facility')
    FuelType%FuelOil2FacilityIndex = GetMeterIndex('FuelOil#2:Facility')
    FuelType%PropaneFacilityIndex  = GetMeterIndex('Propane:Facility')
    FuelType%OtherFuel1FacilityIndex  = GetMeterIndex('OtherFuel1:Facility')
    FuelType%OtherFuel2FacilityIndex  = GetMeterIndex('OtherFuel2:Facility')
    FuelType%ElecProducedFacilityIndex = GetMeterIndex('ElectricityProduced:Facility')
    FuelType%SteamFacilityIndex    = GetMeterIndex('Steam:Facility')
    FuelType%ElecPurchasedFacilityIndex = GetMeterIndex('ElectricityPurchased:Facility')
    FuelType%ElecSurplusSoldFacilityIndex = GetMeterIndex('ElectricitySurplusSold:Facility')

    IF (PollutionReportSetup) THEN  ! only do this if reporting on the pollution
      !Need to go through all of the Fuel Types and make sure a Fuel Factor was found for each type of energy being simulated
      ! Check for Electricity
      If(.not. Pollution%ElecCoef%FuelFactorUsed  &
              .and. ((FuelType%ElecFacilityIndex > 0) .or. (FuelType%ElecProducedFacilityIndex > 0)  &
              .or. (FuelType%PurchCoolFacilityIndex > 0))) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for ELECTRICITY')
          ErrorsFound=.true.
      End If
      ! Check for Natural Gas
      If(.not. Pollution%NatGasCoef%FuelFactorUsed &
               .and. ((FuelType%NatGasFacilityIndex > 0) .or. (FuelType%PurchHeatFacilityIndex>0) &
               .or. (FuelType%SteamFacilityIndex  > 0))) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for NATURAL GAS')
          ErrorsFound=.true.
      End If
      ! Check for Residual Oil
      If(.not. Pollution%FuelOil2Coef%FuelFactorUsed .and. (FuelType%FuelOil2FacilityIndex>0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for RESIDUAL/FUEL OIL #2')
          ErrorsFound=.true.
      End If
      ! Check for Distillate Oil
      If(.not. Pollution%FuelOil1Coef%FuelFactorUsed .and. (FuelType%FuelOil1FacilityIndex>0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for DISTILLATE/FUEL OIL #1')
          ErrorsFound=.true.
      End If
      ! Check for Coal
      If(.not. Pollution%CoalCoef%FuelFactorUsed .and. (FuelType%CoalFacilityIndex > 0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for COAL')
          ErrorsFound=.true.
      End If
      ! Check for Gasoline
      If(.not. Pollution%GasolineCoef%FuelFactorUsed .and. (FuelType%GasolineFacilityIndex > 0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for GASOLINE')
          ErrorsFound=.true.
      End If
      ! Check for Propane
      If(.not. Pollution%PropaneCoef%FuelFactorUsed .and. (FuelType%PropaneFacilityIndex > 0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for PROPANE')
          ErrorsFound=.true.
      End If
      ! Check for Diesel
      If(.not. Pollution%DieselCoef%FuelFactorUsed .and. (FuelType%DieselFacilityIndex > 0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for DIESEL')
          ErrorsFound=.true.
      End If
      ! Check for OtherFuel1
      If(.not. Pollution%OtherFuel1Coef%FuelFactorUsed .and. (FuelType%OtherFuel1FacilityIndex > 0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for OTHERFUEL1')
          ErrorsFound=.true.
      End If
      ! Check for OtherFuel2
      If(.not. Pollution%OtherFuel2Coef%FuelFactorUsed .and. (FuelType%OtherFuel2FacilityIndex > 0)) Then
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
             ' Not Found or Fuel not specified For Pollution Calculation for OTHERFUEL2')
          ErrorsFound=.true.
      End If
    END IF

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in getting Pollution Calculation Reporting Input')
    ENDIF

  RETURN

END SUBROUTINE GetPollutionFactorInput


SUBROUTINE SetupPollutionMeterReporting

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  December 2003 RJL

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: MakeUPPERCase

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Loop


    IF (GetInputFlagPollution) THEN
      CALL GetPollutionFactorInput
      GetInputFlagPollution=.false.
    ENDIF

    Do Loop = 1,PollFactorNumTypes

      IF (FuelType%FuelTypeNames(Loop) == Blank) CYCLE

      SELECT CASE (MakeUPPERCase(FuelType%FuelTypeNames(Loop)))
        CASE ('NATURALGAS','NATURAL GAS','GAS')
          !Pollutants from Natural Gas
          CALL SetupOutputVariable('Environmental Impact Natural Gas Source Energy [J]', Pollution%NatGasComp%Source,            &
         'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas CO2 Emissions Mass [kg]', Pollution%NatGasComp%CO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas CO Emissions Mass [kg]', Pollution%NatGasComp%COPollution,  &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas CH4 Emissions Mass [kg]', Pollution%NatGasComp%CH4Pollution,&
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas NOx Emissions Mass [kg]', Pollution%NatGasComp%NOxPollution,&
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas N2O Emissions Mass [kg]', Pollution%NatGasComp%N2OPollution,&
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas SO2 Emissions Mass [kg]', Pollution%NatGasComp%SO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas PM Emissions Mass [kg]', Pollution%NatGasComp%PMPollution,  &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas PM10 Emissions Mass [kg]', &
                                    Pollution%NatGasComp%PM10Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas PM2.5 Emissions Mass [kg]', &
                                    Pollution%NatGasComp%PM25Pollution,           &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas NH3 Emissions Mass [kg]', Pollution%NatGasComp%NH3Pollution,&
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas NMVOC Emissions Mass [kg]', &
                                    Pollution%NatGasComp%NMVOCPollution,           &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas Hg Emissions Mass [kg]', Pollution%NatGasComp%HgPollution,  &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas Pb Emissions Mass [kg]', Pollution%NatGasComp%PbPollution,  &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas Water Consumption Volume [L]', &
                                    Pollution%NatGasComp%WaterPollution,           &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas Nuclear High Level Waste Mass [kg]', &
                                    Pollution%NatGasComp%NucHiPollution, &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='NaturalGasEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Natural Gas Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%NatGasComp%NucLoPollution, &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='NaturalGasEmissions',GroupKey='')

        CASE ('RESIDUALOIL','RESIDUAL OIL','FUEL OIL #2','FUELOIL#2')
          !Pollutants from FuelOil#2
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 Source Energy [J]', Pollution%FuelOil2Comp%Source,           &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 CO2 Emissions Mass [kg]', &
                                Pollution%FuelOil2Comp%CO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 CO Emissions Mass [kg]', Pollution%FuelOil2Comp%COPollution,&
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 CH4 Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%CH4Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 NOx Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%NOxPollution,            &
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 N2O Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%N2OPollution,            &
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 SO2 Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%SO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 PM Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%PMPollution,              &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 PM10 Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%PM10Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 PM2.5 Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%PM25Pollution,           &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 NH3 Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%NH3Pollution,              &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 NMVOC Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%NMVOCPollution,           &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 Hg Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%HgPollution,              &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 Pb Emissions Mass [kg]', &
                                    Pollution%FuelOil2Comp%PbPollution,              &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 Water Consumption Volume [L]', &
                                    Pollution%FuelOil2Comp%WaterPollution,  &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 Nuclear High Level Waste Mass [kg]', &
                                    Pollution%FuelOil2Comp%NucHiPollution, &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='FuelOil#2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #2 Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%FuelOil2Comp%NucLoPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='FuelOil#2Emissions',GroupKey='')

        CASE ('DISTILLATEOIL','DISTILLATE OIL','FUEL OIL #1','FUELOIL#1','FUEL OIL')
          !Pollutants from FuelOil#1
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 Source Energy [J]', Pollution%FuelOil1Comp%Source,  &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 CO2 Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%CO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 CO Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%COPollution,            &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 CH4 Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%CH4Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 NOx Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%NOxPollution,            &
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 N2O Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%N2OPollution,            &
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 SO2 Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%SO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 PM Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%PMPollution,              &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 PM10 Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%PM10Pollution,           &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 PM2.5 Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%PM25Pollution,           &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 NH3 Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%NH3Pollution,              &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 NMVOC Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%NMVOCPollution,           &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 Hg Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%HgPollution,              &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 Pb Emissions Mass [kg]', &
                                    Pollution%FuelOil1Comp%PbPollution,              &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 Water Consumption Volume [L]', &
                                    Pollution%FuelOil1Comp%WaterPollution,           &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 Nuclear High Level Waste Mass [kg]', &
                                    Pollution%FuelOil1Comp%NucHiPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='FuelOil#1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Fuel Oil #1 Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%FuelOil1Comp%NucLoPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='FuelOil#1Emissions',GroupKey='')

        CASE ('COAL')
          !Pollutants from Coal
          CALL SetupOutputVariable('Environmental Impact Coal Source Energy [J]', Pollution%CoalComp%Source,            &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal CO2 Emissions Mass [kg]', Pollution%CoalComp%CO2Pollution,     &
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal CO Emissions Mass [kg]', Pollution%CoalComp%COPollution,       &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal CH4 Emissions Mass [kg]', Pollution%CoalComp%CH4Pollution,  &
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal NOx Emissions Mass [kg]', Pollution%CoalComp%NOxPollution,     &
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal N2O Emissions Mass [kg]', Pollution%CoalComp%N2OPollution,     &
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal SO2 Emissions Mass [kg]', Pollution%CoalComp%SO2Pollution,     &
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal PM Emissions Mass [kg]', Pollution%CoalComp%PMPollution,       &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal PM10 Emissions Mass [kg]', Pollution%CoalComp%PM10Pollution,   &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal PM2.5 Emissions Mass [kg]', Pollution%CoalComp%PM25Pollution,  &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal NH3 Emissions Mass [kg]', Pollution%CoalComp%NH3Pollution,     &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal NMVOC Emissions Mass [kg]', Pollution%CoalComp%NMVOCPollution, &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal Hg Emissions Mass [kg]', Pollution%CoalComp%HgPollution,       &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal Pb Emissions Mass [kg]', Pollution%CoalComp%PbPollution,       &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal Water Consumption Volume [L]', Pollution%CoalComp%WaterPollution,&
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal Nuclear High Level Waste Mass [kg]', &
                                    Pollution%CoalComp%NucHiPollution,&
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='CoalEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Coal Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%CoalComp%NucLoPollution,&
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='CoalEmissions',GroupKey='')

        CASE ('ELECTRICITY','ELECTRIC','ELEC')
          !Pollutants from Electricity
          CALL SetupOutputVariable('Environmental Impact Electricity Source Energy [J]', Pollution%ElecComp%Source,          &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity CO2 Emissions Mass [kg]', Pollution%ElecComp%CO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity CO Emissions Mass [kg]', Pollution%ElecComp%COPollution,  &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity CH4 Emissions Mass [kg]', Pollution%ElecComp%CH4Pollution,&
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity NOx Emissions Mass [kg]', Pollution%ElecComp%NOxPollution,&
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity N2O Emissions Mass [kg]', Pollution%ElecComp%N2OPollution,&
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity SO2 Emissions Mass [kg]', Pollution%ElecComp%SO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity PM Emissions Mass [kg]', Pollution%ElecComp%PMPollution,   &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity PM10 Emissions Mass [kg]', Pollution%ElecComp%PM10Pollution,&
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity PM2.5 Emissions Mass [kg]', Pollution%ElecComp%PM25Pollution,&
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity NH3 Emissions Mass [kg]', Pollution%ElecComp%NH3Pollution,  &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity NMVOC Emissions Mass [kg]', &
                                    Pollution%ElecComp%NMVOCPollution,&
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity Hg Emissions Mass [kg]', Pollution%ElecComp%HgPollution,     &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity Pb Emissions Mass [kg]', Pollution%ElecComp%PbPollution,     &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity Water Consumption Volume [L]', &
                                    Pollution%ElecComp%WaterPollution,           &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity Nuclear High Level Waste Mass [kg]', &
                                    Pollution%ElecComp%NucHiPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Electricity Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%ElecComp%NucLoPollution, &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='ElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Purchased Electricity Source Energy [J]', &
                                     Pollution%ElecPurchComp%Source,            &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='PurchasedElectricEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Surplus Sold Electricity Source [J]', &
                                     Pollution%ElecSurplusSoldComp%Source,            &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='SoldElectricEmissions',GroupKey='')
        CASE ('GASOLINE')
          !Pollutants from Gasoline
          CALL SetupOutputVariable('Environmental Impact Gasoline Source Energy [J]', Pollution%GasolineComp%Source,           &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline CO2 Emissions Mass [kg]', Pollution%GasolineComp%CO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline CO Emissions Mass [kg]', Pollution%GasolineComp%COPollution,  &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline CH4 Emissions Mass [kg]', Pollution%GasolineComp%CH4Pollution,&
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline NOx Emissions Mass [kg]', Pollution%GasolineComp%NOxPollution,&
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline N2O Emissions Mass [kg]', Pollution%GasolineComp%N2OPollution,&
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline SO2 Emissions Mass [kg]', Pollution%GasolineComp%SO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline PM Emissions Mass [kg]', Pollution%GasolineComp%PMPollution,  &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline PM10 Emissions Mass [kg]', &
                                    Pollution%GasolineComp%PM10Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline PM2.5 Emissions Mass [kg]', &
                                    Pollution%GasolineComp%PM25Pollution,           &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline NH3 Emissions Mass [kg]', &
                                    Pollution%GasolineComp%NH3Pollution,              &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline NMVOC Emissions Mass [kg]', &
                                    Pollution%GasolineComp%NMVOCPollution,            &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline Hg Emissions Mass [kg]', &
                                    Pollution%GasolineComp%HgPollution,              &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline Pb Emissions Mass [kg]', &
                                    Pollution%GasolineComp%PbPollution,              &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline Water Consumption Volume [L]', &
                                    Pollution%GasolineComp%WaterPollution,           &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline Nuclear High Level Waste Mass [kg]', &
                                    Pollution%GasolineComp%NucHiPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='GasolineEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Gasoline Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%GasolineComp%NucLoPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='GasolineEmissions',GroupKey='')

        CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
          !Pollutants from Propane
          CALL SetupOutputVariable('Environmental Impact Propane Source Energy [J]', Pollution%PropaneComp%Source,            &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane CO2 Emissions Mass [kg]', Pollution%PropaneComp%CO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane CO Emissions Mass [kg]', Pollution%PropaneComp%COPollution, &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane CH4 Emissions Mass [kg]', Pollution%PropaneComp%CH4Pollution,&
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane NOx Emissions Mass [kg]', Pollution%PropaneComp%NOxPollution,&
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane N2O Emissions Mass [kg]', Pollution%PropaneComp%N2OPollution,&
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane SO2 Emissions Mass [kg]', Pollution%PropaneComp%SO2Pollution,&
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane PM Emissions Mass [kg]', Pollution%PropaneComp%PMPollution,  &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane PM10 Emissions Mass [kg]', Pollution%PropaneComp%PM10Pollution,&
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane PM2.5 Emissions Mass [kg]', &
                                    Pollution%PropaneComp%PM25Pollution,           &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane NH3 Emissions Mass [kg]', Pollution%PropaneComp%NH3Pollution,  &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane NMVOC Emissions Mass [kg]', Pollution%PropaneComp%NMVOCPollution,&
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane Hg Emissions Mass [kg]', Pollution%PropaneComp%HgPollution,  &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane Pb Emissions Mass [kg]', Pollution%PropaneComp%PbPollution,  &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane Water Consumption Volume [L]', &
                                    Pollution%PropaneComp%WaterPollution,            &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane Nuclear High Level Waste Mass [kg]', &
                                    Pollution%PropaneComp%NucHiPollution,           &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='PropaneEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Propane Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%PropaneComp%NucLoPollution,            &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='PropaneEmissions',GroupKey='')

        CASE ('DIESEL')
          !Pollutants from Diesel
          CALL SetupOutputVariable('Environmental Impact Diesel Source Energy [J]', Pollution%DieselComp%Source,       &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel CO2 Emissions Mass [kg]', Pollution%DieselComp%CO2Pollution, &
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel CO Emissions Mass [kg]', Pollution%DieselComp%COPollution,   &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel CH4 Emissions Mass [kg]', Pollution%DieselComp%CH4Pollution, &
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel NOx Emissions Mass [kg]', Pollution%DieselComp%NOxPollution, &
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel N2O Emissions Mass [kg]', Pollution%DieselComp%N2OPollution, &
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel SO2 Emissions Mass [kg]', Pollution%DieselComp%SO2Pollution, &
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel PM Emissions Mass [kg]', Pollution%DieselComp%PMPollution,   &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel PM10 Emissions Mass [kg]', Pollution%DieselComp%PM10Pollution,&
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel PM2.5 Emissions Mass [kg]', Pollution%DieselComp%PM25Pollution,&
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel NH3 Emissions Mass [kg]', Pollution%DieselComp%NH3Pollution,  &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel NMVOC Emissions Mass [kg]', Pollution%DieselComp%NMVOCPollution,&
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel Hg Emissions Mass [kg]', Pollution%DieselComp%HgPollution,  &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel Pb Emissions Mass [kg]', Pollution%DieselComp%PbPollution,   &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel Water Consumption Volume [L]', &
                                    Pollution%DieselComp%WaterPollution, &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel Nuclear High Level Waste Mass [kg]', &
                                    Pollution%DieselComp%NucHiPollution,  &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='DieselEmissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact Diesel Nuclear Low Level Waste Volume [m3]', &
                                    Pollution%DieselComp%NucLoPollution,           &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='DieselEmissions',GroupKey='')

        CASE ('OTHERFUEL1')
          !Pollutants from OtherFuel1
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 Source Energy [J]',   &
             Pollution%OtherFuel1Comp%Source,            &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 CO2 Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%CO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 CO Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%COPollution,            &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 CH4 Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%CH4Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 NOx Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%NOxPollution,            &
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 N2O Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%N2OPollution,            &
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 SO2 Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%SO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 PM Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%PMPollution,              &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 PM10 Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%PM10Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 PM2.5 Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%PM25Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 NH3 Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%NH3Pollution,              &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 NMVOC Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%NMVOCPollution,            &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 Hg Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%HgPollution,              &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 Pb Emissions Mass [kg]',   &
             Pollution%OtherFuel1Comp%PbPollution,              &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 CO2 Water Consumption Volume [L]',   &
             Pollution%OtherFuel1Comp%WaterPollution,            &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 Nuclear High Level Waste Mass [kg]',   &
             Pollution%OtherFuel1Comp%NucHiPollution,          &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='OtherFuel1Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel1 Nuclear Low Level Waste Volume [m3]',   &
             Pollution%OtherFuel1Comp%NucLoPollution,           &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='OtherFuel1Emissions',GroupKey='')

        CASE ('OTHERFUEL2')
          !Pollutants from OtherFuel2
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 Source Energy [J]',   &
             Pollution%OtherFuel2Comp%Source,            &
           'System','Sum','Site',ResourceTypeKey='Source',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 CO2 Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%CO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CO2',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 CO Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%COPollution,            &
           'System','Sum','Site',ResourceTypeKey='CO',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 CH4 Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%CH4Pollution,            &
           'System','Sum','Site',ResourceTypeKey='CH4',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 NOx Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%NOxPollution,            &
           'System','Sum','Site',ResourceTypeKey='NOx',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 N2O Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%N2OPollution,            &
           'System','Sum','Site',ResourceTypeKey='N2O',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 SO2 Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%SO2Pollution,            &
           'System','Sum','Site',ResourceTypeKey='SO2',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 PM Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%PMPollution,              &
           'System','Sum','Site',ResourceTypeKey='PM',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 PM10 Emissions Mass [kg]',   &
             Pollution%OtherFuel2Comp%PM10Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM10',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 PM2.5 Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%PM25Pollution,            &
           'System','Sum','Site',ResourceTypeKey='PM2.5',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 NH3 Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%NH3Pollution,              &
           'System','Sum','Site',ResourceTypeKey='NH3',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 NMVOC Emissions Mass [kg]',   &
             Pollution%OtherFuel2Comp%NMVOCPollution,            &
           'System','Sum','Site',ResourceTypeKey='NMVOC',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 Hg Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%HgPollution,              &
           'System','Sum','Site',ResourceTypeKey='Hg',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 Pb Emissions Mass [kg]',  &
             Pollution%OtherFuel2Comp%PbPollution,              &
           'System','Sum','Site',ResourceTypeKey='Pb',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 CO2 Water Consumption Volume [L]',  &
             Pollution%OtherFuel2Comp%WaterPollution,            &
           'System','Sum','Site',ResourceTypeKey='WaterEnvironmentalFactors',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 Nuclear High Level Waste Mass [kg]',   &
             Pollution%OtherFuel2Comp%NucHiPollution,          &
           'System','Sum','Site',ResourceTypeKey='Nuclear High',EndUseKey='OtherFuel2Emissions',GroupKey='')
          CALL SetupOutputVariable('Environmental Impact OtherFuel2 Nuclear Low Level Waste Volume [m3]',  &
             Pollution%OtherFuel2Comp%NucLoPollution,           &
           'System','Sum','Site',ResourceTypeKey='Nuclear Low',EndUseKey='OtherFuel2Emissions',GroupKey='')

      END SELECT

    End Do ! End of the NumEnergyTypes Do Loop


    ! Always setup the Total Carbon Equivalent
    CALL SetupOutputVariable('Environmental Impact Total N2O Emissions Carbon Equivalent Mass [kg]', &
                              Pollution%TotCarbonEquivFromN2O,            &
        'System','Sum','Site',ResourceTypeKey='Carbon Equivalent',EndUseKey='CarbonEquivalentEmissions',GroupKey='')
    CALL SetupOutputVariable('Environmental Impact Total CH4 Emissions Carbon Equivalent Mass [kg]', &
                              Pollution%TotCarbonEquivFromCH4,            &
        'System','Sum','Site',ResourceTypeKey='Carbon Equivalent',EndUseKey='CarbonEquivalentEmissions',GroupKey='')
    CALL SetupOutputVariable('Environmental Impact Total CO2 Emissions Carbon Equivalent Mass [kg]', &
                              Pollution%TotCarbonEquivFromCO2,            &
        'System','Sum','Site',ResourceTypeKey='Carbon Equivalent',EndUseKey='CarbonEquivalentEmissions',GroupKey='')

  RETURN

END SUBROUTINE SetupPollutionMeterReporting

SUBROUTINE CheckPollutionMeterReporting

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError

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
    LOGICAL, external :: ReportingThisVariable


  ! in progress
    IF (NumFuelFactors == 0 .or. NumEnvImpactFactors == 0) THEN
      IF (ReportingThisVariable('Environmental Impact Total N2O Emissions Carbon Equivalent Mass') .or.   &
          ReportingThisVariable('Environmental Impact Total CH4 Emissions Carbon Equivalent Mass') .or.   &
          ReportingThisVariable('Environmental Impact Total CO2 Emissions Carbon Equivalent Mass') .or.   &
          ReportingThisVariable('Carbon Equivalent:Facility') .or.   &
          ReportingThisVariable('CarbonEquivalentEmissions:Carbon Equivalent') ) THEN
        CALL ShowWarningError('GetPollutionFactorInput: Requested reporting for Carbon Equivalent Pollution, '//  &
          'but insufficient information is entered.')
        CALL ShowContinueError('Both "FuelFactors" and "EnvironmentalImpactFactors" must be entered or the '//  &
          'displayed carbon pollution will all be zero.')
      ENDIF
    ENDIF

  RETURN

END SUBROUTINE CheckPollutionMeterReporting

SUBROUTINE CheckFFSchedule(currentModuleObject,resourceType,fieldName,scheduleName,schedulePtr,errorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This support routine performs the "obtain schedule pointer" and checks Fuel Factor
          ! schedules for validity (values must be >= 0).


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowSevereError,ShowContinueError
    USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: currentModuleObject  ! the module Object
  CHARACTER(len=*), INTENT(IN) :: resourceType         ! resource type (Natural Gas, etc)
  CHARACTER(len=*), INTENT(IN) :: fieldName            ! Actual field name
  CHARACTER(len=*), INTENT(IN) :: scheduleName         ! Schedule Name as input
  INTEGER, INTENT(INOUT)       :: schedulePtr          ! Schedule Index
  LOGICAL, INTENT(INOUT)       :: errorsFound          ! true if errors found

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  schedulePtr = GetScheduleIndex(scheduleName)
  IF (schedulePtr == 0) THEN
    CALL ShowSevereError(currentModuleObject//': '//resourceType//', invalid '//fieldName//  &
           '="'//scheduleName//'" not found.')
    errorsFound = .true.
  ELSEIF (.NOT. CheckScheduleValueMinMax(schedulePtr,'>=',0.0d0)) THEN
    CALL ShowSevereError(currentModuleObject//': '//resourceType//', invalid '//fieldName//  &
           '="'//scheduleName//'" invalid values.')
    CALL ShowContinueError('Schedule values must be (>=0.).')
    errorsFound=.true.
  END IF

  RETURN

END SUBROUTINE CheckFFSchedule

! End of Get Input subroutines for the Pollution Module
!******************************************************************************


 SUBROUTINE CalcPollution
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   1998
          !       MODIFIED       na
          !       RE-ENGINEERED  December 2003 RJL

          ! PURPOSE OF THIS SUBROUTINE:
          ! CalcPollution - Does the Pollutant Calculation

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

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
      REAL(r64) :: ElecValue
      REAL(r64) :: NatGasValue
      REAL(r64) :: FuelOil1Value
      REAL(r64) :: FuelOil2Value
      REAL(r64) :: CoalValue
      REAL(r64) :: GasolineValue
      REAL(r64) :: PropaneValue
      REAL(r64) :: DieselValue
      REAL(r64) :: OtherFuel1Value
      REAL(r64) :: OtherFuel2Value


!       Then the amount of Pollution produced by each fuel type is
!       calculated in kgs.
!       Input units for the coefficients is not standard and needs to be converted here.
!       Most of the units are g/MJ, however water is in L/MJ and low level nuclear water is m3/MJ
!       so only the energy has to be converted from J to MJ.

!     For each pollution/fuel type, Schedule values are allowed.  Thus, calculations are bundled.

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%CO2Pollution     = 0.0d0
        IF (Pollution%ElecCoef%CO2Sched == 0) THEN
          ElecValue = Pollution%ElecCoef%CO2/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%CO2*GetCurrentScheduleValue(Pollution%ElecCoef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%ElecComp%CO2Pollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%CO2Pollution   = 0.0d0
        IF (Pollution%NatGasCoef%CO2Sched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%CO2/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%CO2*GetCurrentScheduleValue(Pollution%NatGasCoef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%CO2Pollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%CO2Pollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%CO2Sched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%CO2/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%CO2*GetCurrentScheduleValue(Pollution%FuelOil1Coef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%CO2Pollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%CO2Pollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%CO2Sched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%CO2/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%CO2*GetCurrentScheduleValue(Pollution%FuelOil2Coef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%CO2Pollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%CO2Pollution  = 0.0d0
        IF (Pollution%CoalCoef%CO2Sched == 0) THEN
          CoalValue = Pollution%CoalCoef%CO2/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%CO2*GetCurrentScheduleValue(Pollution%CoalCoef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%CoalComp%CO2Pollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%CO2Pollution  = 0.0d0
        IF (Pollution%GasolineCoef%CO2Sched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%CO2/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%CO2*GetCurrentScheduleValue(Pollution%GasolineCoef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%CO2Pollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%CO2Pollution  = 0.0d0
        IF (Pollution%PropaneCoef%CO2Sched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%CO2/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%CO2*GetCurrentScheduleValue(Pollution%PropaneCoef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%CO2Pollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%CO2Pollution  = 0.0d0
        IF (Pollution%DieselCoef%CO2Sched == 0) THEN
          DieselValue = Pollution%DieselCoef%CO2/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%CO2*GetCurrentScheduleValue(Pollution%DieselCoef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%DieselComp%CO2Pollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF

      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%CO2Pollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%CO2Sched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%CO2/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%CO2*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%CO2Pollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF

      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%CO2Pollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%CO2Sched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%CO2/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%CO2*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%CO2Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%CO2Pollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF


      Pollution%CO2PollutTotal = Pollution%ElecComp%CO2Pollution +     &
                        Pollution%NatGasComp%CO2Pollution        +      &
                        Pollution%FuelOil1Comp%CO2Pollution      +      &
                        Pollution%FuelOil2Comp%CO2Pollution      +      &
                        Pollution%CoalComp%CO2Pollution          +      &
                        Pollution%GasolineComp%CO2Pollution      +      &
                        Pollution%PropaneComp%CO2Pollution       +      &
                        Pollution%DieselComp%CO2Pollution        +      &
                        Pollution%OtherFuel1Comp%CO2Pollution    +      &
                        Pollution%OtherFuel2Comp%CO2Pollution

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%NOxPollution     = 0.0d0
        IF (Pollution%ElecCoef%NOxSched == 0) THEN
          ElecValue = Pollution%ElecCoef%NOx/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%NOx*GetCurrentScheduleValue(Pollution%ElecCoef%NOxSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%NOxPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%NOxPollution   = 0.0d0
        IF (Pollution%NatGasCoef%NOxSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%NOx/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%NOx*GetCurrentScheduleValue(Pollution%NatGasCoef%NOxSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%NOxPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%NOxPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%NOxSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%NOx/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%NOx*GetCurrentScheduleValue(Pollution%FuelOil1Coef%NOxSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%NOxPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%NOxPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%NOxSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%NOx/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%NOx*GetCurrentScheduleValue(Pollution%FuelOil2Coef%NOxSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%NOxPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%NOxPollution  = 0.0d0
        IF (Pollution%CoalCoef%NOxSched == 0) THEN
          CoalValue = Pollution%CoalCoef%NOx/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%NOx*GetCurrentScheduleValue(Pollution%CoalCoef%NOxSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%NOxPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%NOxPollution  = 0.0d0
        IF (Pollution%GasolineCoef%NOxSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%NOx/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%NOx*GetCurrentScheduleValue(Pollution%GasolineCoef%NOxSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%NOxPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%NOxPollution  = 0.0d0
        IF (Pollution%PropaneCoef%NOxSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%NOx/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%NOx*GetCurrentScheduleValue(Pollution%PropaneCoef%NOxSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%NOxPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%NOxPollution  = 0.0d0
        IF (Pollution%DieselCoef%NOxSched == 0) THEN
          DieselValue = Pollution%DieselCoef%NOx/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%NOx*GetCurrentScheduleValue(Pollution%DieselCoef%NOxSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%NOxPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%NOxPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%NOxSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%NOx/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%NOx*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%NOxSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%NOxPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%NOxPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%NOxSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%NOx/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%NOx*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%NOxSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%NOxPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%CH4Pollution     = 0.0d0
        IF (Pollution%ElecCoef%CH4Sched == 0) THEN
          ElecValue = Pollution%ElecCoef%CH4/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%CH4*GetCurrentScheduleValue(Pollution%ElecCoef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%ElecComp%CH4Pollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%CH4Pollution   = 0.0d0
        IF (Pollution%NatGasCoef%CH4Sched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%CH4/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%CH4*GetCurrentScheduleValue(Pollution%NatGasCoef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%CH4Pollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%CH4Pollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%CH4Sched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%CH4/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%CH4*GetCurrentScheduleValue(Pollution%FuelOil1Coef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%CH4Pollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%CH4Pollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%CH4Sched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%CH4/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%CH4*GetCurrentScheduleValue(Pollution%FuelOil2Coef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%CH4Pollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%CH4Pollution  = 0.0d0
        IF (Pollution%CoalCoef%CH4Sched == 0) THEN
          CoalValue = Pollution%CoalCoef%CH4/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%CH4*GetCurrentScheduleValue(Pollution%CoalCoef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%CoalComp%CH4Pollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%CH4Pollution  = 0.0d0
        IF (Pollution%GasolineCoef%CH4Sched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%CH4/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%CH4*GetCurrentScheduleValue(Pollution%GasolineCoef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%CH4Pollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%CH4Pollution  = 0.0d0
        IF (Pollution%PropaneCoef%CH4Sched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%CH4/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%CH4*GetCurrentScheduleValue(Pollution%PropaneCoef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%CH4Pollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%CH4Pollution  = 0.0d0
        IF (Pollution%DieselCoef%CH4Sched == 0) THEN
          DieselValue = Pollution%DieselCoef%CH4/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%CH4*GetCurrentScheduleValue(Pollution%DieselCoef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%DieselComp%CH4Pollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%CH4Pollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%CH4Sched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%CH4/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%CH4*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%CH4Pollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%CH4Pollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%CH4Sched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%CH4/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%CH4*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%CH4Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%CH4Pollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      Pollution%CH4PollutTotal = Pollution%ElecComp%CH4Pollution +     &
                        Pollution%NatGasComp%CH4Pollution        +      &
                        Pollution%FuelOil1Comp%CH4Pollution      +      &
                        Pollution%FuelOil2Comp%CH4Pollution      +      &
                        Pollution%CoalComp%CH4Pollution          +      &
                        Pollution%GasolineComp%CH4Pollution      +      &
                        Pollution%PropaneComp%CH4Pollution       +      &
                        Pollution%DieselComp%CH4Pollution        +      &
                        Pollution%OtherFuel1Comp%CH4Pollution    +      &
                        Pollution%OtherFuel1Comp%CH4Pollution

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%COPollution     = 0.0d0
        IF (Pollution%ElecCoef%COSched == 0) THEN
          ElecValue = Pollution%ElecCoef%CO/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%CO*GetCurrentScheduleValue(Pollution%ElecCoef%COSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%COPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%COPollution   = 0.0d0
        IF (Pollution%NatGasCoef%COSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%CO/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%CO*GetCurrentScheduleValue(Pollution%NatGasCoef%COSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%COPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%COPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%COSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%CO/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%CO*GetCurrentScheduleValue(Pollution%FuelOil1Coef%COSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%COPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%COPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%COSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%CO/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%CO*GetCurrentScheduleValue(Pollution%FuelOil2Coef%COSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%COPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%COPollution  = 0.0d0
        IF (Pollution%CoalCoef%COSched == 0) THEN
          CoalValue = Pollution%CoalCoef%CO/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%CO*GetCurrentScheduleValue(Pollution%CoalCoef%COSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%COPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%COPollution  = 0.0d0
        IF (Pollution%GasolineCoef%COSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%CO/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%CO*GetCurrentScheduleValue(Pollution%GasolineCoef%COSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%COPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%COPollution  = 0.0d0
        IF (Pollution%PropaneCoef%COSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%CO/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%CO*GetCurrentScheduleValue(Pollution%PropaneCoef%COSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%COPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%COPollution  = 0.0d0
        IF (Pollution%DieselCoef%COSched == 0) THEN
          DieselValue = Pollution%DieselCoef%CO/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%CO*GetCurrentScheduleValue(Pollution%DieselCoef%COSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%COPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%COPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%COSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%CO/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%CO*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%COSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%COPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%COPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%COSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%CO/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%CO*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%COSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%COPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%N2OPollution     = 0.0d0
        IF (Pollution%ElecCoef%N2OSched == 0) THEN
          ElecValue = Pollution%ElecCoef%N2O/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%N2O*GetCurrentScheduleValue(Pollution%ElecCoef%N2OSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%N2OPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%N2OPollution   = 0.0d0
        IF (Pollution%NatGasCoef%N2OSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%N2O/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%N2O*GetCurrentScheduleValue(Pollution%NatGasCoef%N2OSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%N2OPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%N2OPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%N2OSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%N2O/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%N2O*GetCurrentScheduleValue(Pollution%FuelOil1Coef%N2OSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%N2OPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%N2OPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%N2OSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%N2O/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%N2O*GetCurrentScheduleValue(Pollution%FuelOil2Coef%N2OSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%N2OPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%N2OPollution  = 0.0d0
        IF (Pollution%CoalCoef%N2OSched == 0) THEN
          CoalValue = Pollution%CoalCoef%N2O/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%N2O*GetCurrentScheduleValue(Pollution%CoalCoef%N2OSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%N2OPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%N2OPollution  = 0.0d0
        IF (Pollution%GasolineCoef%N2OSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%N2O/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%N2O*GetCurrentScheduleValue(Pollution%GasolineCoef%N2OSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%N2OPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%N2OPollution  = 0.0d0
        IF (Pollution%PropaneCoef%N2OSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%N2O/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%N2O*GetCurrentScheduleValue(Pollution%PropaneCoef%N2OSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%N2OPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%N2OPollution  = 0.0d0
        IF (Pollution%DieselCoef%N2OSched == 0) THEN
          DieselValue = Pollution%DieselCoef%N2O/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%N2O*GetCurrentScheduleValue(Pollution%DieselCoef%N2OSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%N2OPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%N2OPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%N2OSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%N2O/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%N2O*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%N2OSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%N2OPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%N2OPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%N2OSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%N2O/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%N2O*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%N2OSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%N2OPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      Pollution%N2OPollutTotal = Pollution%ElecComp%N2OPollution +     &
                        Pollution%NatGasComp%N2OPollution        +      &
                        Pollution%FuelOil1Comp%N2OPollution      +      &
                        Pollution%FuelOil2Comp%N2OPollution      +      &
                        Pollution%CoalComp%N2OPollution          +      &
                        Pollution%GasolineComp%N2OPollution      +      &
                        Pollution%PropaneComp%N2OPollution       +      &
                        Pollution%DieselComp%N2OPollution        +      &
                        Pollution%OtherFuel1Comp%N2OPollution    +      &
                        Pollution%OtherFuel2Comp%N2OPollution

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%SO2Pollution     = 0.0d0
        IF (Pollution%ElecCoef%SO2Sched == 0) THEN
          ElecValue = Pollution%ElecCoef%SO2/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%SO2*GetCurrentScheduleValue(Pollution%ElecCoef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%ElecComp%SO2Pollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%SO2Pollution   = 0.0d0
        IF (Pollution%NatGasCoef%SO2Sched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%SO2/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%SO2*GetCurrentScheduleValue(Pollution%NatGasCoef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%SO2Pollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%SO2Pollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%SO2Sched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%SO2/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%SO2*GetCurrentScheduleValue(Pollution%FuelOil1Coef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%SO2Pollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%SO2Pollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%SO2Sched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%SO2/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%SO2*GetCurrentScheduleValue(Pollution%FuelOil2Coef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%SO2Pollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%SO2Pollution  = 0.0d0
        IF (Pollution%CoalCoef%SO2Sched == 0) THEN
          CoalValue = Pollution%CoalCoef%SO2/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%SO2*GetCurrentScheduleValue(Pollution%CoalCoef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%CoalComp%SO2Pollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%SO2Pollution  = 0.0d0
        IF (Pollution%GasolineCoef%SO2Sched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%SO2/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%SO2*GetCurrentScheduleValue(Pollution%GasolineCoef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%SO2Pollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%SO2Pollution  = 0.0d0
        IF (Pollution%PropaneCoef%SO2Sched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%SO2/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%SO2*GetCurrentScheduleValue(Pollution%PropaneCoef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%SO2Pollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%SO2Pollution  = 0.0d0
        IF (Pollution%DieselCoef%SO2Sched == 0) THEN
          DieselValue = Pollution%DieselCoef%SO2/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%SO2*GetCurrentScheduleValue(Pollution%DieselCoef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%DieselComp%SO2Pollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%SO2Pollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%SO2Sched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%SO2/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%SO2*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%SO2Pollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%SO2Pollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%SO2Sched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%SO2/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%SO2*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%SO2Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%SO2Pollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%PMPollution     = 0.0d0
        IF (Pollution%ElecCoef%PMSched == 0) THEN
          ElecValue = Pollution%ElecCoef%PM/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%PM*GetCurrentScheduleValue(Pollution%ElecCoef%PMSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%PMPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%PMPollution   = 0.0d0
        IF (Pollution%NatGasCoef%PMSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%PM/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%PM*GetCurrentScheduleValue(Pollution%NatGasCoef%PMSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%PMPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%PMPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%PMSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%PM/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%PM*GetCurrentScheduleValue(Pollution%FuelOil1Coef%PMSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%PMPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%PMPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%PMSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%PM/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%PM*GetCurrentScheduleValue(Pollution%FuelOil2Coef%PMSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%PMPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%PMPollution  = 0.0d0
        IF (Pollution%CoalCoef%PMSched == 0) THEN
          CoalValue = Pollution%CoalCoef%PM/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%PM*GetCurrentScheduleValue(Pollution%CoalCoef%PMSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%PMPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%PMPollution  = 0.0d0
        IF (Pollution%GasolineCoef%PMSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%PM/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%PM*GetCurrentScheduleValue(Pollution%GasolineCoef%PMSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%PMPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%PMPollution  = 0.0d0
        IF (Pollution%PropaneCoef%PMSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%PM/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%PM*GetCurrentScheduleValue(Pollution%PropaneCoef%PMSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%PMPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%PMPollution  = 0.0d0
        IF (Pollution%DieselCoef%PMSched == 0) THEN
          DieselValue = Pollution%DieselCoef%PM/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%PM*GetCurrentScheduleValue(Pollution%DieselCoef%PMSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%PMPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%PMPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%PMSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%PM/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%PM*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%PMSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%PMPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%PMPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%PMSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%PM/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%PM*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%PMSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%PMPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%PM10Pollution     = 0.0d0
        IF (Pollution%ElecCoef%PM10Sched == 0) THEN
          ElecValue = Pollution%ElecCoef%PM10/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%PM10*GetCurrentScheduleValue(Pollution%ElecCoef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%ElecComp%PM10Pollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%PM10Pollution   = 0.0d0
        IF (Pollution%NatGasCoef%PM10Sched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%PM10/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%PM10*GetCurrentScheduleValue(Pollution%NatGasCoef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%PM10Pollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%PM10Pollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%PM10Sched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%PM10/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%PM10*GetCurrentScheduleValue(Pollution%FuelOil1Coef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%PM10Pollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%PM10Pollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%PM10Sched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%PM10/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%PM10*GetCurrentScheduleValue(Pollution%FuelOil2Coef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%PM10Pollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%PM10Pollution  = 0.0d0
        IF (Pollution%CoalCoef%PM10Sched == 0) THEN
          CoalValue = Pollution%CoalCoef%PM10/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%PM10*GetCurrentScheduleValue(Pollution%CoalCoef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%CoalComp%PM10Pollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%PM10Pollution  = 0.0d0
        IF (Pollution%GasolineCoef%PM10Sched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%PM10/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%PM10*GetCurrentScheduleValue(Pollution%GasolineCoef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%PM10Pollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%PM10Pollution  = 0.0d0
        IF (Pollution%PropaneCoef%PM10Sched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%PM10/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%PM10*GetCurrentScheduleValue(Pollution%PropaneCoef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%PM10Pollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%PM10Pollution  = 0.0d0
        IF (Pollution%DieselCoef%PM10Sched == 0) THEN
          DieselValue = Pollution%DieselCoef%PM10/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%PM10*GetCurrentScheduleValue(Pollution%DieselCoef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%DieselComp%PM10Pollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%PM10Pollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%PM10Sched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%PM10/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%PM10*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%PM10Pollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%PM10Pollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%PM10Sched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%PM10/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%PM10*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%PM10Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%PM10Pollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%PM25Pollution     = 0.0d0
        IF (Pollution%ElecCoef%PM25Sched == 0) THEN
          ElecValue = Pollution%ElecCoef%PM25/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%PM25*GetCurrentScheduleValue(Pollution%ElecCoef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%ElecComp%PM25Pollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%PM25Pollution   = 0.0d0
        IF (Pollution%NatGasCoef%PM25Sched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%PM25/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%PM25*GetCurrentScheduleValue(Pollution%NatGasCoef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%PM25Pollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%PM25Pollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%PM25Sched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%PM25/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%PM25*GetCurrentScheduleValue(Pollution%FuelOil1Coef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%PM25Pollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%PM25Pollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%PM25Sched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%PM25/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%PM25*GetCurrentScheduleValue(Pollution%FuelOil2Coef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%PM25Pollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%PM25Pollution  = 0.0d0
        IF (Pollution%CoalCoef%PM25Sched == 0) THEN
          CoalValue = Pollution%CoalCoef%PM25/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%PM25*GetCurrentScheduleValue(Pollution%CoalCoef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%CoalComp%PM25Pollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%PM25Pollution  = 0.0d0
        IF (Pollution%GasolineCoef%PM25Sched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%PM25/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%PM25*GetCurrentScheduleValue(Pollution%GasolineCoef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%PM25Pollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%PM25Pollution  = 0.0d0
        IF (Pollution%PropaneCoef%PM25Sched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%PM25/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%PM25*GetCurrentScheduleValue(Pollution%PropaneCoef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%PM25Pollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%PM25Pollution  = 0.0d0
        IF (Pollution%DieselCoef%PM25Sched == 0) THEN
          DieselValue = Pollution%DieselCoef%PM25/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%PM25*GetCurrentScheduleValue(Pollution%DieselCoef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%DieselComp%PM25Pollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%PM25Pollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%PM25Sched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%PM25/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%PM25*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%PM25Pollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%PM25Pollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%PM25Sched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%PM25/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%PM25*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%PM25Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%PM25Pollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%NH3Pollution     = 0.0d0
        IF (Pollution%ElecCoef%NH3Sched == 0) THEN
          ElecValue = Pollution%ElecCoef%NH3/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%NH3*GetCurrentScheduleValue(Pollution%ElecCoef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%ElecComp%NH3Pollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%NH3Pollution   = 0.0d0
        IF (Pollution%NatGasCoef%NH3Sched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%NH3/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%NH3*GetCurrentScheduleValue(Pollution%NatGasCoef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%NH3Pollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%NH3Pollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%NH3Sched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%NH3/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%NH3*GetCurrentScheduleValue(Pollution%FuelOil1Coef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%NH3Pollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%NH3Pollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%NH3Sched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%NH3/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%NH3*GetCurrentScheduleValue(Pollution%FuelOil2Coef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%NH3Pollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%NH3Pollution  = 0.0d0
        IF (Pollution%CoalCoef%NH3Sched == 0) THEN
          CoalValue = Pollution%CoalCoef%NH3/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%NH3*GetCurrentScheduleValue(Pollution%CoalCoef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%CoalComp%NH3Pollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%NH3Pollution  = 0.0d0
        IF (Pollution%GasolineCoef%NH3Sched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%NH3/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%NH3*GetCurrentScheduleValue(Pollution%GasolineCoef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%NH3Pollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%NH3Pollution  = 0.0d0
        IF (Pollution%PropaneCoef%NH3Sched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%NH3/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%NH3*GetCurrentScheduleValue(Pollution%PropaneCoef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%NH3Pollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%NH3Pollution  = 0.0d0
        IF (Pollution%DieselCoef%NH3Sched == 0) THEN
          DieselValue = Pollution%DieselCoef%NH3/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%NH3*GetCurrentScheduleValue(Pollution%DieselCoef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%DieselComp%NH3Pollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%NH3Pollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%NH3Sched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%NH3/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%NH3*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%NH3Pollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%NH3Pollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%NH3Sched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%NH3/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%NH3*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%NH3Sched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%NH3Pollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%NMVOCPollution     = 0.0d0
        IF (Pollution%ElecCoef%NMVOCSched == 0) THEN
          ElecValue = Pollution%ElecCoef%NMVOC/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%NMVOC*GetCurrentScheduleValue(Pollution%ElecCoef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%NMVOCPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%NMVOCPollution   = 0.0d0
        IF (Pollution%NatGasCoef%NMVOCSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%NMVOC/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%NMVOC*GetCurrentScheduleValue(Pollution%NatGasCoef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%NMVOCPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%NMVOCPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%NMVOCSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%NMVOC/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%NMVOC*GetCurrentScheduleValue(Pollution%FuelOil1Coef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%NMVOCPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%NMVOCPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%NMVOCSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%NMVOC/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%NMVOC*GetCurrentScheduleValue(Pollution%FuelOil2Coef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%NMVOCPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%NMVOCPollution  = 0.0d0
        IF (Pollution%CoalCoef%NMVOCSched == 0) THEN
          CoalValue = Pollution%CoalCoef%NMVOC/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%NMVOC*GetCurrentScheduleValue(Pollution%CoalCoef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%NMVOCPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%NMVOCPollution  = 0.0d0
        IF (Pollution%GasolineCoef%NMVOCSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%NMVOC/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%NMVOC*GetCurrentScheduleValue(Pollution%GasolineCoef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%NMVOCPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%NMVOCPollution  = 0.0d0
        IF (Pollution%PropaneCoef%NMVOCSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%NMVOC/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%NMVOC*GetCurrentScheduleValue(Pollution%PropaneCoef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%NMVOCPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%NMVOCPollution  = 0.0d0
        IF (Pollution%DieselCoef%NMVOCSched == 0) THEN
          DieselValue = Pollution%DieselCoef%NMVOC/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%NMVOC*GetCurrentScheduleValue(Pollution%DieselCoef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%NMVOCPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%NMVOCPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%NMVOCSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%NMVOC/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%NMVOC*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%NMVOCPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%NMVOCPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%NMVOCSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%NMVOC/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%NMVOC*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%NMVOCSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%NMVOCPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%HgPollution     = 0.0d0
        IF (Pollution%ElecCoef%HgSched == 0) THEN
          ElecValue = Pollution%ElecCoef%Hg/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%Hg*GetCurrentScheduleValue(Pollution%ElecCoef%HgSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%HgPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%HgPollution   = 0.0d0
        IF (Pollution%NatGasCoef%HgSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%Hg/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%Hg*GetCurrentScheduleValue(Pollution%NatGasCoef%HgSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%HgPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%HgPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%HgSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%Hg/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%Hg*GetCurrentScheduleValue(Pollution%FuelOil1Coef%HgSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%HgPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%HgPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%HgSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%Hg/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%Hg*GetCurrentScheduleValue(Pollution%FuelOil2Coef%HgSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%HgPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%HgPollution  = 0.0d0
        IF (Pollution%CoalCoef%HgSched == 0) THEN
          CoalValue = Pollution%CoalCoef%Hg/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%Hg*GetCurrentScheduleValue(Pollution%CoalCoef%HgSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%HgPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%HgPollution  = 0.0d0
        IF (Pollution%GasolineCoef%HgSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%Hg/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%Hg*GetCurrentScheduleValue(Pollution%GasolineCoef%HgSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%HgPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%HgPollution  = 0.0d0
        IF (Pollution%PropaneCoef%HgSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%Hg/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%Hg*GetCurrentScheduleValue(Pollution%PropaneCoef%HgSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%HgPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%HgPollution  = 0.0d0
        IF (Pollution%DieselCoef%HgSched == 0) THEN
          DieselValue = Pollution%DieselCoef%Hg/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%Hg*GetCurrentScheduleValue(Pollution%DieselCoef%HgSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%HgPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%HgPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%HgSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%Hg/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%Hg*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%HgSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%HgPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%HgPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%HgSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%Hg/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%Hg*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%HgSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%HgPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%PbPollution     = 0.0d0
        IF (Pollution%ElecCoef%PbSched == 0) THEN
          ElecValue = Pollution%ElecCoef%Pb/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%Pb*GetCurrentScheduleValue(Pollution%ElecCoef%PbSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%PbPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%PbPollution   = 0.0d0
        IF (Pollution%NatGasCoef%PbSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%Pb/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%Pb*GetCurrentScheduleValue(Pollution%NatGasCoef%PbSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%PbPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%PbPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%PbSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%Pb/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%Pb*GetCurrentScheduleValue(Pollution%FuelOil1Coef%PbSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%PbPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%PbPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%PbSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%Pb/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%Pb*GetCurrentScheduleValue(Pollution%FuelOil2Coef%PbSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%PbPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%PbPollution  = 0.0d0
        IF (Pollution%CoalCoef%PbSched == 0) THEN
          CoalValue = Pollution%CoalCoef%Pb/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%Pb*GetCurrentScheduleValue(Pollution%CoalCoef%PbSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%PbPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%PbPollution  = 0.0d0
        IF (Pollution%GasolineCoef%PbSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%Pb/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%Pb*GetCurrentScheduleValue(Pollution%GasolineCoef%PbSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%PbPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%PbPollution  = 0.0d0
        IF (Pollution%PropaneCoef%PbSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%Pb/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%Pb*GetCurrentScheduleValue(Pollution%PropaneCoef%PbSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%PbPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%PbPollution  = 0.0d0
        IF (Pollution%DieselCoef%PbSched == 0) THEN
          DieselValue = Pollution%DieselCoef%Pb/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%Pb*GetCurrentScheduleValue(Pollution%DieselCoef%PbSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%PbPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%PbPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%PbSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%Pb/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%Pb*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%PbSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%PbPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%PbPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%PbSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%Pb/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%Pb*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%PbSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%PbPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%WaterPollution     = 0.0d0
        IF (Pollution%ElecCoef%WaterSched == 0) THEN
          ElecValue = Pollution%ElecCoef%Water
        ELSE
          ElecValue = Pollution%ElecCoef%Water*GetCurrentScheduleValue(Pollution%ElecCoef%WaterSched)
        ENDIF
        Pollution%ElecComp%WaterPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%WaterPollution   = 0.0d0
        IF (Pollution%NatGasCoef%WaterSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%Water
        ELSE
          NatGasValue = Pollution%NatGasCoef%Water*GetCurrentScheduleValue(Pollution%NatGasCoef%WaterSched)
        ENDIF
        Pollution%NatGasComp%WaterPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%WaterPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%WaterSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%Water
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%Water*GetCurrentScheduleValue(Pollution%FuelOil1Coef%WaterSched)
        ENDIF
        Pollution%FuelOil1Comp%WaterPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%WaterPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%WaterSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%Water
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%Water*GetCurrentScheduleValue(Pollution%FuelOil2Coef%WaterSched)
        ENDIF
        Pollution%FuelOil2Comp%WaterPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%WaterPollution  = 0.0d0
        IF (Pollution%CoalCoef%WaterSched == 0) THEN
          CoalValue = Pollution%CoalCoef%Water
        ELSE
          CoalValue = Pollution%CoalCoef%Water*GetCurrentScheduleValue(Pollution%CoalCoef%WaterSched)
        ENDIF
        Pollution%CoalComp%WaterPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%WaterPollution  = 0.0d0
        IF (Pollution%GasolineCoef%WaterSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%Water
        ELSE
          GasolineValue = Pollution%GasolineCoef%Water*GetCurrentScheduleValue(Pollution%GasolineCoef%WaterSched)
        ENDIF
        Pollution%GasolineComp%WaterPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%WaterPollution  = 0.0d0
        IF (Pollution%PropaneCoef%WaterSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%Water
        ELSE
          PropaneValue = Pollution%PropaneCoef%Water*GetCurrentScheduleValue(Pollution%PropaneCoef%WaterSched)
        ENDIF
        Pollution%PropaneComp%WaterPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%WaterPollution  = 0.0d0
        IF (Pollution%DieselCoef%WaterSched == 0) THEN
          DieselValue = Pollution%DieselCoef%Water
        ELSE
          DieselValue = Pollution%DieselCoef%Water*GetCurrentScheduleValue(Pollution%DieselCoef%WaterSched)
        ENDIF
        Pollution%DieselComp%WaterPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%WaterPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%WaterSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%Water
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%Water*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%WaterSched)
        ENDIF
        Pollution%OtherFuel1Comp%WaterPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%WaterPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%WaterSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%Water
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%Water*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%WaterSched)
        ENDIF
        Pollution%OtherFuel2Comp%WaterPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%NucHiPollution     = 0.0d0
        IF (Pollution%ElecCoef%NucHiSched == 0) THEN
          ElecValue = Pollution%ElecCoef%NucHi/1000.0d0
        ELSE
          ElecValue = Pollution%ElecCoef%NucHi*GetCurrentScheduleValue(Pollution%ElecCoef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%ElecComp%NucHiPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%NucHiPollution   = 0.0d0
        IF (Pollution%NatGasCoef%NucHiSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%NucHi/1000.0d0
        ELSE
          NatGasValue = Pollution%NatGasCoef%NucHi*GetCurrentScheduleValue(Pollution%NatGasCoef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%NatGasComp%NucHiPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%NucHiPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%NucHiSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%NucHi/1000.0d0
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%NucHi*GetCurrentScheduleValue(Pollution%FuelOil1Coef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%FuelOil1Comp%NucHiPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%NucHiPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%NucHiSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%NucHi/1000.0d0
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%NucHi*GetCurrentScheduleValue(Pollution%FuelOil2Coef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%FuelOil2Comp%NucHiPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%NucHiPollution  = 0.0d0
        IF (Pollution%CoalCoef%NucHiSched == 0) THEN
          CoalValue = Pollution%CoalCoef%NucHi/1000.0d0
        ELSE
          CoalValue = Pollution%CoalCoef%NucHi*GetCurrentScheduleValue(Pollution%CoalCoef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%CoalComp%NucHiPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%NucHiPollution  = 0.0d0
        IF (Pollution%GasolineCoef%NucHiSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%NucHi/1000.0d0
        ELSE
          GasolineValue = Pollution%GasolineCoef%NucHi*GetCurrentScheduleValue(Pollution%GasolineCoef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%GasolineComp%NucHiPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%NucHiPollution  = 0.0d0
        IF (Pollution%PropaneCoef%NucHiSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%NucHi/1000.0d0
        ELSE
          PropaneValue = Pollution%PropaneCoef%NucHi*GetCurrentScheduleValue(Pollution%PropaneCoef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%PropaneComp%NucHiPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%NucHiPollution  = 0.0d0
        IF (Pollution%DieselCoef%NucHiSched == 0) THEN
          DieselValue = Pollution%DieselCoef%NucHi/1000.0d0
        ELSE
          DieselValue = Pollution%DieselCoef%NucHi*GetCurrentScheduleValue(Pollution%DieselCoef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%DieselComp%NucHiPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%NucHiPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%NucHiSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%NucHi/1000.0d0
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%NucHi*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel1Comp%NucHiPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%NucHiPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%NucHiSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%NucHi/1000.0d0
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%NucHi*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%NucHiSched)/1000.0d0
        ENDIF
        Pollution%OtherFuel2Comp%NucHiPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%FuelFactorUsed) THEN
        Pollution%ElecComp%NucLoPollution     = 0.0d0
        IF (Pollution%ElecCoef%NucLoSched == 0) THEN
          ElecValue = Pollution%ElecCoef%NucLo
        ELSE
          ElecValue = Pollution%ElecCoef%NucLo*GetCurrentScheduleValue(Pollution%ElecCoef%NucLoSched)
        ENDIF
        Pollution%ElecComp%NucLoPollution     = (FuelType%Elec/1.0d6)*ElecValue
      ENDIF
      IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
        Pollution%NatGasComp%NucLoPollution   = 0.0d0
        IF (Pollution%NatGasCoef%NucLoSched == 0) THEN
          NatGasValue = Pollution%NatGasCoef%NucLo
        ELSE
          NatGasValue = Pollution%NatGasCoef%NucLo*GetCurrentScheduleValue(Pollution%NatGasCoef%NucLoSched)
        ENDIF
        Pollution%NatGasComp%NucLoPollution   = (FuelType%NatGas/1.0d6)*NatGasValue
      ENDIF
      IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
        Pollution%FuelOil1Comp%NucLoPollution  = 0.0d0
        IF (Pollution%FuelOil1Coef%NucLoSched == 0) THEN
          FuelOil1Value = Pollution%FuelOil1Coef%NucLo
        ELSE
          FuelOil1Value = Pollution%FuelOil1Coef%NucLo*GetCurrentScheduleValue(Pollution%FuelOil1Coef%NucLoSched)
        ENDIF
        Pollution%FuelOil1Comp%NucLoPollution  = (FuelType%FuelOil1/1.0d6)*FuelOil1Value
      ENDIF
      IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
        Pollution%FuelOil2Comp%NucLoPollution  = 0.0d0
        IF (Pollution%FuelOil2Coef%NucLoSched == 0) THEN
          FuelOil2Value = Pollution%FuelOil2Coef%NucLo
        ELSE
          FuelOil2Value = Pollution%FuelOil2Coef%NucLo*GetCurrentScheduleValue(Pollution%FuelOil2Coef%NucLoSched)
        ENDIF
        Pollution%FuelOil2Comp%NucLoPollution = (FuelType%FuelOil2/1.0d6)*FuelOil2Value
      ENDIF
      IF (Pollution%CoalCoef%FuelFactorUsed) THEN
        Pollution%CoalComp%NucLoPollution  = 0.0d0
        IF (Pollution%CoalCoef%NucLoSched == 0) THEN
          CoalValue = Pollution%CoalCoef%NucLo
        ELSE
          CoalValue = Pollution%CoalCoef%NucLo*GetCurrentScheduleValue(Pollution%CoalCoef%NucLoSched)
        ENDIF
        Pollution%CoalComp%NucLoPollution     = (FuelType%Coal/1.0d6)*CoalValue
      ENDIF
      IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
        Pollution%GasolineComp%NucLoPollution  = 0.0d0
        IF (Pollution%GasolineCoef%NucLoSched == 0) THEN
          GasolineValue = Pollution%GasolineCoef%NucLo
        ELSE
          GasolineValue = Pollution%GasolineCoef%NucLo*GetCurrentScheduleValue(Pollution%GasolineCoef%NucLoSched)
        ENDIF
        Pollution%GasolineComp%NucLoPollution      = (FuelType%Gasoline/1.0d6)*GasolineValue
      ENDIF
      IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
        Pollution%PropaneComp%NucLoPollution  = 0.0d0
        IF (Pollution%PropaneCoef%NucLoSched == 0) THEN
          PropaneValue = Pollution%PropaneCoef%NucLo
        ELSE
          PropaneValue = Pollution%PropaneCoef%NucLo*GetCurrentScheduleValue(Pollution%PropaneCoef%NucLoSched)
        ENDIF
        Pollution%PropaneComp%NucLoPollution     = (FuelType%Propane/1.0d6)*PropaneValue
      ENDIF
      IF (Pollution%DieselCoef%FuelFactorUsed) THEN
        Pollution%DieselComp%NucLoPollution  = 0.0d0
        IF (Pollution%DieselCoef%NucLoSched == 0) THEN
          DieselValue = Pollution%DieselCoef%NucLo
        ELSE
          DieselValue = Pollution%DieselCoef%NucLo*GetCurrentScheduleValue(Pollution%DieselCoef%NucLoSched)
        ENDIF
        Pollution%DieselComp%NucLoPollution   = (FuelType%Diesel/1.0d6)*DieselValue
      ENDIF
      IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel1Comp%NucLoPollution  = 0.0d0
        IF (Pollution%OtherFuel1Coef%NucLoSched == 0) THEN
          OtherFuel1Value = Pollution%OtherFuel1Coef%NucLo
        ELSE
          OtherFuel1Value = Pollution%OtherFuel1Coef%NucLo*GetCurrentScheduleValue(Pollution%OtherFuel1Coef%NucLoSched)
        ENDIF
        Pollution%OtherFuel1Comp%NucLoPollution   = (FuelType%OtherFuel1/1.0d6)*OtherFuel1Value
      ENDIF
      IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
        Pollution%OtherFuel2Comp%NucLoPollution  = 0.0d0
        IF (Pollution%OtherFuel2Coef%NucLoSched == 0) THEN
          OtherFuel2Value = Pollution%OtherFuel2Coef%NucLo
        ELSE
          OtherFuel2Value = Pollution%OtherFuel2Coef%NucLo*GetCurrentScheduleValue(Pollution%OtherFuel2Coef%NucLoSched)
        ENDIF
        Pollution%OtherFuel2Comp%NucLoPollution   = (FuelType%OtherFuel2/1.0d6)*OtherFuel2Value
      ENDIF

      Pollution%TotCarbonEquivFromN2O = Pollution%N2OPollutTotal*Pollution%CarbonEquivN2O
      Pollution%TotCarbonEquivFromCH4  = Pollution%CH4PollutTotal*Pollution%CarbonEquivCH4
      Pollution%TotCarbonEquivFromCO2 = Pollution%CO2PollutTotal*Pollution%CarbonEquivCO2

      ElecValue     = 0.0d0
      NatGasValue   = 0.0d0
      FuelOil1Value = 0.0d0
      FuelOil2Value = 0.0d0
      CoalValue     = 0.0d0
      GasolineValue = 0.0d0
      PropaneValue  = 0.0d0
      DieselValue   = 0.0d0
      OtherFuel1Value   = 0.0d0
      OtherFuel2Value   = 0.0d0

      IF (Pollution%ElecCoef%SourceSched .NE. 0) THEN
        Pollution%ElecComp%Source   = FuelType%Elec*Pollution%ElecCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%ElecCoef%SourceSched)
        Pollution%ElecPurchComp%Source   = FuelType%ElecPurch*Pollution%ElecCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%ElecCoef%SourceSched)
        Pollution%ElecSurplusSoldComp%Source   = FuelType%ElecSold*Pollution%ElecCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%ElecCoef%SourceSched)
      ELSE
        Pollution%ElecComp%Source   = FuelType%Elec*Pollution%ElecCoef%Source
        Pollution%ElecPurchComp%Source   = FuelType%ElecPurch*Pollution%ElecCoef%Source
        Pollution%ElecSurplusSoldComp%Source   = FuelType%ElecSold*Pollution%ElecCoef%Source
      END IF
      IF (Pollution%NatGasCoef%SourceSched .NE. 0) THEN
      ! does not include district heating or steam
        Pollution%NatGasComp%Source = FuelType%NatGasFacility*Pollution%NatGasCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%NatGasCoef%SourceSched)
      ELSE
        Pollution%NatGasComp%Source = FuelType%NatGasFacility*Pollution%NatGasCoef%Source
      END IF
      IF (Pollution%FuelOil1Coef%SourceSched .NE. 0) THEN
        Pollution%FuelOil1Comp%Source= FuelType%FuelOil1*Pollution%FuelOil1Coef%Source &
                                             * GetCurrentScheduleValue(Pollution%FuelOil1Coef%SourceSched)
      ELSE
        Pollution%FuelOil1Comp%Source= FuelType%FuelOil1*Pollution%FuelOil1Coef%Source
      END IF
      IF (Pollution%FuelOil2Coef%SourceSched .NE. 0) THEN
        Pollution%FuelOil2Comp%Source = FuelType%FuelOil2*Pollution%FuelOil2Coef%Source &
                                             * GetCurrentScheduleValue(Pollution%FuelOil2Coef%SourceSched)
      ELSE
        Pollution%FuelOil1Comp%Source = FuelType%FuelOil2*Pollution%FuelOil2Coef%Source
      END IF
      IF (Pollution%CoalCoef%SourceSched .NE. 0) THEN
        Pollution%CoalComp%Source   = FuelType%Coal*Pollution%CoalCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%CoalCoef%SourceSched)
      ELSE
        Pollution%CoalComp%Source   = FuelType%Coal*Pollution%CoalCoef%Source
      END IF
      IF (Pollution%GasolineCoef%SourceSched .NE. 0) THEN
        Pollution%GasolineComp%Source    = FuelType%Gasoline*Pollution%GasolineCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%GasolineCoef%SourceSched)
      ELSE
        Pollution%GasolineComp%Source    = FuelType%Gasoline*Pollution%GasolineCoef%Source
      END IF
      IF (Pollution%PropaneCoef%SourceSched .NE. 0) THEN
        Pollution%PropaneComp%Source   = FuelType%Propane*Pollution%PropaneCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%PropaneCoef%SourceSched)
      ELSE
        Pollution%PropaneComp%Source   = FuelType%Propane*Pollution%PropaneCoef%Source
      END IF
      IF (Pollution%DieselCoef%SourceSched .NE. 0) THEN
        Pollution%DieselComp%Source = FuelType%Diesel*Pollution%DieselCoef%Source &
                                             * GetCurrentScheduleValue(Pollution%DieselCoef%SourceSched)
      ELSE
        Pollution%DieselComp%Source = FuelType%Diesel*Pollution%DieselCoef%Source
      END IF
      IF (Pollution%OtherFuel1Coef%SourceSched .NE. 0) THEN
        Pollution%OtherFuel1Comp%Source = FuelType%OtherFuel1*Pollution%OtherFuel1Coef%Source &
                                             * GetCurrentScheduleValue(Pollution%OtherFuel1Coef%SourceSched)
      ELSE
        Pollution%OtherFuel1Comp%Source = FuelType%OtherFuel1*Pollution%OtherFuel1Coef%Source
      END IF
      IF (Pollution%OtherFuel2Coef%SourceSched .NE. 0) THEN
        Pollution%OtherFuel2Comp%Source = FuelType%OtherFuel2*Pollution%OtherFuel2Coef%Source &
                                             * GetCurrentScheduleValue(Pollution%OtherFuel2Coef%SourceSched)
      ELSE
        Pollution%OtherFuel2Comp%Source = FuelType%OtherFuel2*Pollution%OtherFuel2Coef%Source
      END IF


  Return
 END SUBROUTINE CalcPollution



 SUBROUTINE ReadEnergyMeters
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   1998
          !       MODIFIED       na
          !       RE-ENGINEERED  December 2003 RJL

          ! PURPOSE OF THIS SUBROUTINE:
          !       Read Energy Results from the meters
          ! This routine reads the meters for the energy used

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: FracTimeStepZone

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
    REAL(r64), external :: GetInstantMeterValue

    FuelType%ElecFacility      =GetInstantMeterValue(FuelType%ElecFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%ElecFacilityIndex,2)
    FuelType%DieselFacility    =GetInstantMeterValue(FuelType%DieselFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%DieselFacilityIndex,2)
    FuelType%PurchCoolFacility =GetInstantMeterValue(FuelType%PurchCoolFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%PurchCoolFacilityIndex,2)
    FuelType%PurchHeatFacility =GetInstantMeterValue(FuelType%PurchHeatFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%PurchHeatFacilityIndex,2)
    FuelType%NatGasFacility    =GetInstantMeterValue(FuelType%NatGasFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%NatGasFacilityIndex,2)
    FuelType%GasolineFacility  =GetInstantMeterValue(FuelType%GasolineFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%GasolineFacilityIndex,2)
    FuelType%CoalFacility      =GetInstantMeterValue(FuelType%CoalFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%CoalFacilityIndex,2)
    FuelType%FuelOil1Facility  =GetInstantMeterValue(FuelType%FuelOil1FacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%FuelOil1FacilityIndex,2)
    FuelType%FuelOil2Facility  =GetInstantMeterValue(FuelType%FuelOil2FacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%FuelOil2FacilityIndex,2)
    FuelType%PropaneFacility   =GetInstantMeterValue(FuelType%PropaneFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%PropaneFacilityIndex,2)
    FuelType%OtherFuel1Facility=GetInstantMeterValue(FuelType%OtherFuel1FacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%OtherFuel1FacilityIndex,2)
    FuelType%OtherFuel2Facility=GetInstantMeterValue(FuelType%OtherFuel2FacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%OtherFuel2FacilityIndex,2)
    FuelType%ElecProducedFacility =GetInstantMeterValue(FuelType%ElecProducedFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%ElecProducedFacilityIndex,2)
    FuelType%SteamFacility     =GetInstantMeterValue(FuelType%SteamFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%SteamFacilityIndex,2)
    FuelType%ElecPurchasedFacility =GetInstantMeterValue(FuelType%ElecPurchasedFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%ElecPurchasedFacilityIndex,2)
    FuelType%ElecSurplusSoldFacility =GetInstantMeterValue(FuelType%ElecSurplusSoldFacilityIndex,1)*FracTimeStepZone + &
                                GetInstantMeterValue(FuelType%ElecSurplusSoldFacilityIndex,2)

    !Now these fuel types have to be sorted and summed into categories that we have pollution factors for.
    !The Off-Site Electricity is the total needed by the facility minus the amount generated on-site.
    !The on-site pollutants will end up being other fuel types used by the generators.
    !If the difference between the 2 electric quantities is <0.0 then it will be zero for that time step.
    !We will also add the District Cooling here with a rough conversion from Energy using the User
    !defined COP.
    FuelType%Elec = FuelType%ElecFacility - FuelType%ElecProducedFacility +   &
                    FuelType%PurchCoolFacility/Pollution%PurchCoolCOP
    If(FuelType%Elec .le. 0.0d0) FuelType%Elec = 0.0d0

    !The Natural Gas fuel type will be summed from the meters with the District Heating using an efficiency.
    FuelType%NatGas = FuelType%NatGasFacility +    &
                      FuelType%PurchHeatFacility/Pollution%PurchHeatEffic  +  &
                      FuelType%SteamFacility/Pollution%SteamConvEffic

    !The Distillate Oil or Fuel Oil #1
    FuelType%FuelOil1 = FuelType%FuelOil1Facility

    !The Residual Oil fuel type will be summed with the Fuel Oils
    FuelType%FuelOil2 = FuelType%FuelOil2Facility

    !The Gasoline fuel type will be summed
    FuelType%Gasoline = FuelType%GasolineFacility

    !The Natural Gas fuel type will be summed with the Nat gas and Propane fuel types from the meters and the Purchased
    FuelType%Propane = FuelType%PropaneFacility

    !The Coal fuel type will be assigned Coal
    FuelType%Coal = FuelType%CoalFacility

    !The Diesel fuel type will be summed
    FuelType%Diesel = FuelType%DieselFacility

    !The OtherFuel1 fuel type will be summed
    FuelType%OtherFuel1 = FuelType%OtherFuel1Facility

    !The OtherFuel2 fuel type will be summed
    FuelType%OtherFuel2 = FuelType%OtherFuel2Facility

    FuelType%ElecPurch = FuelType%ElecPurchasedFacility

    FuelType%ElecSold = FuelType%ElecSurplusSoldFacility

    RETURN

 END SUBROUTINE ReadEnergyMeters

! *****************************************************************************
! Utility Routines to allow access to data inside this module.
! *****************************************************************************

SUBROUTINE GetFuelFactorInfo(fuelName,fuelFactorUsed,fuelSourceFactor,fuelFactorScheduleUsed,ffScheduleIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine allows access to data inside this module from other modules (specifically the
          ! output tabular reports.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: fuelName               ! input fuel name  (standard from Tabular reports)
  LOGICAL, INTENT(OUT)         :: fuelFactorUsed         ! return value true if user has entered this fuel
  REAL(r64), INTENT(OUT)       :: fuelSourceFactor       ! if used, the source factor
  LOGICAL, INTENT(OUT)         :: fuelFactorScheduleUsed ! if true, schedules for this fuel are used
  INTEGER, INTENT(OUT)         :: ffScheduleIndex        ! if schedules for this fuel are used, return schedule index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    IF (GetInputFlagPollution) THEN
      CALL GetPollutionFactorInput
      GetInputFlagPollution=.false.
    ENDIF

    fuelFactorUsed=.false.
    fuelSourceFactor=0.0d0
    fuelFactorScheduleUsed=.false.
    ffScheduleIndex = 0

    SELECT CASE(fuelName)

      CASE ('NaturalGas', 'Gas')
        IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%NatGasCoef%Source
          IF (Pollution%NatGasCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%NatGasCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.084d0
        ENDIF

      CASE ('Electricity')
        IF (Pollution%ElecCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%ElecCoef%Source
          IF (Pollution%ElecCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%ElecCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 3.167d0
        ENDIF

     CASE ('ResidualOil', 'FuelOil#2')
        IF (Pollution%FuelOil2Coef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%FuelOil2Coef%Source
          IF (Pollution%FuelOil2Coef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%FuelOil2Coef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.05d0
        ENDIF

     CASE ('DistillateOil', 'FuelOil#1')
        IF (Pollution%FuelOil1Coef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%FuelOil1Coef%Source
          IF (Pollution%FuelOil1Coef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%FuelOil1Coef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.05d0
        ENDIF

      CASE ('Coal')
        IF (Pollution%CoalCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%CoalCoef%Source
          IF (Pollution%CoalCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%CoalCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.05d0
        ENDIF

      CASE ('Gasoline')
        IF (Pollution%GasolineCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%GasolineCoef%Source
          IF (Pollution%GasolineCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%GasolineCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.05d0
        ENDIF

      CASE ('Propane')
        IF (Pollution%PropaneCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%PropaneCoef%Source
          IF (Pollution%PropaneCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%PropaneCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.05d0
        ENDIF

      CASE ('Diesel')
        IF (Pollution%DieselCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%DieselCoef%Source
          IF (Pollution%DieselCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%DieselCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.05d0
        ENDIF

      CASE ('OtherFuel1')
        IF (Pollution%OtherFuel1Coef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%OtherFuel1Coef%Source
          IF (Pollution%OtherFuel1Coef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%OtherFuel1Coef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.0d0
        ENDIF

      CASE ('OtherFuel2')
        IF (Pollution%OtherFuel2Coef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%OtherFuel2Coef%Source
          IF (Pollution%OtherFuel2Coef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%OtherFuel2Coef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.0d0
        ENDIF

      CASE ('DistrictHeating')
        IF (Pollution%NatGasCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%NatGasCoef%Source / Pollution%PurchHeatEffic
          IF (Pollution%NatGasCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%NatGasCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 1.084d0 / Pollution%PurchHeatEffic
        ENDIF

      CASE ('DistrictCooling')
        IF (Pollution%ElecCoef%FuelFactorUsed) THEN
          fuelFactorUsed=.true.
          fuelSourceFactor=Pollution%ElecCoef%Source / Pollution%PurchCoolCOP
          IF (Pollution%ElecCoef%SourceSched == 0) THEN
            fuelFactorScheduleUsed=.false.
          ELSE
            fuelFactorScheduleUsed=.true.
            ffScheduleIndex = Pollution%ElecCoef%SourceSched
          ENDIF
        ELSE
          fuelSourceFactor = 3.167d0 / Pollution%PurchCoolCOP
        ENDIF

      CASE ('Steam')
        fuelSourceFactor = 0.3d0

      CASE DEFAULT
    END SELECT

  RETURN

END SUBROUTINE GetFuelFactorInfo

SUBROUTINE GetEnvironmentalImpactFactorInfo(efficiencyDistrictHeating,efficiencyDistrictCooling,sourceFactorSteam)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine allows access to data inside this module from other modules (specifically the
          ! output tabular reports.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! Each of the arguments must be entered in the EnvironmentalImpactFactors object
  REAL(r64), INTENT(INOUT) :: efficiencyDistrictHeating    ! if entered, the efficiency of District Heating
  REAL(r64), INTENT(INOUT) :: efficiencyDistrictCooling    ! if entered, the efficiency of District Cooling
  REAL(r64), INTENT(INOUT) :: sourceFactorSteam            ! if entered, the source factor for Steam

    IF (GetInputFlagPollution) THEN
      CALL GetPollutionFactorInput
      GetInputFlagPollution=.false.
    ENDIF

  IF (NumEnvImpactFactors > 0) THEN
    efficiencyDistrictHeating = Pollution%PurchHeatEffic
    efficiencyDistrictCooling = Pollution%PurchCoolCOP
    sourceFactorSteam = Pollution%SteamConvEffic
  ENDIF

  RETURN

END SUBROUTINE GetEnvironmentalImpactFactorInfo

! *****************************************************************************
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
End Module PollutionModule

