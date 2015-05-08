MODULE DataGlobalConstants

          ! Module containing the data constants for components, meters, etc throughout
          ! EnergyPlus

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Provide a central storage place for various constants and their "integer equivalents"
          ! used throughout EnergyPlus.  Integer equivalents are needed for efficiency in run time.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Data only module -- public data

          ! MODULE PARAMETER DEFINITIONS:
! End Use Parameters
INTEGER, PARAMETER :: NumEndUses = 14

INTEGER, PARAMETER :: endUseHeating = 1
INTEGER, PARAMETER :: endUseCooling = 2
INTEGER, PARAMETER :: endUseInteriorLights = 3
INTEGER, PARAMETER :: endUseExteriorLights = 4
INTEGER, PARAMETER :: endUseInteriorEquipment = 5
INTEGER, PARAMETER :: endUseExteriorEquipment = 6
INTEGER, PARAMETER :: endUseFans = 7
INTEGER, PARAMETER :: endUsePumps = 8
INTEGER, PARAMETER :: endUseHeatRejection = 9
INTEGER, PARAMETER :: endUseHumidification = 10
INTEGER, PARAMETER :: endUseHeatRecovery = 11
INTEGER, PARAMETER :: endUseWaterSystem = 12
INTEGER, PARAMETER :: endUseRefrigeration = 13
INTEGER, PARAMETER :: endUseCogeneration = 14

! Resource Types
INTEGER, PARAMETER  :: istrLeng=60
CHARACTER(len=istrLeng), PARAMETER :: cRT_None='None'
CHARACTER(len=istrLeng), PARAMETER :: cRT_NoneUC='NONE'
INTEGER, PARAMETER          :: iRT_None=1000
CHARACTER(len=istrLeng), PARAMETER :: cRT_Electricity='Electricity'
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityUC='ELECTRICITY'
INTEGER, PARAMETER          :: iRT_Electricity=1001
CHARACTER(len=istrLeng), PARAMETER :: cRT_Natural_Gas='NaturalGas'
CHARACTER(len=istrLeng), PARAMETER :: cRT_Natural_GasUC='NATURALGAS'
INTEGER, PARAMETER          :: iRT_Natural_Gas=1002
CHARACTER(len=istrLeng), PARAMETER :: cRT_Gas='Gas'
CHARACTER(len=istrLeng), PARAMETER :: cRT_GasUC='GAS'
INTEGER, PARAMETER          :: iRT_Gas=1002
CHARACTER(len=istrLeng), PARAMETER :: cRT_Gasoline='Gasoline'
CHARACTER(len=istrLeng), PARAMETER :: cRT_GasolineUC='GASOLINE'
INTEGER, PARAMETER          :: iRT_Gasoline=1003
CHARACTER(len=istrLeng), PARAMETER :: cRT_Diesel='Diesel'
CHARACTER(len=istrLeng), PARAMETER :: cRT_DieselUC='DIESEL'
INTEGER, PARAMETER          :: iRT_Diesel=1004
CHARACTER(len=istrLeng), PARAMETER :: cRT_Coal='Coal'
CHARACTER(len=istrLeng), PARAMETER :: cRT_CoalUC='COAL'
INTEGER, PARAMETER          :: iRT_Coal=1005
CHARACTER(len=istrLeng), PARAMETER :: cRT_FuelOil_1='FuelOil#1'
CHARACTER(len=istrLeng), PARAMETER :: cRT_FuelOil_1UC='FUELOIL#1'
INTEGER, PARAMETER          :: iRT_FuelOil_1=1006
CHARACTER(len=istrLeng), PARAMETER :: cRT_DistillateOil='DistillateOil'
CHARACTER(len=istrLeng), PARAMETER :: cRT_DistillateOilUC='DISTILLATEOIL'
INTEGER, PARAMETER          :: iRT_DistillateOil=1006
CHARACTER(len=istrLeng), PARAMETER :: cRT_FuelOil_2='FuelOil#2'
CHARACTER(len=istrLeng), PARAMETER :: cRT_FuelOil_2UC='FUELOIL#2'
INTEGER, PARAMETER          :: iRT_FuelOil_2=1007
CHARACTER(len=istrLeng), PARAMETER :: cRT_ResidualOil='ResidualOil'
CHARACTER(len=istrLeng), PARAMETER :: cRT_ResidualOilUC='RESIDUALOIL'
INTEGER, PARAMETER          :: iRT_ResidualOil=1007
CHARACTER(len=istrLeng), PARAMETER :: cRT_Propane='Propane'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PropaneUC='PROPANE'
INTEGER, PARAMETER          :: iRT_Propane=1008
CHARACTER(len=istrLeng), PARAMETER :: cRT_LPG='LPG'
CHARACTER(len=istrLeng), PARAMETER :: cRT_LPGUC='LPG'
INTEGER, PARAMETER          :: iRT_LPG=1008
CHARACTER(len=istrLeng), PARAMETER :: cRT_Water='Water'
CHARACTER(len=istrLeng), PARAMETER :: cRT_WaterUC='WATER'
INTEGER, PARAMETER          :: iRT_Water=1009
CHARACTER(len=istrLeng), PARAMETER :: cRT_EnergyTransfer='EnergyTransfer'
CHARACTER(len=istrLeng), PARAMETER :: cRT_EnergyTransferUC='ENERGYTRANSFER'
INTEGER, PARAMETER          :: iRT_EnergyTransfer=1010
CHARACTER(len=istrLeng), PARAMETER :: cRT_Steam='Steam'
CHARACTER(len=istrLeng), PARAMETER :: cRT_SteamUC='STEAM'
INTEGER, PARAMETER          :: iRT_Steam=1011
CHARACTER(len=istrLeng), PARAMETER :: cRT_DistrictCooling='DistrictCooling'
CHARACTER(len=istrLeng), PARAMETER :: cRT_DistrictCoolingUC='DISTRICTCOOLING'
INTEGER, PARAMETER          :: iRT_DistrictCooling=1012
CHARACTER(len=istrLeng), PARAMETER :: cRT_DistrictHeating='DistrictHeating'
CHARACTER(len=istrLeng), PARAMETER :: cRT_DistrictHeatingUC='DISTRICTHEATING'
INTEGER, PARAMETER          :: iRT_DistrictHeating=1013
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityProduced='ElectricityProduced'
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityProducedUC='ELECTRICITYPRODUCED'
INTEGER, PARAMETER          :: iRT_ElectricityProduced=1014
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityPurchased='ElectricityPurchased'
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityPurchasedUC='ELECTRICITYPURCHASED'
INTEGER, PARAMETER          :: iRT_ElectricityPurchased=1015
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricitySurplusSold='ElectricitySurplusSold'
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricitySurplusSoldUC='ELECTRICITYSURPLUSSOLD'
INTEGER, PARAMETER          :: iRT_ElectricitySurplusSold=1016
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityNet='ElectricityNet'
CHARACTER(len=istrLeng), PARAMETER :: cRT_ElectricityNetUC='ELECTRICITYNET'
INTEGER, PARAMETER          :: iRT_ElectricityNet=1017
CHARACTER(len=istrLeng), PARAMETER :: cRT_SolarWater='SolarWater'
CHARACTER(len=istrLeng), PARAMETER :: cRT_SolarWaterUC='SOLARWATER'
INTEGER, PARAMETER          :: iRT_SolarWater=1018
CHARACTER(len=istrLeng), PARAMETER :: cRT_SolarAir='SolarAir'
CHARACTER(len=istrLeng), PARAMETER :: cRT_SolarAirUC='SOLARAIR'
INTEGER, PARAMETER          :: iRT_SolarAir=1019
CHARACTER(len=istrLeng), PARAMETER :: cRT_SO2='SO2'
CHARACTER(len=istrLeng), PARAMETER :: cRT_SO2UC='SO2'
INTEGER, PARAMETER          :: iRT_SO2=1020
CHARACTER(len=istrLeng), PARAMETER :: cRT_NOx='NOx'
CHARACTER(len=istrLeng), PARAMETER :: cRT_NOxUC='NOX'
INTEGER, PARAMETER          :: iRT_NOx=1021
CHARACTER(len=istrLeng), PARAMETER :: cRT_N2O='N2O'
CHARACTER(len=istrLeng), PARAMETER :: cRT_N2OUC='N2O'
INTEGER, PARAMETER          :: iRT_N2O=1022
CHARACTER(len=istrLeng), PARAMETER :: cRT_PM='PM'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PMUC='PM'
INTEGER, PARAMETER          :: iRT_PM=1023
CHARACTER(len=istrLeng), PARAMETER :: cRT_PM2_5='PM2.5'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PM2_5UC='PM2.5'
INTEGER, PARAMETER          :: iRT_PM2_5=1024
CHARACTER(len=istrLeng), PARAMETER :: cRT_PM10='PM10'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PM10UC='PM10'
INTEGER, PARAMETER          :: iRT_PM10=1025
CHARACTER(len=istrLeng), PARAMETER :: cRT_CO='CO'
CHARACTER(len=istrLeng), PARAMETER :: cRT_COUC='CO'
INTEGER, PARAMETER          :: iRT_CO=1026
CHARACTER(len=istrLeng), PARAMETER :: cRT_CO2='CO2'
CHARACTER(len=istrLeng), PARAMETER :: cRT_CO2UC='CO2'
INTEGER, PARAMETER          :: iRT_CO2=1027
CHARACTER(len=istrLeng), PARAMETER :: cRT_CH4='CH4'
CHARACTER(len=istrLeng), PARAMETER :: cRT_CH4UC='CH4'
INTEGER, PARAMETER          :: iRT_CH4=1028
CHARACTER(len=istrLeng), PARAMETER :: cRT_NH3='NH3'
CHARACTER(len=istrLeng), PARAMETER :: cRT_NH3UC='NH3'
INTEGER, PARAMETER          :: iRT_NH3=1029
CHARACTER(len=istrLeng), PARAMETER :: cRT_NMVOC='NMVOC'
CHARACTER(len=istrLeng), PARAMETER :: cRT_NMVOCUC='NMVOC'
INTEGER, PARAMETER          :: iRT_NMVOC=1030
CHARACTER(len=istrLeng), PARAMETER :: cRT_Hg='Hg'
CHARACTER(len=istrLeng), PARAMETER :: cRT_HgUC='HG'
INTEGER, PARAMETER          :: iRT_Hg=1031
CHARACTER(len=istrLeng), PARAMETER :: cRT_Pb='Pb'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PbUC='PB'
INTEGER, PARAMETER          :: iRT_Pb=1032
CHARACTER(len=istrLeng), PARAMETER :: cRT_NuclearHigh='NuclearHigh'
CHARACTER(len=istrLeng), PARAMETER :: cRT_NuclearHighUC='NUCLEARHIGH'
INTEGER, PARAMETER          :: iRT_NuclearHigh=1033
CHARACTER(len=istrLeng), PARAMETER :: cRT_NuclearLow='NuclearLow'
CHARACTER(len=istrLeng), PARAMETER :: cRT_NuclearLowUC='NUCLEARLOW'
INTEGER, PARAMETER          :: iRT_NuclearLow=1034
CHARACTER(len=istrLeng), PARAMETER :: cRT_WaterEnvironmentalFactors='WaterEnvironmentalFactors'
CHARACTER(len=istrLeng), PARAMETER :: cRT_WaterEnvironmentalFactorsUC='WATERENVIRONMENTALFACTORS'
INTEGER, PARAMETER          :: iRT_WaterEnvironmentalFactors=1035
CHARACTER(len=istrLeng), PARAMETER :: cRT_CarbonEquivalent='Carbon Equivalent'
CHARACTER(len=istrLeng), PARAMETER :: cRT_CarbonEquivalentUC='CARBON EQUIVALENT'
INTEGER, PARAMETER          :: iRT_CarbonEquivalent=1036
CHARACTER(len=istrLeng), PARAMETER :: cRT_Source='Source'
CHARACTER(len=istrLeng), PARAMETER :: cRT_SourceUC='SOURCE'
INTEGER, PARAMETER          :: iRT_Source=1037
CHARACTER(len=istrLeng), PARAMETER :: cRT_PlantLoopHeatingDemand='PlantLoopHeatingDemand'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PlantLoopHeatingDemandUC='PLANTLOOPHEATINGDEMAND'
INTEGER, PARAMETER          :: iRT_PlantLoopHeatingDemand=1038
CHARACTER(len=istrLeng), PARAMETER :: cRT_PlantLoopCoolingDemand='PlantLoopCoolingDemand'
CHARACTER(len=istrLeng), PARAMETER :: cRT_PlantLoopCoolingDemandUC='PLANTLOOPCOOLINGDEMAND'
INTEGER, PARAMETER          :: iRT_PlantLoopCoolingDemand=1039
CHARACTER(len=istrLeng), PARAMETER :: cRT_OnSiteWater='OnSiteWater'
CHARACTER(len=istrLeng), PARAMETER :: cRT_OnSiteWaterUC='ONSITEWATER'
INTEGER, PARAMETER          :: iRT_OnSiteWater=1040
CHARACTER(len=istrLeng), PARAMETER :: cRT_MainsWater='MainsWater'
CHARACTER(len=istrLeng), PARAMETER :: cRT_MainsWaterUC='MAINSWATER'
INTEGER, PARAMETER          :: iRT_MainsWater=1041
CHARACTER(len=istrLeng), PARAMETER :: cRT_RainWater='RainWater'
CHARACTER(len=istrLeng), PARAMETER :: cRT_RainWaterUC='RAINWATER'
INTEGER, PARAMETER          :: iRT_RainWater=1042
CHARACTER(len=istrLeng), PARAMETER :: cRT_WellWater='WellWater'
CHARACTER(len=istrLeng), PARAMETER :: cRT_WellWaterUC='WellWATER'
INTEGER, PARAMETER          :: iRT_WellWater=1043
CHARACTER(len=istrLeng), PARAMETER :: cRT_Condensate='Condensate'
CHARACTER(len=istrLeng), PARAMETER :: cRT_CondensateUC='CONDENSATE'
INTEGER, PARAMETER          :: iRT_Condensate=1044
CHARACTER(len=istrLeng), PARAMETER :: cRT_OtherFuel1='OtherFuel1'
CHARACTER(len=istrLeng), PARAMETER :: cRT_OtherFuel1UC='OTHERFUEL1'
INTEGER, PARAMETER          :: iRT_OtherFuel1=1045
CHARACTER(len=istrLeng), PARAMETER :: cRT_OtherFuel2='OtherFuel2'
CHARACTER(len=istrLeng), PARAMETER :: cRT_OtherFuel2UC='OTHERFUEL2'
INTEGER, PARAMETER          :: iRT_OtherFuel2=1046
INTEGER, PARAMETER          :: NumOfResourceTypes=46
INTEGER, PARAMETER          :: ResourceTypeInitialOffset=1000   ! to reach "ValidTypes"
CHARACTER(len=istrLeng), PARAMETER, DIMENSION(0:NumOfResourceTypes) :: cRT_ValidTypes = (/  &
               cRT_None,  &
               cRT_Electricity, &
               cRT_Gas, &
               cRT_Gasoline, &
               cRT_Diesel, &
               cRT_Coal, &
               cRT_FuelOil_1, &
               cRT_FuelOil_2, &
               cRT_Propane, &
               cRT_Water, &
               cRT_EnergyTransfer, &
               cRT_Steam, &
               cRT_DistrictCooling, &
               cRT_DistrictHeating, &
               cRT_ElectricityProduced, &
               cRT_ElectricityPurchased, &
               cRT_ElectricitySurplusSold, &
               cRT_ElectricityNet, &
               cRT_SolarWater, &
               cRT_SolarAir, &
               cRT_SO2, &
               cRT_NOx, &
               cRT_N2O, &
               cRT_PM, &
               cRT_PM2_5, &
               cRT_PM10, &
               cRT_CO, &
               cRT_CO2, &
               cRT_CH4, &
               cRT_NH3, &
               cRT_NMVOC, &
               cRT_Hg, &
               cRT_Pb, &
               cRT_NuclearHigh, &
               cRT_NuclearLow, &
               cRT_WaterEnvironmentalFactors, &
               cRT_CarbonEquivalent, &
               cRT_Source, &
               cRT_PlantLoopHeatingDemand, &
               cRT_PlantLoopCoolingDemand, &
               cRT_OnSiteWater, &
               cRT_MainsWater, &
               cRT_RainWater, &
               cRT_WellWater, &
               cRT_Condensate,  &
               cRT_OtherFuel1,  &
               cRT_OtherFuel2/)

 INTEGER, PARAMETER :: iGeneratorICEngine     =1
 INTEGER, PARAMETER :: iGeneratorCombTurbine  =2
 INTEGER, PARAMETER :: iGeneratorPV           =3
 INTEGER, PARAMETER :: iGeneratorFuelCell     =4
 INTEGER, PARAMETER :: iGeneratorMicroCHP     =5
 INTEGER, PARAMETER :: iGeneratorMicroturbine =6
 INTEGER, PARAMETER :: iGeneratorWindTurbine  =7


 INTEGER, PARAMETER :: iEvapCoolerDirectCELDEKPAD       = 1001
 INTEGER, PARAMETER :: iEvapCoolerInDirectCELDEKPAD     = 1002
 INTEGER, PARAMETER :: iEvapCoolerInDirectWETCOIL       = 1003
 INTEGER, PARAMETER :: iEvapCoolerInDirectRDDSpecial    = 1004
 INTEGER, PARAMETER :: iEvapCoolerDirectResearchSpecial = 1005

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE DataGlobalConstants
PUBLIC  AssignResourceTypeNum
PUBLIC  GetResourceTypeChar

CONTAINS

FUNCTION AssignResourceTypeNum(ResourceTypeChar) RESULT(ResourceTypeNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Assists in assigning proper numeric resource types to data structures.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ResourceTypeChar
  INTEGER :: ResourceTypeNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ResourceTypeNum=0

  SELECT CASE (MakeUPPERCase(ResourceTypeChar))

  CASE ('ELECTRICITY', 'ELECTRIC')
    ResourceTypeNum=iRT_Electricity

  CASE ('GAS','NATURALGAS')
    ResourceTypeNum=iRT_Gas

  CASE ('GASOLINE')
    ResourceTypeNum=iRT_Gasoline

  CASE ('DIESEL')
    ResourceTypeNum=iRT_Diesel

  CASE ('COAL')
    ResourceTypeNum=iRT_Coal

  CASE ('FUELOIL#1','DISTILLATE OIL')
    ResourceTypeNum=iRT_FuelOil_1

  CASE ('FUELOIL#2','RESIDUAL OIL')
    ResourceTypeNum=iRT_FuelOil_2

  CASE ('PROPANE','LPG')
    ResourceTypeNum=iRT_Propane

  CASE ('OTHERFUEL1')
    ResourceTypeNum=iRT_OtherFuel1

  CASE ('OTHERFUEL2')
    ResourceTypeNum=iRT_OtherFuel2

  CASE ('WATER','H2O')
    ResourceTypeNum=iRT_Water  ! use record keeping

  CASE ('ONSITEWATER', 'WATERPRODUCED', 'ONSITE WATER')
    ResourceTypeNum=iRT_OnSiteWater ! these are for supply record keeping

  CASE ('MAINSWATER', 'WATERSUPPLY')
    ResourceTypeNum=iRT_MainsWater ! record keeping

  CASE ('RAINWATER', 'PRECIPITATION')
    ResourceTypeNum=iRT_RainWater ! record keeping

  CASE ('WELLWATER', 'Groundwater')
    ResourceTypeNum=iRT_WellWater ! record keeping

  CASE ('CONDENSATE')
    ResourceTypeNum=  iRT_Condensate

  CASE ('ENERGYTRANSFER')
    ResourceTypeNum=iRT_EnergyTransfer

  CASE ('STEAM')
    ResourceTypeNum=iRT_Steam

  CASE ('DISTRICTCOOLING')
    ResourceTypeNum=iRT_DistrictCooling

  CASE ('DISTRICTHEATING')
    ResourceTypeNum=iRT_DistrictHeating

  CASE ('ELECTRICITYPRODUCED')
    ResourceTypeNum=iRT_ElectricityProduced

  CASE ('ELECTRICITYPURCHASED')
    ResourceTypeNum=iRT_ElectricityPurchased

  CASE ('ELECTRICITYSURPLUSSOLD')
    ResourceTypeNum=iRT_ElectricitySurplusSold

  CASE ('ELECTRICITYNET')
    ResourceTypeNum=iRT_ElectricityNet

  CASE ('SOLARWATER')
    ResourceTypeNum=iRT_SolarWater

  CASE ('SOLARAIR')
    ResourceTypeNum=iRT_SolarAir

  CASE ('SO2')
    ResourceTypeNum=iRT_SO2

  CASE ('NOX')
    ResourceTypeNum=iRT_NOx

  CASE ('N2O')
    ResourceTypeNum=iRT_N2O

  CASE ('PM')
    ResourceTypeNum=iRT_PM

  CASE ('PM2.5')
    ResourceTypeNum=iRT_PM2_5

  CASE ('PM10')
    ResourceTypeNum=iRT_PM10

  CASE ('CO')
    ResourceTypeNum=iRT_CO

  CASE ('CO2')
    ResourceTypeNum=iRT_CO2

  CASE ('CH4')
    ResourceTypeNum=iRT_CH4

  CASE ('NH3')
    ResourceTypeNum=iRT_NH3

  CASE ('NMVOC')
    ResourceTypeNum=iRT_NMVOC

  CASE ('HG')
    ResourceTypeNum=iRT_Hg

  CASE ('PB')
    ResourceTypeNum=iRT_Pb

  CASE ('NUCLEAR HIGH')
    ResourceTypeNum=iRT_NuclearHigh

  CASE ('NUCLEAR LOW')
    ResourceTypeNum=iRT_NuclearLow

  CASE ('WATERENVIRONMENTALFACTORS')
    ResourceTypeNum=iRT_WaterEnvironmentalFactors

  CASE ('CARBON EQUIVALENT')
    ResourceTypeNum=iRT_CarbonEquivalent

  CASE ('SOURCE')
    ResourceTypeNum=iRT_Source

  CASE ('PLANTLOOPHEATINGDEMAND')
    ResourceTypeNum=iRT_PlantLoopHeatingDemand

  CASE ('PLANTLOOPCOOLINGDEMAND')
    ResourceTypeNum=iRT_PlantLoopCoolingDemand

  CASE DEFAULT
    ResourceTypeNum=0

  END SELECT

  RETURN

END FUNCTION AssignResourceTypeNum

FUNCTION GetResourceTypeChar(ResourceTypeNum) RESULT(ResourceTypeChar)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Shows the resource type character string, given the resource type numeric.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ResourceTypeNum
  CHARACTER(len=MaxNameLength) :: ResourceTypeChar
          ! na

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  SELECT CASE (ResourceTypeNum)

  CASE (iRT_Electricity)
    ResourceTypeChar='Electricity'

  CASE (iRT_Gas)
    ResourceTypeChar='Gas'

  CASE (iRT_Gasoline)
    ResourceTypeChar='Gasoline'

  CASE (iRT_Diesel)
    ResourceTypeChar='Diesel'

  CASE (iRT_Coal)
    ResourceTypeChar='Coal'

  CASE (iRT_FuelOil_1)
    ResourceTypeChar='FuelOil#1'

  CASE (iRT_FuelOil_2)
    ResourceTypeChar='FuelOil#2'

  CASE (iRT_Propane)
    ResourceTypeChar='Propane'

  CASE (iRT_OtherFuel1)
    ResourceTypeChar='OtherFuel1'

  CASE (iRT_OtherFuel2)
    ResourceTypeChar='OtherFuel2'

  CASE (iRT_Water)
    ResourceTypeChar='Water'

  CASE (iRT_OnSiteWater)
    ResourceTypeChar='OnSiteWater'

  CASE (iRT_MainsWater)
    ResourceTypeChar='MainsWater'

  CASE (iRT_RainWater)
    ResourceTypeChar='RainWater'

  CASE (iRT_Condensate)
    ResourceTypeChar='Condensate'

  CASE (iRT_WellWater)
    ResourceTypeChar='WellWater'

  CASE (iRT_EnergyTransfer)
    ResourceTypeChar='EnergyTransfer'

  CASE (iRT_Steam)
    ResourceTypeChar='Steam'

  CASE (iRT_DistrictCooling)
    ResourceTypeChar='DistrictCooling'

  CASE (iRT_DistrictHeating)
    ResourceTypeChar='DistrictHeating'

  CASE (iRT_ElectricityProduced)
    ResourceTypeChar='ElectricityProduced'

  CASE (iRT_ElectricityPurchased)
    ResourceTypeChar='ElectricityPurchased'

  CASE (iRT_ElectricitySurplusSold)
    ResourceTypeChar='ElectricitySurplusSold'

  CASE (iRT_ElectricityNet)
    ResourceTypeChar='ElectricityNet'

  CASE (iRT_SolarWater)
    ResourceTypeChar='SolarWater'

  CASE (iRT_SolarAir)
    ResourceTypeChar='SolarAir'

  CASE (iRT_SO2)
    ResourceTypeChar='SO2'

  CASE (iRT_NOx)
    ResourceTypeChar='NOx'

  CASE (iRT_N2O)
    ResourceTypeChar='N2O'

  CASE (iRT_PM)
    ResourceTypeChar='PM'

  CASE (iRT_PM2_5)
    ResourceTypeChar='PM2.5'

  CASE (iRT_PM10)
    ResourceTypeChar='PM10'

  CASE (iRT_CO)
    ResourceTypeChar='CO'

  CASE (iRT_CO2)
    ResourceTypeChar='CO2'

  CASE (iRT_CH4)
    ResourceTypeChar='CH4'

  CASE (iRT_NH3)
    ResourceTypeChar='NH3'

  CASE (iRT_NMVOC)
    ResourceTypeChar='NMVOC'

  CASE (iRT_Hg)
    ResourceTypeChar='Hg'

  CASE (iRT_Pb)
    ResourceTypeChar='Pb'

  CASE (iRT_NuclearHigh)
    ResourceTypeChar='Nuclear High'

  CASE (iRT_NuclearLow)
    ResourceTypeChar='Nuclear Low'

  CASE (iRT_WaterEnvironmentalFactors)
    ResourceTypeChar='WaterEnvironmentalFactors'

  CASE (iRT_CarbonEquivalent)
    ResourceTypeChar='Carbon Equivalent'

  CASE (iRT_Source)
    ResourceTypeChar='Source'

  CASE (iRT_PlantLoopHeatingDemand)
    ResourceTypeChar='PlantLoopHeatingDemand'

  CASE (iRT_PlantLoopCoolingDemand)
    ResourceTypeChar='PlantLoopCoolingDemand'

  CASE DEFAULT
    ResourceTypeChar='Unknown'
  END SELECT

  RETURN

END FUNCTION GetResourceTypeChar


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

END MODULE DataGlobalConstants

