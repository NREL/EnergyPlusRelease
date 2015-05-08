!Note: Multiple fortran Modules related to Generators are included in this file.
!      Find the Generator you are looking for by searching for "Module <name>"
! File Contents:
!
!_______________________________________________
!Utility modules used by other generators.
!
! GeneratorFuelSupply
!   reused among some generators to define gaseous fuel chemistry, optional compressor)
!
! GeneratorDynamicsManager
!   reused among some generators to track on/off state, transient limits, control implications etc.
!
!_______________________________________________________________________
! IEA Annex 42 generators:
! MicroCHPElectricGenerator
! (small-scale/residential internal combustion and Stirling Engine for CHP, )
!
! FuelCellElectricGenerator  (Solid Oxide Fuel Cell (SOFC))
!                            (Proton Exchange Membrane FC (PEMFC))
!                            IEA/ECBCS Annex 42 model)

!__________________________________________________________________________
! BLAST inherited generators:
! ICENGINEElectricGenerator (Internal Combustion, curve fit from BLAST)
!
! CTElectricGenerator (COMBUSTION Turbine)
!__________________________________________________________________________
!
! New microturbine model added by FSEC:
!   MicroturbineElectricGenerator  ! Microturbine Electric Generator Module
!
!*******************************************************************************************************
! ****************************************************************************************************
MODULE GeneratorFuelSupply

          ! Module containing the routines dealing with the fuel supply for some generators
          ! different generator modules can reuse the same fuel supply code, hence a seperate module

          ! MODULE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! data defined in DataGenerators.f90
          ! this module only provides input and subroutines for other component simulations
          !  no specific energyplus component is modeled here.  it is used by other generators

          ! REFERENCES:
          ! Annex 42 documentation

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGenerators
USE DataGlobals, ONLY: MaxNameLength, OutputFileInits, HoursInDay
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError

          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE

          ! <name Public routines, optionally name Private routines within this module>

PUBLIC GetGeneratorFuelSupplyInput
PUBLIC SetupFuelConstituentData

CONTAINS

SUBROUTINE GetGeneratorFuelSupplyInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   July 2006,
          !       MODIFIED       na
          !       RE-ENGINEERED  this module extracted from older SOFC module for
          !                      reuse with both Annex 42 models,

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,    ONLY: GetNumObjectsFound, VerifyName, GetObjectItem, SameString ! might also use FindItemInList
  USE DataIPShortCuts
  USE NodeInputManager,  ONLY: GetOnlySingleNode
  USE CurveManager,      ONLY: GetCurveIndex
  USE ScheduleManager,   ONLY: GetScheduleIndex
  USE DataLoopNode,      ONLY: NodeConnectionType_Sensor, NodeType_Air, ObjectIsNotParent
  USE General, ONLY: RoundSigDigits

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
!  INTEGER                     :: GeneratorNum !Generator counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(25)  :: AlphArray !character string data
  REAL(r64),                        DIMENSION(200)  :: NumArray  !numeric data TODO deal with allocatable for extensible
  LOGICAL, SAVE :: ErrorsFound=.false.  ! error flag
  LOGICAL       :: IsNotOK              ! Flag to verify name
  LOGICAL       :: IsBlank              ! Flag for blank name
  INTEGER                        :: FuelSupNum    !
  LOGICAL, SAVE :: MyOneTimeFlag = .true. !
  CHARACTER(len=MaxNameLength) :: ObjMSGName
  INTEGER       :: ConstitNum

IF (MyOneTimeFlag) then
  cCurrentModuleObject = 'Generator:FuelSupply'
  NumGeneratorFuelSups = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumGeneratorFuelSups <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

  Allocate(FuelSupply(NumGeneratorFuelSups))

  DO FuelSupNum = 1 , NumGeneratorFuelSups
    CALL GetObjectItem(cCurrentModuleObject,FuelSupNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelSupply%Name,FuelSupNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
     ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    FuelSupply(FuelSupNum)%Name     = AlphArray(1)
    ObjMSGName =TRIM(cCurrentModuleObject)//' Named '//trim(AlphArray(1))
    IF(SameString('TemperatureFromAirNode', AlphArray(2))) THEN
        FuelSupply(FuelSupNum)%FuelTempMode = FuelInTempFromNode
    ELSEIF (SameString('Scheduled', AlphArray(2))) THEN
        FuelSupply(FuelSupNum)%FuelTempMode = FuelInTempSchedule
    ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.
    ENDIF

    FuelSupply(FuelSupNum)%NodeName = AlphArray(3)
    FuelSupply(FuelSupNum)%NodeNum  = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

    FuelSupply(FuelSupNum)%SchedNum = GetScheduleIndex(AlphArray(4))
    IF ((FuelSupply(FuelSupNum)%SchedNum == 0) .AND. &
       (FuelSupply(FuelSupNum)%FuelTempMode == FuelInTempSchedule)) THEN
       CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
       CAll ShowContinueError('Schedule named was not found')
       errorsFound = .true.
    ENDIF

    FuelSupply(FuelSupNum)%CompPowerCurveID = GetCurveIndex(AlphArray(5))
    IF (FuelSupply(FuelSupNum)%CompPowerCurveID == 0 ) then
       CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(AlphArray(5)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
       Call ShowContinueError('Curve named was not found ' )
       errorsFound = .true.
    ENDIF

    FuelSupply%CompPowerLossFactor = NumArray(1)

    IF (SameString(AlphArray(6), 'GaseousConstituents')) THEN
      FuelSupply(FuelSupNum)%FuelTypeMode = fuelModeGaseousConstituents
    ELSEIF (samestring(AlphArray(6), 'LiquidGeneric')) THEN
      FuelSupply(FuelSupNum)%FuelTypeMode = fuelModeGenericLiquid
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ENDIF

    FuelSupply(FuelSupNum)%LHVliquid = NumArray(2)*1000.0d0 !generic liquid LHV  (kJ/kG input converted to J/kG )
    FuelSupply(FuelSupNum)%HHV = NumArray(3)*1000.0d0 !generic liquid HHV (kJ/kG input converted to J/kG )
    FuelSupply(FuelSupNum)%MW  = NumArray(4) !
    FuelSupply(FuelSupNum)%eCO2 = NumArray(5) !

    IF (FuelSupply(FuelSupNum)%FuelTypeMode == fuelModeGaseousConstituents) then
      NumFuelConstit  = NumArray(6)
      FuelSupply(FuelSupNum)%NumConstituents = NumFuelConstit

      IF (NumFuelConstit > 12) THEN
        CALL showSevereError(TRIM(cCurrentModuleObject)//' model not set up for more than 12 fuel constituents')
        errorsfound = .true.
      ENDIF
      If (NumFuelConstit < 1) THEN
        CALL showSevereError(TRIM(cCurrentModuleObject)//' model needs at least one fuel constituent')
        errorsfound = .true.
      ENDIF

      DO ConstitNum=1, NumFuelConstit
        FuelSupply(FuelSupNum)%ConstitName(ConstitNum)       = AlphArray(ConstitNum + 6)
        FuelSupply(FuelSupNum)%ConstitMolalFract(ConstitNum) = NumArray(ConstitNum + 6)

      ENDDO

      ! check for molar fractions summing to 1.0.
      IF (ABS(SUM(FuelSupply(FuelSupNum)%ConstitMolalFract)-1.0d0) > .0001d0) THEN
        CALL showSevereError(TRIM(cCurrentModuleObject)//' molar fractions do not sum to 1.0')
        CALL ShowContinueError('Sum was='//TRIM(RoundSigDigits(SUM(FuelSupply(FuelSupNum)%ConstitMolalFract),5)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        errorsfound = .true.
      ENDIF
    ENDIF

  ENDDO

  !now make calls to Setup

  DO FuelSupNum = 1 , NumGeneratorFuelSups
    CAll SetupFuelConstituentData(FuelSupNum, ErrorsFound)
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Problem found processing input for '//TRIM(cCurrentModuleObject) )
  ENDIF

  MyOneTimeFlag = .FALSE.
ENDIF ! MyOneTimeFlag

RETURN

END SUBROUTINE GetGeneratorFuelSupplyInput
!******************************************************************************
SUBROUTINE SetupFuelConstituentData(FuelSupplyNum, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Aug 2005,
          !       MODIFIED       na
          !       RE-ENGINEERED  July/Aug 2006, extracted to own module. added liquid fuel option

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill data structure for gas phase thermochemistry

          ! METHODOLOGY EMPLOYED:
          ! Hardcoded data from NIST is filled into data structure one time only

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE InputProcessor, ONLY: FindItemInList, FindItem
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, Intent(IN)    :: FuelSupplyNum
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: NumHardCodedConstituents  !number of gases included in data
  REAL(r64) :: LHVfuel ! lower heating value of fuel, working var
  REAL(r64) :: HHVfuel ! higher heating value of fuel, working var
  REAL(r64) :: O2Stoic ! stochiometric oxygen coef in chemical equation (15)
  REAL(r64) :: CO2ProdStoic ! product gases carbon dioxide coeff
  REAL(r64) :: H20ProdStoic ! product gases water coeff
  INTEGER   :: i  ! loop index
  Character(Len=maxnamelength) :: thisName ! working string var
  INTEGER   :: thisGasID  ! working index in Gas phase data structure
  INTEGER   :: CO2dataID  ! hard wired to CO2 index in gas data struct
  INTEGER   :: WaterDataID ! hard wired to Water index in gas data struct
  REAL(r64) :: LHVi  !working var for lower heating value calc
  REAL(r64) :: HHVi  !working var for higher heating value calc
!  INTEGER   :: thisConstituent
  REAL(r64) :: MWfuel
!unused  REAL(r64) :: DelfHfuel
!unused  REAL(r64) :: h_i
!unused  REAL(r64) :: LHV

  NumHardCodedConstituents = 14

  IF (.NOT. ALLOCATED(GasPhaseThermoChemistryData)) THEN
     ALLOCATE(GasPhaseThermoChemistryData(NumHardCodedConstituents))

  endif
  ! Carbon Dioxide (CO2) Temp K 298-1200 (Chase 1998)
  GasPhaseThermoChemistryData(1)%ConstituentName                  = 'CarbonDioxide'
  GasPhaseThermoChemistryData(1)%ConstituentFormula               = 'CO2'
  GasPhaseThermoChemistryData(1)%StdRefMolarEnthOfForm = -393.5224d0  !KJ/mol
  GasPhaseThermoChemistryData(1)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(1)%ShomateA =  24.99735d0
  GasPhaseThermoChemistryData(1)%ShomateB =  55.18696d0
  GasPhaseThermoChemistryData(1)%ShomateC = -33.69137d0
  GasPhaseThermoChemistryData(1)%ShomateD =  7.948387d0
  GasPhaseThermoChemistryData(1)%ShomateE = -0.136638d0
  GasPhaseThermoChemistryData(1)%ShomateF = -403.6075d0
  GasPhaseThermoChemistryData(1)%ShomateG =  228.2431d0
  GasPhaseThermoChemistryData(1)%ShomateH = -393.5224d0
  GasPhaseThermoChemistryData(1)%NumCarbons = 1.0d0
  GasPhaseThermoChemistryData(1)%NumHydrogens = 0.0d0
  GasPhaseThermoChemistryData(1)%NumOxygens   = 2.0d0
  GasPhaseThermoChemistryData(1)%MolecularWeight = 44.01d0


  ! Nitrogen (N2) Temp (K) 298-6000
  GasPhaseThermoChemistryData(2)%ConstituentName                  = 'Nitrogen'
  GasPhaseThermoChemistryData(2)%ConstituentFormula               = 'N2'
  GasPhaseThermoChemistryData(2)%StdRefMolarEnthOfForm = 0.0d0 !
  GasPhaseThermoChemistryData(2)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(2)%ShomateA = 26.092d0
  GasPhaseThermoChemistryData(2)%ShomateB = 8.218801d0
  GasPhaseThermoChemistryData(2)%ShomateC = -1.976141d0
  GasPhaseThermoChemistryData(2)%ShomateD = 0.159274d0
  GasPhaseThermoChemistryData(2)%ShomateE = 0.044434d0
  GasPhaseThermoChemistryData(2)%ShomateF = -7.98923d0
  GasPhaseThermoChemistryData(2)%ShomateG = 221.02d0
  GasPhaseThermoChemistryData(2)%ShomateH = 0.000d0
  GasPhaseThermoChemistryData(2)%NumCarbons = 0.0d0
  GasPhaseThermoChemistryData(2)%NumHydrogens = 0.0d0
  GasPhaseThermoChemistryData(2)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(2)%MolecularWeight = 28.01d0

  ! Oxygen (O2) Temp (K) 298-6000
  GasPhaseThermoChemistryData(3)%ConstituentName                  =  'Oxygen'
  GasPhaseThermoChemistryData(3)%ConstituentFormula               =  'O2'
  GasPhaseThermoChemistryData(3)%StdRefMolarEnthOfForm =  0.0d0
  GasPhaseThermoChemistryData(3)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(3)%ShomateA = 29.659d0
  GasPhaseThermoChemistryData(3)%ShomateB = 6.137261d0
  GasPhaseThermoChemistryData(3)%ShomateC = -1.186521d0
  GasPhaseThermoChemistryData(3)%ShomateD = 0.095780d0
  GasPhaseThermoChemistryData(3)%ShomateE = -0.219663d0
  GasPhaseThermoChemistryData(3)%ShomateF = -9.861391d0
  GasPhaseThermoChemistryData(3)%ShomateG = 237.948d0
  GasPhaseThermoChemistryData(3)%ShomateH = 0.0d0
  GasPhaseThermoChemistryData(3)%NumCarbons = 0.0d0
  GasPhaseThermoChemistryData(3)%NumHydrogens = 0.0d0
  GasPhaseThermoChemistryData(3)%NumOxygens   = 2.0d0
  GasPhaseThermoChemistryData(3)%MolecularWeight = 32.00d0

  ! Water (H2O) Temp K 300-1700
  ! need lower temperature range for Shomate coef for Water Vapor..
  GasPhaseThermoChemistryData(4)%ConstituentName                  =  'Water'
  GasPhaseThermoChemistryData(4)%ConstituentFormula               =  'H2O'
  GasPhaseThermoChemistryData(4)%StdRefMolarEnthOfForm =   -241.8264d0 !KJ/mol
  GasPhaseThermoChemistryData(4)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(4)%ShomateA =   29.0373d0
  GasPhaseThermoChemistryData(4)%ShomateB =    10.2573d0
  GasPhaseThermoChemistryData(4)%ShomateC =    2.81048d0
  GasPhaseThermoChemistryData(4)%ShomateD =   -0.95914d0
  GasPhaseThermoChemistryData(4)%ShomateE =    0.11725d0
  GasPhaseThermoChemistryData(4)%ShomateF = -250.569d0
  GasPhaseThermoChemistryData(4)%ShomateG =  223.3967d0
  GasPhaseThermoChemistryData(4)%ShomateH = -241.8264d0
  GasPhaseThermoChemistryData(4)%NumCarbons = 0.0d0
  GasPhaseThermoChemistryData(4)%NumHydrogens = 2.0d0
  GasPhaseThermoChemistryData(4)%NumOxygens   = 1.0d0
  GasPhaseThermoChemistryData(4)%MolecularWeight = 18.02d0

  ! Argon (Ar)  Temp K 298-600

  GasPhaseThermoChemistryData(5)%ConstituentName                  =  'Argon'
  GasPhaseThermoChemistryData(5)%ConstituentFormula               =  'Ar'
  GasPhaseThermoChemistryData(5)%StdRefMolarEnthOfForm =   0.0d0
  GasPhaseThermoChemistryData(5)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(5)%ShomateA =  20.786d0
  GasPhaseThermoChemistryData(5)%ShomateB =   2.825911d-07
  GasPhaseThermoChemistryData(5)%ShomateC =  -1.464191d-07
  GasPhaseThermoChemistryData(5)%ShomateD =   1.092131d-08
  GasPhaseThermoChemistryData(5)%ShomateE =  -3.661371d-08
  GasPhaseThermoChemistryData(5)%ShomateF =  -6.19735d0
  GasPhaseThermoChemistryData(5)%ShomateG = 179.999d0
  GasPhaseThermoChemistryData(5)%ShomateH =   0.0d0
  GasPhaseThermoChemistryData(5)%NumCarbons = 0.0d0
  GasPhaseThermoChemistryData(5)%NumHydrogens = 0.0d0
  GasPhaseThermoChemistryData(5)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(5)%MolecularWeight = 39.95d0

  ! Hydrogen (H2) Temp K 298-1000
  GasPhaseThermoChemistryData(6)%ConstituentName                  = 'Hydrogen'
  GasPhaseThermoChemistryData(6)%ConstituentFormula               = 'H2'
  GasPhaseThermoChemistryData(6)%StdRefMolarEnthOfForm =  0.0d0
  GasPhaseThermoChemistryData(6)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(6)%ShomateA =   33.066178d0
  GasPhaseThermoChemistryData(6)%ShomateB =  -11.363417d0
  GasPhaseThermoChemistryData(6)%ShomateC =   11.432816d0
  GasPhaseThermoChemistryData(6)%ShomateD =   -2.772874d0
  GasPhaseThermoChemistryData(6)%ShomateE =   -0.158558d0
  GasPhaseThermoChemistryData(6)%ShomateF =   -9.980797d0
  GasPhaseThermoChemistryData(6)%ShomateG =  172.707974d0
  GasPhaseThermoChemistryData(6)%ShomateH =    0.0d0
  GasPhaseThermoChemistryData(6)%NumCarbons = 0.0d0
  GasPhaseThermoChemistryData(6)%NumHydrogens = 2.0d0
  GasPhaseThermoChemistryData(6)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(6)%MolecularWeight = 2.02d0

  ! Methane (CH4) Temp K 298-1300
  GasPhaseThermoChemistryData(7)%ConstituentName                  =  'Methane'
  GasPhaseThermoChemistryData(7)%ConstituentFormula               =  'CH4'
  GasPhaseThermoChemistryData(7)%StdRefMolarEnthOfForm =   -74.8731d0  !KJ/mol (Chase 1998)
  GasPhaseThermoChemistryData(7)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(7)%ShomateA =  -0.703029d0
  GasPhaseThermoChemistryData(7)%ShomateB = 108.4773d0
  GasPhaseThermoChemistryData(7)%ShomateC = -42.52157d0
  GasPhaseThermoChemistryData(7)%ShomateD =   5.862788d0
  GasPhaseThermoChemistryData(7)%ShomateE =   0.678565d0
  GasPhaseThermoChemistryData(7)%ShomateF = -76.84376d0
  GasPhaseThermoChemistryData(7)%ShomateG = 158.7163d0
  GasPhaseThermoChemistryData(7)%ShomateH = -74.87310d0
  GasPhaseThermoChemistryData(7)%NumCarbons = 1.0d0
  GasPhaseThermoChemistryData(7)%NumHydrogens = 4.0d0
  GasPhaseThermoChemistryData(7)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(7)%MolecularWeight = 16.04d0


  ! Ethane (C2H6)
  GasPhaseThermoChemistryData(8)%ConstituentName                  =  'Ethane'
  GasPhaseThermoChemistryData(8)%ConstituentFormula               =  'C2H6'
  GasPhaseThermoChemistryData(8)%StdRefMolarEnthOfForm =  -83.8605d0 ! -83.8 !KJ/mol (Pittam and Pilcher 1972)
  GasPhaseThermoChemistryData(8)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(8)%ShomateA = -3.03849d0
  GasPhaseThermoChemistryData(8)%ShomateB = 199.202d0
  GasPhaseThermoChemistryData(8)%ShomateC = -84.9812d0
  GasPhaseThermoChemistryData(8)%ShomateD = 11.0348d0
  GasPhaseThermoChemistryData(8)%ShomateE = 0.30348d0
  GasPhaseThermoChemistryData(8)%ShomateF = -90.0633d0
  GasPhaseThermoChemistryData(8)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(8)%ShomateH = -83.8605d0
  GasPhaseThermoChemistryData(8)%NumCarbons = 2.0d0
  GasPhaseThermoChemistryData(8)%NumHydrogens = 6.0d0
  GasPhaseThermoChemistryData(8)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(8)%MolecularWeight = 30.07d0
  GasPhaseThermoChemistryData(8)%NASA_A1 = 0.14625388d+01
  GasPhaseThermoChemistryData(8)%NASA_A2 = 0.15494667d-01
  GasPhaseThermoChemistryData(8)%NASA_A3 = 0.05780507d-04
  GasPhaseThermoChemistryData(8)%NASA_A4 = -0.12578319d-07
  GasPhaseThermoChemistryData(8)%NASA_A5 = 0.04586267d-10
  GasPhaseThermoChemistryData(8)%NASA_A6 = -0.11239176d+05
  GasPhaseThermoChemistryData(8)%NASA_A7 = 0.14432295d+02

  ! Propane (C3H8)
  GasPhaseThermoChemistryData(9)%ConstituentName                  =  'Propane'
  GasPhaseThermoChemistryData(9)%ConstituentFormula               =  'C3H8'
  GasPhaseThermoChemistryData(9)%StdRefMolarEnthOfForm = -103.855d0 !  -104.7 !kJ/mol  (Pittam and Pilcher 1972)
  GasPhaseThermoChemistryData(9)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(9)%ShomateA = -23.1747d0
  GasPhaseThermoChemistryData(9)%ShomateB = 363.742d0
  GasPhaseThermoChemistryData(9)%ShomateC = -222.981d0
  GasPhaseThermoChemistryData(9)%ShomateD = 56.253d0
  GasPhaseThermoChemistryData(9)%ShomateE = 0.61164d0
  GasPhaseThermoChemistryData(9)%ShomateF = -109.206d0
  GasPhaseThermoChemistryData(9)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(9)%ShomateH = -103.855d0
  GasPhaseThermoChemistryData(9)%NumCarbons = 3.0d0
  GasPhaseThermoChemistryData(9)%NumHydrogens = 8.0d0
  GasPhaseThermoChemistryData(9)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(9)%MolecularWeight = 44.10d0
  GasPhaseThermoChemistryData(9)%NASA_A1 = 0.08969208d+01
  GasPhaseThermoChemistryData(9)%NASA_A2 = 0.02668986d+00
  GasPhaseThermoChemistryData(9)%NASA_A3 = 0.05431425d-04
  GasPhaseThermoChemistryData(9)%NASA_A4 = -0.02126000d-06
  GasPhaseThermoChemistryData(9)%NASA_A5 = 0.09243330d-10
  GasPhaseThermoChemistryData(9)%NASA_A6 = -0.13954918d+05
  GasPhaseThermoChemistryData(9)%NASA_A7 = 0.01935533d+03


  ! Butane (C4H10)
  GasPhaseThermoChemistryData(10)%ConstituentName                  =  'Butane'
  GasPhaseThermoChemistryData(10)%ConstituentFormula               =  'C4H10'
  GasPhaseThermoChemistryData(10)%StdRefMolarEnthOfForm =   -133.218d0 ! -125.6 !kJ/mol  (Pittam and Pilcher 1972)
  GasPhaseThermoChemistryData(10)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(10)%ShomateA = -5.24343d0
  GasPhaseThermoChemistryData(10)%ShomateB = 426.442d0
  GasPhaseThermoChemistryData(10)%ShomateC = -257.955d0
  GasPhaseThermoChemistryData(10)%ShomateD = 66.535d0
  GasPhaseThermoChemistryData(10)%ShomateE = -0.26994d0
  GasPhaseThermoChemistryData(10)%ShomateF = -149.365d0
  GasPhaseThermoChemistryData(10)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(10)%ShomateH = -133.218d0
  GasPhaseThermoChemistryData(10)%NumCarbons = 4.0d0
  GasPhaseThermoChemistryData(10)%NumHydrogens = 10.0d0
  GasPhaseThermoChemistryData(10)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(10)%MolecularWeight = 58.12d0
  GasPhaseThermoChemistryData(10)%NASA_A1 = -0.02256618d+02
  GasPhaseThermoChemistryData(10)%NASA_A2 =  0.05881732d+00
  GasPhaseThermoChemistryData(10)%NASA_A3 = -0.04525782d-03
  GasPhaseThermoChemistryData(10)%NASA_A4 =  0.02037115d-06
  GasPhaseThermoChemistryData(10)%NASA_A5 = -0.04079458d-10
  GasPhaseThermoChemistryData(10)%NASA_A6 = -0.01760233d+06
  GasPhaseThermoChemistryData(10)%NASA_A7 =  0.03329595d+03

  ! Pentane (C5H12)
  GasPhaseThermoChemistryData(11)%ConstituentName                  =  'Pentane'
  GasPhaseThermoChemistryData(11)%ConstituentFormula               =  'C5H12'
  GasPhaseThermoChemistryData(11)%StdRefMolarEnthOfForm =  -146.348d0 ! -146.8 !kJ/mol (Good 1970)
  GasPhaseThermoChemistryData(11)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(11)%ShomateA = -34.9431d0
  GasPhaseThermoChemistryData(11)%ShomateB = 576.777d0
  GasPhaseThermoChemistryData(11)%ShomateC = -338.353d0
  GasPhaseThermoChemistryData(11)%ShomateD = 76.8232d0
  GasPhaseThermoChemistryData(11)%ShomateE = 1.00948d0
  GasPhaseThermoChemistryData(11)%ShomateF = -155.348d0
  GasPhaseThermoChemistryData(11)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(11)%ShomateH = -146.348d0
  GasPhaseThermoChemistryData(11)%NumCarbons = 5.0d0
  GasPhaseThermoChemistryData(11)%NumHydrogens = 12.0d0
  GasPhaseThermoChemistryData(11)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(11)%MolecularWeight = 72.15d0
  GasPhaseThermoChemistryData(11)%NASA_A1 =  0.01877907d+02
  GasPhaseThermoChemistryData(11)%NASA_A2 =  0.04121645d+00
  GasPhaseThermoChemistryData(11)%NASA_A3 =  0.12532337d-04
  GasPhaseThermoChemistryData(11)%NASA_A4 = -0.03701536d-06
  GasPhaseThermoChemistryData(11)%NASA_A5 =  0.15255685d-10
  GasPhaseThermoChemistryData(11)%NASA_A6 = -0.02003815d+06
  GasPhaseThermoChemistryData(11)%NASA_A7 =  0.01877256d+03

  ! Hexane  (C6H14)
  GasPhaseThermoChemistryData(12)%ConstituentName                  =  'Hexane'
  GasPhaseThermoChemistryData(12)%ConstituentFormula               =  'C6H14'
  GasPhaseThermoChemistryData(12)%StdRefMolarEnthOfForm =   -166.966d0 ! -167.2 !kJ/mol (Prosen and Rossini 1945)
  GasPhaseThermoChemistryData(12)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(12)%ShomateA = -46.7786d0
  GasPhaseThermoChemistryData(12)%ShomateB = 711.187d0
  GasPhaseThermoChemistryData(12)%ShomateC = -438.39d0
  GasPhaseThermoChemistryData(12)%ShomateD = 103.784d0
  GasPhaseThermoChemistryData(12)%ShomateE = 1.23887d0
  GasPhaseThermoChemistryData(12)%ShomateF = -176.813d0
  GasPhaseThermoChemistryData(12)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(12)%ShomateH = -166.966d0
  GasPhaseThermoChemistryData(12)%NumCarbons = 6.0d0
  GasPhaseThermoChemistryData(12)%NumHydrogens = 14.0d0
  GasPhaseThermoChemistryData(12)%NumOxygens   = 0.0d0
  GasPhaseThermoChemistryData(12)%MolecularWeight = 86.18d0
  GasPhaseThermoChemistryData(12)%NASA_A1 =  0.01836174d+02
  GasPhaseThermoChemistryData(12)%NASA_A2 =  0.05098461d+00
  GasPhaseThermoChemistryData(12)%NASA_A3 =  0.12595857d-04
  GasPhaseThermoChemistryData(12)%NASA_A4 = -0.04428362d-06
  GasPhaseThermoChemistryData(12)%NASA_A5 =  0.01872237d-09
  GasPhaseThermoChemistryData(12)%NASA_A6 = -0.02292749d+06
  GasPhaseThermoChemistryData(12)%NASA_A7 =  0.02088145d+03

 ! Methanol (CH3OH)
 ! No Shomate coefficients???
  GasPhaseThermoChemistryData(13)%ConstituentName                  =  'Methanol'
  GasPhaseThermoChemistryData(13)%ConstituentFormula               =  'CH3OH'
  GasPhaseThermoChemistryData(13)%StdRefMolarEnthOfForm =  -201.102d0 ! -201.0 !kJ/mol (Hine and Arata 1976)
  GasPhaseThermoChemistryData(13)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(13)%ShomateA = 14.1952d0
  GasPhaseThermoChemistryData(13)%ShomateB = 97.7218d0
  GasPhaseThermoChemistryData(13)%ShomateC = -9.73279d0
  GasPhaseThermoChemistryData(13)%ShomateD = -12.8461d0
  GasPhaseThermoChemistryData(13)%ShomateE = 0.15819d0
  GasPhaseThermoChemistryData(13)%ShomateF = -209.037d0
  GasPhaseThermoChemistryData(13)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(13)%ShomateH = -201.102d0
  GasPhaseThermoChemistryData(13)%NumCarbons = 1.0d0
  GasPhaseThermoChemistryData(13)%NumHydrogens = 4.0d0
  GasPhaseThermoChemistryData(13)%NumOxygens   = 1.0d0
  GasPhaseThermoChemistryData(13)%MolecularWeight = 32.04d0
  GasPhaseThermoChemistryData(13)%NASA_A1 =  0.02660115d+02
  GasPhaseThermoChemistryData(13)%NASA_A2 =  0.07341508d-01
  GasPhaseThermoChemistryData(13)%NASA_A3 =  0.07170050d-04
  GasPhaseThermoChemistryData(13)%NASA_A4 = -0.08793194d-07
  GasPhaseThermoChemistryData(13)%NASA_A5 =  0.02390570d-10
  GasPhaseThermoChemistryData(13)%NASA_A6 = -0.02535348d+06
  GasPhaseThermoChemistryData(13)%NASA_A7 =  0.11232631d+02

  ! Ethanol (C2H5OH)
  ! No Shomate coefficients???
  GasPhaseThermoChemistryData(14)%ConstituentName                  =  'Ethanol'
  GasPhaseThermoChemistryData(14)%ConstituentFormula               =  'C2H5OH'
  GasPhaseThermoChemistryData(14)%StdRefMolarEnthOfForm = -234.441d0 !  -235.3 !kJ/mol (Green 1960)
  GasPhaseThermoChemistryData(14)%ThermoMode = NISTShomate
  GasPhaseThermoChemistryData(14)%ShomateA = -8.87256d0
  GasPhaseThermoChemistryData(14)%ShomateB = 282.389d0
  GasPhaseThermoChemistryData(14)%ShomateC = -178.85d0
  GasPhaseThermoChemistryData(14)%ShomateD = 46.3528d0
  GasPhaseThermoChemistryData(14)%ShomateE = 0.48364d0
  GasPhaseThermoChemistryData(14)%ShomateF = -241.239d0
  GasPhaseThermoChemistryData(14)%ShomateG = -999.d0
  GasPhaseThermoChemistryData(14)%ShomateH = -234.441d0
  GasPhaseThermoChemistryData(14)%NumCarbons = 2.0d0
  GasPhaseThermoChemistryData(14)%NumHydrogens = 6.0d0
  GasPhaseThermoChemistryData(14)%NumOxygens   = 1.0d0
  GasPhaseThermoChemistryData(14)%MolecularWeight = 46.07d0
  GasPhaseThermoChemistryData(14)%NASA_A1 =  0.18461027d+01
  GasPhaseThermoChemistryData(14)%NASA_A2 =  0.20475008d-01
  GasPhaseThermoChemistryData(14)%NASA_A3 =  0.39904089d-05
  GasPhaseThermoChemistryData(14)%NASA_A4 = -0.16585986d-07
  GasPhaseThermoChemistryData(14)%NASA_A5 =  0.73090440d-11
  GasPhaseThermoChemistryData(14)%NASA_A6 = -0.29663086d+05
  GasPhaseThermoChemistryData(14)%NASA_A7 =  0.17289993d+02

  If (FuelSupply(FuelSupplyNum)%FuelTypeMode == fuelModeGaseousConstituents) then
    ! now calculate LHV of fuel for entire simulation

    ! sum over each constituent
    O2Stoic = 0.0d0
    CO2ProdStoic = 0.0d0
    H20ProdStoic = 0.0d0
    CO2dataID   = 1  !hard-coded above
    WaterDataID = 4  !hard-coded above
    ! Loop over fuel constituents and do one-time setup
    DO i=1, FuelSupply(FuelSupplyNum)%NumConstituents

       thisName = FuelSupply(FuelSupplyNum)%ConstitName(i)
       thisGasID = FindItem(thisName, GasPhaseThermoChemistryData%ConstituentName, NumHardCodedConstituents)
       FuelSupply(FuelSupplyNum)%GasLibID(i) = thisGasID

       IF (thisGasID == 0) THEN
         CALL ShowSevereError('Fuel constituent not found in thermochemistry data: '//trim(thisName ))
         errorsfound = .true.
       ENDif


      !for this fuel mixture, figure stoichiometric oxygen requirement
      O2Stoic = O2Stoic + FuelSupply(FuelSupplyNum)%ConstitMolalFract(i) *                  &
              (  GasPhaseThermoChemistryData(thisGasID)%NumCarbons                         &
               + GasPhaseThermoChemistryData(thisGasID)%NumHydrogens / 4.0d0                 &
               - GasPhaseThermoChemistryData(thisGasID)%NumOxygens / 2.0d0 )
     ! for this fuel mixture, figure stoichiometric Carbon Dioxide in Product Gases

     CO2ProdStoic = CO2ProdStoic + FuelSupply(FuelSupplyNum)%ConstitMolalFract(i) *                  &
                           GasPhaseThermoChemistryData(thisGasID)%NumCarbons

     H20ProdStoic = H20ProdStoic + FuelSupply(FuelSupplyNum)%ConstitMolalFract(i) *                  &
                           GasPhaseThermoChemistryData(thisGasID)%NumHydrogens / 2.0d0
    ENDDO

    FuelSupply(FuelSupplyNum)%StoicOxygenRate = O2Stoic
    FuelSupply(FuelSupplyNum)%CO2ProductGasCoef = CO2ProdStoic
    FuelSupply(FuelSupplyNum)%H20ProductGasCoef = H20ProdStoic

    !Calculate LHV for an NdotFuel of 1.0
    LHVfuel = 0.0d0
    DO i=1, FuelSupply(FuelSupplyNum)%NumConstituents
      thisGasID  = FuelSupply(FuelSupplyNum)%GasLibID(i)
      IF (GasPhaseThermoChemistryData(thisGasID)%NumHydrogens == 0.0d0) THEN
        LHVi = 0.0d0
      ELSE
        LHVi = GasPhaseThermoChemistryData(thisGasID)%StdRefMolarEnthOfForm            &
              - GasPhaseThermoChemistryData(thisGasID)%NumCarbons                    &
                  * GasPhaseThermoChemistryData(CO2dataID)%StdRefMolarEnthOfForm       &
              - (GasPhaseThermoChemistryData(thisGasID)%NumHydrogens/2.0d0)                    &
                  * GasPhaseThermoChemistryData(WaterDataID)%StdRefMolarEnthOfForm
      ENDIF
      LHVfuel = LHVfuel + LHVi * FuelSupply(FuelSupplyNum)%ConstitMolalFract(i)
    ENDDO
    FuelSupply(FuelSupplyNum)%LHV = LHVfuel

  !Calculate HHV for an NdotFuel of 1.0
    HHVfuel = 0.0d0
    DO i=1, FuelSupply(FuelSupplyNum)%NumConstituents
      thisGasID  = FuelSupply(FuelSupplyNum)%GasLibID(i)
      IF (GasPhaseThermoChemistryData(thisGasID)%NumHydrogens == 0.0d0) THEN
        HHVi = 0.0d0
      ELSE
        HHVi = GasPhaseThermoChemistryData(thisGasID)%StdRefMolarEnthOfForm            &
              - GasPhaseThermoChemistryData(thisGasID)%NumCarbons                      &
                  * GasPhaseThermoChemistryData(CO2dataID)%StdRefMolarEnthOfForm       &
              - (GasPhaseThermoChemistryData(thisGasID)%NumHydrogens/2.0d0)              &
                  * GasPhaseThermoChemistryData(WaterDataID)%StdRefMolarEnthOfForm     &
              + (GasPhaseThermoChemistryData(thisGasID)%NumHydrogens/2.0d0)              &
                * (GasPhaseThermoChemistryData(WaterDataID)%StdRefMolarEnthOfForm + 285.8304d0)
      ENDIF
      HHVfuel = HHVfuel + HHVi * FuelSupply(FuelSupplyNum)%ConstitMolalFract(i)
    ENDDO

    !Calculate Molecular Weight for this fuel
    MWfuel = 0.0d0
    DO I=1, FuelSupply(FuelsupplyNum)%NumConstituents
      thisGasID = FuelSupply(FuelSupplyNum)%GasLibID(i)
      MWfuel = MWfuel + &
             FuelSupply(FuelSupplyNum)%ConstitMolalFract(i)*GasPhaseThermoChemistryData(thisGasID)%MolecularWeight
    ENDDO
    FuelSupply(FuelSupplyNum)%MW = MWfuel
    FuelSupply(FuelSupplyNum)%KmolPerSecToKgPerSec =  MWfuel !TODO check this, guessing on conversion...
    FuelSupply(FuelSupplyNum)%HHV = 1000000.0d0 * HHVfuel  / MWfuel ! (1000/k) (1000/k) (kJ/mol)/(g/mol) = J/kg
    FuelSupply(FuelSupplyNum)%LHVJperkg = FuelSupply(FuelSupplyNum)%LHV * 1000000.0d0 / FuelSupply(FuelSupplyNum)%MW

  ELSEIF(FuelSupply(FuelSupplyNum)%FuelTypeMode == fuelModeGenericLiquid) THEN
    FuelSupply(FuelSupplyNum)%LHV = FuelSupply(FuelSupplyNum)%LHVliquid  &
                                    * FuelSupply(FuelSupplyNum)%MW /1000000.0d0 ! J/kg * g/mol (k/1000) (k/10000)

  ELSE

  ENDIF

 ! report Heating Values in EIO.
  WRITE(OutputFileInits, '(A)') '! <Fuel Supply>, Fuel Supply Name, Lower Heating Value [J/kmol], Lower Heating Value [kJ/kg], '  &
                                // 'Higher Heating Value [KJ/kg],  Molecular Weight [g/mol] '
  WRITE(OutputFileInits, 501) TRIM(FuelSupply(FuelSupplyNum)%Name) , FuelSupply(FuelSupplyNum)%LHV*1000000.0d0,  &
                               FuelSupply(FuelSupplyNum)%LHVJperkg/ 1000.0d0 , FuelSupply(FuelSupplyNum)%HHV / 1000.0d0 ,  &
                               FuelSupply(FuelSupplyNum)%MW

  501 FORMAT(' Fuel Supply, ', A, ',', G13.6E2, ',' , G13.6E2, ',' , G13.6E2, ',', G13.6E2)


  RETURN

END SUBROUTINE SetupFuelConstituentData

END MODULE GeneratorFuelSupply
!_____________________________________________________________________________________________________________________
MODULE GeneratorDynamicsManager

          ! Module containing the routines dealing with the management of dynamic constraints on Generator response

          ! MODULE INFORMATION:
          !       AUTHOR        B. Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! collect routines for managing generator states
          ! reused by different generator models
          !  determine response that generator is capable of providing
          !  given load request data
          !   models requiring calculations across timesteps

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataGenerators
USE DataGlobals,    ONLY: CurrentTime, DayOfSim, HoursInDay, SecInHour
USE DataInterfaces, ONLY: ShowFatalError, ShowContinueError
          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:
Public ManageGeneratorControlState
Public SetupGeneratorControlStateManager
PUBLIC ManageGeneratorFuelFlow
PUBLIC FuncDetermineCWMdotForInternalFlowControl

CONTAINS

SUBROUTINE SetupGeneratorControlStateManager(GenNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! sets up data structures

          ! METHODOLOGY EMPLOYED:
          ! like a get input routine but feeds from
          !  parent objects, could have its own input object someday
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: GenNum ! index of generator to setup

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


! get the number of generators that might use this module
NumGensWDynamics = NumMicroCHPs !TODO  + NumFuelCellCGenerators

IF (.not. ALLOCATED(GeneratorDynamics)) Then
  ALLOCATE(GeneratorDynamics(NumGensWDynamics))
ELSE
  DEALLOCATE(GeneratorDynamics)
  ALLOCATE(GeneratorDynamics(NumGensWDynamics))
ENDIF

!first populate with Micro CHP data

! DO GenNum = 1, NumMicroCHPs
    GeneratorDynamics(GenNum)%name                  = MicroCHP(GenNum)%Name
    GeneratorDynamics(GenNum)%PelMin                = MicroCHP(GenNum)%A42Model%MinElecPower
    GeneratorDynamics(GenNum)%PelMax                = MicroCHP(GenNum)%A42Model%MaxElecPower
    GeneratorDynamics(GenNum)%UpTranLimit           = MicroCHP(GenNum)%A42Model%DeltaPelMax
    GeneratorDynamics(GenNum)%DownTranLimit         = MicroCHP(GenNum)%A42Model%DeltaPelMax
    GeneratorDynamics(GenNum)%UpTranLimitFuel       = MicroCHP(GenNum)%A42Model%DeltaFuelMdotMax
    GeneratorDynamics(GenNum)%DownTranLimitFuel     = MicroCHP(GenNum)%A42Model%DeltaFuelMdotMax
    GeneratorDynamics(GenNum)%WarmUpByTimeDelay     = MicroCHP(GenNum)%A42Model%WarmUpByTimeDelay
    GeneratorDynamics(GenNum)%WarmUpByEngineTemp    = MicroCHP(GenNum)%A42Model%WarmUpByEngineTemp
    GeneratorDynamics(GenNum)%MandatoryFullCoolDown = MicroCHP(GenNum)%A42Model%MandatoryFullCoolDown
    GeneratorDynamics(GenNum)%WarmRestartOkay       = MicroCHP(GenNum)%A42Model%WarmRestartOkay
    GeneratorDynamics(GenNum)%WarmUpDelay           = MicroCHP(GenNum)%A42Model%WarmUpDelay
    GeneratorDynamics(GenNum)%CoolDownDelay         = MicroCHP(GenNum)%A42Model%CoolDownDelay / SecInHour ! seconds to hours
    GeneratorDynamics(GenNum)%PcoolDown             = MicroCHP(GenNum)%A42Model%PcoolDown
    GeneratorDynamics(GenNum)%Pstandby              = MicroCHP(GenNum)%A42Model%Pstandby
    GeneratorDynamics(GenNum)%MCeng                 = MicroCHP(GenNum)%A42Model%MCeng
    GeneratorDynamics(GenNum)%MCcw                  = MicroCHP(GenNum)%A42Model%MCcw
    GeneratorDynamics(GenNum)%kf                    = MicroCHP(GenNum)%A42Model%kf
    GeneratorDynamics(GenNum)%TnomEngOp             = MicroCHP(GenNum)%A42Model%TnomEngOp
    GeneratorDynamics(GenNum)%kp                    = MicroCHP(GenNum)%A42Model%kp
    GeneratorDynamics(GenNum)%AvailabilitySchedID   = MicroCHP(GenNum)%AvailabilitySchedID
    GeneratorDynamics(GenNum)%StartUpTimeDelay      = MicroCHP(GenNum)%A42Model%WarmUpDelay / SecInHour ! seconds to hours

    GeneratorDynamics(GenNum)%ElectEffNom           = MicroCHP(GenNum)%A42Model%ElecEff
    GeneratorDynamics(GenNum)%ThermEffNom           = MicroCHP(GenNum)%A42Model%ThermEff
    GeneratorDynamics(GenNum)%QdotHXMax             = MicroCHP(GenNum)%A42Model%ThermEff          &
                                                        * MicroCHP(GenNum)%A42Model%MaxElecPower  &
                                                        /  MicroCHP(GenNum)%A42Model%ElecEff
    GeneratorDynamics(GenNum)%QdotHXMin             = MicroCHP(GenNum)%A42Model%ThermEff          &
                                                        * MicroCHP(GenNum)%A42Model%MinElecPower  &
                                                        /  MicroCHP(GenNum)%A42Model%ElecEff
    GeneratorDynamics(GenNum)%QdotHXOpt             = GeneratorDynamics(GenNum)%QdotHXMax
    MicroCHP(GenNum)%DynamicsControlID              = GenNum
! ENDDO


  RETURN

END SUBROUTINE SetupGeneratorControlStateManager


SUBROUTINE ManageGeneratorControlState(GeneratorType, GeneratorName, GeneratorNum,RunFlagElectCenter, &
                                      RunFlagPlant,ElecLoadRequest, ThermalLoadRequest,               &
                                      ElecLoadProvided, OperatingMode, PLRforSubtimestepStartUp, PLRforSubtimestepShutDown , &
                                      FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   February-March 2007  (replaced July 2006 attempt)
          !       MODIFIED       Dec 2009, check and constrain with flow available from plant
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provide a service to other generators to make decisions, mostly temporal, or cross-timestep issues
          !  used to model internal controlling issues within an individual generator model
          !  This subroutine determines the current operating mode and returns the allowed power and
          ! and part load ratio for certain sub-time step switching e.g. in and out of normal mode or cool down mode

          ! METHODOLOGY EMPLOYED:
          ! model controls-related issues, rules based algorithm
          ! Control decision results include:
          !     -- electrical load allowed/resulting/provided
          !     -- new operating mode
          !     -- part load this timestep for shift to normal mode occuring midway in timestep
          !     -- part load this timestep for shift out of cool down mode

          ! Input data used to make control decisions include:
          !     -- Electrical load request
          !     -- Thermal Load request
          !     -- RunFlagElectricCenter
          !     -- RunFlagPlant
          !     -- previous timestep operating mode
          !     -- previous timestep Power generated
          !     -- availability schedule (off if not available)
          !     -- Generator control parameter constants including
          !           ** Start Up Time Delay  (in hours)
          !           ** Cool-down time delay (in hours)
          !     -- Expected Plant flow rate
          !     -- minimum cooling water flow rate

          ! Algorithm summary
          !   1.  examine calling run flags and refine electric load request to account for
          !       thermal load requests (not yet ready for prime time)
          !   2.  Determine states of various control inputs that change during simulation
          !
          !   3.  enter case statement based on previous operating mode.
          !       --  decide on current operating mode
          !       --  calculate part loads

          !   4.  based on current operating mode determine allowed/provided electrical load
          !        a. set allowed elec load by mode
          !        b. set allowed elec load by constraints on rate of change
          !        c. set allowed elec load by min and max

          !   5.  Calculated part load ratios for special cases.
          !
          !
          ! REFERENCES:
          ! controls specifications in Annex 42 model specs.
          !
          ! USE STATEMENTS:
  USE DataGlobalConstants
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE ScheduleManager, ONLY: GetScheduleIndex, GetCurrentScheduleValue
  USE DataLoopNode ,   ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)         :: GeneratorType     ! type of Generator
  CHARACTER(len=*), INTENT(IN) :: GeneratorName     ! user specified name of Generator
  INTEGER, INTENT(IN)         :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)         :: RunFlagElectCenter  ! TRUE when Generator operating per electric load center request
  LOGICAL, INTENT(IN)         :: RunFlagPlant        ! TRUE when generator operating per Plant request (always false)
  REAL(r64)   , INTENT(IN)    :: ElecLoadRequest     ! Generator Electrical power demand
  REAL(r64)   , INTENT(IN)    :: ThermalLoadRequest  ! cogenerator Thermal power demand
  REAL(r64),    INTENT(OUT)   :: ElecLoadProvided  ! power allowed
  INTEGER, INTENT(OUT)        :: OperatingMode  ! operating mode
  REAL(r64)   , INTENT(OUT)   :: PLRforSubtimestepStartUp ! part load ratio for switch to normal from start up
                                                         ! this is the part in normal mode
  REAL(r64)   , INTENT(OUT)   :: PLRforSubtimestepShutDown ! part load ratio for switch from cool down to other
                                                      ! this is the part in cool down mode.
  LOGICAL, INTENT(IN)         :: FirstHVACIteration ! True is this is first HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: RunFlag ! true if generator supposed to run
  INTEGER :: DynaCntrlNum !index in GeneratorDynamics structure for this generator          ! na
  REAL(r64)    :: CurrentFractionalDay !working var, time in decimal days
  REAL(r64)    :: EndingFractionalDay  !working var, time is decimal days
  REAL(r64)    :: LastSystemTimeStepFractionalDay ! working var, time is decimal days
  REAL(r64)    :: MaxPel !working variable for max allowed by transient constraint
  REAL(r64)    :: MinPel !working variabel for min allowed by transient constraint
  REAL(r64)    :: PelInput !holds initial value of IN var
  REAL(r64)    :: Pel !
  INTEGER      :: newOpMode
  REAL(r64)    :: schedVal
!  REAL(r64)    :: PelDiff
  REAL(r64)    :: ElectLoadForThermalRequest
  LOGICAL :: ConstrainedMaxP ! true if request was altered because of max power limit
  LOGICAL :: ConstrainedMinP ! true if request was altered because of min power limit
  LOGICAL :: ConstrainedIncreasingPdot ! true if request was altered because of power rate of change up
  LOGICAL :: ConstrainedDecreasingPdot ! true if request was altered because of power rate of change down
  LOGICAL :: ConstrainedByPlant ! true if request was altered because of cooling water problem
  LOGICAL :: PLRStartUp  ! true if subtimestep issue involving startup
  LOGICAL :: PLRShutDown
!  INTEGER :: OutletCWnode        = 0 ! cooling water outlet node ID
  INTEGER :: InletCWnode         = 0 ! cooling water inlet node ID
  LOGICAL :: InternalFlowControl = .FALSE.
  REAL(r64) :: TcwIn             = 0.0D0 ! inlet cooling water temperature (C)
  REAL(r64) :: TrialMdotcw       = 0.0D0 ! test or estimate of what the plant flows are going to be (kg/s)
  REAL(r64) :: LimitMinMdotcw    = 0.0D0 ! lower limit for cooling water flow for generatior operation (kg/s)

  ! inits
  PLRforSubtimestepStartUp  = 1.0d0
  PLRforSubtimestepShutDown = 0.0d0
  ConstrainedMaxP           = .false.
  ConstrainedMinP           = .false.
  ConstrainedIncreasingPdot = .false.
  ConstrainedDecreasingPdot = .false.
  ConstrainedByPlant        = .false.
  PLRStartUp                = .false.
  PLRShutDown               = .false.
  InternalFlowControl       = .FALSE.

  ! get index for this generator in dynamics control structure
  SELECT CASE (GeneratorType)
  CASE (iGeneratorMicroCHP)
    DynaCntrlNum = MicroCHP(GeneratorNum)%DynamicsControlID
   ! OutletCWnode = MicroCHP(GeneratorNum)%PlantOutletNodeID
    InletCWnode  = MicroCHP(GeneratorNum)%PlantInletNodeID
    TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp
    IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
      InternalFlowControl = .TRUE.
    ENDIF
    LimitMinMdotcw = MicroCHP(GeneratorNum)%A42Model%MinWaterMdot
  CASE (iGeneratorFuelCell)
     ! not yet
  CASE DEFAULT
  END SELECT

  PelInput = ElecLoadRequest
  ElectLoadForThermalRequest = 0.0d0
  IF ((ThermalLoadRequest > 0.0d0) .AND. RunFlagPlant) THEN  ! deal with possible thermal load following
  !Modify electric load request based on thermal load following signal using nominal efficiencies
    ElectLoadForThermalRequest = GeneratorDynamics(DynaCntrlNum)%ThermEffNom &
                               * ThermalLoadRequest / GeneratorDynamics(DynaCntrlNum)%ElectEffNom
    PelInput = Max(PelInput, ElectLoadForThermalRequest)

  ENDIF

  IF ((RunFlagElectCenter) .OR. (RunFlagPlant)) then
    RunFlag = .true.
  ELSE
    RunFlag = .false.
  ENDIF

  ! check availability schedule
  schedval  = GetCurrentScheduleValue(GeneratorDynamics(DynaCntrlNum)%AvailabilitySchedID)

  Pel      = PelInput

  ! get data to check if sufficient flow available from Plant
  IF (InternalFlowControl .AND. (schedval > 0.d0) ) THEN
    TrialMdotcw = FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pel, TcwIn)
  ELSE
    TrialMdotcw = Node(InletCWnode)%MassFlowRate
  ENDIF

  !determine current operating mode.
  SELECT CASE (GeneratorDynamics(DynaCntrlNum)%LastOpMode)

  CASE(OpModeOFF, OpModeStandby)
    ! possible future states {Off, Standby, WarmUp,Normal }
     IF (schedval == 0.0d0) THEN
       newOpMode = OpModeOFF

     ELSEIF (((schedval /= 0.0d0)  .AND. ( .NOT. RunFlag)) .OR. (TrialMdotcw < LimitMinMdotcw)) THEN
       newOpMode = OpModeStandby
     ELSEIF ((schedval /= 0.0d0)  .AND. (Runflag) ) THEN

       IF (GeneratorDynamics(DynaCntrlNum)%WarmUpByTimeDelay) THEN

         If (GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay == 0.0d0) Then
           newOpMode = OpModeNormal

         ! is startUp time delay longer than timestep?
         ELSEIf (GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay >= TimeStepSys) THEN !
           newOpMode = OpModeWarmUp
           ! generator just started so set start time
           GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastStartUp = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime) - TimeStepSys )))/HoursInDay

         ELSE  ! warm up period is less than a single system time step
           newOpMode = OpModeNormal
           PLRStartUp = .true.
           PLRforSubtimestepStartUp = (TimeStepSys - GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay) / TimeStepSys
         ENDIF
       ENDIF
       IF (GeneratorDynamics(DynaCntrlNum)%WarmUpByEngineTemp) THEN
         IF (MicroCHP(GeneratorNum)%A42Model%Teng  >= GeneratorDynamics(DynaCntrlNum)%TnomEngOp ) THEN
           newOpMode = OpModeNormal
           ! assume linear interpolation for PLR
           PLRStartUp = .true.
           IF  ( (MicroCHP(GeneratorNum)%A42Model%Teng - MicroCHP(GeneratorNum)%A42Model%TengLast) > 0.0d0 ) THEN
               ! protect divide by zero or neg
             PLRforSubtimestepStartUp = (MicroCHP(GeneratorNum)%A42Model%Teng - GeneratorDynamics(DynaCntrlNum)%TnomEngOp ) &
                                       /  (MicroCHP(GeneratorNum)%A42Model%Teng - MicroCHP(GeneratorNum)%A42Model%TengLast)
           ELSE
             PLRforSubtimestepStartUp = 1.0d0
           ENDIF
         ELSE
           newOpMode = OpModeWarmUp
         ENDIF
       ENDIF

     ENDIF

  CASE(OpModeWarmUp)
    ! possible Future states {OFF, WarmUp, Normal, CoolDown }
        ! check availability manager
     IF (schedval == 0.0d0) THEN
       ! to off unless cool down time period is needed
       IF (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay == 0.0d0) THEN
         newOpMode = OpModeOFF
       ELSE
         IF (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay > TimeStepSys) THEN
           newOpMode = OpModeCoolDown
                      ! need to reset time of last shut down here
           GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
         ELSE
           newOpMode = OpModeOFF
         ENDIF
       ENDIF
     ELSEIF (((schedval /= 0.0d0)  .AND. ( .NOT. RunFlag)) .OR. (TrialMdotcw < LimitMinMdotcw)) THEN
       ! to standby unless cool down time period is needed
       IF (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay == 0.0d0) THEN
         newOpMode = OpModeStandby
       ELSE
         IF (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay > TimeStepSys) THEN
           newOpMode = OpModeCoolDown
           ! need to reset time of last shut down here
           GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay

         ELSE
           newOpMode = OpModeStandby
           ! assuming no PLR situation unless engine made to normal operation.
         ENDIF
       ENDIF
     ELSEIF ((schedval /= 0.0d0)  .AND. (RunFlag)) THEN
       ! either warm up or normal
       ! check if warm up completed, depends on type of warm up control time delay or reach nominal temperature
       IF (GeneratorDynamics(DynaCntrlNum)%WarmUpByTimeDelay) THEN
         ! compare current time to when warm up is over
          !calculate time for end of warmup period
         CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
         EndingFractionalDay = GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastStartUp  &
                               + GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay/HoursInDay
         IF (( ABS(CurrentFractionalDay - EndingFractionalDay) < 0.000001d0)  &
                .or. (CurrentFractionalDay > EndingFractionalDay)) THEN
           newOpMode = OpModeNormal
           PLRStartUp = .true.
           LastSystemTimeStepFractionalDay = CurrentFractionalDay - ( TimeStepSys/HoursInDay )
           PLRforSubtimestepStartUp = ( (CurrentFractionalDay - EndingFractionalDay ) &
                                      / (CurrentFractionalDay - LastSystemTimeStepFractionalDay) )
         ELSE
           newOpMode = OpModeWarmUp
         ENDIF

       ELSEIF (GeneratorDynamics(DynaCntrlNum)%WarmUpByEngineTemp) THEN
         IF (GeneratorType == iGeneratorMicroCHP) THEN
           !only change to normal if this is result from completed timestep, not just an interation
           IF (MicroCHP(GeneratorNum)%A42Model%TengLast  >= GeneratorDynamics(DynaCntrlNum)%TnomEngOp ) THEN
             newOpMode = OpModeNormal
             ! assume linear interpolation for PLR
             PLRStartUp = .true.
             IF  ( (MicroCHP(GeneratorNum)%A42Model%Teng - MicroCHP(GeneratorNum)%A42Model%TengLast) > 0.0d0 ) THEN
                 ! protect divide by zero or neg
               PLRforSubtimestepStartUp = (MicroCHP(GeneratorNum)%A42Model%Teng - GeneratorDynamics(DynaCntrlNum)%TnomEngOp ) &
                                       /  (MicroCHP(GeneratorNum)%A42Model%Teng - MicroCHP(GeneratorNum)%A42Model%TengLast)
             ELSE
               PLRforSubtimestepStartUp = 1.0d0
             ENDIF
           ELSE
             newOpMode = OpModeWarmUp
           ENDIF
         ENDIF
       ELSE
         ! shouldn't come here
        ! Write(*,*) 'problem with warm up type of control logical flags'
       ENDif

     ENDIF

  CASE(OpModeNormal)
   !possible Future states {CoolDown, standby, off}
     IF (((schedval == 0.0d0) .OR. ( .NOT. RunFlag)).OR. (TrialMdotcw < LimitMinMdotcw)) THEN
        ! is cool down time delay longer than timestep?
       IF (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay == 0.0d0) THEN
         If (schedval /= 0.0d0) then
           newOpMode = OpModeStandBy
         else
           newOpMode = OpModeOff
         endif
       ElseIf (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay >= TimeStepSys) THEN !
         newOpMode = OpModeCoolDown
         ! also, generator just shut down so record shut down time
         GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
       ELSE  ! cool down period is less than a single system time step
         If (schedval /= 0.0d0) then
           newOpMode = OpModeStandBy
         else
           newOpMode = OpModeOff
         endif
         PLRShutDown = .true.
         PLRforSubtimestepShutdown = (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay) / TimeStepSys

                  ! also, generator just shut down so record shut down time
         GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
       ENDIF
     ELSEIF ((schedval /= 0.0d0)  .AND. ( RunFlag)) THEN


       newOpMode = OpModeNormal
     ENDIF

  CASE(opModeCoolDown)
   !possible Future States {Standby, OFF, WarmUp, Normal}

     IF (schedval == 0.0d0)  THEN ! no longer available.
       ! probably goes to off but could be stuck in cool down for awhile
       If (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay > 0.0d0) Then
         ! calculate time for end of cool down period
         CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
         EndingFractionalDay = GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown  &
                               + GeneratorDynamics(DynaCntrlNum)%CoolDownDelay/HoursInDay - ( TimeStepSys/HoursInDay )
         IF (( ABS(CurrentFractionalDay - EndingFractionalDay) < 0.000001d0)  & ! CurrentFractionalDay == EndingFractionalDay
                .or. (CurrentFractionalDay > EndingFractionalDay)) THEN
           newOpMode = opModeOFF
           PLRShutDown = .true.
           LastSystemTimeStepFractionalDay = CurrentFractionalDay - ( TimeStepSys/HoursInDay )
           PLRforSubtimestepShutDown  = (EndingFractionalDay - LastSystemTimeStepFractionalDay)*HoursInDay  &
                                        / TimeStepSys
         ELSE ! CurrentFractionalDay > EndingFractionalDay
           newOpMode = opModeCoolDown
         ENDIF
       ELSE !
         newOpMode = opModeOFF
       ENDIF
     ELSEIF (((schedval /= 0.0d0)  .AND. ( .NOT. RunFlag) ) .OR. (TrialMdotcw < LimitMinMdotcw))Then
       ! probably goes to standby but could be stuck in cool down for awhile
       If (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay > 0.0d0) Then
         ! calculate time for end of cool down period
         CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
         EndingFractionalDay = GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown  &
                               + GeneratorDynamics(DynaCntrlNum)%CoolDownDelay/HoursInDay - ( TimeStepSys /HoursInDay )
         IF (( ABS(CurrentFractionalDay - EndingFractionalDay) < 0.000001d0)  & ! CurrentFractionalDay == EndingFractionalDay
                .or. (CurrentFractionalDay > EndingFractionalDay)) THEN
           newOpMode = OpModeStandby
           PLRShutDown = .true.
           LastSystemTimeStepFractionalDay = CurrentFractionalDay - ( TimeStepSys/HoursInDay )
           PLRforSubtimestepShutDown  = (EndingFractionalDay - LastSystemTimeStepFractionalDay)*HoursInDay  &
                                        / TimeStepSys
         ELSE ! CurrentFractionalDay < EndingFractionalDay
           newOpMode = opModeCoolDown
         ENDIF
       ELSE !
         newOpMode = OpModeStandby
       ENDIF
     ELSEIF ((schedval /= 0.0d0)  .AND. ( RunFlag) ) THEN
       ! was in cool down mode but is now being asked to restart
       ! probably goes to warm up but could be stuck in cool down or jump to normal
       IF (GeneratorDynamics(DynaCntrlNum)%MandatoryFullCoolDown) then
         ! is cool down done or not?
         If (GeneratorDynamics(DynaCntrlNum)%CoolDownDelay > 0.0d0) Then
           ! calculate time for end of cool down period
           CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
           EndingFractionalDay = GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown  &
                               + GeneratorDynamics(DynaCntrlNum)%CoolDownDelay/HoursInDay - ( TimeStepSys/HoursInDay )
           IF (( ABS(CurrentFractionalDay - EndingFractionalDay) < 0.000001d0)  & ! CurrentFractionalDay == EndingFractionalDay
                .or. (CurrentFractionalDay < EndingFractionalDay)) THEN

             newOpMode = opModeCoolDown
           ELSE ! CurrentFractionalDay > EndingFractionalDay
             ! could go to warm up or normal now
             PLRShutDown = .true.
             LastSystemTimeStepFractionalDay = CurrentFractionalDay - ( TimeStepSys/HoursInDay )
             PLRforSubtimestepShutDown  = (EndingFractionalDay - LastSystemTimeStepFractionalDay)*HoursInDay  &
                                        / TimeStepSys
             If (GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay == 0.0d0) then
               newOpMode = opModeNormal
               ! possible PLR on start up.
               PLRStartUp = .true.
               PLRforSubtimestepStartUp = ( (CurrentFractionalDay - EndingFractionalDay ) &
                                      / (CurrentFractionalDay - LastSystemTimeStepFractionalDay) )

             ELSEIF (GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay > 0.0d0) then
               ! is remaining time enough?
               IF (( CurrentFractionalDay - EndingFractionalDay) > GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay ) THEN
                 newOpMode = opModeNormal
                 ! possible PLR on start up.
                 PLRStartUp = .true.
                 PLRforSubtimestepStartUp = ( (CurrentFractionalDay - EndingFractionalDay ) &
                                      / (CurrentFractionalDay - LastSystemTimeStepFractionalDay) )
               ELSE
                 newOpMode = OpModeWarmUp
                 ! generator just started so set start time
                 GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastStartUp = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime) - TimeStepSys )))/HoursInDay

               ENDIF
             ENDIF
           ENDIF
         ELSE !

           newOpMode = OpModeStandby
         ENDIF
       ELSE !not mandetory cool donw
         ! likely to go into warm up but if no warm up then back to normal
         IF (GeneratorDynamics(DynaCntrlNum)%WarmUpByTimeDelay) THEN
           If (GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay == 0.0d0) then
             newOpMode = opModeNormal

           ELSEIF (GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay > 0.0d0) then
             CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
             EndingFractionalDay = GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown  &
                               + GeneratorDynamics(DynaCntrlNum)%CoolDownDelay/HoursInDay
             IF (( ABS(CurrentFractionalDay - EndingFractionalDay) < 0.000001d0)  & ! CurrentFractionalDay == EndingFractionalDay
                .or. (CurrentFractionalDay > EndingFractionalDay)) THEN
               newOpMode = opModeNormal
                 ! possible PLR on start up.
               PLRStartUp = .true.
               LastSystemTimeStepFractionalDay = CurrentFractionalDay - ( TimeStepSys/HoursInDay )
               PLRforSubtimestepStartUp = ( (CurrentFractionalDay - EndingFractionalDay ) &
                                      / (CurrentFractionalDay - LastSystemTimeStepFractionalDay) )
             ELSE
               newOpMode = OpModeWarmUp
               ! set start up time
               ! generator just started so set start time
               GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastStartUp = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime) - TimeStepSys )))/HoursInDay

             ENDIF

            ENDIF
          ENDIF
       ENDIF

     ENDIF
  END SELECT !previous case

  If (PLRforSubtimestepStartUp < 0.0d0) PLRforSubtimestepStartUp = 0.0d0
  If (PLRforSubtimestepStartUp > 1.0d0) PLRforSubtimestepStartUp = 1.0d0

  If (PLRforSubtimestepShutDown < 0.0d0) PLRforSubtimestepShutDown = 0.0d0
  If (PLRforSubtimestepShutDown > 1.0d0) PLRforSubtimestepShutDown = 1.0d0

  IF (newOpmode == OpModeWarmUp) THEN
    SELECT CASE (GeneratorType)

    CASE( iGeneratorFuelCell  )
        !constant power level during start up (modeling artifact)
          !? hours or seconds here?
      Pel = GeneratorDynamics(DynaCntrlNum)%StartUpElectProd/GeneratorDynamics(DynaCntrlNum)%StartUpTimeDelay

    CASE ( iGeneratorMicroCHP)


      Pel =    PelInput * PLRforSubtimestepStartUp

    END SELECT
  ENDIF


  If (newOpmode == OpModeNormal) then
    ! correct if switched to normal at sub timestep
    Pel = Pel * PLRforSubtimestepStartUp
    !unit may have constraints from transient limits or operating ranges.
    IF (Pel > GeneratorDynamics(DynaCntrlNum)%PelLastTimeStep) THEN ! powering up
      MaxPel = GeneratorDynamics(DynaCntrlNum)%PelLastTimeStep  &
               + GeneratorDynamics(DynaCntrlNum)%UpTranLimit * TimeStepSys *  SecInHour
      IF (MaxPel < Pel) THEN
        Pel = MaxPel
      ENDIF
    ELSEIF (Pel< GeneratorDynamics(DynaCntrlNum)%PelLastTimeStep) THEN !powering down
      MinPel = GeneratorDynamics(DynaCntrlNum)%PelLastTimeStep &
                - GeneratorDynamics(DynaCntrlNum)%DownTranLimit * TimeStepSys *  SecInHour
      IF (Pel < MinPel) THEN
        Pel = MinPel
      ENDIF
    ENDIF

  ENDIF !

  IF (newOpmode == opModeCoolDown) THEN
    Pel = 0.0d0 ! assumes no power generated during shut down
  ENDIF

  IF (newOpmode == OpModeOFF) THEN
    Pel = 0.0d0 ! assumes no power generated during OFF mode
  ENDIF

  IF (newOpmode == OpModeStandby) THEN
    Pel = 0.0d0 ! assumes no power generated during standby mode
  ENDIF

  ! Control step 3: adjust for max and min limits on Pel

  IF ( Pel < GeneratorDynamics(DynaCntrlNum)%PelMin) THEN
   Pel = GeneratorDynamics(DynaCntrlNum)%PelMin
  ENDIF
  IF ( Pel > GeneratorDynamics(DynaCntrlNum)%PelMax) THEN
   Pel = GeneratorDynamics(DynaCntrlNum)%PelMax

  ENDIF

  !now do record keeping for amount of time spent in various operating modes
  SELECT CASE (GeneratorType)
  CASE (iGeneratorMicroCHP)
    ! first clear out values
    MicroCHP(GeneratorNum)%A42model%OffModeTime       = 0.0d0
    MicroCHP(GeneratorNum)%A42model%StandyByModeTime  = 0.0d0
    MicroCHP(GeneratorNum)%A42model%WarmUpModeTime    = 0.0d0
    MicroCHP(GeneratorNum)%A42model%NormalModeTime    = 0.0d0
    MicroCHP(GeneratorNum)%A42model%CoolDownModeTime  = 0.0d0
    SELECT CASE (newOpMode)

    CASE (OpModeOFF)
      IF (PLRforSubtimestepShutDown == 0.0d0) THEN
        MicroCHP(GeneratorNum)%A42model%OffModeTime  = TimeStepSys *  SecInHour
      ELSEIF ( (PLRforSubtimestepShutDown > 0.0d0) .AND. (PLRforSubtimestepShutDown < 1.0d0)) THEN
        MicroCHP(GeneratorNum)%A42model%CoolDownModeTime  = TimeStepSys *  SecInHour * (PLRforSubtimestepShutDown )
        MicroCHP(GeneratorNum)%A42model%OffModeTime  = TimeStepSys *  SecInHour * (1.0d0 - PLRforSubtimestepShutDown )
      ELSE
        MicroCHP(GeneratorNum)%A42model%OffModeTime = TimeStepSys *  SecInHour
      ENDIF
    CASE (OpModeStandby)
      IF (PLRforSubtimestepShutDown == 0.0d0) THEN
        MicroCHP(GeneratorNum)%A42model%StandyByModeTime  = TimeStepSys *  SecInHour
      ELSEIF ( (PLRforSubtimestepShutDown > 0.0d0) .AND. (PLRforSubtimestepShutDown < 1.0d0)) THEN
        MicroCHP(GeneratorNum)%A42model%CoolDownModeTime  = TimeStepSys *  SecInHour * (PLRforSubtimestepShutDown )
        MicroCHP(GeneratorNum)%A42model%StandyByModeTime  = TimeStepSys *  SecInHour * (1.0d0 - PLRforSubtimestepShutDown )
      ELSE
        MicroCHP(GeneratorNum)%A42model%StandyByModeTime = TimeStepSys *  SecInHour
      ENDIF
    CASE (OpModeWarmUp)
      IF (PLRforSubtimestepShutDown == 0.0d0) THEN
        MicroCHP(GeneratorNum)%A42model%WarmUpModeTime  = TimeStepSys *  SecInHour
      ELSEIF ( (PLRforSubtimestepShutDown > 0.0d0) .AND. (PLRforSubtimestepShutDown < 1.0d0)) THEN
        MicroCHP(GeneratorNum)%A42model%CoolDownModeTime  = TimeStepSys *  SecInHour * (PLRforSubtimestepShutDown )
        MicroCHP(GeneratorNum)%A42model%WarmUpModeTime  = TimeStepSys *  SecInHour * (1.0d0 - PLRforSubtimestepShutDown )
      ELSE
        MicroCHP(GeneratorNum)%A42model%WarmUpModeTime  = TimeStepSys *  SecInHour
      ENDIF

    CASE (OpModeNormal)
      IF (PLRforSubtimestepStartUp == 0.0d0) THEN
        MicroCHP(GeneratorNum)%A42model%WarmUpModeTime  = TimeStepSys *  SecInHour

      ELSEIF ( (PLRforSubtimestepStartUp > 0.0d0) .AND. (PLRforSubtimestepStartUp < 1.0d0)) THEN
        MicroCHP(GeneratorNum)%A42model%WarmUpModeTime  = TimeStepSys *  SecInHour * ( 1.0d0 -PLRforSubtimestepStartUp )
        MicroCHP(GeneratorNum)%A42model%NormalModeTime  = TimeStepSys *  SecInHour * ( PLRforSubtimestepStartUp )

      ELSE
        IF (PLRforSubtimestepShutDown == 0.0d0) THEN
          MicroCHP(GeneratorNum)%A42model%NormalModeTime  = TimeStepSys *  SecInHour
        ELSEIF ( (PLRforSubtimestepShutDown > 0.0d0) .AND. (PLRforSubtimestepShutDown < 1.0d0)) THEN
          MicroCHP(GeneratorNum)%A42model%CoolDownModeTime  = TimeStepSys *  SecInHour * (PLRforSubtimestepShutDown )
          MicroCHP(GeneratorNum)%A42model%NormalModeTime  = TimeStepSys *  SecInHour * (1.0d0 - PLRforSubtimestepShutDown )
        ELSE
          MicroCHP(GeneratorNum)%A42model%NormalModeTime  = TimeStepSys *  SecInHour
        ENDIF
      ENDIF

    CASE (opModeCoolDown)

      MicroCHP(GeneratorNum)%A42model%CoolDownModeTime  = TimeStepSys *  SecInHour
    END SELECT

  CASE (iGeneratorFuelCell)
     ! not yet using this control manager
  CASE DEFAULT
  END SELECT



  ElecLoadProvided = Pel

  GeneratorDynamics(DynaCntrlNum)%CurrentOpMode = newOpMode
  OperatingMode = newOpMode

  RETURN

END SUBROUTINE ManageGeneratorControlState


SUBROUTINE ManageGeneratorFuelFlow(GeneratorType, GeneratorName, GeneratorNum,RunFlag,FuelFlowRequest, &
                                      FuelFlowProvided, ConstrainedIncreasingMdot, ConstrainedDecreasingMdot)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! check if change in fuel flow rate is okay

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataGlobalConstants
  USE DataHVACGlobals, only:TimeStepSys
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GeneratorType     ! type of Generator
  CHARACTER(len=*), INTENT(IN) :: GeneratorName     ! user specified name of Generator
  INTEGER, INTENT(IN)    :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when Generator operating
  REAL(r64)  , INTENT(IN)     :: FuelFlowRequest    ! Generator demand mdot kg/ s
  REAL(r64),    INTENT(OUT)   :: FuelFlowProvided   ! allowed after constraints kg/s
  LOGICAL, INTENT(OUT)   :: ConstrainedIncreasingMdot ! true if request was altered because of fuel rate of change up
  LOGICAL, INTENT(OUT)   :: ConstrainedDecreasingMdot ! true if request was altered because of fuel rate of change down

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 REAL(r64)    :: MdotFuel
 REAL(r64)    :: MaxMdot
 REAL(r64)    :: MinMdot
 INTEGER :: DynaCntrlNum

 ConstrainedIncreasingMdot = .false.
 ConstrainedDecreasingMdot = .false.
 MdotFuel = FuelFlowRequest

 ! get index from GeneratorNum
SELECT CASE (GeneratorType)
CASE (iGeneratorMicroCHP)
     DynaCntrlNum = MicroCHP(GeneratorNum)%DynamicsControlID

END SELECT

IF (FuelFlowRequest > GeneratorDynamics(DynaCntrlNum)%FuelMdotLastTimestep) THEN ! fuel flow is up
  MaxMdot = GeneratorDynamics(DynaCntrlNum)%FuelMdotLastTimestep  &
            + GeneratorDynamics(DynaCntrlNum)%UpTranLimitFuel * TimeStepSys *  SecInHour
  IF (MaxMdot < FuelFlowRequest ) THEN
     MdotFuel = MaxMdot
     ConstrainedIncreasingMdot = .true.
  ENDIF

ELSEIF (FuelFlowRequest < GeneratorDynamics(DynaCntrlNum)%FuelMdotLastTimestep) THEN ! fuel flow is down
  MinMdot = GeneratorDynamics(DynaCntrlNum)%FuelMdotLastTimestep  &
            - GeneratorDynamics(DynaCntrlNum)%DownTranLimitFuel * TimeStepSys *  SecInHour
  IF (FuelFlowRequest < MinMdot ) THEN
     MdotFuel = MinMdot
     ConstrainedDecreasingMdot = .true.
  ENDIF
ELSE
 ! do nothing
ENDIF

FuelFlowProvided = MdotFuel

RETURN

END SUBROUTINE ManageGeneratorFuelFlow

REAL(r64) FUNCTION FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, Sept 2010, plant upgrade

          ! PURPOSE OF THIS FUNCTION:
          ! common place to figure flow rates with internal flow control

          ! METHODOLOGY EMPLOYED:
          ! apply contraints imposed by plant according to flow lock, first HVAC iteration etc.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,   ONLY: CurveValue
  USE DataPlant,      ONLY: PlantLoop
  USE DataLoopNode,   ONLY: Node
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: GeneratorNum ! ID of generator
  REAL(r64), INTENT(IN) :: Pnetss ! power net steady state
  REAL(r64), INTENT(IN) :: TcwIn ! temperature of cooling water at inlet

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MdotCW
  INTEGER   :: InletNode
  INTEGER   :: OutletNode

  InletNode = MicroCHP(GeneratorNum)%PlantInletNodeID
  OutletNode = MicroCHP(GeneratorNum)%PlantOutletNodeID

  ! first evaluate curve
  Mdotcw = CurveValue(MicroCHP(GeneratorNum)%A42Model%WaterFlowCurveID, Pnetss, TcwIn)

  ! now apply constraints
  Mdotcw  = MAX(0.0D0, Mdotcw)

  !make sure plant can provide, utility call may change flow
  IF (MicroCHP(GeneratorNum)%CWLoopNum > 0) THEN ! protect early calls
    CALL SetComponentFlowRate(Mdotcw,InletNode,OutletNode,  &
                       MicroCHP(GeneratorNum)%CWLoopNum,     &
                       MicroCHP(GeneratorNum)%CWLoopSideNum, &
                       MicroCHP(GeneratorNum)%CWBranchNum,   &
                       MicroCHP(GeneratorNum)%CWCompNum)
  ENDIF

  FuncDetermineCWMdotForInternalFlowControl = Mdotcw
  RETURN

END FUNCTION FuncDetermineCWMdotForInternalFlowControl

END MODULE GeneratorDynamicsManager

!************************************************************************************
MODULE MicroCHPElectricGenerator

          ! MODULE INFORMATION:
          !       AUTHOR         Brent Griffth
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the operation of Internal Combustion and Stirling Engine
          !  residential-scale generators for combined heat and power.

          ! METHODOLOGY EMPLOYED:
          ! Once the ElectricPowerManager determines that the Combustion Generator
          ! is available to meet an electric load demand, it calls SimCombustionGenerator
          ! which in turn calls the Combustion model.
          ! See DataFuelCells.f90 for structures and variables

          ! REFERENCES:
          ! IEA/ECBCS Annex 42 model specification titled: " A Generic Model Specification for
          ! Combustion-based Residential CHP Devices"  Alex Ferguson, Nick Kelly, IEA Annex 42.
          !
          ! Module developed from

          ! OTHER NOTES:
          ! N/A

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGenerators
USE DataLoopNode
USE DataGlobals ,   ONLY : MaxNameLength, NumOfTimeStepInHour, NumOfZones, KelvinConv, HoursInDay, ScheduleAlwaysOn
USE DataInterfaces
USE DataGlobalConstants, ONLY: iGeneratorFuelCell
USE GeneratorDynamicsManager
USE GeneratorFuelSupply

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS


  ! DERIVED TYPE DEFINITIONS
LOGICAL     :: GetMicroCHPInput = .TRUE.! When TRUE, calls subroutine to read input file.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag

          ! SUBROUTINE SPECIFICATIONS FOR MODULE Combustion ElectricGenerator
PUBLIC     SimMicroCHPGenerator  ! call handler, gets input first time
PUBLIC     GetMicroCHPGeneratorResults

PRIVATE    GetMicroCHPGeneratorInput ! gathers user data from input processor

PRIVATE    InitMicroCHPNoNormalizeGenerators

PRIVATE    CalcMicroCHPNoNormalizeGeneratorModel ! main calculations for Combustion poer module
PUBLIC     FigureMicroCHPZoneGains
PRIVATE    CalcUpdateHeatRecovery !
PUBLIC     SimMicroCHPPlantHeatRecovery
PRIVATE    UpdateMicroCHPGeneratorRecords ! Sets variables used in reporting


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Combustion Generator Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimMicroCHPGenerator(GeneratorType,GeneratorName,GeneratorIndex,RunFlagElectCenter, RunFlagPlant,&
                                MyElectricLoad,MyThermalLoad, FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   July 2006
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the main driver for the model for
          ! internal combustion engine and
          ! Stirling cycle engine model from IEA/ECBCS Annex 42 r.  It
          ! gets the input for the models, initializes simulation variables, call
          ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE DataPlant,      ONLY: PlantSizeNotComplete

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GeneratorType     ! type of Generator
  CHARACTER(len=*), INTENT(IN) :: GeneratorName     ! user specified name of Generator
  INTEGER, INTENT(INOUT) :: GeneratorIndex
  LOGICAL , INTENT(IN)   :: RunFlagElectCenter  ! simulate Generator when TRUE
  LOGICAL , INTENT(IN)   :: RunFlagPlant  ! simulate generator when true.
  REAL(r64), INTENT(IN)  :: MyElectricLoad      ! demand on electric generator
  REAL(r64), INTENT(IN)       :: MyThermalLoad           ! thermal demand on cogenerator
  LOGICAL , INTENT(IN)   :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: GenNum           ! Generator number counter

        !Get Generator data from input file
  IF (GetMicroCHPInput) THEN
    CALL GetMicroCHPGeneratorInput
    GetMicroCHPInput = .FALSE.
  END IF

!  Call InitMicroCHPNoNormalizeGenerators

  IF (GeneratorIndex == 0) THEN
    GenNum = FindItemInList(GeneratorName,MicroCHP%Name,NumMicroCHPs)
    IF (GenNum == 0) CALL ShowFatalError('SimMicroCHPGenerator: Specified Generator not one of Valid Micro CHP Generators '//  &
                                       TRIM(GeneratorName))
    GeneratorIndex=GenNum
  ELSE
    GenNum=GeneratorIndex
    IF (GenNum > NumMicroCHPs .or. GenNum < 1) THEN
      CALL ShowFatalError('SimMicroCHPGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                          ', Number of Micro CHP Generators='//TRIM(TrimSigDigits(NumMicroCHPs))//  &
                          ', Generator name='//TRIM(GeneratorName))
    ENDIF
    IF (CheckEquipName(GenNum)) THEN
      IF (GeneratorName /= MicroCHP(GenNum)%Name) THEN
        CALL ShowFatalError('SimMicroCHPNoNormalizeGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                            ', Generator name='//TRIM(GeneratorName)//', stored Generator Name for that index='//  &
                            TRIM(MicroCHP(GenNum)%Name))
      ENDIF
      CheckEquipName(GenNum)=.false.
    ENDIF
  ENDIF

  If (MicroCHP(GenNum)%ModelTypeAnnex42) Then ! call the non normalize calc routines (set for future extension to normalize ones)

    CALL InitMicroCHPNoNormalizeGenerators(GenNum, FirstHVACIteration)
    IF (PlantSizeNotComplete) RETURN
    CALL CalcMicroCHPNoNormalizeGeneratorModel(GenNum,RunFlagElectCenter, RunFlagPlant,MyElectricLoad,  &
                                                  MyThermalLoad, FirstHVACIteration)

    CALL CalcUpdateHeatRecovery(GenNum, FirstHVACIteration)

    CALL UpdateMicroCHPGeneratorRecords(GenNum)

  ENDIF

RETURN
END SUBROUTINE SimMicroCHPGenerator

! End MicroCHPNoNormalize Generator Module Driver Subroutines
!******************************************************************************


! Beginning of Combustion Generator Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetMicroCHPGeneratorInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Brent Griffith
            !       DATE WRITTEN:    July 2005

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the Micro CHP Generator models.

            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataGenerators
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE CurveManager,   ONLY : GetCurveCheck, CurveValue
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataHeatBalance,  ONLY: Zone, IntGainTypeOf_GeneratorMicroCHP
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE General, ONLY: RoundSigDigits
  USE GeneratorFuelSupply
  USE GeneratorDynamicsManager

  IMPLICIT NONE !

  INTEGER                     :: GeneratorNum !Generator counter
  INTEGER                     :: NumAlphas    ! Number of elements in the alpha array
  INTEGER                     :: NumNums      ! Number of elements in the numeric array
  INTEGER                     :: IOStat       ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(25)  :: AlphArray !character string data
  REAL(r64),                        DIMENSION(200)  :: NumArray  !numeric data TODO deal with allocatable for extensible
  LOGICAL, SAVE :: ErrorsFound=.false.  ! error flag
  LOGICAL       :: IsNotOK              ! Flag to verify name
  LOGICAL       :: IsBlank              ! Flag for blank name
!  INTEGER       :: thisMicroCHP  !temporary index
!  INTEGER       :: otherMicroCHP !loop counter and temporary indexer
!  INTEGER       :: I   ! loop counter
  Logical, SAve :: myonetimeFlag = .true.
  INTEGER       :: CHPParamNum ! loop count and temporary index
  CHARACTER(len = 100) :: ObjMSGName ! string for error messages
  INTEGER       :: thisParamID

  ! execution
If (myonetimeflag) then

 ! call to Fuel supply module to set up data there.
  CALL GetGeneratorFuelSupplyInput

 ! First get the Micro CHP Parameters so they can be nested in structure later
  cCurrentModuleObject = 'Generator:MicroCHP:NonNormalizedParameters'
  NumMicroCHPParams = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumMicroCHPParams <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

  Allocate(MicroCHPParamInput(NumMicroCHPParams))
  Allocate(CheckEquipName(NumMicroCHPParams))
  CheckEquipName=.true.


  DO CHPParamNum = 1 , NumMicroCHPParams
    CALL GetObjectItem(cCurrentModuleObject,CHPParamNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!  Can't validate this name.
!    IsNotOK=.false.
!    IsBlank=.false.

!    CALL VerifyName(AlphArray(1),MicroCHP%Name,CHPParamNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject))
!    IF (IsNotOK) THEN
!      ErrorsFound=.true.
!      IF (IsBlank) AlphArray(1)='xxxxx'
!    ENDIF

    ObjMSGName = TRIM(cCurrentModuleObject)//' Named ' // TRIM(AlphArray(1))

    MicroCHPParamInput(CHPParamNum)%Name         = AlphArray(1)  !A1 name
    MicroCHPParamInput(CHPParamNum)%MaxElecPower = NumArray(1)   !N1 Maximum Electric Power [W]
    MicroCHPParamInput(CHPParamNum)%MinElecPower = NumArray(2)   !N2 Minimum Electric Power [W]
    MicroCHPParamInput(CHPParamNum)%MinWaterMdot = NumArray(3)   !N3 Minimum Cooling Water Flow Rate [kg/s]
    MicroCHPParamInput(CHPParamNum)%MaxWaterTemp = NumArray(4)   !N3 Maximum Cooling Water Inlet Temp [C]
    MicroCHPParamInput(CHPParamNum)%ElecEffCurveID  = GetCurveCheck(AlphArray(2), ErrorsFound,ObjMSGName ) !Electrical Eff. ID
    MicroCHPParamInput(CHPParamNum)%ThermalEffCurveID= GetCurveCheck(AlphArray(3), ErrorsFound,ObjMSGName) !Thermal Efficiency

    IF (SameString(AlphArray(4), 'InternalControl'))  THEN
      MicroCHPParamInput(CHPParamNum)%InternalFlowControl = .TRUE. !  A4, \field Cooling Water Flow Rate Mode
      MicroCHPParamInput(CHPParamNum)%PlantFlowControl = .false.
    ENDIF
    IF ( (.NOT. (SameString(AlphArray(4), 'InternalControl'))) .AND. ( .NOT. (SameSTring(AlphArray(4), 'PlantControl'))) ) THEN
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ENDIF
    If (MicroCHPParamInput(CHPParamNum)%InternalFlowControl) Then ! get the curve
      MicroCHPParamInput(CHPParamNum)%WaterFlowCurveID = GetCurveCheck(AlphArray(5), ErrorsFound, ObjMSGName )
                                                           !  Curve for Cooling Water Flow Rate
    ENDIF
    MicroCHPParamInput(CHPParamNum)%AirFlowCurveID    = GetCurveCheck(AlphArray(6), Errorsfound, ObjMSGName)
                                                                          !  Name of Curve for Air Flow Rate
    MicroCHPParamInput(CHPParamNum)%DeltaPelMax       = NumArray(5) ! N5 Maximum rate of change in net electrical power [W/s]
    MicroCHPParamInput(CHPParamNum)%DeltaFuelMdotMax  = NumArray(6) !N6 Maximum Rate of change in fuel flow rate [kg/s2]
    MicroCHPParamInput(CHPParamNum)%UAhx              = NumArray(7) ! N7 Heat Exchanger UA_hx
    MicroCHPParamInput(CHPParamNum)%UAskin            = NumArray(8) !N8 Skin Loss UA_loss
    MicroCHPParamInput(CHPParamNum)%RadiativeFraction = NumArray(9) !N9 radiative fraction for skin losses
    MicroCHPParamInput(CHPParamNum)%MCeng             = NumArray(10) ! N10 Aggregated Thermal Mass of Generator MC_eng
    IF (MicroCHPParamInput(CHPParamNum)%MCeng <= 0.d0) THEN
      CALL ShowSevereError('Invalid, '//TRIM(cNumericFieldNames(10))//' = '//TRIM(RoundSigDigits(NumArray(10), 5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Thermal mass must be greater than zero')
      ErrorsFound = .TRUE.
    ENDIF
    MicroCHPParamInput(CHPParamNum)%MCcw  = NumArray(11) ! Aggregated Thermal Mass of Heat Recovery MC_cw
    IF (MicroCHPParamInput(CHPParamNum)%MCcw  <= 0.d0) THEN
      CALL ShowSevereError('Invalid, '//TRIM(cNumericFieldNames(11))//' = '//TRIM(RoundSigDigits(NumArray(11), 5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Thermal mass must be greater than zero')
      ErrorsFound = .TRUE.
    ENDIF
    MicroCHPParamInput(CHPParamNum)%Pstandby = NumArray(12)  ! N12 Standby Power [W]

    IF (SameString(AlphArray(7), 'TimeDelay'))  THEN
      MicroCHPParamInput(CHPParamNum)%WarmUpByTimeDelay = .TRUE. !
      MicroCHPParamInput(CHPParamNum)%WarmUpByEngineTemp = .false.
    ENDIF
    IF ( (.NOT. (SameString(AlphArray(7), 'NominalEngineTemperature')))  &
           .AND. ( .NOT. (SameSTring(AlphArray(7), 'TimeDelay'))) ) THEN
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(AlphArray(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ENDIF
    MicroCHPParamInput(CHPParamNum)%kf  = NumArray(13) ! N13 Warmup Fuel Flow Rate Coefficient k_f
    MicroCHPParamInput(CHPParamNum)%TnomEngOp  = NumArray(14) ! N14 Nominal Engine Operating Temperature [C]
    MicroCHPParamInput(CHPParamNum)%kp  = NumArray(15) ! N15 Warmup Power Coefficient k_p
    MicroCHPParamInput(CHPParamNum)%Rfuelwarmup = NumArray(16) ! N16 Warm Up Fuel Flow Rate Limit Ratio
    MicroCHPParamInput(CHPParamNum)%WarmUpDelay  = NumArray(17) ! N17 Warm Up Delay Time

    MicroCHPParamInput(CHPParamNum)%PcoolDown  = NumArray(18)  ! N18 Cool Down Power

    MicroCHPParamInput(CHPParamNum)%CoolDownDelay = NumArray(19) ! N19 Cool Down Delay Time in seconds

    IF (SameString(AlphArray(8), 'MandatoryCoolDown'))  THEN
      MicroCHPParamInput(CHPParamNum)%MandatoryFullCoolDown = .TRUE.
      MicroCHPParamInput(CHPParamNum)%WarmRestartOkay = .false.
    ENDIF
    IF ( (.NOT. (SameString(AlphArray(8), 'MandatoryCoolDown')))  &
          .AND. ( .NOT. (SameSTring(AlphArray(8), 'OptionalCoolDown'))) ) THEN
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(AlphArray(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ENDIF
  ENDDO

  cCurrentModuleObject = 'Generator:MicroCHP'
  NumMicroCHPs = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumMicroCHPs <= 0) THEN
    ! shouldn't ever come here?
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

         !ALLOCATE ARRAYS

  IF (.not.(ALLOCATED(MicroCHP))) Then
    ALLOCATE (MicroCHP( NumMicroCHPs ))  ! inits handeled in derived type definitions
  ENDIF

  ! load in Micro CHPs
  DO GeneratorNum = 1 , NumMicroCHPs
    CALL GetObjectItem(cCurrentModuleObject,GeneratorNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),MicroCHP%Name,GeneratorNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
  !GENERATOR:MICRO CHP,
    MicroCHP(GeneratorNum)%Name = AlphArray(1) !  A1 Generator name
    ObjMSGName = TRIM(cCurrentModuleObject)//' Named ' // TRIM(alphArray(1))
    MicroCHP(GeneratorNum)%ParamObjName = AlphArray(2) !  A2 Micro CHP Parameter Object Name
    !find input structure
    thisParamID = FindItemInList(AlphArray(2), MicroCHPParamInput%Name, NumMicroCHPParams)
    IF (thisParamID /= 0) THEN
      MicroCHP(GeneratorNum)%A42Model =  MicroCHPParamInput(thisParamID) ! entire structure of input data assigned here!
    ELSE
       CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    ENDIF
 
    IF (.NOT. lAlphaFieldBlanks(3) ) THEN
      MicroCHP(GeneratorNum)%ZoneName     = AlphArray(3) !  A3 Zone Name
      MicroCHP(GeneratorNum)%ZoneID   = FindItemInList(MicroCHP(GeneratorNum)%ZoneName, Zone%Name, NumOfZones)
      IF (MicroCHP(GeneratorNum)%ZoneID == 0 ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound = .true.
      ENDIF
    ELSE
      MicroCHP(GeneratorNum)%ZoneID  =  0
    ENDIF
    MicroCHP(GeneratorNum)%PlantInletNodeName  = AlphArray(4)  !  A4 Cooling Water Inlet Node Name
    MicroCHP(GeneratorNum)%PlantOutletNodeName = AlphArray(5) !  A5 Cooling Water Outlet Node Name
    !find node ids for water path
    MicroCHP(GeneratorNum)%PlantInletNodeID =  &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent )
    MicroCHP(GeneratorNum)%PlantOutletNodeID =  &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent )
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),  &
                            'Heat Recovery Nodes')

    MicroCHP(GeneratorNum)%AirInletNodeName    = AlphArray(6) !  A6 Air Inlet Node Name
         ! check the node connections
    MicroCHP(GeneratorNum)%AirInletNodeId = &
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)

    MicroCHP(GeneratorNum)%AirOutletNodeName   = AlphArray(7) !  A7 Air Outlet Node Name
    MicroCHP(GeneratorNum)%AirOutletNodeId = &
               GetOnlySingleNode(AlphArray(7),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent)

    MicroCHP(GeneratorNum)%FuelSupplyID  = FindItemInList(AlphArray(8), FuelSupply%name,NumGeneratorFuelSups) ! Fuel Supply ID
    IF (MicroCHP(GeneratorNum)%FuelSupplyID == 0) THEN
       CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(AlphArray(8)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    ENDIF

    IF (lAlphaFieldBlanks(9)) THEN
      MicroCHP(GeneratorNum)%AvailabilitySchedID = ScheduleAlwaysOn
    ELSE
      MicroCHP(GeneratorNum)%AvailabilitySchedID = GetScheduleIndex(AlphArray(9))
      IF ( MicroCHP(GeneratorNum)%AvailabilitySchedID == 0) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(AlphArray(9)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound = .true.
      ENDIF
    ENDIF
    MicroCHP(GeneratorNum)%A42Model%TengLast      = 20.0D0  ! inits
    MicroCHP(GeneratorNum)%A42Model%TempCWOutLast = 20.0D0  ! inits

  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

!setup report variables
  DO GeneratorNum = 1, NumMicroCHPs

     CALL SetupOutputVariable('Generator Off Mode Time [s]', &
          MicroCHP(GeneratorNum)%Report%OffModeTime, 'System', 'Sum', MicroCHP(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Standby Mode Time [s]', &
          MicroCHP(GeneratorNum)%Report%StandyByModeTime, 'System', 'Sum', MicroCHP(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Warm Up Mode Time [s]', &
          MicroCHP(GeneratorNum)%Report%WarmUpModeTime, 'System', 'Sum', MicroCHP(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Normal Operating Mode Time [s]', &
          MicroCHP(GeneratorNum)%Report%NormalModeTime, 'System', 'Sum', MicroCHP(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Cool Down Mode Time [s]', &
          MicroCHP(GeneratorNum)%Report%CoolDownModeTime, 'System', 'Sum', MicroCHP(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator Produced Electric Power [W]', &
          MicroCHP(GeneratorNum)%Report%ACPowerGen,'System','Average',MicroCHP(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Produced Electric Energy [J]', &
          MicroCHP(GeneratorNum)%Report%ACEnergyGen,'System','Sum',MicroCHP(GeneratorNum)%Name, &
                           ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')
     CALL SetupOutputVariable('Generator Produced Thermal Rate [W]', & !
          MicroCHP(GeneratorNum)%report%QdotHR, 'system', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', & !
          MicroCHP(GeneratorNum)%report%TotalHeatEnergyRec, 'system', 'Sum', MicroCHP(GeneratorNum)%Name , &
          ResourceTypeKey='ENERGYTRANSFER' , EndUseKey='COGENERATION',GroupKey='Plant')

     CALL SetupOutputVariable('Generator Electric Efficiency []',  &
          MicroCHP(GeneratorNum)%Report%ElectEfficiency, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Thermal Efficiency []',  &
          MicroCHP(GeneratorNum)%Report%ThermalEfficiency, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Gross Input Heat Rate [W]', & !
          MicroCHP(GeneratorNum)%report%QdotGross, 'system', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Steady State Engine Heat Generation Rate [W]', & !
          MicroCHP(GeneratorNum)%report%Qgenss, 'system', 'Average', MicroCHP(GeneratorNum)%Name )


     CALL SetupOutputVariable('Generator Engine Heat Exchange Rate [W]', & !
          MicroCHP(GeneratorNum)%report%QdotHX, 'system', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Air Mass Flow Rate [kg/s]',  &
          MicroCHP(GeneratorNum)%Report%MdotAir, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Fuel Molar Flow Rate [kmol/s]' , &
          MicroCHP(GeneratorNum)%Report%NdotFuel, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Fuel Mass Flow Rate [kg/s]' , &
          MicroCHP(GeneratorNum)%Report%MdotFuel, 'System', 'Average', MicroCHP(GeneratorNum)%Name )

     CALL SetupOutputVariable('Generator Engine Temperature [C]' , &
          MicroCHP(GeneratorNum)%Report%Tengine, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Coolant Inlet Temperature [C]' , &
          MicroCHP(GeneratorNum)%Report%HeatRecInletTemp, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Coolant Outlet Temperature [C]' , &
          MicroCHP(GeneratorNum)%Report%HeatRecOutletTemp, 'System', 'Average', MicroCHP(GeneratorNum)%Name )

    ! this next one needs to be reconciled with non-gas fuel constituents.
    !   need custom resourceTypeKey or something for user defined fuel compositions.
     CALL SetupOutputVariable('Generator Fuel HHV Basis Energy [J]' , &
          MicroCHP(GeneratorNum)%Report%FuelEnergyHHV, 'System', 'Sum', MicroCHP(GeneratorNum)%Name , &
          ResourceTypeKey='Gas' , EndUseKey='COGENERATION', GroupKey='Plant')

     CALL SetupOutputVariable('Generator Fuel HHV Basis Rate [W]' , &
          MicroCHP(GeneratorNum)%Report%FuelEnergyUseRateHHV, 'System', 'Average', MicroCHP(GeneratorNum)%Name )

     CALL SetupOutputVariable('Generator Fuel LHV Basis Energy [J]' , &
          MicroCHP(GeneratorNum)%Report%FuelEnergyLHV, 'System', 'Sum', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Fuel LHV Basis Rate [W]' , &
          MicroCHP(GeneratorNum)%Report%FuelEnergyUseRateLHV, 'System', 'Average', MicroCHP(GeneratorNum)%Name )

     CALL SetupOutputVariable('Generator Fuel Compressor Electric Power [W]' , &
          MicroCHP(GeneratorNum)%Report%FuelCompressPower, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Fuel Compressor Electric Energy [J]',  &
          MicroCHP(GeneratorNum)%Report%FuelCompressEnergy, 'System', 'Sum', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Fuel Compressor Skin Heat Loss Rate [W]' , &
          MicroCHP(GeneratorNum)%Report%FuelCompressSkinLoss, 'System', 'Average', MicroCHP(GeneratorNum)%Name )

     CALL SetupOutputVariable('Generator Zone Sensible Heat Transfer Rate [W]' , &
          MicroCHP(GeneratorNum)%Report%SkinLossPower, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Zone Sensible Heat Transfer Energy [J]' , &
          MicroCHP(GeneratorNum)%Report%SkinLossEnergy, 'System', 'Sum', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Zone Convection Heat Transfer Rate [W]' , &
          MicroCHP(GeneratorNum)%Report%SkinLossConvect, 'System', 'Average', MicroCHP(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Zone Radiation Heat Transfer Rate [W]' , &
          MicroCHP(GeneratorNum)%Report%SkinLossRadiat, 'System', 'Average', MicroCHP(GeneratorNum)%Name )

     IF (MicroCHP(GeneratorNum)%ZoneID > 0) THEN
       CALL SetupZoneInternalGain(MicroCHP(GeneratorNum)%ZoneID, &
                   'Generator:MicroCHP',  &
                   MicroCHP(GeneratorNum)%Name, &
                   IntGainTypeOf_GeneratorMicroCHP,    &
                   ConvectionGainRate    = MicroCHP(GeneratorNum)%Report%SkinLossConvect, &
                   ThermalRadiationGainRate = MicroCHP(GeneratorNum)%Report%SkinLossRadiat)
     ENDIF

  END DO

myonetimeflag = .false.
ENDIF
RETURN

END SUBROUTINE GetMicroCHPGeneratorInput

            ! PARAMETERS

SUBROUTINE InitMicroCHPNoNormalizeGenerators(GeneratorNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   March 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! inits

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE DataGlobals    , ONLY: TimeStep, TimeStepZone, SecInHour, BeginEnvrnFlag, HourOfDay, &
                             SysSizingCalc, HoursInDay
  USE DataPlant,       ONLY: ScanPlantLoopsForObject, TypeOf_Generator_MicroCHP, PlantLoop, &
                             PlantSizeNotComplete, SupplySide, LoopFlowStatus_TakesWhatGets, &
                             PlantSizesOkayToFinalize, DemandSide
  USE DataSizing,      ONLY: PlantSizData
  USE PlantUtilities,  ONLY: SetComponentFlowRate, InitComponentNodes, RegisterPlantCompDesignFlow
  USE CurveManager,    ONLY: GetCurveCheck, CurveValue
  USE FluidProperties, ONLY: GetDensityGlycol
  USE GeneratorDynamicsManager

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: DynaCntrlNum = 0
  REAL(r64)    :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! Used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag

  LOGICAL :: errFlag
  REAL(r64)  :: mdot ! local temporary for mass flow rate
  REAL(r64)  :: rho  ! local temporary for fluid density

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumMicroCHPs))
    ALLOCATE(MyPlantScanFlag(NumMicroCHPs))
    ALLOCATE(MySizeFlag(NumMicroCHPs))
    MyEnvrnFlag     = .TRUE.
    MyPlantScanFlag = .TRUE.
    MySizeFlag      = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyPlantScanFlag(GeneratorNum) .AND. ALLOCATED(PlantLoop)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(MicroCHP(GeneratorNum)%Name, &
                                 TypeOf_Generator_MicroCHP, &
                                 MicroCHP(GeneratorNum)%CWLoopNum, &
                                 MicroCHP(GeneratorNum)%CWLoopSideNum, &
                                 MicroCHP(GeneratorNum)%CWBranchNum, &
                                 MicroCHP(GeneratorNum)%CWCompNum,   &
                                 errFlag=errFlag)

    IF (errFlag) THEN
      CALL ShowFatalError('InitMicroCHPNoNormalizeGenerators: Program terminated for previous conditions.')
    ENDIF

    IF (.NOT. MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
      !IF this is on the supply side and not internal flow control then reset flow priority to lower
      IF (MicroCHP(GeneratorNum)%CWLoopSideNum == SupplySide) THEN
        PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%LoopSide(MicroCHP(GeneratorNum)%CWLoopSideNum)&
          %Branch(MicroCHP(GeneratorNum)%CWBranchNum)%Comp(MicroCHP(GeneratorNum)%CWCompNum)%FlowPriority &
            = LoopFlowStatus_TakesWhatGets

      ENDIF

    ENDIF

    MyPlantScanFlag(GeneratorNum) = .FALSE.
  ENDIF


  IF (.NOT. SysSizingCalc .AND. MySizeFlag(GeneratorNum) .AND. .NOT. MyPlantScanFlag(GeneratorNum) &
       .AND. (PlantSizesOkayToFinalize) ) THEN
    rho = GetDensityGlycol(PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%FluidName, &
                           Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp, &
                           PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%FluidIndex, &
                          'InitMicroCHPNoNormalizeGenerators')
    IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN ! got a curve
      MicroCHP(GeneratorNum)%PlantMassFlowRateMax = 2.0d0 * CurveValue(MicroCHP(GeneratorNum)%A42Model%WaterFlowCurveID, &
                                                               MicroCHP(GeneratorNum)%A42Model%MaxElecPower ,    &
                                                               Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp )
    ELSEIF (MicroCHP(GeneratorNum)%CWLoopSideNum == SupplySide) THEN
      IF (PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%MaxMassFlowRate > 0.d0) THEN
        MicroCHP(GeneratorNum)%PlantMassFlowRateMax = PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%MaxMassFlowRate
      ELSEIF(PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%PlantSizNum > 0) THEN
        MicroCHP(GeneratorNum)%PlantMassFlowRateMax = PlantSizData(MicroCHP(GeneratorNum)%CWLoopNum)%DesVolFlowRate * rho
      ELSE
        MicroCHP(GeneratorNum)%PlantMassFlowRateMax =  2.d0 !
      ENDIF

    ELSEIF (MicroCHP(GeneratorNum)%CWLoopSideNum == DemandSide) THEN
      MicroCHP(GeneratorNum)%PlantMassFlowRateMax =  2.d0 ! would like to use plant loop max but not ready yet
    ENDIF

    CALL RegisterPlantCompDesignFlow(MicroCHP(GeneratorNum)%PlantInletNodeID,MicroCHP(GeneratorNum)%PlantMassFlowRateMax / rho )

    MicroCHP(GeneratorNum)%A42Model%ElecEff                                                   &
                        = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID ,          &
                           MicroCHP(GeneratorNum)%A42Model%MaxElecPower ,                      &
                           MicroCHP(GeneratorNum)%PlantMassFlowRateMax ,&
                           Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp )

    MicroCHP(GeneratorNum)%A42Model%ThermEff                                                  &
                        = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID,        &
                           MicroCHP(GeneratorNum)%A42Model%MaxElecPower ,                      &
                           MicroCHP(GeneratorNum)%PlantMassFlowRateMax ,&
                           Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp )

    CALL SetupGeneratorControlStateManager(GeneratorNum)
    MySizeFlag(GeneratorNum) = .FALSE.
  ENDIF

  If (MySizeFlag(GeneratorNum)) RETURN

  DynaCntrlNum = MicroCHP(GeneratorNum)%DynamicsControlID

  IF (BeginEnvrnFlag .and. MyEnvrnFlag(GeneratorNum) ) THEN
    !reset to starting condition for different environment runperiods, design days
    MicroCHP(GeneratorNum)%A42Model%TengLast      = 20.d0
    MicroCHP(GeneratorNum)%A42Model%TempCWOutLast = 20.d0
    MicroCHP(GeneratorNum)%A42Model%TimeElapsed   = 0.d0
    MicroCHP(GeneratorNum)%A42Model%opMode        = 0
    MicroCHP(GeneratorNum)%A42Model%OffModeTime   = 0.d0
    MicroCHP(GeneratorNum)%A42Model%StandyByModeTime = 0.d0
    MicroCHP(GeneratorNum)%A42Model%WarmUpModeTime = 0.d0
    MicroCHP(GeneratorNum)%A42Model%NormalModeTime = 0.d0
    MicroCHP(GeneratorNum)%A42Model%CoolDownModeTime = 0.d0
    MicroCHP(GeneratorNum)%A42Model%Pnet          = 0.d0
    MicroCHP(GeneratorNum)%A42Model%ElecEff       = 0.d0
    MicroCHP(GeneratorNum)%A42Model%Qgross        = 0.d0
    MicroCHP(GeneratorNum)%A42Model%ThermEff      = 0.d0
    MicroCHP(GeneratorNum)%A42Model%Qgenss        = 0.d0
    MicroCHP(GeneratorNum)%A42Model%NdotFuel      = 0.d0
    MicroCHP(GeneratorNum)%A42Model%MdotFuel      = 0.d0
    MicroCHP(GeneratorNum)%A42Model%Teng          = 20.d0
    MicroCHP(GeneratorNum)%A42Model%Tcwin         = 20.d0
    MicroCHP(GeneratorNum)%A42Model%Tcwout        = 20.d0
    MicroCHP(GeneratorNum)%A42Model%MdotAir       = 0.d0
    MicroCHP(GeneratorNum)%A42Model%QdotSkin      = 0.d0
    MicroCHP(GeneratorNum)%A42Model%QdotConvZone  = 0.d0
    MicroCHP(GeneratorNum)%A42Model%QdotRadZone   = 0.d0
    GeneratorDynamics(DynaCntrlNum)%LastOpMode    = OpModeOFF
    GeneratorDynamics(DynaCntrlNum)%CurrentOpMode = OpModeOFF
    GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastShutDown = 0.d0
    GeneratorDynamics(DynaCntrlNum)%FractionalDayofLastStartUp  = 0.d0
    GeneratorDynamics(DynaCntrlNum)%HasBeenOn      = .FALSE.
    GeneratorDynamics(DynaCntrlNum)%DuringStartUp  = .FALSE.
    GeneratorDynamics(DynaCntrlNum)%DuringShutDown = .FALSE.
    GeneratorDynamics(DynaCntrlNum)%FuelMdotLastTimestep = 0.d0
    GeneratorDynamics(DynaCntrlNum)%PelLastTimeStep = 0.d0
    GeneratorDynamics(DynaCntrlNum)%NumCycles       = 0

    FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%QskinLoss = 0.d0

    CALL InitComponentNodes( 0.d0, MicroCHP(GeneratorNum)%PlantMassFlowRateMax, &
                                 MicroCHP(GeneratorNum)%PlantInletNodeID,  &
                                 MicroCHP(GeneratorNum)%PlantOutletNodeID, &
                                 MicroCHP(GeneratorNum)%CWLoopNum, &
                                 MicroCHP(GeneratorNum)%CWLoopSideNum, &
                                 MicroCHP(GeneratorNum)%CWBranchNum, &
                                 MicroCHP(GeneratorNum)%CWCompNum )


  ENDIF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(GeneratorNum) = .TRUE.
  END IF

  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed
  IF (MicroCHP(GeneratorNum)%A42Model%TimeElapsed /= TimeElapsed) THEN
    ! The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
    ! timestep for use as the initial conditions of each iteration that does not advance the system timestep.
    MicroCHP(GeneratorNum)%A42Model%TengLast      = MicroCHP(GeneratorNum)%A42Model%Teng
    MicroCHP(GeneratorNum)%A42Model%TempCWOutLast = MicroCHP(GeneratorNum)%A42Model%Tcwout
    MicroCHP(GeneratorNum)%A42Model%TimeElapsed   = TimeElapsed
    GeneratorDynamics(DynaCntrlNum)%LastOpMode    = GeneratorDynamics(DynaCntrlNum)%CurrentOpMode
    GeneratorDynamics(DynaCntrlNum)%FuelMdotLastTimestep = MicroCHP(GeneratorNum)%A42Model%MdotFuel
    GeneratorDynamics(DynaCntrlNum)%PelLastTimeStep = MicroCHP(GeneratorNum)%A42Model%Pnet
  ENDIF

  IF ( .NOT.  MicroCHP(GeneratorNum)%A42Model%InternalFlowControl ) THEN

    mdot     = MicroCHP(GeneratorNum)%PlantMassFlowRateMax
    CALL SetComponentFlowRate(mdot, &
                      MicroCHP(GeneratorNum)%PlantInletNodeID,  &
                      MicroCHP(GeneratorNum)%PlantOutletNodeID, &
                      MicroCHP(GeneratorNum)%CWLoopNum,     &
                      MicroCHP(GeneratorNum)%CWLoopSideNum, &
                      MicroCHP(GeneratorNum)%CWBranchNum,   &
                      MicroCHP(GeneratorNum)%CWCompNum)
    MicroCHP(GeneratorNum)%PlantMassFlowRate = mdot

  ENDIF

  RETURN

END SUBROUTINE InitMicroCHPNoNormalizeGenerators


SUBROUTINE CalcMicroCHPNoNormalizeGeneratorModel(GeneratorNum,RunFlagElectCenter,  &
                                           RunFlagPlant, MyElectricLoad,MyThermalLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR        B Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main calculation subroutine for the IEA Annex 42 model

          ! METHODOLOGY EMPLOYED:
          ! curve fit, dynamic control limits,

          ! REFERENCES:
          ! IEA Annex 42 FC-COGEN-SIM "A Generic Model Specification for Combustion-based Residential CHP Devices"
          ! Alex Ferguson, Nick Kelly, Version 3, June 26, 2006

          ! USE STATEMENTS:
  USE DataLoopNode ,   ONLY: Node
  USE DataHeatBalFanSys, ONLY: MAT
  Use DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE DataGlobals    , ONLY: TimeStep, TimeStepZone, SecInHour, HoursInDay
  USE CurveManager,    ONLY: CurveValue
  USE DataGlobalConstants
  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataPlant,       ONLY: PlantLoop
  USE PlantUtilities,  ONLY: SetComponentFlowRate
  USE DataEnvironment, ONLY: OutDryBulbTemp

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)     :: RunFlagElectCenter ! TRUE when Generator operating
  LOGICAL, INTENT(IN)    :: RunFlagPlant       !
  REAL(r64) , INTENT(IN)  :: MyElectricload          ! Generator demand
  REAL(r64)  , INTENT(IN)     :: MyThermalLoad
  LOGICAL, INTENT(IN)     :: FirstHVACIteration


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: AllowedLoad               = 0.0d0
  INTEGER :: CurrentOpMode             = 0
  REAL(r64)    :: PLRforSubtimestepStartUp  = 1.0d0
  REAL(r64)    :: PLRforSubtimestepShutDown = 0.0d0
  LOGICAL :: RunFlag                   = .false.
  INTEGER :: DynaCntrlNum              = 0
  REAL(r64)    :: Pnetss = 0.0d0
  REAL(r64)    :: Pstandby = 0.0d0 ! power draw during standby, positive here means negative production
  REAL(r64)    :: Pcooler  = 0.0d0 ! power draw during cool down, positive here means negative production
!  REAL(r64)    :: Pnet   = 0.0d0
  REAL(r64)    :: NdotFuel = 0.0d0

  LOGICAL :: ConstrainedIncreasingNdot = .false.
  LOGICAL :: ConstrainedDecreasingNdot = .false.
  INTEGER :: I       = 0
  REAL(r64)    :: dt      = 0.0d0
  REAL(r64)    :: ElecEff = 0.0d0
  REAL(r64)    :: MdotAir = 0.0d0
  REAL(r64)    :: Qgenss  = 0.0d0
  REAL(r64)    :: Mdotcw  = 0.0d0
  REAL(r64)    :: TcwIn   = 0.0d0
  REAL(r64)    ::  Tcwout = 0.0d0
  REAL(r64)    :: MdotFuel = 0.0d0
  REAL(r64)    :: MdotFuelAllowed = 0.0d0
  REAL(r64)    :: MdotFuelMax = 0.0d0
  REAL(r64)    :: MdotFuelWarmup = 0.0d0
  REAL(r64)    :: Pmax    = 0.0d0
  REAL(r64)    :: Qgross  = 0.0d0
  REAL(r64)    :: Teng    = 0.0d0
  REAL(r64)    :: ThermEff = 0.0d0
  REAL(r64)    :: Cp = 0.d0 ! local fluid specific heat
  REAL(r64)    :: thisAmbientTemp = 0.d0

  LOGICAL :: EnergyBalOK ! check for balance to exit loop

  DynaCntrlNum = MicroCHP(GeneratorNum)%DynamicsControlID

  CALL ManageGeneratorControlState(iGeneratorMicroCHP, MicroCHP(GeneratorNum)%Name, &
                                   GeneratorNum, RunFlagElectCenter, RunFlagPlant, MyElectricLoad,  MyThermalLoad,   &
                                   AllowedLoad, CurrentOpMode,&
                                   PLRforSubtimestepStartUp, PLRforSubtimestepShutDown, FirstHVACIteration)

  If (RunFlagElectCenter .or. RunFlagPlant ) RunFlag = .true.

  Teng   =  MicroCHP(GeneratorNum)%A42Model%Teng
  Tcwout =  MicroCHP(GeneratorNum)%A42Model%Tcwout
  dt = TimeStepSys *  SecInHour

  IF (MicroCHP(GeneratorNum)%ZoneID > 0) THEN
    thisAmbientTemp = MAT(MicroCHP(GeneratorNum)%ZoneID)
  ELSE ! outdoor location, no zone
    thisAmbientTemp = OutDryBulbTemp
  ENDIF

  Select CASE (CurrentOpMode)

  CASE (OpModeOFF)  ! same as standby in model spec but no Pnet standby electicity losses.

    Qgenss   = 0.0d0
    Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
    TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
    Pnetss   = 0.0d0
    Pstandby = 0.0d0
    Pcooler  = MicroCHP(GeneratorNum)%A42Model%PcoolDown * PLRforSubtimestepShutDown
    ElecEff  = 0.0d0
    ThermEff = 0.0d0
    Qgross   = 0.0d0
    NdotFuel = 0.0d0
    MdotFuel = 0.0d0
    MdotAir  = 0.0d0

    Mdotcw = 0.d0
    CALL SetComponentFlowRate(Mdotcw, &
                    MicroCHP(GeneratorNum)%PlantInletNodeID,  &
                    MicroCHP(GeneratorNum)%PlantOutletNodeID, &
                    MicroCHP(GeneratorNum)%CWLoopNum,     &
                    MicroCHP(GeneratorNum)%CWLoopSideNum, &
                    MicroCHP(GeneratorNum)%CWBranchNum,   &
                    MicroCHP(GeneratorNum)%CWCompNum)
    MicroCHP(GeneratorNum)%PlantMassFlowRate = Mdotcw


  CASE (OpModeStandby)
    Qgenss = 0.0d0
    Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
    TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
    Pnetss   = 0.0d0
    Pstandby = MicroCHP(GeneratorNum)%A42Model%Pstandby * (1.0 - PLRforSubtimestepShutDown)
    Pcooler  = MicroCHP(GeneratorNum)%A42Model%PcoolDown * PLRforSubtimestepShutDown
    ElecEff  = 0.0d0
    ThermEff = 0.0d0
    Qgross   = 0.0d0
    NdotFuel = 0.0d0
    MdotFuel = 0.0d0
    MdotAir  = 0.0d0

    Mdotcw = 0.d0
    CALL SetComponentFlowRate(Mdotcw, &
                    MicroCHP(GeneratorNum)%PlantInletNodeID,  &
                    MicroCHP(GeneratorNum)%PlantOutletNodeID, &
                    MicroCHP(GeneratorNum)%CWLoopNum,     &
                    MicroCHP(GeneratorNum)%CWLoopSideNum, &
                    MicroCHP(GeneratorNum)%CWBranchNum,   &
                    MicroCHP(GeneratorNum)%CWCompNum)
    MicroCHP(GeneratorNum)%PlantMassFlowRate = Mdotcw


  CASE (OpModeWarmUp)

   IF ( MicroCHP(GeneratorNum)%A42Model%WarmUpByTimeDelay ) Then
     ! Internal combustion engine.  This is just like normal  operation but no net power yet.
      Pnetss   =  MyElectricLoad ! W
      Pstandby = 0.0d0
      Pcooler  = MicroCHP(GeneratorNum)%A42Model%PcoolDown * PLRforSubtimestepShutDown
      TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
      Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
      IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
        Mdotcw = FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn)
      ENDIF
      ElecEff  = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID, Pnetss, Mdotcw , TcwIn )
      ElecEff  = MAX(0.0D0, ElecEff) !protect against bad curve result

      IF ( ElecEff > 0.0d0) THEN ! trap divide by bad thing
        Qgross   = Pnetss/ElecEff     !W
      ELSE
        Qgross = 0.0d0
      ENDIF
      ThermEff = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID, Pnetss, Mdotcw , TcwIN )
      ThermEff  = MAX(0.0D0, ThermEff) !protect against bad curve result

      Qgenss   = ThermEff * Qgross !W

      MdotFuel = Qgross / (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 *1000.0d0)&
       *  FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
       !  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)

      CALL ManageGeneratorFuelFlow(iGeneratorMicroCHP, MicroCHP(GeneratorNum)%Name, GeneratorNum,RunFlag,MdotFuel, &
                                      MdotFuelAllowed, ConstrainedIncreasingNdot, ConstrainedDecreasingNdot)

      IF (ConstrainedIncreasingNdot .OR. ConstrainedDecreasingNdot) THEN ! recalculate Pnetss with new NdotFuel with iteration
        MdotFuel = MdotFuelAllowed
        NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
        Qgross = NdotFuel * (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  *1000.0d0* 1000.0d0)

        DO I=1, 20 ! iterating here  could add use of seach method .
          Pnetss = Qgross * ElecEff
          IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
            Mdotcw = FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn)
          ENDIF
          ElecEff  = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID, Pnetss,  Mdotcw, TcwIN)
          ElecEff  = MAX(0.0D0, ElecEff) !protect against bad curve result
        ENDDO

        ThermEff = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID, Pnetss, Mdotcw, TcwIN )
        ThermEff  = MAX(0.0D0, ThermEff) !protect against bad curve result
        Qgenss   = ThermEff * Qgross !W

      ENDIf
      Pnetss   = 0.0d0 ! no actually power produced here.
      NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
      MdotAir = CurveValue(MicroCHP(GeneratorNum)%A42Model%AirFlowCurveID, MdotFuel)
      MdotAir  = MAX(0.0D0, MdotAir) !protect against bad curve result

    ELSEIF ( MicroCHP(GeneratorNum)%A42Model%WarmUpByEngineTemp) THEN
    ! Stirling engine mode warm up
    !   find MdotFuelMax
      Pmax = MicroCHP(GeneratorNum)%A42Model%MaxElecPower
      Pnetss = 0.0d0
      Pstandby = 0.0d0
      Pcooler  = MicroCHP(GeneratorNum)%A42Model%PcoolDown * PLRforSubtimestepShutDown ! could be here with part load in cool down
      TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
      Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
      ElecEff  = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID, Pmax, Mdotcw, TcwIN)
      ElecEff  = MAX(0.0D0, ElecEff) !protect against bad curve result
      IF ( ElecEff > 0.0d0) THEN ! trap divide by bad thing
         Qgross   = Pmax/ElecEff     !W
      ELSE
         Qgross   = 0.0d0
      ENDIF
      NdotFuel = Qgross / (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 *1000.0d0)
       !  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
      MdotFuelMax = NdotFuel * FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec

      If (Teng > thisAmbientTemp ) Then
        MdotFuelWarmup = MdotFuelMax + MicroCHP(GeneratorNum)%A42Model%kf *  MdotFuelMax *  &
                            (  ( MicroCHP(GeneratorNum)%A42Model%TnomEngOp -  thisAmbientTemp ) &
                            / (Teng -  thisAmbientTemp ) )
          ! check that numerical answer didn't blow up beyond limit, and reset if it did
        IF (MdotFuelWarmup > MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax) THEN
              MdotFuelWarmup = MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax
        ENDIF
      ELSEIF (Teng < thisAmbientTemp)  Then
        MdotFuelWarmup = MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax
      ELSE ! equal would divide by zero
        MdotFuelWarmup = MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax
      ENDIF

      If (MicroCHP(GeneratorNum)%A42Model%TnomEngOp > thisAmbientTemp ) Then
        Pnetss = Pmax * MicroCHP(GeneratorNum)%A42Model%kp *  &
                          ( (Teng -  thisAmbientTemp ) &
                            /  ( MicroCHP(GeneratorNum)%A42Model%TnomEngOp -  thisAmbientTemp ) )
      ELSEIF (MicroCHP(GeneratorNum)%A42Model%TnomEngOp < thisAmbientTemp)  Then
        Pnetss = Pmax
      ELSE ! equal would divide by zero
        Pnetss = Pmax
      ENDIF

      !If ( MicroCHP(GeneratorNum)%A42Model%TnomEngOp < thisAmbientTemp) then
      !    !this case where zone is super hot and more than engine op. temp.
      !    !  never going to get here because E+ zones don't like to be over 50C. (and no cogen devices should operate below 50C)
      !ENDIF


      MdotFuel = MdotFuelWarmup
      NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
      MdotAir = CurveValue(MicroCHP(GeneratorNum)%A42Model%AirFlowCurveID, MdotFuelWarmup)
      MdotAir  = MAX(0.0D0, MdotAir) !protect against bad curve result
      Qgross   = NdotFuel * (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 *1000.0d0)
      ThermEff = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID, Pmax, Mdotcw, TcwIN)
      Qgenss   = ThermEff * Qgross !W

   ENDIF
   NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec

  CASE (OpModeNormal)
    If (PLRforSubtimestepStartUp < 1.0d0) then
      If (RunFlagElectCenter)  Pnetss   = MyElectricLoad  ! W
      If (RunFlagPlant) Pnetss   = AllowedLoad
    else
      Pnetss   = AllowedLoad
    endif
    Pstandby = 0.0d0
    Pcooler  = 0.0d0
    TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
    Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
    IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
      Mdotcw = FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn)
    ENDIF

    ElecEff  = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID, Pnetss, Mdotcw, TcwIN )
    ElecEff  = MAX(0.0D0, ElecEff) !protect against bad curve result

    IF ( ElecEff > 0.0d0) THEN ! trap divide by bad thing
      Qgross   = Pnetss/ElecEff     !W
    ELSE
      Qgross = 0.0d0
    ENDIF

    ThermEff = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID, Pnetss, Mdotcw, TcwIN)
    ThermEff  = MAX(0.0D0, ThermEff) !protect against bad curve result
    Qgenss   = ThermEff * Qgross !W
    MdotFuel = Qgross / (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 *1000.0d0) &
       *  FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
       !  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
    CALL ManageGeneratorFuelFlow(iGeneratorMicroCHP, MicroCHP(GeneratorNum)%Name, GeneratorNum,RunFlag,MdotFuel, &
                                      MdotFuelAllowed, ConstrainedIncreasingNdot, ConstrainedDecreasingNdot)

    IF (ConstrainedIncreasingNdot .OR. ConstrainedDecreasingNdot) THEN ! recalculate Pnetss with new NdotFuel with iteration
      MdotFuel = MdotFuelAllowed
      NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
      Qgross = NdotFuel * (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 * 1000.0d0)

      DO I=1, 20 ! iterating here,  could add use of seach method error signal
        Pnetss = Qgross * ElecEff
        IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
          Mdotcw = FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn)
        ENDIF
        ElecEff  = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID, Pnetss, Mdotcw, TcwIN )
        ElecEff  = MAX(0.0D0, ElecEff) !protect against bad curve result
      ENDDO

      ThermEff = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID, Pnetss,Mdotcw , TcwIN )
      ThermEff  = MAX(0.0D0, ThermEff) !protect against bad curve result
      Qgenss   = ThermEff * Qgross !W

    ENDIF

    NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
    MdotAir = CurveValue(MicroCHP(GeneratorNum)%A42Model%AirFlowCurveID, MdotFuel)
    MdotAir  = MAX(0.0D0, MdotAir) !protect against bad curve result
    IF (PLRforSubtimestepStartUp < 1.0d0) THEN
      Pnetss = AllowedLoad
    ENDIF

  CASE (opModeCoolDown)

      Pnetss   = 0.0d0
      Pstandby = 0.0d0
      Pcooler  = MicroCHP(GeneratorNum)%A42Model%PcoolDown
      TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
      Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
      IF (MicroCHP(GeneratorNum)%A42Model%InternalFlowControl) THEN
        Mdotcw = FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn )
      ENDIF
      NdotFuel = 0.0d0
      MdotFuel = 0.0d0
      MdotAir  = 0.0d0
      ElecEff  = 0.0d0
      ThermEff = 0.0d0
      Qgross   = 0.0d0
      Qgenss   = 0.0d0
  END SELECT

  EnergyBalOK = .false.
  DO I=1, 20 ! sequential search with exit criteria
    ! calculate new value for engine temperature
    ! for Stirling in warmup, need to include dependency of Qgness on Teng
    If ((MicroCHP(GeneratorNum)%A42Model%WarmUpByEngineTemp) .AND. ( CurrentOpMode == OpModeWarmUp)) then

      Pmax = MicroCHP(GeneratorNum)%A42Model%MaxElecPower
      TcwIn    = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%Temp  !C
      Mdotcw   = Node(MicroCHP(GeneratorNum)%PlantInletNodeID)%MassFlowRate  !kg/s
      ElecEff  = CurveValue(MicroCHP(GeneratorNum)%A42Model%ElecEffCurveID, Pmax, Mdotcw, TcwIN)
      ElecEff  = MAX(0.0D0, ElecEff) !protect against bad curve result
      IF ( ElecEff > 0.0d0) THEN ! trap divide by bad thing
         Qgross   = Pmax/ElecEff     !W
      ELSE
         Qgross   = 0.0d0
      ENDIF
      NdotFuel = Qgross / (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 *1000.0d0)
       !  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
      MdotFuelMax = NdotFuel * FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec

      IF (Teng > thisAmbientTemp ) Then
        MdotFuelWarmup = MdotFuelMax + MicroCHP(GeneratorNum)%A42Model%kf *  MdotFuelMax *  &
                           (  ( MicroCHP(GeneratorNum)%A42Model%TnomEngOp -  thisAmbientTemp ) &
                            / (Teng - thisAmbientTemp ) )

         ! check that numerical answer didn't blow up beyond limit, and reset if it did
        IF (MdotFuelWarmup > MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax) THEN
             MdotFuelWarmup = MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax
        ENDIF
        If (MicroCHP(GeneratorNum)%A42Model%TnomEngOp > thisAmbientTemp ) Then
          Pnetss = Pmax * MicroCHP(GeneratorNum)%A42Model%kp *  &
                          ( (Teng -  thisAmbientTemp ) &
                           /  ( MicroCHP(GeneratorNum)%A42Model%TnomEngOp -  thisAmbientTemp ) )
        ELSEIF (MicroCHP(GeneratorNum)%A42Model%TnomEngOp < thisAmbientTemp)  Then
          Pnetss = Pmax
        ELSE ! equal would divide by zero
          Pnetss = Pmax
        ENDIF

      ELSEIF (Teng < thisAmbientTemp)  Then
        MdotFuelWarmup = MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax
      ELSE ! equal would divide by zero
        MdotFuelWarmup = MicroCHP(GeneratorNum)%A42Model%Rfuelwarmup * MdotFuelMax
      ENDIF
      MdotFuel = MdotFuelWarmup
      NdotFuel = MdotFuel / FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%KmolPerSecToKgPerSec
      MdotAir = CurveValue(MicroCHP(GeneratorNum)%A42Model%AirFlowCurveID, MdotFuelWarmup)
      MdotAir  = MAX(0.0D0, MdotAir) !protect against bad curve result
      Qgross   = NdotFuel * (FuelSupply(MicroCHP(GeneratorNum)%FuelSupplyID)%LHV  * 1000.0d0 *1000.0d0)
      ThermEff = CurveValue(MicroCHP(GeneratorNum)%A42Model%ThermalEffCurveID, Pmax, Mdotcw, TcwIN)
      ThermEff  = MAX(0.0D0, ThermEff) !protect against bad curve result
      Qgenss   = ThermEff * Qgross !W
    ENDIF

    Teng = FuncDetermineEngineTemp(Tcwout, MicroCHP(GeneratorNum)%A42Model%MCeng, MicroCHP(GeneratorNum)%A42Model%UAhx,&
                              MicroCHP(GeneratorNum)%A42Model%UAskin, thisAmbientTemp, Qgenss, &
                              MicroCHP(GeneratorNum)%A42Model%TengLast, dt )

    Cp     = GetSpecificHeatGlycol(PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%FluidName, &
                              TcwIn, &
                              PlantLoop(MicroCHP(GeneratorNum)%CWLoopNum)%FluidIndex, &
                              'CalcMicroCHPNoNormalizeGeneratorModel')

    Tcwout = FuncDetermineCoolantWaterExitTemp(TcwIn,MicroCHP(GeneratorNum)%A42Model%MCcw,MicroCHP(GeneratorNum)%A42Model%UAhx , &
                              Mdotcw * Cp , Teng,  MicroCHP(GeneratorNum)%A42Model%TempCWOutLast, dt)

     ! form balance and exit once met.
    EnergyBalOK = CheckMicroCHPThermalBalance(MicroCHP(GeneratorNum)%A42Model%MaxElecPower, Tcwin, Tcwout, Teng,  &
                              thisAmbientTemp, MicroCHP(GeneratorNum)%A42Model%UAhx, &
                              MicroCHP(GeneratorNum)%A42Model%UAskin, Qgenss, MicroCHP(GeneratorNum)%A42Model%MCeng, &
                              MicroCHP(GeneratorNum)%A42Model%MCcw , Mdotcw * Cp  )

    If (EnergyBalOK .AND. (I > 4)) Exit

  ENDDO

  MicroCHP(GeneratorNum)%PlantMassFlowRate  = Mdotcw
  MicroCHP(GeneratorNum)%A42Model%Pnet      = Pnetss - Pcooler - Pstandby
  MicroCHP(GeneratorNum)%A42Model%ElecEff   = ElecEff
  MicroCHP(GeneratorNum)%A42Model%Qgross    = Qgross
  MicroCHP(GeneratorNum)%A42Model%ThermEff  = ThermEff
  MicroCHP(GeneratorNum)%A42Model%Qgenss    = Qgenss
  MicroCHP(GeneratorNum)%A42Model%NdotFuel  = NdotFuel
  MicroCHP(GeneratorNum)%A42Model%MdotFuel  = MdotFuel
  MicroCHP(GeneratorNum)%A42Model%Teng      = Teng
  MicroCHP(GeneratorNum)%A42Model%Tcwout    = Tcwout
  MicroCHP(GeneratorNum)%A42Model%Tcwin     = Tcwin
  MicroCHP(GeneratorNum)%A42Model%MdotAir   = MdotAir
  MicroCHP(GeneratorNum)%A42Model%QdotSkin  = MicroCHP(GeneratorNum)%A42Model%UAskin*(Teng - thisAmbientTemp)

  MicroCHP(GeneratorNum)%A42Model%OpMode = CurrentOpMode

  RETURN

END SUBROUTINE CalcMicroCHPNoNormalizeGeneratorModel

REAL(r64) FUNCTION FuncDetermineEngineTemp(Tcwout, MCeng, UAHX, UAskin, Troom, Qgenss, TengLast, time)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate engine temperaure,

          ! METHODOLOGY EMPLOYED:
          ! model is dynamic in that previous condition affects current timestep
          !  solve ode for engine temp using analytical solution

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: Tcwout ! hot water leaving temp
  REAL(r64), INTENT(IN) :: MCeng  ! Fictitious mass and heat capacity of engine
  REAL(r64), INTENT(IN) :: UAHX   ! Heat exchanger UA
  REAL(r64), INTENT(IN) :: UAskin ! Skin losses UA
  REAL(r64), INTENT(IN) :: Troom  ! surrounding zone temperature C
  REAL(r64), INTENT(IN) :: Qgenss ! steady state generator heat generation
  REAL(r64), INTENT(IN) :: TengLast ! engine temp at previous time step
  REAL(r64), INTENT(IN) :: time ! elapsed time since previous evaluation



          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: a
  REAL(r64) :: b

  a = (  ( UAHX * Tcwout / MCeng) + ( UAskin * Troom /  MCeng )  + (  Qgenss / MCeng ) )
  b = ( ( -1.0d0 * UAHX / MCeng )  + ( -1.0d0 * UAskin / MCeng ) )

  FuncDetermineEngineTemp = (TengLast + a/b )* exp(b*time) - a / b

  RETURN

END FUNCTION FuncDetermineEngineTemp

REAL(r64) FUNCTION FuncDetermineCoolantWaterExitTemp(Tcwin, MCcw, UAHX, MdotCpcw, Teng, TcwoutLast, time)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate coolan water leaving temperaure,

          ! METHODOLOGY EMPLOYED:
          ! model is dynamic in that previous condition affects current timestep
          !  solve ode for coolant water outlet temp using analytical solution

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals , ONLY: MaxEXPArg

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: Tcwin ! hot water inlet temp
  REAL(r64), INTENT(IN) :: MCcw  ! Fictitious mass and heat capacity of coolant hx
  REAL(r64), INTENT(IN) :: UAHX   ! Heat exchanger UA
  REAL(r64), INTENT(IN) :: MdotCpcw ! mass flow and specific heat of coolant water
  REAL(r64), INTENT(IN) :: Teng  ! engine mass temperature C
  REAL(r64), INTENT(IN) :: TcwoutLast ! coolant water leaving temp at previous time step
  REAL(r64), INTENT(IN) :: time ! elapsed time since previous evaluation



          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: a
  REAL(r64) :: b

  a =  ( MdotCpcw * Tcwin / MCcw) + ( UAHX * Teng /  MCcw )
  b = ( ( -1.0d0 * MdotCpcw / MCcw )  + ( -1.0d0 * UAHX / MCcw ) )

  IF (b*time < (-1.d0 * MaxEXPArg)) THEN

    FuncDetermineCoolantWaterExitTemp = - a / b
  ELSE

    FuncDetermineCoolantWaterExitTemp = (TcwoutLast + a/b )* exp(b*time) - a / b
  ENDIF
  RETURN

END FUNCTION FuncDetermineCoolantWaterExitTemp

Logical FUNCTION CheckMicroCHPThermalBalance(NomHeatGen, Tcwin, Tcwout, Teng, Troom, &
                                             UAHX, UAskin, Qgenss, MCeng, MCcw , MdotCpcw  )

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Check for energy balance to test if can exit iteration loop

          ! METHODOLOGY EMPLOYED:
          ! put all terms of dynamic energy balances on RHS and compute magnitude of imbalance
          !  compare imbalance to scalable thresholds and make a boolean conclusion.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: NomHeatGen ! nominal heat generation rate for scaling
  REAL(r64), INTENT(IN) :: Tcwin ! hot water inlet temp
  REAL(r64), INTENT(IN) :: Tcwout ! hot water leaving temp
  REAL(r64), INTENT(IN) :: Teng  ! engine mass temperature C
  REAL(r64), INTENT(IN) :: Troom  ! surrounding zone temperature C
  REAL(r64), INTENT(IN) :: UAHX   ! Heat exchanger UA
  REAL(r64), INTENT(IN) :: UAskin ! Skin losses UA
  REAL(r64), INTENT(IN) :: Qgenss ! steady state generator heat generation
  REAL(r64), INTENT(IN) :: MCeng  ! Fictitious mass and heat capacity of engine
  REAL(r64), INTENT(IN) :: MCcw  ! Fictitious mass and heat capacity of coolant hx
  REAL(r64), INTENT(IN) :: MdotCpcw ! mass flow and specific heat of coolant water

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: a ! working variable, "a" term in generic ODE
  REAL(r64)  :: b ! working variable "b" term in generic ODE
  REAL(r64)  :: DTengDTime ! derivative of engine temp wrt time
  REAL(r64)  :: DCoolOutTDtime ! derivative of coolant exit temp wrt time
  REAL(r64)  :: magImbalEng ! energy imbalance for engine control volume
  REAL(r64)  :: magImbalCooling ! energy imbalance for coolant control volume
  REAL(r64)  :: threshold ! criteria for when to call energy balance okay
  !first compute derivatives using a + bT

  a = (  ( UAHX * Tcwout / MCeng) + ( UAskin * Troom /  MCeng )  + (  Qgenss / MCeng ) )
  b = ( ( -1.0d0 * UAHX / MCeng )  + ( -1.0d0 * UAskin / MCeng )  )
  DTengDtime = a + b*Teng

  a =  ( MdotCpcw * Tcwin / MCcw) + ( UAHX * Teng /  MCcw )
  b = ( ( -1.0d0 * MdotCpcw / MCcw )  + ( -1.0d0 * UAHX / MCcw ) )
  DCoolOutTDtime = a + b*Tcwout

  magImbalEng     = UAHX * (Tcwout - Teng) + UAskin*(Troom - Teng) + Qgenss - MCeng * DTengDtime

  magImbalCooling = MdotCpcw * (Tcwin - Tcwout) +  UAHX * (Teng - Tcwout)- MCcw * DCoolOutTDtime

  threshold = NomHeatGen / 10000000.0d0

  CheckMicroCHPThermalBalance = .false.

  If ((threshold > magImbalEng ) .and. (threshold > magImbalCooling) ) then
    CheckMicroCHPThermalBalance =  .true.
  endif

  RETURN

END FUNCTION CheckMicroCHPThermalBalance

SUBROUTINE FigureMicroCHPZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Couple equiment skin losses to the Zone Heat Balance
          !

          ! METHODOLOGY EMPLOYED:
          ! This routine adds up the various skin losses and then
          !  sets the values in the ZoneIntGain structure

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance , ONLY: ZoneIntGain
  USE DataGlobals,      ONLY: BeginEnvrnFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER :: thisZone ! index in Zone structure array
  REAL(r64)    :: TotalZoneHeatGain
  INTEGER :: CHPnum
!  INTEGER :: ZoneNum
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.

  IF (NumMicroCHPs == 0) RETURN

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    FuelSupply%QskinLoss  = 0.d0
    MicroCHP%A42Model%QdotSkin = 0.d0
    MicroCHP%Report%SkinLossConvect = 0.d0
    MicroCHP%Report%SkinLossRadiat  = 0.d0
    MyEnvrnFlag = .FALSE.
  END IF

  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

  DO CHPnum = 1, NumMicroCHPs
    TotalZoneHeatGain =   FuelSupply(MicroCHP(CHPnum)%FuelSupplyID )%QskinLoss &
                       + MicroCHP( CHPnum )%A42Model%QdotSkin

    MicroCHP(CHPnum)%A42Model%QdotConvZone = TotalZoneHeatGain * (1 - MicroCHP(CHPnum)%A42Model%RadiativeFraction)
    MicroCHP(CHPnum)%Report%SkinLossConvect = MicroCHP(CHPnum)%A42Model%QdotConvZone
    MicroCHP(CHPnum)%A42Model%QdotRadZone  = TotalZoneHeatGain * MicroCHP(CHPnum)%A42Model%RadiativeFraction
    MicroCHP(CHPnum)%Report%SkinLossRadiat = MicroCHP(CHPnum)%A42Model%QdotRadZone
  ENDDO



  ! this routine needs to do something for zone gains during sizing
!  IF(DoingSizing)THEN

!  ENDIF

  RETURN

END SUBROUTINE FigureMicroCHPZoneGains

SUBROUTINE CalcUpdateHeatRecovery(Num, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Aug 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update plant loop interactions, do any calcs needed

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataPlant,       ONLY: PlantLoop
  USE PlantUtilities,  ONLY: SafeCopyPlantNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: Num       ! Generator number
  LOGICAL, INTENT(IN)      :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InNodeNum
  INTEGER :: OutNodeNum
  REAL(r64) :: Cp ! local Specific heat of fluid
!  REAL(r64) :: mdot !local mass flow rate

  ! now update water outlet node Changing to Kg/s!
  OutNodeNum = MicroCHP(Num)%PlantOutletNodeID
  InNodeNum  = MicroCHP(Num)%PlantInletNodeID

  CAll SafeCopyPlantNode (InNodeNum, OutNodeNum)

  Node(OutNodeNum)%Temp = MicroCHP(Num)%A42Model%Tcwout

  Cp = GetSpecificHeatGlycol(PlantLoop(MicroCHP(Num)%CWLoopNum)%FluidName, &
                             MicroCHP(Num)%A42Model%Tcwin, &
                             PlantLoop(MicroCHP(Num)%CWLoopNum)%FluidIndex, &
                             'CalcUpdateHeatRecovery')

  Node(OutNodeNum)%Enthalpy = MicroCHP(Num)%A42Model%Tcwout * Cp


  RETURN

END SUBROUTINE CalcUpdateHeatRecovery

SUBROUTINE SimMicroCHPPlantHeatRecovery(CompType,CompName,CompNum,RunFlag,InitLoopEquip,  & !DSU
                          MyThermalLoad,MaxCap,MinCap,OptCap,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! makes sure input are gotten and setup from Plant loop perspective.
          ! does not (re)simulate entire MicroCHP model

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE InputProcessor, ONLY: FindItemInList
  Use DataGlobalConstants
  USE PlantUtilities, ONLY : UpdateComponentHeatRecoverySide
  USE DataPlant,      ONLY : TypeOf_Generator_MicroCHP

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(INOUT)       :: CompNum
  LOGICAL, INTENT(IN)          :: RunFlag
  !INTEGER, INTENT(IN)          :: FlowLock !DSU
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(INOUT)     :: MyThermalLoad
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  IF (GetMicroCHPInput) THEN

     ! Read input data.
     CALL  GetMicroCHPGeneratorInput
     GetMicroCHPInput=.false.
  ENDIF

  IF (InitLoopEquip) THEN
    CompNum = FindItemInList(CompName,MicroCHP%Name,NumMicroCHPs)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimMicroCHPPlantHeatRecovery: MicroCHP Generator Unit not found='//TRIM(CompName))
      RETURN
    ENDIF
    CALL InitMicroCHPNoNormalizeGenerators(CompNum, FirstHVACIteration)
    IF (MySizeFlag(CompNum)) RETURN
    MinCap  = GeneratorDynamics(MicroCHP(CompNum)%DynamicsControlID)%QdotHXMin
    MaxCap  = GeneratorDynamics(MicroCHP(CompNum)%DynamicsControlID)%QdotHXMax
    OptCap  = GeneratorDynamics(MicroCHP(CompNum)%DynamicsControlID)%QdotHXOpt
    RETURN
  END IF  ! End Of InitLoopEquip

  CALL UpdateComponentHeatRecoverySide(MicroCHP(CompNum)%CWLoopNum,               &
                                    MicroCHP(CompNum)%CWLoopSideNum,           &
                                    TypeOf_Generator_MicroCHP,                           &
                                    MicroCHP(CompNum)%PlantInletNodeID,     &
                                    MicroCHP(CompNum)%PlantOutletNodeID,    &
                                    MicroCHP(CompNum)%Report%QdotHR,     &
                                    MicroCHP(CompNum)%Report%HeatRecInletTemp,  &
                                    MicroCHP(CompNum)%Report%HeatRecOutletTemp, &
                                    MicroCHP(CompNum)%Report%HeatRecMdot ,  &
                                    FirstHVACIteration)

  RETURN

END SUBROUTINE SimMicroCHPPlantHeatRecovery

SUBROUTINE UpdateMicroCHPGeneratorRecords(Num)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update variables in structures linked to output reports

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE DataPlant,       ONLY: PlantLoop
  USE FluidProperties, ONLY: GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: Num       ! Generator number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: Cp ! local fluid specific heat

          ! na
  MicroCHP(Num)%Report%Mode              = MicroCHP(Num)%A42Model%OpMode
  MicroCHP(Num)%Report%OffModeTime       = MicroCHP(Num)%A42Model%OffModeTime
  MicroCHP(Num)%Report%StandyByModeTime  = MicroCHP(Num)%A42Model%StandyByModeTime
  MicroCHP(Num)%Report%WarmUpModeTime    = MicroCHP(Num)%A42Model%WarmUpModeTime
  MicroCHP(Num)%Report%NormalModeTime    = MicroCHP(Num)%A42Model%NormalModeTime
  MicroCHP(Num)%Report%CoolDownModeTime  = MicroCHP(Num)%A42Model%CoolDownModeTime


  MicroCHP(Num)%Report%ACPowerGen     = MicroCHP(Num)%A42Model%Pnet  !electrical power produced [W]
  MicroCHP(Num)%Report%ACEnergyGen    = MicroCHP(Num)%A42Model%Pnet*TimeStepSys*SecInHour ! energy produced (J)
  MicroCHP(Num)%Report%QdotGross      = MicroCHP(Num)%A42Model%Qgross
  MicroCHP(Num)%Report%Qgenss         = MicroCHP(Num)%A42Model%Qgenss
  MicroCHP(Num)%Report%QdotHX         = MicroCHP(Num)%A42Model%UAhx &
                     * (MicroCHP(Num)%A42Model%Teng  - MicroCHP(Num)%A42Model%Tcwout) !  heat recovered rate (W)

  Cp = GetSpecificHeatGlycol(PlantLoop(MicroCHP(Num)%CWLoopNum)%FluidName, &
                             MicroCHP(Num)%A42Model%Tcwin, &
                             PlantLoop(MicroCHP(Num)%CWLoopNum)%FluidIndex, &
                             'UpdateMicroCHPGeneratorRecords')

  MicroCHP(Num)%Report%QdotHR         = MicroCHP(Num)%PlantMassFlowRate * Cp &
                                        * (MicroCHP(Num)%A42Model%Tcwout - MicroCHP(Num)%A42Model%Tcwin)
  MicroCHP(Num)%Report%TotalHeatEnergyRec  = MicroCHP(Num)%Report%QdotHR & ! heat recovered energy (J)
                                        * TimeStepSys*SecInHour

  MicroCHP(Num)%Report%HeatRecInletTemp           = MicroCHP(Num)%A42Model%Tcwin ! Heat Recovery Loop Inlet Temperature (C)
  MicroCHP(Num)%Report%HeatRecOutletTemp          = MicroCHP(Num)%A42Model%Tcwout ! Heat Recovery Loop Outlet Temperature (C)
  MicroCHP(Num)%Report%HeatRecMdot                = MicroCHP(Num)%PlantMassFlowRate ! Heat Recovery Loop Mass flow rate (kg/s)
  MicroCHP(Num)%Report%Tengine           = MicroCHP(Num)%A42Model%Teng
  MicroCHP(Num)%Report%ElectEfficiency   = MicroCHP(Num)%A42Model%ElecEff
  MicroCHP(Num)%Report%ThermalEfficiency = MicroCHP(Num)%A42Model%ThermEff

  MicroCHP(Num)%Report%OverallEfficiency = MicroCHP(Num)%A42Model%ElecEff + MicroCHP(Num)%A42Model%ThermEff

  MicroCHP(Num)%Report%MdotAir           = MicroCHP(Num)%A42Model%MdotAir        ! air flow in kg/sec

  MicroCHP(Num)%Report%NdotFuel          = MicroCHP(Num)%A42Model%NdotFuel       ! fuel flow in kmol/sec
  MicroCHP(Num)%Report%MdotFuel          = MicroCHP(Num)%A42Model%MdotFuel       ! fuel flow in kg/sec

  MicroCHP(Num)%Report%FuelCompressPower = FuelSupply(MicroCHP(Num)%FuelSupplyID)%PfuelCompEl
                                                       ! electrical power used by fuel supply compressor [W]
  MicroCHP(Num)%Report%FuelCompressEnergy = FuelSupply(MicroCHP(Num)%FuelSupplyID)%PfuelCompEl*TimeStepSys*SecInHour ! elect energy
  MicroCHP(Num)%Report%FuelCompressSkinLoss = FuelSupply(MicroCHP(Num)%FuelSupplyID)%QskinLoss
                                                              !heat rate of losses.by fuel supply compressor [W]
  MicroCHP(Num)%Report%FuelEnergyHHV        = MicroCHP(Num)%A42Model%NdotFuel * FuelSupply(MicroCHP(Num)%FuelSupplyID)%HHV &
                                          * FuelSupply(MicroCHP(Num)%FuelSupplyID)%KmolPerSecToKgPerSec *TimeStepSys*SecInHour
              ! reporting: Fuel Energy used (W)
  MicroCHP(Num)%Report%FuelEnergyUseRateHHV = MicroCHP(Num)%A42Model%NdotFuel * FuelSupply(MicroCHP(Num)%FuelSupplyID)%HHV &
                                          * FuelSupply(MicroCHP(Num)%FuelSupplyID)%KmolPerSecToKgPerSec
              ! reporting: Fuel Energy used (J)
  MicroCHP(Num)%Report%FuelEnergyLHV        = MicroCHP(Num)%A42Model%NdotFuel * FuelSupply(MicroCHP(Num)%FuelSupplyID)%LHV &
                                          * 1000000.0d0 *TimeStepSys*SecInHour
              ! reporting: Fuel Energy used (W)
  MicroCHP(Num)%Report%FuelEnergyUseRateLHV = MicroCHP(Num)%A42Model%NdotFuel * FuelSupply(MicroCHP(Num)%FuelSupplyID)%LHV &
                                          * 1000000.0d0

  MicroCHP(Num)%Report%SkinLossPower   = MicroCHP(Num)%A42Model%QdotconvZone + MicroCHP(Num)%A42Model%QdotRadZone
  MicroCHP(Num)%Report%SkinLossEnergy  = (MicroCHP(Num)%A42Model%QdotConvZone +   &
                                          MicroCHP(Num)%A42Model%QdotRadZone)*TimeStepSys*SecInHour
  MicroCHP(Num)%Report%SkinLossConvect = MicroCHP(Num)%A42Model%QdotConvZone
  MicroCHP(Num)%Report%SkinLossRadiat  = MicroCHP(Num)%A42Model%QdotRadZone

! update node data for air inlet (and outlet)
  IF (MicroCHP(Num)%AirInletNodeId > 0) THEN
    Node(MicroCHP(Num)%AirInletNodeId)%MassFlowRate  = MicroCHP(Num)%Report%MdotAir
  ENDIF
  IF (MicroCHP(Num)%AirOutletNodeID > 0) THEN
    Node(MicroCHP(Num)%AirOutletNodeID)%MassFlowRate = MicroCHP(Num)%Report%MdotAir
    Node(MicroCHP(Num)%AirOutletNodeID)%Temp         = MicroCHP(Num)%A42Model%Teng
  ENDIF

  RETURN

END SUBROUTINE UpdateMicroCHPGeneratorRecords

SUBROUTINE GetMicroCHPGeneratorResults(GeneratorType, GeneratorIndex, &
                                 GeneratorPower,  GeneratorEnergy, ThermalPower, ThermalEnergy)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provide a get method to collect results at the load center level

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator
  INTEGER, INTENT(IN)           :: GeneratorIndex
  REAL(r64), INTENT(OUT)        :: GeneratorPower  ! electrical power
  REAL(r64), INTENT(OUT)        :: GeneratorEnergy ! electrical energy
  REAL(r64), INTENT(OUT)        :: ThermalPower  ! heat power
  REAL(r64), INTENT(OUT)        :: ThermalEnergy ! heat energy
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  GeneratorPower  =  MicroCHP(GeneratorIndex)%Report%ACPowerGen
  GeneratorEnergy =  MicroCHP(GeneratorIndex)%Report%ACEnergyGen
  ThermalPower    =  MicroCHP(GeneratorIndex)%report%QdotHR
  ThermalEnergy   =  MicroCHP(GeneratorIndex)%report%TotalHeatEnergyRec

  RETURN

END SUBROUTINE GetMicroCHPGeneratorResults



End Module MicroCHPElectricGenerator
!




!*******************************************************************************************************
MODULE FuelCellElectricGenerator

          ! MODULE INFORMATION:
          !       AUTHOR         Brent Griffth
          !       DATE WRITTEN   August. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the operation of Solid oxide fuel cell Generators.

          ! METHODOLOGY EMPLOYED:
          ! Once the ElectricPowerManager determines that the FuelCell Generator
          ! is available to meet an electric load demand, it calls SimFuelCellGenerator
          ! which in turn calls the FuelCell model.
          ! See DataGenerators.f90 for structures and variables

          ! REFERENCES:
          ! IEA/ECBCS Annex 42 model specification for Solid oxide and proton exchange membrane fuel cells

          ! OTHER NOTES:
          ! N/A

          ! USE STATEMENTS:
USE DataGenerators
USE DataLoopNode
USE DataGlobals ,   ONLY : MaxNameLength, NumOfTimeStepInHour, NumOfZones, CurrentTime, DayOfSim, SecInHour,   &
                           BeginEnvrnFlag, InitConvTemp, WarmUpFlag, KelvinConv, HoursInDay
USE DataInterfaces
USE DataGlobalConstants, ONLY: iGeneratorFuelCell
USE GeneratorFuelSupply
USE GeneratorDynamicsManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS


  ! DERIVED TYPE DEFINITIONS

          ! MODULE VARIABLE DECLARATIONS:
LOGICAL  :: GetFuelCellInput = .TRUE.! When TRUE, calls subroutine to read input file.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE FuelCell ElectricGenerator
PUBLIC     SimFuelCellGenerator  ! call handler, gets input first time
PUBLIC     GetFuelCellGeneratorResults

PRIVATE    GetFuelCellGeneratorInput ! gathers user data from input processor
!PRIVATE    SetupFuelAndAirConstituentData ! hardwired data for gas phase thermochemistry calcs
PRIVATE    InitFuelCellGenerators

PRIVATE    CalcFuelCellGeneratorModel ! main calculations for FuelCell poer module

PRIVATE    FigureAirHeatCap  ! molar heat capacity for "air" constituents
PRIVATE    FigureAirEnthalpy ! molar enthalpy for "air" constituents
PRIVATE    FigureFuelHeatCap ! molar heat capacity for fuel constituents
PRIVATE    FigureFuelEnthalpy  ! molar enthalpy for fuel constituents
PRIVATE    FigureProductGasHeatCap  ! molar heat capacity for product gas constituents
PRIVATE    FigureProductGasesEnthalpy ! molar enthalpy for product gas constituents
PRIVATE    FigureLiquidWaterHeatCap ! calculate shomate eq. for pure liquid water
PRIVATE    FuelCellProductGasEnthResidual ! function in call to numerical solver, uses FigureProductGasesEnthalpy
PRIVATE    FigureLHVofFuel ! Not used, for Eq. 6 in Annex 42 spec, see SetupFuelAndAirConstituentData

PRIVATE    FigureACAncillaries
PRIVATE    FigurePowerConditioningLosses
PRIVATE    FigureTransientConstraints

PRIVATE    CalcFuelCellAuxHeater
PRIVATE    CalcFuelCellGenHeatRecovery

PUBLIC     SimFuelCellPlantHeatRecovery

PUBLIC     FigureFuelCellZoneGains
PRIVATE    UpdateExhaustAirFlows
PRIVATE    ManageElectStorInteractions ! electrical storage (battery) inside the unit
PRIVATE    CalcUpdateHeatRecovery
PRIVATE    UpdateFuelCellGeneratorRecords ! Sets variables used in reporting

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of FuelCell Generator Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimFuelCellGenerator(GeneratorType,GeneratorName,GeneratorIndex,RunFlag, MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   MArch 2005
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the Solid oxide fuel cell Generator model driver.  It
          ! gets the input for the models, initializes simulation variables, call
          ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GeneratorType     ! type of Generator
  CHARACTER(len=*), INTENT(IN) :: GeneratorName     ! user specified name of Generator
  INTEGER, INTENT(INOUT) :: GeneratorIndex
  LOGICAL , INTENT(IN)   :: RunFlag                 ! simulate Generator when TRUE
  REAL(r64), INTENT(IN)       :: MyLoad                  ! demand on electric generator
  LOGICAL, INTENT (IN)   :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: GenNum           ! Generator number counter

        !Get Generator data from input file
  IF (GetFuelCellInput) THEN
    CALL GetFuelCellGeneratorInput
    GetFuelCellInput = .FALSE.
  END IF

  IF (GeneratorIndex == 0) THEN
    GenNum = FindItemInList(GeneratorName,FuelCell%Name,NumFuelCellGenerators)
    IF (GenNum == 0) CALL ShowFatalError('SimFuelCellGenerator: Specified Generator not one of Valid FuelCell Generators '//  &
                                       TRIM(GeneratorName))
    GeneratorIndex=GenNum
  ELSE
    GenNum=GeneratorIndex
    IF (GenNum > NumFuelCellGenerators .or. GenNum < 1) THEN
      CALL ShowFatalError('SimFuelCellGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                          ', Number of FuelCell Generators='//TRIM(TrimSigDigits(NumFuelCellGenerators))//  &
                          ', Generator name='//TRIM(GeneratorName))
    ENDIF
    IF (CheckEquipName(GenNum)) THEN
      IF (GeneratorName /= FuelCell(GenNum)%Name) THEN
        CALL ShowFatalError('SimFuelCellGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                            ', Generator name='//TRIM(GeneratorName)//', stored Generator Name for that index='//  &
                            TRIM(FuelCell(GenNum)%Name))
      ENDIF
      CheckEquipName(GenNum)=.false.
    ENDIF
  ENDIF

  Call InitFuelCellGenerators(GenNum)

  CALL CalcFuelCellGeneratorModel(GenNum,RunFlag,MyLoad,FirstHVACIteration)

  CALL CalcUpdateHeatRecovery(GenNum, FirstHVACIteration)

  CALL UpdateFuelCellGeneratorRecords(RunFlag,GenNum)

RETURN
END SUBROUTINE SimFuelCellGenerator

! End FuelCell Generator Module Driver Subroutines
!******************************************************************************


! Beginning of FuelCell Generator Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetFuelCellGeneratorInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Brent Griffith
            !       DATE WRITTEN:    April 2005

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the FuelCell Generator models.

            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataGenerators
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, SameString, &
                             FindItem
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE CurveManager,   ONLY : GetCurveIndex
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataHeatBalance,  ONLY: Zone, IntGainTypeOf_GeneratorFuelCell
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE General, ONLY: RoundSigDigits
  USE DataGlobals, ONLY: DisplayAdvancedReportVariables
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: GeneratorNum !Generator counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(25)  :: AlphArray !character string data
  REAL(r64),                        DIMENSION(200)  :: NumArray  !numeric data TODO deal with allocatable for extensible
  LOGICAL, SAVE :: ErrorsFound=.false.  ! error flag
  LOGICAL       :: IsNotOK              ! Flag to verify name
  LOGICAL       :: IsBlank              ! Flag for blank name
  INTEGER       :: NumFuelCellPMs           ! number of power subsystems in input file
  INTEGER       :: NumFuelCellAirSups       ! number of air supply subsystems in input file
!  INTEGER       :: NumFuelCellFuelSups      ! number of fuel supply subsystems in input file
  INTEGER       :: NumFCWaterSups       ! number of water supply subsystems in input file
  INTEGER       :: NumFuelCellAuxilHeaters
  INTEGER       :: NumFCExhaustGasHXs
  INTEGER       :: NumFCElecStorageUnits !number of electrical storage objects in input file
!  INTEGER       :: NumBatteries  !number of Manwell and McGowan battery data objects
  INTEGER       :: NumFCPowerCondUnits  ! number of power conditioning units (inverter)
  INTEGER       :: NumFCStackCoolers    ! number of stack coolers.
  INTEGER       :: NumAirConstit        ! number of gas constituents in air
  INTEGER       :: FCPMNum   !loop counter over power subsystems
  INTEGER       :: FCAirSupNum !loop counter over air supply subsystems
!  INTEGER       :: FCFuelSupNum !loop counter over fuel supply subsystems
  INTEGER       :: ConstitNum ! loop counter for consituents
  INTEGER       :: FCWaterSupNum !loop counter over water supply subsystems
  INTEGER       :: FCHXNum !loop counter for heat exchangers
  INTEGER       :: FCAuxHeatNum  !loop counter over auxiliar heater
  INTEGER       :: FCPCUNum      !loop counter over inverters
  INTEGER       :: StorageNum    !loop counter over electrical storage subsystems
  INTEGER       :: FCScoolNum    ! loop counter over stack coolers
  INTEGER       :: thisFuelCell  !temporary index
  INTEGER       :: otherFuelCell !loop counter and temporary indexer
  INTEGER       :: I   ! loop counter
  Logical, SAve :: myonetimeFlag = .true.
  CHARACTER(len=MaxNameLength) :: thisname
  INTEGER       :: NumHardCodedConstituents
  INTEGER       :: thisConstituent
  INTEGER       :: thisGasID
  INTEGER       :: FuelSupNum

  ! execution
If (myonetimeflag) then

  cCurrentModuleObject = 'Generator:FuelCell'
  NumFuelCellGenerators = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFuelCellGenerators <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

         !ALLOCATE ARRAYS
  ALLOCATE (FuelCell(NumFuelCellGenerators))  ! inits handeled in derived type definitions
  ALLOCATE(CheckEquipName(NumFuelCellGenerators))
  CheckEquipName=.true.

  ! first load in FuelCell names
  DO GeneratorNum = 1 , NumFuelCellGenerators
    CALL GetObjectItem(cCurrentModuleObject,GeneratorNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%Name,GeneratorNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    FuelCell(GeneratorNum)%Name                = AlphArray(1)
    FuelCell(GeneratorNum)%NameFCPM            = AlphArray(2)
    FuelCell(GeneratorNum)%NameFCAirSup        = AlphArray(3)
    FuelCell(GeneratorNum)%NameFCFuelSup       = AlphArray(4)
    FuelCell(GeneratorNum)%NameFCWaterSup      = AlphArray(5)
    FuelCell(GeneratorNum)%NameFCAuxilHeat     = AlphArray(6)
    FuelCell(GeneratorNum)%NameExhaustHX       = AlphArray(7)
    FuelCell(GeneratorNum)%NameElecStorage     = AlphArray(8)
    FuelCell(GeneratorNum)%NameInverter        = AlphArray(9)
    If (numAlphas == 10) then
      FuelCell(GeneratorNum)%NameStackCooler     = AlphArray(10)
    ENDIF
  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:PowerModule'
  NumFuelCellPMs = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFuelCellPMs <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF


  DO FCPMNum = 1 , NumFuelCellPMs
    CALL GetObjectItem(cCurrentModuleObject,FCPMNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%FCPM%Name,FCPMNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameFCPM,NumFuelCellGenerators)
    IF (thisFuelCell > 0) THEN !cr9323

      FuelCell(thisFuelCell)%FCPM%Name = AlphArray(1)
      IF (SameString(AlphArray(2), 'ANNEX42')) FuelCell(thisFuelCell)%FCPM%EffMode = DirectCurveMode
      IF (SameSTring(AlphArray(2), 'NORMALIZED')) FuelCell(thisFuelCell)%FCPM%EffMode = NormalizedCurveMode
      IF (FuelCell(thisFuelCell)%FCPM%EffMode == 0) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound = .true.
      ENDIF
      FuelCell(thisFuelCell)%FCPM%EffCurveID = GetCurveIndex(AlphArray(3))
      IF (FuelCell(thisFuelCell)%FCPM%EffCurveID == 0) THEN
         CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
         ErrorsFound = .true.
      ENDIF

      FuelCell(thisFuelCell)%FCPM%NomEff           = NumArray(1)
      FuelCell(thisFuelCell)%FCPM%NomPel           = NumArray(2)
      FuelCell(thisFuelCell)%FCPM%NumCycles        = NumArray(3)
      FuelCell(thisFuelCell)%FCPM%CyclingDegradRat = NumArray(4)
      FuelCell(thisFuelCell)%FCPM%NumRunHours      = NumArray(5)
      FuelCell(thisFuelCell)%FCPM%OperateDegradRat = NumArray(6)
      FuelCell(thisFuelCell)%FCPM%ThreshRunHours   = NumArray(7)
      FuelCell(thisFuelCell)%FCPM%UpTranLimit      = NumArray(8)
      FuelCell(thisFuelCell)%FCPM%DownTranLimit    = NumArray(9)
      FuelCell(thisFuelCell)%FCPM%StartUpTime      = NumArray(10)/SecInHour !convert to hours from seconds
      FuelCell(thisFuelCell)%FCPM%StartUpFuel      = NumArray(11)
      FuelCell(thisFuelCell)%FCPM%StartUpElectConsum = NumArray(12)
      FuelCell(thisFuelCell)%FCPM%StartUpElectProd = NumArray(13)
      FuelCell(thisFuelCell)%FCPM%ShutDownTime    = NumArray(14)/SecInHour !convert to hours from seconds
      FuelCell(thisFuelCell)%FCPM%ShutDownFuel    = NumArray(15)
      FuelCell(thisFuelCell)%FCPM%ShutDownElectConsum = NumArray(16)
      FuelCell(thisFuelCell)%FCPM%ANC0           = NumArray(17)
      FuelCell(thisFuelCell)%FCPM%ANC1           = NumArray(18)
      IF (SameString(AlphArray(4),'ConstantRate')) &
                   FuelCell(thisFuelCell)%FCPM%SkinLossMode = ConstantRateSkinLoss
      IF (SameString(AlphArray(4),'UAForProcessGasTemperature')) &
                   FuelCell(thisFuelCell)%FCPM%SkinLossMode = UADTSkinLoss
      IF (SameString(AlphArray(4),'QUADRATIC FUNCTION OF FUEL RATE')) &
                   FuelCell(thisFuelCell)%FCPM%SkinLossMode = QuadraticFuelNdotSkin
      IF (FuelCell(thisFuelCell)%FCPM%SkinLossMode == 0) THEN
         !throw error
         CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
         ErrorsFound = .true.

      ENDIF
      FuelCell(thisFuelCell)%FCPM%ZoneName = AlphArray(5)
      FuelCell(thisFuelCell)%FCPM%ZoneID   = FindItemInList(FuelCell(thisFuelCell)%FCPM%ZoneName, Zone%Name, NumOfZones)
      IF (FuelCell(thisFuelCell)%FCPM%ZoneID == 0 ) THEN
         CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(AlphArray(5)))
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
         Call ShowContinueError('Zone Name was not found ')
         ErrorsFound = .true.
      ENDIF

      FuelCell(thisFuelCell)%FCPM%RadiativeFract = NumArray(19)
      FuelCell(thisFuelCell)%FCPM%QdotSkin       = NumArray(20)
      FuelCell(thisFuelCell)%FCPM%UAskin         = NumArray(21)

      FuelCell(thisFuelCell)%FCPM%SkinLossCurveID = GetCurveIndex(AlphArray(6))
      IF (FuelCell(thisFuelCell)%FCPM%SkinLossCurveID == 0) THEN
         If (FuelCell(thisFuelCell)%FCPM%SkinLossMode == QuadraticFuelNdotSkin) THEN
           CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
           CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
           ErrorsFound = .true.
         ENDIF
      ENDIF

      FuelCell(thisFuelCell)%FCPM%NdotDilutionAir = NumArray(22)
      FuelCell(thisFuelCell)%FCPM%StackHeatLossToDilution = NumArray(23)
      FuelCell(thisFuelCell)%FCPM%DilutionInletNodeName = AlphArray(7)
      FuelCell(thisFuelCell)%FCPM%DilutionInletNode =     &
                 GetOnlySingleNode(AlphArray(7),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      FuelCell(thisFuelCell)%FCPM%DilutionExhaustNodeName = AlphArray(8)
      FuelCell(thisFuelCell)%FCPM%DilutionExhaustNode =     &
                 GetOnlySingleNode(AlphArray(8),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      FuelCell(thisFuelCell)%FCPM%PelMin = NumArray(24)
      FuelCell(thisFuelCell)%FCPM%PelMax = NumArray(25)

      !check for other FuelCell using the same power module and fill
      DO otherFuelCell = thisFuelCell+1, NumFuelCellGenerators
        IF (SameString(FuelCell(otherFuelCell)%FCPM%Name, FuelCell(thisFuelCell)%FCPM%Name)) THEN
           FuelCell(otherFuelCell)%FCPM = FuelCell(thisFuelCell)%FCPM
        ENDIF

      ENDDO
    ELSE ! throw warning, did not find power module input
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF
  ENDDO ! loop over NumFuelCellPMs

  Call GetGeneratorFuelSupplyInput

  DO FuelSupNum = 1 , NumGeneratorFuelSups
    CAll SetupFuelConstituentData(FuelSupNum, ErrorsFound)
  ENDDO

 !set fuel supply ID in Fuel cell structure
  DO GeneratorNum = 1, NumFuelCellGenerators
    FuelCell(GeneratorNum)%FuelSupNum  =   &
       FindItemInList( FuelCell(GeneratorNum)%NameFCFuelSup , FuelSupply%name,NumGeneratorFuelSups) ! Fuel Supply ID
    IF (FuelCell(GeneratorNum)%FuelSupNum == 0) THEN
      CALL ShowSevereError('Fuel Supply Name: '// TRIM(FuelCell(GeneratorNum)%NameFCFuelSup) &
            //' not found in ' //   FuelCell(GeneratorNum)%Name )
      ErrorsFound = .true.
    ENDIF
  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:AirSupply'
  NumFuelCellAirSups = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFuelCellAirSups <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

  DO FCAirSupNum = 1 , NumFuelCellAirSups
    CALL GetObjectItem(cCurrentModuleObject,FCAirSupNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)


    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%AirSup%Name,FCAirSupNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameFCAirSup,NumFuelCellGenerators)
    IF (thisFuelCell > 0 ) THEN

      FuelCell(thisFuelCell)%AirSup%Name     = AlphArray(1)
      FuelCell(thisFuelCell)%AirSup%NodeName = AlphArray(2)

       ! check the node connections
      FuelCell(thisFuelCell)%AirSup%SupNodeNum = &
                 GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      FuelCell(thisFuelCell)%AirSup%BlowerPowerCurveID = GetCurveIndex(AlphArray(3))
      If (FuelCell(thisFuelCell)%AirSup%BlowerPowerCurveID == 0) Then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Curve name was not found ')
        errorsFound = .true.
      ENDIF
      FuelCell(thisFuelCell)%AirSup%BlowerHeatLossFactor = NumArray(1)

      IF      (SameString(AlphArray(4), 'AirRatiobyStoics')) THEN
        FuelCell(thisFuelCell)%AirSup%AirSupRateMode = ConstantStoicsAirRat
      ELSEIF (SameString(AlphArray(4), 'QuadraticFunctionofElectricPower')) THEN
        FuelCell(thisFuelCell)%AirSup%AirSupRateMode = QuadraticFuncofPel
      ELSEIF (SameString(AlphArray(4), 'QUADRATIC FUNCTION OF FUEL RATE'))   THEN
        FuelCell(thisFuelCell)%AirSup%AirSupRateMode = QuadraticFuncofNdot
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.
      ENDIF

      FuelCell(thisFuelCell)%AirSup%Stoics = NumArray(2) + 1.0d0  !

      FuelCell(thisFuelCell)%AirSup%AirFuncPelCurveID = GetCurveIndex(AlphArray(5))
      If ((FuelCell(thisFuelCell)%AirSup%AirFuncPelCurveID == 0) .AND. &
          (FuelCell(thisFuelCell)%AirSup%AirSupRateMode == QuadraticFuncofPel)) then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(AlphArray(5)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowSevereError('Curve name was not found')
        errorsFound = .true.
      ENDIF

      FuelCell(thisFuelCell)%AirSup%AirTempCoeff = NumArray(3)

      FuelCell(thisFuelCell)%AirSup%AirFuncNdotCurveID = GetCurveIndex(AlphArray(6))
      If ((FuelCell(thisFuelCell)%AirSup%AirFuncNdotCurveID ==0) .AND. &
          (FuelCell(thisFuelCell)%AirSup%AirSupRateMode == QuadraticFuncofNdot)) then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowSevereError('Curve name was not found')
        errorsFound = .true.
      ENDIF

      If(SameString('RecoverBurnerInverterStorage',AlphArray(7))) THEN
        FuelCell(thisFuelCell)%AirSup%IntakeRecoveryMode = RecoverBurnInvertBatt
      ELSEIF (SameString('RecoverAuxiliaryBurner',AlphArray(7))) THEN
        FuelCell(thisFuelCell)%AirSup%IntakeRecoveryMode = RecoverAuxiliaryBurner
      ELSEIF (SameString('RecoverInverterandStorage',AlphArray(7))) THEN
        FuelCell(thisFuelCell)%AirSup%IntakeRecoveryMode = RecoverInverterBatt
      ELSEIF (SameString('RecoverInverter',AlphArray(7))) THEN
        FuelCell(thisFuelCell)%AirSup%IntakeRecoveryMode = RecoverInverter
      ELSEIF (SameString('RecoverElectricalStorage',AlphArray(7))) THEN
        FuelCell(thisFuelCell)%AirSup%IntakeRecoveryMode = RecoverBattery
      ELSEIF (SameString('NoRecovery',AlphArray(7))) THEN
        FuelCell(thisFuelCell)%AirSup%IntakeRecoveryMode = NoRecoveryOnAirIntake
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(AlphArray(7)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.
      ENDIF


      If(SameString('AmbientAir',AlphArray(8))) THEN
        FuelCell(thisFuelCell)%AirSup%ConstituentMode = RegularAir
      ELSEIF (SameString('UserDefinedConstituents',AlphArray(8))) THEN
        FuelCell(thisFuelCell)%AirSup%ConstituentMode = UserDefinedConstituents
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(AlphArray(8)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.
      ENDIF

      If (FuelCell(thisFuelCell)%AirSup%ConstituentMode == UserDefinedConstituents) THEN
        NumAirConstit  = NumArray(4)
        FuelCell(thisFuelCell)%AirSup%NumConstituents =NumAirConstit

        IF (NumAirConstit > 5) THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(4))//'='//TRIM(RoundSigDigits(NumArray(4),2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
          CALL ShowContinueError('Fuel Cell model not set up for more than 5 air constituents')
          ErrorsFound = .true.
        ENDIF

        DO ConstitNum=1, NumAirConstit
          FuelCell(thisFuelCell)%AirSup%ConstitName(ConstitNum)       = AlphArray(ConstitNum + 8)
          FuelCell(thisFuelCell)%AirSup%ConstitMolalFract(ConstitNum) = NumArray(ConstitNum + 4)

        ENDDO

      ELSE  !regular air
        NumAirConstit  = 5

        FuelCell(thisFuelCell)%AirSup%NumConstituents = NumAirConstit

        FuelCell(thisFuelCell)%AirSup%ConstitName(1)       = 'Nitrogen'
        FuelCell(thisFuelCell)%AirSup%ConstitMolalFract(1) = 0.7728d0

        FuelCell(thisFuelCell)%AirSup%ConstitName(2)       = 'Oxygen'
        FuelCell(thisFuelCell)%AirSup%ConstitMolalFract(2) = 0.2073d0

        FuelCell(thisFuelCell)%AirSup%ConstitName(3)       = 'Water'
        FuelCell(thisFuelCell)%AirSup%ConstitMolalFract(3) = 0.0104d0

        FuelCell(thisFuelCell)%AirSup%ConstitName(4)       = 'Argon'
        FuelCell(thisFuelCell)%AirSup%ConstitMolalFract(4) = 0.0092d0

        FuelCell(thisFuelCell)%AirSup%ConstitName(5)       = 'CarbonDioxide'
        FuelCell(thisFuelCell)%AirSup%ConstitMolalFract(5) = 0.0003d0

      ENDIF

      ! check for molar fractions summing to 1.0.
      IF (ABS(SUM(FuelCell(thisFuelCell)%AirSup%ConstitMolalFract)-1.0d0) > .0001d0) THEN

        CALL showSevereError(TRIM(cCurrentModuleObject)//' molar fractions do not sum to 1.0' )
        CALL ShowContinueError('..Sum was='//TRIM(RoundSigDigits(SUM(FuelCell(thisFuelCell)%AirSup%ConstitMolalFract),1)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        errorsfound = .true.
      ENDIF

      !check for other FuelCell using the same Air Supply module and fill
      DO otherFuelCell = thisFuelCell+1, NumFuelCellGenerators
        IF (SameString(FuelCell(otherFuelCell)%AirSup%Name, FuelCell(thisFuelCell)%AirSup%Name)) THEN
           FuelCell(otherFuelCell)%AirSup = FuelCell(thisFuelCell)%AirSup
        ENDIF
      ENDDO
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF

  ENDDO

  DO GeneratorNum = 1, NumFuelCellGenerators
    ! find molal fraction of oxygen in air supply
    thisConstituent = FindItem('Oxygen', FuelCell(GeneratorNum)%AirSup%ConstitName, &
                                               FuelCell(GeneratorNum)%AirSup%NumConstituents)
    IF (thisConstituent > 0) FuelCell(GeneratorNum)%AirSup%O2fraction =   &
       FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisConstituent)

    NumHardCodedConstituents = 14

    ! Loop over air constituents and do one-time setup
    DO i=1, FuelCell(GeneratorNum)%AirSup%NumConstituents

      thisName = FuelCell(GeneratorNum)%AirSup%ConstitName(i)

      thisGasID = FindItem(thisName, GasPhaseThermoChemistryData%ConstituentName, NumHardCodedConstituents)

      FuelCell(GeneratorNum)%AirSup%GasLibID(i) = thisGasID

    ENDDO

    !set up gas constiuents for product gases
    FuelCell(GeneratorNum)%FCPM%GasLibID(1) = 1 !Carbon Dioxide
    FuelCell(GeneratorNum)%FCPM%GasLibID(2) = 2 !Nitrogen
    FuelCell(GeneratorNum)%FCPM%GasLibID(3) = 3 !Oxygen
    FuelCell(GeneratorNum)%FCPM%GasLibID(4) = 4 !Water
    FuelCell(GeneratorNum)%FCPM%GasLibID(5) = 5 !Argon
  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:WaterSupply'
  NumFCWaterSups = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFCWaterSups <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

  DO FCWaterSupNum = 1 , NumFCWaterSups
    CALL GetObjectItem(cCurrentModuleObject,FCWaterSupNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%WaterSup%Name,FCWaterSupNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameFCWaterSup,NumFuelCellGenerators)
    IF (thisFuelCell > 0) THEN
     !  this is only the first instance of a FuelCell generator using this type of Water supply module
      FuelCell(thisFuelCell)%WaterSup%Name = AlphArray(1)
      FuelCell(thisFuelCell)%WaterSup%WaterSupRateCurveID =  GetCurveIndex(AlphArray(2))
      IF (FuelCell(thisFuelCell)%WaterSup%WaterSupRateCurveID == 0) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Curve name was not found ')
        errorsFound = .true.
      ENDIF
      FuelCell(thisFuelCell)%WaterSup%PmpPowerCurveID = GetCurveIndex(AlphArray(3))
      IF (FuelCell(thisFuelCell)%WaterSup%PmpPowerCurveID == 0 ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Curve name was not found ')
        errorsFound = .true.
      ENDIF
      FuelCell(thisFuelCell)%WaterSup%PmpPowerLossFactor = NumArray(1)

  !!CR9240?
      IF(SameString('TemperatureFromAirNode',AlphArray(4))) THEN
        FuelCell(thisFuelCell)%WaterSup%WaterTempMode = WaterInReformAirNode

        FuelCell(thisFuelCell)%WaterSup%NodeName = AlphArray(5)
        FuelCell(thisFuelCell)%WaterSup%NodeNum  = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

      ELSEIF(SameString('TemperatureFromWaterNode',AlphArray(4))) THEN
        FuelCell(thisFuelCell)%WaterSup%WaterTempMode = WaterInReformWaterNode

        FuelCell(thisFuelCell)%WaterSup%NodeName = AlphArray(5)
        FuelCell(thisFuelCell)%WaterSup%NodeNum  = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Sensor,1,ObjectIsNotParent)

      ELSEIF(SameString('MainsWaterTemperature' , AlphArray(4))) Then
        FuelCell(thisFuelCell)%WaterSup%WaterTempMode = WaterInReformMains

      ELSEIF (SameString('TemperatureFromSchedule', AlphArray(4))) THEN
        FuelCell(thisFuelCell)%WaterSup%WaterTempMode = WaterInReformSchedule
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.
      ENDIF


      FuelCell(thisFuelCell)%WaterSup%SchedNum = GetScheduleIndex(AlphArray(6))
      IF ((FuelCell(thisFuelCell)%WaterSup%SchedNum == 0) .AND. &
         (FuelCell(thisFuelCell)%WaterSup%WaterTempMode == WaterInReformSchedule)) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CAll ShowContinueError('Schedule was not found')
        errorsFound = .true.
      ENDIF


      !check for other FuelCell using the same Water Supply module and fill
      DO otherFuelCell = thisFuelCell+1, NumFuelCellGenerators
        IF (SameString(FuelCell(otherFuelCell)%WaterSup%Name, FuelCell(thisFuelCell)%WaterSup%Name)) THEN
           FuelCell(otherFuelCell)%WaterSup = FuelCell(thisFuelCell)%WaterSup
        ENDIF
      ENDDO
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF

  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:AuxiliaryHeater'
  NumFuelCellAuxilHeaters = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFuelCellAuxilHeaters <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

  DO FCAuxHeatNum = 1 , NumFuelCellAuxilHeaters
    CALL GetObjectItem(cCurrentModuleObject,FCAuxHeatNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%AuxilHeat%Name,FCAuxHeatNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameFCAuxilHeat,NumFuelCellGenerators)
    IF (thisFuelCell > 0) THEN
      FuelCell(thisFuelCell)%AuxilHeat%Name = AlphArray(1)

      FuelCell(thisFuelCell)%AuxilHeat%ExcessAirRAT = NumArray(1)
      FuelCell(thisFuelCell)%AuxilHeat%ANC0         = NumArray(2)
      FuelCell(thisFuelCell)%AuxilHeat%ANC1         = NumArray(3)
      FuelCell(thisFuelCell)%AuxilHeat%UASkin       = NumArray(4)

      If (SameString('SurroundingZone', AlphArray(2))) Then
        FuelCell(thisFuelCell)%AuxilHeat%SkinLossDestination  = SurroundingZone
      ELSEIF (SameString('AirInletForFuelCell', AlphArray(2))) Then
        FuelCell(thisFuelCell)%AuxilHeat%SkinLossDestination  =  AirInletForFC
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound = .true.
      ENDIF

      FuelCell(thisFuelCell)%AuxilHeat%ZoneName = AlphArray(3)
      FuelCell(thisFuelCell)%AuxilHeat%ZoneID = FindItemInList(AlphArray(3),Zone%Name,size(Zone) )
      IF ((FuelCell(thisFuelCell)%AuxilHeat%ZoneID == 0 )    &
         .AND. (FuelCell(thisFuelCell)%AuxilHeat%SkinLossDestination == SurroundingZone)) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Zone name was not found ')
        errorsfound = .true.
      ENDIF
      FuelCell(thisFuelCell)%AuxilHeat%MaxPowerW   = NumArray(5)
      FuelCell(thisFuelCell)%AuxilHeat%MinPowerW   = NumArray(6)
      FuelCell(thisFuelCell)%AuxilHeat%MaxPowerkmolperSec   = NumArray(7)
      FuelCell(thisFuelCell)%AuxilHeat%MinPowerkmolperSec   = NumArray(8)

      ! TODO finish Auxiliary heater

      !check for other FuelCell using the same Auxiliary Heating module and fill
      DO otherFuelCell = thisFuelCell+1, NumFuelCellGenerators
        IF (SameString(FuelCell(otherFuelCell)%AuxilHeat%Name, FuelCell(thisFuelCell)%AuxilHeat%Name)) THEN
           FuelCell(otherFuelCell)%AuxilHeat = FuelCell(thisFuelCell)%AuxilHeat
        ENDIF
      ENDDO
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF

  ENDDO

  ! exhaust gas heat exchanger
  cCurrentModuleObject  = 'Generator:FuelCell:ExhaustGasToWaterHeatExchanger'
  NumFCExhaustGasHXs    = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumFCExhaustGasHXs  <= 0) THEN
    CALL ShowWarningError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    CALL ShowContinueError('Fuel Cell model requires an '//TRIM(cCurrentModuleObject)//' object')
    ErrorsFound=.true.
  ENDIF

  DO FCHXNum = 1 , NumFCExhaustGasHXs
    CALL GetObjectItem(cCurrentModuleObject,FCHXNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%ExhaustHX%Name,FCHXNum-1,IsNotOK,IsBlank,  &
                            TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameExhaustHX,NumFuelCellGenerators)
    IF (thisFuelCell > 0) THEN
      FuelCell(thisFuelCell)%ExhaustHX%Name            = AlphArray(1)
      FuelCell(thisFuelCell)%ExhaustHX%WaterInNodeName = AlphArray(2)
      FuelCell(thisFuelCell)%ExhaustHX%WaterOutNodeName= AlphArray(3)
      !find node ids for water path
      FuelCell(thisFuelCell)%ExhaustHX%WaterInNode =  &
                 GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                 NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent )
      FuelCell(thisFuelCell)%ExhaustHX%WaterOutNode =  &
                 GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                 NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent )
      CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),  &
                              'Heat Recovery Nodes')

  !CR9240
      FuelCell(thisFuelCell)%ExhaustHX%ExhaustOutNodeName =  AlphArray(4)
      FuelCell(thisFuelCell)%ExhaustHX%ExhaustOutNode     = &
                 GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                 NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent )

      IF     (SameString('FixedEffectiveness', AlphArray(5))) THEN
        FuelCell(thisFuelCell)%ExhaustHX%HXmodelMode = FixedEffectiveness
      ELSEIF (SameString('EmpiricalUAeff', AlphArray(5))) THEN
        FuelCell(thisFuelCell)%ExhaustHX%HXmodelMode = LMTDempiricalUAeff
      ELSEIF (SameString('FundementalUAeff', AlphArray(5))) THEN
        FuelCell(thisFuelCell)%ExhaustHX%HXmodelMode = LMTDfundementalUAeff
      ELSEIF (SameString('CONDENSING', AlphArray(5))) THEN
        FuelCell(thisFuelCell)%ExhaustHX%HXmodelMode = Condensing
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(AlphArray(5)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.

      ENDIF
      FuelCell(thisFuelCell)%ExhaustHX%WaterVolumeFlowMax= NumArray(1)
      FuelCell(thisFuelCell)%ExhaustHX%HXEffect          = NumArray(2)
      FuelCell(thisFuelCell)%ExhaustHX%hxs0              = NumArray(3)
      FuelCell(thisFuelCell)%ExhaustHX%hxs1              = NumArray(4)
      FuelCell(thisFuelCell)%ExhaustHX%hxs2              = NumArray(5)
      FuelCell(thisFuelCell)%ExhaustHX%hxs3              = NumArray(6)
      FuelCell(thisFuelCell)%ExhaustHX%hxs4              = NumArray(7)
      FuelCell(thisFuelCell)%ExhaustHX%h0gas             = NumArray(8)
      FuelCell(thisFuelCell)%ExhaustHX%NdotGasRef        = NumArray(9)
      FuelCell(thisFuelCell)%ExhaustHX%nCoeff            = NumArray(10)
      FuelCell(thisFuelCell)%ExhaustHX%AreaGas           = NumArray(11)
      FuelCell(thisFuelCell)%ExhaustHX%h0Water           = NumArray(12)
      FuelCell(thisFuelCell)%ExhaustHX%NdotWaterRef      = NumArray(13)
      FuelCell(thisFuelCell)%ExhaustHX%mCoeff            = NumArray(14)
      FuelCell(thisFuelCell)%ExhaustHX%AreaWater         = NumArray(15)
      FuelCell(thisFuelCell)%ExhaustHX%Fadjust           = NumArray(16)
      FuelCell(thisFuelCell)%ExhaustHX%l1Coeff           = NumArray(17)
      FuelCell(thisFuelCell)%ExhaustHX%l2Coeff           = NumArray(18)
      FuelCell(thisFuelCell)%ExhaustHX%CondensationThresholdTemp =  NumArray(19)

      ! store cooling water volume flow rate for autosizing system
      CALL RegisterPlantCompDesignFlow(FuelCell(thisFuelCell)%ExhaustHX%WaterInNode, &
                                 FuelCell(thisFuelCell)%ExhaustHX%WaterVolumeFlowMax )
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF
  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:ElectricalStorage'
  NumFCElecStorageUnits = GetNumObjectsFound(cCurrentModuleObject)

  If (NumFCElecStorageUnits <= 0) THEN
    Call ShowWarningError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    CALL ShowContinueError('Fuel Cell model requires an '//TRIM(cCurrentModuleObject)//' object')
    errorsFound = .true.
  ENDIF

  DO StorageNum = 1, NumFCElecStorageUnits
    CALL GetObjectItem(cCurrentModuleObject,StorageNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%ElecStorage%Name,StorageNum-1,IsNotOK,IsBlank,  &
       TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameElecStorage,NumFuelCellGenerators)
    IF (thisFuelCell > 0) THEN
      FuelCell(thisFuelCell)%ElecStorage%Name = AlphArray(1)

      IF (SameString(AlphArray(2), 'SimpleEfficiencyWithConstraints')) THEN
         FuelCell(thisFuelCell)%ElecStorage%StorageModelMode  = SimpleEffConstraints
      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsFound = .true.

      ENDIF
      FuelCell(thisFuelCell)%ElecStorage%EnergeticEfficCharge    = NumArray(1)
      FuelCell(thisFuelCell)%ElecStorage%EnergeticEfficDischarge = NumArray(2)
      FuelCell(thisFuelCell)%ElecStorage%NominalEnergyCapacity   = NumArray(3)
      FuelCell(thisFuelCell)%ElecStorage%MaxPowerDraw            = NumArray(4)
      FuelCell(thisFuelCell)%ElecStorage%MaxPowerStore           = NumArray(5)
      FuelCell(thisFuelCell)%ElecStorage%StartingEnergyStored    = NumArray(6)

          !check for other FuelCell using the same Electrical Storage and fill
      DO otherFuelCell = thisFuelCell+1, NumFuelCellGenerators
        IF (SameString(FuelCell(otherFuelCell)%ElecStorage%Name, FuelCell(thisFuelCell)%ElecStorage%Name)) THEN
           FuelCell(otherFuelCell)%ElecStorage = FuelCell(thisFuelCell)%ElecStorage
        ENDIF
      ENDDO
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF

  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:Inverter'
  NumFCPowerCondUnits = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFCPowerCondUnits  <= 0) THEN
    CALL ShowWarningError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    CALL ShowContinueError('Fuel Cell model requires a '//TRIM(cCurrentModuleObject)//' object')

    ErrorsFound=.true.
  ENDIF

  DO FCPCUNum = 1 , NumFCPowerCondUnits
    CALL GetObjectItem(cCurrentModuleObject,FCPCUNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)


    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),FuelCell%Inverter%Name,FCPCUNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameInverter,NumFuelCellGenerators)
    IF (thisFuelCell > 0) THEN
      FuelCell(thisFuelCell)%Inverter%Name = AlphArray(1)

      IF (SameString(AlphArray(2), 'QUADRATIC')) FuelCell(thisFuelCell)%Inverter%EffMode = InverterEffQuadratic
      IF (SameSTring(AlphArray(2), 'Constant'))  FuelCell(thisFuelCell)%Inverter%EffMode = InverterEffConstant
      IF (FuelCell(thisFuelCell)%Inverter%EffMode == 0) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound = .true.
      ENDIF

      FuelCell(thisFuelCell)%Inverter%ConstEff = NumArray(1)

      FuelCell(thisFuelCell)%Inverter%EffQuadraticCurveID  = GetCurveIndex(AlphArray(3))
      If ((FuelCell(thisFuelCell)%Inverter%EffQuadraticCurveID == 0)  &
           .AND. (FuelCell(thisFuelCell)%Inverter%EffMode == InverterEffQuadratic))  Then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Curve was not found ')
        ErrorsFound = .true.
      ENDIF

          !check for other FuelCell using the same Inverter and fill
      DO otherFuelCell = thisFuelCell+1, NumFuelCellGenerators
        IF (SameString(FuelCell(otherFuelCell)%Inverter%Name, FuelCell(thisFuelCell)%Inverter%Name)) THEN
           FuelCell(otherFuelCell)%Inverter = FuelCell(thisFuelCell)%Inverter
        ENDIF
      ENDDO
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    ENDIF

  ENDDO

  cCurrentModuleObject = 'Generator:FuelCell:StackCooler'
  NumFCStackCoolers = GetNumObjectsFound(cCurrentModuleObject)

  If (NumFCStackCoolers > 0) then  ! get stack cooler input data
    Do FCScoolNum = 1, NumFCStackCoolers
      CALL GetObjectItem(cCurrentModuleObject,FCScoolNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)

      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(AlphArray(1),FuelCell%StackCooler%Name,NumFCStackCoolers-1,IsNotOK,IsBlank,  &
         TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      thisFuelCell = FindItemInList(AlphArray(1),FuelCell%NameStackCooler,NumFuelCellGenerators)
      If (thisFuelCell > 0) then
        FuelCell(thisFuelCell)%StackCooler%Name = AlphArray(1)
        FuelCell(thisFuelCell)%StackCooler%WaterInNodeName = AlphArray(2)

        FuelCell(thisFuelCell)%StackCooler%WaterOutNodeName = AlphArray(3)

        FuelCell(thisFuelCell)%StackCooler%WaterInNode =  &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent )
        FuelCell(thisFuelCell)%StackCooler%WaterOutNode =  &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent )
        CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),  &
                            'Heat Recovery Nodes')

        FuelCell(thisFuelCell)%StackCooler%TstackNom    = NumArray(1)
        FuelCell(thisFuelCell)%StackCooler%TstackActual = NumArray(2)
        FuelCell(thisFuelCell)%StackCooler%r0           = NumArray(3)
        FuelCell(thisFuelCell)%StackCooler%r1           = NumArray(4)
        FuelCell(thisFuelCell)%StackCooler%r2           = NumArray(5)
        FuelCell(thisFuelCell)%StackCooler%r3           = NumArray(6)
        FuelCell(thisFuelCell)%StackCooler%MdotStackCoolant =  NumArray(7)
        FuelCell(thisFuelCell)%StackCooler%UAs_cool     = NumArray(8)
        FuelCell(thisFuelCell)%StackCooler%Fs_cogen     = NumArray(9)
        FuelCell(thisFuelCell)%StackCooler%As_cogen     = NumArray(10)
        FuelCell(thisFuelCell)%StackCooler%MdotCogenNom = NumArray(11)
        FuelCell(thisFuelCell)%StackCooler%hCogenNom    = NumArray(12)
        FuelCell(thisFuelCell)%StackCooler%ns           = NumArray(13)
        FuelCell(thisFuelCell)%StackCooler%PstackPumpEl = NumArray(14)
        FuelCell(thisFuelCell)%StackCooler%PmpPowerLossFactor = NumArray(15)
        FuelCell(thisFuelCell)%StackCooler%f0           = NumArray(16)
        FuelCell(thisFuelCell)%StackCooler%f1           = NumArray(17)
        FuelCell(thisFuelCell)%StackCooler%f1           = NumArray(18)

        FuelCell(thisFuelCell)%StackCooler%StackCoolerPresent = .true.

      ELSE
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(AlphArray(1)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        errorsfound=.true.
      ENDIF
    ENDDO
  ENDIF


  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting input for fuel cell model ')
  ENDIF

  DO GeneratorNum = 1, NumFuelCellGenerators
     CALL SetupOutputVariable('Generator Produced Electric Power [W]', &
          FuelCell(GeneratorNum)%Report%ACPowerGen,'System','Average',FuelCell(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Produced Electric Energy [J]', &
          FuelCell(GeneratorNum)%Report%ACEnergyGen,'System','Sum',FuelCell(GeneratorNum)%Name, &
                           ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')
     CALL SetupOutputVariable('Generator Produced Thermal Rate [W]', &
          FuelCell(GeneratorNum)%Report%qHX, 'System', 'Average', FuelCell(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', &
          FuelCell(GeneratorNum)%Report%HXenergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name , &
          ResourceTypeKey='ENERGYTRANSFER' , EndUseKey='COGENERATION',GroupKey='Plant')

     CALL SetupOutputVariable('Generator Fuel HHV Basis Energy [J]' , &
          FuelCell(GeneratorNum)%Report%FuelEnergyHHV, 'System', 'Sum', FuelCell(GeneratorNum)%Name , &
          ResourceTypeKey='Gas' , EndUseKey='COGENERATION',GroupKey='Plant')
     CALL SetupOutputVariable('Generator Fuel HHV Basis Rate [W]' , &
          FuelCell(GeneratorNum)%Report%FuelEnergyUseRateHHV, 'System', 'Average', FuelCell(GeneratorNum)%Name )

     CALL SetupOutputVariable('Generator Zone Sensible Heat Transfer Rate [W]' , &
          FuelCell(GeneratorNum)%Report%SkinLossPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Zone Sensible Heat Transfer Energy [J]' , &
          FuelCell(GeneratorNum)%Report%SkinLossEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Zone Convection Heat Transfer Rate [W]' , &
          FuelCell(GeneratorNum)%Report%SkinLossConvect, 'System', 'Average', FuelCell(GeneratorNum)%Name )
     CALL SetupOutputVariable('Generator Zone Radiation Heat Transfer Rate [W]' , &
          FuelCell(GeneratorNum)%Report%SkinLossRadiat, 'System', 'Average', FuelCell(GeneratorNum)%Name )

     CALL SetupZoneInternalGain(FuelCell(GeneratorNum)%FCPM%zoneID, &
                     'Generator:FuelCell',  &
                     FuelCell(GeneratorNum)%Name, &
                     IntGainTypeOf_GeneratorFuelCell,    &
                     ConvectionGainRate    = FuelCell(GeneratorNum)%Report%SkinLossConvect, &
                     ThermalRadiationGainRate = FuelCell(GeneratorNum)%Report%SkinLossRadiat)

     IF (DisplayAdvancedReportVariables) THEN ! show extra data originally needed for detailed comparative testing
       CALL SetupOutputVariable('Generator Air Inlet Temperature [C]', &
            FuelCell(GeneratorNum)%Report%TairInlet, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Power Module Entering Air Temperature [C]', &
            FuelCell(GeneratorNum)%Report%TairIntoFCPM, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Air Molar Flow Rate [kmol/s]',  &
            FuelCell(GeneratorNum)%Report%NdotAir, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Power Module Entering Air Enthalpy [W]' , &
            FuelCell(GeneratorNum)%Report%TotAirInEnthalphy, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Blower Electric Power [W]',  &
            FuelCell(GeneratorNum)%Report%BlowerPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Blower Electric Energy [J]',  &
            FuelCell(GeneratorNum)%Report%BlowerEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Blower Skin Heat Loss Rate [W]',  &
            FuelCell(GeneratorNum)%Report%BlowerSkinLoss, 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Fuel Inlet Temperature [C]' , &
            FuelCell(GeneratorNum)%Report%TfuelInlet, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Power Module Entering Fuel Temperature [C]', &
            FuelCell(GeneratorNum)%Report%TfuelIntoFCPM, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Molar Flow Rate [kmol/s]' , &
            FuelCell(GeneratorNum)%Report%NdotFuel, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Consumption LHV Basis Energy [J]' , &
            FuelCell(GeneratorNum)%Report%FuelEnergyLHV, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Consumption Rate LHV Basis [W]' , &
            FuelCell(GeneratorNum)%Report%FuelEnergyUseRateLHV, 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Power Module Entering Fuel Enthalpy [W]',  &
            FuelCell(GeneratorNum)%Report%TotFuelInEnthalpy, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Compressor Electric Power [W]' , &
            FuelCell(GeneratorNum)%Report%FuelCompressPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Compressor Electric Energy [J]',  &
            FuelCell(GeneratorNum)%Report%FuelCompressEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Compressor Skin Heat Loss Rate [W]' , &
            FuelCell(GeneratorNum)%Report%FuelCompressSkinLoss, 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Fuel Reformer Water Inlet Temperature [C]', &
            FuelCell(GeneratorNum)%Report%TwaterInlet, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Power Module Entering Reforming Water Temperature [C]', &
            FuelCell(GeneratorNum)%Report%TwaterIntoFCPM, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Reformer Water Molar Flow Rate [kmol/s]',  &
            FuelCell(GeneratorNum)%Report%NdotWater, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Reformer Water Pump Electric Power [W]' , &
            FuelCell(GeneratorNum)%Report%WaterPumpPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Fuel Reformer Water Pump Electric Energy [J]' , &
            FuelCell(GeneratorNum)%Report%WaterPumpEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Power Module Entering Reforming Water Enthalpy [W]',  &
            FuelCell(GeneratorNum)%Report%WaterIntoFCPMEnthalpy, 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Product Gas Temperature [C]', &
            FuelCell(GeneratorNum)%Report%TprodGas, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas Enthalpy [W]', &
            FuelCell(GeneratorNum)%Report%EnthalProdGas, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas Molar Flow Rate [kmol/s]', &
            FuelCell(GeneratorNum)%Report%NdotProdGas, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas Ar Molar Flow Rate [kmol/s]', &
            FuelCell(GeneratorNum)%Report%NdotProdAr, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas CO2 Molar Flow Rate [kmol/s]', &
            FuelCell(GeneratorNum)%Report%NdotProdCO2, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas H2O Vapor Molar Flow Rate [kmol/s]', &
            FuelCell(GeneratorNum)%Report%NdotProdH2O, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas N2 Molar Flow Rate [kmol/s]', &
            FuelCell(GeneratorNum)%Report%NdotProdN2, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Product Gas O2 Molar Flow Rate [kmol/s]', &
            FuelCell(GeneratorNum)%Report%NdotProdO2, 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Heat Recovery Exit Gas Temperature [C]', &
            FuelCell(GeneratorNum)%Report%THXexh, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Heat Recovery Exit Gas H2O Vapor Fraction [ ]', &
              FuelCell(GeneratorNum)%Report%WaterVaporFractExh, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Heat Recovery Water Condensate Molar Flow Rate [kmol/s]', &
              FuelCell(GeneratorNum)%Report%CondensateRate , 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Inverter Loss Power [W]', &
            FuelCell(GeneratorNum)%Report%PCUlosses, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Produced DC Electric Power [W]', &
            FuelCell(GeneratorNum)%Report%DCPowerGen, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator DC Power Efficiency [ ]', &
            FuelCell(GeneratorNum)%Report%DCPowerEff, 'System', 'Average', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Electric Storage Charge State [J]', &
            FuelCell(GeneratorNum)%Report%ElectEnergyinStorage, 'System', 'Average', FuelCell(GeneratorNum)%Name ) !? 'Sum'
       CALL SetupOutputVariable('Generator DC Storage Charging Power [W]', &
            FuelCell(GeneratorNum)%Report%StoredPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator DC Storage Charging Energy [J]', &
            FuelCell(GeneratorNum)%Report%StoredEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator DC Storage Discharging Power [W]', &
            FuelCell(GeneratorNum)%Report%DrawnPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator DC Storage Discharging Energy [J]', &
            FuelCell(GeneratorNum)%Report%DrawnEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Ancillary AC Electric Power [W]', &
            FuelCell(GeneratorNum)%Report%ACancillariesPower, 'System', 'Average', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Ancillary AC Electric Energy [J]', &
            FuelCell(GeneratorNum)%Report%ACancillariesEnergy, 'System', 'Sum', FuelCell(GeneratorNum)%Name )

       CALL SetupOutputVariable('Generator Fuel Cell Model Iteration Count [ ]' , &
            FuelCell(GeneratorNum)%Report%SeqSubstIterations, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
       CALL SetupOutputVariable('Generator Regula Falsi Iteration Count [ ]', &
            FuelCell(GeneratorNum)%Report%RegulaFalsiIterations, 'System', 'Sum', FuelCell(GeneratorNum)%Name )
     ENDIF
  END DO

myonetimeflag = .false.
ENDIF

RETURN
END SUBROUTINE GetFuelCellGeneratorInput

! End of Get Input subroutines for the FuelCell Generator Module


! Beginning of Generator model Subroutines
! *****************************************************************************

SUBROUTINE CalcFuelCellGeneratorModel(GeneratorNum,RunFlag,MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED     na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a FuelCell generator using the Annex 42 model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:
          ! many subdomains such as fuel and air compressors, wa



          ! REFERENCES: IEA/ECBCS Annex 42....

          ! USE STATEMENTS:
  USE DataHVACGlobals,   ONLY : FirstTimeStepSysFlag,TimeStepSys, SysTimeElapsed
  USE CurveManager,      ONLY : CurveValue
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE DataHeatBalFanSys, ONLY : ZT
  USE DataEnvironment  , ONLY: WaterMainsTemp
  USE General          , ONLY: SolveRegulaFalsi, RoundSigDigits
  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when Generator operating
  REAL(r64)  , INTENT(IN)     :: myload          ! Generator demand
  LOGICAL, INTENT(IN)    :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER   :: KJtoJ = 1000.d0        !convert Kjoules to joules

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) , save     :: PpcuLosses ! losses in inverter [W]
  REAL(r64) :: Pel     ! DC power generated in Fuel Cell Power Module
  REAL(r64) :: Pdemand
  REAL(r64) :: Eel
  REAL(r64) :: Tavg  ! working average temperature
  LOGICAL   :: constrainedFCPM = .false. ! true if power prod is constrained for some reason
  LOGICAL   :: ConstrainedFCPMTrans = .false.
  REAL(r64) :: PelDiff !
  INTEGER   :: iter ! loop index over repeating set of inter dependent calculaitons
  REAL(r64) :: NdotO2 ! molar rate coeff working varible
  REAL(r64) :: CpWater ! heat capacity of water in molar units
  REAL(r64) :: WaterEnthOfForm  ! Standard molar enthalpy of formation
  REAL(r64) :: NdotFuel !fuel flow rate
  REAL(r64) :: NdotStoicAir ! Air to match fuel molar rate coeff, working variable
  REAL(r64) :: NdotExcessAir ! Air in excess of match for fuel
  REAL(r64) :: NdotCO2ProdGas ! CO2 from reaction
  REAL(r64) :: NdotH20ProdGas ! Water from reaction
  REAL(r64) :: NdotCO2 !temp CO2 molar rate coef product gas stream
  REAL(r64) :: NdotN2  !temp Nitrogen rate coef product gas stream
  REAL(r64) :: Ndot02  !temp Oxygen rate coef product gas stream
  REAL(r64) :: NdotH20 !temp Water rate coef product gas stream
  REAL(r64) :: NdotAr  !tmep Argon rate coef product gas stream
  REAL(r64) :: Cp      !temp Heat Capacity, used in thermochemistry units of (J/mol K)
  REAL(r64) :: Hmolfuel   !temp enthalpy of fuel mixture in KJ/mol
  REAL(r64) :: Hmolair    !temp enthalpy of air mixture in KJ/mol
  REAL(r64) :: HmolProdGases !  enthalpy of product gas mixture in KJ/mol
  REAL(r64) :: HLiqWater ! temp enthalpy of liquid water in KJ/mol   No Formation
  REAL(r64) :: HGasWater ! temp enthalpy of gaseous water in KJ/mol  No Formation
  INTEGER   :: thisGas !loop index
  REAL(r64) :: MagofImbalance ! error signal to control exiting loop and targeting product enthalpy
  REAL(r64) :: tmpTotProdGasEnthalphy !
  REAL(r64) :: Acc  ! accuracy control for SolveRegulaFalsi
  INTEGER   :: MaxIter !iteration control for SolveRegulaFalsi
  INTEGER   :: SolverFlag !feed back flag from SolveRegulaFalsi
  REAL(r64), Dimension(3) :: Par  ! parameters passed in to SolveRegulaFalsi
                             ! Par(1) = generator number index in structure
                             ! Par(2) = targeted enthalpy (W)
                             ! Par(3) = molar flow rate of product gases (kmol/s)
  REAL(r64) :: tmpTprodGas
!unused  REAL(r64) :: LHV  !Lower Heating Value
  LOGICAL   :: ConstrainedStorage ! contrained overall elect because of storage
  REAL(r64) :: PgridExtra !extra electric power that should go into storage but can't
  REAL(r64) :: Pstorage ! power into storage (+),  power from storage (-)
  REAL(r64) :: PintoInverter ! power into inverter after storage interactions
  REAL(r64) :: PoutofInverter ! power out of inverter after losses and including storage
  REAL(r64) :: PacAncillariesTotal ! total AC ancillaries

!! begin controls block to be moved out to GeneratorDynamics module
          !If no loop demand or Generator OFF, return
  IF (.NOT. Runflag) THEN

! TODO zero out terms as appropriate

    If (FuelCell(GeneratorNum)%FCPM%HasBeenOn) Then
       !FuelCell just now beginning to shut down,

       ! set Day and Time of Last Shut Down
       FuelCell(GeneratorNum)%FCPM%FractionalDayofLastShutDown = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay
       FuelCell(GeneratorNum)%FCPM%HasBeenOn = .false.

       If (FuelCell(GeneratorNum)%FCPM%ShutDownTime > 0.0d0) FuelCell(GeneratorNum)%FCPM%DuringShutDown = .true.

    endif

    !TODO  check to see if still in shut down mode and using fuel.
    If (FuelCell(GeneratorNum)%FCPM%DuringShutDown) Then


    endif


    RETURN
  END IF

  IF ( .not. FuelCell(GeneratorNum)%FCPM%HasBeenOn ) Then
    !fuel cell just turned on
    ! set Day and Time of Last STart Up

       FuelCell(GeneratorNum)%FCPM%FractionalDayofLastStartUp = REAL(DayOfSim,r64)  &
                                  + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay

       FuelCell(GeneratorNum)%FCPM%HasBeenOn = .true.
       FuelCell(GeneratorNum)%FCPM%NumCycles = FuelCell(GeneratorNum)%FCPM%NumCycles + 1 ! increment cycling counter

       If (FuelCell(GeneratorNum)%FCPM%StartUpTime > 0.0d0) FuelCell(GeneratorNum)%FCPM%DuringStartUp = .true.

  ENDIF

 !TODO deal with things when jump out if not running?
 If ( .not. RunFlag) Then
  Return
 ENDIF

  ! Note: Myload (input) is Pdemand (electical Power requested)
  Pdemand = myLoad
  PacAncillariesTotal = 0.0d0
  PpcuLosses          = 0.0d0
  Pstorage            = 0.0d0
  PgridExtra          = 0.0d0
  PoutofInverter      = 0.0d0
  ConstrainedFCPM     = .false.

 !!BEGIN SEQUENTIAL SUBSTITUTION to handle a lot of inter-related calcs
DO iter=1, 20
     IF (iter > 1) then
          Call FigurePowerConditioningLosses(GeneratorNum, PoutofInverter , PpcuLosses)
          CALL FigureACAncillaries(GeneratorNum, PacAncillariesTotal)
          Pdemand = myLoad + PacAncillariesTotal + PpcuLosses
     ELSE
         ! control Step 1a: Figure ancillary AC power draws
          CALL FigureACAncillaries(GeneratorNum, PacAncillariesTotal)
          Pdemand = myLoad + PacAncillariesTotal
          ! Control Step 1b: Calculate losses associated with Power conditioning
          Call FigurePowerConditioningLosses(GeneratorNum, Pdemand, PpcuLosses)
          Pdemand = Pdemand + PpcuLosses
          Pel = Pdemand
     ENDIF

     FuelCell(GeneratorNum)%Inverter%PCUlosses = PpcuLosses

   ! Control step 2: adjust for transient and startup/shut down constraints

  Call FigureTransientConstraints(GeneratorNum, Pel, ConstrainedFCPMTrans, PelDiff)

    ! Control step 3: adjust for max and min limits on Pel

  IF ( Pel < FuelCell(GeneratorNum)%FCPM%PelMin) THEN
   PelDiff = PelDiff +(FuelCell(GeneratorNum)%FCPM%PelMin - Pel)
   Pel = FuelCell(GeneratorNum)%FCPM%PelMin

   ConstrainedFCPM = .true.
  ENDIF
  IF ( Pel > FuelCell(GeneratorNum)%FCPM%PelMax) THEN
   PelDiff = PelDiff + (FuelCell(GeneratorNum)%FCPM%PelMax - Pel)
   Pel = FuelCell(GeneratorNum)%FCPM%PelMax
   ConstrainedFCPM = .true.

  ENDIF
  If (ConstrainedFCPM) then

  ENDIF

  FuelCell(GeneratorNum)%FCPM%Pel =  Pel
  !Now calculate FC models.  return to controls and batter after


  !Calculation Step 1. Determine electrical Efficiency Eel

  IF (FuelCell(GeneratorNum)%FCPM%EffMode == NormalizedCurveMode) THEN
        !Equation (8) in FuelCell Spec modified for normalized curve

    Eel = CurveValue(FuelCell(GeneratorNum)%FCPM%EffCurveID, Pel/FuelCell(GeneratorNum)%FCPM%NomPel)  &
               *FuelCell(GeneratorNum)%FCPM%NomEff &
         * (1.0d0 - FuelCell(GeneratorNum)%FCPM%NumCycles * FuelCell(GeneratorNum)%FCPM%CyclingDegradRat) &
         * (1.0d0 - MAX((FuelCell(GeneratorNum)%FCPM%NumRunHours-FuelCell(GeneratorNum)%FCPM%ThreshRunHours), 0.0d0) &
                   *FuelCell(GeneratorNum)%FCPM%OperateDegradRat)

  ELSEIF  (FuelCell(GeneratorNum)%FCPM%EffMode == DirectCurveMode ) THEN
   !Equation (8) in FuelCell Spec
    Eel = CurveValue(FuelCell(GeneratorNum)%FCPM%EffCurveID, Pel) &
         * (1.0d0 - FuelCell(GeneratorNum)%FCPM%NumCycles * FuelCell(GeneratorNum)%FCPM%CyclingDegradRat) &
         * (1.0d0 - MAX((FuelCell(GeneratorNum)%FCPM%NumRunHours-FuelCell(GeneratorNum)%FCPM%ThreshRunHours), 0.0d0) &
                   *FuelCell(GeneratorNum)%FCPM%OperateDegradRat )
  ENDIF

  FuelCell(GeneratorNum)%FCPM%Eel = Eel
  ! Calculation Step 2. Determine fuel rate

  NdotFuel = Pel / (Eel * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%LHV * 1000000.0d0) !Eq. 10 solved for Ndot

  FuelCell(GeneratorNum)%FCPM%NdotFuel = NdotFuel
  If (Pel <= 0.0d0) Then
    !TODO zero stuff before leaving
    Pel = 0.0d0
    FuelCell(GeneratorNum)%FCPM%Pel = 0.0d0
    RETURN
  Else

  FuelCell(GeneratorNum)%FCPM%Pel =  Pel
  endif




  ! Calculation Step 3. Determine Air rate

  IF     (FuelCell(GeneratorNum)%AirSup%AirSupRateMode == ConstantStoicsAirRat) THEN !MEthod 1
     NdotO2 = FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%StoicOxygenRate    &
              * FuelCell(GeneratorNum)%FCPM%NdotFuel            &
              * FuelCell(GeneratorNum)%AirSup%Stoics

     FuelCell(GeneratorNum)%FCPM%NdotAir = NdotO2 / FuelCell(GeneratorNum)%AirSup%O2fraction


  ELSEIF (FuelCell(GeneratorNum)%AirSup%AirSupRateMode == QuadraticFuncofPel) THEN !MEthod 2

     FuelCell(GeneratorNum)%FCPM%NdotAir = CurveValue(FuelCell(GeneratorNum)%AirSup%AirFuncPelCurveID, Pel) &
                                       * (1 + FuelCell(GeneratorNum)%AirSup%AirTempCoeff &
                                              * FuelCell(GeneratorNum)%AirSup%TairIntoFCPM)


  ELSEIF (FuelCell(GeneratorNum)%AirSup%AirSupRateMode == QuadraticFuncofNdot)  THEN ! method 3
     FuelCell(GeneratorNum)%FCPM%NdotAir = CurveValue(FuelCell(GeneratorNum)%AirSup%AirFuncNdotCurveID, &
                                                              FuelCell(GeneratorNum)%FCPM%NdotFuel) &
                                       * (1 + FuelCell(GeneratorNum)%AirSup%AirTempCoeff &
                                              * FuelCell(GeneratorNum)%AirSup%TairIntoFCPM)

  ENDIF


  ! Calculation Step 4. fuel compressor power

  FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%PfuelCompEl    &
     = CurveValue( FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%CompPowerCurveID, FuelCell(GeneratorNum)%FCPM%NdotFuel)

  ! calculation Step 5, Fuel Compressor (need outlet temperature)

  If (FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%FuelTempMode == FuelInTempFromNode) then

     FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoCompress  &
            = Node(FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%NodeNum)%Temp

  ELSEIF (FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%FuelTempMode == FuelInTempSchedule) then

     FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoCompress   &
           = GetCurrentScheduleValue(FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%SchedNum)

  ENDIF

       !  evaluate  heat capacity at average temperature usign shomate
  Tavg  = (FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoCompress   &
     + FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoFCPM)/ 2.0d0
  Call FigureFuelHeatCap(GeneratorNum, Tavg, Cp)  ! Cp in (J/mol K)

    ! calculate a Temp of fuel out of compressor and into power module

  IF (FuelCell(GeneratorNum)%FCPM%NdotFuel <= 0.0d0) THEN  !just pass through, domain probably collapased in modeling
     FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoFCPM =  FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoCompress
  ELSE
     FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoFCPM = (  &
                                             (1.0d0 - FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%CompPowerLossFactor)  &
                                                 *FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%PfuelCompEl              &
                                           / (FuelCell(GeneratorNum)%FCPM%NdotFuel * Cp * 1000.0d0)) & !1000 Cp units mol-> kmol
                                                + FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoCompress
  ENDIF
    ! calc skin losses from fuel compressor
  FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%QskinLoss     = FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%CompPowerLossFactor &
                                               *FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%PfuelCompEl

  If (FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%QskinLoss < 0.0d0) then
!   write(*,*) 'problem in FuelSupply%QskinLoss ', FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%QskinLoss
   CALL ShowWarningError('problem in FuelSupply%QskinLoss '//  &
                   TRIM(RoundSigDigits(FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%QskinLoss,3)))
   FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%QskinLoss = 0.0d0
  endif

    ! calculate tatal fuel enthalpy coming into power module

  ! (Hmolfuel in KJ/mol)
  CAll FigureFuelEnthalpy(GeneratorNum, FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%TfuelIntoFCPM, Hmolfuel)

  ! units, NdotFuel in kmol/sec. Hmolfule in KJ/mol ,
  !        factor of 1000's to get to J/s or watts
  FuelCell(GeneratorNum)%FCPM%TotFuelInEnthalphy  = Hmolfuel * 1000.0d0 * FuelCell(GeneratorNum)%FCPM%NdotFuel * 1000.0d0

  !Calculation Step 6, water compressor calculations

   ! calculate water consumption

  FuelCell(GeneratorNum)%FCPM%NdotLiqwater = CurveValue(FuelCell(GeneratorNum)%WaterSup%WaterSupRateCurveID , &
                                                    FuelCell(GeneratorNum)%FCPM%NdotFuel )

   ! set inlet temp.  (could move to init)

  SELECT CASE (FuelCell(GeneratorNum)%WaterSup%WaterTempMode)

  CASE (WaterInReformMains)

    FuelCell(GeneratorNum)%WaterSup%TwaterIntoCompress = WaterMainsTemp

  CASE (WaterInReformAirNode,WaterInReformWaterNode)

    FuelCell(GeneratorNum)%WaterSup%TwaterIntoCompress = Node(FuelCell(GeneratorNum)%WaterSup%NodeNum)%Temp

  CASE (WaterInReformSchedule)

    FuelCell(GeneratorNum)%WaterSup%TwaterIntoCompress = GetCurrentScheduleValue(FuelCell(GeneratorNum)%WaterSup%SchedNum)

  END SELECT


  FuelCell(GeneratorNum)%WaterSup%PwaterCompEl  = CurveValue(FuelCell(GeneratorNum)%WaterSup%PmpPowerCurveID, &
                                                          FuelCell(GeneratorNum)%FCPM%NdotLiqwater )

     ! 75.325  J/mol K Water at 0.1 MPa and 298 K, reference NIST WEBBOOK
  call FigureLiquidWaterHeatCap(FuelCell(GeneratorNum)%WaterSup%TwaterIntoCompress, CpWater)

  WaterEnthOfForm =   -241.8264d0 !KJ/mol

  IF (FuelCell(GeneratorNum)%FCPM%NdotLiqwater <= 0.0d0) THEN  !just pass through, domain probably collapased in modeling
   FuelCell(GeneratorNum)%WaterSup%TwaterIntoFCPM = FuelCell(GeneratorNum)%WaterSup%TwaterIntoCompress !
  ELSE

    FuelCell(GeneratorNum)%WaterSup%TwaterIntoFCPM = ((1 - FuelCell(GeneratorNum)%WaterSup%PmpPowerLossFactor)  &
                                               *FuelCell(GeneratorNum)%WaterSup%PwaterCompEl              &
                                               / (FuelCell(GeneratorNum)%FCPM%NdotLiqwater * CpWater * 1000.0d0)) &
                                               + FuelCell(GeneratorNum)%WaterSup%TwaterIntoCompress
  ENDIF

  FuelCell(GeneratorNum)%WaterSup%QskinLoss = FuelCell(GeneratorNum)%WaterSup%PmpPowerLossFactor   &
                                          * FuelCell(GeneratorNum)%WaterSup%PwaterCompEl

  If (FuelCell(GeneratorNum)%WaterSup%QskinLoss < 0.0d0) then
  ! write(*,*) 'problem in WaterSup%QskinLoss ',FuelCell(GeneratorNum)%WaterSup%QskinLoss
   FuelCell(GeneratorNum)%WaterSup%QskinLoss = 0.0d0
  endif

  Call FigureLiquidWaterEnthalpy(FuelCell(GeneratorNum)%WaterSup%TwaterIntoFCPM, HLiqWater)   ! HLiqWater in KJ/mol

  FuelCell(GeneratorNum)%FCPM%WaterInEnthalpy = FuelCell(GeneratorNum)%FCPM%NdotLiqwater*HLiqWater*1000.0d0*1000.0d0

  !Calculation Step 7, Air compressor

  FuelCell(GeneratorNum)%AirSup%TairIntoBlower = Node(FuelCell(GeneratorNum)%AirSup%SupNodeNum)%Temp

  FuelCell(GeneratorNum)%AirSup%PairCompEl  = CurveValue(FuelCell(GeneratorNum)%AirSup%BlowerPowerCurveID, &
                                                       FuelCell(GeneratorNum)%FCPM%NdotAir)

  Tavg = ( FuelCell(GeneratorNum)%AirSup%TairIntoBlower + FuelCell(GeneratorNum)%AirSup%TairIntoFCPM ) / 2.0d0

  CALL FigureAirHeatCap(GeneratorNum, Tavg, Cp)  ! Cp in (J/mol K)

  ! if PEMFC with stack cooler, then calculate stack cooler impacts
  If (FuelCell(GeneratorNum)%StackCooler%StackCoolerPresent) then

    FuelCell(GeneratorNum)%StackCooler%qs_cool =    ( FuelCell(GeneratorNum)%StackCooler%r0                       &
                                                  + FuelCell(GeneratorNum)%StackCooler%r1                         &
                                                  *(FuelCell(GeneratorNum)%StackCooler%TstackActual               &
                                                      - FuelCell(GeneratorNum)%StackCooler%TstackNom) )           &
                                                  *( 1 + FuelCell(GeneratorNum)%StackCooler%r2*Pel                &
                                                       + FuelCell(GeneratorNum)%StackCooler%r3*Pel*Pel) * Pel

    FuelCell(GeneratorNum)%FCPM%QdotStackCool = FuelCell(GeneratorNum)%StackCooler%qs_cool

  ENDIF


  !Figure heat recovery from Electrical Storage, power conditioning, and auxiliary burner

  SELECT CASE (FuelCell(GeneratorNum)%AirSup%IntakeRecoveryMode)

  CASE(RecoverBurnInvertBatt)
    FuelCell(GeneratorNum)%AirSup%QintakeRecovery =   FuelCell(GeneratorNum)%AuxilHeat%QairIntake    &
                                               + FuelCell(GeneratorNum)%ElecStorage%QairIntake   &
                                               + FuelCell(GeneratorNum)%Inverter%QairIntake
  CASE(RecoverAuxiliaryBurner)
    FuelCell(GeneratorNum)%AirSup%QintakeRecovery =   FuelCell(GeneratorNum)%AuxilHeat%QairIntake
  CASE(RecoverInverterBatt)
    FuelCell(GeneratorNum)%AirSup%QintakeRecovery =  FuelCell(GeneratorNum)%ElecStorage%QairIntake   &
                                               + FuelCell(GeneratorNum)%Inverter%QairIntake
  CASE(RecoverInverter)
    FuelCell(GeneratorNum)%AirSup%QintakeRecovery = FuelCell(GeneratorNum)%Inverter%QairIntake
  CASE(RecoverBattery)
    FuelCell(GeneratorNum)%AirSup%QintakeRecovery = FuelCell(GeneratorNum)%ElecStorage%QairIntake
  CASE(NoRecoveryOnAirIntake)
    FuelCell(GeneratorNum)%AirSup%QintakeRecovery =  0.0d0

  END SELECT

  IF ( FuelCell(GeneratorNum)%FCPM%NdotAir <= 0.0d0 ) THEN !just pass through, domain probably collapased in modeling
     FuelCell(GeneratorNum)%AirSup%TairIntoFCPM  = FuelCell(GeneratorNum)%AirSup%TairIntoBlower

  ELSE
     FuelCell(GeneratorNum)%AirSup%TairIntoFCPM = (((1 - FuelCell(GeneratorNum)%AirSup%BlowerHeatLossFactor) &
                                               * FuelCell(GeneratorNum)%AirSup%PairCompEl                &
                                               + FuelCell(GeneratorNum)%AirSup%QintakeRecovery)          &
                                             / (FuelCell(GeneratorNum)%FCPM%NdotAir * Cp * 1000.0d0))   & !1000 Cp units mol-> kmol
                                               + FuelCell(GeneratorNum)%AirSup%TairIntoBlower
  ENDIF

  FuelCell(GeneratorNum)%AirSup%QskinLoss = FuelCell(GeneratorNum)%AirSup%BlowerHeatLossFactor &
                                         * FuelCell(GeneratorNum)%AirSup%PairCompEl

  If (FuelCell(GeneratorNum)%AirSup%QskinLoss < 0.0d0) then
!   write(*,*) 'problem in AirSup%QskinLoss ', FuelCell(GeneratorNum)%AirSup%QskinLoss
   CALL ShowWarningError('problem in AirSup%QskinLoss '//TRIM(RoundSigDigits(FuelCell(GeneratorNum)%AirSup%QskinLoss,3)))
   FuelCell(GeneratorNum)%AirSup%QskinLoss = 0.0d0
  endif

  CAll FigureAirEnthalpy(GeneratorNum, FuelCell(GeneratorNum)%AirSup%TairIntoFCPM, Hmolair) ! (Hmolair in KJ/mol)

  ! units, NdotAir in kmol/sec.; Hmolfuel in KJ/mol ,
  !        factor of 1000's to get to J/s or watts
  FuelCell(GeneratorNum)%FCPM%TotAirInEnthalphy  = Hmolair * 1000.0d0 * FuelCell(GeneratorNum)%FCPM%NdotAir * 1000.0d0

  ! calculation Step 8, Figure Product Gases

     ! figure stoic N dot for air
  NdotO2 = FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%StoicOxygenRate    &
              * FuelCell(GeneratorNum)%FCPM%NdotFuel

  NdotStoicAir = NdotO2 / FuelCell(GeneratorNum)%AirSup%O2fraction

     ! figure excess air rate

  NdotExcessAir = FuelCell(GeneratorNum)%FCPM%NdotAir - NdotStoicAir

  IF (NdotExcessAir < 0) THEN !can't meet stoichiometric fuel reaction

      CALL ShowWarningError('Air flow rate into fuel cell is too low for stoichiometric fuel reaction')
      CALL ShowContinueError('Increase air flow in GENERATOR:FC:AIR SUPPLY object:' &
                               //Trim(FuelCell(GeneratorNum)%AirSup%Name))
  ENDIF

     ! figure CO2 and Water rate from products (coefs setup during one-time processing in gas phase library )

  NdotCO2ProdGas = FuelCell(GeneratorNum)%FCPM%NdotFuel * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%CO2ProductGasCoef

  NdotH20ProdGas = FuelCell(GeneratorNum)%FCPM%NdotFuel * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%H20ProductGasCoef

     !  set product gas constituent fractions  (assume five usual components)
  NdotCO2 = 0.0d0
  NdotN2  = 0.0d0
  Ndot02  = 0.0d0
  NdotH20 = 0.0d0
  NdotAr  = 0.0d0

! Product gas constiuents are fixed (not a user defined thing)
!

  Do thisGas = 1, FuelCell(GeneratorNum)%AirSup%NumConstituents

      SELECT CASE (FuelCell(GeneratorNum)%AirSup%GasLibID(thisGas))

      CASE (1)
        ! all the CO2 coming in plus the new CO2 from reactions
        NdotCO2 = NdotCO2ProdGas + FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisGas) * FuelCell(GeneratorNum)%FCPM%NdotAir

      CASE (2)
        ! all the nitrogen comming in
        NdotN2 = FuelCell(GeneratorNum)%FCPM%NdotAir * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisGas)

      CASE (3)
        ! all the oxygen in the excess air stream
        Ndot02 = NdotExcessAir * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisGas)

      CASE (4)
        ! all the H20 comming in plus the new H20 from reactions and the H20 from water used in reforming
        NdotH20 = NdotH20ProdGas + FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisGas) * FuelCell(GeneratorNum)%FCPM%NdotAir
                   !+ FuelCell(GeneratorNum)%FCPM%NdotLiqwater

      CASE (5)
        ! all the argon coming in.
        NdotAr = FuelCell(GeneratorNum)%FCPM%NdotAir * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisGas)

      CASE DEFAULT

      END SELECT
  ENDDO

  FuelCell(GeneratorNum)%FCPM%NdotProdGas = NdotCO2 + NdotN2 + Ndot02 + NdotH20 + NdotAr

    ! now that we have the total, figure molar fractions


  FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(1) =  NdotCO2 / FuelCell(GeneratorNum)%FCPM%NdotProdGas

     ! all the nitrogen comming in
  FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(2) =  NdotN2 / FuelCell(GeneratorNum)%FCPM%NdotProdGas


        ! all the oxygen in the excess air stream
  FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(3) =  Ndot02 / FuelCell(GeneratorNum)%FCPM%NdotProdGas

       ! all the H20 comming in plus the new H20 from reactions and the H20 from water used in reforming
  FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(4) = NdotH20 / FuelCell(GeneratorNum)%FCPM%NdotProdGas

        ! all the argon coming in.
  FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(5) = NdotAr / FuelCell(GeneratorNum)%FCPM%NdotProdGas

  !HmolProdGases KJ/mol)
  CALL FigureProductGasesEnthalpy(GeneratorNum, FuelCell(GeneratorNum)%FCPM%TprodGasLeavingFCPM, HmolProdGases)

  ! units, NdotProdGas in kmol/sec.; HmolProdGases in KJ/mol ,
  !        factor of 1000's to get to J/s or watts
  FuelCell(GeneratorNum)%FCPM%TotProdGasEnthalphy  = HmolProdGases * 1000.0d0 * FuelCell(GeneratorNum)%FCPM%NdotProdGas * 1000.0d0


  ! calculation Step 9, Figure Skin lossess

  IF ( FuelCell(GeneratorNum)%FCPM%SkinLossMode == ConstantRateSkinLoss ) THEN
   ! do nothing just use QdotSkin

  ELSEIF ( FuelCell(GeneratorNum)%FCPM%SkinLossMode == UADTSkinLoss ) then

    ! get zone air temp
    FuelCell(GeneratorNum)%FCPM%QdotSkin =   FuelCell(GeneratorNum)%FCPM%UAskin * &
                                       ( FuelCell(GeneratorNum)%FCPM%TprodGasLeavingFCPM  &
                                          - ZT(FuelCell(GeneratorNum)%FCPM%ZoneID ))

  ELSEIF ( FuelCell(GeneratorNum)%FCPM%SkinLossMode == QuadraticFuelNdotSkin) then

    FuelCell(GeneratorNum)%FCPM%QdotSkin = CurveValue( FuelCell(GeneratorNum)%FCPM%SkinLossCurveID, &
                                                   FuelCell(GeneratorNum)%FCPM%NdotFuel )

  ENDIF

  ! calculation Step 10, AC FCPM power ancillaries

  FuelCell(GeneratorNum)%FCPM%PelancillariesAC = FuelCell(GeneratorNum)%FCPM%ANC0   &
                                             + FuelCell(GeneratorNum)%FCPM%ANC1 * FuelCell(GeneratorNum)%FCPM%NdotFuel

  ! calculation Step 11, Dilution air
  CAll FigureAirEnthalpy(GeneratorNum, FuelCell(GeneratorNum)%AirSup%TairIntoBlower, Hmolair) ! (Hmolair in KJ/mol)

  ! units, NdotDilutionAir in kmol/sec.; Hmolair in KJ/mol ,
  !        factor of 1000's to get to J/s or watts
  FuelCell(GeneratorNum)%FCPM%DilutionAirInEnthalpy  = Hmolair * 1000.0d0 * FuelCell(GeneratorNum)%FCPM%NdotDilutionAir * 1000.0d0
  FuelCell(GeneratorNum)%FCPM%DilutionAirOutEnthalpy = FuelCell(GeneratorNum)%FCPM%DilutionAirInEnthalpy   &
                                                   + FuelCell(GeneratorNum)%FCPM%StackHeatLossToDilution

  ! calculation Step 12, Calculate Reforming water out enthalpy
  Call FigureGaseousWaterEnthalpy(FuelCell(GeneratorNum)%FCPM%TprodGasLeavingFCPM, HGasWater)

  FuelCell(GeneratorNum)%FCPM%WaterOutEnthalpy = HGasWater * 1000.0d0 * FuelCell(GeneratorNum)%FCPM%NdotLiqwater * 1000.0d0

  ! calculation Step 13, Calculate Heat balance
  !    move all terms in Equation 7 to RHS and calculate imbalance

   MagofImbalance = - FuelCell(GeneratorNum)%FCPM%TotFuelInEnthalphy  &
                    - FuelCell(GeneratorNum)%FCPM%TotAirInEnthalphy   &
                    - FuelCell(GeneratorNum)%FCPM%WaterInEnthalpy     &
                    - FuelCell(GeneratorNum)%FCPM%DilutionAirInEnthalpy &
                    - FuelCell(GeneratorNum)%FCPM%NdotFuel * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%LHV * 1000000.0d0 &
                    - FuelCell(GeneratorNum)%FCPM%PelancillariesAC    &
                    + FuelCell(GeneratorNum)%FCPM%Pel                 &
                    + FuelCell(GeneratorNum)%FCPM%TotProdGasEnthalphy &
                    + FuelCell(GeneratorNum)%FCPM%WaterOutEnthalpy    &
                    + FuelCell(GeneratorNum)%FCPM%QdotStackCool       &
                    + FuelCell(GeneratorNum)%FCPM%QdotSkin            &
                    + FuelCell(GeneratorNum)%FCPM%DilutionAirOutEnthalpy



  ! Now find a new total prod Gas Enthalphy that would result in an energy balance
  ! TODO check signs...
  tmpTotProdGasEnthalphy = FuelCell(GeneratorNum)%FCPM%TotProdGasEnthalphy - MagofImbalance

  ! solve for a new TprodGasLeavingFCPM using regula falsi method

  Acc = 0.01d0  ! guessing need to refine
  MaxIter = 150 ! guessing need to refine
  SolverFlag = 0 !init
  Par(1) = real(GeneratorNum,r64)
  Par(2) = tmpTotProdGasEnthalphy
  Par(3) = FuelCell(GeneratorNum)%FCPM%NdotProdGas
  tmpTprodGas = FuelCell(GeneratorNum)%FCPM%TprodGasLeavingFCPM
  CALL SolveRegulaFalsi(Acc, MaxIter, SolverFlag, tmpTprodGas, FuelCellProductGasEnthResidual,   &
     MinProductGasTemp,MaxProductGasTemp, Par)

  IF (SolverFlag == -2) THEN

    CALL ShowWarningError('CalcFuelCellGeneratorModel: Regula falsi problem, flag = -2, check signs, all positive')

  ENDIF
  IF (SolverFlag == -1) THEN
    CALL ShowWarningError('CalcFuelCellGeneratorModel: '//  &
       'Regula falsi problem, flag = -1, check accuracy and iterations, did not converge')

  ENDIF
  IF (solverFlag > 0) THEN
    FuelCell(GeneratorNum)%FCPM%TprodGasLeavingFCPM  =  tmpTprodGas
  !  write(*,*) 'Number of regula falsi iterations: ', solverFlag
  ENDIF


  !  moved call to HeatBalanceInternalGains.   Call FigureFuelCellZoneGains(GeneratorNum)

  ! Control Step 3 determine interaction with electrical storage
  ! How much power is really going into inverter?
  PintoInverter = Pel + Pstorage    ! Back out so we can reapply
  CALL ManageElectStorInteractions(GeneratorNum, Pdemand, PpcuLosses, &
                                     ConstrainedStorage, Pstorage, PgridExtra)
  PintoInverter = Pel - Pstorage    !
  ! refine power conditioning losses with more current power production

   IF (FuelCell(GeneratorNum)%Inverter%EffMode ==  InverterEffConstant) THEN

     PpcuLosses = (1.0d0 - FuelCell(GeneratorNum)%Inverter%ConstEff )* PintoInverter

   ENDIF

   IF (FuelCell(GeneratorNum)%Inverter%EffMode ==  InverterEffQuadratic) THEN

     PpcuLosses = (1.0d0 - CurveValue(FuelCell(GeneratorNum)%Inverter%EffQuadraticCurveID, PintoInverter))* PintoInverter

   ENDIF

   PoutofInverter = PintoInverter - PpcuLosses

   FuelCell(GeneratorNum)%ACPowerGen = PoutofInverter - FuelCell(GeneratorNum)%FCPM%PelancillariesAC &
                                                  - FuelCell(GeneratorNum)%AirSup%PairCompEl     &
                                                  - FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%PfuelCompEl   &
                                                  - FuelCell(GeneratorNum)%WaterSup%PwaterCompEl
   FuelCell(GeneratorNum)%Inverter%PCUlosses = PpcuLosses
   ! model assumes air intake is drawn over power conditioner to recovery heat
   FuelCell(GeneratorNum)%Inverter%QairIntake = FuelCell(GeneratorNum)%Inverter%PCUlosses

   Call CalcFuelCellAuxHeater(GeneratorNum)

   Call CalcFuelCellGenHeatRecovery(GeneratorNum)
 ! calculation Step 11, If imbalance below threshold, then exit out of do loop.

  IF ( (ABS(MagofImbalance) < ABS(ImBalanceTol * FuelCell(GeneratorNum)%FCPM%Pel)) &
      .AND. (Iter > 2) )   THEN
    EXIT
  ENDIF


ENDDO !sequential substitution loop

  FuelCell(GeneratorNum)%FCPM%SeqSubstitIter  = iter
  FuelCell(GeneratorNum)%FCPM%RegulaFalsiIter = solverFlag

RETURN
END SUBROUTINE CalcFuelCellGeneratorModel

SUBROUTINE ManageElectStorInteractions(Num,Pdemand, PpcuLosses, &
                             Constrained, Pstorage, PgridOverage)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! manage controls and calculations related to electrical storage in FuelCell model

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Num ! Generator number, index for structure
  REAL(r64)   , INTENT(IN)  :: Pdemand !
  REAL(r64)   , INTENT(IN)  :: PpcuLosses
  Logical, INTENT(OUT) :: Constrained
  REAL(r64)   , INTENT(OUT) :: Pstorage
  REAL(r64)   , INTENT(OUT) :: PgridOverage  !electricity that can't be stored and needs to go out

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: tmpPdraw ! power draw from storage, working var
  REAL(r64)    :: tmpPcharge ! power charge to storage, working var
  Logical :: drawing ! true if drawing power
  Logical :: charging ! true if charging


   !initialize locals
  tmpPdraw   = 0.0d0
  tmpPcharge = 0.0d0
  drawing  = .false.
  charging = .false.
  Constrained = .false.
  Pstorage = 0.0d0
  PgridOverage = 0.0d0

  ! step 1 figure out what is desired of electrical storage system

  If (FuelCell(Num)%FCPM%Pel < (Pdemand )) THEN
    !draw from storage
    tmpPdraw = (Pdemand ) - FuelCell(Num)%FCPM%Pel
    drawing = .true.
  ENDIF

  IF (FuelCell(Num)%FCPM%Pel > (Pdemand )) THEN
    !add to storage
    tmpPcharge = FuelCell(Num)%FCPM%Pel - (Pdemand )
    charging = .true.

  ENDIF

  !  step 2, figure out what is possible for electrical storage draws/charges

  If (charging) then

    IF (FuelCell(Num)%ElecStorage%StorageModelMode == SimpleEffConstraints) THEN

      IF (FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge >= FuelCell(Num)%ElecStorage%NominalEnergyCapacity) THEN
          ! storage full!  no more allowed!
          PgridOverage = tmpPcharge
          tmpPcharge = 0.0d0
          Constrained = .true.
      ENDIF
      IF (tmpPcharge > FuelCell(Num)%ElecStorage%MaxPowerStore) THEN
          PgridOverage = tmpPcharge - FuelCell(Num)%ElecStorage%MaxPowerStore
          tmpPcharge = FuelCell(Num)%ElecStorage%MaxPowerStore
          Constrained = .true.
      ENDIF


      !now add energy to storage from charging
     IF ((FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge &
         + tmpPcharge *TimeStepSys*SecInHour*FuelCell(Num)%ElecStorage%EnergeticEfficCharge) &
          < FuelCell(Num)%ElecStorage%NominalEnergyCapacity) THEN

         FuelCell(Num)%ElecStorage%ThisTimeStepStateOfCharge =  FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge &
                               + tmpPcharge *TimeStepSys*SecInHour*FuelCell(Num)%ElecStorage%EnergeticEfficCharge
     ELSE ! would over charge this time step

        tmpPcharge = (FuelCell(Num)%ElecStorage%NominalEnergyCapacity - FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge) &
                     /(TimeStepSys*SecInHour*FuelCell(Num)%ElecStorage%EnergeticEfficCharge)
        Constrained = .true.
        FuelCell(Num)%ElecStorage%ThisTimeStepStateOfCharge =  FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge &
                               + tmpPcharge *TimeStepSys*SecInHour*FuelCell(Num)%ElecStorage%EnergeticEfficCharge
     ENDIF

        !losses go into QairIntake
        FuelCell(Num)%ElecStorage%QairIntake  = tmpPcharge * (1.0d0 - FuelCell(Num)%ElecStorage%EnergeticEfficCharge)


    ELSEIF (FuelCell(Num)%ElecStorage%StorageModelMode == LeadAcidBatterManwellMcGowan) THEN
      CALL ShowWarningError('ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ')

    ELSEIF (FuelCell(Num)%ElecStorage%StorageModelMode == LeadAcidBatterySaupe) THEN
      CALL ShowWarningError('ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ')

    ELSE

      !should not come here
    ENDIF

    Pstorage = tmpPcharge

  ENDIF  !charging

  IF (drawing) THEN
    IF (FuelCell(Num)%ElecStorage%StorageModelMode == SimpleEffConstraints) THEN

      IF (FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge <= 0.0d0) THEN
          ! storage empty  no more allowed!
          tmpPdraw = 0.0d0
          Constrained = .true.
          drawing = .false.
      ENDIF
      IF (tmpPdraw > FuelCell(Num)%ElecStorage%MaxPowerDraw) THEN
          tmpPdraw = FuelCell(Num)%ElecStorage%MaxPowerDraw
          Constrained = .true.
      ENDIF

            !now take energy from storage by drawing  (amplified by energetic effic)
      IF ((FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge  &
          - tmpPdraw *TimeStepSys*SecInHour/FuelCell(Num)%ElecStorage%EnergeticEfficDischarge) > 0.0d0 ) Then

        FuelCell(Num)%ElecStorage%ThisTimeStepStateOfCharge =  FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge &
                                - tmpPdraw *TimeStepSys*SecInHour/FuelCell(Num)%ElecStorage%EnergeticEfficDischarge
      ELSE !would over drain storage this timestep so reduce tmpPdraw
        tmpPdraw = FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge * FuelCell(Num)%ElecStorage%EnergeticEfficDischarge &
                   /(TimeStepSys*SecInHour)
        FuelCell(Num)%ElecStorage%ThisTimeStepStateOfCharge =  FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge &
                                - tmpPdraw *TimeStepSys*SecInHour/FuelCell(Num)%ElecStorage%EnergeticEfficDischarge

        Constrained = .true.
      ENDIF
        !losses go into QairIntake
      FuelCell(Num)%ElecStorage%QairIntake  = tmpPdraw * (1.0d0/FuelCell(Num)%ElecStorage%EnergeticEfficDischarge - 1.0d0)
    ELSEIF (FuelCell(Num)%ElecStorage%StorageModelMode == LeadAcidBatterManwellMcGowan) THEN
      CALL ShowWarningError('ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 ')

    ELSEIF (FuelCell(Num)%ElecStorage%StorageModelMode == LeadAcidBatterySaupe) THEN
      CALL ShowWarningError('ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 ')

    ELSE

      !should not come here
    ENDIF

    Pstorage = - tmpPdraw

  ENDIF !drawing

  IF ((.not. charging) .and. ( .not. drawing)) THEN

     FuelCell(Num)%ElecStorage%ThisTimeStepStateOfCharge = FuelCell(Num)%ElecStorage%LastTimeStepStateOfCharge
     FuelCell(Num)%ElecStorage%PelNeedFromStorage = 0.0d0
     FuelCell(Num)%ElecStorage%PelFromStorage = 0.0d0
     FuelCell(Num)%ElecStorage%QairIntake  = 0.0d0
  ENDIF

  IF (Pstorage >= 0.0d0) THEN

    FuelCell(Num)%ElecStorage%PelIntoStorage = Pstorage
    FuelCell(Num)%ElecStorage%PelFromStorage = 0.0d0
  ENDIF
  IF (Pstorage < 0.0d0) THEN

    FuelCell(Num)%ElecStorage%PelIntoStorage = 0.0d0
    FuelCell(Num)%ElecStorage%PelFromStorage = - Pstorage

  ENDIF


  RETURN

END SUBROUTINE ManageElectStorInteractions


FUNCTION FuelCellProductGasEnthResidual(TprodGas, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith NREL
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provide function for call to regula falsi search
          ! Search for an product gas stream temperature that provides a
          ! certain enthaply. (enthalpy is based on Shomate and can't be inverted)


          ! METHODOLOGY EMPLOYED:
          ! Calculates residual function for product gas enthalpy
          ! calls procedure FigureProductGasesEnthalpy

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: TprodGas ! temperature, this is "x" being searched
  REAL(r64), INTENT(IN), Dimension(:), optional :: Par ! par(1) = Generator Number
                                        ! par(2) = Desired Enthalpy
  REAL(r64)                      :: Residuum ! F(x)
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  Integer  :: GeneratorNum
  REAL(r64)     :: thisHmolalProdGases
  REAL(r64)     :: desiredHprodGases
  REAL(r64)     :: NdotProdGases

  GeneratorNum      = floor(PAR(1))
  desiredHprodGases = PAR(2)
  NdotProdGases     = PAR(3)

  Call FigureProductGasesEnthalpy(GeneratorNum, TprodGas, thisHmolalProdGases)

  Residuum = (thisHmolalProdGases * NdotProdGases * 1000000.0d0) - desiredHprodGases

  RETURN

END FUNCTION FuelCellProductGasEnthResidual

SUBROUTINE FigureAirHeatCap(GeneratorNum, FluidTemp, Cp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Cp from Shomate equations for fuel

          ! METHODOLOGY EMPLOYED:
          ! sum by weighting molar fractions of all Air constituents.
          ! assumes mixture is sum of parts.

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Cp           !  (J/mol*K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempCp
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff


  ! loop through fuel constituents and sum up Cp

  ! two different themodynamic curve fits might be used

  tempCp = 0.0d0
  Tkel = (FluidTemp +KelvinConv)
  Tsho = (FluidTemp +KelvinConv) / 1000.0d0

  DO thisConstit=1, FuelCell(GeneratorNum)%AirSup%NumConstituents
    gasID = FuelCell(GeneratorNum)%AirSup%GasLibID(thisConstit)
    IF (gasID > 0) THEN
     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) THEN

       A = GasPhaseThermoChemistryData(gasID)%ShomateA
       B = GasPhaseThermoChemistryData(gasID)%ShomateB
       C = GasPhaseThermoChemistryData(gasID)%ShomateC
       D = GasPhaseThermoChemistryData(gasID)%ShomateD
       E = GasPhaseThermoChemistryData(gasID)%ShomateE

       tempCp = tempCp + ((A + B*Tsho + C*Tsho**2 + D*Tsho**3 + E/(Tsho**2)) &
                      * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisConstit))
     ENDIF

     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN

       A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
       A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
       A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
       A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
       A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5

       tempCp = tempCp + (A1 + A2*Tkel + A3*Tkel**2 + A4*Tkel**3 + A5*Tkel**4)*RinKJperMolpK &
                      * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisConstit)

     ENDIF
    ENDIF
  ENDDO

  Cp = tempCp

  RETURN

END SUBROUTINE FigureAirHeatCap

SUBROUTINE FigureAirEnthalpy(GeneratorNum, FluidTemp, Hair)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Enthalpy from Shomate equations for fuel

          ! METHODOLOGY EMPLOYED:
          ! sum by weighting molar fractions of all fuel constituents.
          ! assumes mixture is sum of parts.

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Hair         ! (kJ/mol)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempHair
  REAL(r64)  :: HairI
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: F ! shomate coeff
  REAL(r64) :: H ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff
  REAL(r64) :: A6 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempHair = 0.0d0

  DO thisConstit=1, FuelCell(GeneratorNum)%AirSup%NumConstituents
    gasID = FuelCell(GeneratorNum)%AirSup%GasLibID(thisConstit)
    If (gasID > 0) THEN
     If (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) Then

       A = GasPhaseThermoChemistryData(gasID)%ShomateA
       B = GasPhaseThermoChemistryData(gasID)%ShomateB
       C = GasPhaseThermoChemistryData(gasID)%ShomateC
       D = GasPhaseThermoChemistryData(gasID)%ShomateD
       E = GasPhaseThermoChemistryData(gasID)%ShomateE
       F = GasPhaseThermoChemistryData(gasID)%ShomateF
       H = GasPhaseThermoChemistryData(gasID)%ShomateH

       HairI =  (A*Tsho + B*(Tsho**2)/2.0d0 + C*(Tsho**3)/3.0d0 + D*(Tsho**4)/4.0d0 - E/Tsho + F - H )

       tempHair = tempHair + HairI * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisConstit)

     ENDIF
     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN
       A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
       A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
       A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
       A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
       A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5
       A6 = GasPhaseThermoChemistryData(gasID)%NASA_A6

       tempHair = tempHair + ( ( (A1 + A2*Tkel/2.0d0 + A3*Tkel**2/3.0d0 + A4*Tkel**3/4.0d0 + A5*Tkel**4/5.0d0 + A6/Tkel)  &
                               * RinKJperMolpK * Tkel) - GasPhaseThermoChemistryData(gasID)%StdRefMolarEnthOfForm) &
                               * FuelCell(GeneratorNum)%AirSup%ConstitMolalFract(thisConstit)
     ENDIF
    ENDIF
  ENDDO

  Hair = tempHair

  RETURN

END SUBROUTINE FigureAirEnthalpy

SUBROUTINE FigureFuelHeatCap(GeneratorNum, FluidTemp, Cp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Cp from Shomate equations for fuel

          ! METHODOLOGY EMPLOYED:
          ! sum by weighting molar fractions of all fuel constituents.
          ! assumes mixture is sum of parts.

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Cp           !  (J/mol*K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempCp
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempCp = 0.0d0

  DO thisConstit=1, FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%NumConstituents
    gasID = FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%GasLibID(thisConstit)
    IF (GasID > 0) THEN
     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) THEN

       A = GasPhaseThermoChemistryData(gasID)%ShomateA
       B = GasPhaseThermoChemistryData(gasID)%ShomateB
       C = GasPhaseThermoChemistryData(gasID)%ShomateC
       D = GasPhaseThermoChemistryData(gasID)%ShomateD
       E = GasPhaseThermoChemistryData(gasID)%ShomateE

       tempCp = tempCp + ((A + B*Tsho + C*Tsho**2 + D*Tsho**3 + E/(Tsho**2)) &
                       * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%ConstitMolalFract(thisConstit))
     ENDIF

     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN
       A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
       A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
       A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
       A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
       A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5

       tempCp = tempCp + (A1 + A2*Tkel + A3*Tkel**2 + A4*Tkel**3 + A5*Tkel**4)*RinKJperMolpK &
                      * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%ConstitMolalFract(thisConstit)

     ENDIF
    ENDIF
  ENDDO

  Cp = tempCp

  RETURN

END SUBROUTINE FigureFuelHeatCap

SUBROUTINE FigureFuelEnthalpy(GeneratorNum, FluidTemp, Hfuel)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Enthalpy from Shomate equations for fuel

          ! METHODOLOGY EMPLOYED:
          ! sum by weighting molar fractions of all fuel constituents.
          ! assumes mixture is sum of parts.

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Hfuel        ! kJ/mol

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempHfuel
  REAL(r64)  :: HfuelI
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: F ! shomate coeff
  REAL(r64) :: H ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff
  REAL(r64) :: A6 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempHfuel = 0.0d0

  DO thisConstit=1, FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%NumConstituents
    gasID = FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%GasLibID(thisConstit)
    IF (gasID > 0) THEN
     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) Then
       A = GasPhaseThermoChemistryData(gasID)%ShomateA
       B = GasPhaseThermoChemistryData(gasID)%ShomateB
       C = GasPhaseThermoChemistryData(gasID)%ShomateC
       D = GasPhaseThermoChemistryData(gasID)%ShomateD
       E = GasPhaseThermoChemistryData(gasID)%ShomateE
       F = GasPhaseThermoChemistryData(gasID)%ShomateF
       H = GasPhaseThermoChemistryData(gasID)%ShomateH

       HfuelI  = (A*Tsho + B*(Tsho**2)/2.0d0 + C*(Tsho**3)/3.0d0 + D*(Tsho**4)/4.0d0 - E/Tsho + F - H )

       tempHfuel = tempHfuel + HfuelI* FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%ConstitMolalFract(thisConstit)

     ENDIF
     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN

       A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
       A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
       A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
       A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
       A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5
       A6 = GasPhaseThermoChemistryData(gasID)%NASA_A6

       tempHfuel = tempHfuel + (((A1 + A2*Tkel/2.0d0 + A3*Tkel**2/3.0d0 + A4*Tkel**3/4.0d0 + A5*Tkel**4/5.0d0 + A6/Tkel)      &
                               * RinKJperMolpK * Tkel) - GasPhaseThermoChemistryData(gasID)%StdRefMolarEnthOfForm) &
                               * FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%ConstitMolalFract(thisConstit)
     ENDIF
    ENDIF
  ENDDO

  Hfuel = tempHfuel

  RETURN

END SUBROUTINE FigureFuelEnthalpy

SUBROUTINE FigureProductGasesEnthalpy(GeneratorNum, FluidTemp, HProdGases)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Enthalpy from Shomate equations for gases

          ! METHODOLOGY EMPLOYED:
          ! sum by weighting molar fractions of all product gas constituents.
          ! assumes mixture is sum of parts.

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   HProdGases   ! kJ/mol

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempHprodGases
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: F ! shomate coeff
  REAL(r64) :: H ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff
  REAL(r64) :: A6 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempHprodGases = 0.0d0

  DO thisConstit=1, 5
    gasID = FuelCell(GeneratorNum)%FCPM%GasLibID(thisConstit)
    IF (gasID > 0) THEN
     If (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) Then
       A = GasPhaseThermoChemistryData(gasID)%ShomateA
       B = GasPhaseThermoChemistryData(gasID)%ShomateB
       C = GasPhaseThermoChemistryData(gasID)%ShomateC
       D = GasPhaseThermoChemistryData(gasID)%ShomateD
       E = GasPhaseThermoChemistryData(gasID)%ShomateE
       F = GasPhaseThermoChemistryData(gasID)%ShomateF
       H = GasPhaseThermoChemistryData(gasID)%ShomateH

       tempHprodGases = tempHprodGases + ( (A*Tsho + B*(Tsho**2)/2.0d0 + C*(Tsho**3)/3.0d0 + D*(Tsho**4)/4.0d0 - E/Tsho + F - H ) &
                               * FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(thisConstit) )
     ENDIF
     IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN
       A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
       A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
       A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
       A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
       A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5
       A6 = GasPhaseThermoChemistryData(gasID)%NASA_A6

       tempHprodGases = tempHprodGases +   &
                 (((A1 + A2*Tkel/2.0d0 + A3*Tkel**2/3.0d0 + A4*Tkel**3/4.0d0 + A5*Tkel**4/5.0d0 + A6/Tkel)     &
                   * RinKJperMolpK * Tkel) - GasPhaseThermoChemistryData(gasID)%StdRefMolarEnthOfForm) &
                   * FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(thisConstit)
     ENDIF
    ENDIF ! gasid > 0
  ENDDO

  HProdGases = tempHprodGases

  RETURN

END SUBROUTINE FigureProductGasesEnthalpy



SUBROUTINE FigureProductGasHeatCap(GeneratorNum, FluidTemp, Cp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Cp           !  (J/mol*K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempCp
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempCp = 0.0d0

  DO thisConstit=1, Size(FuelCell(GeneratorNum)%FCPM%GasLibID)
    gasID = FuelCell(GeneratorNum)%FCPM%GasLibID(thisConstit)
    IF (gasID > 0) THEN
      IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) THEN

        A = GasPhaseThermoChemistryData(gasID)%ShomateA
        B = GasPhaseThermoChemistryData(gasID)%ShomateB
        C = GasPhaseThermoChemistryData(gasID)%ShomateC
        D = GasPhaseThermoChemistryData(gasID)%ShomateD
        E = GasPhaseThermoChemistryData(gasID)%ShomateE

        tempCp = tempCp + ((A + B*Tsho + C*Tsho**2 + D*Tsho**3 + E/(Tsho**2)) &
                       * FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(thisConstit))
      ENDIF

      IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN
        A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
        A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
        A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
        A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
        A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5

        tempCp = tempCp + (A1 + A2*Tkel + A3*Tkel**2 + A4*Tkel**3 + A5*Tkel**4)*RinKJperMolpK &
                      * FuelCell(GeneratorNum)%FCPM%ConstitMolalFract(thisConstit)

      ENDIF

    ENDIF

  ENDDO

  Cp = tempCp

  RETURN

END SUBROUTINE FigureProductGasHeatCap

SUBROUTINE FigureAuxilHeatGasHeatCap(GeneratorNum, FluidTemp, Cp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Cp           !  (J/mol*K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempCp
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempCp = 0.0d0

  DO thisConstit=1, Size(FuelCell(GeneratorNum)%AuxilHeat%GasLibID)
    gasID = FuelCell(GeneratorNum)%AuxilHeat%GasLibID(thisConstit)
    IF (gasID > 0) THEN
      IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) THEN

        A = GasPhaseThermoChemistryData(gasID)%ShomateA
        B = GasPhaseThermoChemistryData(gasID)%ShomateB
        C = GasPhaseThermoChemistryData(gasID)%ShomateC
        D = GasPhaseThermoChemistryData(gasID)%ShomateD
        E = GasPhaseThermoChemistryData(gasID)%ShomateE

        tempCp = tempCp + ((A + B*Tsho + C*Tsho**2 + D*Tsho**3 + E/(Tsho**2)) &
                       * FuelCell(GeneratorNum)%AuxilHeat%ConstitMolalFract(thisConstit))
      ENDIF

      IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN
        A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
        A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
        A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
        A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
        A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5

        tempCp = tempCp + (A1 + A2*Tkel + A3*Tkel**2 + A4*Tkel**3 + A5*Tkel**4)*RinKJperMolpK &
                      * FuelCell(GeneratorNum)%AuxilHeat%ConstitMolalFract(thisConstit)

      ENDIF

    ENDIF

  ENDDO

  Cp = tempCp

  RETURN

END SUBROUTINE FigureAuxilHeatGasHeatCap

SUBROUTINE FigureHXleavingGasHeatCap(GeneratorNum, FluidTemp, Cp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  INTEGER, INTENT(IN)  ::   GeneratorNum ! ID of generator FuelCell data structure
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Cp           !  (J/mol*K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64)  :: Tkel ! temp for NASA eq. in Kelvin
  REAL(r64)  :: tempCp
  INTEGER   :: thisConstit !loop index
  INTEGER   :: gasID !look up into Gas structure
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: A1 ! NASA poly coeff
  REAL(r64) :: A2 ! NASA poly coeff
  REAL(r64) :: A3 ! NASA poly coeff
  REAL(r64) :: A4 ! NASA poly coeff
  REAL(r64) :: A5 ! NASA poly coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0
  Tkel = (FluidTemp +KelvinConv)
  ! loop through fuel constituents and sum up Cp

  tempCp = 0.0d0

  DO thisConstit=1, Size(FuelCell(GeneratorNum)%ExhaustHX%GasLibID)
    gasID = FuelCell(GeneratorNum)%ExhaustHX%GasLibID(thisConstit)
    IF (gasID > 0) THEN
      IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NISTShomate) THEN

        A = GasPhaseThermoChemistryData(gasID)%ShomateA
        B = GasPhaseThermoChemistryData(gasID)%ShomateB
        C = GasPhaseThermoChemistryData(gasID)%ShomateC
        D = GasPhaseThermoChemistryData(gasID)%ShomateD
        E = GasPhaseThermoChemistryData(gasID)%ShomateE

        tempCp = tempCp + ((A + B*Tsho + C*Tsho**2 + D*Tsho**3 + E/(Tsho**2)) &
                       * FuelCell(GeneratorNum)%ExhaustHX%ConstitMolalFract(thisConstit))
      ENDIF

      IF (GasPhaseThermoChemistryData(gasID)%ThermoMode == NASAPolynomial) THEN
        A1 = GasPhaseThermoChemistryData(gasID)%NASA_A1
        A2 = GasPhaseThermoChemistryData(gasID)%NASA_A2
        A3 = GasPhaseThermoChemistryData(gasID)%NASA_A3
        A4 = GasPhaseThermoChemistryData(gasID)%NASA_A4
        A5 = GasPhaseThermoChemistryData(gasID)%NASA_A5

        tempCp = tempCp + (A1 + A2*Tkel + A3*Tkel**2 + A4*Tkel**3 + A5*Tkel**4)*RinKJperMolpK &
                      * FuelCell(GeneratorNum)%ExhaustHX%ConstitMolalFract(thisConstit)

      ENDIF

    ENDIF

  ENDDO

  Cp = tempCp

  RETURN

END SUBROUTINE FigureHXleavingGasHeatCap


SUBROUTINE FigureGaseousWaterEnthalpy(FluidTemp, HGasWater)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   December 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Enthalpy from Shomate equations for gaseous water
          ! No ethalphy of formation in this one.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   HGasWater    ! kJ/mol

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: F ! shomate coeff
!  REAL(r64) :: H ! shomate coeff


  Tsho = (FluidTemp +KelvinConv) / 1000.0d0

  A = 29.0373d0
  B = 10.2573d0
  C = 2.81048d0
  D = -0.95914d0
  E = 0.11725d0
  F = -250.569d0


  HGasWater = A*Tsho + B*(Tsho**2)/2.0d0 + C*(Tsho**3)/3.0d0 + D*(Tsho**4)/4.0d0 - E/Tsho + F !- H

  RETURN
END SUBROUTINE FigureGaseousWaterEnthalpy

SUBROUTINE FigureLiquidWaterEnthalpy(FluidTemp, HLiqWater)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B griffith
          !       DATE WRITTEN   December 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate Enthalpy from Shomate equations for liquid water
          ! No enthalpy of formation in this one

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! NIST Webbook on gas phase thermochemistry

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   HLiqWater    ! kJ/mol

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff
  REAL(r64) :: F ! shomate coeff
  REAL(r64) :: H ! shomate coeff


  Tsho = (FluidTemp +KelvinConv) / 1000.0d0

  A = -203.606d0
  B = 1523.29d0
  C = -3196.413d0
  D = 2474.455d0
  E = 3.85533d0
  F = -256.5478d0
  H = -285.8304d0

  HLiqWater = A*Tsho + B*(Tsho**2)/2.0d0 + C*(Tsho**3)/3.0d0 + D*(Tsho**4)/4.0d0 - E/Tsho + F !- H

  RETURN
END SUBROUTINE FigureLiquidWaterEnthalpy

SUBROUTINE FigureLiquidWaterHeatCap(FluidTemp, Cp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   December 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate shomate eq. for pure liquid water

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  REAL(r64),    INTENT(IN)  ::   FluidTemp    ! degree C
  REAL(r64),    INTENT(OUT) ::   Cp           !  (J/mol*K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tsho ! temp for Shomate eq  in (Kelvin/1000)
  REAL(r64) :: A ! shomate coeff
  REAL(r64) :: B ! shomate coeff
  REAL(r64) :: C ! shomate coeff
  REAL(r64) :: D ! shomate coeff
  REAL(r64) :: E ! shomate coeff

  Tsho = (FluidTemp +KelvinConv) / 1000.0d0

  A = -203.606d0
  B = 1523.29d0
  C = -3196.413d0
  D = 2474.455d0
  E = 3.85533d0

  Cp = A + B*Tsho + C*Tsho**2 + D*Tsho**3 + E/(Tsho**2)

  RETURN
END SUBROUTINE FigureLiquidWaterHeatCap


SUBROUTINE FigureLHVofFuel(Num,NdotFuel, NdotCO2,  NdotH20, LHV)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate LHV

          ! METHODOLOGY EMPLOYED:
          ! ANNEX 42 eq. 6 method from molar enthalpies

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Num
  REAL(r64)   , INTENT(IN) :: NdotFuel
  REAL(r64)   , INTENT(IN) :: NdotCO2
  REAL(r64)   , INTENT(IN) :: NdotH20
  REAL(r64)   , INTENT(OUT):: LHV
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: DelfHfuel
  REAL(r64)  :: DelfHCO2
  REAL(r64)  :: DelfHH20
  INTEGER ::I
  REAL(r64)  :: h_i
  INTEGER :: CO2dataID
  INTEGER :: WaterDataID
  INTEGER :: thisGasID

  CO2dataID   = 1  !hard-coded in SetupFuelAndAirConstituentData
  WaterDataID = 4  !hard-coded in SetupFuelAndAirConstituentData
  DelfHfuel = 0.0d0

  DO I = 1, FuelSupply(FuelCell(Num)%FuelSupNum)%NumConstituents
     thisGasID = FuelSupply(FuelCell(Num)%FuelSupNum)%GasLibID(i)

     h_i = GasPhaseThermoChemistryData(thisGasID)%StdRefMolarEnthOfForm

     DelfHfuel = DelfHfuel + NdotFuel * h_i * FuelSupply(FuelCell(Num)%FuelSupNum)%ConstitMolalFract(i)

  ENDDO

  DelfHCO2 = GasPhaseThermoChemistryData(CO2dataID)%StdRefMolarEnthOfForm * NdotCO2

  DelfHH20 = GasPhaseThermoChemistryData(WaterDataID)%StdRefMolarEnthOfForm * NdotH20


  LHV = (DelfHfuel - DelfHCO2 - DelfHH20) / NdotFuel  !Equation 6


  RETURN

END SUBROUTINE FigureLHVofFuel

SUBROUTINE FigureACAncillaries(GeneratorNum, PacAncill)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   March 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the AC ancillaries to determine Pel

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE CurveManager,    ONLY : CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: GeneratorNum
  REAL(r64),    INTENT(OUT)  :: PacAncill

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

 !  Using lagged values inside a sequential substitution loop
  PacAncill = 0.0d0
  ! sect. 5.9
  FuelCell(GeneratorNum)%FCPM%PelancillariesAC = FuelCell(GeneratorNum)%FCPM%ANC0   &
                                             + FuelCell(GeneratorNum)%FCPM%ANC1 * FuelCell(GeneratorNum)%FCPM%NdotFuel

  ! sect 6.0
  FuelCell(GeneratorNum)%AirSup%PairCompEl  = CurveValue(FuelCell(GeneratorNum)%AirSup%BlowerPowerCurveID, &
                                                       FuelCell(GeneratorNum)%FCPM%NdotAir)
  ! sect 7.0
  FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%PfuelCompEl  &
          = CurveValue(FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%CompPowerCurveID, FuelCell(GeneratorNum)%FCPM%NdotFuel)

  ! sect. 8.0
  FuelCell(GeneratorNum)%WaterSup%PwaterCompEl  = CurveValue(FuelCell(GeneratorNum)%WaterSup%PmpPowerCurveID, &
                                                          FuelCell(GeneratorNum)%FCPM%NdotLiqwater )

  PacAncill =  FuelCell(GeneratorNum)%FCPM%PelancillariesAC &
             + FuelCell(GeneratorNum)%AirSup%PairCompEl     &
             + FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%PfuelCompEl   &
             + FuelCell(GeneratorNum)%WaterSup%PwaterCompEl

  RETURN

END SUBROUTINE FigureACAncillaries


SUBROUTINE FigurePowerConditioningLosses(GeneratorNum, Pdemand, PpcuLosses)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate inverter losses

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE CurveManager,    ONLY : CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: GeneratorNum
  REAL(r64),    INTENT(IN)   :: Pdemand
  REAL(r64),    INTENT(OUT)  :: PpcuLosses

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: lastPpcuLosses !used in iterative solution
  INTEGER :: iter
  REAL(r64)    :: Pel

  IF (FuelCell(GeneratorNum)%Inverter%EffMode ==  InverterEffConstant) THEN

    PpcuLosses = Pdemand * (1- FuelCell(GeneratorNum)%Inverter%ConstEff) &
                 / FuelCell(GeneratorNum)%Inverter%ConstEff

  ENDIF

  IF (FuelCell(GeneratorNum)%Inverter%EffMode ==  InverterEffQuadratic) THEN

    ! first use Pdemand instead of Pel to get initial estimate
    lastPpcuLosses = Pdemand * ( 1.0d0 - CurveValue(FuelCell(GeneratorNum)%Inverter%EffQuadraticCurveID, Pdemand) )  &
                     /CurveValue(FuelCell(GeneratorNum)%Inverter%EffQuadraticCurveID, Pdemand)

    Do iter=1, 20  ! seems like need to iterate (??) Need to investigate number and convergence success here

      Pel = Pdemand + lastPpcuLosses

      lastPpcuLosses  = (1.0d0 - CurveValue(FuelCell(GeneratorNum)%Inverter%EffQuadraticCurveID, Pel) ) * Pel

    ENDDO

    PpcuLosses = lastPpcuLosses

  ENDIF

  RETURN

END SUBROUTINE FigurePowerConditioningLosses

SUBROUTINE FigureTransientConstraints(GeneratorNum, Pel, Constrained, PelDiff)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: GeneratorNum !index number for accessing correct generator
  REAL(r64),    INTENT(INOUT)   :: Pel         ! DC power control setting for power module
  LOGICAL, INTENT(OUT)     :: Constrained ! true if transient constraints kick in
  REAL(r64),    INTENT(OUT)     :: PelDiff     ! if constrained then this is the difference, positive
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused  REAL(r64)  :: CurrentHours
  REAL(r64)  :: CurrentFractionalDay !working var, time in decimal days
  REAL(r64)  :: EndingFractionalDay  !working var, time is decimal days
  REAL(r64)  :: MaxPel !working variable for max allowed by transient constraint
  REAL(r64)  :: MinPel !working variabel for min allowed by transient constraint
  REAL(r64)  :: PelInput !hold initial value of inout var

  PelInput = Pel

 ! Check if in start up and if it still should be
   IF (FuelCell(GeneratorNum)%FCPM%DuringStartUp) THEN

     !calculate time for end of start up period
     CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay

     EndingFractionalDay = FuelCell(GeneratorNum)%FCPM%FractionalDayofLastStartUp +   &
        FuelCell(GeneratorNum)%FCPM%StartUpTime/HoursInDay

     IF (CurrentFractionalDay > EndingFractionalDay) THEN
      !start up period is now over
         FuelCell(GeneratorNum)%FCPM%DuringStartUp = .false.
     ENDIF
   ENDIF

 ! Check if in shut down up and if it still should be
   IF (FuelCell(GeneratorNum)%FCPM%DuringShutDown) THEN

     !calculate time for end of shut down period
     CurrentFractionalDay = REAL(DayOfSim,r64)  &
                            + (INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime))))/HoursInDay !

     EndingFractionalDay = FuelCell(GeneratorNum)%FCPM%FractionalDayofLastShutDown +   &
        FuelCell(GeneratorNum)%FCPM%ShutDownTime/HoursInDay

     IF (CurrentFractionalDay > EndingFractionalDay) THEN
      !start up period is now over
         FuelCell(GeneratorNum)%FCPM%DuringShutDown = .false.
     ENDIF
   ENDIF
 !compare

  If (.NOT. (FuelCell(GeneratorNum)%FCPM%DuringShutDown) .AND.  .NOT. (FuelCell(GeneratorNum)%FCPM%DuringStartUp)) then
    !unit is neither starting or stopping and the only constraints would come from transient limits
    IF (Pel > FuelCell(GeneratorNum)%FCPM%PelLastTimeStep) THEN ! powering up
      MaxPel = FuelCell(GeneratorNum)%FCPM%PelLastTimeStep + FuelCell(GeneratorNum)%FCPM%UpTranLimit * TimeStepSys *  SecInHour
      IF (MaxPel < Pel) THEN
        Pel = MaxPel
        Constrained = .TRUE.
      Else
        Constrained = .FALSE.
      ENDIF
    ELSEIF (Pel< FuelCell(GeneratorNum)%FCPM%PelLastTimeStep) THEN !powering down
      MinPel = FuelCell(GeneratorNum)%FCPM%PelLastTimeStep - FuelCell(GeneratorNum)%FCPM%DownTranLimit * TimeStepSys *  SecInHour
      IF (Pel < MinPel) THEN
        Pel = MinPel
        Constrained = .TRUE.
      ELSE
        Constrained = .FALSE.
      ENDIF
    ELSE !the same
    !do nothing
      Constrained = .FALSE.
    ENDIF

  ENDIF !not in start up or shut down

  IF (FuelCell(GeneratorNum)%FCPM%DuringStartUp) THEN
   !constant during start up modeling artifact
    Pel = FuelCell(GeneratorNum)%FCPM%StartUpElectProd/FuelCell(GeneratorNum)%FCPM%StartUpTime
    Constrained = .true.
  ENDIF

  IF (FuelCell(GeneratorNum)%FCPM%DuringShutDown) THEN

    Pel = 0.0d0 ! assumes no power generated during shut down
    Constrained = .true.
  ENDIF

  PelDiff = 0.0d0
  IF (constrained) then
   PelDiff = PelInput - Pel
  endif


  RETURN

END SUBROUTINE FigureTransientConstraints

SUBROUTINE CalcFuelCellAuxHeater(num)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT(IN)          :: Num     ! Generator number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  !not yet implemented, just pass product gases thru nul domain

  FuelCell(Num)%AuxilHeat%TauxMix           = FuelCell(Num)%FCPM%TprodGasLeavingFCPM
  FuelCell(Num)%AuxilHeat%NdotAuxMix        = FuelCell(Num)%FCPM%NdotProdGas
  FuelCell(Num)%AuxilHeat%ConstitMolalFract = FuelCell(Num)%FCPM%ConstitMolalFract
  FuelCell(Num)%AuxilHeat%GasLibID          = FuelCell(Num)%FCPM%GasLibID

  RETURN

END SUBROUTINE CalcFuelCellAuxHeater


SUBROUTINE CalcFuelCellGenHeatRecovery(Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Brent Griffith
            !       DATE WRITTEN:    Aug. 2005

            ! PURPOSE OF THIS SUBROUTINE:
            ! To perform heat recovery calculations and node updates


            ! METHODOLOGY EMPLOYED:
            ! model exhaust gas to water heat exchanger


            ! REFERENCES: Annex 42 model documentation

            ! USE STATEMENTS:
  USE FluidProperties , ONLY: GetSpecificHeatGlycol
  USE DataPlant,        ONLY: PlantLoop

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT(IN)          :: Num     ! Generator number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: eHX ! fixed effectiveness
  REAL(r64)  :: MdotWater
  INTEGER  :: inNodeNum
  REAL(r64)  :: MWwater
  REAL(r64)  :: NdotWater
  REAL(r64)  :: TwaterIn
  REAL(r64)  :: CpWaterMol
  REAL(r64)  :: NdotGas
  REAL(r64)  :: TprodGasIn
  REAL(r64)  :: CpProdGasMol
  REAL(r64)  :: NdotCp
  REAL(r64)  :: qHX
  REAL(r64)  :: UAeff
  REAL(r64)  :: TauxMix
  REAL(r64)  :: NdotCpWater
  REAL(r64)  :: NdotCpAuxMix
  REAL(r64)  :: THXexh
  REAL(r64)  :: TwaterOut
  REAL(r64)  :: hgas
  REAL(r64)  :: hwater
  REAL(r64)  :: waterFract=0.0d0
  REAL(r64)  :: NdotWaterVapor
  REAL(r64)  :: TcondThresh
  REAL(r64)  :: hxl1
  REAL(r64)  :: hxl2
  REAL(r64)  :: NdotWaterCond=0.0d0
  REAL(r64)  :: hfpwater
  INTEGER    :: I

  REAL(r64)  :: qSens
  REAL(r64)  :: qLatent
  INTEGER    :: loop
  REAL(r64)  :: Cp

  SELECT CASE (FuelCell(Num)%ExhaustHX%HXmodelMode)

  CASE(FixedEffectiveness) !Method 1

    eHX = FuelCell(Num)%ExhaustHX%HXEffect

    inNodeNum = FuelCell(Num)%ExhaustHX%WaterInNode

    MdotWater = FuelCell(Num)%ExhaustHX%WaterMassFlowRate
    MWwater   =   GasPhaseThermoChemistryData(4)%MolecularWeight
    NdotWater = MdotWater/MWwater
    TwaterIn  = FuelCell(Num)%ExhaustHX%WaterInletTemp

    CALL FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol)

    NdotGas   =   FuelCell(Num)%AuxilHeat%NdotAuxMix
    TprodGasIn =  FuelCell(Num)%AuxilHeat%TauxMix
    CALL FigureAuxilHeatGasHeatCap(Num, TprodGasIn , CpProdGasMol) ! Cp in (J/mol*K)
    !factor of 1000.0 for kmol -> mol
    NdotCp = MIN(NdotGas*CpProdGasMol*1000.0d0, NdotWater*CpWaterMol*1000.0d0)

    qHX   = eHX * NdotCp*(TprodGasIn - TwaterIn)

    THXexh = TprodGasIn - qHX / (NdotGas*CpProdGasMol*1000.0d0)

    Cp = GetSpecificHeatGlycol(PlantLoop(FuelCell(Num)%CWLoopNum)%FluidName, &
                               TwaterIn, &
                               PlantLoop(FuelCell(Num)%CWLoopNum)%FluidIndex, &
                               'CalcFuelCellGenHeatRecovery')

    IF (MdotWater * Cp  <= 0.0d0) THEN
      TwaterOut =  TwaterIn
    ELSE
      TwaterOut  =  TwaterIn + qHX / (MdotWater * Cp)
    ENDIF

  CASE(LMTDempiricalUAeff) !method 2
    inNodeNum = FuelCell(Num)%ExhaustHX%WaterInNode
    MdotWater = FuelCell(Num)%ExhaustHX%WaterMassFlowRate
    MWwater   =   GasPhaseThermoChemistryData(4)%MolecularWeight
    NdotWater = MdotWater/MWwater
    NdotGas   = FuelCell(Num)%AuxilHeat%NdotAuxMix

    UAeff = FuelCell(Num)%ExhaustHX%hxs0 + FuelCell(Num)%ExhaustHX%hxs1*NdotWater   &
            + FuelCell(Num)%ExhaustHX%hxs2 * (NdotWater**2) +                     &
            FuelCell(Num)%ExhaustHX%hxs3 * NdotGas + FuelCell(Num)%ExhaustHX%hxs4*(NdotGas**2)

    TauxMix = FuelCell(Num)%AuxilHeat%TauxMix
    TwaterIn  = FuelCell(Num)%ExhaustHX%WaterInletTemp
    CALL FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol)
        !factor of 1000.0 for kmol -> mol
    NdotCpWater = NdotWater*CpWaterMol*1000.0d0
    CALL FigureAuxilHeatGasHeatCap(Num, TauxMix , CpProdGasMol) ! Cp in (J/mol*K)
    NdotCpAuxMix = NdotGas*CpProdGasMol*1000.0d0

    ! commented out protection for taking exponent of too large a number
    !   because it hasn't been a problem in testing
    !testVal = log(huge(NdotCpAuxMix))
    !ExpTestVal = 700.0
    !IF (UAeff*(1/NdotCpAuxMix) > ExpTestVal) THEN
      ! write(*,*) 'Houston, we have a problem, EXP [] func will fail for UAeff*(1/NdotCpAuxMix):', UAeff*(1/NdotCpAuxMix)
   ! ELSEIF (UAeff*(1/NdotCpWater) > ExpTestVal) THEN
    !   write(*,*) 'Houston, we have a problem, EXP [] func will fail for UAeff*(1/NdotCpWater:', UAeff*(1/NdotCpWater)
  !  ELSE

    If ((NdotCpWater /= 0.0d0) .AND. (NdotCpAuxMix /= 0.0d0)) then ! trap divide by zero
      ! now evaluate Eq. 44
      THXexh = ((1.0d0-NdotCpAuxMix/NdotCpWater)/ &
                  (EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) - NdotCpAuxMix/NdotCpWater) )* TauxMix  &
            +(    (EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) -1.0d0)               &
                 /(EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) - NdotCpAuxMix/NdotCpWater) )* TwaterIn

      TwaterOut = TwaterIn + (NdotCpAuxMix/NdotCpWater) * (TauxMix - THXexh)  ! Eq. 42

    ELSE
      THXexh = TauxMix
      TwaterOut = TwaterIn
    ENDIF
   ! ENDIF

    IF ((THXexh - TwaterIn) /= 0.0d0) THEN ! trap divide by zero
      qHX = UAeff * ( (TauxMix - TwaterOut) - (THXexh - TwaterIn) )/LOG( (TauxMix - TwaterOut) / (THXexh - TwaterIn) )
    ELSE
      qHX = 0.0d0
    ENDIF

  CASE(LMTDfundementalUAeff) !method 3
    NdotGas   =   FuelCell(Num)%AuxilHeat%NdotAuxMix
    inNodeNum = FuelCell(Num)%ExhaustHX%WaterInNode
    MdotWater = FuelCell(Num)%ExhaustHX%WaterMassFlowRate
    MWwater   =   GasPhaseThermoChemistryData(4)%MolecularWeight
    NdotWater = MdotWater/MWwater

    hgas = FuelCell(Num)%ExhaustHX%h0gas * (NdotGas/FuelCell(Num)%ExhaustHX%NdotGasRef)**FuelCell(Num)%ExhaustHX%nCoeff !Eq. 48

    hwater = FuelCell(Num)%ExhaustHX%h0Water   &
       * (NdotWater/FuelCell(Num)%ExhaustHX%NdotWaterRef)**FuelCell(Num)%ExhaustHX%mCoeff !Eq. 48

    ! now equation 47
    UAeff = 1.0d0/(1.0d0/(hgas*FuelCell(Num)%ExhaustHX%AreaGas) + 1.0d0/(hwater*FuelCell(Num)%ExhaustHX%AreaWater)   &
       +  FuelCell(Num)%ExhaustHX%Fadjust)

    TauxMix = FuelCell(Num)%AuxilHeat%TauxMix
    TwaterIn  = FuelCell(Num)%ExhaustHX%WaterInletTemp
    CALL FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol)
    NdotCpWater = NdotWater*CpWaterMol*1000.0d0
    CALL FigureAuxilHeatGasHeatCap(Num, TauxMix , CpProdGasMol) ! Cp in (J/mol*K)
    NdotCpAuxMix = NdotGas*CpProdGasMol*1000.0d0

    If ((NdotCpWater /= 0.0d0) .AND. (NdotCpAuxMix /= 0.0d0)) then ! trap divide by zero
    ! now evaluate Eq. 44
      THXexh = ((1.0d0-NdotCpAuxMix/NdotCpWater)/ &
                  (EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) - NdotCpAuxMix/NdotCpWater) )* TauxMix  &
            +(    (EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) -1.0d0)               &
                 /(EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) - NdotCpAuxMix/NdotCpWater) )* TwaterIn

      TwaterOut = TwaterIn + (NdotCpAuxMix/NdotCpWater) * (TauxMix - THXexh)  ! Eq. 42

    ELSE
      THXexh    = TauxMix
      TwaterOut = TwaterIn
    ENDIF

    IF ((THXexh - TwaterIn) /= 0.0d0) THEN ! trap divide by zero
      qHX = UAeff * ( (TauxMix - TwaterOut) - (THXexh - TwaterIn) )/LOG( (TauxMix - TwaterOut) / (THXexh - TwaterIn) )
    ELSE
      qHX = 0.0d0
    ENDIF

  CASE(Condensing) !method 4
    inNodeNum = FuelCell(Num)%ExhaustHX%WaterInNode
    MdotWater = FuelCell(Num)%ExhaustHX%WaterMassFlowRate
    If (MdotWater /= 0.0D0) Then !


      MWwater   =   GasPhaseThermoChemistryData(4)%MolecularWeight
      NdotWater = MdotWater/MWwater
      NdotGas   =   FuelCell(Num)%AuxilHeat%NdotAuxMix

      UAeff = FuelCell(Num)%ExhaustHX%hxs0 + FuelCell(Num)%ExhaustHX%hxs1*NdotWater   &
              + FuelCell(Num)%ExhaustHX%hxs2 * NdotWater**2 +                     &
              FuelCell(Num)%ExhaustHX%hxs3 * NdotGas + FuelCell(Num)%ExhaustHX%hxs4*NdotGas**2

      TauxMix = FuelCell(Num)%AuxilHeat%TauxMix
      TwaterIn  = FuelCell(Num)%ExhaustHX%WaterInletTemp
      CALL FigureLiquidWaterHeatCap(TwaterIn, CpWaterMol)
      NdotCpWater = NdotWater*CpWaterMol*1000.0d0
      CALL FigureAuxilHeatGasHeatCap(Num, TauxMix , CpProdGasMol) ! Cp in (J/mol*K)
      NdotCpAuxMix = NdotGas*CpProdGasMol*1000.0d0

     !find water fraction in incoming gas stream
      DO I = 1, Size(FuelCell(Num)%AuxilHeat%GasLibID)
        If (FuelCell(Num)%AuxilHeat%GasLibID(I) == 4) waterFract = FuelCell(Num)%AuxilHeat%ConstitMolalFract(I)
      ENDDO
      NdotWaterVapor =waterFract * NdotGas

      TcondThresh = FuelCell(Num)%ExhaustHX%CondensationThresholdTemp
      hxl1 = FuelCell(Num)%ExhaustHX%l1Coeff
      hxl2 = FuelCell(Num)%ExhaustHX%l2Coeff

      NdotWaterCond = (TcondThresh - TwaterIn) * ( hxl1 * (NdotWaterVapor/NdotGas) + hxl2*(NdotWaterVapor/NdotGas)**2)

      If (NdotWaterCond < 0.0d0) NdotWaterCond = 0.0d0

      hfpwater =  4.4004d+07  ! molal heat of vaporization of water J/kmol)

      If ((NdotCpWater /= 0.0d0) .AND. (NdotCpAuxMix /= 0.0d0)) then ! trap divide by zero

      ! now evaluate Eq. 44
        THXexh = ((1.0d0-NdotCpAuxMix/NdotCpWater)/    &
                    (EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) - NdotCpAuxMix/NdotCpWater) )* TauxMix  &
              +(    (EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) -1.0d0)               &
                   /(EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater)) - NdotCpAuxMix/NdotCpWater) )* TwaterIn

        TwaterOut = TwaterIn + (NdotCpAuxMix/NdotCpWater) * (TauxMix - THXexh) + (NdotWaterCond * hfpwater)/NdotCpWater

        IF (NdotWaterCond > 0) then ! Eq. 44 is not correct. use its result as first guess for revised way...

               do loop = 1, 5 ! iterative soluion because in condensing case THXexh is function of qSens and qLatent

                  IF ((THXexh - TwaterIn) /= 0.0d0) THEN ! trap divide by zero
                    qSens = UAeff * ( (TauxMix - TwaterOut) - (THXexh - TwaterIn) )/  &
                                           LOG( (TauxMix - TwaterOut) / (THXexh - TwaterIn) )
                  else
                    qSens = 0.0d0
                  endif
                  qLatent=  NdotWaterCond * hfpwater
                  If (qSens > 0) then
                    THXexh = TauxMix *( (1.0d0-NdotCpAuxMix/NdotCpWater)                                                     &
                                        /(  ( EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater) )                           &
                                           /( EXP((UAeff * qLatent)/(NdotCpWater * qSens)) ) ) - NdotCpAuxMix/NdotCpWater) ) &
                           + TwaterIn *( ( EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater))                               &
                                          /( EXP((UAeff * qLatent)/(NdotCpWater * qSens)) )      -1.0d0)                     &
                                          /(EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater))                              &
                                           /( EXP((UAeff * qLatent)/(NdotCpWater * qSens)) ) - NdotCpAuxMix/NdotCpWater) )   &
                           -( (qLatent/NdotCpWater) /(EXP(UAeff*(1.0d0/NdotCpAuxMix - 1.0d0/NdotCpWater))                    &
                                           /( EXP((UAeff * qLatent)/(NdotCpWater * qSens)) ) - NdotCpAuxMix/NdotCpWater) )
                  else
                    THXexh = TauxMix
                  endif

                  TwaterOut = TwaterIn + (NdotCpAuxMix/NdotCpWater) * (TauxMix - THXexh) + (NdotWaterCond * hfpwater)/NdotCpWater


               enddo

        endif

      ELSE
        THXexh    = TauxMix
        TwaterOut = TwaterIn

      ENDIF


      IF ((THXexh - TwaterIn) /= 0.0d0) THEN ! trap divide by zero

        qHX = UAeff * ( (TauxMix - TwaterOut) - (THXexh - TwaterIn) )/LOG( (TauxMix - TwaterOut) / (THXexh - TwaterIn) ) &
               + NdotWaterCond * hfpwater
      ELSE
        qHX = 0.0d0
      ENDIF
    ELSE !no cooling water flow, model will blow up.
      qHX       = 0.0D0
      THXexh    = FuelCell(Num)%AuxilHeat%TauxMix
      TwaterOut = FuelCell(Num)%ExhaustHX%WaterInletTemp
      NdotWaterCond = 0.0D0
      waterFract = -9999.0D0 ! not defined

    ENDIF
    !init input from Auxiliary heater
   ! FuelCell(Num)%ExhaustHX%NdotHXleaving      = FuelCell(Num)%AuxilHeat%NdotAuxMix
   ! FuelCell(Num)%ExhaustHX%ConstitMolalFract  = FuelCell(Num)%AuxilHeat%ConstitMolalFract
   ! FuelCell(Num)%ExhaustHX%GasLibID           = FuelCell(Num)%AuxilHeat%GasLibID

    ! now modify leaving gas constituents for condensed water.
   ! FuelCell(Num)%ExhaustHX%NdotHXleaving = FuelCell(Num)%AuxilHeat%NdotAuxMix - NdotWaterCond
   ! If ( FuelCell(Num)%ExhaustHX%NdotHXleaving > 0) then
   !   DO I = 1, Size(FuelCell(Num)%AuxilHeat%GasLibID)
   !     If (FuelCell(Num)%AuxilHeat%GasLibID(I) == 4) then ! water constituent
   !       FuelCell(Num)%ExhaustHX%ConstitMolalFract(I) = &
   !            (FuelCell(Num)%AuxilHeat%ConstitMolalFract(I)* FuelCell(Num)%AuxilHeat%NdotAuxMix - NdotWaterCond) &
   !            /     FuelCell(Num)%ExhaustHX%NdotHXleaving
   !       cycle
   !     ENDIF

   !     FuelCell(Num)%ExhaustHX%ConstitMolalFract(I) = FuelCell(Num)%AuxilHeat%ConstitMolalFract(I) &
   !                                       * FuelCell(Num)%AuxilHeat%NdotAuxMix / FuelCell(Num)%ExhaustHX%NdotHXleaving
   !   ENDDO
   ! ENDIF


    ! get new average heat capacity
    !CALL FigureHXleavingGasHeatCap(Num, (THXexh + TauxMix)/2 , CpHXleavingGasMol)

   ! NdotCpHXleaving = FuelCell(Num)%ExhaustHX%NdotHXleaving*CpHXleavingGasMol* 1000.0

    ! update gas leaving temperature with modified heat transfer rate
  !  IF ((NdotCpHXleaving > 0) .AND. (qHX > 0)) THEN
  !     THXexh = TauxMix - (qHX / NdotCpHXleaving)
  !  ELSE
  !     THXexh = TauxMix
  !  ENDIF
    ! update water leaving temperature with modified heat transfer rate
  !  IF (MdotWater * CPCW( (TwaterIn + TwaterOut)/2 )  <= 0.0) THEN
  !    TwaterOut =  TwaterIn
  !  ELSE
  !    TwaterOut  =  TwaterIn + qHX / (MdotWater * CPCW( (TwaterIn + TwaterOut)/2 ))
  !  ENDIF

  END SELECT

 ! update results in data structure.
  FuelCell(Num)%ExhaustHX%qHX      = qHX
  FuelCell(Num)%ExhaustHX%THXexh   = THXexh
  FuelCell(Num)%ExhaustHX%WaterMassFlowRate = MdotWater
  FuelCell(Num)%ExhaustHX%WaterVaporFractExh = waterFract

  FuelCell(Num)%ExhaustHX%CondensateRate  =  NdotWaterCond
  FuelCell(Num)%ExhaustHX%WaterOutletTemp =  Twaterout
  FuelCell(Num)%ExhaustHX%WaterOutletEnthalpy = Node(inNodeNum)%Enthalpy + qHX

  ! now update water outlet node Changing to Kg/s!
!  OutNodeNum = FuelCell(Num)%ExhaustHX%WaterOutNode
!  inNodeNum  = FuelCell(Num)%ExhaustHX%WaterInNode
!  Node(OutNodeNum)%Temp = Twaterout
!  Node(OutNodeNum)%Enthalpy =
!  Node(OutNodeNum)%MassFlowRate = MdotWater


RETURN
END SUBROUTINE CalcFuelCellGenHeatRecovery


SUBROUTINE SimFuelCellPlantHeatRecovery(CompType,CompName,CompTypeNum,CompNum,RunFlag,InitLoopEquip,  & !DSU
                          MyLoad,MaxCap,MinCap,OptCap,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! makes sure input are gotten and setup from Plant loop perspective.
          ! does not (re)simulate entire FuelCell model

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE InputProcessor, ONLY: FindItemInList
  USE DataPlant,      ONLY: TypeOf_Generator_FCExhaust, TypeOf_Generator_FCStackCooler
  USE PlantUtilities, ONLY: UpdateComponentHeatRecoverySide

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(IN)          :: CompTypeNum
  INTEGER, INTENT(INOUT)       :: CompNum
  LOGICAL, INTENT(IN)          :: RunFlag
 ! INTEGER, INTENT(IN)          :: FlowLock !DSU
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(INOUT)          :: MyLoad !unused1208
  REAL(r64), INTENT(OUT)            :: MinCap
  REAL(r64), INTENT(OUT)            :: MaxCap
  REAL(r64), INTENT(OUT)            :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  IF (GetFuelCellInput) THEN

    ! Read input data.
    CALL  GetFuelCellGeneratorInput
    GetFuelCellInput=.false.
  ENDIF

  If (InitLoopEquip) Then
    IF (CompTypeNum == TypeOf_Generator_FCExhaust) THEN
      CompNum = FindItemInList(CompName,FuelCell%NameExhaustHX,NumFuelCellGenerators)
    ELSEIF (CompTypeNum == TypeOf_Generator_FCStackCooler) THEN
      CompNum = FindItemInList(CompName,FuelCell%NameStackCooler,NumFuelCellGenerators)
    ENDIF
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimFuelCellPlantHeatRecovery: Fuel Cell Generator Unit not found='//TRIM(CompName))
    ENDIF
    MinCap  = 0.0d0
    MaxCap  = 0.0d0
    OptCap  = 0.0d0
    RETURN
  END IF  ! End Of InitLoopEquip

  IF (CompTypeNum == TypeOf_Generator_FCStackCooler) THEN
    CALL UpdateComponentHeatRecoverySide(FuelCell(CompNum)%CWLoopNum,               &
                                    FuelCell(CompNum)%CWLoopSideNum,           &
                                    TypeOf_Generator_FCStackCooler,            &
                                    FuelCell(CompNum)%StackCooler%WaterInNode,     &
                                    FuelCell(CompNum)%StackCooler%WaterOutNode,    &
                                    FuelCell(CompNum)%Report%qHX,     &
                                    FuelCell(CompNum)%Report%HeatRecInletTemp,  &
                                    FuelCell(CompNum)%Report%HeatRecOutletTemp, &
                                    FuelCell(CompNum)%Report%HeatRecMdot ,  &
                                    FirstHVACIteration)
  ELSEIF (CompTypeNum == TypeOf_Generator_FCExhaust) THEN
    CALL UpdateComponentHeatRecoverySide(FuelCell(CompNum)%CWLoopNum,               &
                                    FuelCell(CompNum)%CWLoopSideNum,           &
                                    TypeOf_Generator_FCExhaust,            &
                                    FuelCell(CompNum)%ExhaustHX%WaterInNode,     &
                                    FuelCell(CompNum)%ExhaustHX%WaterOutNode,    &
                                    FuelCell(CompNum)%Report%qHX,     &
                                    FuelCell(CompNum)%Report%HeatRecInletTemp,  &
                                    FuelCell(CompNum)%Report%HeatRecOutletTemp, &
                                    FuelCell(CompNum)%Report%HeatRecMdot ,  &
                                    FirstHVACIteration)

  ENDIF


  RETURN

END SUBROUTINE SimFuelCellPlantHeatRecovery


! End FuelCell Generator Module Model Subroutines
! *****************************************************************************

! Begin FuelCell Generator Module Utility Subroutines
! *****************************************************************************
SUBROUTINE InitFuelCellGenerators(FCnum )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Sept 2010, plant upgrades

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the FuelCell generators.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataLoopNode ,     ONLY: Node
  USE DataHVACGlobals,   ONLY: SysTimeElapsed, TimeStepSys
  USE DataGlobals    ,   ONLY: TimeStep, TimeStepZone, SecInHour, BeginEnvrnFlag, HourOfDay
  USE PlantUtilities,    ONLY: InitComponentNodes, SetComponentFlowRate
  USE DataPlant,         ONLY: ScanPlantLoopsForObject, PlantLoop, TypeOf_Generator_FCExhaust
  USE FluidProperties,   ONLY: GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: FCnum ! index to specific fuel cell generator

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: InitGeneratorOnce = .TRUE. ! flag for 1 time initialization
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! flag for init once at start of environment
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyWarmupFlag ! flag for init after warmup complete
  INTEGER             :: inNode  ! inlet index in Node array
  INTEGER             :: outNode ! outlet, index in Node array
  REAL(r64)    :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  REAL(r64)   :: mdot ! local temporary mass flow rate
  REAL(r64)   :: rho  ! local temporary fluid density
  LOGICAL     :: errFlag

          ! FLOW:
! Do the one time initializations
IF (InitGeneratorOnce) THEN
  ALLOCATE(MyEnvrnFlag(NumFuelCellGenerators))
  ALLOCATE(MyWarmupFlag(NumFuelCellGenerators))
  ALLOCATE(MyPlantScanFlag(NumFuelCellGenerators))
  MyEnvrnFlag = .TRUE.
  MyWarmupFlag = .FALSE.
  InitGeneratorOnce = .FALSE.
  MyPlantScanFlag = .TRUE.
END IF ! end one time setups and inits

IF (MyPlantScanFlag(FCnum) .AND. ALLOCATED(PlantLoop)) THEN
  errFlag = .FALSE.
  CALL ScanPlantLoopsForObject(FuelCell(FCnum)%NameExhaustHX, &
                                 TypeOf_Generator_FCExhaust, &
                                 FuelCell(FCnum)%CWLoopNum, &
                                 FuelCell(FCnum)%CWLoopSideNum, &
                                 FuelCell(FCnum)%CWBranchNum, &
                                 FuelCell(FCnum)%CWCompNum , &
                                 errFlag = errFlag)
  IF (errFlag) THEN
    CALL ShowFatalError('InitFuelCellGenerators: Program terminated due to previous condition(s).')
  ENDIF
  MyPlantScanFlag(FCnum) = .FALSE.
ENDIF

! Do the Begin Environment initializations
IF (BeginEnvrnFlag .and. MyEnvrnFlag(FCnum) .AND. .NOT. MyPlantScanFlag(FCnum)) THEN

  FuelSupply(FuelCell(FCnum)%FuelSupNum)%PfuelCompEl = 0.d0
  FuelSupply(FuelCell(FCnum)%FuelSupNum)%TfuelIntoFCPM = 0.d0
  FuelSupply(FuelCell(FCnum)%FuelSupNum)%TfuelIntoCompress    = 0.d0
  FuelSupply(FuelCell(FCnum)%FuelSupNum)%QskinLoss = 0.d0

  FuelCell(FCnum)%AirSup%TairIntoFCPM   = 0.d0
  FuelCell(FCnum)%AirSup%PairCompEl     = 0.d0
  FuelCell(FCnum)%AirSup%TairIntoBlower = 0.d0
  FuelCell(FCnum)%AirSup%QskinLoss      = 0.d0
  FuelCell(FCnum)%AirSup%QintakeRecovery = 0.d0
  FuelCell(FCnum)%FCPM%NumCycles  = 0
  FuelCell(FCnum)%FCPM%Pel  = 0.d0
  FuelCell(FCnum)%FCPM%PelLastTimeStep = 0.d0
  FuelCell(FCnum)%FCPM%Eel  = 0.d0
  FuelCell(FCnum)%FCPM%PelancillariesAC = 0.d0
  FuelCell(FCnum)%FCPM%NdotFuel      = 0.d0
  FuelCell(FCnum)%FCPM%TotFuelInEnthalphy = 0.d0
  FuelCell(FCnum)%FCPM%NdotProdGas   = 0.d0
  FuelCell(FCnum)%FCPM%TprodGasLeavingFCPM = 0.d0
  FuelCell(FCnum)%FCPM%TotProdGasEnthalphy = 0.d0
  FuelCell(FCnum)%FCPM%NdotAir       = 0.d0
  FuelCell(FCnum)%FCPM%TotAirInEnthalphy = 0.d0
  FuelCell(FCnum)%FCPM%NdotLiqwater  = 0.d0
  FuelCell(FCnum)%FCPM%TwaterInlet   = 0.d0
  FuelCell(FCnum)%FCPM%WaterInEnthalpy   = 0.d0
  FuelCell(FCnum)%FCPM%TprodGasLeavingFCPM = 200.0d0
  FuelCell(FCnum)%FCPM%FractionalDayofLastStartUp = 0.d0
  FuelCell(FCnum)%FCPM%FractionalDayofLastShutDown = 0.d0
  FuelCell(FCnum)%FCPM%HasBeenOn = .true.
  FuelCell(FCnum)%FCPM%DuringShutDown = .FALSE.
  FuelCell(FCnum)%FCPM%DuringStartUp  = .FALSE.
  FuelCell(FCnum)%WaterSup%TwaterIntoCompress = 0.d0
  FuelCell(FCnum)%WaterSup%TwaterIntoFCPM = 0.d0
  FuelCell(FCnum)%WaterSup%PwaterCompEl  = 0.d0
  FuelCell(FCnum)%WaterSup%QskinLoss     = 0.d0
  FuelCell(FCnum)%AuxilHeat%TauxMix      = 0.d0
  FuelCell(FCnum)%AuxilHeat%NdotAuxMix   = 0.d0
  FuelCell(FCnum)%AuxilHeat%QskinLoss    = 0.d0
  FuelCell(FCnum)%AuxilHeat%QairIntake   = 0.d0
  FuelCell(FCnum)%ExhaustHX%NdotHXleaving = 0.d0
  FuelCell(FCnum)%ExhaustHX%WaterOutletTemp = 0.d0
  FuelCell(FCnum)%ExhaustHX%WaterOutletEnthalpy = 0.d0

  FuelCell(FCnum)%ElecStorage%LastTimeStepStateOfCharge = FuelCell(FCnum)%ElecStorage%StartingEnergyStored
  FuelCell(FCnum)%ElecStorage%ThisTimeStepStateOfCharge = FuelCell(FCnum)%ElecStorage%StartingEnergyStored
  FuelCell(FCnum)%ElecStorage%PelNeedFromStorage = 0.d0
  FuelCell(FCnum)%ElecStorage%IdesiredDischargeCurrent = 0.d0
  FuelCell(FCnum)%ElecStorage%PelFromStorage  = 0.d0
  FuelCell(FCnum)%ElecStorage%IfromStorage  = 0.d0
  FuelCell(FCnum)%ElecStorage%PelIntoStorage  = 0.d0
  FuelCell(FCnum)%ElecStorage%QairIntake   = 0.d0

  FuelCell(FCnum)%Inverter%PCUlosses  = 0.d0
  FuelCell(FCnum)%Inverter%QairIntake = 0.d0

  rho = GetDensityGlycol(PlantLoop(FuelCell(FCnum)%CWLoopNum)%FluidName, &
                         InitHRTemp, &
                         PlantLoop(FuelCell(FCnum)%CWLoopNum)%FluidIndex, &
                         'InitFuelCellGenerators')

  FuelCell(FCnum)%ExhaustHX%WaterMassFlowRateDesign = FuelCell(FCnum)%ExhaustHX%WaterVolumeFlowMax * rho
  FuelCell(FCnum)%ExhaustHX%WaterMassFlowRate       = FuelCell(FCnum)%ExhaustHX%WaterMassFlowRateDesign
  inNode  = FuelCell(FCnum)%ExhaustHX%WaterInNode
  outNode = FuelCell(FCnum)%ExhaustHX%WaterOutNode
  Node(inNode)%Temp  = InitHRTemp
  Node(outNode)%Temp  = InitHRTemp

  CALL InitComponentNodes( 0.d0, FuelCell(FCnum)%ExhaustHX%WaterMassFlowRateDesign, &
                                 inNode, outNode, &
                                 FuelCell(FCnum)%CWLoopNum, &
                                 FuelCell(FCnum)%CWLoopSideNum, &
                                 FuelCell(FCnum)%CWBranchNum, &
                                 FuelCell(FCnum)%CWCompNum )

  MyEnvrnFlag(FCnum) = .FALSE.
  MyWarmupFlag(FCNum) = .TRUE.
END IF ! end environmental inits

IF (.not. BeginEnvrnFlag) THEN
  MyEnvrnFlag(FCnum)=.true.
ENDIF

IF (MyWarmupFlag(FCNum) .AND. (.NOT. WarmUpFlag) ) THEN
  ! need to reset initial state of charge at beginning of environment but after warm up is complete
  FuelCell(FCNum)%ElecStorage%LastTimeStepStateOfCharge = FuelCell(FCNum)%ElecStorage%StartingEnergyStored
  FuelCell(FCNum)%ElecStorage%ThisTimeStepStateOfCharge = FuelCell(FCNum)%ElecStorage%StartingEnergyStored
  MyWarmupFlag(FCNum) = .FALSE.
ENDIF

!using and elapsed time method rather than FirstHVACIteration here
TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed
If (FuelCell(FCnum)%TimeElapsed /= TimeElapsed) Then

  FuelCell(FCnum)%ElecStorage%LastTimeStepStateOfCharge = FuelCell(FCnum)%ElecStorage%ThisTimeStepStateOfCharge
  FuelCell(FCnum)%FCPM%PelLastTimeStep                  = FuelCell(FCnum)%FCPM%Pel


  inNode  = FuelCell(FCnum)%ExhaustHX%WaterInNode
  outNode = FuelCell(FCnum)%ExhaustHX%WaterOutNode
 ! intialize flow rate in water loop, this is "requesting" flow
  mdot = FuelCell(FCnum)%ExhaustHX%WaterMassFlowRateDesign

  Call SetComponentFlowRate( mdot, inNode,  outNode, &
                                 FuelCell(FCnum)%CWLoopNum, &
                                 FuelCell(FCnum)%CWLoopSideNum, &
                                 FuelCell(FCnum)%CWBranchNum, &
                                 FuelCell(FCnum)%CWCompNum )

  FuelCell(FCnum)%ExhaustHX%WaterMassFlowRate = mdot
  FuelCell(FCnum)%ExhaustHX%WaterInletTemp    = Node(inNode)%Temp
  FuelCell(FCnum)%TimeElapsed = TimeElapsed
ELSE
  inNode  = FuelCell(FCnum)%ExhaustHX%WaterInNode

  CALL SetComponentFlowRate(  FuelCell(FCnum)%ExhaustHX%WaterMassFlowRate,  &
                                 FuelCell(FCnum)%ExhaustHX%WaterInNode, &
                                 FuelCell(FCnum)%ExhaustHX%WaterOutNode, &
                                 FuelCell(FCnum)%CWLoopNum, &
                                 FuelCell(FCnum)%CWLoopSideNum, &
                                 FuelCell(FCnum)%CWBranchNum, &
                                 FuelCell(FCnum)%CWCompNum )

  FuelCell(FCnum)%ExhaustHX%WaterInletTemp    = Node(inNode)%Temp
ENDIF

RETURN
END SUBROUTINE InitFuelCellGenerators
! End FuelCell Generator Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the FuelCell Generator Module
! *****************************************************************************
SUBROUTINE FigureFuelCellZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       BG March 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Couple equpment skin losses to the Zone Heat Balance
          ! calculate skin losses from different subsystems and set the value

          ! METHODOLOGY EMPLOYED:
          ! This routine adds up the various skin losses and then
          !  sets the values in the ZoneIntGain structure

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataHeatBalance , ONLY: ZoneIntGain
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused  INTEGER :: thisZone ! index in Zone structure array
  REAL(r64)    :: TotalZoneHeatGain ! working variable for zone gain [w]
!  INTEGER :: ZoneNum
  INTEGER :: FCnum ! number of fuel cell
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.

  IF (NumFuelCellGenerators == 0) RETURN

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    FuelCell%FCPM%HasBeenOn = .FALSE.
    FuelCell%AirSup%PairCompEl = 0.d0
    FuelCell%QconvZone = 0.d0
    FuelCell%QradZone  = 0.d0
    FuelCell%AirSup%QskinLoss = 0.d0
    FuelSupply%QskinLoss = 0.d0
    FuelCell%WaterSup%QskinLoss = 0.d0
    FuelCell%AuxilHeat%QskinLoss = 0.d0
    FuelCell%FCPM%QdotSkin = 0.d0
    FuelCell%Report%SkinLossConvect = 0.d0
    FuelCell%Report%SkinLossRadiat  = 0.d0
    FuelCell%AuxilHeat%QairIntake   = 0.d0
    FuelCell%ElecStorage%QairIntake = 0.d0
    FuelCell%Inverter%QairIntake    = 0.d0
    MyEnvrnFlag = .FALSE.
  END IF

  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

  ! this routine needs to do something for zone gains during sizing

   !first collect skin losses from different subsystems
  Do FCnum =1, NumFuelCellGenerators
    TotalZoneHeatGain =   FuelCell(FCnum)%AirSup%QskinLoss      & ! intake Blower losses to zone
                       + FuelSupply(FuelCell(FCnum)%FuelSupNum)%QskinLoss     & ! fuel compressor losses to zone
                       + FuelCell(FCnum)%WaterSup%QskinLoss    & ! water pump losses to zone
                       + FuelCell(FCnum)%AuxilHeat%QskinLoss   & ! auxil burner losses to zone
                       + FuelCell(FCnum)%FCPM%QdotSkin           ! power module (stack and reformer) losses to zone

    ! now account for other subsystems that may or may not have air intake recovery
    SELECT CASE (FuelCell(FCnum)%AirSup%IntakeRecoveryMode)

    CASE(NoRecoveryOnAirIntake) ! then the heat has to go into zone
      TotalZoneHeatGain = TotalZoneHeatGain + FuelCell(FCnum)%AuxilHeat%QairIntake    &
                                          + FuelCell(FCnum)%ElecStorage%QairIntake  &
                                          + FuelCell(FCnum)%Inverter%QairIntake
    CASE(RecoverAuxiliaryBurner)
      TotalZoneHeatGain = TotalZoneHeatGain + FuelCell(FCnum)%ElecStorage%QairIntake  &
                                          + FuelCell(FCnum)%Inverter%QairIntake

    CASE(RecoverInverterBatt)
      TotalZoneHeatGain = TotalZoneHeatGain + FuelCell(FCnum)%AuxilHeat%QairIntake

    CASE(RecoverInverter)
      TotalZoneHeatGain = TotalZoneHeatGain + FuelCell(FCnum)%AuxilHeat%QairIntake    &
                                          + FuelCell(FCnum)%ElecStorage%QairIntake
    CASE(RecoverBattery)
      TotalZoneHeatGain = TotalZoneHeatGain + FuelCell(FCnum)%AuxilHeat%QairIntake    &
                                          + FuelCell(FCnum)%Inverter%QairIntake

    CASE(RecoverBurnInvertBatt)
      ! do nothing

    END SELECT

    FuelCell(FCnum)%QconvZone = TotalZoneHeatGain * (1 - FuelCell(FCnum)%FCPM%RadiativeFract)
    FuelCell(FCnum)%Report%SkinLossConvect = FuelCell(FCnum)%QconvZone
    FuelCell(FCnum)%QradZone  = TotalZoneHeatGain * FuelCell(FCnum)%FCPM%RadiativeFract
    FuelCell(FCnum)%Report%SkinLossRadiat = FuelCell(FCnum)%QradZone

  ENDDO ! over number of Fuel cells



!  IF(DoingSizing)THEN

!  ENDIF

  RETURN

END SUBROUTINE FigureFuelCellZoneGains

SUBROUTINE UpdateExhaustAirFlows(Num)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Num ! generator number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na



  RETURN

END SUBROUTINE UpdateExhaustAirFlows

SUBROUTINE CalcUpdateHeatRecovery(Num, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update plant loop interactions, do any calcs needed

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SafeCopyPlantNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: Num       ! Generator number
  LOGICAL, INTENT(IN)      :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InNodeNum
  INTEGER :: outnodenum

  ! now update water outlet node Changing to Kg/s!
  OutNodeNum = FuelCell(Num)%ExhaustHX%WaterOutNode
  inNodeNum  = FuelCell(Num)%ExhaustHX%WaterInNode

  CALL SafeCopyPlantNode(InNodeNum, OutNodeNum)

  Node(OutNodeNum)%Temp     = FuelCell(Num)%ExhaustHX%WaterOutletTemp
  Node(OutNodeNum)%Enthalpy = FuelCell(Num)%ExhaustHX%WaterOutletEnthalpy

!  IF (FirstHVACIteration) Then
!    Node(InNodeNum)%MassFlowRateMaxAvail     = FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign
!    Node(InNodeNum)%MassFlowRateMinAvail     = 0.0D0
!    Node(InNodeNum)%MassFlowRate             = Max(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
!                                                   Node(InNodeNum)%MassFlowRateMin)
!    Node(InNodeNum)%MassFlowRate             = Min(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
!                                                   Node(InNodeNum)%MassFlowRateMax)
!
!  ELSE
!    Node(InNodeNum)%MassFlowRate             = Max(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
!                                                   Node(InNodeNum)%MassFlowRateMin)
!    Node(InNodeNum)%MassFlowRate             = Max(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
!                                                   Node(InNodeNum)%MassFlowRateMinAvail)
!    Node(InNodeNum)%MassFlowRate             = Min(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
!                                                   Node(InNodeNum)%MassFlowRateMax)
!    Node(InNodeNum)%MassFlowRate             = Min(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
!                                                   Node(InNodeNum)%MassFlowRateMaxAvail)
!  ENDIF
!
!  Node(OutNodeNum)%MassFlowRate             = Node(InNodeNum)%MassFlowRate
!  Node(OutNodeNum)%MassFlowRateMaxAvail     = Node(InNodeNum)%MassFlowRateMaxAvail
!  Node(OutNodeNum)%MassFlowRateMinAvail     = Node(InNodeNum)%MassFlowRateMinAvail
!  Node(OutNodeNum)%MassFlowRateMax          = Node(InNodeNum)%MassFlowRateMax
!  Node(OutNodeNum)%MassFlowRateMin          = Node(InNodeNum)%MassFlowRateMin

  RETURN

END SUBROUTINE CalcUpdateHeatRecovery

SUBROUTINE UpdateFuelCellGeneratorRecords(RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          BG
            !       DATE WRITTEN:

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if Generator operating
  INTEGER, INTENT(IN)      :: Num       ! Generator number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  FuelCell(Num)%Report%ACPowerGen          = FuelCell(Num)%ACPowerGen !electrical power produced [W]
  FuelCell(Num)%Report%ACEnergyGen         = FuelCell(Num)%ACPowerGen*TimeStepSys*SecInHour ! energy produced (J)
  FuelCell(Num)%Report%QdotExhaust                = 0.0d0 ! reporting: exhaust gas heat recovered (W)
  FuelCell(Num)%Report%TotalHeatEnergyRec         = 0.0d0 ! reporting: total heat recovered (J)
  FuelCell(Num)%Report%ExhaustEnergyRec           = 0.0d0 ! reporting: exhaust gas heat recovered (J)

  FuelCell(Num)%Report%HeatRecInletTemp           = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
  FuelCell(Num)%Report%HeatRecOutletTemp          = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
  FuelCell(Num)%Report%HeatRecMdot                = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)

  FuelCell(Num)%Report%ElectEfficiency   = 0.0d0
  FuelCell(Num)%Report%ThermalEfficiency = 0.0d0
  FuelCell(Num)%Report%OverallEfficiency = 0.0d0
  FuelCell(Num)%Report%ExergyEfficiency  = 0.0d0

  FuelCell(Num)%Report%TairInlet         = FuelCell(Num)%AirSup%TairIntoBlower ! State point 1
  FuelCell(Num)%Report%TairIntoFCPM      = FuelCell(Num)%AirSup%TairIntoFCPM   ! State point 4
  FuelCell(Num)%Report%NdotAir           = FuelCell(Num)%FCPM%NdotAir        ! air flow in kmol/sec
  FuelCell(Num)%Report%TotAirInEnthalphy = FuelCell(Num)%FCPM%TotAirInEnthalphy ! State point 4
  FuelCell(Num)%Report%BlowerPower       = FuelCell(Num)%AirSup%PairCompEl ! electrical power used by air supply blower
  FuelCell(Num)%Report%BlowerEnergy      = FuelCell(Num)%AirSup%PairCompEl*TimeStepSys*SecInHour ! electrical energy
  FuelCell(Num)%Report%BlowerSkinLoss    = FuelCell(Num)%AirSup%QskinLoss ! heat rate of losses by blower

  FuelCell(Num)%Report%TfuelInlet        = FuelSupply(FuelCell(Num)%FuelSupNum)%TfuelIntoCompress    ! State point 2
  FuelCell(Num)%Report%TfuelIntoFCPM     = FuelSupply(FuelCell(Num)%FuelSupNum)%TfuelIntoFCPM ! TEmperature state point 5 [C]
  FuelCell(Num)%Report%NdotFuel          = FuelCell(Num)%FCPM%NdotFuel       ! fuel flow in kmol/sec
  FuelCell(Num)%Report%TotFuelInEnthalpy = FuelCell(Num)%FCPM%TotFuelInEnthalphy ! enthalpy at state point 5 [W]
  FuelCell(Num)%Report%FuelCompressPower = FuelSupply(FuelCell(Num)%FuelSupNum)%PfuelCompEl
                                                       ! electrical power used by fuel supply compressor [W]
  FuelCell(Num)%Report%FuelCompressEnergy = FuelSupply(FuelCell(Num)%FuelSupNum)%PfuelCompEl*TimeStepSys*SecInHour ! elect energy
  FuelCell(Num)%Report%FuelCompressSkinLoss = FuelSupply(FuelCell(Num)%FuelSupNum)%QskinLoss
                                                              !heat rate of losses.by fuel supply compressor [W]
  FuelCell(Num)%Report%FuelEnergyLHV        = FuelCell(Num)%FCPM%NdotFuel * FuelSupply(FuelCell(Num)%FuelSupNum)%LHV &
                                          * 1000000.0d0 *TimeStepSys*SecInHour ! reporting: Fuel Energy used (J)
  FuelCell(Num)%Report%FuelEnergyUseRateLHV = FuelCell(Num)%FCPM%NdotFuel * FuelSupply(FuelCell(Num)%FuelSupNum)%LHV &
                                          * 1000000.0d0 ! reporting: Fuel Energy used (W)
  FuelCell(Num)%Report%FuelEnergyHHV        = FuelCell(Num)%FCPM%NdotFuel * FuelSupply(FuelCell(Num)%FuelSupNum)%HHV &
                                          * FuelSupply(FuelCell(Num)%FuelSupNum)%KmolPerSecToKgPerSec*TimeStepSys*SecInHour

  FuelCell(Num)%Report%FuelEnergyUseRateHHV = FuelCell(Num)%FCPM%NdotFuel * FuelSupply(FuelCell(Num)%FuelSupNum)%HHV &
                                          * FuelSupply(FuelCell(Num)%FuelSupNum)%KmolPerSecToKgPerSec

  FuelCell(Num)%Report%FuelRateMdot         = 0.0d0 ! (Kg/s)

  FuelCell(Num)%Report%TwaterInlet    = FuelCell(Num)%WaterSup%TwaterIntoCompress
  FuelCell(Num)%Report%TwaterIntoFCPM = FuelCell(Num)%WaterSup%TwaterIntoFCPM
  FuelCell(Num)%Report%NdotWater      = FuelCell(Num)%FCPM%NdotLiqwater  ! water flow in kmol/sec (reformer water)
  FuelCell(Num)%Report%WaterPumpPower = FuelCell(Num)%WaterSup%PwaterCompEl
  FuelCell(Num)%Report%WaterPumpEnergy = FuelCell(Num)%WaterSup%PwaterCompEl*TimeStepSys*SecInHour ! electrical energy
  FuelCell(Num)%Report%WaterIntoFCPMEnthalpy = FuelCell(Num)%FCPM%WaterInEnthalpy

  FuelCell(Num)%Report%TprodGas       = FuelCell(Num)%FCPM%TprodGasLeavingFCPM ! temperature at State point 7
  FuelCell(Num)%Report%EnthalProdGas  = FuelCell(Num)%FCPM%TotProdGasEnthalphy ! enthalpy at State point 7
  FuelCell(Num)%Report%NdotProdGas    = FuelCell(Num)%FCPM%NdotProdGas         ! flow rate at point 7 [kmol/sec]
  FuelCell(Num)%Report%NdotProdAr     = FuelCell(Num)%FCPM%ConstitMolalFract(5)  * FuelCell(Num)%FCPM%NdotProdGas
  FuelCell(Num)%Report%NdotProdCO2    = FuelCell(Num)%FCPM%ConstitMolalFract(1)  * FuelCell(Num)%FCPM%NdotProdGas
  FuelCell(Num)%Report%NdotProdH2O    = FuelCell(Num)%FCPM%ConstitMolalFract(4)  * FuelCell(Num)%FCPM%NdotProdGas
  FuelCell(Num)%Report%NdotProdN2     = FuelCell(Num)%FCPM%ConstitMolalFract(2)  * FuelCell(Num)%FCPM%NdotProdGas
  FuelCell(Num)%Report%NdotProdO2     = FuelCell(Num)%FCPM%ConstitMolalFract(3)  * FuelCell(Num)%FCPM%NdotProdGas

  FuelCell(Num)%Report%qHX            = FuelCell(Num)%ExhaustHX%qHX
  FuelCell(Num)%Report%HXenergy       = FuelCell(Num)%ExhaustHX%qHX*TimeStepSys*SecInHour
  FuelCell(Num)%Report%THXexh         = FuelCell(Num)%ExhaustHX%THXexh
  FuelCell(Num)%Report%WaterVaporFractExh =  FuelCell(Num)%ExhaustHX%WaterVaporFractExh
  FuelCell(Num)%Report%CondensateRate = FuelCell(Num)%ExhaustHX%CondensateRate

  FuelCell(Num)%Report%SeqSubstIterations    = FuelCell(Num)%FCPM%SeqSubstitIter  ! number of iterations in FuelCell loop
  FuelCell(Num)%Report%RegulaFalsiIterations = FuelCell(Num)%FCPM%RegulaFalsiIter ! number of iterations in Tproduct gas solving

  FuelCell(Num)%Report%ACancillariesPower  = FuelCell(Num)%FCPM%PelancillariesAC
  FuelCell(Num)%Report%ACancillariesEnergy = FuelCell(Num)%FCPM%PelancillariesAC*TimeStepSys*SecInHour

  FuelCell(Num)%Report%PCULosses    = FuelCell(Num)%Inverter%PCUlosses  ! inverter losses
  FuelCell(Num)%Report%DCPowerGen   = FuelCell(Num)%FCPM%Pel  !DC power out of FCPM.
  FuelCell(Num)%Report%DCPowerEff   = FuelCell(Num)%FCPM%Eel  ! FCPM efficienty Eel.
  FuelCell(Num)%Report%ElectEnergyinStorage = FuelCell(Num)%ElecStorage%ThisTimeStepStateOfCharge
  FuelCell(Num)%Report%StoredPower  = FuelCell(Num)%ElecStorage%PelIntoStorage
  FuelCell(Num)%Report%StoredEnergy = FuelCell(Num)%ElecStorage%PelIntoStorage*TimeStepSys*SecInHour
  FuelCell(Num)%Report%DrawnPower   = FuelCell(Num)%ElecStorage%PelFromStorage
  FuelCell(Num)%Report%DrawnEnergy  = FuelCell(Num)%ElecStorage%PelFromStorage*TimeStepSys*SecInHour

  FuelCell(Num)%Report%SkinLossPower   = FuelCell(Num)%QconvZone + FuelCell(Num)%QradZone
  FuelCell(Num)%Report%SkinLossEnergy  = (FuelCell(Num)%QconvZone + FuelCell(Num)%QradZone)*TimeStepSys*SecInHour
  FuelCell(Num)%Report%SkinLossConvect = FuelCell(Num)%QconvZone
  FuelCell(Num)%Report%SkinLossRadiat  = FuelCell(Num)%QradZone


RETURN
END SUBROUTINE UpdateFuelCellGeneratorRecords

SUBROUTINE GetFuelCellGeneratorResults(GeneratorType, GeneratorIndex, &
                                 GeneratorPower,  GeneratorEnergy, ThermalPower, ThermalEnergy)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provide a get method to collect results at the load center level

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator
  INTEGER, INTENT(IN)           :: GeneratorIndex
  REAL(r64), INTENT(OUT)        :: GeneratorPower  ! electrical power
  REAL(r64), INTENT(OUT)        :: GeneratorEnergy ! electrical energy
  REAL(r64), INTENT(OUT)        :: ThermalPower  ! heat power
  REAL(r64), INTENT(OUT)        :: ThermalEnergy ! heat energy

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  GeneratorPower  =  FuelCell(GeneratorIndex)%Report%ACPowerGen
  GeneratorEnergy =  FuelCell(GeneratorIndex)%Report%ACEnergyGen
  ThermalPower    =  FuelCell(GeneratorIndex)%Report%qHX
  ThermalEnergy   =  FuelCell(GeneratorIndex)%Report%HXenergy

  RETURN

END SUBROUTINE GetFuelCellGeneratorResults




END MODULE FuelCellElectricGenerator

! End of the FuelCell Generator Module
! ****************************************************************************************************

MODULE ICEngineElectricGenerator

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the operation of IC ENGINE Generators.

          ! METHODOLOGY EMPLOYED:
          ! Once the ElectricPowerManager determines that the IC ENGINE Generator
          ! is available to meet an electric load demand, it calls SimICEngineGenerator
          ! which in turn calls the ICEngine Generator model.

          ! REFERENCES:
          ! N/A

          ! OTHER NOTES:
          ! N/A

          ! USE STATEMENTS:
USE DataLoopNode
USE DataGlobals ,   ONLY : MaxNameLength, NumOfTimeStepInHour, SecInHour, BeginEnvrnFlag, InitConvTemp
USE DataInterfaces, ONLY : ShowSevereError, ShowWarningError, ShowFatalError, ShowWarningMessage, &
                           ShowContinueError, SetupOutputVariable,ShowRecurringWarningErrorAtEnd
USE DataGlobalConstants, ONLY: iGeneratorICEngine
USE General, ONLY: RoundSigDigits

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  REAL(r64), PARAMETER   :: ReferenceTemp = 25.0d0 !Reference temperature by which lower heating
                                                   ! value is reported.  This should be subtracted
                                                   ! off of when calculated exhaust energies.


  ! DERIVED TYPE DEFINITIONS

TYPE ICEngineGeneratorSpecs
       CHARACTER(len=MaxNameLength) :: Name           = ' ' ! user identifier
       CHARACTER(len=MaxNameLength) :: TypeOf         = 'Generator:InternalCombustionEngine' ! Type of Generator
       INTEGER                      :: CompType_Num   = iGeneratorICEngine
       CHARACTER(len=MaxNameLength) :: FuelType       = ' ' ! Type of Fuel - DIESEL, GASOLINE, GAS
       REAL(r64)         :: RatedPowerOutput          = 0.0d0 ! W - design nominal capacity of Generator
       INTEGER           :: ElectricCircuitNode       = 0   ! Electric Circuit Node
       REAL(r64)         :: MinPartLoadRat            = 0.0d0 ! (IC ENGINE MIN) min allowed operating frac full load
       REAL(r64)         :: MaxPartLoadRat            = 0.0d0 ! (IC ENGINE MAX) max allowed operating frac full load
       REAL(r64)         :: OptPartLoadRat            = 0.0d0 ! (IC ENGINE BEST) optimal operating frac full load
       REAL(r64)         :: ElecOutputFuelRat         = 0.0d0 !(RELDC) Ratio of Generator output to Fuel Energy Input
       INTEGER           :: ElecOutputFuelCurve       = 0   !Curve Index for generator output to Fuel Energy Input Coeff Poly Fit
       REAL(r64)         :: RecJacHeattoFuelRat       = 0.0d0 !(RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
       INTEGER           :: RecJacHeattoFuelCurve     = 0   !Curve Index for Ratio of Recoverable Jacket Heat to
                                                            ! Fuel Energy Input Coeff Poly Fit
       REAL(r64)         :: RecLubeHeattoFuelRat      = 0.0d0 !(RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
       INTEGER           :: RecLubeHeattoFuelCurve    = 0   !Curve Index for Ratio of Recoverable Lube Oil Heat to
                                                            ! Fuel Energy Input Coef Poly Fit
       REAL(r64)         :: TotExhausttoFuelRat       = 0.0d0 !(REXDC) Total Exhaust heat Input to Fuel Energy Input
       INTEGER           :: TotExhausttoFuelCurve     = 0   !Curve Index for Total Exhaust heat Input to Fuel Energy Input
                                                            ! Coeffs Poly Fit
       REAL(r64)         :: ExhaustTemp               = 0.0d0 !(TEXDC) Exhaust Gas Temp to Fuel Energy Input
       INTEGER           :: ExhaustTempCurve          = 0   !Curve Index for Exhaust Gas Temp to Fuel Energy Input Coeffs Poly Fit
       INTEGER           :: ErrExhaustTempIndex       = 0   ! error index for temp curve
       REAL(r64)         :: UA                        = 0.0d0 !(UACDC) exhaust gas Heat Exchanger UA to Capacity
       REAL(r64),DIMENSION(2) :: UACoef                    = 0.0d0 !Heat Exchanger UA Coeffs Poly Fit
       REAL(r64)         :: MaxExhaustperPowerOutput  = 0.0d0 !MAX EXHAUST FLOW PER W DSL POWER OUTPUT COEFF
       REAL(r64)         :: DesignMinExitGasTemp      = 0.0d0 !Steam Saturation Temperature
       REAL(r64)         :: FuelHeatingValue          = 0.0d0 ! Heating Value of Fuel in kJ/kg
       REAL(r64)         :: DesignHeatRecVolFlowRate  = 0.0d0 ! m3/s, Design Water mass flow rate through heat recovery loop
       REAL(r64)         :: DesignHeatRecMassFlowRate = 0.0d0 ! kg/s, Design Water mass flow rate through heat recovery loop
       LOGICAL           :: HeatRecActive             = .false. ! True if Heat Rec Design Vol Flow Rate > 0
       INTEGER           :: HeatRecInletNodeNum       = 0   ! Node number on the heat recovery inlet side of the condenser
       INTEGER           :: HeatRecOutletNodeNum      = 0   ! Node number on the heat recovery outlet side of the condenser
       REAL(r64)         :: HeatRecInletTemp          = 0.0d0 ! Inlet Temperature of the heat recovery fluid
       REAL(r64)         :: HeatRecOutletTemp         = 0.0d0 ! Outlet Temperature of the heat recovery fluid
       REAL(r64)         :: HeatRecMdotDesign         = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate
       REAL(r64)         :: HeatRecMdotActual         = 0.0d0 !
       REAL(r64)         :: QTotalHeatRecovered       = 0.0d0 ! total heat recovered (W)
       REAL(r64)         :: QJacketRecovered          = 0.0d0 ! heat recovered from jacket (W)
       REAL(r64)         :: QLubeOilRecovered         = 0.0d0 ! heat recovered from lube (W)
       REAL(r64)         :: QExhaustRecovered         = 0.0d0 ! exhaust gas heat recovered (W)
       REAL(r64)         :: FuelEnergyUseRate         = 0.0d0 ! Fuel Energy used (W)
       REAL(r64)         :: TotalHeatEnergyRec        = 0.0d0 ! total heat recovered (J)
       REAL(r64)         :: JacketEnergyRec           = 0.0d0 ! heat recovered from jacket (J)
       REAL(r64)         :: LubeOilEnergyRec          = 0.0d0 ! heat recovered from lube (J)
       REAL(r64)         :: ExhaustEnergyRec          = 0.0d0 ! exhaust gas heat recovered (J)
       REAL(r64)         :: FuelEnergy                = 0.0d0 ! Fuel Energy used (J)
       REAL(r64)         :: FuelMdot                  = 0.0d0 ! Fuel Amount used (Kg/s)
       REAL(r64)         :: ExhaustStackTemp          = 0.0d0 ! Exhaust Stack Temperature (C)
       REAL(r64)         :: ElecPowerGenerated        = 0.0d0 ! Electric Power Generated (W)
       REAL(r64)         :: ElecEnergyGenerated       = 0.0d0 ! Amount of Electric Energy Generated (J)
       REAL(r64)         :: HeatRecMaxTemp            = 0.0d0 !Max Temp that can be produced in heat recovery
       INTEGER           :: HRLoopNum                 = 0   ! cooling water plant loop index number, for heat recovery
       INTEGER           :: HRLoopSideNum             = 0   ! cooling water plant loop side index, for heat recovery
       INTEGER           :: HRBranchNum               = 0   ! cooling water plant loop branch index, for heat recovery
       INTEGER           :: HRCompNum                 = 0   ! cooling water plant loop component index, for heat recovery

END TYPE ICEngineGeneratorSpecs

TYPE ReportVars
  REAL(r64)    :: PowerGen                   = 0.0d0 ! reporting: power (W)
  REAL(r64)    :: EnergyGen                  = 0.0d0 ! reporting: energy (J)
  REAL(r64)    :: QJacketRecovered           = 0.0d0 ! reporting: Heat Recovered from Jacket (W)
  REAL(r64)    :: QLubeOilRecovered          = 0.0d0 ! reporting: Heat Recovered from Lubricant (W)
  REAL(r64)    :: QExhaustRecovered          = 0.0d0 ! reporting: exhaust gas heat recovered (W)
  REAL(r64)    :: QTotalHeatRecovered        = 0.0d0 ! reporting: Total Heat Recovered (W)
  REAL(r64)    :: TotalHeatEnergyRec         = 0.0d0 ! reporting: total heat recovered (J)
  REAL(r64)    :: JacketEnergyRec            = 0.0d0 ! reporting: heat recovered from jacket (J)
  REAL(r64)    :: LubeOilEnergyRec           = 0.0d0 ! reporting: heat recovered from lube (J)
  REAL(r64)    :: ExhaustEnergyRec           = 0.0d0 ! reporting: exhaust gas heat recovered (J)
  REAL(r64)    :: FuelEnergy                 = 0.0d0 ! reporting: Fuel Energy used (J)
  REAL(r64)    :: FuelEnergyUseRate          = 0.0d0 ! reporting: Fuel Energy used (W)
  REAL(r64)    :: FuelMdot                   = 0.0d0 ! reporting: Fuel used (Kg/s)
  REAL(r64)    :: ExhaustStackTemp           = 0.0d0 ! reporting: Exhaust Stack Temperature (C)
  REAL(r64)    :: HeatRecInletTemp           = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
  REAL(r64)    :: HeatRecOutletTemp          = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
  REAL(r64)    :: HeatRecMdot                = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)
END TYPE ReportVars

          ! MODULE VARIABLE DECLARATIONS:
TYPE (ICEngineGeneratorSpecs),    ALLOCATABLE, DIMENSION(:) :: ICEngineGenerator  !dimension to number of machines
TYPE (ReportVars),              ALLOCATABLE, DIMENSION(:) :: ICEngineGeneratorReport
INTEGER  :: NumICEngineGenerators=0 ! number of IC ENGINE Generators specified in input
LOGICAL  :: GetICEInput = .TRUE.   ! When TRUE, calls subroutine to read input file.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
          ! SUBROUTINE SPECIFICATIONS FOR MODULE IC ENGINEElectricGenerator

PUBLIC     SimICEngineGenerator
PUBLIC     GetICEGeneratorResults
PUBLIC     SimICEPlantHeatRecovery
PRIVATE    GetICEngineGeneratorInput
PRIVATE    CalcICEngineGeneratorModel
PRIVATE    CalcICEngineGenHeatRecovery
PRIVATE    InitICEngineGenerators
PRIVATE    UpdateICEngineGeneratorRecords

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of IC ENGINE Generator Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimICEngineGenerator(GeneratorType,GeneratorName,GeneratorIndex,RunFlag, MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the IC ENGINE Generator model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GeneratorType     ! type of Generator
  CHARACTER(len=*), INTENT(IN) :: GeneratorName     ! user specified name of Generator
  INTEGER, INTENT(INOUT) :: GeneratorIndex
  LOGICAL , INTENT(IN)   :: RunFlag                 ! simulate Generator when TRUE
  REAL(r64), INTENT(IN)       :: MyLoad                  ! demand on electric generator
  LOGICAL, INTENT (IN)   :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: GenNum           ! Generator number counter


        !Get Generator data from input file
  IF (GetICEInput) THEN
    CALL GetICEngineGeneratorInput
    GetICEInput = .FALSE.
  END IF


        !SELECT and CALL MODELS
  IF (GeneratorIndex == 0) THEN
    GenNum = FindItemInList(GeneratorName,ICEngineGenerator%Name,NumICEngineGenerators)
    IF (GenNum == 0) CALL ShowFatalError('SimICEngineGenerator: Specified Generator not one of Valid ICEngine Generators '//  &
                                       TRIM(GeneratorName))
    GeneratorIndex=GenNum
  ELSE
    GenNum=GeneratorIndex
    IF (GenNum > NumICEngineGenerators .or. GenNum < 1) THEN
      CALL ShowFatalError('SimICEngineGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                          ', Number of IC Engine Generators='//TRIM(TrimSigDigits(NumICEngineGenerators))//  &
                          ', Generator name='//TRIM(GeneratorName))
    ENDIF
    IF (CheckEquipName(GenNum)) THEN
      IF (GeneratorName /= ICEngineGenerator(GenNum)%Name) THEN
        CALL ShowFatalError('SimICEngineGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                            ', Generator name='//TRIM(GeneratorName)//', stored Generator Name for that index='//  &
                            TRIM(ICEngineGenerator(GenNum)%Name))
      ENDIF
      CheckEquipName(GenNum)=.false.
    ENDIF
  ENDIF

  CALL InitICEngineGenerators(GenNum,RunFlag,MyLoad,FirstHVACIteration)
  CALL CalcICEngineGeneratorModel(GenNum,RunFlag,MyLoad,FirstHVACIteration)
  CALL UpdateICEngineGeneratorRecords(RunFlag,GenNum)

RETURN
END SUBROUTINE SimICEngineGenerator

SUBROUTINE GetICEGeneratorResults(GeneratorType, GeneratorIndex, &
                                 GeneratorPower,  GeneratorEnergy, ThermalPower, ThermalEnergy)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator
  INTEGER, INTENT(IN)           :: GeneratorIndex
  REAL(r64), INTENT(OUT)        :: GeneratorPower  ! electrical power
  REAL(r64), INTENT(OUT)        :: GeneratorEnergy ! electrical energy
  REAL(r64), INTENT(OUT)        :: ThermalPower  ! heat power
  REAL(r64), INTENT(OUT)        :: ThermalEnergy ! heat energy

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  GeneratorPower  =  ICEngineGeneratorReport(GeneratorIndex)%PowerGen
  GeneratorEnergy =  ICEngineGeneratorReport(GeneratorIndex)%EnergyGen
  ThermalPower    =  ICEngineGeneratorReport(GeneratorIndex)%QTotalHeatRecovered
  ThermalEnergy   =  ICEngineGeneratorReport(GeneratorIndex)%TotalHeatEnergyRec

  RETURN

END SUBROUTINE GetICEGeneratorResults

SUBROUTINE SimICEPlantHeatRecovery(CompType,CompName,CompTypeNum,CompNum,RunFlag,InitLoopEquip,  &  !DSU
                          MyLoad,MaxCap,MinCap,OptCap,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill data needed in PlantLoopEquipments

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY: UpdateComponentHeatRecoverySide
  USE DataPlant,      ONLY: TypeOf_Generator_ICEngine

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(IN)          :: CompTypeNum
  INTEGER, INTENT(INOUT)       :: CompNum
  LOGICAL, INTENT(IN)          :: RunFlag
  !INTEGER, INTENT(IN)          :: FlowLock !DSU
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(INOUT)     :: MyLoad
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetICEInput) THEN
    CALL GetICEngineGeneratorInput
    GetICEInput = .FALSE.
  END IF

  If (InitLoopEquip) Then
    CompNum = FindItemInList(CompName, ICEngineGenerator%name, NumICEngineGenerators)
    If (CompNum == 0) Then
      CALL ShowFatalError('SimICEPlantHeatRecovery: ICE Generator Unit not found='//TRIM(CompName))
      RETURN
    ENDIF
      MinCap  = 0.0d0
      MaxCap  = 0.0d0
      OptCap  = 0.0d0
      RETURN
  ENDIF ! End Of InitLoopEquip


  CALL UpdateComponentHeatRecoverySide(ICEngineGenerator(CompNum)%HRLoopNum,               &
                                    ICEngineGenerator(CompNum)%HRLoopSideNum,           &
                                    TypeOf_Generator_ICEngine,                           &
                                    ICEngineGenerator(CompNum)%HeatRecInletNodeNum,     &
                                    ICEngineGenerator(CompNum)%HeatRecOutletNodeNum,    &
                                    ICEngineGeneratorReport(CompNum)%QTotalHeatRecovered,     &
                                    ICEngineGeneratorReport(CompNum)%HeatRecInletTemp,  &
                                    ICEngineGeneratorReport(CompNum)%HeatRecOutletTemp, &
                                    ICEngineGeneratorReport(CompNum)%HeatRecMdot ,  &
                                    FirstHVACIteration)

  RETURN

END SUBROUTINE SimICEPlantHeatRecovery


! End IC ENGINE Generator Module Driver Subroutines
!******************************************************************************


! Beginning of IC ENGINE Generator Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetICEngineGeneratorInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    Sept. 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the IC ENGINE Generator models.

            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE CurveManager,   ONLY : GetCurveIndex, CurveValue
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  Use General, Only:  RoundSigDigits
  Use PlantUtilities, ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: GeneratorNum !Generator counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(10)  :: AlphArray !character string data
  REAL(r64),                   DIMENSION(11)  :: NumArray  !numeric data
  LOGICAL, SAVE :: ErrorsFound=.false.  ! error flag
  LOGICAL       :: IsNotOK              ! Flag to verify name
  LOGICAL       :: IsBlank              ! Flag for blank name
  REAL(r64) :: xValue ! test curve limits

         !FLOW
  cCurrentModuleObject = 'Generator:InternalCombustionEngine'
  NumICEngineGenerators = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumICEngineGenerators <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

         !ALLOCATE ARRAYS
  ALLOCATE (ICEngineGenerator(NumICEngineGenerators))
  ALLOCATE(CheckEquipName(NumICEngineGenerators))
  CheckEquipName=.true.

  ALLOCATE (ICEngineGeneratorReport(NumICEngineGenerators))


         !LOAD ARRAYS WITH IC ENGINE Generator CURVE FIT  DATA
  DO GeneratorNum = 1 , NumICEngineGenerators
    CALL GetObjectItem(cCurrentModuleObject,GeneratorNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),ICEngineGenerator%Name,GeneratorNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    ICEngineGenerator(GeneratorNum)%Name                = AlphArray(1)

    ICEngineGenerator(GeneratorNum)%RatedPowerOutput    = NumArray(1)
    IF (NumArray(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(NumArray(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound=.true.
    ENDIF

           ! Not sure what to do with electric nodes, so do not use optional arguments
    ICEngineGenerator(GeneratorNum)%ElectricCircuitNode    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Electric,NodeConnectionType_Electric,1,ObjectIsNotParent)


    ICEngineGenerator(GeneratorNum)%MinPartLoadRat      = NumArray(2)
    ICEngineGenerator(GeneratorNum)%MaxPartLoadRat      = NumArray(3)
    ICEngineGenerator(GeneratorNum)%OptPartLoadRat      = NumArray(4)

!Load Special IC ENGINE Generator Curve Fit Inputs
    ICEngineGenerator(GeneratorNum)%ElecOutputFuelCurve = GetCurveIndex(AlphArray(3)) ! convert curve name to number
    IF (ICEngineGenerator(GeneratorNum)%ElecOutputFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(AlphArray(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    ICEngineGenerator(GeneratorNum)%RecJacHeattoFuelCurve = GetCurveIndex(AlphArray(4)) ! convert curve name to number
    IF (ICEngineGenerator(GeneratorNum)%RecJacHeattoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(AlphArray(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    ICEngineGenerator(GeneratorNum)%RecLubeHeattoFuelCurve = GetCurveIndex(AlphArray(5)) ! convert curve name to number
    IF (ICEngineGenerator(GeneratorNum)%RecLubeHeattoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(AlphArray(5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    ICEngineGenerator(GeneratorNum)%TotExhausttoFuelCurve = GetCurveIndex(AlphArray(6)) ! convert curve name to number
    IF (ICEngineGenerator(GeneratorNum)%TotExhausttoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(AlphArray(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    ICEngineGenerator(GeneratorNum)%ExhaustTempCurve = GetCurveIndex(AlphArray(7)) ! convert curve name to number
    IF (ICEngineGenerator(GeneratorNum)%ExhaustTempCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphArray(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ELSE
      xValue = CurveValue(ICEngineGenerator(GeneratorNum)%ExhaustTempCurve, 1.0d0)
      IF (xValue < ReferenceTemp) THEN
        CALL ShowSevereError('GetICEngineGeneratorInput: '//trim(cAlphaFieldNames(7))//' output has very low value.')
        CALL ShowContinueError('...curve generates ['//trim(RoundSigDigits(xValue,3))//' C] at PLR=1.0')
        CALL ShowContinueError('...this is less than the Reference Temperature ['//trim(RoundSigDigits(ReferenceTemp,2))//  &
           ' C] and may cause errors.')
      ENDIF
    END IF

    ICEngineGenerator(GeneratorNum)%UACoef(1) = NumArray(5)
    ICEngineGenerator(GeneratorNum)%UACoef(2) = NumArray(6)

    ICEngineGenerator(GeneratorNum)%MaxExhaustperPowerOutput = NumArray(7)
    ICEngineGenerator(GeneratorNum)%DesignMinExitGasTemp = NumArray(8)
    ICEngineGenerator(GeneratorNum)%FuelHeatingValue = NumArray(9)
    ICEngineGenerator(GeneratorNum)%DesignHeatRecVolFlowRate = NumArray(10)
    IF (ICEngineGenerator(GeneratorNum)%DesignHeatRecVolFlowRate > 0.0d0) THEN
      ICEngineGenerator(GeneratorNum)%HeatRecActive=.true.
      ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(AlphArray(8),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      IF (ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(AlphArray(8)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(9),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      IF (ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(AlphArray(9)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(8),AlphArray(9),'Heat Recovery Nodes')
      Call RegisterPlantCompDesignFlow(ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum, &
                                ICEngineGenerator(GeneratorNum)%DesignHeatRecVolFlowRate )
    ELSE
      ICEngineGenerator(GeneratorNum)%HeatRecActive=.false.
      ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum   = 0
      ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum   = 0
      IF (.NOT. lAlphaFieldBlanks(8) .OR. .NOT. lAlphaFieldBlanks(9) ) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '// &
                              TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('However, Node names were specified for Heat Recovery inlet or outlet nodes')
      ENDIF
    ENDIF

    !Fuel Type Case Statement
    SELECT CASE (AlphArray(10))
    CASE ('  ') !If blank then the default is Diesel
      ICEngineGenerator(GeneratorNum)%FuelType = 'Diesel'

    CASE ('GAS','NATURALGAS','NATURAL GAS')
      ICEngineGenerator(GeneratorNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      ICEngineGenerator(GeneratorNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      ICEngineGenerator(GeneratorNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       ICEngineGenerator(GeneratorNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       ICEngineGenerator(GeneratorNum)%FuelType = 'FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
       ICEngineGenerator(GeneratorNum)%FuelType = 'Propane'

    CASE ('OTHERFUEL1')
       ICEngineGenerator(GeneratorNum)%FuelType = 'OtherFuel1'

    CASE ('OTHERFUEL2')
       ICEngineGenerator(GeneratorNum)%FuelType = 'OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(AlphArray(10)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound=.true.
    END SELECT


    ICEngineGenerator(GeneratorNum)%HeatRecMaxTemp = NumArray(11)


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO GeneratorNum = 1, NumICEngineGenerators
     CALL SetupOutputVariable('Generator Produced Electric Power [W]', &
          ICEngineGeneratorReport(GeneratorNum)%PowerGen,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Produced Electric Energy [J]', &
          ICEngineGeneratorReport(GeneratorNum)%EnergyGen,'System','Sum',ICEngineGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')

     CALL SetupOutputVariable('Generator '// TRIM(ICEngineGenerator(GeneratorNum)%FuelType)//' Rate [W]', &
          ICEngineGeneratorReport(GeneratorNum)%FuelEnergyUseRate,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator '// TRIM(ICEngineGenerator(GeneratorNum)%FuelType)//' Energy [J]', &
          ICEngineGeneratorReport(GeneratorNum)%FuelEnergy,'System','Sum',ICEngineGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey=ICEngineGenerator(GeneratorNum)%FuelType,EndUseKey='COGENERATION',GroupKey='Plant')

!    general fuel use report to match other generators.
     CALL SetupOutputVariable('Generator Fuel HHV Basis Rate [W]', &
          ICEngineGeneratorReport(GeneratorNum)%FuelEnergyUseRate,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Fuel HHV Basis Energy [J]', &
          ICEngineGeneratorReport(GeneratorNum)%FuelEnergy,'System','Sum',ICEngineGenerator(GeneratorNum)%Name )

     CALL SetupOutputVariable('Generator '// TRIM(ICEngineGenerator(GeneratorNum)%FuelType)//' Mass Flow Rate [kg/s]', &
          ICEngineGeneratorReport(GeneratorNum)%FuelMdot,'System','Average',ICEngineGenerator(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator Exhaust Air Temperature [C]', &
          ICEngineGeneratorReport(GeneratorNum)%ExhaustStackTemp,'System','Average',ICEngineGenerator(GeneratorNum)%Name)


     IF (ICEngineGenerator(GeneratorNum)%HeatRecActive) THEN
       CALL SetupOutputVariable('Generator Heat Recovery Mass Flow Rate [kg/s]', &
            ICEngineGeneratorReport(GeneratorNum)%HeatRecMdot,'System','Average',ICEngineGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Jacket Heat Recovery Rate [W]', &
            ICEngineGeneratorReport(GeneratorNum)%QJacketRecovered,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Jacket Heat Recovery Energy [J]', &
            ICEngineGeneratorReport(GeneratorNum)%JacketEnergyRec,'System','Sum',ICEngineGenerator(GeneratorNum)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Generator Lube Heat Recovery Rate [W]', &
            ICEngineGeneratorReport(GeneratorNum)%QLubeOilRecovered,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Lube Heat Recovery Energy [J]', &
            ICEngineGeneratorReport(GeneratorNum)%LubeOilEnergyRec,'System','Sum',ICEngineGenerator(GeneratorNum)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Generator Exhaust Heat Recovery Rate [W]', &
            ICEngineGeneratorReport(GeneratorNum)%QExhaustRecovered,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Exhaust Heat Recovery Energy [J]', &
            ICEngineGeneratorReport(GeneratorNum)%ExhaustEnergyRec,'System','Sum',ICEngineGenerator(GeneratorNum)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Generator Produced Thermal Rate [W]', &
            ICEngineGeneratorReport(GeneratorNum)%QTotalHEatRecovered,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', &
            ICEngineGeneratorReport(GeneratorNum)%TotalHeatEnergyRec,'System','Sum',ICEngineGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Heat Recovery Inlet Temperature [C]', &
          ICEngineGeneratorReport(GeneratorNum)%HeatRecInletTemp,'System','Average',ICEngineGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Heat Recovery Outlet Temperature [C]', &
          ICEngineGeneratorReport(GeneratorNum)%HeatRecOutletTemp,'System','Average',ICEngineGenerator(GeneratorNum)%Name)
    ENDIF

  END DO


RETURN
END SUBROUTINE GetICEngineGeneratorInput

! End of Get Input subroutines for the IC ENGINE Generator Module
!******************************************************************************


! Beginning of Generator model Subroutines
! *****************************************************************************

SUBROUTINE CalcICEngineGeneratorModel(GeneratorNum,RunFlag,MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED     na
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a IC ENGINE generator using the BLAST model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag,TimeStepSys
  USE CurveManager,    ONLY : CurveValue
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop
  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)     :: RunFlag         ! TRUE when Generator operating
  REAL(r64)  , INTENT(IN) :: myload          ! Generator demand
  LOGICAL, INTENT(IN)     :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER   :: ExhaustCP = 1.047d0    !Exhaust Gas Specific Heat (J/kg-K)
  REAL(r64), PARAMETER   :: KJtoJ = 1000.d0        !convert Kjoules to joules


          ! DERIVED TYPE DEFINITIONS


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MinPartLoadRat      ! min allowed operating frac full load
  REAL(r64) :: MaxPartLoadRat      ! max allowed operating frac full load
  REAL(r64) :: PLR                 ! Generator operating part load ratio
  REAL(r64) :: RatedPowerOutput    ! Generator nominal capacity (W)
  REAL(r64) :: ElecPowerGenerated  ! Generator output (W)
  REAL(r64) :: ElectricEnergyGen   ! Generator output (J)

! Special variables for IC ENGINE Generator
  REAL(r64) :: MaxExhaustperPowerOutput !curve fit parameter
  REAL(r64) :: ElecOutputFuelRat      !(RELDC) Ratio of generator output to Fuel Energy Input
  REAL(r64) :: RecJacHeattoFuelRat  !(RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
  REAL(r64) :: RecLubeHeattoFuelRat !(RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
  REAL(r64) :: TotExhausttoFuelRat  !(REXDC) Total Exhaust Energy Input to Fuel Energy Input
  REAL(r64) :: ExhaustTemp          !(TEX) Exhaust Gas Temp
  REAL(r64) :: UA                   !(UACDC) exhaust gas Heat Exchanger UA
  REAL(r64) :: FuelEnergyUseRate    ! IC ENGINE fuel use rate (W)
  REAL(r64) :: FuelEnergyUsed       ! IC ENGINE fuel use (J)
  REAL(r64) :: QTotalHeatRecovered
  REAL(r64) :: QJacketRec                 ! water jacket heat recovered (W)
  REAL(r64) :: QLubeOilRec                ! lube oil cooler heat recovered (W)
  REAL(r64) :: QExhaustRec                ! exhaust gas heat recovered (W)
  REAL(r64) :: JacketEnergyRec            ! water jacket heat recovered (J)
  REAL(r64) :: LubeOilEnergyRec           ! lube oil cooler heat recovered (J)
  REAL(r64) :: ExhaustEnergyRec           ! exhaust gas heat recovered (J)
  REAL(r64) :: QExhaustTotal   ! total engine exhaust heat (W)
  REAL(r64) :: ExhaustGasFlow       ! exhaust gas mass flow rate (kg/s)
  REAL(r64) :: ExhaustStackTemp     ! engine stack temp. (C)
  REAL(r64) :: DesignMinExitGasTemp   ! design engine stact saturated steam temp. (C)
  REAL(r64) :: FuelHeatingValue     !Heating Value of Fuel in kJ/kg
  INTEGER :: HeatRecInNode        !Heat Recovery Fluid Inlet Node Num
  REAL(r64) :: HeatRecInTemp        !Heat Recovery Fluid Inlet Temperature (C)
  REAL(r64) :: HeatRecMdot          !Heat Recovery Fluid Mass FlowRate (kg/s)
  REAL(r64) :: HeatRecCp            !Specific Heat of the Heat Recovery Fluid (J/kg-K)
  REAL(r64) :: HRecRatio              !When Max Temp is reached the amount of recovered heat has to be reduced.
                                    ! and this assumption uses this ratio to accomplish this task.



    ! LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  MinPartLoadRat    = ICEngineGenerator(GeneratorNum)%MinPartLoadRat
  MaxPartLoadRat    = ICEngineGenerator(GeneratorNum)%MaxPartLoadRat
  RatedPowerOutput  = ICEngineGenerator(GeneratorNum)%RatedPowerOutput
  MaxExhaustperPowerOutput  = ICEngineGenerator(GeneratorNum)%MaxExhaustperPowerOutput
  IF (ICEngineGenerator(GeneratorNum)%HeatRecActive) THEN
    HeatRecInNode     = ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum
    HeatRecInTemp = Node(HeatRecInNode)%Temp
    HeatRecCp = GetSpecificHeatGlycol(PlantLoop(ICEngineGenerator(GeneratorNum)%HRLoopNum)%FluidName, &
                                 HeatRecInTemp, &
                                 PlantLoop(ICEngineGenerator(GeneratorNum)%HRLoopNum)%FluidIndex, &
                                 'CalcICEngineGeneratorModel')
    HeatRecMdot = Node(HeatRecInNode)%MassFlowRate

  ELSE
    HeatRecInTemp = 0.0d0
    HeatRecMdot   = 0.0d0
  ENDIF

        !If no loop demand or Generator OFF, return
  IF (.NOT. Runflag) THEN
    ICEngineGenerator(GeneratorNum)%ElecPowerGenerated   = 0.0d0
    ICEngineGenerator(GeneratorNum)%ElecEnergyGenerated  = 0.0d0
    ICEngineGenerator(GeneratorNum)%HeatRecInletTemp     = HeatRecInTemp
    ICEngineGenerator(GeneratorNum)%HeatRecOutletTemp    = HeatRecInTemp
    ICEngineGenerator(GeneratorNum)%HeatRecMdotActual    = 0.0d0
    ICEngineGenerator(GeneratorNum)%QJacketRecovered     = 0.0d0
    ICEngineGenerator(GeneratorNum)%QExhaustRecovered    = 0.0d0
    ICEngineGenerator(GeneratorNum)%QLubeOilRecovered    = 0.0d0
    ICEngineGenerator(GeneratorNum)%QTotalHeatRecovered  = 0.0d0
    ICEngineGenerator(GeneratorNum)%JacketEnergyRec      = 0.0d0
    ICEngineGenerator(GeneratorNum)%ExhaustEnergyRec     = 0.0d0
    ICEngineGenerator(GeneratorNum)%LubeOilEnergyRec     = 0.0d0
    ICEngineGenerator(GeneratorNum)%TotalHeatEnergyRec   = 0.0d0
    ICEngineGenerator(GeneratorNum)%FuelEnergyUseRate    = 0.0d0
    ICEngineGenerator(GeneratorNum)%FuelEnergy           = 0.0d0
    ICEngineGenerator(GeneratorNum)%FuelMdot             = 0.0d0
    ICEngineGenerator(GeneratorNum)%ExhaustStackTemp     = 0.0d0

    RETURN
  END IF


     ! CALCULATE POWER GENERATED AND PLR
  ElecPowerGenerated = MIN(MyLoad,RatedPowerOutput)
  ElecPowerGenerated = MAX(ElecPowerGenerated,0.0d0)
  PLR = MIN(ElecPowerGenerated/RatedPowerOutput, MaxPartLoadRat)
  PLR = MAX(PLR, MinPartLoadRat)
  ElecPowerGenerated = PLR*RatedPowerOutput

        !DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

!Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
!energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
    IF (PLR > 0.0d0)THEN
      ElecOutputFuelRat = CurveValue(ICEngineGenerator(GeneratorNum)%ElecOutputFuelCurve, PLR)
      FuelEnergyUseRate = ElecPowerGenerated / ElecOutputFuelRat
    ELSE
      FuelEnergyUseRate = 0.0d0
    END IF

!Use Curve fit to determine heat recovered in the water jacket.  This curve calculates the water jacket heat recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
!particular part load.

      RecJacHeattoFuelRat = CurveValue(ICEngineGenerator(GeneratorNum)%RecJacHeattoFuelCurve, PLR)
      QJacketRec = FuelEnergyUseRate * RecJacHeattoFuelRat

!Use Curve fit to determine Heat Recovered Lubricant heat.  This curve calculates the lube heat recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
!particular part load.
      RecLubeHeattoFuelRat = CurveValue(ICEngineGenerator(GeneratorNum)%RecLubeHeattoFuelCurve, PLR)
      QLubeOilRec = FuelEnergyUseRate * RecLubeHeattoFuelRat

!Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  heat recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
!particular part load.
      TotExhausttoFuelRat = CurveValue(ICEngineGenerator(GeneratorNum)%TotExhausttoFuelCurve, PLR)
      QExhaustTotal = FuelEnergyUseRate * TotExhausttoFuelRat


!Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
!of the exhaust temperature in C to the part load ratio.
    IF (PLR > 0.0d0)THEN
      ExhaustTemp = CurveValue(ICEngineGenerator(GeneratorNum)%ExhaustTempCurve, PLR)

      IF (ExhaustTemp > ReferenceTemp) THEN

        ExhaustGasFlow = QExhaustTotal / (ExhaustCP*(ExhaustTemp-ReferenceTemp))

        !Use Curve fit to determine stack temp after heat recovery
        UA = ICEngineGenerator(GeneratorNum)%UACoef(1) * RatedPowerOutput **  &
                     ICEngineGenerator(GeneratorNum)%UACoef(2)

        DesignMinExitGasTemp = ICEngineGenerator(GeneratorNum)%DesignMinExitGasTemp

        ExhaustStackTemp = DesignMinExitGasTemp + (ExhaustTemp - DesignMinExitGasTemp) / &
                           EXP(UA/(MAX(ExhaustGasFlow, MaxExhaustperPowerOutput * RatedPowerOutput) * ExhaustCP))

        QExhaustRec = MAX(ExhaustGasFlow*ExhaustCP*(ExhaustTemp-ExhaustStackTemp),0.0d0)
      ELSE
        IF (ICEngineGenerator(GeneratorNum)%ErrExhaustTempIndex == 0) THEN
          CALL ShowWarningMessage('CalcICEngineGeneratorModel: '//trim(ICEngineGenerator(GeneratorNum)%TypeOf)//  &
              '="'//trim(ICEngineGenerator(GeneratorNum)%Name)//'" low Exhaust Temperature from Curve Value')
          CALL ShowContinueError('...curve generated temperature=['//trim(RoundSigDigits(ExhaustTemp,3))//  &
              ' C], PLR=['//trim(RoundSigDigits(PLR,3))//'].')
          CALL ShowContinueError('...simulation will continue with exhaust heat reclaim set to 0.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('CalcICEngineGeneratorModel: '//trim(ICEngineGenerator(GeneratorNum)%TypeOf)//  &
          '="'//trim(ICEngineGenerator(GeneratorNum)%Name)//'" low Exhaust Temperature continues...',   &
          ICEngineGenerator(GeneratorNum)%ErrExhaustTempIndex,ReportMinOf=ExhaustTemp,ReportMaxOf=ExhaustTemp,   &
          ReportMaxUnits='[C]',ReportMinUnits='[C]')
        QExhaustRec =0.0d0
        ExhaustStackTemp = ICEngineGenerator(GeneratorNum)%DesignMinExitGasTemp
      ENDIF
    ELSE
      QExhaustRec =0.0d0
    END IF


  QTotalHeatRecovered = QExhaustRec + QLubeOilRec + QJacketRec

  IF (ICEngineGenerator(GeneratorNum)%HeatRecActive) THEN
    CALL CalcICEngineGenHeatRecovery(GeneratorNum,QTotalHeatRecovered,HeatRecMDot,HRecRatio)
    QExhaustRec = QExhaustRec*HRecRatio
    QLubeOilRec = QLubeOilRec*HRecRatio
    QJacketRec  = QJacketRec*HRecRatio
    QTotalHeatRecovered = QTotalHeatRecovered*HRecRatio
  ELSE
    ICEngineGenerator(GeneratorNum)%HeatRecInletTemp    = HeatRecInTemp
    ICEngineGenerator(GeneratorNum)%HeatRecOutletTemp   = HeatRecInTemp
    ICEngineGenerator(GeneratorNum)%HeatRecMdotActual = HeatRecMdot

  ENDIF

      !Calculate Energy
   ElectricEnergyGen    = ElecPowerGenerated*TimeStepSys*SecInHour
   FuelEnergyUsed       = FuelEnergyUseRate*TimeStepSys*SecInHour
   JacketEnergyRec      = QJacketRec*TimeStepSys*SecInHour
   LubeOilEnergyRec     = QLubeOilRec*TimeStepSys*SecInHour
   ExhaustEnergyRec     = QExhaustRec*TimeStepSys*SecInHour

  ICEngineGenerator(GeneratorNum)%ElecPowerGenerated  = ElecPowerGenerated
  ICEngineGenerator(GeneratorNum)%ElecEnergyGenerated = ElectricEnergyGen
  ICEngineGenerator(GeneratorNum)%QJacketRecovered    = QJacketRec
  ICEngineGenerator(GeneratorNum)%QLubeOilRecovered   = QLubeOilRec
  ICEngineGenerator(GeneratorNum)%QExhaustRecovered   = QExhaustRec
  ICEngineGenerator(GeneratorNum)%QTotalHeatRecovered = QTotalHeatRecovered
  ICEngineGenerator(GeneratorNum)%JacketEnergyRec     = JacketEnergyRec
  ICEngineGenerator(GeneratorNum)%LubeOilEnergyRec    = LubeOilEnergyRec
  ICEngineGenerator(GeneratorNum)%ExhaustEnergyRec    = ExhaustEnergyRec
  ICEngineGenerator(GeneratorNum)%QTotalHeatRecovered = (QExhaustRec + QLubeOilRec + QJacketRec)
  ICEngineGenerator(GeneratorNum)%TotalHeatEnergyRec = (ExhaustEnergyRec + LubeOilEnergyRec + JacketEnergyRec)
  ICEngineGenerator(GeneratorNum)%FuelEnergyUseRate   = ABS(FuelEnergyUseRate)
  ICEngineGenerator(GeneratorNum)%FuelEnergy          = ABS(FuelEnergyUsed)

  FuelHeatingValue = ICEngineGenerator(GeneratorNum)%FuelHeatingValue

  ICEngineGenerator(GeneratorNum)%FuelMdot      =  ABS(FuelEnergyUseRate)/(FuelHeatingValue * KJtoJ)
  ICEngineGenerator(GeneratorNum)%ExhaustStackTemp    = ExhaustStackTemp




RETURN
END SUBROUTINE CalcICEngineGeneratorModel

SUBROUTINE CalcICEngineGenHeatRecovery(Num,EnergyRecovered,HeatRecMdot,HRecRatio)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Brandon Anderson
            !       DATE WRITTEN:    November 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! To perform heat recovery calculations and node updates


            ! METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
            ! It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
            ! The chiller sets the flow on the loop first by the input design flowrate and then
            ! performs a check to verify that

            ! REFERENCES: na

            ! USE STATEMENTS:
USE FluidProperties, ONLY: GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT(IN)          :: Num              ! HR Component number
  REAL(r64), INTENT(IN)            :: EnergyRecovered  ! Amount of heat recovered
  REAL(r64),INTENT(IN)             :: HeatRecMdot
  REAL(r64),INTENT(INOUT)             :: HRecRatio        ! Max Heat recovery ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: HeatRecInNode
  INTEGER                  :: HeatRecOutNode
  REAL(r64)                :: MinHeatRecMdot
  REAL(r64)                :: HeatRecInTemp
  REAL(r64)                :: HeatRecOutTemp
  REAL(r64)                :: HeatRecCp

  !Load inputs to local structure
  HeatRecInNode = ICEngineGenerator(Num)%HeatRecInletNodeNum
  HeatRecOutNode = ICEngineGenerator(Num)%HeatRecOutletNodeNum


  !Need to set the HeatRecRatio to 1.0 if it is not modified
  HRecRatio= 1.0d0


  HeatRecInTemp = Node(HeatRecInNode)%Temp
  HeatRecCp = GetSpecificHeatGlycol(PlantLoop(ICEngineGenerator(Num)%HRLoopNum)%FluidName, &
                                 HeatRecInTemp, &
                                 PlantLoop(ICEngineGenerator(Num)%HRLoopNum)%FluidIndex, &
                                 'CalcICEngineGeneratorModel')

  !Don't divide by zero - Note This also results in no heat recovery when
  !  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
  !  In order to see what minimum heat recovery flow rate is for the design temperature
  !  The design heat recovery flow rate can be set very small, but greater than zero.
  IF ((HeatRecMdot .GT. 0) .AND. (HeatRecCp .GT. 0)) THEN
    HeatRecOutTemp = (EnergyRecovered)/(HeatRecMdot * HeatRecCp) + HeatRecInTemp
  ELSE
    HeatRecOutTemp = HeatRecInTemp
  END IF


    !Note: check to make sure the Max Temperature was not exceeded
  IF(HeatRecOutTemp > ICEngineGenerator(Num)%HeatRecMaxTemp) THEN
    IF(ICEngineGenerator(Num)%HeatRecMaxTemp /= HeatRecInTemp)THEN
      MinHeatRecMdot = (EnergyRecovered)/(HeatRecCp * (ICEngineGenerator(Num)%HeatRecMaxTemp - HeatRecInTemp))
      If(MinHeatRecMdot < 0.0d0) MinHeatRecMdot = 0.0d0
    ELSE
      MinHeatRecMdot = 0.0d0
    END IF

    !Recalculate Outlet Temperature, with adjusted flowrate
    IF ((MinHeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
      HeatRecOutTemp = (EnergyRecovered)/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
      HRecRatio = HeatRecMdot/MinHeatRecMdot
    ELSE
      HeatRecOutTemp = HeatRecInTemp
      HRecRatio = 0.0d0
    END IF

  END IF


  !Update global variables for reporting later
  ICEngineGenerator(Num)%HeatRecInletTemp = HeatRecInTemp
  ICEngineGenerator(Num)%HeatRecOutletTemp = HeatRecOutTemp
  ICEngineGenerator(Num)%HeatRecMdotActual = HeatRecMdot



END SUBROUTINE CalcICEngineGenHeatRecovery


! End IC ENGINE Generator Module Model Subroutines
! *****************************************************************************

! Begin IC ENGINE Generator Module Utility Subroutines
! *****************************************************************************
SUBROUTINE InitICEngineGenerators(GeneratorNum,RunFlag,MyLoad,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  Brent Griffith, Sept 2010, plant upgrades, generalize fluid props

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the IC ENGINE generators.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_Generator_ICEngine
  USE PlantUtilities,  ONLY: SetComponentFlowRate, InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,     INTENT(IN)  :: GeneratorNum    ! Generator number
  LOGICAL,     INTENT(IN)  :: RunFlag         ! TRUE when Generator operating
  REAL(r64)  , INTENT(IN)  :: MyLoad          ! Generator demand
  LOGICAL,     INTENT(IN)  :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: HeatRecInletNode ! inlet node number in heat recovery loop
  INTEGER             :: HeatRecOutletNode ! outlet node number in heat recovery loop
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! Used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeAndNodeInitFlag
  REAL(r64)  :: mdot
  REAL(r64)  :: rho
  LOGICAL    :: errFlag

          ! FLOW:
! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumICEngineGenerators))
    ALLOCATE(MyPlantScanFlag(NumICEngineGenerators))
    ALLOCATE(MySizeAndNodeInitFlag(NumICEngineGenerators))
    MyEnvrnFlag           = .TRUE.
    MyPlantScanFlag       = .TRUE.
    MyOneTimeFlag         = .FALSE.
    MySizeAndNodeInitFlag = .TRUE.
  END IF
  IF (MyPlantScanFlag(GeneratorNum) .AND. ALLOCATED(PlantLoop) &
      .AND. ICEngineGenerator(GeneratorNum)%HeatRecActive) THEN
    errFlag = .FALSE.
    CALL ScanPlantLoopsForObject(ICEngineGenerator(GeneratorNum)%Name, &
                                 TypeOf_Generator_ICEngine, &
                                 ICEngineGenerator(GeneratorNum)%HRLoopNum, &
                                 ICEngineGenerator(GeneratorNum)%HRLoopSideNum, &
                                 ICEngineGenerator(GeneratorNum)%HRBranchNum, &
                                 ICEngineGenerator(GeneratorNum)%HRCompNum , &
                                 errFlag = errFlag )
    IF (errFlag) THEN
      CALL ShowFatalError('InitICEngineGenerators: Program terminated due to previous condition(s).')
    ENDIF

    MyPlantScanFlag(GeneratorNum) = .FALSE.
  ENDIF


  IF (MySizeAndNodeInitFlag(GeneratorNum) .AND. (.NOT. MyPlantScanFlag(GeneratorNum)) &
      .AND.  ICEngineGenerator(GeneratorNum)%HeatRecActive ) THEN

    HeatRecInletNode    = ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum
    HeatRecOutletNode   = ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum

    !size mass flow rate
    rho = GetDensityGlycol(PlantLoop(ICEngineGenerator(GeneratorNum)%HRLoopNum)%FluidName, &
                                     InitConvTemp, &
                                     PlantLoop(ICEngineGenerator(GeneratorNum)%HRLoopNum)%FluidIndex, &
                                     'InitICEngineGenerators')

    ICEngineGenerator(GeneratorNum)%DesignHeatRecMassFlowRate = rho * ICEngineGenerator(GeneratorNum)%DesignHeatRecVolFlowRate
    ICEngineGenerator(GeneratorNum)%HeatRecMdotDesign = ICEngineGenerator(GeneratorNum)%DesignHeatRecMassFlowRate

    CALL InitComponentNodes(0.0D0,  ICEngineGenerator(GeneratorNum)%DesignHeatRecMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 ICEngineGenerator(GeneratorNum)%HRLoopNum, &
                                 ICEngineGenerator(GeneratorNum)%HRLoopSideNum, &
                                 ICEngineGenerator(GeneratorNum)%HRBranchNum, &
                                 ICEngineGenerator(GeneratorNum)%HRCompNum )

    MySizeAndNodeInitFlag(GeneratorNum) = .FALSE.
  END IF ! end one time inits

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(GeneratorNum) .AND. ICEngineGenerator(GeneratorNum)%HeatRecActive) THEN
    HeatRecInletNode    = ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum
    HeatRecOutletNode   = ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum
    ! set the node Temperature, assuming freeze control
    Node(HeatRecInletNode)%Temp = 20.0d0
    Node(HeatRecOutletNode)%Temp = 20.0d0
   ! set the node max and min mass flow rates
    Call InitComponentNodes(0.0D0,  ICEngineGenerator(GeneratorNum)%DesignHeatRecMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 ICEngineGenerator(GeneratorNum)%HRLoopNum, &
                                 ICEngineGenerator(GeneratorNum)%HRLoopSideNum, &
                                 ICEngineGenerator(GeneratorNum)%HRBranchNum, &
                                 ICEngineGenerator(GeneratorNum)%HRCompNum )

    MyEnvrnFlag(GeneratorNum) = .FALSE.
  END IF ! end environmental inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(GeneratorNum) = .TRUE.
  ENDIF

  IF (ICEngineGenerator(GeneratorNum)%HeatRecActive) THEN
    IF ( FirstHVACIteration) Then
      IF (RunFlag) THEN
        mdot =  ICEngineGenerator(GeneratorNum)%DesignHeatRecMassFlowRate
      ELSE
        mdot = 0.d0
      ENDIF
      CALL SetComponentFlowRate(mdot, &
                                   ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum, &
                                   ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum, &
                                   ICEngineGenerator(GeneratorNum)%HRLoopNum, &
                                   ICEngineGenerator(GeneratorNum)%HRLoopSideNum, &
                                   ICEngineGenerator(GeneratorNum)%HRBranchNum, &
                                   ICEngineGenerator(GeneratorNum)%HRCompNum )

    ELSE
      CALL SetComponentFlowRate(   ICEngineGenerator(GeneratorNum)%HeatRecMdotActual, &
                                   ICEngineGenerator(GeneratorNum)%HeatRecInletNodeNum, &
                                   ICEngineGenerator(GeneratorNum)%HeatRecOutletNodeNum, &
                                   ICEngineGenerator(GeneratorNum)%HRLoopNum, &
                                   ICEngineGenerator(GeneratorNum)%HRLoopSideNum, &
                                   ICEngineGenerator(GeneratorNum)%HRBranchNum, &
                                   ICEngineGenerator(GeneratorNum)%HRCompNum )
    ENDIF
  ENDIF

RETURN
END SUBROUTINE InitICEngineGenerators
! End IC ENGINE Generator Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the IC ENGINE Generator Module
! *****************************************************************************

SUBROUTINE UpdateICEngineGeneratorRecords(RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if Generator operating
  INTEGER, INTENT(IN)      :: Num       ! Generator number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: HeatRecInletNode
  INTEGER                  :: HeatRecOutletNode

    IF (ICEngineGenerator(Num)%HeatRecActive) THEN
      HeatRecInletNode    = ICEngineGenerator(Num)%HeatRecInletNodeNum
      HeatRecOutletNode   = ICEngineGenerator(Num)%HeatRecOutletNodeNum
!      Node(HeatRecOutletNode)%MassFlowRate            = ICEngineGenerator(Num)%HeatRecMdotActual
      Node(HeatRecOutletNode)%Temp                    = ICEngineGenerator(Num)%HeatRecOutletTemp
!      Node(HeatRecOutletNode)%MassFlowRateMaxAvail = Node(HeatRecInletNode)%MassFlowRateMaxAvail
!      Node(HeatRecOutletNode)%MassFlowRateMinAvail = Node(HeatRecInletNode)%MassFlowRateMinAvail
    ENDIF
    ICEngineGeneratorReport(Num)%PowerGen             = ICEngineGenerator(Num)%ElecPowerGenerated
    ICEngineGeneratorReport(Num)%QJacketRecovered     = ICEngineGenerator(Num)%QJacketRecovered
    ICEngineGeneratorReport(Num)%QLubeOilRecovered    = ICEngineGenerator(Num)%QLubeOilRecovered
    ICEngineGeneratorReport(Num)%QExhaustRecovered    = ICEngineGenerator(Num)%QExhaustRecovered
    ICEngineGeneratorReport(Num)%QTotalHeatRecovered  = ICEngineGenerator(Num)%QTotalHeatRecovered
    ICEngineGeneratorReport(Num)%FuelEnergyUseRate    = ICEngineGenerator(Num)%FuelEnergyUseRate
    ICEngineGeneratorReport(Num)%EnergyGen            = ICEngineGenerator(Num)%ElecEnergyGenerated
    ICEngineGeneratorReport(Num)%JacketEnergyRec      = ICEngineGenerator(Num)%JacketEnergyRec
    ICEngineGeneratorReport(Num)%LubeOilEnergyRec     = ICEngineGenerator(Num)%LubeOilEnergyRec
    ICEngineGeneratorReport(Num)%ExhaustEnergyRec     = ICEngineGenerator(Num)%ExhaustEnergyRec
    ICEngineGeneratorReport(Num)%TotalHeatEnergyRec   = ICEngineGenerator(Num)%TotalHeatEnergyRec
    ICEngineGeneratorReport(Num)%FuelEnergy           = ICEngineGenerator(Num)%FuelEnergy
    ICEngineGeneratorReport(Num)%FuelMdot             = ICEngineGenerator(Num)%FuelMdot
    ICEngineGeneratorReport(Num)%ExhaustStackTemp     = ICEngineGenerator(Num)%ExhaustStackTemp
    ICEngineGeneratorReport(Num)%HeatRecInletTemp     = ICEngineGenerator(Num)%HeatRecInletTemp
    ICEngineGeneratorReport(Num)%HeatRecOutletTemp    = ICEngineGenerator(Num)%HeatRecOutletTemp
    ICEngineGeneratorReport(Num)%HeatRecMdot          = ICEngineGenerator(Num)%HeatRecMdotActual

RETURN
END SUBROUTINE UpdateICEngineGeneratorRecords

! End of Record Keeping subroutines for the IC ENGINE Generator Module
! *****************************************************************************


END MODULE ICEngineElectricGenerator


!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


MODULE CTElectricGenerator  !COMBUSTION Turbine Generator Module

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the COMBUSTION turbine
          ! Generators.

          ! METHODOLOGY EMPLOYED:
          ! Once the Electric power manager determines that the CT Generator
          ! is available, it calls SimCTGenerator which in turn calls the
          ! appropriate COMBUSTION turbine Generator model.
          ! All CT Generator models are based on a polynomial fit of Generator
          ! performance data.

          ! REFERENCES: na

          ! OTHER NOTES:

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals ,   ONLY : MaxNameLength, NumOfTimeStepInHour, SecInHour, BeginEnvrnFlag, InitConvTemp
USE DataInterfaces, ONLY : ShowSevereError, ShowWarningError, ShowFatalError,  &
                           ShowContinueError, SetupOutputVariable
USE DataGlobalConstants, ONLY: iGeneratorCombTurbine


IMPLICIT NONE

PRIVATE
          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
TYPE CTGeneratorSpecs
       CHARACTER(len=MaxNameLength) :: Name           = ' ' ! user identifier
       CHARACTER(len=MaxNameLength) :: TypeOf         = 'Generator:CombustionTurbine' ! Type of Generator
       INTEGER           :: CompType_Num              = iGeneratorCombTurbine
       CHARACTER(len=MaxNameLength) :: FuelType             ! Type of Fuel - DIESEL, GASOLINE, GAS
       REAL(r64)         :: RatedPowerOutput          = 0.0d0 ! W - design nominal capacity of Generator
       INTEGER           :: ElectricCircuitNode       = 0   ! Electric Circuit Node
       REAL(r64)         :: MinPartLoadRat            = 0.0d0 ! (CT MIN) min allowed operating frac full load
       REAL(r64)         :: MaxPartLoadRat            = 0.0d0 ! (CT MAX) max allowed operating frac full load
       REAL(r64)         :: OptPartLoadRat            = 0.0d0 ! (CT BEST) optimal operating frac full load
       REAL(r64)         :: FuelEnergyUseRate         = 0.0d0 !(EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
       REAL(r64)         :: FuelEnergy                = 0.0d0 !Amount of Fuel Energy Required to run COMBUSTION turbine (J)
       INTEGER           :: PLBasedFuelInputCurve     = 0   !(FUL1GC) Curve Index for Part Load Ratio Based Fuel Input
                                                            ! Coefficients Poly Fit
       INTEGER           :: TempBasedFuelInputCurve   = 0   !(FUL2GC) Curve Index for Ambient Temperature Based Fuel Input
                                                            ! Coeff Poly Fit
       REAL(r64)         :: ExhaustFlow               = 0.0d0 !(FEX) Exhaust Gas Flow Rate cubic meters per second???
       INTEGER           :: ExhaustFlowCurve          = 0   !(FEXGC) Curve Index for Exhaust Gas Flow Rate Input Coef Poly Fit
       REAL(r64)         :: ExhaustTemp               = 0.0d0 !(TEX) Exhaust Gas Temperature in C
       INTEGER           :: PLBasedExhaustTempCurve   = 0   !(TEX1GC) Curve Index for Part Load Ratio Based Exhaust Temp Input
                                                            ! Coeffs Poly Fit
       INTEGER           :: TempBasedExhaustTempCurve = 0   !(TEX2GC) Curve Index for Ambient Temperature Based Exhaust Gas Temp to
                                                            ! Fuel Energy Input Coeffs Poly Fit
       REAL(r64)         :: QLubeOilRecovered         = 0.0d0 !(ELUBE) Recovered Lube Oil Energy (W)
       REAL(r64)         :: QExhaustRecovered         = 0.0d0 !(EEX) Recovered Exhaust heat  (W)
       REAL(r64)         :: QTotalHeatRecovered       = 0.0d0 !total heat recovered (W)
       REAL(r64)         :: LubeOilEnergyRec          = 0.0d0 ! Recovered Lube Oil Energy (J)
       REAL(r64)         :: ExhaustEnergyRec          = 0.0d0 ! Recovered Exhaust heat  (J)
       REAL(r64)         :: TotalHeatEnergyRec        = 0.0d0 !total heat recovered (J)
       INTEGER           :: QLubeOilRecoveredCurve    = 0   !(ELUBEGC) Curve Index for Recoverable Lube Oil heat Input Coef Poly Fit
       REAL(r64)         :: UA                        = 0.0d0 !(UACGC) exhaust gas Heat Exchanger UA
       REAL(r64),DIMENSION(2) :: UACoef                    = 0.0d0 !Heat Exchanger UA  Coeffs Poly Fit
       REAL(r64)         :: MaxExhaustperCTPower      = 0.0d0 !MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
       REAL(r64)         :: DesignHeatRecVolFlowRate  = 0.0d0 ! m3/s, Design Water mass flow rate through heat recovery loop
       REAL(r64)         :: DesignHeatRecMassFlowRate = 0.0d0 ! kg/s, Design Water mass flow rate through heat recovery loop
       REAL(r64)         :: DesignMinExitGasTemp      = 0.0d0 !Steam Saturation Temperature (C)
       REAL(r64)         :: DesignAirInletTemp        = 0.0d0 !Design Turbine Air Inlet Temperature (C)
       REAL(r64)         :: ExhaustStackTemp          = 0.0d0 !turbine exhaust gas temp (C)
       LOGICAL           :: HeatRecActive             = .false. ! true when design max flow rate > 0
       INTEGER           :: HeatRecInletNodeNum       = 0   ! Node number on the heat recovery inlet side of the condenser
       INTEGER           :: HeatRecOutletNodeNum      = 0   ! Node number on the heat recovery outlet side of the condenser
       REAL(r64)         :: HeatRecInletTemp          = 0.0d0 !Inlet Temperature of the heat recovery fluid
       REAL(r64)         :: HeatRecOutletTemp         = 0.0d0 !Outlet Temperature of the heat recovery fluid
       REAL(r64)         :: HeatRecMdot               = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate
       INTEGER           :: HRLoopNum                 = 0   ! cooling water plant loop index number, for heat recovery
       INTEGER           :: HRLoopSideNum             = 0   ! cooling water plant loop side index, for heat recovery
       INTEGER           :: HRBranchNum               = 0   ! cooling water plant loop branch index, for heat recovery
       INTEGER           :: HRCompNum                 = 0   ! cooling water plant loop component index, for heat recovery

       REAL(r64)         :: FuelMdot                  = 0.0d0 ! reporting: Fuel Amount used (kg/s)
       REAL(r64)         :: FuelHeatingValue          = 0.0d0 !Heating Value for Fuel in (kJ/kg)
       REAL(r64)         :: ElecPowerGenerated        = 0.0d0 ! reporting: power generated (W)
       REAL(r64)         :: ElecEnergyGenerated       = 0.0d0 ! reporting: power generated (W)
       REAL(r64)         :: HeatRecMaxTemp            = 0.0d0 !Max Temp that can be produced in heat recovery
       INTEGER           :: OAInletNode               = 0   ! optional inlet node index pointer for outdoor air for compustion
END TYPE CTGeneratorSpecs

TYPE ReportVars
  REAL(r64)    :: PowerGen                = 0.0d0 ! reporting: power (W)
  REAL(r64)    :: EnergyGen               = 0.0d0 ! reporting: power (W)
  REAL(r64)    :: QTotalHeatRecovered     = 0.0d0 ! reporting: total Heat Recovered (W)
  REAL(r64)    :: QLubeOilRecovered       = 0.0d0 ! reporting: Heat Recovered from Lubricant (W)
  REAL(r64)    :: QExhaustRecovered       = 0.0d0 ! reporting: Heat Recovered from exhaust (W)
  REAL(r64)    :: TotalHeatEnergyRec      = 0.0d0 ! reporting: total Heat Recovered (W)
  REAL(r64)    :: LubeOilEnergyRec        = 0.0d0 ! reporting: Heat Recovered from Lubricant (W)
  REAL(r64)    :: ExhaustEnergyRec        = 0.0d0 ! reporting: Heat Recovered from exhaust (W)
  REAL(r64)    :: FuelEnergyUseRate       = 0.0d0 ! reporting: Fuel Energy use rate (W)
  REAL(r64)    :: FuelEnergy              = 0.0d0 ! reporting: Fuel Energy used (J)
  REAL(r64)    :: FuelMdot                = 0.0d0 ! reporting: Fuel Amount used (kg/s)
  REAL(r64)    :: ExhaustStackTemp        = 0.0d0 ! reporting: Exhaust Stack Temperature (C)
  REAL(r64)    :: HeatRecInletTemp        = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
  REAL(r64)    :: HeatRecOutletTemp       = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
  REAL(r64)    :: HeatRecMdot             = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)
END TYPE ReportVars


          ! MODULE VARIABLE DECLARATIONS:
INTEGER            :: NumCTGenerators = 0        ! number of CT Generators specified in input
LOGICAL           :: GetCTInput = .TRUE.! then TRUE, calls subroutine to read input file.

TYPE(CTGeneratorSpecs), ALLOCATABLE, DIMENSION(:) :: CTGenerator  !dimension to number of machines
TYPE(ReportVars),       ALLOCATABLE, DIMENSION(:) :: CTGeneratorReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PRIVATE    CalcCTGeneratorModel
PRIVATE    GetCTGeneratorInput
PRIVATE    UpdateCTGeneratorRecords
PUBLIC     SimCTGenerator
PUBLIC     GetCTGeneratorResults
PUBLIC     SimCTPlantHeatRecovery

CONTAINS
          ! MODULE SUBROUTINES:
! Beginning of CT Generator Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimCTGenerator(GeneratorType,GeneratorName,GeneratorIndex,RunFlag, MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the CT Generator driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GeneratorType     ! type of Generator
  CHARACTER(len=*), INTENT(IN) :: GeneratorName     ! user specified name of Generator
  INTEGER, INTENT(INOUT) :: GeneratorIndex
  LOGICAL , INTENT(IN)   :: RunFlag                 ! simulate Generator when TRUE
  REAL(r64), INTENT(IN)       :: MyLoad                  ! generator demand
  LOGICAL, INTENT (IN)   :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: GenNum         ! Generator number counter


          !Get Generator data from input file
  IF (GetCTInput) THEN
    CALL GetCTGeneratorInput
    GetCTInput = .FALSE.
  END IF



        !SELECT and CALL MODELS
  IF (GeneratorIndex == 0) THEN
    GenNum = FindItemInList(GeneratorName,CTGenerator%Name,NumCTGenerators)
    IF (GenNum == 0) CALL ShowFatalError('SimCTGenerator: Specified Generator not one of Valid COMBUSTION Turbine Generators '// &
                                       TRIM(GeneratorName))
    GeneratorIndex=GenNum
  ELSE
    GenNum=GeneratorIndex
    IF (GenNum > NumCTGenerators .or. GenNum < 1) THEN
      CALL ShowFatalError('SimCTGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                          ', Number of CT Engine Generators='//TRIM(TrimSigDigits(NumCTGenerators))//  &
                          ', Generator name='//TRIM(GeneratorName))
    ENDIF
    IF (CheckEquipName(GenNum)) THEN
      IF (GeneratorName /= CTGenerator(GenNum)%Name) THEN
        CALL ShowFatalError('SimCTGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(GenNum))// &
                            ', Generator name='//TRIM(GeneratorName)//', stored Generator Name for that index='//  &
                            TRIM(CTGenerator(GenNum)%Name))
      ENDIF
      CheckEquipName(GenNum)=.false.
    ENDIF
  ENDIF

  CALL InitCTGenerators(GenNum,Runflag,MyLoad,FirstHVACIteration)
  CALL CalcCTGeneratorModel(GenNum,Runflag,MyLoad,FirstHVACIteration)
  CALL UpdateCTGeneratorRecords(RunFlag,GenNum)

RETURN
END SUBROUTINE SimCTGenerator
SUBROUTINE SimCTPlantHeatRecovery(CompType,CompName,CompTypeNum,CompNum,RunFlag,InitLoopEquip,  &   !DSU
                          MyLoad,MaxCap,MinCap,OptCap,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill data needed in PlantLoopEquipments

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType !unused1208
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(IN)          :: CompTypeNum !unused1208
  INTEGER, INTENT(INOUT)       :: CompNum
  LOGICAL, INTENT(IN)          :: RunFlag
  !INTEGER, INTENT(IN)          :: FlowLock !unused1208 !DSU
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(INOUT)     :: MyLoad
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (GetCTInput) THEN
    CALL GetCTGeneratorInput
    GetCTInput = .FALSE.
  END IF

  If (InitLoopEquip) Then
    CompNum = FindItemInList(CompName, CTGenerator%name, NumCTGenerators)
    If (CompNum == 0) Then
      CALL ShowFatalError('SimCTPlantHeatRecovery: CT Generator Unit not found='//TRIM(CompName))
      RETURN
    ENDIF
      MinCap  = 0.0d0
      MaxCap  = 0.0d0
      OptCap  = 0.0d0
      RETURN
  ENDIF ! End Of InitLoopEquip

  RETURN

END SUBROUTINE SimCTPlantHeatRecovery


! End CT Generator Module Driver Subroutines
!******************************************************************************


! Beginning of CT Generator Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetCTGeneratorInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    April 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the CT Generator models.


            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE CurveManager,   ONLY : GetCurveIndex
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE OutAirNodeManager,  ONLY: CheckOutAirNodeNumber
  USE General, ONLY:  RoundSigDigits
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: GeneratorNum !Generator counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(12)   :: AlphArray !character string data
  REAL(r64),                        DIMENSION(12)  :: NumArray  !numeric data
  LOGICAL, SAVE :: ErrorsFound=.false.   ! error flag
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name

         !FLOW

  cCurrentModuleObject = 'Generator:CombustionTurbine'
  NumCTGenerators = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumCTGenerators <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

         !ALLOCATE ARRAYS
  ALLOCATE (CTGenerator(NumCTGenerators))
  ALLOCATE(CheckEquipName(NumCTGenerators))
  CheckEquipName=.true.

  ALLOCATE (CTGeneratorReport(NumCTGenerators))

         !LOAD ARRAYS WITH CT CURVE FIT Generator DATA
  DO GeneratorNum = 1 , NumCTGenerators
    CALL GetObjectItem(cCurrentModuleObject,GeneratorNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),CTGenerator%Name,GeneratorNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    CTGenerator(GeneratorNum)%Name                = AlphArray(1)

    CTGenerator(GeneratorNum)%RatedPowerOutput              = NumArray(1)
    IF (NumArray(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(NumArray(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound=.true.
    ENDIF

           ! Not sure what to do with electric nodes, so do not use optional arguments
    CTGenerator(GeneratorNum)%ElectricCircuitNode    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Electric,NodeConnectionType_Electric,1,ObjectIsNotParent)

    CTGenerator(GeneratorNum)%MinPartLoadRat      = NumArray(2)
    CTGenerator(GeneratorNum)%MaxPartLoadRat      = NumArray(3)
    CTGenerator(GeneratorNum)%OptPartLoadRat      = NumArray(4)


    !Load Special CT Generator Input

    CTGenerator(GeneratorNum)%PLBasedFuelInputCurve = GetCurveIndex(AlphArray(3)) ! convert curve name to number
    IF (CTGenerator(GeneratorNum)%PLBasedFuelInputCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(AlphArray(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    CTGenerator(GeneratorNum)%TempBasedFuelInputCurve = GetCurveIndex(AlphArray(4)) ! convert curve name to number
    IF (CTGenerator(GeneratorNum)%TempBasedFuelInputCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(AlphArray(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    CTGenerator(GeneratorNum)%ExhaustFlowCurve = GetCurveIndex(AlphArray(5)) ! convert curve name to number
    IF (CTGenerator(GeneratorNum)%ExhaustFlowCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(AlphArray(5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    CTGenerator(GeneratorNum)%PLBasedExhaustTempCurve = GetCurveIndex(AlphArray(6)) ! convert curve name to number
    IF (CTGenerator(GeneratorNum)%PLBasedExhaustTempCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(AlphArray(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    CTGenerator(GeneratorNum)%TempBasedExhaustTempCurve = GetCurveIndex(AlphArray(7)) ! convert curve name to number
    IF (CTGenerator(GeneratorNum)%TempBasedExhaustTempCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphArray(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    CTGenerator(GeneratorNum)%QLubeOilRecoveredCurve = GetCurveIndex(AlphArray(8)) ! convert curve name to number
    IF (CTGenerator(GeneratorNum)%QLubeOilRecoveredCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(AlphArray(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

    CTGenerator(GeneratorNum)%UACoef(1) = NumArray(5)
    CTGenerator(GeneratorNum)%UACoef(2) = NumArray(6)

    CTGenerator(GeneratorNum)%MaxExhaustperCTPower = NumArray(7)
    CTGenerator(GeneratorNum)%DesignMinExitGasTemp = NumArray(8)
    CTGenerator(GeneratorNum)%DesignAirInletTemp = NumArray(9)
    CTGenerator(GeneratorNum)%FuelHeatingValue = NumArray(10)
    CTGenerator(GeneratorNum)%DesignHeatRecVolFlowRate = NumArray(11)

    IF (CTGenerator(GeneratorNum)%DesignHeatRecVolFlowRate > 0.0d0) THEN
      CTGenerator(GeneratorNum)%HeatRecActive=.true.
      CTGenerator(GeneratorNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(AlphArray(9),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      IF (CTGenerator(GeneratorNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Missing Node Name, Heat Recovery Inlet, for '// &
                              TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      CTGenerator(GeneratorNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(10),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      IF (CTGenerator(GeneratorNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Missing Node Name, Heat Recovery Outlet, for '// &
                              TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(9),AlphArray(10),'Heat Recovery Nodes')
      Call RegisterPlantCompDesignFlow(CTGenerator(GeneratorNum)%HeatRecInletNodeNum,     &
                                 CTGenerator(GeneratorNum)%DesignHeatRecVolFlowRate )
    ELSE
      CTGenerator(GeneratorNum)%HeatRecActive=.false.
      CTGenerator(GeneratorNum)%HeatRecInletNodeNum   = 0
      CTGenerator(GeneratorNum)%HeatRecOutletNodeNum   = 0
      IF (.NOT. lAlphaFieldBlanks(9) .OR. .NOT. lAlphaFieldBlanks(10) ) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '// &
                              TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('However, Node names were specified for Heat Recovery inlet or outlet nodes')
      ENDIF
    ENDIF

    !Fuel Type Case Statement
    SELECT CASE (AlphArray(11))
    CASE ('  ') !If blank then the default is Natural Gas
      CTGenerator(GeneratorNum)%FuelType = 'Gas'

    CASE ('GAS','NATURALGAS','NATURAL GAS')
      CTGenerator(GeneratorNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      CTGenerator(GeneratorNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      CTGenerator(GeneratorNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       CTGenerator(GeneratorNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       CTGenerator(GeneratorNum)%FuelType = 'FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
       CTGenerator(GeneratorNum)%FuelType = 'Propane'

    CASE ('OTHERFUEL1')
       CTGenerator(GeneratorNum)%FuelType = 'OtherFuel1'

    CASE ('OTHERFUEL2')
       CTGenerator(GeneratorNum)%FuelType = 'OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(AlphArray(11)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound=.true.
    END SELECT

    CTGenerator(GeneratorNum)%HeatRecMaxTemp = NumArray(12)


    !begin CR7021
    IF (lAlphaFieldBlanks(12) ) THEN
         CTGenerator(GeneratorNum)%OAInletNode  =  0
      ELSE
         CTGenerator(GeneratorNum)%OAInletNode  = &
               GetOnlySingleNode(AlphArray(12),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1),  &
                            NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
         IF ( .not. CheckOutAirNodeNumber(CTGenerator(GeneratorNum)%OAInletNode)) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(CTGenerator(GeneratorNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(12)))
           CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
           ErrorsFound=.true.
         ENDIF

      ENDIF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO GeneratorNum = 1, NumCTGenerators
     CALL SetupOutputVariable('Generator Produced Electric Power [W]', &
          CTGeneratorReport(GeneratorNum)%PowerGen,'System','Average',CTGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Produced Electric Energy [J]', &
          CTGeneratorReport(GeneratorNum)%EnergyGen,'System','Sum',CTGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')

     CALL SetupOutputVariable('Generator '// TRIM(CTGenerator(GeneratorNum)%FuelType)//' Rate [W]', &
          CTGeneratorReport(GeneratorNum)%FuelEnergyUseRate,'System','Average',CTGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator '// TRIM(CTGenerator(GeneratorNum)%FuelType)//' Energy [J]', &
          CTGeneratorReport(GeneratorNum)%FuelEnergy,'System','Sum',CTGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey=CTGenerator(GeneratorNum)%FuelType,EndUseKey='COGENERATION',GroupKey='Plant')

!    general fuel use report (to match other generators)
     CALL SetupOutputVariable('Generator Fuel HHV Basis Rate [W]', &
          CTGeneratorReport(GeneratorNum)%FuelEnergyUseRate,'System','Average',CTGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Fuel HHV Basis Energy [J]', &
          CTGeneratorReport(GeneratorNum)%FuelEnergy,'System','Sum',CTGenerator(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator '// TRIM(CTGenerator(GeneratorNum)%FuelType)//' Mass Flow Rate [kg/s]', &
          CTGeneratorReport(GeneratorNum)%FuelMdot,'System','Average',CTGenerator(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator Exhaust Air Temperature [C]', &
          CTGeneratorReport(GeneratorNum)%ExhaustStackTemp,'System','Average',CTGenerator(GeneratorNum)%Name)


     IF (CTGenerator(GeneratorNum)%HeatRecActive) THEN
       CALL SetupOutputVariable('Generator Exhaust Heat Recovery Rate [W]', &
            CTGeneratorReport(GeneratorNum)%QExhaustRecovered,'System','Average',CTGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Exhaust Heat Recovery Energy [J]', &
            CTGeneratorReport(GeneratorNum)%ExhaustEnergyRec,'System','Sum',CTGenerator(GeneratorNum)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Generator Lube Heat Recovery Rate [W]', &
            CTGeneratorReport(GeneratorNum)%QLubeOilRecovered,'System','Average',CTGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Lube Heat Recovery Energy [J]', &
            CTGeneratorReport(GeneratorNum)%LubeOilEnergyRec,'System','Sum',CTGenerator(GeneratorNum)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Generator Produced Thermal Rate [W]', &
            CTGeneratorReport(GeneratorNum)%QTotalHeatRecovered,'System','Average',CTGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', &
            CTGeneratorReport(GeneratorNum)%TotalHeatEnergyRec,'System','Sum',CTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Heat Recovery Inlet Temperature [C]', &
            CTGeneratorReport(GeneratorNum)%HeatRecInletTemp,'System','Average',CTGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Heat Recovery Outlet Temperature [C]', &
            CTGeneratorReport(GeneratorNum)%HeatRecOutletTemp,'System','Average',CTGenerator(GeneratorNum)%Name)
       CALL SetupOutputVariable('Generator Heat Recovery Mass Flow Rate [kg/s]', &
            CTGeneratorReport(GeneratorNum)%HeatRecMdot,'System','Average',CTGenerator(GeneratorNum)%Name)
     ENDIF

  END DO



RETURN
END SUBROUTINE GetCTGeneratorInput

! End of Get Input subroutines for the CT Generator Module
!******************************************************************************


! Beginning of Generator model Subroutines
! *****************************************************************************

SUBROUTINE CalcCTGeneratorModel(GeneratorNum,Runflag,MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression Generator using the CT model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data.  This model was originally
          ! developed by Dale Herron for the BLAST program

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys

  USE DataEnvironment, ONLY : OutDryBulbTemp
  USE CurveManager,    ONLY : CurveValue
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)   , INTENT(IN)    :: MyLoad          ! Generator demand
  INTEGER, INTENT(IN)    :: GeneratorNum    ! Generator number
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when Generator operating
  LOGICAL, INTENT(IN)    :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER   :: ExhaustCP = 1.047d0    !Exhaust Gas Specific Heat (J/kg-K)
  REAL(r64), PARAMETER   :: KJtoJ = 1000.d0        !convert Kjoules to joules

          ! INTERFACE BLOCK SPECIFICATIONS
            ! INTERFACE

            !  REAL(r64) FUNCTION CurveValue(CurveIndex,Var1,Var2)
            !
            !    INTEGER, INTENT (IN)        :: CurveIndex  ! index of curve in curve array
            !    REAL(r64), INTENT (IN)           :: Var1        ! 1st independent variable
            !    REAL(r64), INTENT (IN), OPTIONAL :: Var2        ! 2nd independent variable
            !
            !  END FUNCTION CurveValue
            !
            ! END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: MinPartLoadRat      ! min allowed operating frac full load
  REAL(r64)         :: MaxPartLoadRat      ! max allowed operating frac full load
  REAL(r64)         :: RatedPowerOutput    ! Generator nominal capacity (W)
  REAL(r64)         :: ElecPowerGenerated  ! Generator output (W)
  REAL(r64)         :: ElectricEnergyGen   ! Generator output (J)

! Special variables for CT Generator
  REAL(r64)    :: MaxExhaustperCTPower   !MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
  REAL(r64)    :: PLR                    ! Generator operating part load ratio
  REAL(r64)    :: FuelUseRate            !(EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
  REAL(r64)    :: FuelEnergyUsed         !Amount of Fuel Energy Required to run COMBUSTION turbine (J)
  REAL(r64)    :: ExhaustFlow            !(FEX) Exhaust Gas Flow Rate cubic meters per second???
  REAL(r64)    :: ExhaustTemp            !(TEX) Exhaust Gas Temperature in C
  REAL(r64)    :: UA             !(UACGC) Heat Exchanger UA to Capacity
  REAL(r64)    :: AmbientDeltaT          !(ATAIR) Difference between ambient actual and ambient design temperatures
  REAL(r64)    :: DesignAirInletTemp     ! design turbine inlet temperature (C)
  REAL(r64)    :: QLubeOilRec          ! recovered lube oil heat (W)
  REAL(r64)    :: QExhaustRec          ! recovered exhaust heat (W)
  REAL(r64)    :: LubeOilEnergyRec     ! recovered lube oil heat (J)
  REAL(r64)    :: ExhaustEnergyRec     ! recovered exhaust heat (J)
  REAL(r64)    :: MinHeatRecMdot       ! Heat Recovery Flow Rate if minimal heat recovery is accomplished
  REAL(r64)    :: DesignMinExitGasTemp     ! design engine stact saturated steam temp. (C)
  REAL(r64)    :: ExhaustStackTemp       ! turbine stack temp. (C)
  INTEGER :: HeatRecInNode          !Heat Recovery Fluid Inlet Node Num
!notused  INTEGER :: HeatRecOutNode         !Heat Recovery Fluid Outlet Node Num
  REAL(r64)    :: HeatRecInTemp          !Heat Recovery Fluid Inlet Temperature (C)
  REAL(r64)    :: HeatRecOutTemp         !Heat Recovery Fluid Outlet Temperature (C)
  REAL(r64)    :: HeatRecMdot            !Heat Recovery Fluid Mass FlowRate (kg/s)
  REAL(r64)    :: HeatRecCp              !Specific Heat of the Heat Recovery Fluid (J/kg-K)
  REAL(r64)    :: FuelHeatingValue       !Heating Value of Fuel in (kJ/kg)
  REAL(r64)    :: HRecRatio              !When Max Temp is reached the amount of recovered heat has to be reduced.
                                    ! and this assumption uses this ratio to accomplish this task.

          !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  MinPartLoadRat       = CTGenerator(GeneratorNum)%MinPartLoadRat
  MaxPartLoadRat       = CTGenerator(GeneratorNum)%MaxPartLoadRat
  RatedPowerOutput     = CTGenerator(GeneratorNum)%RatedPowerOutput
  MaxExhaustperCTPower = CTGenerator(GeneratorNum)%MaxExhaustperCTPower
  DesignAirInletTemp   = CTGenerator(GeneratorNum)%DesignAirInletTemp
  IF (CTGenerator(GeneratorNum)%HeatRecActive) THEN
    HeatRecInNode        = CTGenerator(GeneratorNum)%HeatRecInletNodeNum
    HeatRecInTemp = Node(HeatRecInNode)%Temp

    HeatRecCp = GetSpecificHeatGlycol(PlantLoop(CTGenerator(GeneratorNum)%HRLoopNum)%FluidName, &
                             HeatRecInTemp, &
                             PlantLoop(CTGenerator(GeneratorNum)%HRLoopNum)%FluidIndex, &
                             'CalcCTGeneratorModel')
    If(FirstHVACIteration .AND. RunFlag) Then
       HeatRecMdot = CTGenerator(GeneratorNum)%DesignHeatRecMassFlowRate
    Else
       HeatRecMdot = Node(HeatRecInNode)%MassFlowRate
    End If
  ELSE
    HeatRecInTemp=0.0d0
    HeatRecCp=0.0d0
    HeatRecMdot=0.0d0
  ENDIF

        !If no loop demand or Generator OFF, return
  IF (.NOT. Runflag) THEN
    CTGenerator(GeneratorNum)%ElecPowerGenerated  = 0.0d0
    CTGenerator(GeneratorNum)%ElecEnergyGenerated  = 0.0d0
    CTGenerator(GeneratorNum)%HeatRecInletTemp    = HeatRecInTemp
    CTGenerator(GeneratorNum)%HeatRecOutletTemp   = HeatRecInTemp
    CTGenerator(GeneratorNum)%HeatRecMdot         = 0.0d0
    CTGenerator(GeneratorNum)%QLubeOilRecovered   = 0.0d0
    CTGenerator(GeneratorNum)%QExhaustRecovered   = 0.0d0
    CTGenerator(GeneratorNum)%QTotalHeatRecovered   = 0.0d0
    CTGenerator(GeneratorNum)%LubeOilEnergyRec   = 0.0d0
    CTGenerator(GeneratorNum)%ExhaustEnergyRec  = 0.0d0
    CTGenerator(GeneratorNum)%TotalHeatEnergyRec  = 0.0d0
    CTGenerator(GeneratorNum)%FuelEnergyUseRate      = 0.0d0
    CTGenerator(GeneratorNum)%FuelEnergy     = 0.0d0
    CTGenerator(GeneratorNum)%FuelMdot      = 0.0d0
    CTGenerator(GeneratorNum)%ExhaustStackTemp    = 0.0d0
    RETURN
  END IF



    ! CALCULATE POWER GENERATED AND PLR
  ElecPowerGenerated = MIN(MyLoad,RatedPowerOutput)
  ElecPowerGenerated = MAX(ElecPowerGenerated,0.0d0)
  PLR = MIN(ElecPowerGenerated/RatedPowerOutput, MaxPartLoadRat)
  PLR = MAX(PLR, MinPartLoadRat)
  ElecPowerGenerated = PLR*RatedPowerOutput


    ! SET OFF-DESIGN AIR TEMPERATURE DIFFERENCE
    !   use OA node if set by user CR7021
  If  (CTGenerator(GeneratorNum)%OAInletNode == 0) then
    AmbientDeltaT = OutDryBulbTemp - DesignAirInletTemp
  ELSE
    AmbientDeltaT = Node(CTGenerator(GeneratorNum)%OAInletNode)%Temp - DesignAirInletTemp
  ENDIF



!Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
!energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
!The TempBasedFuelInputCurve is a correction based on deviation from design inlet air temperature conditions.
!The first coefficient of this fit should be 1.0 to ensure that no correction is made at design conditions.
  FuelUseRate = ElecPowerGenerated * CurveValue(CTGenerator(GeneratorNum)%PLBasedFuelInputCurve, PLR)  * &
                                      CurveValue(CTGenerator(GeneratorNum)%TempBasedFuelInputCurve, AmbientDeltaT)

!Use Curve fit to determine Exhaust Flow.  This curve shows the ratio of exhaust gas flow (kg/s) to electric power
!output (J/s).  The units on ExhaustFlowCurve are (kg/J).  When multiplied by the rated power of the unit,
!it gives the exhaust flow rate in kg/s
  ExhaustFlow = RatedPowerOutput * CurveValue(CTGenerator(GeneratorNum)%ExhaustFlowCurve, AmbientDeltaT)

!Use Curve fit to determine Exhaust Temperature.  This curve calculates the exhaust temperature (C) by
!multiplying the exhaust temperature (C) for a particular part load as given by PLBasedExhaustTempCurve
!a correction factor based on the deviation from design temperature, TempBasedExhaustTempCurve
  IF ((PLR > 0.0d0) .AND. ( (ExhaustFlow > 0.0D0) .or. (MaxExhaustperCTPower > 0.0D0))) THEN

    ExhaustTemp = CurveValue(CTGenerator(GeneratorNum)%PLBasedExhaustTempCurve, PLR)  * &
                  CurveValue(CTGenerator(GeneratorNum)%TempBasedExhaustTempCurve, AmbientDeltaT)

    UA = CTGenerator(GeneratorNum)%UACoef(1) * RatedPowerOutput **  &
                   CTGenerator(GeneratorNum)%UACoef(2)

    DesignMinExitGasTemp = CTGenerator(GeneratorNum)%DesignMinExitGasTemp
    ExhaustStackTemp = DesignMinExitGasTemp + (ExhaustTemp - DesignMinExitGasTemp) / &
                         EXP(UA/(MAX(ExhaustFlow, MaxExhaustperCTPower * RatedPowerOutput) * ExhaustCP))

    QExhaustRec = MAX(ExhaustFlow*ExhaustCP*(ExhaustTemp-ExhaustStackTemp),0.0d0)
  ELSE
    ExhaustStackTemp = CTGenerator(GeneratorNum)%DesignMinExitGasTemp
    QExhaustRec = 0.0d0
  END IF

!Use Curve fit to determine Heat Recovered Lubricant heat.  This curve calculates the lube heat recovered (J/s) by
!multiplying the total power generated by the fraction of that power that could be recovered in the lube oil at that
!particular part load.
  QLubeOilRec = ElecPowerGenerated * CurveValue(CTGenerator(GeneratorNum)%QLubeOilRecoveredCurve, PLR)


!Check for divide by zero
  IF ((HeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
    HeatRecOutTemp = (QExhaustRec + QLubeOilRec)/(HeatRecMdot * HeatRecCp) + HeatRecInTemp
  ELSE
    HeatRecMdot = 0.0d0
    HeatRecOutTemp = HeatRecInTemp
    QExhaustRec =0.0d0
    QLubeOilRec =0.0d0
  END IF



  !Now verify the maximum temperature was not exceeded
  HRecRatio = 1.0d0
  MinHeatRecMdot=0.0d0
  IF(HeatRecOutTemp > CTGenerator(GeneratorNum)%HeatRecMaxTemp) THEN
   IF(CTGenerator(GeneratorNum)%HeatRecMaxTemp /= HeatRecInTemp)THEN
      MinHeatRecMdot = (QExhaustRec + QLubeOilRec)/(HeatRecCp * (CTGenerator(GeneratorNum)%HeatRecMaxTemp - HeatRecInTemp))
      If(MinHeatRecMdot < 0.0d0) MinHeatRecMdot = 0.0d0
   END IF

    !Recalculate Outlet Temperature, with adjusted flowrate
    IF ((MinHeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
      HeatRecOutTemp = (QExhaustRec + QLubeOilRec)/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
      HRecRatio = HeatRecMdot/MinHeatRecMdot
    ELSE
      HeatRecOutTemp = HeatRecInTemp
      HRecRatio = 0.0d0
    END IF
    QLubeOilRec = QLubeOilRec*HRecRatio
    QExhaustRec = QExhaustRec*HRecRatio
  END IF


        !Calculate Energy
  ElectricEnergyGen    = ElecPowerGenerated*TimeStepSys*SecInHour
  FuelEnergyUsed       = FuelUseRate*TimeStepSys*SecInHour
  LubeOilEnergyRec     = QLubeOilRec*TimeStepSys*SecInHour
  ExhaustEnergyRec     = QExhaustRec*TimeStepSys*SecInHour


  CTGenerator(GeneratorNum)%ElecPowerGenerated = ElecPowerGenerated
  CTGenerator(GeneratorNum)%ElecEnergyGenerated = ElectricEnergyGen

  CTGenerator(GeneratorNum)%HeatRecInletTemp = HeatRecInTemp
  CTGenerator(GeneratorNum)%HeatRecOutletTemp = HeatRecOutTemp

  CTGenerator(GeneratorNum)%HeatRecMdot         = HeatRecMdot
  CTGenerator(GeneratorNum)%QExhaustRecovered   = QExhaustRec
  CTGenerator(GeneratorNum)%QLubeOilRecovered   = QLubeOilRec
  CTGenerator(GeneratorNum)%QTotalHeatRecovered = QExhaustRec + QLubeOilRec
  CTGenerator(GeneratorNum)%FuelEnergyUseRate   = ABS(FuelUseRate)
  CTGenerator(GeneratorNum)%ExhaustEnergyRec    = ExhaustEnergyRec
  CTGenerator(GeneratorNum)%LubeOilEnergyRec    = LubeOilEnergyRec
  CTGenerator(GeneratorNum)%TotalHeatEnergyRec  = ExhaustEnergyRec + LubeOilEnergyRec
  CTGenerator(GeneratorNum)%FuelEnergy          = ABS(FuelEnergyUsed)

  FuelHeatingValue = CTGenerator(GeneratorNum)%FuelHeatingValue

  CTGenerator(GeneratorNum)%FuelMdot =  ABS(FuelUseRate)/(FuelHeatingValue  * KJtoJ)

  CTGenerator(GeneratorNum)%ExhaustStackTemp = ExhaustStackTemp

  RETURN
END SUBROUTINE CalcCTGeneratorModel

! End of CT Generator Module Model Subroutines
! *****************************************************************************

! Begin CT Generator Module Utility Subroutines
! *****************************************************************************
SUBROUTINE InitCTGenerators(GeneratorNum, RunFlag, MyLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  Brent Griffith, Sept 2010 plant upgrades, generalize fluid props


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the CT generators.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_Generator_CTurbine
  USE PlantUtilities,  ONLY: SetComponentFlowRate, InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,     INTENT(IN)  :: GeneratorNum    ! Generator number
  LOGICAL,     INTENT(IN)  :: RunFlag         ! TRUE when Generator operating
  REAL(r64)  , INTENT(IN)  :: MyLoad          ! Generator demand
  LOGICAL,     INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: HeatRecInletNode ! inlet node number in heat recovery loop
  INTEGER             :: HeatRecOutletNode ! outlet node number in heat recovery loop
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! Used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeAndNodeInitFlag
  REAL(r64)  :: mdot
  REAL(r64)  :: rho
  LOGICAL    :: errFlag

            ! FLOW:

! Do the one time initializations

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumCTGenerators))
    ALLOCATE(MyPlantScanFlag(NumCTGenerators))
    ALLOCATE(MySizeAndNodeInitFlag(NumCTGenerators))
    MyEnvrnFlag           = .TRUE.
    MyPlantScanFlag       = .TRUE.
    MyOneTimeFlag         = .FALSE.
    MySizeAndNodeInitFlag = .TRUE.
  END IF

  IF (MyPlantScanFlag(GeneratorNum) .AND. ALLOCATED(PlantLoop) &
      .AND. CTGenerator(GeneratorNum)%HeatRecActive) THEN
    errFlag = .FALSE.
    CALL ScanPlantLoopsForObject(CTGenerator(GeneratorNum)%Name, &
                                 TypeOf_Generator_CTurbine, &
                                 CTGenerator(GeneratorNum)%HRLoopNum, &
                                 CTGenerator(GeneratorNum)%HRLoopSideNum, &
                                 CTGenerator(GeneratorNum)%HRBranchNum, &
                                 CTGenerator(GeneratorNum)%HRCompNum , &
                                 errFlag = errFlag )
    If (errFlag) THEN
      CALL ShowFatalError('InitCTGenerators: Program terminated due to previous condition(s).')
    ENDIF
    MyPlantScanFlag(GeneratorNum) = .FALSE.
  ENDIF


  IF (MySizeAndNodeInitFlag(GeneratorNum) .AND. (.NOT. MyPlantScanFlag(GeneratorNum)) &
      .AND.  CTGenerator(GeneratorNum)%HeatRecActive ) THEN
    HeatRecInletNode    = CTGenerator(GeneratorNum)%HeatRecInletNodeNum
    HeatRecOutletNode   = CTGenerator(GeneratorNum)%HeatRecOutletNodeNum

    !size mass flow rate
    rho = GetDensityGlycol(PlantLoop(CTGenerator(GeneratorNum)%HRLoopNum)%FluidName, &
                                     InitConvTemp, &
                                     PlantLoop(CTGenerator(GeneratorNum)%HRLoopNum)%FluidIndex, &
                                     'InitICEngineGenerators')

    CTGenerator(GeneratorNum)%DesignHeatRecMassFlowRate = rho * CTGenerator(GeneratorNum)%DesignHeatRecVolFlowRate

    CALL InitComponentNodes(0.0D0,  CTGenerator(GeneratorNum)%DesignHeatRecMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 CTGenerator(GeneratorNum)%HRLoopNum, &
                                 CTGenerator(GeneratorNum)%HRLoopSideNum, &
                                 CTGenerator(GeneratorNum)%HRBranchNum, &
                                 CTGenerator(GeneratorNum)%HRCompNum )

    MySizeAndNodeInitFlag(GeneratorNum) = .FALSE.
  END IF ! end one time inits

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(GeneratorNum) .AND. CTGenerator(GeneratorNum)%HeatRecActive) THEN
    HeatRecInletNode    = CTGenerator(GeneratorNum)%HeatRecInletNodeNum
    HeatRecOutletNode   = CTGenerator(GeneratorNum)%HeatRecOutletNodeNum
    ! set the node Temperature, assuming freeze control
    Node(HeatRecInletNode)%Temp = 20.0d0
    Node(HeatRecOutletNode)%Temp = 20.0d0
   ! set the node max and min mass flow rates
    CALL InitComponentNodes(0.0D0,  CTGenerator(GeneratorNum)%DesignHeatRecMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 CTGenerator(GeneratorNum)%HRLoopNum, &
                                 CTGenerator(GeneratorNum)%HRLoopSideNum, &
                                 CTGenerator(GeneratorNum)%HRBranchNum, &
                                 CTGenerator(GeneratorNum)%HRCompNum )

    MyEnvrnFlag(GeneratorNum) = .FALSE.
  END IF ! end environmental inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(GeneratorNum) = .TRUE.
  ENDIF

  IF (CTGenerator(GeneratorNum)%HeatRecActive) THEN
    IF ( FirstHVACIteration) Then
      IF (RunFlag) THEN
        mdot =  CTGenerator(GeneratorNum)%DesignHeatRecMassFlowRate
      ELSE
        mdot = 0.d0
      ENDIF
      CALL SetComponentFlowRate(mdot, &
                                   CTGenerator(GeneratorNum)%HeatRecInletNodeNum, &
                                   CTGenerator(GeneratorNum)%HeatRecOutletNodeNum, &
                                   CTGenerator(GeneratorNum)%HRLoopNum, &
                                   CTGenerator(GeneratorNum)%HRLoopSideNum, &
                                   CTGenerator(GeneratorNum)%HRBranchNum, &
                                   CTGenerator(GeneratorNum)%HRCompNum )

    ELSE
      CALL SetComponentFlowRate(   CTGenerator(GeneratorNum)%HeatRecMdot, &
                                   CTGenerator(GeneratorNum)%HeatRecInletNodeNum, &
                                   CTGenerator(GeneratorNum)%HeatRecOutletNodeNum, &
                                   CTGenerator(GeneratorNum)%HRLoopNum, &
                                   CTGenerator(GeneratorNum)%HRLoopSideNum, &
                                   CTGenerator(GeneratorNum)%HRBranchNum, &
                                   CTGenerator(GeneratorNum)%HRCompNum )
    ENDIF
  ENDIF

RETURN
END SUBROUTINE InitCTGenerators
! End CT Generator Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the CT Generator Module
! *****************************************************************************

SUBROUTINE UpdateCTGeneratorRecords(RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if Generator operating
  INTEGER, INTENT(IN)      :: Num       ! Generator number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: HeatRecInletNode
  INTEGER                  :: HeatRecOutletNode

  IF (CTGenerator(Num)%HeatRecActive) THEN
    HeatRecInletNode = CTGenerator(Num)%HeatRecInletNodeNum
    HeatRecOutletNode = CTGenerator(Num)%HeatRecOutletNodeNum

!    Node(HeatRecOutletNode)%MassFlowRate = CTGenerator(Num)%HeatRecMdot
    Node(HeatRecOutletNode)%Temp = CTGenerator(Num)%HeatRecOutletTemp
!    Node(HeatRecOutletNode)%MassFlowRateMaxAvail = Node(HeatRecInletNode)%MassFlowRateMaxAvail
!    Node(HeatRecOutletNode)%MassFlowRateMinAvail = Node(HeatRecInletNode)%MassFlowRateMinAvail

  ENDIF
    CTGeneratorReport(Num)%PowerGen            = CTGenerator(Num)%ElecPowerGenerated
    CTGeneratorReport(Num)%EnergyGen            = CTGenerator(Num)%ElecEnergyGenerated
    CTGeneratorReport(Num)%QExhaustRecovered = CTGenerator(Num)%QExhaustRecovered
    CTGeneratorReport(Num)%QLubeOilRecovered    = CTGenerator(Num)%QLubeOilRecovered
    CTGeneratorReport(Num)%ExhaustEnergyRec = CTGenerator(Num)%ExhaustEnergyRec
    CTGeneratorReport(Num)%LubeOilEnergyRec    = CTGenerator(Num)%LubeOilEnergyRec
    CTGeneratorReport(Num)%QTotalHeatRecovered    = CTGenerator(Num)%QTotalHeatRecovered
    CTGeneratorReport(Num)%TotalHeatEnergyRec    = CTGenerator(Num)%TotalHeatEnergyRec
    CTGeneratorReport(Num)%FuelEnergyUseRate       = CTGenerator(Num)%FuelEnergyUseRate
    CTGeneratorReport(Num)%FuelEnergy       = CTGenerator(Num)%FuelEnergy
    CTGeneratorReport(Num)%FuelMdot       = CTGenerator(Num)%FuelMdot
    CTGeneratorReport(Num)%ExhaustStackTemp     = CTGenerator(Num)%ExhaustStackTemp
    CTGeneratorReport(Num)%HeatRecInletTemp     = CTGenerator(Num)%HeatRecInletTemp
    CTGeneratorReport(Num)%HeatRecOutletTemp    = CTGenerator(Num)%HeatRecOutletTemp
    CTGeneratorReport(Num)%HeatRecMdot          = CTGenerator(Num)%HeatRecMdot


RETURN
END SUBROUTINE UpdateCTGeneratorRecords


SUBROUTINE GetCTGeneratorResults(GeneratorType, GeneratorIndex, &
                                 GeneratorPower,  GeneratorEnergy, ThermalPower, ThermalEnergy)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get some results for load center's aggregation

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator
  INTEGER, INTENT(IN)           :: GeneratorIndex
  REAL(r64), INTENT(OUT)        :: GeneratorPower  ! electrical power
  REAL(r64), INTENT(OUT)        :: GeneratorEnergy ! electrical energy
  REAL(r64), INTENT(OUT)        :: ThermalPower  ! heat power
  REAL(r64), INTENT(OUT)        :: ThermalEnergy ! heat energy
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  GeneratorPower  =  CTGeneratorReport(GeneratorIndex)%PowerGen
  GeneratorEnergy =  CTGeneratorReport(GeneratorIndex)%EnergyGen
  ThermalPower    =  CTGeneratorReport(GeneratorIndex)%QTotalHeatRecovered
  ThermalEnergy   =  CTGeneratorReport(GeneratorIndex)%TotalHeatEnergyRec

  RETURN

END SUBROUTINE GetCTGeneratorResults


! End of Record Keeping subroutines for the CT Generator Module
! *****************************************************************************

END MODULE CTElectricGenerator


!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE MicroturbineElectricGenerator  ! Microturbine Electric Generator Module

          ! MODULE INFORMATION:
          !       AUTHOR         R. Raustad/D. Shirey
          !       DATE WRITTEN   Mar 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !  This module simulates the performance of microturbine electric
          !  generators.

          ! METHODOLOGY EMPLOYED:
          !  Once the electric power manager determines that the MT Generator
          !  is available, it calls SimMTGenerator which in turn calls the
          !  appropriate microturbine generator model.
          !
          !  MT Generator models are based on polynomial curve fits of generator
          !  performance data.

          ! REFERENCES: na

          ! OTHER NOTES: na

          ! USE STATEMENTS:

USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,   ONLY : MaxNameLength, NumOfTimeStepInHour, SecInHour, BeginEnvrnFlag, InitConvTemp
USE DataInterfaces, ONLY : ShowSevereError, ShowWarningError, ShowSevereMessage, ShowWarningMessage, ShowFatalError,  &
                           ShowContinueError, SetupOutputVariable,  &
                           ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
USE DataGlobalConstants, ONLY: iGeneratorMicroturbine


IMPLICIT NONE

PRIVATE
          ! MODULE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! DERIVED TYPE DEFINITIONS:
TYPE MTGeneratorSpecs
!      User inputs
       CHARACTER(len=MaxNameLength) :: Name       = ' '   ! User identifier (name)
       REAL(r64)  :: RefElecPowerOutput           = 0.0d0 ! Reference Electrical Power Output from generator (W)
       REAL(r64)  :: MinElecPowerOutput           = 0.0d0 ! Minimum Electrical Power Output (W)
       REAL(r64)  :: MaxElecPowerOutput           = 0.0d0 ! Maximum Electrical Power Output (W)
       REAL(r64)  :: RefThermalPowerOutput        = 0.0d0 ! Reference Electrical Power Output from generator (W)
       REAL(r64)  :: MinThermalPowerOutput        = 0.0d0 ! Minimum Electrical Power Output (W)
       REAL(r64)  :: MaxThermalPowerOutput        = 0.0d0 ! Maximum Electrical Power Output (W)

       REAL(r64)  :: RefElecEfficiencyLHV         = 0.0d0 ! Reference Electrical Efficiency based on fuel LHV
       REAL(r64)  :: RefCombustAirInletTemp       = 0.0d0 ! Reference Combustion Air Inlet Temperature (C)
       REAL(r64)  :: RefCombustAirInletHumRat     = 0.0d0 ! Reference Combustion Air Inlet Humidity Ratio (kg/kg)
       REAL(r64)  :: RefElevation                 = 0.0d0 ! Reference Elevation (m)
       INTEGER    :: ElecPowFTempElevCurveNum     = 0     ! Curve index for Electrical Power as a function of temp and elev.
       INTEGER    :: ElecEffFTempCurveNum         = 0     ! Curve index for Electrical Efficiency function of temp
       INTEGER    :: ElecEffFPLRCurveNum          = 0     ! Curve index for Electrical Efficiency as a function of PLR
       REAL(r64)  :: FuelHigherHeatingValue       = 0.0d0 ! Higher Heating Value for Fuel (kJ/kg)
       REAL(r64)  :: FuelLowerHeatingValue        = 0.0d0 ! Lower Heating Value for Fuel (kJ/kg)
       REAL(r64)  :: StandbyPower                 = 0.0d0 ! Standby Power entered by user (W)
       REAL(r64)  :: AncillaryPower               = 0.0d0 ! Ancillary Power entered by user (W)
       INTEGER    :: AncillaryPowerFuelCurveNum   = 0     ! Index to ancillary power modifer curve (function of fuel input)
       INTEGER    :: HeatRecInletNodeNum          = 0     ! Heat Recovery Water Inlet Node number
       INTEGER    :: HeatRecOutletNodeNum         = 0     ! Heat Recovery Water Outlet Node number
       REAL(r64)  :: RefThermalEffLHV             = 0.0d0 ! Reference Thermal Efficiency (LHV Basis)
       REAL(r64)  :: RefInletWaterTemp            = 0.0d0 ! Reference Inlet Water Temperature for heat recovery (C)
       LOGICAL    :: InternalFlowControl          = .FALSE. !  A9, \field Heat Recovery Water Flow Operating Mode
       LOGICAL    :: PlantFlowControl             = .TRUE.  !  Default = Plant Control
       REAL(r64)  :: RefHeatRecVolFlowRate        = 0.0d0 ! Reference Heat Recovery Water Flow Rate (m3/s)
       INTEGER    :: HeatRecFlowFTempPowCurveNum  = 0     ! Curve index for Heat Recovery Water Flow Rate function of temp & power
       INTEGER    :: ThermEffFTempElevCurveNum    = 0     ! Curve index for Thermal Efficiency function of temp & elevation
       INTEGER    :: HeatRecRateFPLRCurveNum      = 0     ! Curve index for Heat Recovery Rate function of part-load ratio
       INTEGER    :: HeatRecRateFTempCurveNum     = 0     ! Curve index for Heat Recovery Rate function of inlet water temp
       INTEGER    :: HeatRecRateFWaterFlowCurveNum = 0    ! Curve index for Heat Recovery Rate function of water flow rate
       REAL(r64)  :: HeatRecMinVolFlowRate        = 0.0d0 ! Minimum Heat Recovery Water volume Flow Rate (m3/s)
       REAL(r64)  :: HeatRecMaxVolFlowRate        = 0.0d0 ! Maximum Heat Recovery Water volume Flow Rate (m3/s)
       REAL(r64)  :: HeatRecMaxWaterTemp          = 0.0d0 ! Maximum Heat Recovery Water Temperature (C)
       INTEGER    :: CombustionAirInletNodeNum    = 0     ! Combustion Air Inlet Node number
       INTEGER    :: CombustionAirOutletNodeNum   = 0     ! Combustion Air Outlet (Exhaust) Node number
       LOGICAL    :: ExhAirCalcsActive            = .FALSE. ! Flag to enable exhaust air calculations
       REAL(r64)  :: RefExhaustAirMassFlowRate    = 0.0d0   ! Reference Exhaust Air Mass Flow Rate (kg/s)
       REAL(r64)  :: ExhaustAirMassFlowRate       = 0.0d0   ! Actual Exhaust Air Mass Flow Rate (kg/s)
       INTEGER    :: ExhFlowFTempCurveNum         = 0     ! Curve index for Exhaust Air Flow Rate function of inlet air temp
       INTEGER    :: ExhFlowFPLRCurveNum          = 0     ! Curve index for Exhaust Air Flow Rate function of part-load ratio
       REAL(r64)  :: NomExhAirOutletTemp          = 0.0d0   ! Nominal Exhaust Air Outlet Temperature (C)
       INTEGER    :: ExhAirTempFTempCurveNum      = 0     ! Curve index for Exhaust Air Temperature function of inlet air temp
       INTEGER    :: ExhAirTempFPLRCurveNum       = 0     ! Curve index for Exhaust Air Temperature function of part-load ratio
       REAL(r64)  :: ExhaustAirTemperature        = 0.0d0   ! Combustion exhaust air temperature (C)
       REAL(r64)  :: ExhaustAirHumRat             = 0.0d0   ! Combustion exhaust air humidity ratio (kg/kg)

!      Other required variables/calculated values
       INTEGER    :: CompType_Num                 = iGeneratorMicroturbine
       REAL(r64)  :: RefCombustAirInletDensity    = 0.0d0 ! Reference combustion air inlet density (kg/m3)
       REAL(r64)  :: MinPartLoadRat               = 0.0d0 ! Min allowed operating frac full load
       REAL(r64)  :: MaxPartLoadRat               = 0.0d0 ! Max allowed operating frac full load
       REAL(r64)  :: FuelEnergyUseRateHHV         = 0.0d0 ! Rate of Fuel Energy required to run microturbine, HHV basis (W)
       REAL(r64)  :: FuelEnergyUseRateLHV         = 0.0d0 ! Rate of Fuel Energy required to run microturbine, LHV basis (W)
       REAL(r64)  :: QHeatRecovered               = 0.0d0 ! Recovered exhaust energy rate to heat water  (W)
       REAL(r64)  :: ExhaustEnergyRec             = 0.0d0 ! Recovered exhaust energy to heat water (J)
       REAL(r64)  :: DesignHeatRecMassFlowRate    = 0.0d0 ! Design Water mass flow rate through heat recovery loop (kg/s)
       LOGICAL    :: HeatRecActive                = .FALSE. ! TRUE when heat recovery water inlet and outlet nodes are defined
       REAL(r64)  :: HeatRecInletTemp             = 0.0d0 ! Inlet Temperature of the heat recovery fluid (C)
       REAL(r64)  :: HeatRecOutletTemp            = 0.0d0 ! Outlet Temperature of the heat recovery fluid (C)
       REAL(r64)  :: HeatRecMinMassFlowRate       = 0.0d0 ! Minimum heat recovery water mass flow rate (kg/s)
       REAL(r64)  :: HeatRecMaxMassFlowRate       = 0.0d0 ! Maximum heat recovery water mass flow rate (kg/s)
       REAL(r64)  :: HeatRecMdot                  = 0.0d0 ! Heat Recovery Loop Mass flow rate (kg/s)
       INTEGER    :: HRLoopNum                    = 0     ! cooling water plant loop index number, for heat recovery
       INTEGER    :: HRLoopSideNum                = 0     ! cooling water plant loop side index, for heat recovery
       INTEGER    :: HRBranchNum                  = 0     ! cooling water plant loop branch index, for heat recovery
       INTEGER    :: HRCompNum                    = 0     ! cooling water plant loop component index, for heat recovery

       REAL(r64)  :: FuelMdot                     = 0.0d0 ! Fuel Amount used (kg/s)
       REAL(r64)  :: ElecPowerGenerated           = 0.0d0 ! Electric power generated (W)

       REAL(r64)  :: StandbyPowerRate             = 0.0d0 ! Standby power rate this time step (W)
       REAL(r64)  :: AncillaryPowerRate           = 0.0d0 ! Ancillary power rate this time step (W)

!     Warning message variables
       INTEGER    :: PowerFTempElevErrorIndex     = 0   ! Index to power as a function of temp/elevation warning message
!       INTEGER    :: PowerFTempElevErrorCount     = 0   ! Counter for power as a function of temp/elevation warning messages
       INTEGER    :: EffFTempErrorIndex           = 0   ! Index to efficiency as a function of temperature warning message
!       INTEGER    :: EffFTempErrorCount           = 0   ! Counter for efficiency as a function of temperature warning messages
       INTEGER    :: EffFPLRErrorIndex            = 0   ! Index to efficiency as a function of PLR warning message
!       INTEGER    :: EffFPLRErrorCount            = 0   ! Counter for efficiency as a function of PLR warning messages
       INTEGER    :: ExhFlowFTempErrorIndex       = 0   ! Index to exhaust flow as a function of temp warning message
!       INTEGER    :: ExhFlowFTempErrorCount       = 0   ! Counter for exhaust flow as a function of temp warning messages
       INTEGER    :: ExhFlowFPLRErrorIndex        = 0   ! Index to exhaust flow as a function of PLR warning message
!       INTEGER    :: ExhFlowFPLRErrorCount        = 0   ! Counter for exhaust flow as a function of PLR warning messages
       INTEGER    :: ExhTempFTempErrorIndex       = 0   ! Index to exhaust temp as a function of temp warning message
!       INTEGER    :: ExhTempFTempErrorCount       = 0   ! Counter for exhaust temp as a function of temp warning messages
       INTEGER    :: ExhTempFPLRErrorIndex        = 0   ! Index to exhaust temp as a function of PLR warning message
!       INTEGER    :: ExhTempFPLRErrorCount        = 0   ! Counter for exhaust temp as a function of PLR warning messages
       INTEGER    :: HRMinFlowErrorIndex          = 0   ! Index to reclaim water flow rate warning message
!       INTEGER    :: HRMinFlowErrorCount          = 0   ! Counter for reclaim water flow rate warning messages
       INTEGER    :: HRMaxFlowErrorIndex          = 0   ! Index to reclaim water flow rate warning message
!       INTEGER    :: HRMaxFlowErrorCount          = 0   ! Counter for reclaim water flow rate warning messages
       INTEGER    :: ExhTempLTInletTempIndex      = 0   ! Index to exhaust temp < combustion inlet air temp warning messages
!       INTEGER    :: ExhTempLTInletTempCount      = 0   ! Counter for exhaust temp < combustion inlet air temp warning messages
       INTEGER    :: ExhHRLTInletHRIndex          = 0   ! Index to exhaust hum rat < combustion inlet air hum rat warning messages
!       INTEGER    :: ExhHRLTInletHRCount          = 0   ! Counter for exhaust hum rat < combustion inlet air hum rat warn messages
       INTEGER    :: AnciPowerIterErrorIndex      = 0   ! Index to Ancillary Power iteration loop warning messages
!       INTEGER    :: AnciPowerIterErrorCount      = 0   ! Count for Ancillary Power iteration loop warning messages
       INTEGER    :: AnciPowerFMdotFuelErrorIndex = 0   ! Index to Ancillary Power as a function of fuel input warning messages
!       INTEGER    :: AnciPowerFMdotFuelErrorCount = 0   ! Count for Ancillary Power as a function of fuel input warning messages
       INTEGER    :: HeatRecRateFPLRErrorIndex    = 0   ! Index to heat recovery rate as a function of PLR warning messages
!       INTEGER    :: HeatRecRateFPLRErrorCount    = 0   ! Count for heat recovery rate as a function of PLR warning messages
       INTEGER    :: HeatRecRateFTempErrorIndex   = 0   ! Index to heat recovery rate as a function of temp warning messages
!       INTEGER    :: HeatRecRateFTempErrorCount   = 0   ! Count for heat recovery rate as a function of temp warning messages
       INTEGER    :: HeatRecRateFFlowErrorIndex   = 0   ! Index to heat recovery rate as a function of flow warning messages
!       INTEGER    :: HeatRecRateFFlowErrorCount   = 0   ! Count for heat recovery rate as a function of flow warning messages
       INTEGER    :: ThermEffFTempElevErrorIndex  = 0   ! Index to thermal efficiency as a function of temp/elevation warnings
!       INTEGER    :: ThermEffFTempElevErrorCount  = 0   ! Count for thermal efficiency as a function of temp/elevation warnings

END TYPE MTGeneratorSpecs

TYPE ReportVars
  REAL(r64) :: PowerGen                = 0.0d0 ! Reporting: Electric power produced (W)
  REAL(r64) :: EnergyGen               = 0.0d0 ! Reporting: Electric energy produced (J)
  REAL(r64) :: QHeatRecovered          = 0.0d0 ! Reporting: Heat recovered from exhaust to heat water (W)
  REAL(r64) :: ExhaustEnergyRec        = 0.0d0 ! Reporting: Heat recovered from exhaust to heat water (J)
  REAL(r64) :: FuelEnergyUseRateHHV    = 0.0d0 ! Reporting: Fuel Energy use rate, HHV basis (W)
  REAL(r64) :: FuelEnergyHHV           = 0.0d0 ! Reporting: Fuel Energy used (J)
  REAL(r64) :: FuelMdot                = 0.0d0 ! Reporting: Fuel Amount used (kg/s)
  REAL(r64) :: ElectricEfficiencyLHV   = 0.0d0 ! Reporting: Electric efficiency LHV (-)
  REAL(r64) :: ThermalEfficiencyLHV    = 0.0d0 ! Reporting: Thermal (heat recovery to water) efficiency LHV (-)
  REAL(r64) :: HeatRecInletTemp        = 0.0d0 ! Reporting: Heat Recovery Loop Inlet Temperature (C)
  REAL(r64) :: HeatRecOutletTemp       = 0.0d0 ! Reporting: Heat Recovery Loop Outlet Temperature (C)
  REAL(r64) :: HeatRecMdot             = 0.0d0 ! Reporting: Heat Recovery Loop Mass flow rate (kg/s)
  REAL(r64) :: AncillaryPowerRate      = 0.0d0 ! Reporting: Ancillary power use rate (W)
  REAL(r64) :: AncillaryEnergy         = 0.0d0 ! Reporting: Ancillary energy use (J)
  REAL(r64) :: StandbyPowerRate        = 0.0d0 ! Reporting: Standby power use rate (W)
  REAL(r64) :: StandbyEnergy           = 0.0d0 ! Reporting: Standby energy use (J)
  REAL(r64)  :: ExhAirMassFlowRate       = 0.0d0   ! Actual Exhaust Air Mass Flow Rate (kg/s)
  REAL(r64)  :: ExhAirTemperature        = 0.0d0   ! Combustion exhaust air temperature (C)
END TYPE ReportVars


          ! MODULE VARIABLE DECLARATIONS:
INTEGER            :: NumMTGenerators = 0        ! number of MT Generators specified in input
LOGICAL            :: GetMTInput = .TRUE.! then TRUE, calls subroutine to read input file.

TYPE(MTGeneratorSpecs), ALLOCATABLE, DIMENSION(:) :: MTGenerator  ! dimension to number of generators
TYPE(ReportVars),       ALLOCATABLE, DIMENSION(:) :: MTGeneratorReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


          ! SUBROUTINE SPECIFICATIONS FOR MODULE MicroturbineElectricGenerator
PUBLIC     SimMTGenerator
PRIVATE    GetMTGeneratorInput
PRIVATE    InitMTGenerators
PRIVATE    CalcMTGeneratorModel
PRIVATE    UpdateMTGeneratorRecords
PUBLIC     GetMTGeneratorResults
PUBLIC     SimMTPlantHeatRecovery
PUBLIC GetMTGeneratorExhaustNode

CONTAINS
          ! MODULE SUBROUTINES:
! Beginning of MT Generator Module Driver Subroutine
!*************************************************************************

SUBROUTINE SimMTGenerator(GeneratorType,GeneratorName,GeneratorIndex,RunFlag,MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad/D. Shirey
          !       DATE WRITTEN   Mar 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the MT Generator driver subroutine. It gets the input
          !                             for the model, initializes simulation variables, calls
          !                             the appropriate model and updates reporting variables.

          ! METHODOLOGY EMPLOYED:       Uses empirical models based on manufacturers data

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:

  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: GeneratorType      ! Type of generator !unused1208
  CHARACTER(len=*), INTENT(IN) :: GeneratorName      ! User-specified name of generator
  INTEGER, INTENT(INOUT)       :: GeneratorIndex     ! Index to microturbine generator
  LOGICAL, INTENT(IN)          :: RunFlag            ! Simulate generator when TRUE
  REAL(r64), INTENT(IN)        :: MyLoad             ! Generator demand (W)
  LOGICAL, INTENT (IN)         :: FirstHVACIteration ! Simulation flag for First HVAC (system) iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: GenNum           ! Generator number counter


          ! Get Generator data from input file
  IF (GetMTInput) THEN
    CALL GetMTGeneratorInput
    GetMTInput = .FALSE.
  END IF



        ! SELECT and CALL GENERATOR MODEL
  IF (GeneratorIndex == 0) THEN
    GenNum = FindItemInList(GeneratorName,MTGenerator%Name,NumMTGenerators)
    IF (GenNum == 0) CALL ShowFatalError('SimMTGenerator: Specified Generator not a valid COMBUSTION Turbine Generator '// &
                                       TRIM(GeneratorName))
    GeneratorIndex = GenNum
  ELSE
    GenNum = GeneratorIndex
    IF (GenNum > NumMTGenerators .or. GenNum < 1) THEN
      CALL ShowFatalError('SimMTGenerator: Invalid GeneratorIndex passed = '//TRIM(TrimSigDigits(GenNum))// &
                          ', Number of CT Engine Generators = '//TRIM(TrimSigDigits(NumMTGenerators))//  &
                          ', Generator name = '//TRIM(GeneratorName))
    END IF

    IF (CheckEquipName(GenNum)) THEN
      IF (GeneratorName /= MTGenerator(GenNum)%Name) THEN
        CALL ShowFatalError('SimMTGenerator: Invalid GeneratorIndex passed = '//TRIM(TrimSigDigits(GenNum))// &
                            ', Generator name = '//TRIM(GeneratorName)//', stored Generator Name for that index = '//  &
                            TRIM(MTGenerator(GenNum)%Name))
      END IF
      CheckEquipName(GenNum)=.false.
    ENDIF
  END IF

  CALL InitMTGenerators(GenNum, Runflag, MyLoad, FirstHVACIteration)
  CALL CalcMTGeneratorModel(GenNum,Runflag,MyLoad,FirstHVACIteration)
  CALL UpdateMTGeneratorRecords(GenNum)

  RETURN

END SUBROUTINE SimMTGenerator

SUBROUTINE SimMTPlantHeatRecovery(CompType,CompName,CompTypeNum,CompNum,RunFlag,InitLoopEquip,  &   !DSU
                          MyLoad,MaxCap,MinCap,OptCap,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill data needed in PlantLoopEquipments

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType !unused1208
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(IN)          :: CompTypeNum !unused1208
  INTEGER, INTENT(INOUT)       :: CompNum
  LOGICAL, INTENT(IN)          :: RunFlag !unused1208
 ! INTEGER, INTENT(IN)          :: FlowLock !unused1208 !DSU
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(INOUT)     :: MyLoad !unused1208
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetMTInput) THEN
    CALL GetMTGeneratorInput
    GetMTInput = .FALSE.
  END IF

  IF (InitLoopEquip) THEN
    CompNum = FindItemInList(CompName, MTGenerator%name, NumMTGenerators)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimMTPlantHeatRecovery: Microturbine Generator Unit not found='//TRIM(CompName))
      RETURN
    END IF
      MinCap  = MTGenerator(CompNum)%MinThermalPowerOutput
      MaxCap  = MTGenerator(CompNum)%MaxThermalPowerOutput
      OptCap  = MTGenerator(CompNum)%RefThermalPowerOutput
      RETURN
  END IF ! End Of InitLoopEquip

  RETURN

END SUBROUTINE SimMTPlantHeatRecovery


! End MT Generator Module Driver Subroutine
!******************************************************************************


! Beginning of Microturbine (MT) Generator Module Get Input Subroutine
!******************************************************************************


SUBROUTINE GetMTGeneratorInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR         R. Raustad/D. Shirey
            !       DATE WRITTEN   Mar 2008
            !       MODIFIED       na
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine gets the input information for the Microturbine (MT) Generator model.

            ! METHODOLOGY EMPLOYED:
            !  EnergyPlus input processor.

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE BranchNodeConnections, ONLY: TestCompSet
  USE CurveManager,          ONLY: GetCurveIndex, CurveValue, GetCurveType, GetCurveMinMaxValues
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE General,               ONLY: TrimSigDigits,  RoundSigDigits
  USE Psychrometrics,        ONLY: PsyRhoAirFnPbTdbW
  USE PlantUtilities,        ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE !

            ! PARAMETERS:
            !  na

            ! LOCAL VARIABLES:
  INTEGER       :: GeneratorNum             ! Index to generator
  INTEGER       :: NumAlphas                ! Number of elements in the alpha array
  INTEGER       :: NumNums                  ! Number of elements in the numeric array
  INTEGER       :: IOStat                   ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.FALSE.      ! Error flag... trips fatal error message at end of get input
  LOGICAL       :: IsNotOK                  ! Flag to verify name
  LOGICAL       :: IsBlank                  ! Flag for blank name
  REAL(r64)     :: ElectOutFTempElevOutput  ! Output of Electrical Power Output Modifier Curve (function of temp and elev)
  REAL(r64)     :: ElecEfficFTempOutput     ! Output of Electrical Efficiency Modifier Curve (function of temp)
  REAL(r64)     :: ElecEfficFPLROutput      ! Output of Electrical Efficiency Modifier Curve (function of PLR)
  REAL(r64)     :: AncillaryPowerOutput     ! Output of Ancillary Power Modifer Curve (function of temps and fuel flow)
  REAL(r64)     :: RefFuelUseMdot           ! Fuel mass flow rate at reference conditions (kg/s)
  REAL(r64)     :: RefBaroPressure          ! Reference barometric pressure, adjusted for reference elevation (Pa)
  REAL(r64)     :: ThermalEffTempElevOutput ! Output of Thermal Efficiency Modifier Curve (function of temp and elevation)
  REAL(r64)     :: HeatRecRateFPLROutput    ! Output of Heat Recovery Rate Modifier Curve (function of PLR)
  REAL(r64)     :: HeatRecRateFTempOutput   ! Output of Heat Recovery Rate Modifier Curve (function of inlet water temp)
  REAL(r64)     :: HeatRecRateFFlowOutput   ! Output of Heat Recovery Rate Modifier Curve (function of water flow rate)
  REAL(r64)     :: ExhFlowFTempOutput       ! Output of Exhaust Air Flow Modifier Curve (function of inlet air temp)
  REAL(r64)     :: ExhFlowFPLROutput        ! Output of Exhaust Air Flow Modifier Curve (function of PLR)
  REAL(r64)     :: ExhAirTempFTempOutput    ! Output of Exhaust Air Temperature Modifier Curve (function of inlet air temp)
  REAL(r64)     :: ExhOutAirTempFPLROutput  ! Output of Exhaust Air Temperature Modifier Curve (function of PLR)
  REAL(r64)     :: Var1Min= 0.0d0           ! Minimum value for variable 1, value obtained from a curve object
  REAL(r64)     :: Var1Max= 0.0d0           ! Maximum value for variable 1, value obtained from a curve object

  REAL(r64),                   DIMENSION(19) :: NumArray      ! Numeric data array

  CHARACTER(len=MaxNameLength),DIMENSION(20) :: AlphArray ! Character string data array
  CHARACTER(len=MaxNameLength)               :: FuelType  ! Type of fuel used for generator

         ! FLOW:
  cCurrentModuleObject = 'Generator:MicroTurbine'
  NumMTGenerators = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumMTGenerators <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.TRUE.
  END IF

         ! ALLOCATE ARRAYS
  ALLOCATE (MTGenerator(NumMTGenerators))
  ALLOCATE (MTGeneratorReport(NumMTGenerators))
  ALLOCATE(CheckEquipName(NumMTGenerators))
  CheckEquipName=.true.

         ! LOAD ARRAYS WITH MICROTURBINE GENERATOR DATA
  DO GeneratorNum = 1 , NumMTGenerators
    CALL GetObjectItem(cCurrentModuleObject,GeneratorNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),MTGenerator%Name,GeneratorNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) AlphArray(1)='xxxxx'
    END IF
    MTGenerator(GeneratorNum)%Name               = AlphArray(1)

    MTGenerator(GeneratorNum)%RefElecPowerOutput = NumArray(1)
    IF (MTGenerator(GeneratorNum)%RefElecPowerOutput .LE. 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(NumArray(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' must be greater than 0.')
      ErrorsFound=.TRUE.
    END IF

    MTGenerator(GeneratorNum)%MinElecPowerOutput     = NumArray(2)
    MTGenerator(GeneratorNum)%MaxElecPowerOutput     = NumArray(3)

    IF (MTGenerator(GeneratorNum)%MinElecPowerOutput .LT. 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(NumArray(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' must be greater than 0.')
      ErrorsFound = .TRUE.
    END IF

    IF (lNumericFieldBlanks(3)) THEN
       MTGenerator(GeneratorNum)%MaxElecPowerOutput = MTGenerator(GeneratorNum)%RefElecPowerOutput
    ELSE
      IF (MTGenerator(GeneratorNum)%MaxElecPowerOutput .LE. 0.0d0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(3))//'='//TRIM(RoundSigDigits(NumArray(3),2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' must be greater than 0.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF (MTGenerator(GeneratorNum)%MinElecPowerOutput .GE. MTGenerator(GeneratorNum)%MaxElecPowerOutput) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= '//TRIM(MTGenerator(GeneratorNum)%Name))
       CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' ['//TRIM(RoundSigDigits(NumArray(2),2))//'] > '//  &
             TRIM(cNumericFieldNames(3))//' ['//TRIM(RoundSigDigits(NumArray(3),2))//']')
       CALL ShowContinueError('Minimum Full Load Electrical Power Output must be less than or equal')
       CALL ShowContinueError('to Maximum Full Load Electrical Power Output.')
       ErrorsFound = .TRUE.
    END IF

    IF (MTGenerator(GeneratorNum)%RefElecPowerOutput .GT. MTGenerator(GeneratorNum)%MaxElecPowerOutput .OR. &
        MTGenerator(GeneratorNum)%RefElecPowerOutput .LT. MTGenerator(GeneratorNum)%MinElecPowerOutput) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= '//TRIM(MTGenerator(GeneratorNum)%Name))
       CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' must be >= '//TRIM(cNumericFieldNames(2)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' must be <= '//TRIM(cNumericFieldNames(3)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' = '//TRIM(RoundSigDigits(NumArray(1),2)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(NumArray(2),2)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' = '//TRIM(RoundSigDigits(NumArray(3),2)) )
       ErrorsFound = .TRUE.
    END IF

    MTGenerator(GeneratorNum)%RefElecEfficiencyLHV     = NumArray(4)

    IF (MTGenerator(GeneratorNum)%RefElecEfficiencyLHV .LE. 0.0d0) THEN
       CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(4))//'='//TRIM(RoundSigDigits(NumArray(4),2)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(4))//' must be greater than 0.')
       ErrorsFound = .TRUE.
    END IF

    MTGenerator(GeneratorNum)%RefCombustAirInletTemp   = NumArray(5)
    MTGenerator(GeneratorNum)%RefCombustAirInletHumRat = NumArray(6)
    MTGenerator(GeneratorNum)%RefElevation             = NumArray(7)

    IF (MTGenerator(GeneratorNum)%RefCombustAirInletHumRat .LE. 0.0d0) THEN
       CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(6))//'='//TRIM(RoundSigDigits(NumArray(6),2)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(6))//' must be greater than 0.')
       ErrorsFound = .TRUE.
    ELSE
!      Barometric pressure adjusted for elevation
       RefBaroPressure = 101325.0d0 * (1.d0-2.25577D-05*MTGenerator(GeneratorNum)%RefElevation)**5.2559d0
       MTGenerator(GeneratorNum)%RefCombustAirInletDensity = PsyRhoAirFnPbTdbW(RefBaroPressure, &
                  MTGenerator(GeneratorNum)%RefCombustAirInletTemp,MTGenerator(GeneratorNum)%RefCombustAirInletHumRat)
    END IF

    MTGenerator(GeneratorNum)%ElecPowFTempElevCurveNum = GetCurveIndex(AlphArray(2)) ! Convert curve name to number
    IF (MTGenerator(GeneratorNum)%ElecPowFTempElevCurveNum .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(AlphArray(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ELSE
      ! Verify curve object, only legal type is BiQuadratic
      SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ElecPowFTempElevCurveNum))

      CASE('BIQUADRATIC')
        ! Check electrical power output at reference combustion inlet temp and elevation
        ElectOutFTempElevOutput = CurveValue(MTGenerator(GeneratorNum)%ElecPowFTempElevCurveNum, &
                                         MTGenerator(GeneratorNum)%RefCombustAirInletTemp, &
                                         MTGenerator(GeneratorNum)%RefElevation)
        IF (ABS(ElectOutFTempElevOutput-1.0d0) .GT. 0.1d0) THEN
         CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
         CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//' = '//TRIM(AlphArray(2)))
         CALL ShowContinueError('...Curve output at reference conditions should equal 1 (+-10%).')
         CALL ShowContinueError('...Reference combustion air inlet temperature = ' &
                               //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefCombustAirInletTemp,4))//' C')
         CALL ShowContinueError('...Reference elevation                        = ' &
                               //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefElevation,4))//' m')
         CALL ShowContinueError('...Curve output                               = '//TRIM(TrimSigDigits(ElectOutFTempElevOutput,4)))
        END IF

      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(2))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ElecPowFTempElevCurveNum)))
        CALL ShowContinueError('... Curve type must be BIQUADRATIC.') !TODO rename point (curves)
        ErrorsFound=.TRUE.

      END SELECT

    END IF

    MTGenerator(GeneratorNum)%ElecEffFTempCurveNum = GetCurveIndex(AlphArray(3)) ! Convert curve name to number
    IF (MTGenerator(GeneratorNum)%ElecEffFTempCurveNum .EQ. 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowSevereError(TRIM(cAlphaFieldNames(3))//' not found = '//TRIM(AlphArray(3)))
      ErrorsFound = .TRUE.
    ELSE
      ! Verify curve object, only legal types are Quadratic and Cubic
      SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ElecEffFTempCurveNum))

      CASE('QUADRATIC', 'CUBIC')
        ! Check electrical efficiency at reference combustion inlet temp
        ElecEfficFTempOutput = CurveValue(MTGenerator(GeneratorNum)%ElecEffFTempCurveNum, &
                                        MTGenerator(GeneratorNum)%RefCombustAirInletTemp)
        IF (ABS(ElecEfficFTempOutput-1.0d0) .GT. 0.1d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(3))//' = '//TRIM(AlphArray(3)))
          CALL ShowContinueError('... Curve output at reference condition should equal 1 (+-10%).')
          CALL ShowContinueError('... Reference combustion air inlet temperature = ' &
                                //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefCombustAirInletTemp,4))//' C')
          CALL ShowContinueError('... Curve output                               = '//TRIM(TrimSigDigits(ElecEfficFTempOutput,4)))
        END IF

      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(3))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ElecEffFTempCurveNum)))
        CALL ShowContinueError('Curve type must be QUADRATIC or CUBIC.') !TODO rename point (curves)
        ErrorsFound=.TRUE.

      END SELECT

    END IF

    MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum = GetCurveIndex(AlphArray(4)) ! Convert curve name to number
    IF (MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum .EQ. 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowSevereError(TRIM(cAlphaFieldNames(4))//' not found = '//TRIM(AlphArray(4)))
      ErrorsFound = .TRUE.
    ELSE
      ! Verify curve object, only legal types are Quadratic and Cubic
      SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum))

      CASE('QUADRATIC', 'CUBIC')
        ! Check electrical efficiency at PLR = 1
        ElecEfficFPLROutput = CurveValue(MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum, 1.0d0)
        IF (ABS(ElecEfficFPLROutput-1.0d0) .GT. 0.1d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
          CALL ShowContinueError('... Curve output at a part-load ratio of 1 should equal 1 (+-10%).')
          CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(ElecEfficFPLROutput,4)))
        END IF

        CALL GetCurveMinMaxValues(MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum,Var1Min,Var1Max)
        MTGenerator(GeneratorNum)%MinPartLoadRat     = Var1Min
        MTGenerator(GeneratorNum)%MaxPartLoadRat     = Var1Max

      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(4))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum)))
        CALL ShowContinueError('Curve type must be QUADRATIC or CUBIC.') !TODO rename point (curves)
        ErrorsFound=.TRUE.

      END SELECT

    END IF

    ! Fuel Type case statement
    SELECT CASE (TRIM(AlphArray(5)))
    CASE ('  ') ! If blank, then the default is Natural Gas
      FuelType = 'Gas'

    CASE ('GAS','NATURALGAS','NATURAL GAS')
      FuelType = 'Gas'

!    CASE ('DIESEL')
!      FuelType = 'Diesel'

!    CASE ('GASOLINE')
!      FuelType = 'Gasoline'

!    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
!      FuelType = 'FuelOil#1'

!    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
!      FuelType = 'FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
      FuelType = 'Propane'

!    CASE ('OTHERFUEL1')
!       FuelType = 'OtherFuel1'

!    CASE ('OTHERFUEL2')
!       FuelType = 'OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'  = '//TRIM(AlphArray(5)))
      ErrorsFound=.TRUE.
    END SELECT

    MTGenerator(GeneratorNum)%FuelHigherHeatingValue = NumArray(8)
    MTGenerator(GeneratorNum)%FuelLowerHeatingValue  = NumArray(9)

    IF(MTGenerator(GeneratorNum)%FuelLowerHeatingValue .LE. 0.0d0)THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(9))//'='//TRIM(RoundSigDigits(NumArray(9),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' must be greater than 0.')
      ErrorsFound=.TRUE.
    END IF

    IF(MTGenerator(GeneratorNum)%FuelHigherHeatingValue .LE. 0.0d0)THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(8))//'='//TRIM(RoundSigDigits(NumArray(8),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(8))//' must be greater than 0.')
      ErrorsFound=.TRUE.
    END IF

    IF(MTGenerator(GeneratorNum)%FuelLowerHeatingValue .GT. MTGenerator(GeneratorNum)%FuelHigherHeatingValue)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowContinueError(TRIM(cNumericFieldNames(8))//' must be greater than the '//&
                             TRIM(cNumericFieldNames(9)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(8))//'='//TRIM(RoundSigDigits(NumArray(8),2)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(9))//'='//TRIM(RoundSigDigits(NumArray(9),2)))
      ErrorsFound=.TRUE.
    END IF

    MTGenerator(GeneratorNum)%StandbyPower   = NumArray(10)
    IF( MTGenerator(GeneratorNum)%StandbyPower .LT. 0.0d0)THEN
      CALL ShowWarningError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(NumArray(10),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' must be greater than 0.')
      CALL ShowContinueError('Resetting to 0 and the simulation continues.')
      MTGenerator(GeneratorNum)%StandbyPower = 0.0d0
    END IF

    MTGenerator(GeneratorNum)%AncillaryPower = NumArray(11)
    IF( MTGenerator(GeneratorNum)%AncillaryPower .LT. 0.0d0)THEN
      CALL ShowWarningError('Invalid '//TRIM(cNumericFieldNames(11))//'='//TRIM(RoundSigDigits(NumArray(11),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(11))//' must be greater than 0.')
      CALL ShowContinueError('Resetting to 0 and the simulation continues.')
      MTGenerator(GeneratorNum)%AncillaryPower = 0.0d0
    END IF

    MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum = GetCurveIndex(AlphArray(6)) ! Convert curve name to number
!   If blank, then the calc routine assumes modifier curve value = 1 for entire simulation
    IF (.NOT. lAlphaFieldBlanks(6) .AND. MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(AlphArray(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    ELSE IF(MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum .GT. 0)THEN
      ! Verify curve object, only legal type is Quadratic
      SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum))

      CASE('QUADRATIC')

      IF (MTGenerator(GeneratorNum)%FuelLowerHeatingValue.GT.0.0d0 .AND. &
         MTGenerator(GeneratorNum)%RefElecEfficiencyLHV.GT.0.0d0) THEN

        RefFuelUseMdot = (MTGenerator(GeneratorNum)%RefElecPowerOutput / MTGenerator(GeneratorNum)%RefElecEfficiencyLHV) / &
                      (MTGenerator(GeneratorNum)%FuelLowerHeatingValue  * 1000.0d0)
        AncillaryPowerOutput = CurveValue(MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum, RefFuelUseMdot)
        IF (ABS(AncillaryPowerOutput-1.0d0) .GT. 0.1d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
          CALL ShowContinueError('... Curve output at reference conditions should equal 1 (+-10%).')
          CALL ShowContinueError('... Reference Electrical Power Output           = '// &
                                      TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefElecPowerOutput,2))//' W')
          CALL ShowContinueError('... Reference Electrical Efficiency (LHV basis) = '// &
                                      TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefElecEfficiencyLHV,4)))
          CALL ShowContinueError('... Fuel Lower Heating Value                    = '// &
                                      TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%FuelLowerHeatingValue,2))//' kJ/kg')
          CALL ShowContinueError('... Calculated fuel flow                        = '// &
                                      TRIM(TrimSigDigits(RefFuelUseMdot,4))//' kg/s')
          CALL ShowContinueError('... Curve output                                = '// &
                                      TRIM(TrimSigDigits(AncillaryPowerOutput,4)))
        END IF
      END IF

      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(6))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum)))
        CALL ShowContinueError('... Curve type must be QUADRATIC.')
        ErrorsFound=.TRUE.

      END SELECT

    END IF

    IF (.NOT. lAlphaFieldBlanks(7) ) THEN
      MTGenerator(GeneratorNum)%HeatRecInletNodeNum   = &
                 GetOnlySingleNode(AlphArray(7),ErrorsFound,TRIM(cCurrentModuleObject),MTGenerator(GeneratorNum)%Name, &
                 NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    END IF

    IF (.NOT. lAlphaFieldBlanks(8) ) THEN
      MTGenerator(GeneratorNum)%HeatRecOutletNodeNum   = &
                 GetOnlySingleNode(AlphArray(8),ErrorsFound,TRIM(cCurrentModuleObject),MTGenerator(GeneratorNum)%Name, &
                 NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    END IF

    IF (MTGenerator(GeneratorNum)%HeatRecInletNodeNum .GT. 0 .AND. &
        MTGenerator(GeneratorNum)%HeatRecOutletNodeNum .GT.0) THEN
      CALL TestCompSet(TRIM(cCurrentModuleObject),MTGenerator(GeneratorNum)%Name,AlphArray(7),AlphArray(8),'Heat Recovery Nodes')
    END IF

    IF ( (MTGenerator(GeneratorNum)%HeatRecOutletNodeNum .GT. 0 .AND. MTGenerator(GeneratorNum)%HeatRecInletNodeNum == 0) .OR. &
         (MTGenerator(GeneratorNum)%HeatRecOutletNodeNum == 0 .AND. MTGenerator(GeneratorNum)%HeatRecInletNodeNum .GT. 0) ) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowContinueError('... If one Heat Recovery Water Node Name is specified, then both the Inlet and Outlet Heat Recovery')
      CALL ShowContinueError('... Water Node Names must be specified. Only one water node is being specified for this generator.')
      ErrorsFound=.TRUE.
    END IF

!   Heat recovery to water input fields only valid if water nodes are defined
    IF (MTGenerator(GeneratorNum)%HeatRecInletNodeNum .NE. 0 .AND. &
        MTGenerator(GeneratorNum)%HeatRecOutletNodeNum .NE. 0) THEN

      MTGenerator(GeneratorNum)%HeatRecActive=.TRUE.

      MTGenerator(GeneratorNum)%RefThermalEffLHV  = NumArray(12)
      IF( MTGenerator(GeneratorNum)%RefThermalEffLHV .LT. 0.0d0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(12))//' must be >= 0.')
        CALL ShowContinueError('Resetting to 0 and the simulation continues.')
        MTGenerator(GeneratorNum)%RefThermalEffLHV = 0.0d0
      END IF

      ! Next store thermal power output ranges using nominal thermal to electrical efficiency ratio and electrical power data
      MTGenerator(GeneratorNum)%RefThermalPowerOutput = MTGenerator(GeneratorNum)%RefElecPowerOutput * &
                          MTGenerator(GeneratorNum)%RefThermalEffLHV / MTGenerator(GeneratorNum)%RefElecEfficiencyLHV
      MTGenerator(GeneratorNum)%MinThermalPowerOutput = MTGenerator(GeneratorNum)%MinElecPowerOutput * &
                          MTGenerator(GeneratorNum)%RefThermalEffLHV / MTGenerator(GeneratorNum)%RefElecEfficiencyLHV
      MTGenerator(GeneratorNum)%MaxThermalPowerOutput = MTGenerator(GeneratorNum)%MaxElecPowerOutput * &
                          MTGenerator(GeneratorNum)%RefThermalEffLHV / MTGenerator(GeneratorNum)%RefElecEfficiencyLHV

      MTGenerator(GeneratorNum)%RefInletWaterTemp = NumArray(13)

      IF (SameString(AlphArray(9), 'InternalControl'))  THEN
        MTGenerator(GeneratorNum)%InternalFlowControl = .TRUE. !  A9, \field Heat Recovery Water Flow Operating Mode
        MTGenerator(GeneratorNum)%PlantFlowControl    = .FALSE.
      END IF
      IF ( (.NOT. (SameString(AlphArray(9), 'InternalControl'))) .AND. &
           (.NOT. (SameString(AlphArray(9), 'PlantControl'))) ) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(AlphArray(9)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Operating Mode must be INTERNAL CONTROL or PLANT CONTROL.')
        ErrorsFound = .TRUE.
      END IF

      MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate = NumArray(14)

      IF(MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate .LE. 0.0d0)THEN
        CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(14))//'='//TRIM(RoundSigDigits(NumArray(14),2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError(TRIM(cNumericFieldNames(14))//' must be greater than 0.')
        ErrorsFound = .TRUE.
      END IF

      IF (MTGenerator(GeneratorNum)%InternalFlowControl) THEN ! Get Heat Recovery Water Flow Rate Modifier Curve

        MTGenerator(GeneratorNum)%HeatRecFlowFTempPowCurveNum = GetCurveIndex(AlphArray(10))
        IF (MTGenerator(GeneratorNum)%HeatRecFlowFTempPowCurveNum .NE. 0) THEN
          ! Verify curve object, only legal type is BiQuadratic
          SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%HeatRecFlowFTempPowCurveNum))

          CASE('BIQUADRATIC')
!          NEED TO FIGURE OUT WHAT TO USE FOR Pnet............Shirey
!
!    HeatRecFlowFTempPowCurveOutput = CurveValue(MTGenerator(GeneratorNum)%HeatRecFlowFTempPowCurveNum, Pnet)
!    IF(ABS(HeatRecFlowFTempPowCurveOutput-1.0d0) .GT. 0.1d0)THEN !
!      CALL ShowWarningError('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
!      CALL ShowContinueError('Heat Recovery Water Flow Rate Modifier Curve (function of temp and power) = '//TRIM(AlphArray(10)))
!      CALL ShowContinueError('... Curve ouput at a reference conditions should equal 1 (+-10%).')
!      CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(HeatRecFlowFTempPowCurveOutput,4)))
!    END IF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(10))//' type'// &
                                ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%HeatRecFlowFTempPowCurveNum)))
            CALL ShowContinueError('Curve type must be BIQUADRATIC.')
            ErrorsFound=.TRUE.

          END SELECT

        END IF

      END IF ! End of IF (MTGenerator(GeneratorNum)%InternalFlowControl) THEN

      MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum = GetCurveIndex(AlphArray(11)) ! convert curve name to number
      IF (MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum .NE. 0) THEN
        ! Verify curve object, only legal types are BiQuadratic and BiCubic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum))

        CASE('BIQUADRATIC', 'BICUBIC')

          ThermalEffTempElevOutput = CurveValue(MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum, &
                                                MTGenerator(GeneratorNum)%RefCombustAirInletTemp, &
                                                MTGenerator(GeneratorNum)%RefElevation)

          IF (ABS(ThermalEffTempElevOutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(11))//' = '  //TRIM(AlphArray(11)))
            CALL ShowContinueError('... Curve output at reference conditions should equal 1 (+-10%).')
            CALL ShowContinueError('... Reference combustion air inlet temperature      = ' &
                                  //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefCombustAirInletTemp,4))//' C')
            CALL ShowContinueError('... Reference elevation                             = ' &
                                  //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefElevation,4))//' m')
            CALL ShowContinueError('... Curve output                                    = ' &
                                  //TRIM(TrimSigDigits(ThermalEffTempElevOutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(11))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum)))
          CALL ShowContinueError('Curve type must be BIQUADRATIC or BICUBIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum = GetCurveIndex(AlphArray(12)) ! convert curve name to number
      IF (MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum .NE. 0) THEN
        ! Verify curve object, only legal types are Quadratic or Cubic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum))

        CASE('QUADRATIC', 'CUBIC')

          HeatRecRateFPLROutput = CurveValue(MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum, 1.0d0)

          IF (ABS(HeatRecRateFPLROutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(12))//' = '//TRIM(AlphArray(12)))
            CALL ShowContinueError('... Curve output at a part-load ratio of 1 should equal 1 (+-10%).')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(HeatRecRateFPLROutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(12))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC or CUBIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum = GetCurveIndex(AlphArray(13)) ! convert curve name to number
      IF (MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum .NE. 0) THEN
        ! Verify curve object, only legal type is Quadratic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum))

        CASE('QUADRATIC')

          HeatRecRateFTempOutput = CurveValue(MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum, &
                                              MTGenerator(GeneratorNum)%RefInletWaterTemp)

          IF (ABS(HeatRecRateFTempOutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(13))//' = ' //TRIM(AlphArray(13)))
            CALL ShowContinueError('... Curve output at reference condition should equal 1 (+-10%).')
            CALL ShowContinueError('... Reference inlet water temperature temperature      = ' &
                                  //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefInletWaterTemp,4))//' C')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(HeatRecRateFTempOutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(13))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum = GetCurveIndex(AlphArray(14))
      IF (MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum .NE. 0) THEN
        ! Verify curve object, only legal type is Quadratic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum))

        CASE('QUADRATIC')

          HeatRecRateFFlowOutput = CurveValue(MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum, &
                                              MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate)

          IF (ABS(HeatRecRateFFlowOutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(14))//' = ' //TRIM(AlphArray(14)))
            CALL ShowContinueError('... Curve output at reference condition should equal 1 (+-10%).')
            CALL ShowContinueError('... Reference Heat Recovery Water Flow Rate      = ' &
                                  //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate,4))//' m3/s')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(HeatRecRateFFlowOutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(14))//' type'// &
                             ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate = NumArray(15)
      IF (MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate .LT. 0.0d0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(15))//' must be >= 0.')
        CALL ShowContinueError('Resetting to 0 and the simulation continues.')
        MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate = 0.0d0
      END IF

      MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate = NumArray(16)
      IF (MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate .LT. 0.0d0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(16))//' must be >= 0.')
        CALL ShowContinueError('Resetting to 0 and the simulation continues.')
        MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate = 0.0d0
      END IF

      IF (MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate .LT. MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(16))//' must be >= '//TRIM(cNumericFieldNames(15)))
        CALL ShowContinueError('Resetting '//TRIM(cNumericFieldNames(16))//' = '//TRIM(cNumericFieldNames(15))// &
                               ' and the simulation continues.')
        MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate = MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate
      END IF

!     Check if reference heat recovery water flow rate is below the minimum flow rate
      IF (MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate .LT. MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(14))//' must be >= '//TRIM(cNumericFieldNames(15)))
        CALL ShowContinueError('Resetting '//TRIM(cNumericFieldNames(14))//' = '//TRIM(cNumericFieldNames(15))// &
                               ' and the simulation continues.')
        MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate = MTGenerator(GeneratorNum)%HeatRecMinVolFlowRate
      END IF

!     Check if reference heat recovery water flow rate is above the maximum flow rate
      IF (MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate .GT. MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(14))//' must be <= '//TRIM(cNumericFieldNames(16)))
        CALL ShowContinueError('Resetting '//TRIM(cNumericFieldNames(14))//' = '//TRIM(cNumericFieldNames(16))// &
                               ' and the simulation continues.')
        MTGenerator(GeneratorNum)%RefHeatRecVolFlowRate = MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate
      END IF


      CALL RegisterPlantCompDesignFlow( MTGenerator(GeneratorNum)%HeatRecInletNodeNum ,&
                                 MTGenerator(GeneratorNum)%HeatRecMaxVolFlowRate)

      MTGenerator(GeneratorNum)%HeatRecMaxWaterTemp = NumArray(17)

    END IF  ! End of 'IF (MTGenerator(GeneratorNum)%HeatRecInletNodeNum .NE. 0 .AND. &
            !             MTGenerator(GeneratorNum)%HeatRecOutletNodeNum .NE. 0) THEN'


    IF (.NOT. lAlphaFieldBlanks(15) ) THEN
      MTGenerator(GeneratorNum)%CombustionAirInletNodeNum   = &
                  GetOnlySingleNode(AlphArray(15),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                  NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)
    END IF

!    Combustion air inlet node must be an outside air node
    IF (.NOT. lAlphaFieldBlanks(15) .AND. .not. CheckOutAirNodeNumber(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(15))//' is not a valid Outdoor Air Node = '//TRIM(AlphArray(15)))
      CALL ShowContinueError('it does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
      ErrorsFound=.TRUE.
    END IF

    IF (.NOT. lAlphaFieldBlanks(16)) THEN
      MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum = &
                  GetOnlySingleNode(AlphArray(16),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
                  NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent)
    END IF

    IF (MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum .GT. 0 .AND. &
        MTGenerator(GeneratorNum)%CombustionAirInletNodeNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
      CALL ShowContinueError('A '//TRIM(cAlphaFieldNames(15))//' must be specified when a '//TRIM(cAlphaFieldNames(16))// &
                              ' is specified.')
      ErrorsFound=.TRUE.
    END IF

!   Get other exhaust air inputs only if combustion air inlet and outlet nodes are valid
    IF (MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum .GT. 0 .AND. &
        MTGenerator(GeneratorNum)%CombustionAirInletNodeNum .GT. 0) THEN

      MTGenerator(GeneratorNum)%ExhAirCalcsActive = .TRUE.
      MTGenerator(GeneratorNum)%RefExhaustAirMassFlowRate = NumArray(18)
      IF (MTGenerator(GeneratorNum)%RefExhaustAirMassFlowRate .LE. 0.0d0 .AND. &
          .NOT. lNumericFieldBlanks(18)) THEN
        CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(18))//'='//TRIM(RoundSigDigits(NumArray(18),2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError(TRIM(cNumericFieldNames(18))//' must be greater than 0.')
        ErrorsFound=.TRUE.
      END IF

      MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum = GetCurveIndex(AlphArray(17))
      IF (MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum .NE. 0) THEN
        ! Verify curve object, only legal types are Quadratic and Cubic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum))

        CASE('QUADRATIC', 'CUBIC')

          ExhFlowFTempOutput = CurveValue(MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum, &
                                          MTGenerator(GeneratorNum)%RefCombustAirInletTemp)

          IF (ABS(ExhFlowFTempOutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(17))//' = ' //TRIM(AlphArray(17)))
            CALL ShowContinueError('... Curve output at reference condition should equal 1 (+-10%).')
            CALL ShowContinueError('... Reference combustion air inlet temperature      = ' &
                                  //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefCombustAirInletTemp,4))//' C')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(ExhFlowFTempOutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(17))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC or CUBIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum = GetCurveIndex(AlphArray(18)) ! convert curve name to number
      IF (MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum .NE. 0) THEN
        ! Verify curve object, legal types are Quadratic or Cubic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum))

        CASE('QUADRATIC', 'CUBIC')

          ExhFlowFPLROutput = CurveValue(MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum, 1.0d0)

          IF (ABS(ExhFlowFPLROutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(18))//' = ' //TRIM(AlphArray(18)))
            CALL ShowContinueError('... Curve output at a part-load ratio of 1 should equal 1 (+-10%).')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(ExhFlowFPLROutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(18))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC or CUBIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%NomExhAirOutletTemp = NumArray(19)

      MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum = GetCurveIndex(AlphArray(19))
      IF (MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum .NE. 0) THEN
        ! Verify curve object, only legal types are Quadratic and Cubic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum))

        CASE('QUADRATIC', 'CUBIC')

          ExhAirTempFTempOutput = CurveValue(MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum, &
                                                MTGenerator(GeneratorNum)%RefCombustAirInletTemp)

          IF (ABS(ExhAirTempFTempOutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(19))//' = ' //TRIM(AlphArray(19)))
            CALL ShowContinueError('... Curve output at reference condition should equal 1 (+-10%).')
            CALL ShowContinueError('... Reference combustion air inlet temperature      = ' &
                                  //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%RefCombustAirInletTemp,4))//' C')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(ExhAirTempFTempOutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(19))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC or CUBIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

      MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum = GetCurveIndex(AlphArray(20)) ! convert curve name to number
      IF (MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum .NE. 0) THEN
        ! Verify curve object, legal types are Quadratic or Cubic
        SELECT CASE(GetCurveType(MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum))

        CASE('QUADRATIC', 'CUBIC')

          ExhOutAirTempFPLROutput = CurveValue(MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum, 1.0d0)

          IF (ABS(ExhOutAirTempFPLROutput-1.0d0) .GT. 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cAlphaFieldNames(20))//' = ' //TRIM(AlphArray(20)))
            CALL ShowContinueError('... Curve output at a part-load ratio of 1 should equal 1 (+-10%).')
            CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(ExhOutAirTempFPLROutput,4)))
          END IF

        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(20))//' type'// &
                               ' for this object = '//TRIM(GetCurveType(MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum)))
          CALL ShowContinueError('... Curve type must be QUADRATIC or CUBIC.')
          ErrorsFound=.TRUE.

        END SELECT

      END IF

    END IF ! End of '    IF (MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum .GT. 0 .AND. &
           !                 MTGenerator(GeneratorNum)%CombustionAirInletNodeNum .GT. 0) THEN

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  END IF

  DO GeneratorNum = 1, NumMTGenerators
     CALL SetupOutputVariable('Generator Produced Electric Power [W]', &
          MTGeneratorReport(GeneratorNum)%PowerGen,'System','Average',MTGenerator(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator Produced Electric Energy [J]', &
          MTGeneratorReport(GeneratorNum)%EnergyGen,'System','Sum',MTGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')

     CALL SetupOutputVariable('Generator LHV Basis Electric Efficiency []', &
          MTGeneratorReport(GeneratorNum)%ElectricEfficiencyLHV,'System','Average',MTGenerator(GeneratorNum)%Name)

!    Fuel specific report variables
     CALL SetupOutputVariable('Generator '// TRIM(FuelType)//' HHV Basis Rate [W]', &
          MTGeneratorReport(GeneratorNum)%FuelEnergyUseRateHHV,'System','Average',MTGenerator(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator '// TRIM(FuelType)//' HHV Basis Energy [J]', &
          MTGeneratorReport(GeneratorNum)%FuelEnergyHHV,'System','Sum',MTGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey=FuelType,EndUseKey='COGENERATION',GroupKey='Plant')

     CALL SetupOutputVariable('Generator '// TRIM(FuelType)//' Mass Flow Rate [kg/s]', &
          MTGeneratorReport(GeneratorNum)%FuelMdot,'System','Average',MTGenerator(GeneratorNum)%Name)

!    general fuel use report (to match other generators)
     CALL SetupOutputVariable('Generator Fuel HHV Basis Rate [W]', &
          MTGeneratorReport(GeneratorNum)%FuelEnergyUseRateHHV,'System','Average',MTGenerator(GeneratorNum)%Name)

     CALL SetupOutputVariable('Generator Fuel HHV Basis Energy [J]', &
          MTGeneratorReport(GeneratorNum)%FuelEnergyHHV,'System','Sum',MTGenerator(GeneratorNum)%Name)

!    Heat recovery (to water) report variables
     IF (MTGenerator(GeneratorNum)%HeatRecActive) THEN

       CALL SetupOutputVariable('Generator Produced Thermal Rate [W]', &
            MTGeneratorReport(GeneratorNum)%QHeatRecovered,'System','Average',MTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', &
            MTGeneratorReport(GeneratorNum)%ExhaustEnergyRec,'System','Sum',MTGenerator(GeneratorNum)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Generator Thermal Efficiency LHV Basis []', &
            MTGeneratorReport(GeneratorNum)%ThermalEfficiencyLHV,'System','Average',MTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Heat Recovery Inlet Temperature [C]', &
          MTGeneratorReport(GeneratorNum)%HeatRecInletTemp,'System','Average',MTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Heat Recovery Outlet Temperature [C]', &
          MTGeneratorReport(GeneratorNum)%HeatRecOutletTemp,'System','Average',MTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Heat Recovery Water Mass Flow Rate [kg/s]', &
          MTGeneratorReport(GeneratorNum)%HeatRecMdot,'System','Average',MTGenerator(GeneratorNum)%Name)

     END IF

     IF (MTGenerator(GeneratorNum)%StandbyPower .GT. 0.0d0) THEN ! Report Standby Power if entered by user
       CALL SetupOutputVariable('Generator Standby Electric Power [W]', &
            MTGeneratorReport(GeneratorNum)%StandbyPowerRate,'System','Average',MTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Standby Electric Energy [J]', &
            MTGeneratorReport(GeneratorNum)%StandbyEnergy,'System','Sum',MTGenerator(GeneratorNum)%Name, &
                           ResourceTypeKey='Electricity',EndUseKey='Cogeneration',GroupKey='Plant')
     END IF

     IF (MTGenerator(GeneratorNum)%AncillaryPower .GT. 0.0d0) THEN ! Report Ancillary Power if entered by user
       CALL SetupOutputVariable('Generator Ancillary Electric Power [W]', &
            MTGeneratorReport(GeneratorNum)%AncillaryPowerRate,'System','Average',MTGenerator(GeneratorNum)%Name)

       CALL SetupOutputVariable('Generator Ancillary Electric Energy [J]', &
            MTGeneratorReport(GeneratorNum)%AncillaryEnergy,'System','Sum',MTGenerator(GeneratorNum)%Name)
     END IF
!   Report combustion air outlet conditions if exhaust air calculations are active
   IF (MTGenerator(GeneratorNum)%ExhAirCalcsActive) THEN
       CALL SetupOutputVariable('Generator Exhaust Air Mass Flow Rate [kg/s]', &
     MTGeneratorReport(GeneratorNum)%ExhAirMassFlowRate ,'System','Average',MTGenerator(GeneratorNum)%Name)
     CALL SetupOutputVariable('Generator Exhaust Air Temperature  [C]', &
       MTGeneratorReport(GeneratorNum)%ExhAirTemperature,'System','Average',MTGenerator(GeneratorNum)%Name)
    ENDIF

  END DO

  RETURN

END SUBROUTINE GetMTGeneratorInput

! End of Get Input subroutine for the MT Generator Module
!******************************************************************************

! Begin MT Generator Module Initialize Subroutine
! *****************************************************************************

SUBROUTINE InitMTGenerators(GenNum, RunFlag, MyLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad/D. Shirey
          !       DATE WRITTEN   Mar 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, Sept 2010, plant upgrades, general fluid props

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for initializations of the CT generators.

          ! METHODOLOGY EMPLOYED:
          !  Uses the status flags to trigger initializations.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: GetDensityGlycol
  USE CurveManager, ONLY: CurveValue
  USE DataPlant,    ONLY: PlantLoop,ScanPlantLoopsForObject, TypeOf_Generator_MicroTurbine
  USE PlantUtilities,  ONLY: SetComponentFlowRate, InitComponentNodes


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GenNum
  LOGICAL, INTENT(IN) :: RunFlag !
  REAL(r64), INTENT(IN) :: MyLoad ! electrical load in W
  Logical, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: Num                        ! Loop index over all generators
  INTEGER             :: HeatRecInletNode           ! Inlet node number in heat recovery loop
  INTEGER             :: HeatRecOutletNode          ! Outlet node number in heat recovery loop
  LOGICAL,SAVE        :: InitGeneratorOnce = .TRUE. ! Flag for 1 time initialization
  LOGICAL,SAVE, ALLOCATABLE, DIMENSION(:)  :: MyEnvrnFlag  ! Flag for init once at start of environment
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeAndNodeInitFlag
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag
  REAL(r64)           :: rho ! local temporary fluid density
  REAL(r64)           :: DesiredMassFlowRate
  LOGICAL             :: errFlag

          ! FLOW:
  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE (MyEnvrnFlag    (NumMTGenerators))
    ALLOCATE (MyPlantScanFlag(NumMTGenerators))
    ALLOCATE(MySizeAndNodeInitFlag(NumMTGenerators))
    MyEnvrnFlag           = .TRUE.
    MyPlantScanFlag       = .TRUE.
    MySizeAndNodeInitFlag = .TRUE.
    MyOneTimeFlag         = .FALSE.
  ENDIF

  IF (MyPlantScanFlag(GenNum) .AND. ALLOCATED(PlantLoop) &
      .AND. MTGenerator(GenNum)%HeatRecActive) THEN
    errFlag = .FALSE.
    CALL ScanPlantLoopsForObject(MTGenerator(GenNum)%Name, &
                                 TypeOf_Generator_MicroTurbine, &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum, &
                                 errFlag = errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitMTGenerators: Program terminated due to previous condition(s).')
    ENDIF

    MyPlantScanFlag(GenNum) = .FALSE.
  ENDIF

  IF (MySizeAndNodeInitFlag(GenNum) .AND. (.NOT. MyPlantScanFlag(GenNum)) &
      .AND.  MTGenerator(GenNum)%HeatRecActive ) THEN


    HeatRecInletNode    = MTGenerator(GenNum)%HeatRecInletNodeNum
    HeatRecOutletNode   = MTGenerator(GenNum)%HeatRecOutletNodeNum

    !size mass flow rate
    rho = GetDensityGlycol(PlantLoop(MTGenerator(GenNum)%HRLoopNum)%FluidName, &
                                     InitConvTemp, &
                                     PlantLoop(MTGenerator(GenNum)%HRLoopNum)%FluidIndex, &
                                     'InitMTGenerators')

    MTGenerator(GenNum)%DesignHeatRecMassFlowRate = rho * MTGenerator(GenNum)%RefHeatRecVolFlowRate
    MTGenerator(GenNum)%HeatRecMaxMassFlowRate = rho * MTGenerator(GenNum)%HeatRecMaxVolFlowRate

    CALL InitComponentNodes(0.0D0,  MTGenerator(GenNum)%HeatRecMaxMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum )

    MySizeAndNodeInitFlag(GenNum) = .FALSE.

  END IF ! end one time inits

  IF (.not. MTGenerator(GenNum)%HeatRecActive) RETURN

  HeatRecInletNode    = MTGenerator(GenNum)%HeatRecInletNodeNum
  HeatRecOutletNode   = MTGenerator(GenNum)%HeatRecOutletNodeNum
  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(GenNum)) THEN
      ! set the node max and min mass flow rates
    CALL InitComponentNodes(0.0D0,  MTGenerator(GenNum)%HeatRecMaxMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum )

    Node(HeatRecInletNode)%Temp = 20.0d0 ! Set the node temperature, assuming freeze control
    Node(HeatRecOutletNode)%Temp = 20.0d0

    MyEnvrnFlag(GenNum) = .FALSE.
  END IF ! end environmental inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(GenNum) =.TRUE.
  END IF

  ! set/request flow rates
  IF (FirstHVACIteration) THEN

    IF (.NOT. RunFlag) THEN
      DesiredMassFlowRate = 0.d0

    ELSEIF (RunFlag .AND. MTGenerator(GenNum)%InternalFlowControl) THEN
      ! assume dispatch power in MyLoad is what gets produced (future, reset during calc routine and iterate)
      IF (MTGenerator(GenNum)%HeatRecFlowFTempPowCurveNum .NE. 0) THEN
        DesiredMassFlowRate = MTGenerator(GenNum)%DesignHeatRecMassFlowRate &
                              * CurveValue(MTGenerator(GenNum)%HeatRecFlowFTempPowCurveNum, &
                               Node(HeatRecInletNode)%Temp, &
                               MyLoad)
      ELSE
        DesiredMassFlowRate = MTGenerator(GenNum)%DesignHeatRecMassFlowRate ! Assume modifier = 1 if curve not specified
      END IF

      DesiredMassFlowRate = MAX(constant_zero,  DesiredMassFlowRate) ! protect from neg. curve result

    ELSEIF (RunFlag .AND. (.NOT. MTGenerator(GenNum)%InternalFlowControl)) THEN
      DesiredMassFlowRate = MTGenerator(GenNum)%DesignHeatRecMassFlowRate
    END IF

    CALL SetComponentFlowRate(DesiredMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum )
  ELSE ! not FirstHVACIteration
    IF (.NOT. RunFlag) THEN
      Node(HeatRecInletNode)%MassFlowRate = MIN(constant_zero, Node(HeatRecInletNode)%MassFlowRateMaxAvail)
      Node(HeatRecInletNode)%MassFlowRate = MAX(constant_zero, Node(HeatRecInletNode)%MassFlowRateMinAvail)

    ELSE IF (RunFlag .AND. MTGenerator(GenNum)%InternalFlowControl) THEN
      ! assume dispatch power in MyLoad is what gets produced (future, reset during calc routine and iterate)
      IF (MTGenerator(GenNum)%HeatRecFlowFTempPowCurveNum .NE. 0) THEN
        DesiredMassFlowRate = MTGenerator(GenNum)%DesignHeatRecMassFlowRate &
                              * CurveValue(MTGenerator(GenNum)%HeatRecFlowFTempPowCurveNum, &
                               Node(HeatRecInletNode)%Temp, &
                               MyLoad)
        CALL SetComponentFlowRate(DesiredMassFlowRate,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum )
      ELSE
        CALL SetComponentFlowRate(MTGenerator(GenNum)%HeatRecMdot,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum )
      END IF
    ELSE IF (RunFlag .AND. (.NOT. MTGenerator(GenNum)%InternalFlowControl)) THEN
        CALL SetComponentFlowRate(MTGenerator(GenNum)%HeatRecMdot,  &
                                 HeatRecInletNode,        &
                                 HeatRecOutletNode,       &
                                 MTGenerator(GenNum)%HRLoopNum, &
                                 MTGenerator(GenNum)%HRLoopSideNum, &
                                 MTGenerator(GenNum)%HRBranchNum, &
                                 MTGenerator(GenNum)%HRCompNum )
    ENDIF
  END IF

  RETURN

END SUBROUTINE InitMTGenerators

!  End of MT Generator Module Initialize Subroutine
! *****************************************************************************


!  Beginning of MT Generator Model Calculation Subroutine
! *****************************************************************************

SUBROUTINE CalcMTGeneratorModel(GeneratorNum,Runflag,MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad/D. Shirey
          !       DATE WRITTEN   Mar 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate a combustion generator.

          ! METHODOLOGY EMPLOYED:
          !  Curve fits of performance data.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: FirstTimeStepSysFlag
  USE DataEnvironment, ONLY: OutDryBulbTemp, OutHumRat, OutBaroPress, Elevation
  USE CurveManager,    ONLY: CurveValue
  USE Psychrometrics,  ONLY: PsyHFnTdbW, PsyCpAirFnWTdb, PsyHfgAirFnWTdb, PsyRhoAirFnPbTdbW
  USE General,         ONLY: TrimSigDigits
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop


  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: MyLoad                    ! Generator demand (W)
  INTEGER, INTENT(IN)    :: GeneratorNum              ! Generator number
  LOGICAL, INTENT(IN)    :: RunFlag                   ! TRUE when generator is being asked to operate
  LOGICAL, INTENT(IN)    :: FirstHVACIteration !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER   :: KJtoJ = 1000.0d0          ! Convert kilojoules to joules
  INTEGER,   PARAMETER   :: MaxAncPowerIter = 50      ! Maximum number of iteration (subroutine ancillary power iteration loop)
  REAL(r64), PARAMETER   :: AncPowerDiffToler = 5.0d0 ! Tolerance for Ancillary Power Difference (W)
  REAL(r64), PARAMETER   :: RelaxFactor = 0.7d0       ! Relaxation factor for iteration loop

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MinPartLoadRat          ! Min allowed operating fraction at full load
  REAL(r64) :: MaxPartLoadRat          ! Max allowed operating fraction at full load
  REAL(r64) :: ReferencePowerOutput    ! Generator reference capacity (W)
  REAL(r64) :: RefElecEfficiency       ! Reference electrical efficiency
  REAL(r64) :: OperatingElecEfficiency ! Actual operating efficiency
  REAL(r64) :: ElecEfficiencyFTemp     ! Electrical efficiency as a function of temperature curve output
  REAL(r64) :: ElecEfficiencyFPLR      ! Electrical efficiency as a function of PLR curve output
  REAL(r64) :: ThermalEffFTempElev     ! Thermal efficiency as a function of air temperature and elevation
  REAL(r64) :: PLR                     ! Generator operating part load ratio
  REAL(r64) :: PowerFTempElev          ! Power ratio as a function of inlet air temperature and elevation
  REAL(r64) :: CombustionAirInletTemp  ! Combustion air inlet temperature (C)
  REAL(r64) :: CombustionAirInletPress ! Barometric pressure of combustion inlet air (Pa)
  REAL(r64) :: CombustionAirInletW     ! Combustion air inlet humidity ratio (kg/kg)
  REAL(r64) :: ExhFlowFTemp            ! Exhaust air flow rate as a function of temperature curve output
  REAL(r64) :: ExhFlowFPLR             ! Exhaust air flow rate as a function of part-load ratio curve output
  REAL(r64) :: ExhAirMassFlowRate      ! Actual exhaust air mass flow rate (accounting for temp and PLR modifier curves)
  REAL(r64) :: ExhAirTempFTemp         ! Exhaust air temperature as a function of inlet air temp curve output
  REAL(r64) :: ExhAirTempFPLR          ! Exhaust air temperature as a function of part-load ratio curve output
  REAL(r64) :: ExhaustAirTemp          ! Actual exhaust air temperature (accounting for temp and PLR modifier curves)
  REAL(r64) :: CpAir                   ! Heat capacity of air (J/kg-C)
  REAL(r64) :: H2OHtOfVap              ! Heat of vaporization of water (J/kg)
  REAL(r64) :: ActualElevation         ! Actual elevation of the microturbine (m)
  REAL(r64) :: AirDensity              ! Density of air at actual combustion inlet air conditions (kg/m3)

  REAL(r64) :: ElecPowerGenerated      ! Generator electric power output (W)
  REAL(r64) :: FullLoadPowerOutput     ! Generator full-load power output at actual inlet conditions and elevation (W)

  REAL(r64) :: FuelUseEnergyRateLHV    ! Rate of fuel energy required to run microturbine, LHV basis (W)
  REAL(r64) :: QHeatRecToWater         ! Recovered waste heat to water (W)
  REAL(r64) :: MinHeatRecMdot          ! Heat recovery flow rate if minimal heat recovery is accomplished (kg/s)
  INTEGER   :: HeatRecInNode           ! Heat recovery fluid inlet node number
  REAL(r64) :: HeatRecInTemp           ! Heat recovery fluid inlet temperature (C)
  REAL(r64) :: HeatRecOutTemp          ! Heat recovery fluid outlet temperature (C)
  REAL(r64) :: HeatRecMdot             ! Heat recovery fluid mass flow rate (kg/s)
  REAL(r64) :: HeatRecVolFlowRate      ! Heat recovery fluid flow rate (m3/s)
  REAL(r64) :: HeatRecCp               ! Specific heat of the heat recovery fluid (J/kg-K)
  REAL(r64) :: HeatRecRateFPLR         ! Heat recovery rate as a function of PLR curve output
  REAL(r64) :: HeatRecRateFTemp        ! Heat recovery rate as a function of inlet water temp curve output
  REAL(r64) :: HeatRecRateFFlow        ! Heat recovery rate as a function of water flow rate curve output
  REAL(r64) :: FuelHigherHeatingValue  ! Higher heating value (HHV) of fuel (kJ/kg)
  REAL(r64) :: FuelLowerHeatingValue   ! Lower heating value (LLV) of fuel kJ/kg)
  REAL(r64) :: HRecRatio               ! When maximum temperature is reached the amount of recovered heat has to be reduced
  REAL(r64) :: AncillaryPowerRate      ! Ancillary power used by pump (if not specified in manufacturers data)
  REAL(r64) :: AncillaryPowerRateLast  ! Ancillary power used by pump from last iteration (iteration loop within this subroutine)
  REAL(r64) :: AncillaryPowerRateDiff  ! Difference between ancillary power rate and ancillary power rate last (last iteration)
  REAL(r64) :: AnciPowerFMdotFuel      ! Ancillary power as a function of fuel flow curve output
  INTEGER   :: AncPowerCalcIterIndex   ! Index for subroutine iteration loop if Ancillary Power (function of fuel flow) is used
  REAL(r64) :: rho                     ! local fluid density


!   Load local variables from data structure (for code readability)
    MinPartLoadRat       = MTGenerator(GeneratorNum)%MinPartLoadRat
    MaxPartLoadRat       = MTGenerator(GeneratorNum)%MaxPartLoadRat
    ReferencePowerOutput = MTGenerator(GeneratorNum)%RefElecPowerOutput
    RefElecEfficiency    = MTGenerator(GeneratorNum)%RefElecEfficiencyLHV

!   Initialize variables
    MTGenerator(GeneratorNum)%ElecPowerGenerated     = 0.0d0
    MTGenerator(GeneratorNum)%HeatRecInletTemp       = 0.0d0
    MTGenerator(GeneratorNum)%HeatRecOutletTemp      = 0.0d0
    MTGenerator(GeneratorNum)%HeatRecMdot            = 0.0d0
    MTGenerator(GeneratorNum)%QHeatRecovered         = 0.0d0
    MTGenerator(GeneratorNum)%ExhaustEnergyRec       = 0.0d0
    MTGenerator(GeneratorNum)%FuelEnergyUseRateHHV   = 0.0d0
    MTGenerator(GeneratorNum)%FuelMdot               = 0.0d0
    MTGenerator(GeneratorNum)%AncillaryPowerRate     = 0.0d0
    MTGenerator(GeneratorNum)%StandbyPowerRate       = 0.0d0
    MTGenerator(GeneratorNum)%FuelEnergyUseRateLHV   = 0.0d0
    MTGenerator(GeneratorNum)%ExhaustAirMassFlowRate = 0.0d0
    MTGenerator(GeneratorNum)%ExhaustAirTemperature  = 0.0d0
    MTGenerator(GeneratorNum)%ExhaustAirHumRat       = 0.0d0
    ExhAirTempFTemp                                  = 0.0d0
    QHeatRecToWater                                  = 0.0d0

    IF (MTGenerator(GeneratorNum)%HeatRecActive) THEN
      HeatRecInNode = MTGenerator(GeneratorNum)%HeatRecInletNodeNum
      HeatRecInTemp = Node(HeatRecInNode)%Temp
      HeatRecCp =  GetSpecificHeatGlycol(PlantLoop(MTGenerator(GeneratorNum)%HRLoopNum)%FluidName, &
                                 HeatRecInTemp, &
                                 PlantLoop(MTGenerator(GeneratorNum)%HRLoopNum)%FluidIndex, &
                                 'CalcMTGeneratorModel')
      HeatRecMdot = Node(HeatRecInNode)%MassFlowRate
    ELSE
      HeatRecInTemp=0.0d0
      HeatRecCp=0.0d0
      HeatRecMdot=0.0d0
    END IF

!   Set combustion inlet air temperature, humidity ratio and pressure local variables
    IF  (MTGenerator(GeneratorNum)%CombustionAirInletNodeNum == 0) THEN ! no inlet air node specified, so use weather file values
      CombustionAirInletTemp = OutDryBulbTemp
      CombustionAirInletW = OutHumRat
      CombustionAirInletPress = OutBaroPress
      ActualElevation = Elevation ! from DataEnvironment
    ELSE ! use inlet node information
      CombustionAirInletTemp = Node(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)%Temp
      CombustionAirInletW = Node(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)%HumRat
      CombustionAirInletPress = Node(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)%Press
      ActualElevation = Elevation ! from DataEnvironment
      IF (Node(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)%Height .GT. 0.0d0) THEN
        ActualElevation = Elevation +  Node(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)%Height
      END IF
!     Initialize combustion outlet air conditions to inlet air conditions (all node properties)
      IF (MTGenerator(GeneratorNum)%ExhAirCalcsActive) THEN
        Node(MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum) = Node(MTGenerator(GeneratorNum)%CombustionAirInletNodeNum)
      END IF
    END IF

!   If no loop demand or generator OFF, set some variables and then return
!    IF (.NOT. Runflag .OR. MyLoad .LE. 0.0d0) THEN
    IF (MyLoad .LE. 0.0d0) THEN
      MTGenerator(GeneratorNum)%HeatRecInletTemp      = HeatRecInTemp
      MTGenerator(GeneratorNum)%HeatRecOutletTemp     = HeatRecInTemp
      IF (Runflag) THEN
        MTGenerator(GeneratorNum)%StandbyPowerRate    = MTGenerator(GeneratorNum)%StandbyPower
      END IF
      MTGenerator(GeneratorNum)%ExhaustAirTemperature = CombustionAirInletTemp
      MTGenerator(GeneratorNum)%ExhaustAirHumRat      = CombustionAirInletW
      RETURN
    END IF

!   Calculate power modifier curve value (function of inlet air temperature and elevation)
    PowerFTempElev = CurveValue(MTGenerator(GeneratorNum)%ElecPowFTempElevCurveNum, CombustionAirInletTemp, Elevation)

!   Warn user if power modifier curve output is less than 0
    IF (PowerFTempElev .LT. 0.0d0) THEN
      IF (MTGenerator(GeneratorNum)%PowerFTempElevErrorIndex == 0) THEN
!        MTGenerator(GeneratorNum)%PowerFTempElevErrorCount = MTGenerator(GeneratorNum)%PowerFTempElevErrorCount + 1
        CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError('... Electrical Power Modifier curve (function of temperature and elevation) output is '// &
                               'less than zero ('//TRIM(TrimSigDigits(PowerFTempElev,4))//').')
        CALL ShowContinueError('... Value occurs using a combustion inlet air temperature of ' &
                           //TRIM(TrimSigDigits(CombustionAirInletTemp,2))//' C.')
        CALL ShowContinueError('... and an elevation of '//TRIM(TrimSigDigits(Elevation,2))//' m.')
        CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
          ' Electrical Power Modifier curve is less than zero warning continues...' &
          , MTGenerator(GeneratorNum)%PowerFTempElevErrorIndex, PowerFTempElev, PowerFTempElev)
      PowerFTempElev = 0.0d0
    END IF

!   Calculate available full-load power output. cannot exceed maximum full-load power output.
    FullLoadPowerOutput = MIN((ReferencePowerOutput * PowerFTempElev),MTGenerator(GeneratorNum)%MaxElecPowerOutput)
!   Also can't be below the minimum full-load power output.
    FullLoadPowerOutput = MAX(FullLoadPowerOutput,MTGenerator(GeneratorNum)%MinElecPowerOutput)

    AncillaryPowerRate     = MTGenerator(GeneratorNum)%AncillaryPower
    AncillaryPowerRateLast = AncillaryPowerRate
    AncillaryPowerRateDiff = AncPowerDiffToler + 1.0d0 ! Initialize to force through DO WHILE Loop at least once
    AncPowerCalcIterIndex  = 0 ! Initialize iteration index (counter)

    DO WHILE (AncillaryPowerRateDiff .GT. AncPowerDiffToler .AND. AncPowerCalcIterIndex .LE. MaxAncPowerIter)

      AncPowerCalcIterIndex = AncPowerCalcIterIndex + 1 ! Increment iteration loop counter

!     Calculate operating power output (gross)
      ElecPowerGenerated = MIN( MAX(0.0d0,MyLoad + AncillaryPowerRate), FullLoadPowerOutput )

!     Calculate PLR, but must be between the minPLR and maxPLR
      IF (FullLoadPowerOutput .GT. 0.0d0) THEN
        PLR = MIN(ElecPowerGenerated/FullLoadPowerOutput, MaxPartLoadRat)
        PLR = MAX(PLR, MinPartLoadRat)
      ELSE
        PLR = 0.0d0
      END IF

!     Recalculate ElecPowerGenerated based on "final" PLR
      ElecPowerGenerated = FullLoadPowerOutput * PLR

!     Calculate electrical efficiency modifier curve output (function of temp)
      ElecEfficiencyFTemp = CurveValue(MTGenerator(GeneratorNum)%ElecEffFTempCurveNum, CombustionAirInletTemp)

!     Warn user if efficiency modifier curve output is less than 0
      IF (ElecEfficiencyFTemp .LT. 0.0d0) THEN
        IF (MTGenerator(GeneratorNum)%EffFTempErrorIndex == 0) THEN
!          MTGenerator(GeneratorNum)%EffFTempErrorCount = MTGenerator(GeneratorNum)%EffFTempErrorCount + 1
          CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... Electrical Efficiency Modifier (function of temperature) output is less than ' &
                            //'zero ('//TRIM(TrimSigDigits(ElecEfficiencyFTemp,4))//').')
          CALL ShowContinueError('... Value occurs using a combustion inlet air temperature of ' &
                             //TRIM(TrimSigDigits(CombustionAirInletTemp,2))//' C.')
          CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Electrical Efficiency Modifier (function of temperature) output is less than zero warning continues...' &
            , MTGenerator(GeneratorNum)%EffFTempErrorIndex, ElecEfficiencyFTemp, ElecEfficiencyFTemp)
        ElecEfficiencyFTemp = 0.0d0
      END IF

!     Calculate efficiency modifier curve output (function of PLR)
      ElecEfficiencyFPLR  = CurveValue(MTGenerator(GeneratorNum)%ElecEffFPLRCurveNum, PLR)

!     Warn user if efficiency modifier curve output is less than 0
      IF (ElecEfficiencyFPLR .LT. 0.0d0) THEN
        IF (MTGenerator(GeneratorNum)%EffFPLRErrorIndex == 0) THEN
          CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('... Electrical Efficiency Modifier (function of part-load ratio) output is less than'// &
                                 ' zero ('//TRIM(TrimSigDigits(ElecEfficiencyFPLR,4))//').')
          CALL ShowContinueError('... Value occurs using a part-load ratio of ' &
                             //TRIM(TrimSigDigits(PLR,3))//'.')
          CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Electrical Efficiency Modifier (function of part-load ratio) output is less than zero warning'// &
            ' continues...', MTGenerator(GeneratorNum)%EffFPLRErrorIndex, ElecEfficiencyFPLR, ElecEfficiencyFPLR)
        ElecEfficiencyFPLR = 0.0d0
      END IF

!     Calculate operating electrical efficiency
      OperatingElecEfficiency = RefElecEfficiency * ElecEfficiencyFTemp * ElecEfficiencyFPLR

!     Calculate fuel use (W = J/s), LHV basis
      IF (OperatingElecEfficiency .GT. 0.0d0) THEN
        FuelUseEnergyRateLHV = ElecPowerGenerated / OperatingElecEfficiency
      ELSE
        FuelUseEnergyRateLHV = 0.0d0  ! If fuel use rate is zero, then
        ElecPowerGenerated   = 0.0d0  !  electric power generated must be zero.
      END IF

!     Set fuel heating values
      FuelHigherHeatingValue = MTGenerator(GeneratorNum)%FuelHigherHeatingValue
      FuelLowerHeatingValue  = MTGenerator(GeneratorNum)%FuelLowerHeatingValue

!     Calculate fuel mass flow rate
      MTGenerator(GeneratorNum)%FuelMdot = FuelUseEnergyRateLHV / (FuelLowerHeatingValue * KJtoJ)

!     Calculate ancillary power requirement
      IF (MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum .GT. 0) THEN
        AnciPowerFMdotFuel = CurveValue(MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum, MTGenerator(GeneratorNum)%FuelMdot)
!       Warn user if ancillary power modifier curve output is less than 0
        IF (AnciPowerFMdotFuel .LT. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%AnciPowerFMdotFuelErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('... Ancillary Power Modifier (function of fuel input) output is less than'// &
                                   ' zero ('//TRIM(TrimSigDigits(AnciPowerFMdotFuel,4))//').')
            CALL ShowContinueError('... Value occurs using a fuel input mass flow rate of ' &
                                   //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%FuelMdot,4))//' kg/s.')
            CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
              ' Ancillary Power Modifier (function of fuel input) output is less than zero warning'// &
              ' continues...', MTGenerator(GeneratorNum)%AnciPowerFMdotFuelErrorIndex, AnciPowerFMdotFuel, AnciPowerFMdotFuel)
          AnciPowerFMdotFuel = 0.0d0
        END IF
      ELSE
        AnciPowerFMdotFuel = 1.0d0
      END IF

      AncillaryPowerRateLast = AncillaryPowerRate

      IF (MTGenerator(GeneratorNum)%AncillaryPowerFuelCurveNum .GT. 0) THEN
          AncillaryPowerRate = RelaxFactor * MTGenerator(GeneratorNum)%AncillaryPower * AnciPowerFMdotFuel - &
                               (1.0d0 - RelaxFactor) * AncillaryPowerRateLast
      END IF

      AncillaryPowerRateDiff = ABS(AncillaryPowerRate - AncillaryPowerRateLast)

    END DO

    IF (AncPowerCalcIterIndex .GT. MaxAncPowerIter) THEN

      IF (MTGenerator(GeneratorNum)%AnciPowerIterErrorIndex == 0) THEN
        CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
        CALL ShowContinueError('... Iteration loop for electric power generation is not converging within tolerance.')
        CALL ShowContinueError('... Check the Ancillary Power Modifier Curve (function of fuel input).')
        CALL ShowContinueError('... Ancillary Power = '//TRIM(TrimSigDigits(AncillaryPowerRate,1))//' W.')
        CALL ShowContinueError('... Fuel input rate = '//TRIM(TrimSigDigits(AnciPowerFMdotFuel,4))//' kg/s.')
        CALL ShowContinueErrorTimeStamp('... Simulation will continue.')
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Iteration loop for electric power generation is not converging within tolerance continues...', &
              MTGenerator(GeneratorNum)%AnciPowerIterErrorIndex)

    END IF

!   Calculate electrical power generated
    MTGenerator(GeneratorNum)%ElecPowerGenerated = ElecPowerGenerated - AncillaryPowerRate

!   Report fuel energy use rate on HHV basis, which is the unit of measure when the fuel is sold
    MTGenerator(GeneratorNum)%FuelEnergyUseRateHHV  = MTGenerator(GeneratorNum)%FuelMdot * FuelHigherHeatingValue * KJtoJ
    MTGenerator(GeneratorNum)%AncillaryPowerRate    = AncillaryPowerRate  ! Move to data structure for later reporting
    MTGenerator(GeneratorNum)%FuelEnergyUseRateLHV  = FuelUseEnergyRateLHV ! Move to data structure for reporting calculations

!   When generator operates, standby losses are 0
    MTGenerator(GeneratorNum)%StandbyPowerRate = 0.0d0

!   Calculate heat recovery if active
    IF (MTGenerator(GeneratorNum)%HeatRecActive) THEN

      IF (MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum .GT. 0) THEN
         ThermalEffFTempElev = CurveValue(MTGenerator(GeneratorNum)%ThermEffFTempElevCurveNum, CombustionAirInletTemp, Elevation)
!       Warn user if power modifier curve output is less than 0
        IF (ThermalEffFTempElev .LT. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%ThermEffFTempElevErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('... Electrical Power Modifier curve (function of temperature and elevation) output is '// &
                                   'less than zero ('//TRIM(TrimSigDigits(PowerFTempElev,4))//').')
            CALL ShowContinueError('... Value occurs using a combustion inlet air temperature of ' &
                               //TRIM(TrimSigDigits(CombustionAirInletTemp,2))//' C.')
            CALL ShowContinueError('... and an elevation of '//TRIM(TrimSigDigits(Elevation,2))//' m.')
            CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
              ' Electrical Power Modifier curve is less than zero warning continues...' &
              , MTGenerator(GeneratorNum)%ThermEffFTempElevErrorIndex, ThermalEffFTempElev, ThermalEffFTempElev)
          ThermalEffFTempElev = 0.0d0
        END IF
      ELSE
         ThermalEffFTempElev = 1.0d0  ! If no curve provided, assume multiplier factor = 1.0
      END IF

      QHeatRecToWater = FuelUseEnergyRateLHV * MTGenerator(GeneratorNum)%RefThermalEffLHV * ThermalEffFTempElev

!     Calculate heat recovery rate modifier curve output (function of PLR)
      IF (MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum .GT. 0) THEN
        HeatRecRateFPLR  = CurveValue(MTGenerator(GeneratorNum)%HeatRecRateFPLRCurveNum, PLR)
!       Warn user if heat recovery modifier curve output is less than 0
        IF (HeatRecRateFPLR .LT. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%HeatRecRateFPLRErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('... Heat Recovery Rate Modifier (function of part-load ratio) output is less than'// &
                                   ' zero ('//TRIM(TrimSigDigits(HeatRecRateFPLR,4))//').')
            CALL ShowContinueError('... Value occurs using a part-load ratio of ' &
                               //TRIM(TrimSigDigits(PLR,3))//'.')
            CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
              ' Heat Recovery Rate Modifier (function of part-load ratio) output is less than zero warning'// &
              ' continues...', MTGenerator(GeneratorNum)%HeatRecRateFPLRErrorIndex, HeatRecRateFPLR, HeatRecRateFPLR)
          HeatRecRateFPLR = 0.0d0
        END IF
      ELSE
        HeatRecRateFPLR = 1.0d0  ! If no curve provided, assume multiplier factor = 1.0
      END IF

!     Calculate heat recovery rate modifier curve output (function of inlet water temp)
      IF (MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum .GT. 0) THEN
        HeatRecRateFTemp  = CurveValue(MTGenerator(GeneratorNum)%HeatRecRateFTempCurveNum, HeatRecInTemp)
        IF (HeatRecRateFTemp .LT. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%HeatRecRateFTempErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('... Heat Recovery Rate Modifier (function of inlet water temp) output is less than ' &
                              //'zero ('//TRIM(TrimSigDigits(HeatRecRateFTemp,4))//').')
            CALL ShowContinueError('... Value occurs using an inlet water temperature temperature of ' &
                               //TRIM(TrimSigDigits(HeatRecInTemp,2))//' C.')
            CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
                 ' Heat Recovery Rate Modifier (function of inlet water temp) output is less than zero warning continues...' &
                 , MTGenerator(GeneratorNum)%HeatRecRateFTempErrorIndex, HeatRecRateFTemp, HeatRecRateFTemp)
          HeatRecRateFTemp = 0.0d0
        END IF
      ELSE
        HeatRecRateFTemp = 1.0d0  ! If no curve provided, assume multiplier factor = 1.0
      END IF

!     Calculate heat recovery rate modifier curve output (function of water [volumetric] flow rate)
      IF (MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum .GT. 0) THEN
        rho = GetDensityGlycol(PlantLoop(MTGenerator(GeneratorNum)%HRLoopNum)%FluidName, &
                                 HeatRecInTemp, &
                                 PlantLoop(MTGenerator(GeneratorNum)%HRLoopNum)%FluidIndex, &
                                 'CalcMTGeneratorModel')

        HeatRecVolFlowRate = HeatRecMdot / rho
        HeatRecRateFFlow  = CurveValue(MTGenerator(GeneratorNum)%HeatRecRateFWaterFlowCurveNum,HeatRecVolFlowRate)
        IF (HeatRecRateFFlow .LT. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%HeatRecRateFFlowErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('... Heat Recovery Rate Modifier (function of water flow rate) output is less than ' &
                              //'zero ('//TRIM(TrimSigDigits(HeatRecRateFFlow,4))//').')
            CALL ShowContinueError('... Value occurs using a water flow rate of ' &
                               //TRIM(TrimSigDigits(HeatRecVolFlowRate,4))//' m3/s.')
            CALL ShowContinueErrorTimeStamp('... Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
              ' Heat Recovery Rate Modifier (function of water flow rate) output is less than zero warning continues...' &
              , MTGenerator(GeneratorNum)%HeatRecRateFFlowErrorIndex, HeatRecRateFFlow, HeatRecRateFFlow)
          HeatRecRateFFlow = 0.0d0
        END IF
      ELSE
        HeatRecRateFFlow = 1.0d0  ! If no curve provided, assume multiplier factor = 1.0
      END IF

      QHeatRecToWater = QHeatRecToWater * HeatRecRateFPLR * HeatRecRateFTemp * HeatRecRateFFlow

!     Check for divide by zero
      IF ((HeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
        HeatRecOutTemp =  HeatRecInTemp + QHeatRecToWater/ (HeatRecMdot * HeatRecCp)
      ELSE
        HeatRecMdot     = 0.0d0
        HeatRecOutTemp  = HeatRecInTemp
        QHeatRecToWater = 0.0d0
      END IF

!     Now verify the maximum heat recovery temperature was not exceeded
      HRecRatio = 1.0d0
      MinHeatRecMdot=0.0d0
      IF (HeatRecOutTemp > MTGenerator(GeneratorNum)%HeatRecMaxWaterTemp) THEN

        IF (MTGenerator(GeneratorNum)%HeatRecMaxWaterTemp /= HeatRecInTemp) THEN
          MinHeatRecMdot = QHeatRecToWater/(HeatRecCp * (MTGenerator(GeneratorNum)%HeatRecMaxWaterTemp - HeatRecInTemp))
          IF (MinHeatRecMdot < 0.0d0) MinHeatRecMdot = 0.0d0
        END IF

!       Recalculate outlet water temperature with minimum flow rate (will normally match the max water outlet temp,
!       unless the inlet water temp is greater than the max outlet temp)
        IF ((MinHeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
          HeatRecOutTemp = QHeatRecToWater/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
          HRecRatio = HeatRecMdot/MinHeatRecMdot
        ELSE
          HeatRecOutTemp = HeatRecInTemp
          HRecRatio = 0.0d0
        END IF
        QHeatRecToWater = QHeatRecToWater*HRecRatio ! Scale heat recovery rate using HRecRatio. Don't adjust flow rate.

      END IF

!     Check water mass flow rate against minimum
      IF (MTGenerator(GeneratorNum)%HeatRecMinMassFlowRate .GT. HeatRecMdot .AND. HeatRecMdot .GT. 0.0d0) THEN
        IF (MTGenerator(GeneratorNum)%HRMinFlowErrorIndex == 0) THEN
          CALL ShowWarningError('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('...Heat reclaim water flow rate is below the generators minimum mass flow rate of (' &
                            //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%HeatRecMinMassFlowRate,4))//').')
          CALL ShowContinueError('...Heat reclaim water mass flow rate = '//TRIM(TrimSigDigits(HeatRecMdot,4))//'.')
          CALL ShowContinueErrorTimeStamp('...Check inputs for heat recovery water flow rate.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
               ' Heat recovery water flow rate is below the generators minimum mass flow rate warning continues...' &
               , MTGenerator(GeneratorNum)%HRMinFlowErrorIndex, HeatRecMdot, HeatRecMdot)
      END IF

!     Check water mass flow rate against maximum
      IF (HeatRecMdot .GT. MTGenerator(GeneratorNum)%HeatRecMaxMassFlowRate .AND. HeatRecMdot .GT. 0.0d0) THEN
        IF (MTGenerator(GeneratorNum)%HRMaxFlowErrorIndex == 0) THEN
          CALL ShowWarningError('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('...Heat reclaim water flow rate is above the generators maximum mass flow rate of (' &
                            //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%HeatRecMaxMassFlowRate,4))//').')
          CALL ShowContinueError('...Heat reclaim water mass flow rate = '//TRIM(TrimSigDigits(HeatRecMdot,4))//'.')
          CALL ShowContinueErrorTimeStamp('...Check inputs for heat recovery water flow rate.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
               ' Heat recovery water flow rate is above the generators maximum mass flow rate warning continues...' &
               , MTGenerator(GeneratorNum)%HRMaxFlowErrorIndex, HeatRecMdot, HeatRecMdot)
      END IF

!     Set report variables
      MTGenerator(GeneratorNum)%HeatRecInletTemp   = HeatRecInTemp
      MTGenerator(GeneratorNum)%HeatRecOutletTemp  = HeatRecOutTemp
      MTGenerator(GeneratorNum)%HeatRecMdot        = HeatRecMdot
      MTGenerator(GeneratorNum)%QHeatRecovered     = QHeatRecToWater

    END IF ! End of  IF (MTGenerator(GeneratorNum)%HeatRecActive) THEN

!   Calculate combustion air outlet conditions if exhaust air calculations are active
    IF (MTGenerator(GeneratorNum)%ExhAirCalcsActive) THEN

      IF (MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum .NE. 0) THEN  ! Exhaust Flow Rate versus Inlet Air Temp
        ExhFlowFTemp = CurveValue(MTGenerator(GeneratorNum)%ExhFlowFTempCurveNum,CombustionAirInletTemp)
!       Warn user if exhaust modifier curve output is less than or equal to 0
        IF (ExhFlowFTemp .LE. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%ExhFlowFTempErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('...Exhaust Air Flow Rate Modifier (function of temperature) output is less than or equal '&
                              //'to zero ('//TRIM(TrimSigDigits(ExhFlowFTemp,4))//').')
            CALL ShowContinueError('...Value occurs using a combustion inlet air temperature of ' &
                             //TRIM(TrimSigDigits(CombustionAirInletTemp,2))//'.')
            CALL ShowContinueErrorTimeStamp('...Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Exhaust Air Flow Rate Modifier (function of temperature) output is less than or equal to zero warning continues...'&
            , MTGenerator(GeneratorNum)%ExhFlowFTempErrorIndex, ExhFlowFTemp, ExhFlowFTemp)
          ExhFlowFTemp = 0.0d0
        END IF
      ELSE
        ExhFlowFTemp = 1.0d0 ! No curve input means modifier = 1.0 always
      END IF

      IF (MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum .NE. 0) THEN  ! Exhaust Flow Rate versus Part-Load Ratio
        ExhFlowFPLR = CurveValue(MTGenerator(GeneratorNum)%ExhFlowFPLRCurveNum,PLR)
!       Warn user if exhaust modifier curve output is less than or equal to 0
        IF (ExhFlowFPLR .LE. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%ExhFlowFPLRErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('...Exhaust Air Flow Rate Modifier (function of part-load ratio) output is less than or '&
                              //'equal to zero ('//TRIM(TrimSigDigits(ExhFlowFPLR,4))//').')
            CALL ShowContinueError('...Value occurs using a part-load ratio of ' &
                             //TRIM(TrimSigDigits(PLR,2))//'.')
            CALL ShowContinueErrorTimeStamp('...Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Exhaust Air Flow Rate Modifier (function of part-load ratio) output is less than or equal to zero warning'&
             //' continues...', MTGenerator(GeneratorNum)%ExhFlowFPLRErrorIndex, ExhFlowFPLR, ExhFlowFPLR)
          ExhFlowFPLR = 0.0d0
        END IF
      ELSE
        ExhFlowFPLR = 1.0d0 ! No curve input means modifier = 1.0 always
      END IF

!     Calculate exhaust air mass flow, accounting for temperature and PLR modifier factors
      ExhAirMassFlowRate = MTGenerator(GeneratorNum)%RefExhaustAirMassFlowRate * ExhFlowFTemp * ExhFlowFPLR
!     Adjust for difference in air density at reference conditions versus actual inlet air conditions
      AirDensity = PsyRhoAirFnPbTdbW(CombustionAirInletPress,CombustionAirInletTemp,CombustionAirInletW)
      IF (MTGenerator(GeneratorNum)%RefCombustAirInletDensity .GE. 0.0d0) THEN
        ExhAirMassFlowRate = MAX(0.0d0,ExhAirMassFlowRate*AirDensity/MTGenerator(GeneratorNum)%RefCombustAirInletDensity)
      ELSE
        ExhAirMassFlowRate = 0.0d0
      END IF
      MTGenerator(GeneratorNum)%ExhaustAirMassFlowRate = ExhAirMassFlowRate


      IF (MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum .NE. 0) THEN  ! Exhaust Air Temp versus Inlet Air Temp
        ExhAirTempFTemp = CurveValue(MTGenerator(GeneratorNum)%ExhAirTempFTempCurveNum,CombustionAirInletTemp)
!       Warn user if exhaust modifier curve output is less than or equal to 0
        IF (ExhAirTempFTemp .LE. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%ExhTempFTempErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('...Exhaust Air Temperature Modifier (function of temperature) output is less than or equal '&
                              //'to zero ('//TRIM(TrimSigDigits(ExhAirTempFTemp,4))//').')
            CALL ShowContinueError('...Value occurs using a combustion inlet air temperature of ' &
                             //TRIM(TrimSigDigits(CombustionAirInletTemp,2))//'.')
            CALL ShowContinueErrorTimeStamp('...Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Exhaust Air Temperature Modifier (function of temperature) output is less than or equal to zero'//&
            ' warning continues...', MTGenerator(GeneratorNum)%ExhTempFTempErrorIndex, ExhAirTempFTemp, ExhAirTempFTemp)
          ExhAirTempFTemp = 0.0d0
        END IF
      ELSE
        ExhAirTempFTemp = 1.0d0 ! No curve input means modifier = 1.0 always
      END IF

      IF (MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum .NE. 0) THEN  ! Exhaust Air Temp versus Part-Load Ratio
        ExhAirTempFPLR = CurveValue(MTGenerator(GeneratorNum)%ExhAirTempFPLRCurveNum,PLR)
!       Warn user if exhaust modifier curve output is less than or equal to 0
        IF (ExhAirTempFPLR .LE. 0.0d0) THEN
          IF (MTGenerator(GeneratorNum)%ExhTempFPLRErrorIndex == 0) THEN
            CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
            CALL ShowContinueError('...Exhaust Air Temperature Modifier (function of part-load ratio) output is less than or '&
                              //'equal to zero ('//TRIM(TrimSigDigits(ExhAirTempFPLR,4))//').')
            CALL ShowContinueError('...Value occurs using a part-load ratio of ' &
                             //TRIM(TrimSigDigits(PLR,2))//'.')
            CALL ShowContinueErrorTimeStamp('...Resetting curve output to zero and continuing simulation.')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'//&
            ' Exhaust Air Temperature Modifier (function of part-load ratio) output is less than or equal to zero warning' &
             //' continues...', MTGenerator(GeneratorNum)%ExhTempFPLRErrorIndex, ExhAirTempFPLR, ExhAirTempFPLR)
          ExhAirTempFPLR = 0.0d0
        END IF
      ELSE
        ExhAirTempFPLR = 1.0d0 ! No curve input means modifier = 1.0 always
      END IF

      IF (ExhAirMassFlowRate .LE. 0.0d0) THEN
        MTGenerator(GeneratorNum)%ExhaustAirTemperature = CombustionAirInletTemp
        MTGenerator(GeneratorNum)%ExhaustAirHumRat = CombustionAirInletW
      ELSE
!       Calculate exhaust air temperature, accounting for inlet air temperature and PLR modifier factors
        ExhaustAirTemp = MTGenerator(GeneratorNum)%NomExhAirOutletTemp * ExhAirTempFTemp * ExhAirTempFPLR
        MTGenerator(GeneratorNum)%ExhaustAirTemperature = ExhaustAirTemp
!       Adjust exhaust air temperature if heat recovery to water is being done
        IF (QHeatRecToWater .GT. 0.0d0) THEN
          CpAir = PsyCpAirFnWTdb(CombustionAirInletW,CombustionAirInletTemp)
          IF (CpAir .GT. 0.0d0) THEN
            MTGenerator(GeneratorNum)%ExhaustAirTemperature = ExhaustAirTemp - QHeatRecToWater / (CpAir * ExhAirMassFlowRate)
          END IF
        END IF
!       Calculate exhaust air humidity ratio
        H2OHtOfVap = PsyHfgAirFnWTdb(1.0d0,16.0d0,'CalcMTGeneratorModel') ! W not used, passing 1.0 as dummy.
                                                                          ! Assume fuel is at 16C (ASHRAE HOF)
        IF (H2OHtOfVap .GT. 0.0d0) THEN
          MTGenerator(GeneratorNum)%ExhaustAirHumRat = CombustionAirInletW + MTGenerator(GeneratorNum)%FuelMdot * &
                                                       ((FuelHigherHeatingValue-FuelLowerHeatingValue)*KJtoJ/H2OHtOfVap) / &
                                                       ExhAirMassFlowRate
        ELSE
          MTGenerator(GeneratorNum)%ExhaustAirHumRat = CombustionAirInletW
        END IF
      END IF

      IF (MTGenerator(GeneratorNum)%ExhaustAirTemperature .LT. CombustionAirInletTemp) THEN
        IF (MTGenerator(GeneratorNum)%ExhTempLTInletTempIndex == 0) THEN
          CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('...The model has calculated the exhaust air temperature to be less than '&
                            //'the combustion air inlet temperature.')
          CALL ShowContinueError('...Value of exhaust air temperature   =' &
                                 //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%ExhaustAirTemperature,4))//' C.')
          CALL ShowContinueError('...Value of combustion air inlet temp ='//TRIM(TrimSigDigits(CombustionAirInletTemp,4))//' C.')
          CALL ShowContinueErrorTimeStamp('... Simulation will continue.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'// &
          ' Exhaust air temperature less than combustion air inlet temperature warning continues...', &
            MTGenerator(GeneratorNum)%ExhTempLTInletTempIndex, MTGenerator(GeneratorNum)%ExhaustAirTemperature, &
            MTGenerator(GeneratorNum)%ExhaustAirTemperature)
      END IF

      IF (MTGenerator(GeneratorNum)%ExhaustAirHumRat .LT. CombustionAirInletW) THEN
        IF (MTGenerator(GeneratorNum)%ExhHRLTInletHRIndex == 0) THEN
          CALL ShowWarningMessage('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
          CALL ShowContinueError('...The model has calculated the exhaust air humidity ratio to be less than '&
                            //'the combustion air inlet humidity ratio.')
          CALL ShowContinueError('...Value of exhaust air humidity ratio          =' &
                                 //TRIM(TrimSigDigits(MTGenerator(GeneratorNum)%ExhaustAirHumRat,6))//' kgWater/kgDryAir.')
          CALL ShowContinueError('...Value of combustion air inlet humidity ratio ='//TRIM(TrimSigDigits(CombustionAirInletW,6))&
                                 //' kgWater/kgDryAir.')
          CALL ShowContinueErrorTimeStamp('... Simulation will continue.')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'":'// &
          ' Exhaust air humidity ratio less than combustion air inlet humidity ratio warning continues...', &
            MTGenerator(GeneratorNum)%ExhHRLTInletHRIndex, MTGenerator(GeneratorNum)%ExhaustAirHumRat, &
            MTGenerator(GeneratorNum)%ExhaustAirHumRat)
      END IF

    END IF ! End of IF (MTGenerator(GeneratorNum)%ExhAirCalcsActive) THEN

  RETURN

END SUBROUTINE CalcMTGeneratorModel

!  End of MT Generator Model Calculation Subroutine
! *****************************************************************************


!  Beginning of record keeping subroutine for the MT Generator Module
! *****************************************************************************

SUBROUTINE UpdateMTGeneratorRecords(Num)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad/D. Shirey
          !       DATE WRITTEN   Mar 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Reporting and updating nodes if necessary.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys


  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: Num                ! Generator number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER              :: HeatRecInletNode       ! Node number for heat recovery (water) inlet node
  INTEGER              :: HeatRecOutletNode      ! Node number for heat recovery (water) outlet node
  INTEGER              :: ExhaustAirNodeNum      ! Node number for exhaust air node
  INTEGER              :: CombustAirInletNodeNum ! Node number for combustion inlet air node


  IF (MTGenerator(Num)%HeatRecActive) THEN

    HeatRecInletNode  = MTGenerator(Num)%HeatRecInletNodeNum
    HeatRecOutletNode = MTGenerator(Num)%HeatRecOutletNodeNum

!   Node(HeatRecOutletNode)%MassFlowRate         = MTGenerator(Num)%HeatRecMdot
    Node(HeatRecOutletNode)%Temp                 = MTGenerator(Num)%HeatRecOutletTemp
!    Node(HeatRecOutletNode)%MassFlowRateMaxAvail = Node(HeatRecInletNode)%MassFlowRateMaxAvail
!    Node(HeatRecOutletNode)%MassFlowRateMinAvail = Node(HeatRecInletNode)%MassFlowRateMinAvail

  END IF

  IF (MTGenerator(Num)%ExhAirCalcsActive) THEN
    ExhaustAirNodeNum = MTGenerator(Num)%CombustionAirOutletNodeNum
    CombustAirInletNodeNum = MTGenerator(Num)%CombustionAirInletNodeNum

    Node(ExhaustAirNodeNum)%MassFlowRate         = MTGenerator(Num)%ExhaustAirMassFlowRate
    Node(CombustAirInletNodeNum)%MassFlowRate    = MTGenerator(Num)%ExhaustAirMassFlowRate

    Node(ExhaustAirNodeNum)%Temp                 = MTGenerator(Num)%ExhaustAirTemperature
    Node(ExhaustAirNodeNum)%HumRat               = MTGenerator(Num)%ExhaustAirHumRat
    Node(ExhaustAirNodeNum)%MassFlowRateMaxAvail = Node(CombustAirInletNodeNum)%MassFlowRateMaxAvail
    Node(ExhaustAirNodeNum)%MassFlowRateMinAvail = Node(CombustAirInletNodeNum)%MassFlowRateMinAvail

! also update the report variables
   MTGeneratorReport(Num)%ExhAirMassFlowRate     = MTGenerator(Num)%ExhaustAirMassFlowRate
   MTGeneratorReport(Num)%ExhAirTemperature      = MTGenerator(Num)%ExhaustAirTemperature
   ! for exhaust only report

  END IF

  MTGeneratorReport(Num)%PowerGen                = MTGenerator(Num)%ElecPowerGenerated
  MTGeneratorReport(Num)%EnergyGen               = MTGenerator(Num)%ElecPowerGenerated*TimeStepSys*SecInHour
  MTGeneratorReport(Num)%QHeatRecovered          = MTGenerator(Num)%QHeatRecovered
  MTGeneratorReport(Num)%ExhaustEnergyRec        = MTGenerator(Num)%QHeatRecovered*TimeStepSys*SecInHour
  MTGeneratorReport(Num)%FuelEnergyUseRateHHV    = MTGenerator(Num)%FuelEnergyUseRateHHV
  MTGeneratorReport(Num)%FuelEnergyHHV           = MTGenerator(Num)%FuelEnergyUseRateHHV*TimeStepSys*SecInHour
  MTGeneratorReport(Num)%FuelMdot                = MTGenerator(Num)%FuelMdot
  IF (MTGenerator(Num)%FuelEnergyUseRateLHV .GT. 0.0d0) THEN
    MTGeneratorReport(Num)%ElectricEfficiencyLHV   = MTGenerator(Num)%ElecPowerGenerated / MTGenerator(Num)%FuelEnergyUseRateLHV
    MTGeneratorReport(Num)%ThermalEfficiencyLHV    = MTGenerator(Num)%QHeatRecovered / MTGenerator(Num)%FuelEnergyUseRateLHV
  ELSE
    MTGeneratorReport(Num)%ElectricEfficiencyLHV   = 0.0d0
    MTGeneratorReport(Num)%ThermalEfficiencyLHV    = 0.0d0
  END IF
  MTGeneratorReport(Num)%HeatRecInletTemp        = MTGenerator(Num)%HeatRecInletTemp
  MTGeneratorReport(Num)%HeatRecOutletTemp       = MTGenerator(Num)%HeatRecOutletTemp
  MTGeneratorReport(Num)%HeatRecMdot             = MTGenerator(Num)%HeatRecMdot
  MTGeneratorReport(Num)%AncillaryPowerRate      = MTGenerator(Num)%AncillaryPowerRate
  MTGeneratorReport(Num)%AncillaryEnergy         = MTGenerator(Num)%AncillaryPowerRate*TimeStepSys*SecInHour
  MTGeneratorReport(Num)%StandbyPowerRate        = MTGenerator(Num)%StandbyPowerRate
  MTGeneratorReport(Num)%StandbyEnergy           = MTGenerator(Num)%StandbyPowerRate*TimeStepSys*SecInHour

  RETURN

END SUBROUTINE UpdateMTGeneratorRecords



SUBROUTINE GetMTGeneratorResults(GeneratorType, GeneratorIndex, &
                                 GeneratorPower,  GeneratorEnergy, ThermalPower, ThermalEnergy)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get some results for load center's aggregation

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator !unused1208
  INTEGER, INTENT(IN)           :: GeneratorIndex
  REAL(r64), INTENT(OUT)        :: GeneratorPower  ! electrical power
  REAL(r64), INTENT(OUT)        :: GeneratorEnergy ! electrical energy
  REAL(r64), INTENT(OUT)        :: ThermalPower  ! heat power
  REAL(r64), INTENT(OUT)        :: ThermalEnergy ! heat energy

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  GeneratorPower  =  MTGeneratorReport(GeneratorIndex)%PowerGen
  GeneratorEnergy =  MTGeneratorReport(GeneratorIndex)%EnergyGen
  ThermalPower    =  MTGeneratorReport(GeneratorIndex)%QHeatRecovered
  ThermalEnergy   =  MTGeneratorReport(GeneratorIndex)%ExhaustEnergyRec


  RETURN

END SUBROUTINE GetMTGeneratorResults

SUBROUTINE GetMTGeneratorExhaustNode(CompType,CompName,ExhaustOutletNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mahabir Bhandari
          !       DATE WRITTEN   Jul 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To pass exhaust outlet number from Micro Turbine to Exhaust fired absorption chiller.
          !
          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName

  INTEGER, INTENT(OUT)         :: ExhaustOutletNodeNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: CompNum


  IF (GetMTInput) THEN
     ! Read input data.
    CALL GetMTGeneratorInput
    GetMTInput=.false.
  ENDIF

  ExhaustOutletNodeNum = 0

  CompNum = FindItemInList(CompName,MTGenerator%Name,NumMTGenerators)

  IF (CompNum == 0) THEN
    CALL ShowFatalError('GetMTGeneratorExhaustNode: Unit not found='//TRIM(CompName))
  ELSE
    ExhaustOutletNodeNum = MTGenerator(CompNum)%CombustionAirOutletNodeNum
  ENDIF
  RETURN
END SUBROUTINE GetMTGeneratorExhaustNode

! End of Record Keeping subroutine for the MT Generator Module
! *****************************************************************************

END MODULE MicroturbineElectricGenerator

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
