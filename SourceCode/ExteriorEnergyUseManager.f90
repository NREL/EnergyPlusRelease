MODULE ExteriorEnergyUse

  ! Module containing the routines dealing with the reporting of Exterior Energy Usage Elements

  ! MODULE INFORMATION:
  !       AUTHOR         Linda Lawrie
  !       DATE WRITTEN   January 2001
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module provides the reporting for exterior energy usage.  This usage does not directly
  ! affect simulation results for the energy usage in a building but may affect the "metered"
  ! usage of a facility.

  ! METHODOLOGY EMPLOYED:
  ! No simulation, this is just reporting consumption.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, TimeStepZone
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError, &
                       SetupOutputVariable

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: ElecUse=1     ! Electricity
INTEGER, PARAMETER :: GasUse=2      ! Gas (Natural)
INTEGER, PARAMETER :: WaterUse=3    ! Water
INTEGER, PARAMETER :: CoalUse=4     ! Coal
INTEGER, PARAMETER :: FuelOil1Use=5 ! FuelOil#1
INTEGER, PARAMETER :: FuelOil2Use=6 ! FuelOil#2
INTEGER, PARAMETER :: LPGUse=7      ! PropaneGas
INTEGER, PARAMETER :: GasolineUse=8 ! Gasoline
INTEGER, PARAMETER :: DieselUse=9   ! Diesel
INTEGER, PARAMETER :: SteamUse=10   ! Steam
INTEGER, PARAMETER :: DistrictCoolUse=11  ! Purchased Cooling
INTEGER, PARAMETER :: DistrictHeatUse=12  ! Purchased Heating
INTEGER, PARAMETER :: OtherFuel1Use=13 ! OtherFuel1
INTEGER, PARAMETER :: OtherFuel2Use=14 ! OtherFuel2

INTEGER, PARAMETER, PUBLIC :: ScheduleOnly = 1  ! exterior lights only on schedule
INTEGER, PARAMETER, PUBLIC :: AstroClockOverride = 2 !exterior lights controlled to turn off during day.


  ! DERIVED TYPE DEFINITIONS:
TYPE, PUBLIC :: ExteriorLightUsage
  CHARACTER(len=MaxNameLength) :: Name = ' '             ! Descriptive name -- will show on reporting
  INTEGER                      :: SchedPtr = 0           ! Can be scheduled
  REAL(r64)                    :: DesignLevel = 0.0d0      ! Consumption in Watts
  REAL(r64)                    :: Power = 0.0d0            ! Power = DesignLevel * ScheduleValue
  REAL(r64)                    :: CurrentUse = 0.0d0       ! Use for this time step
  INTEGER                      :: ControlMode = 1        ! Control mode Schedule Only or Astronomical Clock plus schedule

  LOGICAL                      :: ManageDemand = .FALSE. ! Flag to indicate whether to use demand limiting
  REAL(r64)                    :: DemandLimit = 0.0d0      ! Demand limit set by demand manager [W]
  LOGICAL                      :: PowerActuatorOn  = .FALSE.       ! EMS flag
  REAL(r64)                    :: PowerActuatorValue     ! EMS value
  REAL(r64)                    :: SumConsumption = 0.0d0   ! sum of electric consumption [J] for reporting
  REAL(r64)                    :: SumTimeNotZeroCons = 0.0d0 ! sum of time of positive electric consumption [hr]
END TYPE

TYPE, PUBLIC :: ExteriorEquipmentUsage
  CHARACTER(len=MaxNameLength) :: Name = ' '             ! Descriptive name -- will show on reporting
  INTEGER                      :: FuelType = 0
  INTEGER                      :: SchedPtr = 0           ! Can be scheduled
  REAL(r64)                    :: DesignLevel = 0.0d0      ! Design Consumption (Watts, except for Water Equipment)
  REAL(r64)                    :: Power = 0.0d0            ! Power = DesignLevel * ScheduleValue
  REAL(r64)                    :: CurrentUse = 0.0d0       ! Use for this time step

  LOGICAL                      :: ManageDemand = .FALSE. ! Flag to indicate whether to use demand limiting
  REAL(r64)                    :: DemandLimit = 0.0d0      ! Demand limit set by demand manager [W]
END TYPE

  ! MODULE VARIABLE DECLARATIONS:
TYPE (ExteriorLightUsage), PUBLIC, ALLOCATABLE, &
                           DIMENSION(:)    :: ExteriorLights     ! Structure for Exterior Light reporting
INTEGER, PUBLIC                            :: NumExteriorLights  ! Number of Exterior Light Inputs
TYPE (ExteriorEquipmentUsage), PUBLIC, ALLOCATABLE, &
                               DIMENSION(:):: ExteriorEquipment  ! Structure for Exterior Equipment Reporting
INTEGER, PUBLIC                            :: NumExteriorEqs     ! Number of Exterior Equipment Inputs

  ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

  ! Name Public routines, optionally name Private routines within this module

PUBLIC  ManageExteriorEnergyUse
PRIVATE GetExteriorEnergyUseInput
PRIVATE ReportExteriorEnergyUse

CONTAINS

SUBROUTINE ManageExteriorEnergyUse

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine provides the usual call for the Simulation Manager.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE      :: GetInputFlag = .true.  ! First time, input is "gotten"

  IF (GetInputFlag) THEN
    CALL GetExteriorEnergyUseInput
    GetInputFlag=.false.
  ENDIF

  CALL ReportExteriorEnergyUse

  RETURN

END SUBROUTINE ManageExteriorEnergyUse

SUBROUTINE GetExteriorEnergyUseInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the Exterior Lights and Equipment.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE ScheduleManager, ONLY: GetScheduleIndex, GetScheduleMinValue, GetScheduleMaxValue, GetScheduleName
  USE General, ONLY: RoundSigDigits
  USE OutputReportPredefined
  USE DataGlobals, ONLY: AnyEnergyManagementSystemInModel
  USE DataInterfaces, ONLY: SetupEMSActuator

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetExteriorEnergyUseInput: '
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: Item    ! Item to be "gotten"
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: NumFuelEq  ! Temporary -- number of ExteriorFuelEquipment statements
  INTEGER                        :: NumWtrEq   ! Temporary -- number of ExteriorWaterEquipment statements
  CHARACTER(len=20)              :: TypeString ! Fuel Type string (returned from Validation)
  CHARACTER(len=4)               :: ConUnits   ! String for Fuel Consumption units (allow Water)
  CHARACTER(len=MaxNameLength)   :: EndUseSubcategoryName
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  REAL(r64)    :: SchMax           ! Max value of schedule for item
  REAL(r64)    :: SchMin           ! Min value of schedule for item
  REAL(r64)    :: sumDesignLevel = 0.0d0 !for predefined report of design level total


  NumExteriorLights=GetNumObjectsFound('Exterior:Lights')
  ALLOCATE(ExteriorLights(NumExteriorLights))

  NumFuelEq=GetNumObjectsFound('Exterior:FuelEquipment')
  NumWtrEq=GetNumObjectsFound('Exterior:WaterEquipment')
  ALLOCATE(ExteriorEquipment(NumFuelEq+NumWtrEq))

  NumExteriorEqs=0

! =================================  Get Exterior Lights

  cCurrentModuleObject='Exterior:Lights'
  DO Item=1,NumExteriorLights
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ExteriorLights%Name,Item,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF
    ExteriorLights(Item)%Name=cAlphaArgs(1)
    ExteriorLights(Item)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
    IF (ExteriorLights(Item)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(2)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(2))//  &
           ' is required, missing for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           ' entered='//TRIM(cAlphaArgs(2))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ExteriorLights(Item)%SchedPtr)
      SchMax=GetScheduleMaxValue(ExteriorLights(Item)%SchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
             ' minimum, is < 0.0 for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError(TRIM(cAlphaArgs(2))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
             ' maximum, is < 0.0 for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError(TRIM(cAlphaArgs(2))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF
    IF (lAlphaFieldBlanks(3)) THEN
      ExteriorLights(Item)%ControlMode = ScheduleOnly
    ELSEIF (SameString(cAlphaArgs(3), 'ScheduleNameOnly')) THEN
      ExteriorLights(Item)%ControlMode = ScheduleOnly
    ELSEIF (SameString(cAlphaArgs(3), 'AstronomicalClock')) then
      ExteriorLights(Item)%ControlMode = AstroClockOverride
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
         '='//TRIM(cAlphaArgs(3))//  &
         ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
    ENDIF

    IF (NumAlphas > 3) THEN
      EndUseSubcategoryName = cAlphaArgs(4)
    ELSE
      EndUseSubcategoryName = 'General'
    END IF

    ExteriorLights(Item)%DesignLevel=rNumericArgs(1)
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('ExteriorLights', ExteriorLights(Item)%Name, 'Electric Power', 'W',  &
        ExteriorLights(Item)%PowerActuatorOn, ExteriorLights(Item)%PowerActuatorValue)
    ENDIF

    CALL SetupOutputVariable('Exterior Lights Electric Power [W]',ExteriorLights(Item)%Power, &
                             'Zone','Average',ExteriorLights(Item)%Name)

    CALL SetupOutputVariable('Exterior Lights Electric Energy [J]',ExteriorLights(Item)%CurrentUse, &
                             'Zone','Sum',ExteriorLights(Item)%Name, &
                             ResourceTypeKey='Electricity',EndUseKey='Exterior Lights',EndUseSubKey=EndUseSubcategoryName)

    ! entries for predefined tables
    CALL PreDefTableEntry(pdchExLtPower,ExteriorLights(Item)%Name,ExteriorLights(Item)%DesignLevel)
    sumDesignLevel = sumDesignLevel + ExteriorLights(Item)%DesignLevel
    IF (ExteriorLights(Item)%ControlMode .EQ. AstroClockOverride) THEN                          !photocell/schedule
      CALL PreDefTableEntry(pdchExLtClock,ExteriorLights(Item)%Name,'AstronomicalClock')
      CALL PreDefTableEntry(pdchExLtSchd,ExteriorLights(Item)%Name,'-')
    ELSE
      CALL PreDefTableEntry(pdchExLtClock,ExteriorLights(Item)%Name,'Schedule')
      CALL PreDefTableEntry(pdchExLtSchd,ExteriorLights(Item)%Name,GetScheduleName(ExteriorLights(Item)%SchedPtr))
    END IF

  ENDDO
  CALL PreDefTableEntry(pdchExLtPower,'Exterior Lighting Total',sumDesignLevel)

! =================================  Get Exterior Fuel Equipment

  cCurrentModuleObject='Exterior:FuelEquipment'
  DO Item=1,NumFuelEq
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ExteriorEquipment%Name,Item,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF
    NumExteriorEqs=NumExteriorEqs+1
    ExteriorEquipment(NumExteriorEqs)%Name=cAlphaArgs(1)

    IF (NumAlphas > 3) THEN
      EndUseSubcategoryName = cAlphaArgs(4)
    ELSE
      EndUseSubcategoryName = 'General'
    END IF

    CALL ValidateFuelType(ExteriorEquipment(NumExteriorEqs)%FuelType,cAlphaArgs(2),TypeString,cCurrentModuleObject,  &
       cAlphaFieldNames(2),cAlphaArgs(2))
    IF (ExteriorEquipment(NumExteriorEqs)%FuelType == 0) THEN
      IF (lAlphaFieldBlanks(2)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(2))//  &
           ' is required, missing for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           ' entered='//TRIM(cAlphaArgs(2))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ENDIF
      ErrorsFound=.true.
    ELSE
      IF (ExteriorEquipment(NumExteriorEqs)%FuelType /= WaterUse) THEN
        CALL SetupOutputVariable('Exterior Equipment Fuel Rate [W]',ExteriorEquipment(NumExteriorEqs)%Power, &
                                 'Zone','Average',ExteriorEquipment(NumExteriorEqs)%Name)

        ConUnits='[J]'
        CALL SetupOutputVariable('Exterior Equipment '//TRIM(TypeString)//' Energy '//TRIM(ConUnits), &
                             ExteriorEquipment(NumExteriorEqs)%CurrentUse, &
                             'Zone','Sum',ExteriorEquipment(NumExteriorEqs)%Name, &
                             ResourceTypeKey=TypeString,EndUseKey='ExteriorEquipment',EndUseSubKey=EndUseSubcategoryName)
      ELSE
        CALL SetupOutputVariable('Exterior Equipment Water Volume Flow Rate [m3/s]',ExteriorEquipment(NumExteriorEqs)%Power, &
                                 'Zone','Average',TRIM(ExteriorEquipment(NumExteriorEqs)%Name))

        ConUnits='[m3]'
        CALL SetupOutputVariable('Exterior Equipment '//TRIM(TypeString)//' Volume '//TRIM(ConUnits), &
                             ExteriorEquipment(NumExteriorEqs)%CurrentUse, &
                             'Zone','Sum',ExteriorEquipment(NumExteriorEqs)%Name, &
                             ResourceTypeKey=TypeString,EndUseKey='ExteriorEquipment',EndUseSubKey=EndUseSubcategoryName)
      ENDIF


    ENDIF
    ExteriorEquipment(NumExteriorEqs)%SchedPtr  = GetScheduleIndex(cAlphaArgs(3))
    IF (ExteriorEquipment(NumExteriorEqs)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(3))//  &
           ' is required, missing for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
           ' entered='//TRIM(cAlphaArgs(3))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ExteriorEquipment(NumExteriorEqs)%SchedPtr)
      SchMax=GetScheduleMaxValue(ExteriorEquipment(NumExteriorEqs)%SchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
             ' minimum, is < 0.0 for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError(TRIM(cAlphaArgs(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
             ' maximum, is < 0.0 for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError(TRIM(cAlphaArgs(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF
    ExteriorEquipment(NumExteriorEqs)%DesignLevel=rNumericArgs(1)
  ENDDO

! =================================  Get Exterior Water Equipment

  cCurrentModuleObject='Exterior:WaterEquipment'
  DO Item=1,NumWtrEq
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ExteriorEquipment%Name,Item,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF
    NumExteriorEqs=NumExteriorEqs+1
    ExteriorEquipment(NumExteriorEqs)%Name=cAlphaArgs(1)
    ExteriorEquipment(NumExteriorEqs)%FuelType=WaterUse
    ExteriorEquipment(NumExteriorEqs)%SchedPtr  = GetScheduleIndex(cAlphaArgs(3))
    IF (ExteriorEquipment(NumExteriorEqs)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(3))//  &
           ' is required, missing for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
           ' entered='//TRIM(cAlphaArgs(3))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ExteriorEquipment(NumExteriorEqs)%SchedPtr)
      SchMax=GetScheduleMaxValue(ExteriorEquipment(NumExteriorEqs)%SchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
             ' minimum, is < 0.0 for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError(TRIM(cAlphaArgs(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(3))//  &
             ' maximum, is < 0.0 for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError(TRIM(cAlphaArgs(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    IF (NumAlphas > 3) THEN
      EndUseSubcategoryName = cAlphaArgs(4)
    ELSE
      EndUseSubcategoryName = 'General'
    END IF

    ExteriorEquipment(NumExteriorEqs)%DesignLevel=rNumericArgs(1)

    CALL SetupOutputVariable('Exterior Equipment Water Volume Flow Rate [m3/s]',ExteriorEquipment(NumExteriorEqs)%Power, &
                             'Zone','Average',ExteriorEquipment(NumExteriorEqs)%Name)

    CALL SetupOutputVariable('Exterior Equipment Water Volume [m3]', &
                             ExteriorEquipment(NumExteriorEqs)%CurrentUse, &
                             'Zone','Sum',ExteriorEquipment(NumExteriorEqs)%Name, &
                             ResourceTypeKey='Water',EndUseKey='ExteriorEquipment',EndUseSubKey=EndUseSubcategoryName)
    CALL SetupOutputVariable('Exterior Equipment Mains Water Volume [m3]', &
                             ExteriorEquipment(NumExteriorEqs)%CurrentUse, &
                             'Zone','Sum',ExteriorEquipment(NumExteriorEqs)%Name, &
                             ResourceTypeKey='MainsWater',EndUseKey='ExteriorEquipment',EndUseSubKey=EndUseSubcategoryName)
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
  ENDIF

  RETURN

END SUBROUTINE GetExteriorEnergyUseInput

SUBROUTINE ValidateFuelType(FuelTypeNumber,FuelTypeAlpha,FuelTypeString,CurrentModuleObject,CurrentField,CurrentName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine compares the input Fuel Type value against the
          ! valid values and sets the correct in the returned FuelTypeNumber.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(OUT)          :: FuelTypeNumber  ! Fuel Type to be set in structure.
  CHARACTER(len=*), INTENT(IN)  :: FuelTypeAlpha   ! Fuel Type String
  CHARACTER(len=*), INTENT(OUT) :: FuelTypeString  ! Standardized Fuel Type String (for variable naming)
  CHARACTER(len=*), INTENT(IN)  :: CurrentModuleObject ! object being parsed
  CHARACTER(len=*), INTENT(IN)  :: CurrentField    ! current field being parsed
  CHARACTER(len=*), INTENT(IN)  :: CurrentName     ! current object name being parsed

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='ValidateFuelType: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


    FuelTypeNumber=0
    FuelTypeString=' '

    !Select the correct Number for the associated ascii name for the fuel type
    IF (SameString(FuelTypeAlpha,'Electricity') .or. SameString(FuelTypeAlpha,'Electric')) THEN
       FuelTypeNumber=ElecUse
       FuelTypeString='Electric'
    ENDIF
    IF (SameString(FuelTypeAlpha,'Gas').or. SameString(FuelTypeAlpha,'NaturalGas')) THEN
      IF (SameString(FuelTypeAlpha,'Gas')) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(CurrentName)//'".')
        CALL ShowContinueError('Deprecated value in '//TRIM(CurrentField)//'="'//    &
              TRIM(FuelTypeAlpha)//'", using "NaturalGas".')
      ENDIF
      FuelTypeNumber=GasUse
      FuelTypeString='Gas'
    ENDIF
    IF (SameString(FuelTypeAlpha,'Coal')) THEN
      FuelTypeNumber=CoalUse
      FuelTypeString='Coal'
    ENDIF
    IF (SameString(FuelTypeAlpha,'FuelOil#1')) THEN
      FuelTypeNumber=FuelOil1Use
      FuelTypeString='FuelOil#1'
    ENDIF
    IF (SameString(FuelTypeAlpha,'PropaneGas') .or. SameString(FuelTypeAlpha,'LPG')) THEN
      IF (SameString(FuelTypeAlpha,'LPG')) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(CurrentName)//'".')
        CALL ShowContinueError('Deprecated value in '//TRIM(CurrentField)//'="'//    &
              TRIM(FuelTypeAlpha)//'", using "PropaneGas".')
      ENDIF
      FuelTypeNumber=LPGUse
      FuelTypeString='Propane'
    ENDIF
    IF (SameString(FuelTypeAlpha,'Gasoline')) THEN
      FuelTypeNumber=GasolineUse
      FuelTypeString='Gasoline'
    ENDIF
    IF (SameString(FuelTypeAlpha,'Diesel')) THEN
      FuelTypeNumber=DieselUse
      FuelTypeString='Diesel'
    ENDIF
    IF (SameString(FuelTypeAlpha,'FuelOil#2')) THEN
      FuelTypeNumber=FuelOil2Use
      FuelTypeString='FuelOil#2'
    ENDIF
    IF (SameString(FuelTypeAlpha,'OtherFuel1')) THEN
      FuelTypeNumber=OtherFuel1Use
      FuelTypeString='OtherFuel1'
    ENDIF
    IF (SameString(FuelTypeAlpha,'OtherFuel2')) THEN
      FuelTypeNumber=OtherFuel1Use
      FuelTypeString='OtherFuel2'
    ENDIF
    IF (SameString(FuelTypeAlpha,'Water')) THEN
      FuelTypeNumber=WaterUse
      FuelTypeString='Water'
    ENDIF
    IF (SameString(FuelTypeAlpha,'Steam')) THEN
      FuelTypeNumber=SteamUse
      FuelTypeString='Steam'
    ENDIF
    IF (SameString(FuelTypeAlpha,'DistrictCooling')) THEN
      FuelTypeNumber=DistrictCoolUse
      FuelTypeString='DistrictCooling'
    ENDIF
    IF (SameString(FuelTypeAlpha,'DistrictHeating')) THEN
      FuelTypeNumber=DistrictHeatUse
      FuelTypeString='DistrictHeating'
    ENDIF

  RETURN

END SUBROUTINE ValidateFuelType

SUBROUTINE ReportExteriorEnergyUse

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs the calculations necessary to report
          ! the exterior energy use types.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour,WarmUpFlag, DoOutputReporting, KindOfSim, ksRunPeriodWeather
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataEnvironment,  ONLY: SunIsUP

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Item   ! Loop Control

  DO Item=1,NumExteriorLights
    SELECT CASE (ExteriorLights(Item)%ControlMode)

    CASE (ScheduleOnly)
        ExteriorLights(Item)%Power = ExteriorLights(Item)%DesignLevel * GetCurrentScheduleValue(ExteriorLights(Item)%SchedPtr)
        ExteriorLights(Item)%CurrentUse = ExteriorLights(Item)%Power * TimeStepZone * SecInHour

    CASE (AstroClockOverride)

      IF (SunIsUP) THEN
        ExteriorLights(Item)%Power = 0.0d0
        ExteriorLights(Item)%CurrentUse = 0.0d0
      ELSE
        ExteriorLights(Item)%Power = ExteriorLights(Item)%DesignLevel * GetCurrentScheduleValue(ExteriorLights(Item)%SchedPtr)
        ExteriorLights(Item)%CurrentUse = ExteriorLights(Item)%Power * TimeStepZone * SecInHour
      END IF

    CASE DEFAULT
       !should not occur

    END SELECT

    ! Reduce lighting power due to demand limiting
    IF (ExteriorLights(Item)%ManageDemand .AND. (ExteriorLights(Item)%Power > ExteriorLights(Item)%DemandLimit)) THEN
      ExteriorLights(Item)%Power = ExteriorLights(Item)%DemandLimit
      ExteriorLights(Item)%CurrentUse = ExteriorLights(Item)%Power * TimeStepZone * SecInHour
    END IF
    ! EMS controls
    IF (ExteriorLights(Item)%PowerActuatorOn) ExteriorLights(Item)%Power = ExteriorLights(Item)%PowerActuatorValue

    ExteriorLights(Item)%CurrentUse = ExteriorLights(Item)%Power * TimeStepZone * SecInHour

    !gather for tabular reports
    IF (.NOT. WarmUpFlag) THEN
!      IF (DoOutputReporting .AND.  WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
      IF (DoOutputReporting  .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
        !for tabular report, accumlate the total electricity used for each ExteriorLights object
        ExteriorLights(Item)%SumConsumption = ExteriorLights(Item)%SumConsumption + ExteriorLights(Item)%CurrentUse
        !for tabular report, accumulate the time when each ExteriorLights has consumption
        !(using a very small threshold instead of zero)
        IF (ExteriorLights(Item)%CurrentUse > 0.01d0) THEN
          ExteriorLights(Item)%SumTimeNotZeroCons = ExteriorLights(Item)%SumTimeNotZeroCons + TimeStepZone
        END IF
      ENDIF
    END IF
  END DO

  DO Item=1,NumExteriorEqs
    ExteriorEquipment(Item)%CurrentUse = ExteriorEquipment(Item)%DesignLevel* &
                         GetCurrentScheduleValue(ExteriorEquipment(Item)%SchedPtr)*TimeStepZone*SecInHour
  END DO

  RETURN

END SUBROUTINE ReportExteriorEnergyUse

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

END MODULE ExteriorEnergyUse

