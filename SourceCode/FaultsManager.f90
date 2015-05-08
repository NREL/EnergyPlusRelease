MODULE FaultsManager

        ! MODULE INFORMATION:
        !       AUTHOR         Tianzhen Hong, LBNL
        !       DATE WRITTEN   August 2013
        !       MODIFIED       
        !       RE-ENGINEERED  

        ! PURPOSE OF THIS MODULE:
        ! This module manages operational faults of buildings and systems.

        ! METHODOLOGY EMPLOYED:
        !  Various methods are employed depending types of faults

        ! USE STATEMENTS:

USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, ScheduleAlwaysOn
USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowContinueError, ShowSevereError
USE InputProcessor

IMPLICIT NONE ! Enforce explicit typing of all variables

          ! MODULE PARAMETER DEFINITIONS

PRIVATE ! Everything private unless explicitly made public

          ! DERIVED TYPE DEFINITIONS:

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE FaultProperties                                     ! Derived type for operational faults
  CHARACTER(len=MaxNameLength) :: Name = ' '  
  CHARACTER(len=MaxNameLength) :: FaultType = ' '        ! Fault type
  CHARACTER(len=MaxNameLength) :: AvaiSchedule = ' '     ! Availability schedule
  CHARACTER(len=MaxNameLength) :: SeveritySchedule = ' ' ! Severity schedule, multipliers to the Offset
  CHARACTER(len=MaxNameLength) :: ControllerType = ' '   ! Controller type
  INTEGER                      :: ControllerTypeEnum = 0
  CHARACTER(len=MaxNameLength) :: ControllerName = ' '   ! Controller name
  INTEGER                      :: ControllerID = 0       ! Point to a controller associated with the fault
  REAL(r64)    :: Offset             = 0.0d0             ! offset, + means sensor reading is higher than actual value
  Logical      :: Status             = .FALSE.           ! for future use
  INTEGER      :: AvaiSchedPtr       = 0
  INTEGER      :: SeveritySchedPtr   = 0
  INTEGER      :: FaultTypeEnum      = 0
END TYPE FaultProperties

! ControllerTypeEnum
INTEGER, PUBLIC, PARAMETER :: iController_AirEconomizer = 1001

          ! MODULE VARIABLE DECLARATIONS:
INTEGER, PARAMETER :: NumFaultTypes = 5

! FaultTypeEnum
INTEGER, PUBLIC, PARAMETER :: iFault_TemperatureSensorOffset_OutdoorAir = 101
INTEGER, PUBLIC, PARAMETER :: iFault_HumiditySensorOffset_OutdoorAir    = 102
INTEGER, PUBLIC, PARAMETER :: iFault_EnthalpySensorOffset_OutdoorAir    = 103
INTEGER, PUBLIC, PARAMETER :: iFault_TemperatureSensorOffset_ReturnAir  = 104
INTEGER, PUBLIC, PARAMETER :: iFault_EnthalpySensorOffset_ReturnAir     = 105

! Types of faults under Group Operational Faults in IDD
!  1. Temperature sensor offset
!  2. Humidity sensor offset
!  3. Enthalpy sensor offset
!
! coming ...
!  4. Pressure sensor offset
!  5. Fouling: coils, chillers, boilers, cooling towers
!  6. Damper leakage: return air, outdoor air
!  7. Blockage: pipe
!  8. Dirty: air filter
!  9. Meter: air flow, water flow
! 10. CO2 sensor
! 11. more
!
CHARACTER(len=*), PARAMETER, DIMENSION(NumFaultTypes) :: cFaults = &   
    (/'FaultModel:TemperatureSensorOffset:OutdoorAir', &
      'FaultModel:HumiditySensorOffset:OutdoorAir   ', &
      'FaultModel:EnthalpySensorOffset:OutdoorAir   ', &
      'FaultModel:TemperatureSensorOffset:ReturnAir ', &
      'FaultModel:EnthalpySensorOffset:ReturnAir    ' /)
      
!      'FaultModel:PressureSensorOffset:OutdoorAir', &
!      'FaultModel:TemperatureSensorOffset:SupplyAir', &
!      'FaultModel:TemperatureSensorOffset:ZoneAir', &
!      'FaultModel:Blockage:Branch', &
!      'FaultModel:Dirty:AirFilter', &
!      'FaultModel:Fouling:Coil', &
!      'FaultModel:Fouling:Chiller', &
!      'FaultModel:Fouling:Boiler', & 
!      'FaultModel:Fouling:CoolingTower', &
!      'FaultModel:DamperLeakage:ReturnAir', &
!      'FaultModel:DamperLeakage:OutdoorAir' /)
!

INTEGER, PARAMETER, DIMENSION(NumFaultTypes) :: iFaultTypeEnums = &   
    (/iFault_TemperatureSensorOffset_OutdoorAir, iFault_HumiditySensorOffset_OutdoorAir, &
      iFault_EnthalpySensorOffset_OutdoorAir, iFault_TemperatureSensorOffset_ReturnAir, &
      iFault_EnthalpySensorOffset_ReturnAir/)

TYPE(FaultProperties), PUBLIC, ALLOCATABLE, DIMENSION(:) :: Faults
LOGICAL, PUBLIC :: AnyFaultsInModel = .FALSE.     ! True if there are operationla faults in the model
INTEGER, PUBLIC :: NumFaults = 0                  ! Number of faults (include multiple faults of same type) in the model

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC CheckAndReadFaults

CONTAINS

SUBROUTINE CheckAndReadFaults

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Tianzhen Hong
          !       DATE WRITTEN   August 2013
          !       MODIFIED       
          !       RE-ENGINEERED  

          ! PURPOSE OF THIS SUBROUTINE:
          !  1. Determine if any operational faults are present in a model and set flags
          !  2. Read faults input
          
          ! METHODOLOGY EMPLOYED:
          ! Get number of faults-related input objects and assign faults input to data structure

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE ScheduleManager, ONLY: GetScheduleIndex
  
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
  LOGICAL, SAVE :: RunMeOnceFlag = .FALSE.
  
  LOGICAL :: ErrorsFound = .false.    ! If errors detected in input
  INTEGER :: NumAlphas                ! Number of Alphas for each GetobjectItem call
  INTEGER :: NumNumbers               ! Number of Numbers for each GetobjectItem call
  INTEGER :: IOStatus
  CHARACTER(len=MaxNameLength), DIMENSION(5) :: cAlphaArgs  ! Alpha input items for object
  LOGICAL, DIMENSION(5) :: lAlphaFieldBlanks = .FALSE.
  LOGICAL, DIMENSION(1) :: lNumericFieldBlanks = .FALSE.
  CHARACTER(len=MaxNameLength), DIMENSION(5) :: cAlphaFieldNames 
  CHARACTER(len=MaxNameLength), DIMENSION(1) :: cNumericFieldNames 
  REAL(r64), DIMENSION(1) :: rNumericArgs       ! Numeric input items for object

  INTEGER :: i
  INTEGER :: j
  INTEGER :: jj
  INTEGER :: iFaults
  INTEGER :: iTotalFaults
  CHARACTER(len=MaxNameLength) :: cFault1
  
  IF (RunMeOnceFlag) RETURN
  
  ! check number of faults
  NumFaults = 0
  Do i = 1, NumFaultTypes
    iFaults = 0
    iFaults = GetNumObjectsFound(cFaults(i))
    NumFaults = NumFaults + iFaults
  ENDDO
  
  IF (NumFaults > 0) THEN
    AnyFaultsInModel = .TRUE.
  ELSE
    AnyFaultsInModel = .FALSE.
  ENDIF

  IF (.NOT. AnyFaultsInModel) THEN
    RunMeOnceFlag = .TRUE.
    RETURN
  ENDIF
  
  ! read faults input
  ALLOCATE (Faults(NumFaults))
  j = 0
  Do i = 1, NumFaultTypes
    cFault1 = cFaults(i)
    iFaults = GetNumObjectsFound(cFault1)
    DO jj = 1, iFaults
      CALL GetObjectItem(cFault1,jj,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
        AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
        AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      
      j = j + 1
      Faults(j)%FaultType = cFault1
      Faults(j)%FaultTypeEnum = iFaultTypeEnums(i)
              
      Faults(j)%Name = cAlphaArgs(1)
      Faults(j)%AvaiSchedule = cAlphaArgs(2)
      ! check availability schedule
      IF (lAlphaFieldBlanks(2)) THEN
        Faults(j)%AvaiSchedPtr = -1  ! returns schedule value of 1
      ELSE
        Faults(j)%AvaiSchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (Faults(j)%AvaiSchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(cFault1)//' = "'//trim(cAlphaArgs(1))//'" invalid '//  &
            trim(cAlphaFieldNames(2))//' = "'//trim(cAlphaArgs(2))//'" not found.')
          ErrorsFound = .TRUE.
        ENDIF
      END IF
      
      Faults(j)%SeveritySchedule = cAlphaArgs(3)
      ! check severity schedule
      IF (lAlphaFieldBlanks(3)) THEN
        Faults(j)%SeveritySchedPtr = -1  ! returns schedule value of 1
      ELSE
        Faults(j)%SeveritySchedPtr = GetScheduleIndex(cAlphaArgs(3))
        IF (Faults(j)%SeveritySchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(cFault1)//' = "'//trim(cAlphaArgs(1))//'" invalid '//  &
            trim(cAlphaFieldNames(3))//' = "'//trim(cAlphaArgs(3))//'" not found.')
          ErrorsFound = .TRUE.
        ENDIF
      END IF
      
      Faults(j)%ControllerType = cAlphaArgs(4)
      ! check controller type
      IF (lAlphaFieldBlanks(4)) THEN
          CALL ShowSevereError(TRIM(cFault1)//' = "'//trim(cAlphaArgs(1))//'" invalid '//  &
            trim(cAlphaFieldNames(4))//' = "'//trim(cAlphaArgs(4))//'" blank.')
          ErrorsFound = .TRUE.        
      ELSE
        SELECT CASE(MakeUPPERCase(TRIM(cAlphaArgs(4))))
          CASE('CONTROLLER:OUTDOORAIR')
            Faults(j)%ControllerTypeEnum = iController_AirEconomizer
          
          !CASE ...
        
          CASE DEFAULT
        END SELECT
      ENDIF
      
      Faults(j)%ControllerName = cAlphaArgs(5)
      ! check controller name
      IF (lAlphaFieldBlanks(5)) THEN
          CALL ShowSevereError(TRIM(cFault1)//' = "'//trim(cAlphaArgs(1))//'" invalid '//  &
            trim(cAlphaFieldNames(5))//' = "'//trim(cAlphaArgs(5))//'" blank.')
          ErrorsFound = .TRUE.        
      ENDIF
      
      ! offset - degree of fault
      Faults(j)%Offset = rNumericArgs(1)
    ENDDO
  ENDDO  
  
  RunMeOnceFlag = .TRUE.

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors getting FaultModel input data.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE CheckAndReadFaults





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

END MODULE FaultsManager