MODULE ISO_C_FUNCTION_BINDING
!    USE, INTRINSIC :: ISO_C_BINDING
END MODULE ISO_C_FUNCTION_BINDING

MODULE SQLiteProcedures

! Note most of the procedures below are stubs -- they have no function other than to satisfy compiler requirements

USE DataPrecisionGlobals

    INTEGER, PARAMETER :: MaxMessageSize     = 4096
LOGICAL, SAVE :: WriteOutputToSQLite = .FALSE.
LOGICAL, SAVE :: WriteTabularDataToSQLite = .FALSE.

    INTEGER            :: SQLdbTimeIndex = 0
PUBLIC

CONTAINS

SUBROUTINE SQLiteBegin
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  INTEGER :: result
END SUBROUTINE SQLiteBegin

SUBROUTINE SQLiteCommit
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  INTEGER :: result
END SUBROUTINE SQLiteCommit

SUBROUTINE CreateSQLiteDatabase

   ! SUBROUTINE INFORMATION:
    !       AUTHOR         Linda Lawrie
    !       DATE WRITTEN   September 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine determines if there is a request for SQLite data and fatals if there is.
    !

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    USE InputProcessor, ONLY: GetNumObjectsFound
    USE DataGlobals, ONLY: MaxNameLength
    USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    IF (GetNumObjectsFound('Output:SQLite') > 0) THEN
      CALL ShowSevereError('SQLite is not available in this version')
      CALL ShowContinueError('Request for SQLite output will be ignored')
      WriteOutputToSQLite = .FALSE.
    ELSE
      WriteOutputToSQLite = .FALSE.
    END IF

END SUBROUTINE CreateSQLiteDatabase

SUBROUTINE CreateSQLiteReportVariableDictionaryRecord (reportVariableReportID, storeTypeIndex, &
           indexGroup, keyedValueString, variableName, indexType, units, reportingFreq, scheduleName)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportVariableReportID
    INTEGER, INTENT(IN) :: storeTypeIndex
    CHARACTER(len=*), INTENT(IN) :: indexGroup
    CHARACTER(len=*), INTENT(IN) :: keyedValueString
    CHARACTER(len=*), INTENT(IN) :: variableName
    INTEGER, INTENT(IN) :: indexType
    CHARACTER(len=*), INTENT(IN) :: units
    INTEGER, INTENT(IN) :: reportingFreq
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: scheduleName

END SUBROUTINE CreateSQLiteReportVariableDictionaryRecord

SUBROUTINE CreateSQLiteReportVariableDataRecord (recordIndex, timeIndex, value, reportingInterval, &
    minValue, minValueDate, maxValue, maxValueDate, minutesPerTimeStep)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: recordIndex
    INTEGER, INTENT(IN) :: timeIndex
    REAL(r64), INTENT(IN) :: value
    INTEGER, INTENT(IN), OPTIONAL :: reportingInterval
    REAL(r64), INTENT(IN), OPTIONAL :: maxValue
    INTEGER, INTENT(IN), OPTIONAL :: maxValueDate
    REAL(r64), INTENT(IN), OPTIONAL :: minValue
    INTEGER, INTENT(IN), OPTIONAL :: minValueDate
    INTEGER, INTENT(IN), OPTIONAL :: minutesPerTimeStep

END SUBROUTINE CreateSQLiteReportVariableDataRecord

INTEGER FUNCTION CreateSQLiteTimeIndexRecord(reportingInterval, recordIndex, CumlativeSimulationDays, &
    Month, DayOfMonth, Hour, EndMinute, StartMinute, DST, DayType)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: reportingInterval
        ! See Module Parameter Definitons for LocalReportEach, LocalReportTimeStep, LocalReportHourly, etc.
    INTEGER, INTENT(IN) :: recordIndex
    INTEGER, INTENT(IN) :: CumlativeSimulationDays
    INTEGER, INTENT(IN), OPTIONAL :: Month
    INTEGER, INTENT(IN), OPTIONAL :: DayOfMonth
    INTEGER, INTENT(IN), OPTIONAL :: Hour
    REAL(r64), INTENT(IN), OPTIONAL :: EndMinute
    REAL(r64), INTENT(IN), OPTIONAL :: StartMinute
    INTEGER, INTENT(IN), OPTIONAL :: DST
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: DayType

    CreateSQLiteTimeIndexRecord = -1

END FUNCTION CreateSQLiteTimeIndexRecord

SUBROUTINE CreateSQLiteZoneTable
END SUBROUTINE CreateSQLiteZoneTable

SUBROUTINE CreateSQLiteNominalLightingTable
END SUBROUTINE CreateSQLiteNominalLightingTable

SUBROUTINE CreateSQLiteNominalPeopleTable
END SUBROUTINE CreateSQLiteNominalPeopleTable

SUBROUTINE CreateSQLiteNominalElectricEquipmentTable
END SUBROUTINE CreateSQLiteNominalElectricEquipmentTable

SUBROUTINE CreateSQLiteNominalGasEquipmentTable
END SUBROUTINE CreateSQLiteNominalGasEquipmentTable

SUBROUTINE CreateSQLiteNominalSteamEquipmentTable
END SUBROUTINE CreateSQLiteNominalSteamEquipmentTable

SUBROUTINE CreateSQLiteNominalHotWaterEquipmentTable
END SUBROUTINE CreateSQLiteNominalHotWaterEquipmentTable

SUBROUTINE CreateSQLiteNominalOtherEquipmentTable
END SUBROUTINE CreateSQLiteNominalOtherEquipmentTable

SUBROUTINE CreateSQLiteNominalBaseboardHeatTable
END SUBROUTINE CreateSQLiteNominalBaseboardHeatTable

SUBROUTINE CreateSQLiteSurfacesTable
END SUBROUTINE CreateSQLiteSurfacesTable

SUBROUTINE CreateSQLiteConstructionsTable
END SUBROUTINE CreateSQLiteConstructionsTable

SUBROUTINE CreateSQLiteMaterialsTable
END SUBROUTINE CreateSQLiteMaterialsTable

SUBROUTINE CreateSQLiteZoneListTable
END SUBROUTINE CreateSQLiteZoneListTable

SUBROUTINE CreateSQLiteZoneGroupTable
END SUBROUTINE CreateSQLiteZoneGroupTable

SUBROUTINE CreateSQLiteInfiltrationTable
END SUBROUTINE CreateSQLiteInfiltrationTable

SUBROUTINE CreateSQLiteVentilationTable
END SUBROUTINE CreateSQLiteVentilationTable

SUBROUTINE AddSQLiteZoneSizingRecord (ZoneName, LoadType, CalcDesLoad, UserDesLoad, CalcDesFlow, UserDesFlow, DesDayName, &
    PeakHrMin, PeakTemp, PeakHumRat, MinOAVolFlow)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: ZoneName     ! the name of the zone
    CHARACTER(len=*), INTENT(IN) :: LoadType     ! the description of the input variable
    REAL(r64), INTENT(IN)        :: CalcDesLoad  ! the value from the sizing calculation [W]
    REAL(r64), INTENT(IN)        :: UserDesLoad  ! the value from the sizing calculation modified by user input [W]
    REAL(r64), INTENT(IN)        :: CalcDesFlow  ! calculated design air flow rate [m3/s]
    REAL(r64), INTENT(IN)        :: UserDesFlow  ! user input or modified design air flow rate [m3/s]
    CHARACTER(len=*), INTENT(IN) :: DesDayName   ! the name of the design day that produced the peak
    CHARACTER(len=*), INTENT(IN) :: PeakHrMin    ! time stamp of the peak
    REAL(r64), INTENT(IN)        :: PeakTemp     ! temperature at peak [C]
    REAL(r64), INTENT(IN)        :: PeakHumRat   ! humidity ratio at peak [kg water/kg dry air]
    REAL(r64), INTENT(IN)        :: MinOAVolFlow ! zone design minimum outside air flow rate [m3/s]

END SUBROUTINE AddSQLiteZoneSizingRecord

SUBROUTINE AddSQLiteSystemSizingRecord (SysName, VarDesc, VarValue)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: SysName      ! the name of the system
    CHARACTER(len=*), INTENT(IN) :: VarDesc      ! the description of the input variable
    REAL(r64), INTENT(IN)        :: VarValue     ! the value from the sizing calculation

END SUBROUTINE AddSQLiteSystemSizingRecord

SUBROUTINE AddSQLiteComponentSizingRecord (CompType, CompName, VarDesc, VarValue)

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: CompType  ! the type of the component
    CHARACTER(len=*), INTENT(IN) :: CompName  ! the name of the component
    CHARACTER(len=*), INTENT(IN) :: VarDesc   ! the description of the input variable
    REAL(r64), INTENT(IN)        :: VarValue  ! the value from the sizing calculation

END SUBROUTINE AddSQLiteComponentSizingRecord

SUBROUTINE CreateSQLiteRoomAirModelTable
END SUBROUTINE CreateSQLiteRoomAirModelTable

SUBROUTINE CreateSQLiteMeterDictionaryRecord (meterReportID, storeTypeIndex, indexGroup, &
           keyedValueString, variableName, indexType, units, reportingFreq, scheduleName)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: meterReportID
    INTEGER, INTENT(IN) :: storeTypeIndex
    CHARACTER(len=*), INTENT(IN) :: indexGroup
    CHARACTER(len=*), INTENT(IN) :: keyedValueString
    CHARACTER(len=*), INTENT(IN) :: variableName
    INTEGER, INTENT(IN) :: indexType
    CHARACTER(len=*), INTENT(IN) :: units
    INTEGER, INTENT(IN) :: reportingFreq
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: scheduleName

END SUBROUTINE CreateSQLiteMeterDictionaryRecord

SUBROUTINE CreateSQLiteMeterRecord (recordIndex, timeIndex, value, reportingInterval, &
    minValue, minValueDate, maxValue, maxValueDate, minutesPerTimeStep)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: recordIndex
    INTEGER, INTENT(IN) :: timeIndex
    REAL(r64), INTENT(IN) :: value
    INTEGER, INTENT(IN), OPTIONAL :: reportingInterval
    REAL(r64), INTENT(IN), OPTIONAL :: maxValue
    INTEGER, INTENT(IN), OPTIONAL :: maxValueDate
    REAL(r64), INTENT(IN), OPTIONAL :: minValue
    INTEGER, INTENT(IN), OPTIONAL :: minValueDate
    INTEGER, INTENT(IN), OPTIONAL :: minutesPerTimeStep

END SUBROUTINE CreateSQLiteMeterRecord

SUBROUTINE SQLiteWriteMessageMacro (message)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: message

END SUBROUTINE SQLiteWriteMessageMacro

SUBROUTINE CreateZoneExtendedOutput
END SUBROUTINE CreateZoneExtendedOutput

SUBROUTINE CreateSQLiteDaylightMapTitle (mapNum, mapName, environmentName, zone, refPt1, refPt2, zCoord)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   Sept 2008
    !       MODIFIED       April 2010, Kyle Benne, Added zCoord
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: mapNum
    CHARACTER(len=*), INTENT(IN) :: mapName
    CHARACTER(len=*), INTENT(IN) :: environmentName
    INTEGER, INTENT(IN) :: zone
    REAL(r64), INTENT(IN) :: zCoord
    CHARACTER(len=*), INTENT(IN) :: refPt1
    CHARACTER(len=*), INTENT(IN) :: refPt2

END SUBROUTINE CreateSQLiteDaylightMapTitle

SUBROUTINE CreateSQLiteDaylightMap (mapNum, month, dayOfMonth, hourOfDay, nX, x, nY, y, illuminance)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   Sept 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals, ONLY: r64

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: mapNum
    INTEGER, INTENT(IN) :: month
    INTEGER, INTENT(IN) :: dayOfMonth
    INTEGER, INTENT(IN) :: hourOfDay
    INTEGER, INTENT(IN) :: nX
    REAL(r64), INTENT(IN), DIMENSION(:) :: x
    INTEGER, INTENT(IN) :: nY
    REAL(r64), INTENT(IN), DIMENSION(:) :: y
    REAL(r64), INTENT(IN), DIMENSION(:,:) :: illuminance

END SUBROUTINE CreateSQLiteDaylightMap

SUBROUTINE CreateSQLiteTabularDataRecords(body,rowLabels,columnLabels,ReportName,ReportForString,TableName)

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*),INTENT(IN),DIMENSION(:,:)    :: body  ! row,column
    CHARACTER(len=*),INTENT(IN),DIMENSION(:)      :: rowLabels
    CHARACTER(len=*),INTENT(IN),DIMENSION(:)      :: columnLabels
    CHARACTER(len=*),INTENT(IN)                   :: ReportName
    CHARACTER(len=*),INTENT(IN)                   :: ReportForString
    CHARACTER(len=*),INTENT(IN)                   :: TableName


END SUBROUTINE CreateSQLiteTabularDataRecords

SUBROUTINE InitializeIndexes

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

END SUBROUTINE InitializeIndexes

SUBROUTINE InitializeTabularDataTable

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

END SUBROUTINE InitializeTabularDataTable

SUBROUTINE InitializeTabularDataView

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

END SUBROUTINE InitializeTabularDataView

SUBROUTINE CreateSQLiteSimulationsRecord(ID)

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: ID

END SUBROUTINE CreateSQLiteSimulationsRecord

SUBROUTINE CreateSQLiteEnvironmentPeriodRecord()

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

END SUBROUTINE CreateSQLiteEnvironmentPeriodRecord

SUBROUTINE CreateSQLiteErrorRecord (simulationIndex, errorType, &
    errorMessage, cnt)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   August 2010
    !       RE-ENGINEERED  na
    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the error data to the Errors table
    !
    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API
    ! REFERENCES:
    ! na
    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING
    USE DataPrecisionGlobals, ONLY: r64
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: simulationIndex
    INTEGER, INTENT(IN) :: errorType
    INTEGER, INTENT(IN) :: cnt
    CHARACTER(len=*), INTENT(IN) :: errorMessage
END SUBROUTINE CreateSQLiteErrorRecord
SUBROUTINE UpdateSQLiteErrorRecord (errorMessage)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   August 2010
    !       RE-ENGINEERED  na
    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine updates error records in the Errors table.
    ! This is used to append text to an error that continues on
    ! to the next line.  The errorMessage is always appended to the
    ! last record inserted into the Errors table.
    !
    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API
    ! REFERENCES:
    ! na
    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING
    USE DataPrecisionGlobals, ONLY: r64
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: errorMessage
END SUBROUTINE UpdateSQLiteErrorRecord
SUBROUTINE UpdateSQLiteSimulationRecord (completed, completedSuccessfully)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   August 2010
    !       RE-ENGINEERED  na
    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine updates simulation records in the Simulations table.
    ! A simulation record is first inserted as
    ! completed = false and
    ! completedSuccessfully = false
    ! This subroutine updates those records.
    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API
    ! REFERENCES:
    ! na
    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING
    USE DataPrecisionGlobals, ONLY: r64
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
    ! SUBROUTINE ARGUMENT DEFINITIONS:
    LOGICAL, INTENT(IN) :: completed
    LOGICAL, INTENT(IN) :: completedSuccessfully
END SUBROUTINE UpdateSQLiteSimulationRecord
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
!     Copyright © 2008 Building Synergies, LLC.  All rights reserved.

END MODULE SQLiteProcedures

