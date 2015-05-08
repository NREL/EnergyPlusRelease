MODULE ISO_C_FUNCTION_BINDING
    USE, INTRINSIC :: ISO_C_BINDING
END MODULE ISO_C_FUNCTION_BINDING

MODULE SQLiteProcedures

    ! Module contains routines for SQLite output

    ! MODULE INFORMATION:
    !       AUTHOR         Gregory B. Stark
    !       DATE WRITTEN   Septemeber 2008
    !       MODIFIED       B. Griffith, October 2009, moved TimeIndex to module scope
    !                      encapsulated methods that don't need to be public
    !       MODIFIED       January 2010, Kyle Benne, Added tabular reports and
    !                      did a general cleanup on the database names.
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS MODULE:
    ! The module provides an interface that allows EnergyPlus output to be written to an SQL database

    ! METHODOLOGY EMPLOYED:
    ! Primarily uses SQL92 commands and queries via a Fortran interface to the standard SQLite3 C++ API

    ! REFERENCES:
    ! www.sqlite.org

    ! OTHER NOTES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError

    IMPLICIT NONE ! Enforce explicit typing of all variables

    INTERFACE
        INTEGER(C_INT) FUNCTION SQLiteOpenDatabase (dbNameBuffer) BIND (C, NAME="SQLiteOpenDatabase")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            TYPE(C_PTR), VALUE :: dbNameBuffer
        END FUNCTION SQLiteOpenDatabase

        INTEGER(C_INT) FUNCTION SQLiteExecuteCommand (commandBuffer) BIND (C, NAME="SQLiteExecuteCommand")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            TYPE(C_PTR), VALUE :: commandBuffer
        END FUNCTION SQLiteExecuteCommand

        INTEGER(C_INT) FUNCTION SQLiteCloseDatabase (commandBuffer, commandLength) BIND (C, NAME="SQLiteCloseDatabase")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            TYPE(C_PTR), VALUE :: commandBuffer
            INTEGER(C_INT), VALUE :: commandLength
        END FUNCTION SQLiteCloseDatabase

        INTEGER(C_INT) FUNCTION SQLitePrepareStatement (stmtType, stmtBuffer)  &
           BIND (C, NAME="SQLitePrepareStatement")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            TYPE(C_PTR), VALUE :: stmtBuffer
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLitePrepareStatement

        INTEGER(C_INT) FUNCTION SQLiteColumnInt (stmtType, iCol)  &
           BIND (C, NAME="SQLiteColumnInt")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: stmtType
            INTEGER(C_INT), VALUE :: iCol
        END FUNCTION SQLiteColumnInt

        INTEGER(C_INT) FUNCTION SQLiteBindText (stmtType, stmtInsertLocationIndex, textBuffer)  &
           BIND (C, NAME="SQLiteBindText")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            TYPE(C_PTR), VALUE :: textBuffer
            INTEGER(C_INT), VALUE :: stmtInsertLocationIndex
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteBindText

        INTEGER(C_INT) FUNCTION SQLiteBindInteger (stmtType, stmtInsertLocationIndex, intToInsert)  &
           BIND (C, NAME="SQLiteBindInteger")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: intToInsert
            INTEGER(C_INT), VALUE :: stmtInsertLocationIndex
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteBindInteger

        INTEGER(C_INT) FUNCTION SQLiteBindDouble (stmtType, stmtInsertLocationIndex, doubleToInsert)  &
           BIND (C, NAME="SQLiteBindDouble")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            REAL(C_DOUBLE), VALUE :: doubleToInsert
            INTEGER(C_INT), VALUE :: stmtInsertLocationIndex
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteBindDouble

        INTEGER(C_INT) FUNCTION SQLiteBindNULL (stmtType, stmtInsertLocationIndex) BIND (C, NAME="SQLiteBindNULL")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: stmtInsertLocationIndex
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteBindNULL

        INTEGER(C_INT) FUNCTION SQLiteStepCommand (stmtType) BIND (C, NAME="SQLiteStepCommand")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteStepCommand

        INTEGER(C_INT) FUNCTION SQLiteResetCommand (stmtType) BIND (C, NAME="SQLiteResetCommand")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteResetCommand

        INTEGER(C_INT) FUNCTION SQLiteClearBindings (stmtType) BIND (C, NAME="SQLiteClearBindings")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteClearBindings

        INTEGER(C_INT) FUNCTION SQLiteFinalizeCommand (stmtType) BIND (C, NAME="SQLiteFinalizeCommand")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            INTEGER(C_INT), VALUE :: stmtType
        END FUNCTION SQLiteFinalizeCommand

        INTEGER(C_INT) FUNCTION SQLiteWriteMessage (messageBuffer) BIND (C, NAME="SQLiteWriteMessage")
            USE ISO_C_FUNCTION_BINDING
            IMPLICIT NONE
            TYPE(C_PTR), VALUE :: messageBuffer
        END FUNCTION SQLiteWriteMessage

    END INTERFACE

    ! MODULE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: LocalReportEach     = -1   ! Write out each time UpdatedataandLocalReport is called
    INTEGER, PARAMETER :: LocalReportTimeStep =  0   ! Write out at 'EndTimeStepFlag'
    INTEGER, PARAMETER :: LocalReportHourly   =  1   ! Write out at 'EndHourFlag'
    INTEGER, PARAMETER :: LocalReportDaily    =  2   ! Write out at 'EndDayFlag'
    INTEGER, PARAMETER :: LocalReportMonthly  =  3   ! Write out at end of month (must be determined)
    INTEGER, PARAMETER :: LocalReportSim      =  4   ! Write out once per environment 'EndEnvrnFlag'

    INTEGER, PARAMETER :: ReportVariableDataInsertStmt       = 1
    INTEGER, PARAMETER :: ReportVariableExtendedDataInsertStmt = 2
    INTEGER, PARAMETER :: TimeIndexInsertStmt                = 3
    INTEGER, PARAMETER :: ReportVariableDictionaryInsertStmt = 4
    INTEGER, PARAMETER :: ZoneInfoInsertStmt                 = 7
    INTEGER, PARAMETER :: NominalLightingInsertStmt          = 8
    INTEGER, PARAMETER :: NominalElectricEquipmentInsertStmt = 9
    INTEGER, PARAMETER :: NominalGasEquipmentInsertStmt      = 10
    INTEGER, PARAMETER :: NominalSteamEquipmentInsertStmt    = 11
    INTEGER, PARAMETER :: NominalHotWaterEquipmentInsertStmt = 12
    INTEGER, PARAMETER :: NominalOtherEquipmentInsertStmt    = 13
    INTEGER, PARAMETER :: NominalBaseboardHeatInsertStmt     = 14
    INTEGER, PARAMETER :: SurfaceInsertStmt                  = 15
    INTEGER, PARAMETER :: ConstructionInsertStmt             = 16
    INTEGER, PARAMETER :: ConstructionLayerInsertStmt        = 17
    INTEGER, PARAMETER :: MaterialInsertStmt                 = 18
    INTEGER, PARAMETER :: ZoneListInsertStmt                 = 19
    INTEGER, PARAMETER :: ZoneGroupInsertStmt                = 20
    INTEGER, PARAMETER :: InfiltrationInsertStmt             = 21
    INTEGER, PARAMETER :: VentilationInsertStmt              = 22
    INTEGER, PARAMETER :: NominalPeopleInsertStmt            = 23
    INTEGER, PARAMETER :: ZoneSizingInsertStmt               = 24
    INTEGER, PARAMETER :: SystemSizingInsertStmt             = 25
    INTEGER, PARAMETER :: ComponentSizingInsertStmt          = 26
    INTEGER, PARAMETER :: RoomAirModelInsertStmt             = 27
    INTEGER, PARAMETER :: GroundTemperatureInsertStmt        = 28
    INTEGER, PARAMETER :: WeatherFileInsertStmt              = 29
    INTEGER, PARAMETER :: MeterDictionaryInsertStmt          = 30
    INTEGER, PARAMETER :: ReportMeterDataInsertStmt          = 31
    INTEGER, PARAMETER :: MeterExtendedDataInsertStmt        = 32
    INTEGER, PARAMETER :: ScheduleInsertStmt                 = 33
    INTEGER, PARAMETER :: DaylightMapTitleInsertStmt         = 34
    INTEGER, PARAMETER :: DaylightMapHorlyTitleInsertStmt    = 35
    INTEGER, PARAMETER :: DaylightMapHorlyDataInsertStmt     = 36
    INTEGER, PARAMETER :: EnvironmentPeriodInsertStmt        = 37
    INTEGER, PARAMETER :: SimulationsInsertStmt              = 38
    INTEGER, PARAMETER :: TabularDataInsertStmt              = 39
    INTEGER, PARAMETER :: StringsInsertStmt                  = 40
    INTEGER, PARAMETER :: StringsLookUpStmt                  = 41
    INTEGER, PARAMETER :: ErrorInsertStmt                    = 42
    INTEGER, PARAMETER :: ErrorUpdateStmt                    = 43
    INTEGER, PARAMETER :: SimulationUpdateStmt               = 44

    INTEGER, PARAMETER :: CommandBufferSize  = 4096
    INTEGER, PARAMETER :: MaxMessageSize     = 4096

    CHARACTER(len=1), PARAMETER :: ReportNameId          = '1'
    CHARACTER(len=1), PARAMETER :: ReportForStringId     = '2'
    CHARACTER(len=1), PARAMETER :: TableNameId           = '3'
    CHARACTER(len=1), PARAMETER :: RowNameId             = '4'
    CHARACTER(len=1), PARAMETER :: ColumnNameId          = '5'
    CHARACTER(len=1), PARAMETER :: UnitsId               = '6'

    LOGICAL, SAVE :: WriteOutputToSQLite = .FALSE.
    LOGICAL, SAVE :: WriteTabularDataToSQLite = .FALSE.

    INTEGER            :: SQLdbTimeIndex = 0
    INTEGER, PARAMETER :: SQLITE_ROW = 100

! public routines
PUBLIC CreateSQLiteDatabase
PUBLIC CreateSQLiteReportVariableDictionaryRecord
PUBLIC CreateSQLiteReportVariableDataRecord
PUBLIC CreateSQLiteTimeIndexRecord
PUBLIC CreateZoneExtendedOutput
PUBLIC AddSQLiteZoneSizingRecord
PUBLIC AddSQLiteSystemSizingRecord
PUBLIC AddSQLiteComponentSizingRecord
PUBLIC SQLiteWriteMessageMacro
PUBLIC CreateSQLiteMeterDictionaryRecord
PUBLIC CreateSQLiteMeterRecord
PUBLIC CreateSQLiteDaylightMapTitle
PUBLIC CreateSQLiteDaylightMap
PUBLIC CreateSQLiteTabularDataRecords

! private routines
PRIVATE InitializeSQLiteTables
PRIVATE InitializeViews
PRIVATE InitializeTimeIndicesTable
PRIVATE InitializeReportVariableDataDictionaryTable
PRIVATE InitializeReportVariableDataTables
PRIVATE SQLiteExecuteCommandMacro
PRIVATE SQLitePrepareStatementMacro
PRIVATE SQLiteBindTextMacro
PRIVATE SQLiteOpenDatabaseMacro
PRIVATE SQLiteBindLogicalMacro
PRIVATE InitializeZoneInfoTable
PRIVATE CreateSQLiteZoneTable
PRIVATE InitializeNominalLightingTable
PRIVATE CreateSQLiteNominalLightingTable
PRIVATE InitializeNominalPeopleTable
PRIVATE CreateSQLiteNominalPeopleTable
PRIVATE InitializeNominalElectricEquipmentTable
PRIVATE CreateSQLiteNominalElectricEquipmentTable
PRIVATE InitializeNominalGasEquipmentTable
PRIVATE CreateSQLiteNominalGasEquipmentTable
PRIVATE InitializeNominalSteamEquipmentTable
PRIVATE CreateSQLiteNominalSteamEquipmentTable
PRIVATE InitializeNominalHotWaterEquipmentTable
PRIVATE CreateSQLiteNominalHotWaterEquipmentTable
PRIVATE InitializeNominalOtherEquipmentTable
PRIVATE CreateSQLiteNominalOtherEquipmentTable
PRIVATE InitializeNominalBaseboardHeatTable
PRIVATE CreateSQLiteNominalBaseboardHeatTable
PRIVATE InitializeSurfacesTable
PRIVATE CreateSQLiteSurfacesTable
PRIVATE LogicalToInteger  ! there is also routine with same name in General.f90
PRIVATE CreatExtBooundCondName
PRIVATE InitializeConstructionsTables
PRIVATE CreateSQLiteConstructionsTable
PRIVATE InitializeMaterialsTable
PRIVATE CreateSQLiteMaterialsTable
PRIVATE InitializeZoneListTable
PRIVATE CreateSQLiteZoneListTable
PRIVATE InitializeZoneGroupTable
PRIVATE CreateSQLiteZoneGroupTable
PRIVATE InitializeNominalInfiltrationTable
PRIVATE CreateSQLiteInfiltrationTable
PRIVATE InitializeNominalVentilationTable
PRIVATE CreateSQLiteVentilationTable
PRIVATE InitializeZoneSizingTable
PRIVATE InitializeSystemSizingTable
PRIVATE InitializeComponentSizingTable
PRIVATE InitializeRoomAirModelTable
PRIVATE CreateSQLiteRoomAirModelTable
PRIVATE AdjustReportingHourAndMinutes
PRIVATE InitializeReportMeterDataDictionaryTable
PRIVATE InitializeReportMeterDataTables
PRIVATE GetUnitsString
PRIVATE InitializeSchedulesTable
PRIVATE CreateSQLiteSchedulesTable
PRIVATE InitializeDaylightMapTables
PRIVATE ReportingFreqName
PRIVATE TimestepTypeName
PRIVATE StorageType

CONTAINS

SUBROUTINE CreateSQLiteDatabase

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne, Added tabular tables
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine creates and opens the SQL database (i.e., eplusout.sql)

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING
    USE InputProcessor
    USE DataGlobals, ONLY: MaxNameLength
    USE DataPrecisionGlobals, ONLY: r64

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
    CHARACTER(len=255) :: nameString

    CHARACTER(len=MaxNameLength), DIMENSION(5) :: Alphas
    REAL(r64), DIMENSION(2) :: Number
    INTEGER NumAlpha, NumNumber, IOStat

    INTEGER :: result
!unused    INTEGER :: DATE_TIME (8)
    INTEGER :: NumberOfSQLiteObjects = 0

    NumberOfSQLiteObjects = GetNumObjectsFound('Output:SQLite')

    IF (NumberOfSQLiteObjects == 1) THEN
        WriteOutputToSQLite = .TRUE.
        CALL GetObjectItem('Output:SQLite',1,Alphas,NumAlpha,Number,NumNumber,IOStat)
        SELECT CASE(MakeUPPERCase(Alphas(1)))
          CASE('SIMPLEANDTABULAR')
            nameString = 'eplusout.sql'
            WriteTabularDataToSQLite = .TRUE.
          CASE('SIMPLE')
            nameString = 'eplusout.sql'
          CASE DEFAULT
            CALL ShowSevereError('Output:SQLite Object, Option Type = ' //Alphas(1))
            CALL ShowContinueError('Valid choices are "Simple" and "SimpleAndTabular"')
            WriteOutputToSQLite = .FALSE.
        END SELECT
    ELSE
        WriteOutputToSQLite = .FALSE.
    END IF

    IF (WriteOutputToSQLite) THEN
        result = SQLiteOpenDatabaseMacro (nameString)
        ! If the result from the open command is none zero, then the db failed to open and we issue a fatal error
        IF (result .NE. 0) THEN
          CALL ShowFatalError('The SQLite database failed to open.  Please close any ' &
                           // 'programs that might already be accessing the database.')
        END IF
        result = SQLiteExecuteCommandMacro ('PRAGMA locking_mode = EXCLUSIVE;')
        result = SQLiteExecuteCommandMacro ('PRAGMA journal_mode = OFF;')
        result = SQLiteExecuteCommandMacro ('PRAGMA synchronous = OFF;')

        CALL InitializeSQLiteTables
        CALL InitializeIndexes

        IF (WriteTabularDataToSQLite) THEN
          CALL InitializeTabularDataTable
          CALL InitializeTabularDataView
        END IF

    END IF

END SUBROUTINE CreateSQLiteDatabase

SUBROUTINE InitializeSQLiteTables

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Added Simulations and Envrionment Periods tables.
    !                      August 2010, Kyle Benne
    !                      Added call to initialize Errors table.
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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

    Call InitializeReportVariableDataDictionaryTable
    CALL InitializeReportVariableDataTables

    CALL InitializeReportMeterDataDictionaryTable
    CALL InitializeReportMeterDataTables

    CALL InitializeTimeIndicesTable

    CALL InitializeZoneInfoTable

    CALL InitializeNominalPeopleTable
    CALL InitializeNominalLightingTable
    CALL InitializeNominalElectricEquipmentTable
    CALL InitializeNominalGasEquipmentTable
    CALL InitializeNominalSteamEquipmentTable
    CALL InitializeNominalHotWaterEquipmentTable
    CALL InitializeNominalOtherEquipmentTable
    CALL InitializeNominalBaseboardHeatTable

    CALL InitializeSurfacesTable
    CALL InitializeConstructionsTables
    CALL InitializeMaterialsTable

    CALL InitializeZoneListTable
    CALL InitializeZoneGroupTable

    CALL InitializeNominalInfiltrationTable
    CALL InitializeNominalVentilationTable

    CALL InitializeZoneSizingTable
    CALL InitializeSystemSizingTable
    CALL InitializeComponentSizingTable
    CALL InitializeRoomAirModelTable

    CALL InitializeSchedulesTable

    CALL InitializeDaylightMapTables

    CALL InitializeViews

    CALL InitializeSimulationsTable

    CALL InitializeEnvironmentPeriodsTable

    CALL InitializeErrorsTable


END SUBROUTINE InitializeSQLiteTables

SUBROUTINE InitializeViews

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   May 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes views in the database
    ! Makes end use of the database easier for those unfamiliar
    ! with SQL queries.

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro ( &
        'CREATE VIEW ReportVariableWithTime ' &
            //'AS ' &
            //'SELECT ReportVariableData.*, Time.*, ReportVariableDataDictionary.*, ReportVariableExtendedData.* ' &
            //'FROM ' &
            //'ReportVariableData LEFT OUTER JOIN ReportVariableExtendedData ' &
            //'INNER JOIN Time ' &
            //'INNER JOIN ReportVariableDataDictionary ' &
            //'ON ' &
            //'(ReportVariableData.ReportVariableExtendedDataIndex '  &
            //'= ReportVariableExtendedData.ReportVariableExtendedDataIndex) ' &
            //'AND ' &
            //'(ReportVariableData.TimeIndex = Time.TimeIndex) ' &
            //'AND ' &
            //'(ReportVariableDataDictionary.ReportVariableDataDictionaryIndex '  &
            //'= ReportVariableData.ReportVariableDataDictionaryIndex);')

END SUBROUTINE InitializeViews

SUBROUTINE InitializeTabularDataView

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the TabularDataView in the database.
    ! This view is created in a separate subroutine because it is only
    ! created when SQLITE tabular reports are created.
    ! This makes end use of the database easier for those unfamiliar
    ! with SQL queries.

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro ( &
             'CREATE VIEW TabularDataWithStrings AS SELECT ' &
             //'td.Value Value, ' &
             //'reportn.Value ReportName, ' &
             //'fs.Value ReportForString, ' &
             //'tn.Value TableName, ' &
             //'rn.Value RowName, ' &
             //'cn.Value ColumnName, ' &
             //'u.Value Units, ' &
             //'RowId ' &
             //'FROM TabularData td ' &
             //'INNER JOIN Strings reportn ON reportn.StringIndex=td.ReportNameIndex ' &
             //'INNER JOIN Strings fs ON fs.StringIndex=td.ReportForStringIndex ' &
             //'INNER JOIN Strings tn ON tn.StringIndex=td.TableNameIndex ' &
             //'INNER JOIN Strings rn ON rn.StringIndex=td.RowNameIndex ' &
             //'INNER JOIN Strings cn ON cn.StringIndex=td.ColumnNameIndex ' &
             //'INNER JOIN Strings u ON u.StringIndex=td.UnitsIndex ' &
             //'WHERE ' &
             //'reportn.StringTypeIndex=1 AND ' &
             //'fs.StringTypeIndex=2 AND ' &
             //'tn.StringTypeIndex=3 AND ' &
             //'rn.StringTypeIndex=4 AND ' &
             //'cn.StringTypeIndex=5 AND ' &
             //'u.StringTypeIndex=6;')

END SUBROUTINE InitializeTabularDataView

SUBROUTINE InitializeIndexes

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine creates index tables to speed up common database queries.

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE INDEX rvdDI ON ReportVariableData (ReportVariableDataDictionaryIndex ASC);')
    result = SQLiteExecuteCommandMacro('CREATE INDEX rmdDI ON ReportMeterData (ReportMeterDataDictionaryIndex ASC);')
    result = SQLiteExecuteCommandMacro('CREATE INDEX tiTI ON Time (TimeIndex ASC);')

END SUBROUTINE InitializeIndexes

SUBROUTINE InitializeSimulationsTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   September 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the Simulations SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro("CREATE TABLE Simulations (SimulationIndex INTEGER PRIMARY KEY, " &
        //"EnergyPlusVersion TEXT, TimeStamp TEXT, NumTimestepsPerHour INTEGER, Completed BOOL, " &
        //"CompletedSuccessfully BOOL);")
    result = SQLitePrepareStatementMacro(SimulationsInsertStmt, "INSERT INTO Simulations " &
        //"VALUES(?,?,?,?,'FALSE','FALSE');")
    result = SQLitePrepareStatementMacro (SimulationUpdateStmt, 'UPDATE Simulations SET ' &
        //'Completed = ?, CompletedSuccessfully = ? ' &
        //'WHERE SimulationIndex = (SELECT count(*) FROM Simulations)')

END SUBROUTINE InitializeSimulationsTable

SUBROUTINE CreateSQLiteSimulationsRecord(ID)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine wites a record to the Simiulations table in the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataStringGlobals, ONLY: VerString, CurrentDateTime
    USE DataGlobals, ONLY: NumOfTimeStepInHour
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: ID

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result

    result = SQLiteBindInteger(SimulationsInsertStmt, 1, ID)
    result = SQLiteBindTextMacro(SimulationsInsertStmt, 2, VerString)
    result = SQLiteBindTextMacro(SimulationsInsertStmt, 3, CurrentDateTime)
    result = SQLiteBindInteger(SimulationsInsertStmt, 4, NumOfTimeStepInHour)

    result = SQLiteStepCommand(SimulationsInsertStmt)
    result = SQLiteResetCommand(SimulationsInsertStmt)

END SUBROUTINE CreateSQLiteSimulationsRecord

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER :: result

    result = SQLiteBindLogicalMacro(SimulationUpdateStmt, 1, completed)
    result = SQLiteBindLogicalMacro(SimulationUpdateStmt, 2, completedSuccessfully)

    result = SQLiteStepCommand (SimulationUpdateStmt)
    result = SQLiteResetCommand (SimulationUpdateStmt)

END SUBROUTINE UpdateSQLiteSimulationRecord

SUBROUTINE InitializeTabularDataTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the TabularData SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro("CREATE TABLE TabularData " &
        //"(ReportNameIndex INTEGER, " &
        //"ReportForStringIndex INTEGER, " &
        //"TableNameIndex INTEGER, " &
        //"SimulationIndex INTEGER, " &
        //"RowNameIndex INTEGER, " &
        //"ColumnNameIndex INTEGER, " &
        //"RowId INTEGER, " &
        //"ColumnId INTEGER, " &
        //"Value TEXT, " &
        //"UnitsIndex INTEGER);")

    result = SQLitePrepareStatementMacro(TabularDataInsertStmt, "INSERT INTO TabularData VALUES(?,?,?,?,?,?,?,?,?,?);")

    result = SQLiteExecuteCommandMacro("CREATE TABLE Strings " &
             //"(StringIndex INTEGER PRIMARY KEY, " &
             //"StringTypeIndex  INTEGER, " &
             //"Value TEXT);")

    result = SQLitePrepareStatementMacro(StringsInsertStmt, "INSERT INTO Strings (StringTypeIndex,Value) VALUES(?,?);")
    result = SQLitePrepareStatementMacro(StringsLookUpStmt, "SELECT StringIndex FROM Strings " &
                                        //"WHERE StringTypeIndex=? AND Value=?;")

    result = SQLiteExecuteCommandMacro("CREATE TABLE StringTypes " &
             //"(StringTypeIndex INTEGER PRIMARY KEY, " &
             //"Value TEXT);")

    result =  SQLiteExecuteCommandMacro("INSERT INTO StringTypes VALUES("// ReportNameId //",'ReportName');")
    result =  SQLiteExecuteCommandMacro("INSERT INTO StringTypes VALUES("// ReportForStringId //",'ReportForString');")
    result =  SQLiteExecuteCommandMacro("INSERT INTO StringTypes VALUES("// TableNameId //",'TableName');")
    result =  SQLiteExecuteCommandMacro("INSERT INTO StringTypes VALUES("// RowNameId //",'RowName');")
    result =  SQLiteExecuteCommandMacro("INSERT INTO StringTypes VALUES("// ColumnNameId //",'ColumnName');")
    result =  SQLiteExecuteCommandMacro("INSERT INTO StringTypes VALUES("// UnitsId //",'Units');")

END SUBROUTINE InitializeTabularDataTable

SUBROUTINE CreateSQLiteTabularDataRecords(body,rowLabels,columnLabels,ReportName,ReportForString,TableName)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine wites records to the TabularData table in the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*),INTENT(IN),DIMENSION(:,:)    :: body  ! row,column
    CHARACTER(len=*),INTENT(IN),DIMENSION(:)      :: rowLabels
    CHARACTER(len=*),INTENT(IN),DIMENSION(:)      :: columnLabels
    CHARACTER(len=*),INTENT(IN)                   :: ReportName
    CHARACTER(len=*),INTENT(IN)                   :: ReportForString
    CHARACTER(len=*),INTENT(IN)                   :: TableName


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result
    INTEGER :: iCol,sizeColumnLabels
    INTEGER :: iRow,sizeRowLabels
    INTEGER :: ReportNameIndex,TableNameIndex,ReportForStringIndex
    INTEGER :: RowLabelIndex,ColumnLabelIndex
    INTEGER :: UnitsIndex
    CHARACTER(100) :: rowLabel,columnLabel,units,rowUnits
    INTEGER :: rowLeftBracket,rowUnitStart,rowLabelEnd,rowRightBracket,rowUnitEnd
    INTEGER :: colLeftBracket,colUnitStart,colLabelEnd,colRightBracket,colUnitEnd

    IF (WriteTabularDataToSQLite) then
      sizeColumnLabels = SIZE(columnLabels)
      sizeRowLabels = SIZE(rowLabels)

      DO iRow = 1, sizeRowLabels
        rowLabel = rowLabels(iRow)

        ! Look in the rowLabel for units
        rowLeftBracket = SCAN(rowLabel,'[')
        rowUnitStart = rowLeftBracket + 1
        rowLabelEnd = rowUnitStart - 2
        rowRightBracket = SCAN(rowLabel,']',BACK = .TRUE.)
        rowUnitEnd = rowRightBracket - 1

        IF ((rowLeftBracket .NE. 0) .AND. (rowRightBracket .NE. 0)) THEN
          rowUnits = rowLabel(rowUnitStart:rowUnitEnd)
          rowLabel = TRIM(rowLabel(1:rowLabelEnd))
        ELSE
          rowUnits = ''
        END IF

        DO iCol = 1, sizeColumnLabels

          columnLabel = columnLabels(iCol)

          ! Look in the colLabel for units
          ! This will override units from row
          colLeftBracket = SCAN(columnLabel,'[')
          colUnitStart = colLeftBracket + 1
          colLabelEnd = colUnitStart - 2
          colRightBracket = SCAN(columnLabel,']',BACK = .TRUE.)
          colUnitEnd = colRightBracket - 1

          IF ((colLeftBracket .NE. 0) .AND. (colRightBracket .NE. 0)) THEN
            units = columnLabel(colUnitStart:colUnitEnd)
            columnLabel = TRIM(columnLabel(1:colLabelEnd))
          ELSE
            units = rowUnits
          END IF

          ReportNameIndex = CreateSQLiteStringTableRecord(ReportName,ReportNameId)
          ReportForStringIndex = CreateSQLiteStringTableRecord(ReportForString,ReportForStringId)
          TableNameIndex = CreateSQLiteStringTableRecord(TableName,TableNameId)
          RowLabelIndex = CreateSQLiteStringTableRecord(rowLabel,RowNameId)
          ColumnLabelIndex = CreateSQLiteStringTableRecord(columnLabel,ColumnNameId)
          UnitsIndex = CreateSQLiteStringTableRecord(units,UnitsId)

          result = SQLiteBindInteger(TabularDataInsertStmt,1,ReportNameIndex)
          result = SQLiteBindInteger(TabularDataInsertStmt,2,ReportForStringIndex)
          result = SQLiteBindInteger(TabularDataInsertStmt,3,TableNameIndex)
          result = SQLiteBindInteger(TabularDataInsertStmt,4,1)
          result = SQLiteBindInteger(TabularDataInsertStmt,5,RowLabelIndex)
          result = SQLiteBindInteger(TabularDataInsertStmt,6,ColumnLabelIndex)
          result = SQLiteBindInteger(TabularDataInsertStmt,7,iRow)
          result = SQLiteBindInteger(TabularDataInsertStmt,8,iCol)
          result = SQLiteBindTextMacro(TabularDataInsertStmt,9,body(iRow,iCol))
          result = SQLiteBindInteger(TabularDataInsertStmt,10,UnitsIndex)

          result = SQLiteStepCommand(TabularDataInsertStmt)
          result = SQLiteResetCommand(TabularDataInsertStmt)
          result = SQLiteClearBindings(TabularDataInsertStmt)

        END DO
      END DO
    END IF
END SUBROUTINE CreateSQLiteTabularDataRecords

FUNCTION CreateSQLiteStringTableRecord(stringValue,stringType) RESULT (iOut)
    ! FUNCTION INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       September 2010, Kyle Benne
    !                      Modified FUNCTION syntax to use RESULT keyword
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This FUNCTION wites a record to the Strings table in the SQL database.

    ! The record's id is returned.  If the string already exists in the table
    ! then the id of the existing record is returned.

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: stringValue
    CHARACTER, INTENT(IN)        :: stringType
    INTEGER                      :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: errcode

    iOut = 0

    errcode = SQLiteBindTextMacro(StringsLookUpStmt, 1, stringType)
    errcode = SQLiteBindTextMacro(StringsLookUpStmt, 2, stringValue)
    errcode = SQLiteStepCommand(StringsLookUpStmt)
    IF (errcode .EQ. SQLITE_ROW) then
      ! If the stringKey is already in the database we just return its ID
      iOut = SQLiteColumnIntMacro(StringsLookUpStmt,0)
    ELSE
      ! If the stringKey is not already in the database we create a new record
      ! using the next available ID

      errcode = SQLiteBindTextMacro(StringsInsertStmt, 1, stringType)
      errcode = SQLiteBindTextMacro(StringsInsertStmt, 2, stringValue)
      errcode = SQLiteStepCommand(StringsInsertStmt)
      errcode = SQLiteResetCommand(StringsInsertStmt)
      errcode = SQLiteClearBindings(StringsInsertStmt)

      errcode = SQLiteResetCommand(StringsLookUpStmt)
      errcode = SQLiteStepCommand(StringsLookUpStmt)
      iOut = SQLiteColumnIntMacro(StringsLookUpStmt,0)
    END IF

    errcode = SQLiteResetCommand(StringsLookUpStmt)
    errcode = SQLiteClearBindings(StringsLookUpStmt)

END FUNCTION CreateSQLiteStringTableRecord

SUBROUTINE InitializeTimeIndicesTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Naming cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the TimeIndices SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro( &
        'CREATE TABLE Time (' &
            //'TimeIndex INTEGER PRIMARY KEY, ' &
            //'Month INTEGER, ' &
            //'Day INTEGER, ' &
            //'Hour INTEGER, ' &
            //'Minute INTEGER, ' &
            //'Dst INTEGER, ' &
            //'Interval INTEGER, ' &
            //'IntervalType INTEGER, ' &
            //'SimulationDays INTEGER, ' &
            //'DayType TEXT, ' &
            //'EnvironmentPeriodIndex INTEGER, ' &
            //'WarmupFlag INTEGER);')

    result = SQLitePrepareStatementMacro(TimeIndexInsertStmt, &
        'INSERT INTO Time (' &
            //'TimeIndex, ' &
            //'Month, ' &
            //'Day, ' &
            //'Hour, ' &
            //'Minute, ' &
            //'DST, ' &
            //'Interval, ' &
            //'IntervalType, ' &
            //'SimulationDays, ' &
            //'DayType, ' &
            //'EnvironmentPeriodIndex, ' &
            //'WarmupFlag) ' &
        //'VALUES(?,?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeTimeIndicesTable

SUBROUTINE InitializeReportVariableDataDictionaryTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the .eso data dictionary SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro( &
        'CREATE TABLE ReportVariableDataDictionary(' &
            //'ReportVariableDataDictionaryIndex INTEGER PRIMARY KEY, ' &
            //'VariableType TEXT, ' &
            //'IndexGroup TEXT, ' &
            //'TimestepType TEXT, ' &
            //'KeyValue TEXT, ' &
            //'VariableName TEXT, ' &
            //'ReportingFrequency TEXT, ' &
            //'ScheduleName TEXT, ' &
            //'VariableUnits TEXT);')

    result = SQLitePrepareStatementMacro(ReportVariableDictionaryInsertStmt, &
        'INSERT INTO ReportVariableDataDictionary (' &
            //'ReportVariableDataDictionaryIndex, ' &
            //'VariableType, ' &
            //'IndexGroup, ' &
            //'TimestepType, ' &
            //'KeyValue, ' &
            //'VariableName, ' &
            //'ReportingFrequency, ' &
            //'ScheduleName, ' &
            //'VariableUnits) ' &
        //'VALUES(?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeReportVariableDataDictionaryTable

SUBROUTINE CreateSQLiteReportVariableDictionaryRecord (reportVariableReportID, storeTypeIndex, &
           indexGroup, keyedValueString, variableName, indexType, units, reportingFreq, scheduleName)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine wites an .eso data record to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result

    result = SQLiteBindInteger (ReportVariableDictionaryInsertStmt, 1, reportVariableReportID)
    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 2, StorageType(storeTypeIndex))
    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 3, indexGroup)
    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 4, TimestepTypeName(indexType))
    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 5, keyedValueString)
    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 6, variableName)
    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 7, ReportingFreqName(reportingFreq))

    IF (PRESENT(scheduleName)) THEN
        result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 8, scheduleName)
    ELSE
        result = SQLiteBindNULL (ReportVariableDictionaryInsertStmt, 8)
    END IF

    result = SQLiteBindTextMacro (ReportVariableDictionaryInsertStmt, 9, units)

    result = SQLiteStepCommand (ReportVariableDictionaryInsertStmt)
    result = SQLiteResetCommand (ReportVariableDictionaryInsertStmt)

END SUBROUTINE CreateSQLiteReportVariableDictionaryRecord

SUBROUTINE InitializeEnvironmentPeriodsTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   September 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the EnvironmentPeriod SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro("CREATE TABLE EnvironmentPeriods (EnvironmentPeriodIndex INTEGER PRIMARY KEY, " &
        //"SimulationIndex INTEGER, EnvironmentName TEXT, EnvironmentType INTEGER);")
    result = SQLitePrepareStatementMacro(EnvironmentPeriodInsertStmt, "INSERT INTO EnvironmentPeriods VALUES(?,?,?,?);")

END SUBROUTINE InitializeEnvironmentPeriodsTable

SUBROUTINE CreateSQLiteEnvironmentPeriodRecord()
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine wites EnvironmentPeriod record to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    USE DataGlobals, ONLY: KindOfSim
    USE DataEnvironment, ONLY: EnvironmentName, CurEnvirNum

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result

    result = SQLiteBindInteger (EnvironmentPeriodInsertStmt, 1, CurEnvirNum)
    result = SQLiteBindInteger (EnvironmentPeriodInsertStmt, 2, 1)
    result = SQLiteBindTextMacro (EnvironmentPeriodInsertStmt, 3, EnvironmentName)
    result = SQLiteBindInteger (EnvironmentPeriodInsertStmt, 4, KindOfSim)

    result = SQLiteStepCommand (EnvironmentPeriodInsertStmt)
    result = SQLiteResetCommand (EnvironmentPeriodInsertStmt)

END SUBROUTINE CreateSQLiteEnvironmentPeriodRecord

SUBROUTINE InitializeReportVariableDataTables

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Naming cleanup and add reference to EnvironmentPeriod
    !                      table
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the .eso data SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro( &
        'CREATE TABLE ReportVariableData (' &
            //'TimeIndex INTEGER, ' &
            //'ReportVariableDataDictionaryIndex INTEGER, ' &
            //'VariableValue REAL, ' &
            //'ReportVariableExtendedDataIndex INTEGER);')

    result = SQLitePrepareStatementMacro(ReportVariableDataInsertStmt, &
        'INSERT INTO ReportVariableData (' &
            //'TimeIndex, ' &
            //'ReportVariableDataDictionaryIndex, ' &
            //'VariableValue, ' &
            //'ReportVariableExtendedDataIndex) ' &
        //'VALUES(?,?,?,?);')

    result = SQLiteExecuteCommandMacro( &
        'CREATE TABLE ReportVariableExtendedData (' &
            //'ReportVariableExtendedDataIndex INTEGER PRIMARY KEY, ' &
            //'MaxValue REAL, ' &
            //'MaxMonth INTEGER, ' &
            //'MaxDay INTEGER, ' &
            //'MaxHour INTEGER, ' &
            //'MaxStartMinute INTEGER, ' &
            //'MaxMinute INTEGER, ' &
            //'MinValue REAL, ' &
            //'MinMonth INTEGER, ' &
            //'MinDay INTEGER, ' &
            //'MinHour INTEGER, ' &
            //'MinStartMinute INTEGER, ' &
            //'MinMinute INTEGER);')


    result = SQLitePrepareStatementMacro(ReportVariableExtendedDataInsertStmt, &
        'INSERT INTO ReportVariableExtendedData (' &
            //'ReportVariableExtendedDataIndex, ' &
            //'MaxValue, ' &
            //'MaxMonth, ' &
            //'MaxDay, ' &
            //'MaxHour, ' &
            //'MaxStartMinute, ' &
            //'MaxMinute, ' &
            //'MinValue, ' &
            //'MinMonth, ' &
            //'MinDay, ' &
            //'MinHour, ' &
            //'MinStartMinute, ' &
            //'MinMinute) ' &
        //'VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeReportVariableDataTables

SUBROUTINE CreateSQLiteReportVariableDataRecord (recordIndex, timeIndex, value, reportingInterval, &
    minValue, minValueDate, maxValue, maxValueDate, minutesPerTimeStep)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Naming cleanup and add reference to EnvironmentPeriod
    !                      table.
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes an .eso data record to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING
    USE DataPrecisionGlobals, ONLY: r64

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=MaxMessageSize) :: mesaageBuffer
    INTEGER MinMonth, MinDay, MinHour, MinMinute, MaxMonth, MaxDay, MaxHour, MaxMinute

    INTEGER, SAVE :: extendedDataIndex = 0
    INTEGER, SAVE :: OID = 0

    INTEGER :: result

    OID = OID + 1

    result = SQLiteBindInteger (ReportVariableDataInsertStmt, 1, timeIndex)
    result = SQLiteBindInteger (ReportVariableDataInsertStmt, 2, recordIndex)
    result = SQLiteBindDouble  (ReportVariableDataInsertStmt, 3, value)
    result = SQLiteBindInteger (ReportVariableDataInsertStmt, 4, OID)


    IF (PRESENT(reportingInterval)) THEN
        CALL DecodeMonDayHrMin(minValueDate, MinMonth, MinDay, MinHour, MinMinute)
        CALL DecodeMonDayHrMin(maxValueDate, MaxMonth, MaxDay, MaxHour, MaxMinute)

        CALL AdjustReportingHourAndMinutes(MinHour, MinMinute)
        CALL AdjustReportingHourAndMinutes(MaxHour, MaxMinute)

        extendedDataIndex = extendedDataIndex + 1

        IF (PRESENT(minutesPerTimeStep)) THEN  ! This is for data created by a 'Report Meter' statement
            SELECT CASE (reportingInterval)
                CASE(LocalReportHourly, LocalReportDaily, LocalReportMonthly, LocalReportSim)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 1, extendedDataIndex)

                    result = SQLiteBindDouble  (ReportVariableExtendedDataInsertStmt, 2, maxValue)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 3, maxMonth)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 4, maxDay)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 5, maxHour)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 6, maxMinute - minutesPerTimeStep + 1)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 7, maxMinute)

                    result = SQLiteBindDouble  (ReportVariableExtendedDataInsertStmt, 8, minValue)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 9, minMonth)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 10, minDay)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 11, minHour)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 12, minMinute - minutesPerTimeStep + 1)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 13, minMinute)

                    result = SQLiteStepCommand (ReportVariableExtendedDataInsertStmt)
                    result = SQLiteResetCommand (ReportVariableExtendedDataInsertStmt)

                CASE(LocalReportTimeStep)
                  extendedDataIndex = extendedDataIndex - 1 ! Reset the data index to account for the error
                  result = SQLiteBindNULL (ReportVariableDataInsertStmt, 4)

                CASE DEFAULT
                    extendedDataIndex = extendedDataIndex - 1 ! Reset the data index to account for the error
                    result = SQLiteBindNULL (ReportVariableDataInsertStmt, 4) ! don't report the erroneous data
                    Write(mesaageBuffer,'(A,I5)') 'Illegal reportingInterval passed to CreateSQLiteReportVariableDataRecord: ',  &
                       reportingInterval
                    CALL SQLiteWriteMessageMacro (mesaageBuffer)

            END SELECT

        ELSE  ! This is for data created by a 'Report Variable' statement

            SELECT CASE (reportingInterval)
                CASE(LocalReportDaily, LocalReportMonthly, LocalReportSim)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 1, extendedDataIndex)

                    result = SQLiteBindDouble  (ReportVariableExtendedDataInsertStmt, 2, maxValue)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 3, maxMonth)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 4, maxDay)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 5, maxHour)
                    result = SQLiteBindNULL (ReportVariableExtendedDataInsertStmt, 6)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 7, maxMinute)

                    result = SQLiteBindDouble  (ReportVariableExtendedDataInsertStmt, 8, minValue)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 9, minMonth)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 10, minDay)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 11, minHour)
                    result = SQLiteBindNULL (ReportVariableExtendedDataInsertStmt, 12)
                    result = SQLiteBindInteger (ReportVariableExtendedDataInsertStmt, 13, minMinute)

                    result = SQLiteStepCommand (ReportVariableExtendedDataInsertStmt)
                    result = SQLiteResetCommand (ReportVariableExtendedDataInsertStmt)

                CASE DEFAULT
                    extendedDataIndex = extendedDataIndex - 1 ! Reset the data index to account for the error
                    result = SQLiteBindNULL (ReportVariableDataInsertStmt, 4) ! don't report the erroneous data
                    Write(mesaageBuffer,'(A,I5)') 'Illegal reportingInterval passed to CreateSQLiteReportVariableDataRecord: ',  &
                       reportingInterval
                    CALL SQLiteWriteMessageMacro (mesaageBuffer)

            END SELECT
        END IF
    ELSE
        result = SQLiteBindNULL (ReportVariableDataInsertStmt, 4)
    END IF

    result = SQLiteStepCommand (ReportVariableDataInsertStmt)
    result = SQLiteResetCommand (ReportVariableDataInsertStmt)

END SUBROUTINE CreateSQLiteReportVariableDataRecord

FUNCTION CreateSQLiteTimeIndexRecord(reportingInterval, recordIndex, CumlativeSimulationDays, &
    Month, DayOfMonth, Hour, EndMinute, StartMinute, DST, DayType) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Modified FUNCTION syntax to use RESULT keyword
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function creates a time index and writes the time data to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS
    USE ISO_C_FUNCTION_BINDING
    USE DataPrecisionGlobals, ONLY: r64
    USE DataEnvironment, ONLY: CurEnvirNum
    USE DataGlobals, ONLY: WarmupFlag

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
    INTEGER :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=MaxMessageSize) :: mesaageBuffer
    INTEGER, PARAMETER, DIMENSION(12) :: LastDayOfMonth = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER :: IntEndEMinute = 60
    INTEGER :: IntStartMinute = 0
    INTEGER :: UpdatedHour
    INTEGER :: IntervalInMinutes

    INTEGER :: errcode

    INTRINSIC NINT

    SELECT CASE (reportingInterval)

        CASE(LocalReportEach, LocalReportTimeStep)
            SQLdbTimeIndex = SQLdbTimeIndex + 1

            UpdatedHour = Hour
            IntEndEMinute = NINT(EndMinute)
            IntStartMinute = NINT(StartMinute)
            CALL AdjustReportingHourAndMinutes(UpdatedHour, IntEndEMinute)
            IntervalInMinutes = NINT(EndMinute) - IntStartMinute

            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 1, SQLdbTimeIndex)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 2, Month)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 3, DayOfMonth)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 4, UpdatedHour)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 5, IntEndEMinute)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 6, DST)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 7, IntervalInMinutes)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 8, reportingInterval)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 9, CumlativeSimulationDays)
            errcode = SQLiteBindTextMacro (TimeIndexInsertStmt, 10, dayType)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 11, CurEnvirNum)
            IF ( WarmupFlag ) THEN
              errcode = SQLiteBindInteger (TimeIndexInsertStmt, 12, 1)
            ELSE
              errcode = SQLiteBindInteger (TimeIndexInsertStmt, 12, 0)
            ENDIF

            errcode = SQLiteStepCommand (TimeIndexInsertStmt)
            errcode = SQLiteResetCommand (TimeIndexInsertStmt)

            iOut = SQLdbTimeIndex

        CASE(LocalReportHourly)
            SQLdbTimeIndex = SQLdbTimeIndex + 1

            IntervalInMinutes = 60
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 1, SQLdbTimeIndex)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 2, Month)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 3, DayOfMonth)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 4, Hour)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 5, 0)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 6, DST)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 7, IntervalInMinutes)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 8, reportingInterval)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 9, CumlativeSimulationDays)
            errcode = SQLiteBindTextMacro (TimeIndexInsertStmt, 10, dayType)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 11, CurEnvirNum)

            errcode = SQLiteStepCommand (TimeIndexInsertStmt)
            errcode = SQLiteResetCommand (TimeIndexInsertStmt)

            iOut = SQLdbTimeIndex

        CASE(LocalReportDaily)
            SQLdbTimeIndex = SQLdbTimeIndex + 1

            IntervalInMinutes = 60*24
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 1, SQLdbTimeIndex)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 2, Month)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 3, DayOfMonth)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 4, 24)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 5, 0)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 6, DST)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 7, IntervalInMinutes)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 8, reportingInterval)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 9, CumlativeSimulationDays)
            errcode = SQLiteBindTextMacro (TimeIndexInsertStmt, 10, dayType)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 11, CurEnvirNum)

            errcode = SQLiteStepCommand (TimeIndexInsertStmt)
            errcode = SQLiteResetCommand (TimeIndexInsertStmt)

            iOut = SQLdbTimeIndex

        CASE(LocalReportMonthly)
            SQLdbTimeIndex = SQLdbTimeIndex + 1

            IntervalInMinutes = 60*24*LastDayOfMonth(Month)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 1, SQLdbTimeIndex)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 2, Month)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 3, LastDayOfMonth(Month))
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 4, 24)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 5, 0)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 6)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 7, IntervalInMinutes)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 8, reportingInterval)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 9, CumlativeSimulationDays)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 10)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 11, CurEnvirNum)

            errcode = SQLiteStepCommand (TimeIndexInsertStmt)
            errcode = SQLiteResetCommand (TimeIndexInsertStmt)

            iOut = SQLdbTimeIndex

        CASE(LocalReportSim)
            SQLdbTimeIndex = SQLdbTimeIndex + 1

            IntervalInMinutes = 60*24*CumlativeSimulationDays
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 1, SQLdbTimeIndex)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 2)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 3)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 4)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 5)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 6)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 7, IntervalInMinutes)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 8, reportingInterval)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 9, CumlativeSimulationDays)
            errcode = SQLiteBindNULL    (TimeIndexInsertStmt, 10)
            errcode = SQLiteBindInteger (TimeIndexInsertStmt, 11, CurEnvirNum)

            errcode = SQLiteStepCommand (TimeIndexInsertStmt)
            errcode = SQLiteResetCommand (TimeIndexInsertStmt)

            iOut = SQLdbTimeIndex

        CASE DEFAULT
            Write(mesaageBuffer,'(A,I5)') 'Illegal reportingInterval passed to CreateSQLiteTimeIndexRecord: ', reportingInterval
            CALL SQLiteWriteMessageMacro (mesaageBuffer)
            iOut = -1  ! this is an error condition

    END SELECT


END FUNCTION CreateSQLiteTimeIndexRecord

FUNCTION SQLiteExecuteCommandMacro(sqlStmt) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Removed the use of TRANSFER
    !                      by adding NULL terminator to character string
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This funtion provides a Fortran interface to the SQL execute command API

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    USE ISO_C_FUNCTION_BINDING
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: sqlStmt
    INTEGER                      :: iOut

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=LEN_TRIM(sqlStmt)+1,KIND=C_CHAR) :: cstring

    cstring = TRIM(sqlStmt) // C_NULL_CHAR

    iOut = SQLiteExecuteCommand(C_LOC(cstring))

END FUNCTION SQLiteExecuteCommandMacro

FUNCTION SQLitePrepareStatementMacro(stmtIndex, sqlStmt) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Removed the use of TRANSFER
    !                      by adding NULL terminator to character string
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This funtion provides a Fortran interface to the SQL prepare statement API
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: stmtIndex
    CHARACTER(len=*), INTENT(IN) :: sqlStmt
    INTEGER                      :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=LEN_TRIM(sqlStmt)+1,KIND=C_CHAR) :: cstring

    cstring = TRIM(sqlStmt) // C_NULL_CHAR

    iOut = SQLitePrepareStatement(stmtIndex, C_LOC(cstring))

END FUNCTION SQLitePrepareStatementMacro

FUNCTION SQLiteColumnIntMacro(stmtType, iCol) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   January 2010
    !       MODIFIED       September 2010, Kyle Benne
    !                      Modified FUNCTION syntax to use RESULT keyword
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This funtion provides a Fortran interface to the SQLite SQLiteColumnInt API
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: stmtType
    INTEGER, INTENT(IN) :: iCol
    INTEGER             :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:

    iOut = SQLiteColumnInt(stmtType,iCol)

END FUNCTION SQLiteColumnIntMacro

FUNCTION SQLiteBindTextMacro(stmtIndex, columnIndex, sqlStmt) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Removed the use of TRANSFER
    !                      by adding NULL terminator to character string
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This funtion provides a Fortran interface to the SQL bind text API

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: stmtIndex
    INTEGER, INTENT(IN) :: columnIndex
    CHARACTER(len=*), INTENT(IN) :: sqlStmt
    INTEGER                      :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=LEN_TRIM(sqlStmt)+1,KIND=C_CHAR) :: cstring

    cstring = TRIM(sqlStmt) // C_NULL_CHAR

    iOut = SQLiteBindText (stmtIndex, columnIndex, C_LOC(cstring))

END FUNCTION SQLiteBindTextMacro

FUNCTION SQLiteOpenDatabaseMacro (sqlStmt) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Removed the use of TRANSFER
    !                      by adding NULL terminator to character string
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This funtion provides a Fortran interface to the SQL open database API

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: sqlStmt
    INTEGER                      :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    CHARACTER(LEN=LEN_TRIM(sqlStmt)+1,KIND=C_CHAR) :: cstring

    cstring = TRIM(sqlStmt) // C_NULL_CHAR

    iOut = SQLiteOpenDatabase(C_LOC(cstring))

END FUNCTION SQLiteOpenDatabaseMacro

FUNCTION SQLiteBindLogicalMacro(stmtIndex, columnIndex, valueToInsert) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Modified FUNCTION syntax to use RESULT keyword
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This funtion maps locial values to integers and then provides a Fortran
    ! interface to the SQL bind integer API

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION PARAMETER DEFINITIONS:
    INTEGER :: iOut

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER, INTENT(IN) :: stmtIndex
    INTEGER, INTENT(IN) :: columnIndex
    LOGICAL, INTENT(IN) :: valueToInsert

    iOut = SQLiteBindInteger (stmtIndex, columnIndex, LogicalToInteger(valueToInsert))

END FUNCTION SQLiteBindLogicalMacro

SUBROUTINE CreateZoneExtendedOutput

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes extended zone information (e.g., minimuns & maximuns) to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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

    IF (WriteOutputToSQLite) THEN
        CALL CreateSQLiteZoneTable
        CALL CreateSQLiteNominalLightingTable
        CALL CreateSQLiteNominalPeopleTable

        CALL CreateSQLiteNominalElectricEquipmentTable
        CALL CreateSQLiteNominalGasEquipmentTable
        CALL CreateSQLiteNominalSteamEquipmentTable
        CALL CreateSQLiteNominalHotWaterEquipmentTable
        CALL CreateSQLiteNominalOtherEquipmentTable
        CALL CreateSQLiteNominalBaseboardHeatTable

        CALL CreateSQLiteInfiltrationTable
        CALL CreateSQLiteVentilationTable

        CALL CreateSQLiteSurfacesTable
        CALL CreateSQLiteConstructionsTable
        CALL CreateSQLiteMaterialsTable

        CALL CreateSQLiteZoneListTable
        CALL CreateSQLiteZoneGroupTable
        CALL CreateSQLiteRoomAirModelTable

        CALL CreateSQLiteSchedulesTable

    END IF

END SUBROUTINE CreateZoneExtendedOutput

SUBROUTINE InitializeZoneInfoTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the zone information SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro (  &
       'CREATE TABLE Zones (' &
            //'ZoneIndex INTEGER PRIMARY KEY, ' &
            //'ZoneName TEXT, ' &
            //'RelNorth REAL, ' &
            //'OriginX REAL, ' &
            //'OriginY REAL, ' &
            //'OriginZ REAL, ' &
            //'CentroidX REAL, ' &
            //'CentroidY REAL, ' &
            //'CentroidZ REAL, ' &
            //'OfType INTEGER, ' &
            //'Multiplier REAL, ' &
            //'ListMultiplier REAL, ' &
            //'MinimumX REAL, ' &
            //'MaximumX REAL, ' &
            //'MinimumY REAL, ' &
            //'MaximumY REAL, ' &
            //'MinimumZ REAL, ' &
            //'MaximumZ REAL, ' &
            //'CeilingHeight REAL, ' &
            //'Volume REAL, ' &
            //'InsideConvectionAlgo INTEGER, ' &
            //'OutsideConvectionAlgo INTEGER, ' &
            //'FloorArea REAL, ' &
            //'ExtGrossWallArea REAL, ' &
            //'ExtNetWallArea REAL, ' &
            //'ExtWindowArea REAL, ' &
            //'IsPartOfTotalArea INTEGER);')

    result = SQLitePrepareStatementMacro (ZoneInfoInsertStmt,  &
       'INSERT INTO Zones (' &
            //'ZoneIndex, ' &
            //'ZoneName, ' &
            //'RelNorth, ' &
            //'OriginX, ' &
            //'OriginY, ' &

            //'OriginZ, ' &
            //'CentroidX, ' &
            //'CentroidY, ' &
            //'CentroidZ, ' &
            //'OfType, ' &

            //'Multiplier, ' &
            //'ListMultiplier, ' &
            //'MinimumX, ' &
            //'MaximumX, ' &
            //'MinimumY, ' &

            //'MaximumY, ' &
            //'MinimumZ, ' &
            //'MaximumZ, ' &
            //'CeilingHeight, ' &
            //'Volume, ' &

            //'InsideConvectionAlgo, ' &
            //'OutsideConvectionAlgo, ' &
            //'FloorArea, ' &
            //'ExtGrossWallArea, ' &
            //'ExtNetWallArea, ' &

            //'ExtWindowArea, ' &
            //'IsPartOfTotalArea) ' &
       //'VALUES (?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?);')

END SUBROUTINE InitializeZoneInfoTable

SUBROUTINE CreateSQLiteZoneTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the zone information data to the respective SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance

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
    INTEGER :: isPartOfTotalArea
    INTEGER :: ZoneNum
    INTEGER :: result

    DO ZoneNum = 1, NumOfZones
        IF (Zone(ZoneNum)%isPartOfTotalArea) THEN  !  Map Zone(ZoneNum)%isPartOfTotalArea LOGICAL  to an INTEGER
            isPartOfTotalArea = 1
        ELSE
            isPartOfTotalArea = 0
        END IF

        result = SQLiteBindInteger (ZoneInfoInsertStmt, 1, ZoneNum)
        result = SQLiteBindTextMacro (ZoneInfoInsertStmt, 2, TRIM(Zone(ZoneNum)%Name))
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 3, Zone(ZoneNum)%RelNorth)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 4, Zone(ZoneNum)%OriginX)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 5, Zone(ZoneNum)%OriginY)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 6, Zone(ZoneNum)%OriginZ)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 7, Zone(ZoneNum)%Centroid%X)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 8, Zone(ZoneNum)%Centroid%Y)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 9, Zone(ZoneNum)%Centroid%Z)
        result = SQLiteBindInteger (ZoneInfoInsertStmt, 10, Zone(ZoneNum)%OfType)
        result = SQLiteBindInteger (ZoneInfoInsertStmt, 11, Zone(ZoneNum)%Multiplier)
        result = SQLiteBindInteger (ZoneInfoInsertStmt, 12, Zone(ZoneNum)%ListMultiplier)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 13, Zone(ZoneNum)%MinimumX)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 14, Zone(ZoneNum)%MaximumX)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 15, Zone(ZoneNum)%MinimumY)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 16, Zone(ZoneNum)%MaximumY)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 17, Zone(ZoneNum)%MinimumZ)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 18, Zone(ZoneNum)%MaximumZ)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 19, Zone(ZoneNum)%CeilingHeight)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 20, Zone(ZoneNum)%Volume)
        result = SQLiteBindInteger (ZoneInfoInsertStmt, 21, Zone(ZoneNum)%InsideConvectionAlgo)
        result = SQLiteBindInteger (ZoneInfoInsertStmt, 22, Zone(ZoneNum)%OutsideConvectionAlgo)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 23, Zone(ZoneNum)%FloorArea)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 24, Zone(ZoneNum)%ExtGrossWallArea)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 25, Zone(ZoneNum)%ExtNetWallArea)
        result = SQLiteBindDouble (ZoneInfoInsertStmt, 26, Zone(ZoneNum)%ExtWindowArea)


        result = SQLiteBindInteger (ZoneInfoInsertStmt, 27, isPartOfTotalArea)

        result = SQLiteStepCommand (ZoneInfoInsertStmt)
        result = SQLiteResetCommand (ZoneInfoInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteZoneTable

SUBROUTINE InitializeNominalLightingTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the at nominal lighting SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro ('CREATE TABLE NominalLighting (NominalLightingIndex INTEGER, ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, FractionReturnAir REAL, FractionRadiant REAL, ' &
        //'FractionShortWave REAL, FractionReplaceable REAL, FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro(NominalLightingInsertStmt, 'INSERT INTO NominalLighting VALUES(?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalLightingTable

SUBROUTINE CreateSQLiteNominalLightingTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal lighting data to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: LightNum
    INTEGER :: result

    DO LightNum = 1, TotLights
        result = SQLiteBindInteger (NominalLightingInsertStmt, 1, LightNum)
        result = SQLiteBindTextMacro (NominalLightingInsertStmt, 2, Lights(LightNum)%Name)
        result = SQLiteBindInteger (NominalLightingInsertStmt, 3, Lights(LightNum)%ZonePtr)
        result = SQLiteBindInteger (NominalLightingInsertStmt, 4, Lights(LightNum)%SchedPtr)
        result = SQLiteBindDouble (NominalLightingInsertStmt, 5, Lights(LightNum)%DesignLevel)

        result = SQLiteBindDouble (NominalLightingInsertStmt, 6, Lights(LightNum)%FractionReturnAir)
        result = SQLiteBindDouble (NominalLightingInsertStmt, 7, Lights(LightNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalLightingInsertStmt, 8, Lights(LightNum)%FractionShortWave)
        result = SQLiteBindDouble (NominalLightingInsertStmt, 9, Lights(LightNum)%FractionReplaceable)
        result = SQLiteBindDouble (NominalLightingInsertStmt, 10, Lights(LightNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalLightingInsertStmt, 11, Lights(LightNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalLightingInsertStmt)
        result = SQLiteResetCommand (NominalLightingInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalLightingTable

SUBROUTINE InitializeNominalPeopleTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal people SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalPeople (NominalPeopleIndex INTEGER, '  &
        //'ObjectName TEXT, ZoneIndex INTEGER,' &
        //'NumberOfPeople INTEGER, NumberOfPeopleScheduleIndex INTEGER, ActivityScheduleIndex INTEGER, FractionRadiant REAL, ' &
        //'FractionConvected REAL, WorkEfficiencyScheduleIndex INTEGER, ClothingEfficiencyScheduleIndex INTEGER, ' &
        //'AirVelocityScheduleIndex INTEGER, Fanger INTEGER, Pierce INTEGER, KSU INTEGER, '  &
        //'MRTCalcType INTEGER, SurfaceIndex INTEGER, ' &
        //'AngleFactorListName TEXT, AngleFactorList INTEGER, UserSpecifeidSensibleFraction REAL, Show55Warning INTEGER' &
        //');')

    result = SQLitePrepareStatementMacro(NominalPeopleInsertStmt,  &
       'INSERT INTO NominalPeople VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalPeopleTable

SUBROUTINE CreateSQLiteNominalPeopleTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal people data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance

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
    INTEGER :: PeopleNum
    INTEGER :: result

    DO PeopleNum = 1, TotPeople
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 1, PeopleNum)
        result = SQLiteBindTextMacro (NominalPeopleInsertStmt, 2, People(PeopleNum)%Name)
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 3, People(PeopleNum)%ZonePtr)
        result = SQLiteBindDouble (NominalPeopleInsertStmt, 4, People(PeopleNum)%NumberOfPeople)
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 5, People(PeopleNum)%NumberOfPeoplePtr)

        result = SQLiteBindInteger (NominalPeopleInsertStmt, 6, People(PeopleNum)%ActivityLevelPtr)
        result = SQLiteBindDouble (NominalPeopleInsertStmt, 7, People(PeopleNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalPeopleInsertStmt, 8, People(PeopleNum)%FractionConvected)

        result = SQLiteBindInteger (NominalPeopleInsertStmt, 9, People(PeopleNum)%WorkEffPtr)
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 10, People(PeopleNum)%ClothingPtr)
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 11, People(PeopleNum)%AirVelocityPtr)

        result = SQLiteBindLogicalMacro (NominalPeopleInsertStmt, 12, People(PeopleNum)%Fanger)
        result = SQLiteBindLogicalMacro (NominalPeopleInsertStmt, 13, People(PeopleNum)%Pierce)
        result = SQLiteBindLogicalMacro (NominalPeopleInsertStmt, 14, People(PeopleNum)%KSU)

        result = SQLiteBindInteger (NominalPeopleInsertStmt, 15, People(PeopleNum)%MRTCalcType)
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 16, People(PeopleNum)%SurfacePtr)
        result = SQLiteBindTextMacro (NominalPeopleInsertStmt, 17, People(PeopleNum)%AngleFactorListName)
        result = SQLiteBindInteger (NominalPeopleInsertStmt, 18, People(PeopleNum)%AngleFactorListPtr)
        result = SQLiteBindDouble (NominalPeopleInsertStmt, 19, People(PeopleNum)%UserSpecSensFrac)
        result = SQLiteBindLogicalMacro (NominalPeopleInsertStmt, 20, People(PeopleNum)%Show55Warning)

        result = SQLiteStepCommand (NominalPeopleInsertStmt)
        result = SQLiteResetCommand (NominalPeopleInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalPeopleTable

SUBROUTINE InitializeNominalElectricEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal electric equipment SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalElectricEquipment (NominalElectricEquipmentIndex INTEGER, ' &
        //'ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, ' &
        //'FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, ' &
        //'FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro(NominalElectricEquipmentInsertStmt,  &
       'INSERT INTO NominalElectricEquipment VALUES(?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalElectricEquipmentTable

SUBROUTINE CreateSQLiteNominalElectricEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal electric equipment data to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: ElecEquipNum
    INTEGER :: result

    DO ElecEquipNum = 1, TotElecEquip
        result = SQLiteBindInteger (NominalElectricEquipmentInsertStmt, 1, ElecEquipNum)
        result = SQLiteBindTextMacro (NominalElectricEquipmentInsertStmt, 2, ZoneElectric(ElecEquipNum)%Name)

        result = SQLiteBindInteger (NominalElectricEquipmentInsertStmt, 3, ZoneElectric(ElecEquipNum)%ZonePtr)
        result = SQLiteBindInteger (NominalElectricEquipmentInsertStmt, 4, ZoneElectric(ElecEquipNum)%SchedPtr)
        result = SQLiteBindDouble (NominalElectricEquipmentInsertStmt, 5, ZoneElectric(ElecEquipNum)%DesignLevel)

        result = SQLiteBindDouble (NominalElectricEquipmentInsertStmt, 6, ZoneElectric(ElecEquipNum)%FractionLatent)
        result = SQLiteBindDouble (NominalElectricEquipmentInsertStmt, 7, ZoneElectric(ElecEquipNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalElectricEquipmentInsertStmt, 8, ZoneElectric(ElecEquipNum)%FractionLost)
        result = SQLiteBindDouble (NominalElectricEquipmentInsertStmt, 9, ZoneElectric(ElecEquipNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalElectricEquipmentInsertStmt, 10, ZoneElectric(ElecEquipNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalElectricEquipmentInsertStmt)
        result = SQLiteResetCommand (NominalElectricEquipmentInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalElectricEquipmentTable

SUBROUTINE InitializeNominalGasEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal gas equipment SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalGasEquipment(NominalGasEquipmentIndex INTEGER, ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, ' &
        //'DesignLevel REAL, FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, ' &
        //'FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro(NominalGasEquipmentInsertStmt,  &
       'INSERT INTO NominalGasEquipment VALUES(?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalGasEquipmentTable

SUBROUTINE CreateSQLiteNominalGasEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal gas equipment data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: GasEquipNum
    INTEGER :: result

    DO GasEquipNum = 1, TotGasEquip
        result = SQLiteBindInteger (NominalGasEquipmentInsertStmt, 1, GasEquipNum)
        result = SQLiteBindTextMacro (NominalGasEquipmentInsertStmt, 2, ZoneGas(GasEquipNum)%Name)

        result = SQLiteBindInteger (NominalGasEquipmentInsertStmt, 3, ZoneGas(GasEquipNum)%ZonePtr)
        result = SQLiteBindInteger (NominalGasEquipmentInsertStmt, 4, ZoneGas(GasEquipNum)%SchedPtr)
        result = SQLiteBindDouble (NominalGasEquipmentInsertStmt, 5, ZoneGas(GasEquipNum)%DesignLevel)

        result = SQLiteBindDouble (NominalGasEquipmentInsertStmt, 6, ZoneGas(GasEquipNum)%FractionLatent)
        result = SQLiteBindDouble (NominalGasEquipmentInsertStmt, 7, ZoneGas(GasEquipNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalGasEquipmentInsertStmt, 8, ZoneGas(GasEquipNum)%FractionLost)
        result = SQLiteBindDouble (NominalGasEquipmentInsertStmt, 9, ZoneGas(GasEquipNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalGasEquipmentInsertStmt, 10, ZoneGas(GasEquipNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalGasEquipmentInsertStmt)
        result = SQLiteResetCommand (NominalGasEquipmentInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalGasEquipmentTable

SUBROUTINE InitializeNominalSteamEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal steam equipment SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalSteamEquipment(NominalSteamEquipmentIndex INTEGER, ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, ' &
        //'FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, ' &
        //'FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro(NominalSteamEquipmentInsertStmt,  &
       'INSERT INTO NominalSteamEquipment VALUES(?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalSteamEquipmentTable

SUBROUTINE CreateSQLiteNominalSteamEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal steam equipment data to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: SteamEquipNum
    INTEGER :: result

    DO SteamEquipNum = 1, TotStmEquip
        result = SQLiteBindInteger (NominalSteamEquipmentInsertStmt, 1, SteamEquipNum)
        result = SQLiteBindTextMacro (NominalSteamEquipmentInsertStmt, 2, ZoneSteamEq(SteamEquipNum)%Name)

        result = SQLiteBindInteger (NominalSteamEquipmentInsertStmt, 3, ZoneSteamEq(SteamEquipNum)%ZonePtr)
        result = SQLiteBindInteger (NominalSteamEquipmentInsertStmt, 4, ZoneSteamEq(SteamEquipNum)%SchedPtr)
        result = SQLiteBindDouble (NominalSteamEquipmentInsertStmt, 5, ZoneSteamEq(SteamEquipNum)%DesignLevel)

        result = SQLiteBindDouble (NominalSteamEquipmentInsertStmt, 6, ZoneSteamEq(SteamEquipNum)%FractionLatent)
        result = SQLiteBindDouble (NominalSteamEquipmentInsertStmt, 7, ZoneSteamEq(SteamEquipNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalSteamEquipmentInsertStmt, 8, ZoneSteamEq(SteamEquipNum)%FractionLost)
        result = SQLiteBindDouble (NominalSteamEquipmentInsertStmt, 9, ZoneSteamEq(SteamEquipNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalSteamEquipmentInsertStmt, 10, ZoneSteamEq(SteamEquipNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalSteamEquipmentInsertStmt)
        result = SQLiteResetCommand (NominalSteamEquipmentInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalSteamEquipmentTable

SUBROUTINE InitializeNominalHotWaterEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal hot water equipment SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalHotWaterEquipment(NominalHotWaterEquipmentIndex INTEGER, ' &
        //'ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, SchedNo INTEGER, DesignLevel REAL, FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, ' &
        //'FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro(NominalHotWaterEquipmentInsertStmt,  &
       'INSERT INTO NominalHotWaterEquipment VALUES(?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalHotWaterEquipmentTable

SUBROUTINE CreateSQLiteNominalHotWaterEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal hot water equipment data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: HWEquipNum
    INTEGER :: result

    DO HWEquipNum = 1, TotHWEquip
        result = SQLiteBindInteger (NominalHotWaterEquipmentInsertStmt, 1, HWEquipNum)
        result = SQLiteBindTextMacro (NominalHotWaterEquipmentInsertStmt, 2, ZoneHWEq(HWEquipNum)%Name)

        result = SQLiteBindInteger (NominalHotWaterEquipmentInsertStmt, 3, ZoneHWEq(HWEquipNum)%ZonePtr)
        result = SQLiteBindInteger (NominalHotWaterEquipmentInsertStmt, 4, ZoneHWEq(HWEquipNum)%SchedPtr)
        result = SQLiteBindDouble (NominalHotWaterEquipmentInsertStmt, 5, ZoneHWEq(HWEquipNum)%DesignLevel)

        result = SQLiteBindDouble (NominalHotWaterEquipmentInsertStmt, 6, ZoneHWEq(HWEquipNum)%FractionLatent)
        result = SQLiteBindDouble (NominalHotWaterEquipmentInsertStmt, 7, ZoneHWEq(HWEquipNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalHotWaterEquipmentInsertStmt, 8, ZoneHWEq(HWEquipNum)%FractionLost)
        result = SQLiteBindDouble (NominalHotWaterEquipmentInsertStmt, 9, ZoneHWEq(HWEquipNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalHotWaterEquipmentInsertStmt, 10, ZoneHWEq(HWEquipNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalHotWaterEquipmentInsertStmt)
        result = SQLiteResetCommand (NominalHotWaterEquipmentInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalHotWaterEquipmentTable

SUBROUTINE InitializeNominalOtherEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal other equipment SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro ('CREATE TABLE NominalOtherEquipment(NominalOtherEquipmentIndex INTEGER, ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, FractionLatent REAL, ' &
        //'FractionRadiant REAL, FractionLost REAL, ' &
        //'FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro (NominalOtherEquipmentInsertStmt,  &
       'INSERT INTO NominalOtherEquipment VALUES(?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalOtherEquipmentTable

SUBROUTINE CreateSQLiteNominalOtherEquipmentTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal other equipment data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: OtherEquipNum
    INTEGER :: result

    DO OtherEquipNum = 1, TotOthEquip
        result = SQLiteBindInteger (NominalOtherEquipmentInsertStmt, 1, OtherEquipNum)
        result = SQLiteBindTextMacro (NominalOtherEquipmentInsertStmt, 2, ZoneOtherEq(OtherEquipNum)%Name)

        result = SQLiteBindInteger (NominalOtherEquipmentInsertStmt, 3, ZoneOtherEq(OtherEquipNum)%ZonePtr)
        result = SQLiteBindInteger (NominalOtherEquipmentInsertStmt, 4, ZoneOtherEq(OtherEquipNum)%SchedPtr)
        result = SQLiteBindDouble (NominalOtherEquipmentInsertStmt, 5, ZoneOtherEq(OtherEquipNum)%DesignLevel)

        result = SQLiteBindDouble (NominalOtherEquipmentInsertStmt, 6, ZoneOtherEq(OtherEquipNum)%FractionLatent)
        result = SQLiteBindDouble (NominalOtherEquipmentInsertStmt, 7, ZoneOtherEq(OtherEquipNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalOtherEquipmentInsertStmt, 8, ZoneOtherEq(OtherEquipNum)%FractionLost)
        result = SQLiteBindDouble (NominalOtherEquipmentInsertStmt, 9, ZoneOtherEq(OtherEquipNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalOtherEquipmentInsertStmt, 10, ZoneOtherEq(OtherEquipNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalOtherEquipmentInsertStmt)
        result = SQLiteResetCommand (NominalOtherEquipmentInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalOtherEquipmentTable

SUBROUTINE InitializeNominalBaseboardHeatTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal baseboard heat SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalBaseboardHeaters (NominalBaseboardHeaterIndex INTEGER, ' &
        //'ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, CapatLowTemperature REAL, LowTemperature REAL, CapatHighTemperature REAL, ' &
        //'HighTemperature REAL, FractionRadiant REAL, FractionConvected REAL, EndUseSubcategory TEXT);')

    result = SQLitePrepareStatementMacro(NominalBaseboardHeatInsertStmt,  &
       'INSERT INTO NominalBaseboardHeaters VALUES(?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeNominalBaseboardHeatTable

SUBROUTINE CreateSQLiteNominalBaseboardHeatTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal baseboard heat data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: BBHeatNum
    INTEGER :: result

    DO BBHeatNum = 1, TotBBHeat
        result = SQLiteBindInteger (NominalBaseboardHeatInsertStmt, 1, BBHeatNum)
        result = SQLiteBindTextMacro (NominalBaseboardHeatInsertStmt, 2, ZoneBBHeat(BBHeatNum)%Name)

        result = SQLiteBindInteger (NominalBaseboardHeatInsertStmt, 3, ZoneBBHeat(BBHeatNum)%ZonePtr)
        result = SQLiteBindInteger (NominalBaseboardHeatInsertStmt, 4, ZoneBBHeat(BBHeatNum)%SchedPtr)
        result = SQLiteBindDouble (NominalBaseboardHeatInsertStmt, 5, ZoneBBHeat(BBHeatNum)%CapatLowTemperature)

        result = SQLiteBindDouble (NominalBaseboardHeatInsertStmt, 6, ZoneBBHeat(BBHeatNum)%LowTemperature)
        result = SQLiteBindDouble (NominalBaseboardHeatInsertStmt, 7, ZoneBBHeat(BBHeatNum)%CapatHighTemperature)
        result = SQLiteBindDouble (NominalBaseboardHeatInsertStmt, 8, ZoneBBHeat(BBHeatNum)%HighTemperature)
        result = SQLiteBindDouble (NominalBaseboardHeatInsertStmt, 9, ZoneBBHeat(BBHeatNum)%FractionRadiant)
        result = SQLiteBindDouble (NominalBaseboardHeatInsertStmt, 10, ZoneBBHeat(BBHeatNum)%FractionConvected)

        result = SQLiteBindTextMacro (NominalBaseboardHeatInsertStmt, 11, ZoneBBHeat(BBHeatNum)%EndUseSubcategory)

        result = SQLiteStepCommand (NominalBaseboardHeatInsertStmt)
        result = SQLiteResetCommand (NominalBaseboardHeatInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteNominalBaseboardHeatTable

SUBROUTINE InitializeSurfacesTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the surfaces SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE Surfaces (SurfaceIndex INTEGER PRIMARY KEY, ' &
    //'SurfaceName, ConstructionIndex INTEGER, ' &
    //'ClassName TEXT, Area REAL, GrossArea REAL, Perimeter REAL, ' &
    //'Azimuth REAL, Height REAL, Reveal REAL, ' &
    //'Shape INTEGER, Sides INTEGER, Tilt REAL, Width REAL, HeatTransferSurf INTEGER, ' &
    //'BaseSurfaceIndex INTEGER, ZoneIndex INTEGER, ExtBoundCond INTEGER,  ' &
    //'ExtSolar INTEGER, ExtWind INTEGER' &
    //');')

    result = SQLitePrepareStatementMacro (SurfaceInsertStmt, &
        'INSERT INTO Surfaces VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeSurfacesTable

SUBROUTINE CreateSQLiteSurfacesTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the surfaces data to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataSurfaces
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: SurfaceNumber
    INTEGER :: result

    DO SurfaceNumber = 1, totsurfaces
        result = SQLiteBindInteger (SurfaceInsertStmt, 1, SurfaceNumber)
        result = SQLiteBindTextMacro (SurfaceInsertStmt, 2, Surface(SurfaceNumber)%Name)
        result = SQLiteBindInteger (SurfaceInsertStmt, 3, Surface(SurfaceNumber)%Construction)
        result = SQLiteBindTextMacro (SurfaceInsertStmt, 4, cSurfaceClass(Surface(SurfaceNumber)%class))

        result = SQLiteBindDouble (SurfaceInsertStmt, 5, Surface(SurfaceNumber)%Area)
        result = SQLiteBindDouble (SurfaceInsertStmt, 6, Surface(SurfaceNumber)%GrossArea)

        result = SQLiteBindDouble (SurfaceInsertStmt, 7, Surface(SurfaceNumber)%Perimeter)
        result = SQLiteBindDouble (SurfaceInsertStmt, 8, Surface(SurfaceNumber)%Azimuth)
        result = SQLiteBindDouble (SurfaceInsertStmt, 9, Surface(SurfaceNumber)%Height)
        result = SQLiteBindDouble (SurfaceInsertStmt, 10, Surface(SurfaceNumber)%Reveal)
        result = SQLiteBindInteger (SurfaceInsertStmt, 11, Surface(SurfaceNumber)%Shape)
        result = SQLiteBindInteger (SurfaceInsertStmt, 12, Surface(SurfaceNumber)%Sides)
        result = SQLiteBindDouble (SurfaceInsertStmt, 13, Surface(SurfaceNumber)%Tilt)
        result = SQLiteBindDouble (SurfaceInsertStmt, 14, Surface(SurfaceNumber)%Width)

        result = SQLiteBindLogicalMacro (SurfaceInsertStmt, 15, Surface(SurfaceNumber)%HeatTransSurf)
        result = SQLiteBindInteger (SurfaceInsertStmt, 16, Surface(SurfaceNumber)%BaseSurf)
        result = SQLiteBindInteger (SurfaceInsertStmt, 17, Surface(SurfaceNumber)%Zone)
        result = SQLiteBindInteger (SurfaceInsertStmt, 18, Surface(SurfaceNumber)%ExtBoundCond)

        result = SQLiteBindLogicalMacro (SurfaceInsertStmt, 19, Surface(SurfaceNumber)%ExtSolar)
        result = SQLiteBindLogicalMacro (SurfaceInsertStmt, 20, Surface(SurfaceNumber)%ExtWind)

        result = SQLiteStepCommand (SurfaceInsertStmt)
        result = SQLiteResetCommand (SurfaceInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteSurfacesTable

FUNCTION LogicalToInteger (value) RESULT (iOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Modified FUNCTION syntax to use RESULT keyword
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! Maps logical values into integer values

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER :: iOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    LOGICAL, INTENT(IN) :: value

    IF (value) THEN
        iOut = 1
    ELSE
        iOut = 0
    END IF

END FUNCTION LogicalToInteger

FUNCTION CreatExtBooundCondName (boundaryConditionType, currentName) RESULT (cOut)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Modified FUNCTION syntax to use RESULT keyword
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This subroutine writes the external boundary condition data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataSurfaces, ONLY: ExternalEnvironment, Ground, OtherSideCoefNoCalcExt, OtherSideCoefCalcExt, OtherSideCondModeledExt

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: boundaryConditionType
    CHARACTER(len=*), INTENT(IN) :: currentName
    CHARACTER(len=MaxNameLength) :: cOut

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

    SELECT CASE (boundaryConditionType)

        CASE(ExternalEnvironment)
            cOut = 'External Environment'

        CASE(Ground)
            cOut = 'Ground'

        CASE(OtherSideCoefNoCalcExt,OtherSideCoefCalcExt)
            cOut = 'Other Side Coefficients'

        CASE(OtherSideCondModeledExt)
            cOut = 'Other Side Conditions'

        CASE DEFAULT
            cOut = currentName

    END SELECT

END FUNCTION CreatExtBooundCondName

SUBROUTINE InitializeConstructionsTables

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the constructions SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    ! Constructions table
    result = SQLiteExecuteCommandMacro(  &
       'CREATE TABLE Constructions (ConstructionIndex INTEGER PRIMARY KEY, Name TEXT, TotalLayers INTEGER, ' &
        //'TotalSolidLayers INTEGER, TotalGlassLayers INTEGER, InsideAbsorpVis REAL, OutsideAbsorpVis REAL,' &
        //' InsideAbsorpSolar REAL, OutsideAbsorpSolar REAL, InsideAbsorpThermal REAL, OutsideAbsorpThermal REAL, ' &
        //'OutsideRoughness INTEGER, TypeIsWindow INTEGER, Uvalue REAL' &
        //');')

    result = SQLitePrepareStatementMacro(ConstructionInsertStmt, 'INSERT INTO Constructions VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);')

    ! ConstructionLayers table
    result = SQLiteExecuteCommandMacro('CREATE TABLE ConstructionLayers (ConstructionIndex INTEGER, ' &
        //'LayerIndex INTEGER, MaterialIndex INTEGER);')
    result = SQLitePrepareStatementMacro(ConstructionLayerInsertStmt, 'INSERT INTO ConstructionLayers VALUES(?,?,?);')

END SUBROUTINE InitializeConstructionsTables

SUBROUTINE CreateSQLiteConstructionsTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the constructions data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: ConstructNum, LayerNum
    INTEGER :: result

    DO ConstructNum = 1, TotConstructs
        result = SQLiteBindInteger (ConstructionInsertStmt, 1, ConstructNum)
        result = SQLiteBindTextMacro (ConstructionInsertStmt, 2, Construct(ConstructNum)%Name)

        result = SQLiteBindInteger (ConstructionInsertStmt, 3, Construct(ConstructNum)%TotLayers)
        result = SQLiteBindInteger (ConstructionInsertStmt, 4, Construct(ConstructNum)%TotSolidLayers)
        result = SQLiteBindInteger (ConstructionInsertStmt, 5, Construct(ConstructNum)%TotGlassLayers)

        DO LayerNum = 1, Construct(ConstructNum)%TotLayers
            result = SQLiteBindInteger (ConstructionLayerInsertStmt, 1, ConstructNum)
            result = SQLiteBindInteger (ConstructionLayerInsertStmt, 2, LayerNum)
            result = SQLiteBindInteger (ConstructionLayerInsertStmt, 3, Construct(ConstructNum)%LayerPoint(LayerNum))

            result = SQLiteStepCommand (ConstructionLayerInsertStmt)
            result = SQLiteResetCommand (ConstructionLayerInsertStmt)
        END DO

        result = SQLiteBindDouble (ConstructionInsertStmt, 6, Construct(ConstructNum)%InsideAbsorpVis)
        result = SQLiteBindDouble (ConstructionInsertStmt, 7, Construct(ConstructNum)%OutsideAbsorpVis)
        result = SQLiteBindDouble (ConstructionInsertStmt, 8, Construct(ConstructNum)%InsideAbsorpSolar)
        result = SQLiteBindDouble (ConstructionInsertStmt, 9, Construct(ConstructNum)%OutsideAbsorpSolar)
        result = SQLiteBindDouble (ConstructionInsertStmt, 10, Construct(ConstructNum)%InsideAbsorpThermal)
        result = SQLiteBindDouble (ConstructionInsertStmt, 11, Construct(ConstructNum)%OutsideAbsorpThermal)

        result = SQLiteBindInteger (ConstructionInsertStmt, 12, Construct(ConstructNum)%OutsideRoughness)

        result = SQLiteBindLogicalMacro (ConstructionInsertStmt, 13, Construct(ConstructNum)%TypeIsWindow)

        IF (Construct(ConstructNum)%TotGlassLayers == 0) THEN
            result = SQLiteBindDouble (ConstructionInsertStmt, 14, Construct(ConstructNum)%Uvalue)
        ELSE
            result = SQLiteBindDouble (ConstructionInsertStmt, 14, NominalU(ConstructNum))
        END IF

        result = SQLiteStepCommand (ConstructionInsertStmt)
        result = SQLiteResetCommand (ConstructionInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteConstructionsTable

SUBROUTINE InitializeMaterialsTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the materials SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro(  &
       'CREATE TABLE Materials (MaterialIndex INTEGER PRIMARY KEY, Name TEXT, MaterialType INTEGER, ' &
        //'Roughness INTEGER, ' &
        //'Conductivity REAL, Density REAL, IsoMoistCap REAL, Porosity REAL, Resistance REAL, ' &
        //'ROnly INTEGER, SpecHeat REAL, ThermGradCoef REAL, Thickness REAL, VaporDiffus' &
        //');')

    result = SQLitePrepareStatementMacro(MaterialInsertStmt, 'INSERT INTO Materials VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeMaterialsTable

SUBROUTINE CreateSQLiteMaterialsTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the materials data to the SQL database
    !

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
!    USE InputProcessor, ONLY: GetNumObjectsFound

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
    INTEGER :: MaterialNum
    INTEGER :: result

    DO MaterialNum = 1, TotMaterials
        result = SQLiteBindInteger (MaterialInsertStmt, 1, MaterialNum)
        result = SQLiteBindTextMacro (MaterialInsertStmt, 2, Material(MaterialNum)%Name)

        result = SQLiteBindInteger (MaterialInsertStmt, 3, Material(MaterialNum)%Group)
        result = SQLiteBindInteger (MaterialInsertStmt, 4, Material(MaterialNum)%Roughness)

        result = SQLiteBindDouble (MaterialInsertStmt, 5, Material(MaterialNum)%Conductivity)
        result = SQLiteBindDouble (MaterialInsertStmt, 6, Material(MaterialNum)%Density)
        result = SQLiteBindDouble (MaterialInsertStmt, 7, Material(MaterialNum)%IsoMoistCap)
        result = SQLiteBindDouble (MaterialInsertStmt, 8, Material(MaterialNum)%Porosity)
        result = SQLiteBindDouble (MaterialInsertStmt, 9, Material(MaterialNum)%Resistance)

        result = SQLiteBindLogicalMacro (MaterialInsertStmt, 10, Material(MaterialNum)%ROnly)
        result = SQLiteBindDouble (MaterialInsertStmt, 11, Material(MaterialNum)%SpecHeat)
        result = SQLiteBindDouble (MaterialInsertStmt, 12, Material(MaterialNum)%ThermGradCoef)

        result = SQLiteBindDouble (MaterialInsertStmt, 13, Material(MaterialNum)%Thickness)
        result = SQLiteBindDouble (MaterialInsertStmt, 14, Material(MaterialNum)%VaporDiffus)

        result = SQLiteStepCommand (MaterialInsertStmt)
        result = SQLiteResetCommand (MaterialInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteMaterialsTable

SUBROUTINE InitializeZoneListTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the zone list SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE ZoneLists (ZoneListIndex INTEGER PRIMARY KEY, Name TEXT, ' &
                                      //'ZoneIndex INTEGER);')

    result = SQLitePrepareStatementMacro(ZoneListInsertStmt, 'INSERT INTO ZoneLists VALUES(?,?,?);')

END SUBROUTINE InitializeZoneListTable

SUBROUTINE CreateSQLiteZoneListTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the zone list data to the respective SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance

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
    INTEGER :: ListNum, ZoneNum
    INTEGER :: result

    DO ListNum = 1, NumOfZoneLists
        DO ZoneNum = 1, ZoneList(ListNum)%NumOfZones
            result = SQLiteBindInteger (ZoneListInsertStmt, 1, ListNum)
            result = SQLiteBindTextMacro (ZoneListInsertStmt, 2, ZoneList(ListNum)%Name)
            result = SQLiteBindInteger (ZoneListInsertStmt, 3, ZoneList(ListNum)%Zone(ZoneNum))

            result = SQLiteStepCommand (ZoneListInsertStmt)
            result = SQLiteResetCommand (ZoneListInsertStmt)
        END DO
    END DO

END SUBROUTINE CreateSQLiteZoneListTable

SUBROUTINE InitializeZoneGroupTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the zone group SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro(  &
       'CREATE TABLE ZoneGroups (ZoneGroupIndex INTEGER PRIMARY KEY, ZoneListName TEXT, ZoneListMultiplier INTEGER);')

    result = SQLitePrepareStatementMacro(ZoneGroupInsertStmt, 'INSERT INTO ZoneGroups VALUES(?,?,?);')

END SUBROUTINE InitializeZoneGroupTable

SUBROUTINE CreateSQLiteZoneGroupTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the zone group data to the respective SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance

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
    INTEGER :: GroupNum
    INTEGER :: result

    DO GroupNum = 1, NumOfZoneGroups
        result = SQLiteBindInteger (ZoneGroupInsertStmt, 1, GroupNum)
        result = SQLiteBindTextMacro (ZoneGroupInsertStmt, 2, ZoneGroup(GroupNum)%Name)
        result = SQLiteBindInteger (ZoneGroupInsertStmt, 3, ZoneGroup(GroupNum)%ZoneList)

        result = SQLiteStepCommand (ZoneGroupInsertStmt)
        result = SQLiteResetCommand (ZoneGroupInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteZoneGroupTable

SUBROUTINE InitializeNominalInfiltrationTable
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal infiltration SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalInfiltration (NominalInfiltrationIndex INTEGER PRIMARY KEY, ' &
        //'ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL);')

    result = SQLitePrepareStatementMacro(InfiltrationInsertStmt, '' &
        //'INSERT INTO NominalInfiltration (NominalInfiltrationIndex, ObjectName, ZoneIndex, ScheduleIndex, DesignLevel)' &
        //'VALUES (?,?,?,?,?);')

END SUBROUTINE InitializeNominalInfiltrationTable

SUBROUTINE CreateSQLiteInfiltrationTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal infiltration data to the respective SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance

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
    INTEGER :: StmtNum
    INTEGER :: result

    DO StmtNum = 1, TotInfiltration
        result = SQLiteBindInteger (InfiltrationInsertStmt, 1, StmtNum)
        result = SQLiteBindTextMacro (InfiltrationInsertStmt, 2, Infiltration(StmtNum)%Name)
        result = SQLiteBindInteger (InfiltrationInsertStmt, 3, Infiltration(StmtNum)%ZonePtr)
        result = SQLiteBindInteger (InfiltrationInsertStmt, 4, Infiltration(StmtNum)%SchedPtr)
        result = SQLiteBindDouble (InfiltrationInsertStmt, 5, Infiltration(StmtNum)%DesignLevel)

        result = SQLiteStepCommand (InfiltrationInsertStmt)
        result = SQLiteResetCommand (InfiltrationInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteInfiltrationTable

SUBROUTINE InitializeNominalVentilationTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the nominal ventilation SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE NominalVentilation (NominalVentilationIndex INTEGER PRIMARY KEY, ' &
        //'ObjectName TEXT, ' &
        //'ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL);')

    result = SQLitePrepareStatementMacro(VentilationInsertStmt, 'INSERT INTO NominalVentilation VALUES(?,?,?,?,?);')

END SUBROUTINE InitializeNominalVentilationTable

SUBROUTINE CreateSQLiteVentilationTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the nominal ventilation data to the respective SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance

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
    INTEGER :: StmtNum
    INTEGER :: result

    DO StmtNum = 1, TotVentilation
        result = SQLiteBindInteger (VentilationInsertStmt, 1, StmtNum)
        result = SQLiteBindTextMacro (VentilationInsertStmt, 2, Ventilation(StmtNum)%Name)
        result = SQLiteBindInteger (VentilationInsertStmt, 3, Ventilation(StmtNum)%ZonePtr)
        result = SQLiteBindInteger (VentilationInsertStmt, 4, Ventilation(StmtNum)%SchedPtr)
        result = SQLiteBindDouble (VentilationInsertStmt, 5, Ventilation(StmtNum)%DesignLevel)

        result = SQLiteStepCommand (VentilationInsertStmt)
        result = SQLiteResetCommand (VentilationInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteVentilationTable

SUBROUTINE InitializeZoneSizingTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the zone sizing SQL data tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE ZoneSizes (ZoneName TEXT, LoadType TEXT, ' &
        //'DesLoad REAL, CalcDesFlow REAL, UserDesFlow REAL, DesDayName TEXT, PeakHrMin TEXT, ' &
        //'PeakTemp REAL, PeakHumRat REAL, CalcOutsideAirFlow REAL' &
        //');')

    result = SQLitePrepareStatementMacro(ZoneSizingInsertStmt, 'INSERT INTO ZoneSizes VALUES(?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeZoneSizingTable

SUBROUTINE AddSQLiteZoneSizingRecord (ZoneName, LoadType, CalcDesLoad, UserDesLoad, CalcDesFlow, UserDesFlow, DesDayName, &
    PeakHrMin, PeakTemp, PeakHumRat, MinOAVolFlow)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes one item of zone sizing data to the 'sql' file..

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY : OutputFileInits

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result

    result = SQLiteBindTextMacro (ZoneSizingInsertStmt, 1, ZoneName)
    result = SQLiteBindTextMacro (ZoneSizingInsertStmt, 2, LoadType)

    result = SQLiteBindDouble (ZoneSizingInsertStmt, 3, CalcDesLoad)
    result = SQLiteBindDouble (ZoneSizingInsertStmt, 4, UserDesLoad)
    result = SQLiteBindDouble (ZoneSizingInsertStmt, 5, CalcDesFlow)
    result = SQLiteBindDouble (ZoneSizingInsertStmt, 6, UserDesFlow)

    result = SQLiteBindTextMacro (ZoneSizingInsertStmt, 7, DesDayName)
    result = SQLiteBindTextMacro (ZoneSizingInsertStmt, 8, PeakHrMin)

    result = SQLiteBindDouble (ZoneSizingInsertStmt, 9, PeakTemp)
    result = SQLiteBindDouble (ZoneSizingInsertStmt, 10, PeakHumRat)
    result = SQLiteBindDouble (ZoneSizingInsertStmt, 11, MinOAVolFlow)

    result = SQLiteStepCommand (ZoneSizingInsertStmt)
    result = SQLiteResetCommand (ZoneSizingInsertStmt)


END SUBROUTINE AddSQLiteZoneSizingRecord

SUBROUTINE InitializeSystemSizingTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the system sizing SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE SystemSizes (SystemName TEXT, Description TEXT, Value REAL, Units TEXT);')
    result = SQLitePrepareStatementMacro(SystemSizingInsertStmt, 'INSERT INTO SystemSizes VALUES(?,?,?,?);')

END SUBROUTINE InitializeSystemSizingTable

SUBROUTINE AddSQLiteSystemSizingRecord (SysName, VarDesc, VarValue)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes zone sizing data to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY : OutputFileInits

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: SysName      ! the name of the system
    CHARACTER(len=*), INTENT(IN) :: VarDesc      ! the description of the input variable
    REAL(r64), INTENT(IN)        :: VarValue     ! the value from the sizing calculation

    ! SUBROUTINE PARAMETER DEFINITIONS:

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result
    INTEGER :: item
    CHARACTER(len=len(VarDesc)) :: Description ! Description without units

    item = INDEX (VarDesc, '[')
    IF (item /= 0) THEN
        Description = VarDesc (1:item - 1)
    ELSE
        Description = VarDesc
    END IF

    result = SQLiteBindTextMacro (SystemSizingInsertStmt, 1, SysName)
    result = SQLiteBindTextMacro (SystemSizingInsertStmt, 2, Description)
    result = SQLiteBindDouble    (SystemSizingInsertStmt, 3, VarValue)
    result = SQLiteBindTextMacro (SystemSizingInsertStmt, 4, GetUnitsString (VarDesc))

    result = SQLiteStepCommand  (SystemSizingInsertStmt)
    result = SQLiteResetCommand (SystemSizingInsertStmt)

END SUBROUTINE AddSQLiteSystemSizingRecord

SUBROUTINE InitializeComponentSizingTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the component sizing SQL table

    ! METHODOLOGY EMPLOYED:
    ! na
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API
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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE ComponentSizes (CompType TEXT, CompName TEXT, ' &
            //'Description TEXT, Value REAL, Units TEXT);')
    result = SQLitePrepareStatementMacro(ComponentSizingInsertStmt, 'INSERT INTO ComponentSizes VALUES (?,?,?,?,?);')

END SUBROUTINE InitializeComponentSizingTable

SUBROUTINE AddSQLiteComponentSizingRecord (CompType, CompName, VarDesc, VarValue)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes one item of zone sizing data to the SQL database
    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY : OutputFileInits

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: CompType  ! the type of the component
    CHARACTER(len=*), INTENT(IN) :: CompName  ! the name of the component
    CHARACTER(len=*), INTENT(IN) :: VarDesc   ! the description of the input variable
    REAL(r64), INTENT(IN)        :: VarValue  ! the value from the sizing calculation

    ! SUBROUTINE PARAMETER DEFINITIONS:

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result
    INTEGER :: item
    CHARACTER(len=len(VarDesc)) :: Description ! Description without units

    item = INDEX (VarDesc, '[')
    IF (item /= 0) THEN
        Description = VarDesc (1:item - 1)
    ELSE
        Description = VarDesc
    END IF

    result = SQLiteBindTextMacro (ComponentSizingInsertStmt, 1, CompType)
    result = SQLiteBindTextMacro (ComponentSizingInsertStmt, 2, CompName)
    result = SQLiteBindTextMacro (ComponentSizingInsertStmt, 3, Description)
    result = SQLiteBindDouble    (ComponentSizingInsertStmt, 4, VarValue)
    result = SQLiteBindTextMacro (ComponentSizingInsertStmt, 5, GetUnitsString (VarDesc))

    result = SQLiteStepCommand (ComponentSizingInsertStmt)
    result = SQLiteResetCommand (ComponentSizingInsertStmt)


END SUBROUTINE AddSQLiteComponentSizingRecord

SUBROUTINE InitializeRoomAirModelTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the room air model SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro(  &
       'CREATE TABLE RoomAirModels (ZoneIndex INTEGER PRIMARY KEY, AirModelName TEXT, AirModelType INTEGER, '// &
       'TempCoupleScheme INTEGER, SimAirModel INTEGER);')

    result = SQLitePrepareStatementMacro (RoomAirModelInsertStmt, 'INSERT INTO RoomAirModels VALUES(?,?,?,?,?);')

END SUBROUTINE InitializeRoomAirModelTable


SUBROUTINE CreateSQLiteRoomAirModelTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes room air model data to the respective SQL data tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals
    USE DataHeatBalance
    USE DataRoomAirModel, ONLY : AirModel, RoomAirModel_Mixing, RoomAirModel_Mundt, RoomAirModel_UCSDDV, RoomAirModel_UCSDCV,  &
                                 DirectCoupling, IndirectCoupling, MundtModelUsed, UCSDModelUsed, UserDefinedUsed,   &
                                 RoomAirModel_UserDefined, RoomAirModel_UCSDUFI, RoomAirModel_UCSDUFE

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
    INTEGER :: ZoneNum
    INTEGER :: result

    DO ZoneNum = 1, NumOfZones
        result = SQLiteBindInteger (RoomAirModelInsertStmt, 1, ZoneNum)
        result = SQLiteBindTextMacro (RoomAirModelInsertStmt, 2, TRIM(AirModel(ZoneNum)%AirModelName))
        result = SQLiteBindInteger (RoomAirModelInsertStmt, 3, AirModel(ZoneNum)%AirModelType)
        result = SQLiteBindInteger (RoomAirModelInsertStmt, 4, AirModel(ZoneNum)%TempCoupleScheme)
        result = SQLiteBindLogicalMacro (RoomAirModelInsertStmt, 5, AirModel(ZoneNum)%SimAirModel)

        result = SQLiteStepCommand (RoomAirModelInsertStmt)
        result = SQLiteResetCommand (RoomAirModelInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteRoomAirModelTable

SUBROUTINE SQLiteWriteMessageMacro (message)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   August 2008
    !       MODIFIED       September 2010, Kyle Benne
    !                      Removed the use of TRANSFER
    !                      by adding NULL terminator to character string
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes SQL related information to an SQL error file.

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: message

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                               :: result = -1

    CHARACTER(LEN=LEN_TRIM(message)+1,KIND=C_CHAR) :: cstring

    cstring = TRIM(message) // C_NULL_CHAR

    result = SQLiteWriteMessage (C_LOC(cstring))

END SUBROUTINE SQLiteWriteMessageMacro

SUBROUTINE AdjustReportingHourAndMinutes (hour, minutes)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine converts in the internal time representation into
    ! one easily understood by databases

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(INOUT) :: hour
    INTEGER, INTENT(INOUT) :: minutes

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    SELECT CASE (minutes)
        CASE(60)
            minutes = 0
        CASE DEFAULT
            hour = hour - 1
    END SELECT

END SUBROUTINE AdjustReportingHourAndMinutes

SUBROUTINE InitializeReportMeterDataDictionaryTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the meter data dictionary SQL table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro (  &
       'CREATE TABLE ReportMeterDataDictionary (ReportMeterDataDictionaryIndex INTEGER PRIMARY KEY, ' &
       //'VariableType TEXT, IndexGroup TEXT, TimestepType TEXT, KeyValue TEXT, ' &
       //'VariableName TEXT, ReportingFrequency TEXT, ScheduleName TEXT, VariableUnits TEXT);')

    result = SQLitePrepareStatementMacro (MeterDictionaryInsertStmt, '' &
       //'INSERT INTO ReportMeterDataDictionary (ReportMeterDataDictionaryIndex, VariableType, IndexGroup, ' &
       //'timestepType, KeyValue, VariableName, ReportingFrequency, ScheduleName, VariableUnits) ' &
       //'VALUES(?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeReportMeterDataDictionaryTable

SUBROUTINE CreateSQLiteMeterDictionaryRecord (meterReportID, storeTypeIndex, indexGroup, &
           keyedValueString, variableName, indexType, units, reportingFreq, scheduleName)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the meter data dictionary information to the SQL database

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ISO_C_FUNCTION_BINDING

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER result

    result = SQLiteBindInteger (MeterDictionaryInsertStmt, 1, meterReportID)
    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 2, StorageType(storeTypeIndex))
    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 3, indexGroup)
    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 4, TimestepTypeName(indexType))
    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 5, keyedValueString)
    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 6, variableName)
    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 7, ReportingFreqName(reportingFreq))

    IF (PRESENT(scheduleName)) THEN
        result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 8, scheduleName)
    ELSE
        result = SQLiteBindNULL (MeterDictionaryInsertStmt, 8)
    END IF

    result = SQLiteBindTextMacro (MeterDictionaryInsertStmt, 9, units)

    result = SQLiteStepCommand (MeterDictionaryInsertStmt)
    result = SQLiteResetCommand (MeterDictionaryInsertStmt)

END SUBROUTINE CreateSQLiteMeterDictionaryRecord

SUBROUTINE InitializeReportMeterDataTables

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the meter data SQL tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro(  &
       'CREATE TABLE ReportMeterData(TimeIndex INTEGER, ReportMeterDataDictionaryIndex INTEGER, VariableValue REAL, '  &
        //'ReportVariableExtendedDataIndex INTEGER);')
    result = SQLitePrepareStatementMacro(ReportMeterDataInsertStmt, 'INSERT INTO ReportMeterData VALUES(?,?,?,?);')

    result = SQLiteExecuteCommandMacro('CREATE TABLE ReportMeterExtendedData (ReportMeterExtendedDataIndex INTEGER PRIMARY KEY, ' &
        //'MaxValue REAL, MaxMonth INTEGER, MaxDay INTEGER, MaxHour INTEGER, MaxStartMinute INTEGER, ' &
        //'MaxMinute INTEGER, MinValue REAL, MinMonth INTEGER, MinDay INTEGER, MinHour INTEGER, ' &
        //'MinStartMinute INTEGER, MinMinute INTEGER);')
    result = SQLitePrepareStatementMacro(MeterExtendedDataInsertStmt, &
        'INSERT INTO ReportMeterExtendedData VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?);')

END SUBROUTINE InitializeReportMeterDataTables

SUBROUTINE CreateSQLiteMeterRecord (recordIndex, timeIndex, value, reportingInterval, &
    minValue, minValueDate, maxValue, maxValueDate, minutesPerTimeStep)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   July 2008
    !       MODIFIED       January 2010, Kyle Benne
    !                      Name cleanup and added reference to
    !                      EnvironmentPeriods table.
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the meter data to the respective SQL tables
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
    INTEGER, INTENT(IN) :: recordIndex
    INTEGER, INTENT(IN) :: timeIndex
    REAL(r64), INTENT(IN) :: value
    INTEGER, INTENT(IN), OPTIONAL :: reportingInterval
    REAL(r64), INTENT(IN), OPTIONAL :: maxValue
    INTEGER, INTENT(IN), OPTIONAL :: maxValueDate
    REAL(r64), INTENT(IN), OPTIONAL :: minValue
    INTEGER, INTENT(IN), OPTIONAL :: minValueDate
    INTEGER, INTENT(IN), OPTIONAL :: minutesPerTimeStep

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=MaxMessageSize) :: mesaageBuffer
    INTEGER   :: MinMonth
    INTEGER   :: MinDay
    INTEGER   :: MinHour
    INTEGER   :: MinMinute
    INTEGER   :: MaxMonth
    INTEGER   :: MaxDay
    INTEGER   :: MaxHour
    INTEGER   :: MaxMinute

    INTEGER, SAVE :: extendedDataIndex = 0
    INTEGER, SAVE :: OID = 0

    INTEGER :: result

    OID = OID + 1

    result = SQLiteBindInteger (ReportMeterDataInsertStmt, 1, timeIndex)
    result = SQLiteBindInteger (ReportMeterDataInsertStmt, 2, recordIndex)
    result = SQLiteBindDouble  (ReportMeterDataInsertStmt, 3, value)
    result = SQLiteBindInteger (ReportMeterDataInsertStmt, 4, OID)

    IF (PRESENT(reportingInterval)) THEN
        CALL DecodeMonDayHrMin(minValueDate, MinMonth, MinDay, MinHour, MinMinute)
        CALL DecodeMonDayHrMin(maxValueDate, MaxMonth, MaxDay, MaxHour, MaxMinute)

        CALL AdjustReportingHourAndMinutes(MinHour, MinMinute)
        CALL AdjustReportingHourAndMinutes(MaxHour, MaxMinute)

        extendedDataIndex = extendedDataIndex + 1

        SELECT CASE (reportingInterval)

            CASE(LocalReportHourly, LocalReportDaily, LocalReportMonthly, LocalReportSim)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 1, extendedDataIndex)
                result = SQLiteBindDouble  (MeterExtendedDataInsertStmt, 2, maxValue)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 3, maxMonth)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 4, maxDay)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 5, maxHour)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 6, maxMinute - minutesPerTimeStep + 1)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 7, maxMinute)

                result = SQLiteBindDouble  (MeterExtendedDataInsertStmt, 8, minValue)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 9, minMonth)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 10, minDay)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 11, minHour)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 12, minMinute - minutesPerTimeStep + 1)
                result = SQLiteBindInteger (MeterExtendedDataInsertStmt, 13, minMinute)

                result = SQLiteStepCommand (MeterExtendedDataInsertStmt)
                result = SQLiteResetCommand (MeterExtendedDataInsertStmt)

            CASE(LocalReportTimeStep)
              extendedDataIndex = extendedDataIndex - 1 ! Reset the data index to account for the error
              result = SQLiteBindNULL (ReportVariableDataInsertStmt, 4)
            CASE DEFAULT
                extendedDataIndex = extendedDataIndex - 1 ! Reset the data index to account for the error
                Write(mesaageBuffer,'(A,I5)') 'Illegal reportingInterval passed to CreateSQLiteMeterRecord: ', reportingInterval
                CALL SQLiteWriteMessageMacro (mesaageBuffer)

        END SELECT
    END IF

    result = SQLiteStepCommand (ReportMeterDataInsertStmt)
    result = SQLiteResetCommand (ReportMeterDataInsertStmt)

END SUBROUTINE CreateSQLiteMeterRecord

FUNCTION GetUnitsString (VariableName) RESULT (ThisUnitsString)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Linda K. Lawrie
    !       DATE WRITTEN   October 2003
    !       MODIFIED       August 2008, Greg Stark; modified for use with SQLite
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function extracts the units from a Variable Name string supplied by
    ! the developer in the call to SetupOutputVariable(s).

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE General, ONLY: TrimSigDigits

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN)      :: VariableName
    CHARACTER(len=len(VariableName))  :: ThisUnitsString

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER lbpos  ! position of the left bracket
    INTEGER rbpos  ! position of the right bracket
    CHARACTER(len=*), PARAMETER :: BlankString = ' '

    ! Units are marked with a [

    lbpos = INDEX (VariableName, '[', .true.)  ! from end of variable name

    !!! Errors here are fatal because should only be encountered during development.
    ThisUnitsString = BlankString
    IF (lbpos > 0) THEN
        rbpos = INDEX (VariableName, ']', .true.)
        IF (rbpos == 0 .or. rbpos < lbpos) THEN
            CALL ShowFatalError ('Ill formed Variable Name Units String, VariableName='//TRIM(VariableName))
            ThisUnitsString = VariableName(lbpos:)
        ELSE
            IF ((rbpos-1)-(lbpos+1)+1 > len(VariableName)) THEN
                CALL ShowFatalError ('Units String too long for VariableName: '//TRIM(VariableName)//   &
                    '; it will be truncated to '//TrimSigDigits(len(VariableName))//' characters.')
            END IF
            IF (lbpos+1 <= rbpos-1) THEN
                ThisUnitsString = VariableName(lbpos+1:rbpos-1)
            ELSE
                ThisUnitsString = BlankString
            END IF
        END IF
    END IF

  RETURN

END FUNCTION GetUnitsString

SUBROUTINE InitializeSchedulesTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   September 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the SQL schedule tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE Schedules (ScheduleIndex INTEGER PRIMARY KEY, ScheduleName TEXT, ' &
    //'ScheduleType TEXT, ScheduleMinimum REAL, ScheduleMaximum REAL);')

    result = SQLitePrepareStatementMacro (ScheduleInsertStmt, 'INSERT INTO Schedules VALUES(?,?,?,?,?);')

END SUBROUTINE InitializeSchedulesTable

SUBROUTINE CreateSQLiteSchedulesTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   September 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes schedule data to the SQL schedule tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE ScheduleManager

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
    INTEGER :: ScheduleNumber
    INTEGER :: result
    INTEGER :: NumberOfSchedules

    NumberOfSchedules = GetNumberOfSchedules()
    DO ScheduleNumber = 1, NumberOfSchedules
        result = SQLiteBindInteger   (ScheduleInsertStmt, 1, ScheduleNumber)
        result = SQLiteBindTextMacro (ScheduleInsertStmt, 2, TRIM(GetScheduleName(ScheduleNumber)))
        result = SQLiteBindTextMacro (ScheduleInsertStmt, 3, TRIM(GetScheduleType(ScheduleNumber)))
        result = SQLiteBindDouble    (ScheduleInsertStmt, 4, GetScheduleMinValue(ScheduleNumber))
        result = SQLiteBindDouble    (ScheduleInsertStmt, 5, GetScheduleMaxValue(ScheduleNumber))

        result = SQLiteStepCommand (ScheduleInsertStmt)
        result = SQLiteResetCommand (ScheduleInsertStmt)
    END DO

END SUBROUTINE CreateSQLiteSchedulesTable

SUBROUTINE InitializeDaylightMapTables

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   Sept 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the daylighting SQL data tables

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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
    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE DaylightMaps (MapNumber INTEGER PRIMARY KEY, MapName TEXT, ' &
        //'Environment TEXT, Zone INTEGER, ReferencePt1 TEXT, ReferencePt2 TEXT, Z REAL);')

    result = SQLitePrepareStatementMacro (DaylightMapTitleInsertStmt, 'INSERT INTO DaylightMaps VALUES(?,?,?,?,?,?,?);')

    result = SQLiteExecuteCommandMacro('CREATE TABLE DaylightMapHourlyReports (HourlyReportIndex INTEGER PRIMARY KEY, ' &
        //'MapNumber INTEGER, Month INTEGER, DayOfMonth INTEGER, Hour INTEGER);')

    result = SQLitePrepareStatementMacro (DaylightMapHorlyTitleInsertStmt, &
        'INSERT INTO DaylightMapHourlyReports VALUES(?,?,?,?,?);')

    result = SQLiteExecuteCommandMacro('CREATE TABLE DaylightMapHourlyData (HourlyReportIndex INTEGER, ' &
        //'X REAL, Y REAL, Illuminance REAL);')

    result = SQLitePrepareStatementMacro (DaylightMapHorlyDataInsertStmt, &
        'INSERT INTO DaylightMapHourlyData VALUES(?,?,?,?);')

END SUBROUTINE InitializeDaylightMapTables

SUBROUTINE InitializeErrorsTable

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Kyle Benne
    !       DATE WRITTEN   August 2010
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine initializes the Errors SQL data table

    ! METHODOLOGY EMPLOYED:
    ! Standard SQL92 queries and commands via the Fortran SQLite3 API

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

    INTEGER :: result

    result = SQLiteExecuteCommandMacro('CREATE TABLE Errors (ErrorIndex INTEGER PRIMARY KEY, SimulationIndex INTEGER, ' &
        //'ErrorType INTEGER, ErrorMessage TEXT, Count INTEGER);')

    result = SQLitePrepareStatementMacro (ErrorInsertStmt, 'INSERT INTO Errors VALUES(?,?,?,?,?);')

    result = SQLitePrepareStatementMacro (ErrorUpdateStmt, 'UPDATE Errors SET ' &
             //'ErrorMessage = ErrorMessage || ? WHERE ErrorIndex = (SELECT count(*) FROM Errors)')

END SUBROUTINE InitializeErrorsTable

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER, SAVE :: errorIndex = 0

    INTEGER :: result

    errorIndex = errorIndex + 1

    result = SQLiteBindInteger (ErrorInsertStmt, 1, errorIndex)
    result = SQLiteBindInteger (ErrorInsertStmt, 2, simulationIndex)
    result = SQLiteBindInteger (ErrorInsertStmt, 3, errorType)
    result = SQLiteBindTextMacro (ErrorInsertStmt, 4, errorMessage)
    result = SQLiteBindInteger (ErrorInsertStmt, 5, cnt)

    result = SQLiteStepCommand (ErrorInsertStmt)
    result = SQLiteResetCommand (ErrorInsertStmt)

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER :: result

    result = SQLiteBindTextMacro (ErrorUpdateStmt, 1, ' '//errorMessage)

    result = SQLiteStepCommand (ErrorUpdateStmt)
    result = SQLiteResetCommand (ErrorUpdateStmt)

END SUBROUTINE UpdateSQLiteErrorRecord


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
    USE DataPrecisionGlobals, ONLY: r64

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: mapNum
    CHARACTER(len=*), INTENT(IN) :: mapName
    CHARACTER(len=*), INTENT(IN) :: environmentName
    INTEGER, INTENT(IN) :: zone
    REAL(r64), INTENT(IN) :: zCoord
    CHARACTER(len=*), INTENT(IN) :: refPt1
    CHARACTER(len=*), INTENT(IN) :: refPt2

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result

    result = SQLiteBindInteger   (DaylightMapTitleInsertStmt, 1, mapNum)
    result = SQLiteBindTextMacro (DaylightMapTitleInsertStmt, 2, TRIM(mapName))
    result = SQLiteBindTextMacro (DaylightMapTitleInsertStmt, 3, TRIM(environmentName))
    result = SQLiteBindInteger   (DaylightMapTitleInsertStmt, 4, zone)
    result = SQLiteBindTextMacro (DaylightMapTitleInsertStmt, 5, TRIM(refPt1))
    result = SQLiteBindTextMacro (DaylightMapTitleInsertStmt, 6, TRIM(refPt2))
    result = SQLiteBindDouble (DaylightMapTitleInsertStmt, 7, zCoord)

    result = SQLiteStepCommand (DaylightMapTitleInsertStmt)
    result = SQLiteResetCommand (DaylightMapTitleInsertStmt)

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

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: result
    INTEGER :: xIndex
    INTEGER :: yIndex
    INTEGER, SAVE :: hourlyReportIndex = 1

    result = SQLiteBindInteger (DaylightMapHorlyTitleInsertStmt, 1, hourlyReportIndex)
    result = SQLiteBindInteger (DaylightMapHorlyTitleInsertStmt, 2, mapNum)
    result = SQLiteBindInteger (DaylightMapHorlyTitleInsertStmt, 3, month)
    result = SQLiteBindInteger (DaylightMapHorlyTitleInsertStmt, 4, dayOfMonth)
    result = SQLiteBindInteger (DaylightMapHorlyTitleInsertStmt, 5, hourOfDay)

    result = SQLiteStepCommand (DaylightMapHorlyTitleInsertStmt)
    result = SQLiteResetCommand (DaylightMapHorlyTitleInsertStmt)

    DO yIndex = 1, nY
        DO xIndex = 1, nx
            result = SQLiteBindInteger (DaylightMapHorlyDataInsertStmt, 1, hourlyReportIndex)
            result = SQLiteBindDouble  (DaylightMapHorlyDataInsertStmt, 2, x(xIndex))
            result = SQLiteBindDouble  (DaylightMapHorlyDataInsertStmt, 3, y(yIndex))
            result = SQLiteBindDouble  (DaylightMapHorlyDataInsertStmt, 4, illuminance(xIndex, yIndex))

            result = SQLiteStepCommand (DaylightMapHorlyDataInsertStmt)
            result = SQLiteResetCommand (DaylightMapHorlyDataInsertStmt)
        END DO
    END DO

    hourlyReportIndex = hourlyReportIndex + 1

    RETURN

END SUBROUTINE CreateSQLiteDaylightMap

FUNCTION ReportingFreqName(reportingFreqIndex) RESULT (freqName)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   May 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function returns the reporting frequency name

    ! METHODOLOGY EMPLOYED:
    ! Look it up in a list of valid index types.

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)             :: reportingFreqIndex  ! reporting frequency index
    CHARACTER(len=100) :: freqName

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

    SELECT CASE (reportingFreqIndex)

        CASE(LocalReportEach)
            freqName = 'HVAC System Timestep'

        CASE(LocalReportTimeStep)
            freqName = 'Zone Timestep'

        CASE(LocalReportHourly)
            freqName = 'Hourly'

        CASE(LocalReportDaily)
            freqName = 'Daily'

        CASE(LocalReportMonthly)
            freqName = 'Monthly'

        CASE(LocalReportSim)
            freqName = 'Run Period'

        CASE DEFAULT
            freqName = 'Unknown!!!'

    END SELECT

    RETURN

END FUNCTION ReportingFreqName

FUNCTION TimestepTypeName(timestepType) RESULT (name)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   May 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function returns the reporting frequency name

    ! METHODOLOGY EMPLOYED:
    ! Look it up in a list of valid index types.

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: timestepType  ! timestep index (1 = HVAC System, 2 = Zone)
    CHARACTER(len=100)  :: name

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

    SELECT CASE (timestepType)

        CASE(1)
            name = 'Zone'

        CASE(2)
            name = 'HVAC System'

        CASE DEFAULT
            name = 'Unknown!!!'

    END SELECT

    RETURN

END FUNCTION TimestepTypeName

FUNCTION StorageType(storageTypeIndex) RESULT (name)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   May 2009
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function returns the reporting frequency name

    ! METHODOLOGY EMPLOYED:
    ! Look it up in a list of valid index types.

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: storageTypeIndex  ! storage type index (1 = Sum, 2 = Avg)
    CHARACTER(len=100)  :: name

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

    SELECT CASE (storageTypeIndex)

        CASE(1)
            name = 'Sum'

        CASE(2)
            name = 'Avg'

        CASE DEFAULT
            name = 'Unknown!!!'

    END SELECT

    RETURN

END FUNCTION StorageType

END MODULE SQLiteProcedures

!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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
