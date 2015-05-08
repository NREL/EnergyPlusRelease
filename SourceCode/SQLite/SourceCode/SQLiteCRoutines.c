/*
 *  SQLiteCRoutines.c
 *  Fortran/C interface to the SQLite C++ API (see www.sqlite.org)
 *
 *  Created by Building Synergies, LLC.  September, 2008.
 *  Copyright 2008 Building Synergies, LLC. All rights reserved.
 *
 */

#include "sqlite3.h"
#include <stdio.h>
#include "strings.h"
#include "SQLiteCRoutines.h"

enum {maxNumberOfPreparedStmts = 100};
static sqlite3 *db;
static sqlite3_stmt *stmt[maxNumberOfPreparedStmts];
static FILE *outputFile;

static int callback(void *NotUsed, int argc, char **argv, char **azColName){
    int i;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        for(i=0; i<argc; i++)
            fprintf(outputFile, "SQLite3 message, %s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
    }

    return 0;
}

int SQLiteOpenDatabase (char *dbNameBuffer)
{
    FILE *fopen();
    int rc = -1;
    char *zErrMsg = 0;

    FILE* testf = NULL;

    outputFile = fopen("sqlite.err", "w");
    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        fprintf(outputFile, "SQLite3 message, sqlite.err open for processing!\n");

        testf = fopen(dbNameBuffer, "r");
        if(testf != NULL)
        {
          fclose(testf);
          rc = sqlite3_open_v2(dbNameBuffer, &db, SQLITE_OPEN_READWRITE, NULL);
          // We test to see if we can write to the database
          // If we can't then there are probably locks on the database
          rc = sqlite3_exec(db, "CREATE TABLE Test(x INTEGER PRIMARY KEY)", NULL, 0, &zErrMsg);
          sqlite3_close(db);
          if( rc )
          {
            fprintf(outputFile, "SQLite3 message, can't get exclusive lock on existing database: %s\n", sqlite3_errmsg(db));
            return rc;
          }
          rc = remove( dbNameBuffer );
          if( rc )
          {
            fprintf(outputFile, "SQLite3 message, can't remove old database: %s\n", sqlite3_errmsg(db));
            return rc;
          }
        }

        rc = sqlite3_open_v2(dbNameBuffer, &db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL);
        if( rc )
        {
            fprintf(outputFile, "SQLite3 message, can't open new database: %s\n", sqlite3_errmsg(db));
            sqlite3_close(db);
            return rc;
        }
    }


    return rc;
}

int SQLiteExecuteCommand (char *commandBuffer)
{
    char *zErrMsg = 0;
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        rc = sqlite3_exec(db, commandBuffer, callback, 0, &zErrMsg);
        if( rc != SQLITE_OK ){
            fprintf(outputFile, "SQLite3 message, error: %s\n", zErrMsg);
            sqlite3_free(zErrMsg);
        }
    }
    return rc;
}

int SQLiteCloseDatabase (char *dbNameBuffer, int dbNameLength)
{
    int rc = -1;

    dbNameBuffer[dbNameLength] = 0;

    sqlite3_close(db);
    return rc;
}

int SQLitePrepareStatement (int stmtType, char *stmtBuffer)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_prepare_v2(db, stmtBuffer, -1, &stmt[stmtType], 0);
            if( rc != SQLITE_OK ){
                fprintf(outputFile, "SQLite3 message, sqlite3_prepare_v2 message: %s\n", sqlite3_errmsg(db));
            }
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_prepare_v2 error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteColumnInt (int stmtType, int iCol)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_column_int(stmt[stmtType], iCol);
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_column_int error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteBindText (int stmtType, int stmtInsertLocationIndex, char *textBuffer)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_bind_text(stmt[stmtType], stmtInsertLocationIndex, textBuffer, -1, SQLITE_TRANSIENT);
            if( rc != SQLITE_OK ){
                fprintf(outputFile, "SQLite3 message, sqlite3_bind_text message: %s\n", sqlite3_errmsg(db));
            }
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_bind_text error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteBindInteger (int stmtType, int stmtInsertLocationIndex, int intToInsert)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_bind_int(stmt[stmtType], stmtInsertLocationIndex, intToInsert);
            if( rc != SQLITE_OK ){
                fprintf(outputFile, "SQLite3 message, sqlite3_bind_int message: %s\n", sqlite3_errmsg(db));
            }
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_bind_int error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteBindDouble (int stmtType, int stmtInsertLocationIndex, double doubleToInsert)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_bind_double(stmt[stmtType], stmtInsertLocationIndex, doubleToInsert);
            if( rc != SQLITE_OK ){
                fprintf(outputFile, "SQLite3 message, sqlite3_bind_double message: %s\n", sqlite3_errmsg(db));
            }
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_bind_double error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteBindNULL (int stmtType, int stmtInsertLocationIndex)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_bind_null(stmt[stmtType], stmtInsertLocationIndex);
            if( rc != SQLITE_OK ){
                fprintf(outputFile, "SQLite3 message, sqlite3_bind_null message: %s\n", sqlite3_errmsg(db));
            }
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_bind_null error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteStepCommand (int stmtType)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_step(stmt[stmtType]);
            switch (rc)
                {
                case SQLITE_DONE:
                case SQLITE_OK:
                case SQLITE_ROW:
                    break;

                default:
                    fprintf(outputFile, "SQLite3 message, sqlite3_step message: %i %s, Stmt Type: %i\n", rc, sqlite3_errmsg(db), stmtType);
                    break;
                }
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_step error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteResetCommand (int stmtType)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_reset(stmt[stmtType]);
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_reset error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteClearBindings (int stmtType)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_clear_bindings(stmt[stmtType]);
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_clear_bindings error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteFinalizeCommand (int stmtType)
{
    int rc = -1;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
        if(stmtType < maxNumberOfPreparedStmts) {
            rc = sqlite3_finalize(stmt[stmtType]);
        } else {
            fprintf(outputFile, "SQLite3 message, sqlite3_finalize error: %i exceeds maximum allowed statement number\n", stmtType);
        }
    }
    return rc;
}

int SQLiteWriteMessage (char *message)
{
    int rc = 0;

    if (outputFile == NULL)
        fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
    else
    {
      fprintf(outputFile, "SQLite3 message, %s\n", message);
    }
    return rc;
}

/*    NOTICE
 !
 !     Copyright © 1996-2008 The Board of Trustees of the University of Illinois
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

 !     Copyright © 2008 Building Synergies, LLC.  All rights reserved.
 */


