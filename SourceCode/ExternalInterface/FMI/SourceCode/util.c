///////////////////////////////////////////////////////
/// \file   util.c
///
/// \brief  utility functions
///
/// \author Wangda Zuo, Thierry S. Nouidui, Michael Wetter
///         Simulation Research Group,
///         LBNL,
///         WZuo@lbl.gov
///
/// \date   2011-11-02
///
///
/// This file provides utility functions for fmus
///
/// Copyright (c) 2012, The Regents of the University of California, 
/// through Lawrence Berkeley National Laboratory 
/// subject to receipt of any required approvals from the U.S. Dept. of Energy.
/// All rights reserved.
///////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "util.h"
#include "fmumini.h"

int debug = 0;  // Control for debug information

///////////////////////////////////////////////////////////////////////////////
/// Translate the double variable to string variable.
///
///\param buffer String variable.
///\param r Double varaible.
///////////////////////////////////////////////////////////////////////////////
void doubleToCommaString(char* buffer, double r){
    char* comma;
    sprintf(buffer, "%.16g", r);
    comma = strchr(buffer, '.');
    if (comma) *comma = ',';
}

//////////////////////////////////////////////////////////////////////////
/// Delete the temporary folder
///
///\param tmpPat The path of the temporary folder
///\return 0 if no error occurred
/////////////////////////////////////////////////////////////////////////
int delete(char* tmpPat){
	int version = 0;
	char* cmd;
	char* filenames;
  struct stat st;

  // Ceck if the folder present
  if(stat(tmpPat,&st) != 0){
    printfError("Folder \"%s\" is not existing.\n", tmpPat);
    return -1;
  }    

	cmd = calloc(sizeof(char), strlen(tmpPat) +18);
	if (cmd == NULL){
	    printfError("Fail to allocate memory for cmd.\n", tmpPat);
	    return -1;
	  }
  
  if (WINDOWS)
  	sprintf(cmd, "rd %s /S/Q", tmpPat); // Command in windows
  else
  	sprintf(cmd, "rm -r %s", tmpPat); // Command in Linux


	printfDebug("Generated cmd: \"%s\".\n", cmd);
	if ( system(cmd) != 0 ){	
	//if(rmdir(tmpPat) != 0)
	  printError("Fail to delete the temporary files");
	  return -1;
	}
	printDebug("Deleted temporary files");
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
/// Get temporary path
///
///\param nam Name to be used for temporary path
///\param length Number of characters to be copied from \c nam.
///\return Point of tmpPat if there is no error occurred.
/////////////////////////////////////////////////////////////////////////////
char *getTmpPath(const char *nam, int length)
{
  char *tmpPat;

  tmpPat = calloc(sizeof(char), length+2);
  
  // Define the temporary folder
  if(strncpy(tmpPat, nam, length) == NULL){
    printError("Fail to allocate memory for temp dir.\n");
    return NULL;    
  }
  if(WINDOWS) strcat(tmpPat, "\\");
  else strcat(tmpPat, "/");

  return tmpPat;
}



//////////////////////////////////////////////////////////////////////////////
/// Set the mode in debug so that the debug information will be printed
///
//////////////////////////////////////////////////////////////////////////////
void setDebug( )
{
  debug = 1;
}

//////////////////////////////////////////////////////////////////////////////
/// Print debug message
///
///\param msg Message to be printed for debugging 
//////////////////////////////////////////////////////////////////////////////
void printDebug(const char* msg){
  if (debug == 1)
  {
	  fprintf(stdout, "Debug: ");
	  fprintf(stdout, "%s\n", msg);
  }
}

//////////////////////////////////////////////////////////////////////////////
/// Print formatted debug message
///
///\param str1 Message to be printed for debugging 
///\param str2 String variable to be printed for debugging
//////////////////////////////////////////////////////////////////////////////
void printfDebug(const char* str1, const char* str2){
	if (debug == 1)
	{
		fprintf(stdout, "Debug: ");
		fprintf(stdout, str1, str2);
	}
}

//////////////////////////////////////////////////////////////////////////////
/// Print formatted debug message with Integer
///
///\param str1 Message to be printed for debugging 
///\param integer Integer variable to be printed for debugging
//////////////////////////////////////////////////////////////////////////////
void printfIntDebug(const char* str1, const int integer){
	if (debug == 1)
	{
		fprintf(stdout, "Debug: ");
		fprintf(stdout, str1, integer);
	}
}

//////////////////////////////////////////////////////////////////////////////
/// Print error message
///
///\param msg Error message to be printed
//////////////////////////////////////////////////////////////////////////////
void printError(const char* msg){
  fprintf(stderr, "*** Error: ");
  fprintf(stderr, "%s\n", msg);
}

//////////////////////////////////////////////////////////////////////////////
/// Print formatted error message
///
///\param str1 Error message to be printed
///\param str2 String variable to be printed
//////////////////////////////////////////////////////////////////////////////
void printfError(const char* str1, const char* str2){
  fprintf(stderr, "*** Error: ");
  fprintf(stderr, str1, str2);
}

//////////////////////////////////////////////////////////////////////////////
/// Unpack the fmu using 7z
///
///\param fmuFilNam The name of fmu file
///\param tmpPat Temporary path of the output file
///\return 0 if no error occurred
/////////////////////////////////////////////////////////////////////////////
int unpack(const char* fmuFilNam, const char* tmpPat){
  
  char* cmd;
  FILE *fmuFil = NULL; 

  // Unzip fmu file
  // First, try to open the file to make sure we have read permission
  printDebug("Testing read access to FMU.");
  fmuFil = fopen (fmuFilNam, "r");
  if (fmuFil == NULL){
    printfError("Cannot open file \"%s\".\n", fmuFilNam);
    return -1;
  }
  // Close fmu file
  fclose(fmuFil);

  // Call the zip program
  printfDebug("Extracting zip file %s.\n", fmuFilNam);
  printfDebug("            to path %s.\n", tmpPat);

  cmd = calloc(sizeof(char), strlen(fmuFilNam) + strlen(tmpPat)+22);

  if (cmd == NULL){
    printfError("Failed to allocate memory for cmd.\n", fmuFilNam);
    return -1;  
  }
  
  // Unzip has bug for some zip file
  //sprintf(cmd, "unzip -qq -u %s -d %s", fmuFilNam, tmpPat); 
  // Use 7z instead of unzip, automatically overwrite the existing files and write the technical infor to null
  sprintf(cmd, "7z e -aoa -o%s %s > null", tmpPat, fmuFilNam); 
  printfDebug("Generated cmd: %s\n", cmd);

  if ( system(cmd) != 0 ){
    printError("Failed to extract the zip file");
    free(cmd);
    return -1;
  }
  printDebug("Successfully extracted zip file");
  free(cmd);
  return 0;
}

//////////////////////////////////////////////////////////////////////////////
/// Unpack the fmu using minizip
///
///\param fmuFilNam The name of fmu file
///\param tmpPat Temporary path of the output file
///\return 0 if no error occurred
/////////////////////////////////////////////////////////////////////////////
int unpackminizip(char* fmuFilNam, char* tmpPat){
  return unpackmz(fmuFilNam, tmpPat);
}

