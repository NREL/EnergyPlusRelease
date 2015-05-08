///////////////////////////////////////////////////////
/// \file   main.c
///
///
/// \author Thierry Stephane Nouidui,
///         Simulation Research Group, 
///         LBNL,
///         TSNouidui@lbl.gov
///
/// \date   2011-11-11
///
/// This files contains the implementation of all 
/// energyplus functions that are needed to mapped 
/// to fmiFunctions

/// This file is based on main.c that is copyrighted by
/// QTronic GmbH and that is distributed under the BSD license. 
/// The original file, its copyright notice and its license can 
/// be found in /Copyright
///
/// The file has been modified for use with the FMU standard for 
/// co-simulation. The original file was developed for model exchange.
/// The original file used 0 as indicator for failure and 
/// 1 as indicator for success.  
/// The new file uses 0 as indicator for success according to STL.
///
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "main.h"
#include <ctype.h>
#include <expat.h>
#include "util.h"
#include <sys/stat.h>
#define BUFSIZE 4096

#ifdef _MSC_VER
#define LIB_EXT   ".dll"
#define LIB_PATH  "%s%s%s.dll"
#define FMU_ROOT_DIR "tmp-fmus\\"
#define XML_FILE "\\modelDescription.xml"
#define BIN_WIN "\\binaries\\winxx\\"
#define BIN_WIN32 "\\binaries\\win32\\"
#define BIN_WIN64 "\\binaries\\win64\\"
#if defined(_WIN32) 
#define OperSys 1
#endif
#if defined(_WIN64)
#define OperSys 2
#endif
#elif __linux__
#include <sys/types.h>
#include<sys/sysinfo.h>
#include <dlfcn.h>
#define LIB_EXT   ".so"
#define LIB_PATH  "%s%s%s.so"
#define FMU_ROOT_DIR "tmp-fmus//"
#define XML_FILE "//modelDescription.xml"
#define BIN_LIN "//binaries//linuxxx//"
#define BIN_LIN32 "//binaries//linux32//"
#define BIN_LIN64 "//binaries//linux64//"
#if defined(__LP32__) 
#define OperSys 3
#elif defined(__LP64__) || defined(__x86_64__) || defined(__ia64__) || defined(__amd64__) || defined(__ppc64__) 
#define OperSys 4
#endif

#elif __APPLE__
#include <dlfcn.h>
#define LIB_EXT   ".dylib"
#define LIB_PATH  "%s%s%s.dylib"
#define FMU_ROOT_DIR "tmp-fmus//"
#define XML_FILE "//modelDescription.xml"
#define BIN_MAC "//binaries//macxx//"
#define BIN_MAC32 "//binaries//mac32//"
#define BIN_MAC64 "//binaries//mac64//"
#if defined(__LP32__) 
#define OperSys 5
#elif defined(__LP64__) || defined(__x86_64__) || defined(__ia64__) || defined(__amd64__) || defined(__ppc64__) 
#define OperSys 6
#endif
#endif



//static FMU fmu; 
FMU fmu; // the fmu to simulate


///////////////////////////////////////////////////////////////////////////////
/// This function checks the operating system and returns an error message 
/// if the operating system is not supported
///
///\param name Error message
///////////////////////////////////////////////////////////////////////////////
  fmiInteger checkOperatingSystem(fmiString errorMessage){
	#ifndef _MSC_VER
	  fmiString msg = "FunctionalMock-up Unit for co-simulation is currently only supported on Windows.";
	  strncpy(errorMessage, msg, strlen(msg));
	  return -1;
    #endif
	return 0;	
}
#ifdef _MSC_VER

///////////////////////////////////////////////////////////////////////////////
/// Check if string \c name is in the string array \c array[]. This function is 
///
///\param name The string to be checked.
///\param kind The type of string to be checked.
///\param array The list of strings for comparison.
///\param n The length of array.
///\return The index of matched string in the \c array[]. 
///        Otherwise, return -1 to indicate error. 
///////////////////////////////////////////////////////////////////////////////
static int checkName2(const char* name, const char* kind, const char* array[], int n){
    int i;
    for (i=0; i<n; i++) {
        if (!strcmp(name, array[i])) return i;
    }
    printf("Illegal %s %s\n", kind, name);
    return -1;
}

///////////////////////////////////////////////////////////////////////////////
/// Check enum value of input string \c enu.
///
///\param enu String to be checked.
///\return The enum value of string if it is found in the enum. 
///        Otherwise, return -1 to indicate an error.
///////////////////////////////////////////////////////////////////////////////
static int checkEnumValue2(const char* enu){
    return checkName2(enu, "enum value", enuNames, SIZEOF_ENU);
}
///////////////////////////////////////////////////////////////////////////////
/// Get address of specific function from specific dll
///
///\param fmu Name of dll file.
///\param funnam Function name .
///\return Address of the specific function.
//////////////////////////////////////////////////////////////////////////////
static void* getAdr(FMU *fmu, const char* modelID, const char* functionName){
    char name[BUFSIZE];
    void* fp;
    sprintf(name, "%s_%s", modelID, functionName); 

	#ifdef _MSC_VER
      fp = GetProcAddress(fmu->dllHandle, name);
    #else
	  fp = dlsym(fmu->dllHandle, name);
    #endif
    if (!fp) {
        printf ("Error: Function %s not found in FMI functions library\n", name); 
    }
    return fp;
}

///////////////////////////////////////////////////////////////////////////////
/// Load the given dll and set function pointers in fmu.
/// It changes the names of the standard FMI functions by adding 
/// the model identifier and links the new functions with QTronic's FMU structure.
///
///\param dllPat Path of the dll file.
///\param fmu Name of FMU.
///\return 0 if there is no error occurred.
///////////////////////////////////////////////////////////////////////////////
static int loadLib(const char* libPath, const char* modelID, FMU *fmu) {
	#ifdef _MSC_VER
	  HINSTANCE h;
    #else
	   void *h;
    #endif

    #ifdef _MSC_VER
	  h = LoadLibrary(libPath); 
	  if (h == NULL) {
	  	printf("Error: unable to load FMI functions library\n");
		return -1;
        }
      if (!h) {
		printf("Error: unable to load FMI functions library\n");
		return -1;
      }
    #else
	  h = dlopen(libPath, RTLD_LAZY); 
	  if (h == NULL) {
	  	printf("Error: unable to load  FMI functions library\n");
		return -1;
        }
      if (!h) {
		printf("Error: unable to load  FMI functions library \n");
		return -1;
      }
    #endif

    fmu->dllHandle = h;
    fmu->getVersion              = (fGetVersion)         getAdr(fmu, modelID, "fmiGetVersion");
	if (!(fmu->getVersion)) {
		return -1;
		}
    fmu->instantiateSlave        = (fInstantiateSlave)   getAdr(fmu, modelID, "fmiInstantiateSlave");
	if (!(fmu->instantiateSlave)) {
		return -1;
		}
    fmu->freeSlaveInstance       = (fFreeSlaveInstance)  getAdr(fmu, modelID, "fmiFreeSlaveInstance");
	if (!(fmu->freeSlaveInstance)) {
		return -1;
		}
    fmu->resetSlaveInstance      = (fResetSlaveInstance) getAdr(fmu, modelID, "fmiResetSlave");	
	if (!(fmu->resetSlaveInstance)) {
		return -1;
		}
    fmu->setDebugLogging         = (fSetDebugLogging)    getAdr(fmu, modelID, "fmiSetDebugLogging");
	if (!(fmu->setDebugLogging)) {
		return -1;
		}
    fmu->setReal                 = (fSetReal)            getAdr(fmu, modelID, "fmiSetReal");
	if (!(fmu->setReal)) {
		return -1;
		}
    fmu->setInteger              = (fSetInteger)         getAdr(fmu, modelID, "fmiSetInteger");
	if (!(fmu->setInteger)) {
		return -1;
		}
    fmu->setBoolean              = (fSetBoolean)         getAdr(fmu, modelID, "fmiSetBoolean");
	if (!(fmu->setBoolean)) {
		return -1;
		}
    fmu->setString               = (fSetString)          getAdr(fmu, modelID, "fmiSetString");
	if (!(fmu->setString)) {
		return -1;
		}
    fmu->initializeSlave         = (fInitializeSlave)    getAdr(fmu, modelID, "fmiInitializeSlave");
	if (!(fmu->initializeSlave)) {
		return -1;
		}
	fmu->getReal                 = (fGetReal)            getAdr(fmu, modelID, "fmiGetReal");
	if (!(fmu->getReal)) {
		return -1;
		}
    fmu->getInteger              = (fGetInteger)         getAdr(fmu, modelID, "fmiGetInteger");
	if (!(fmu->getInteger)) {
		return -1;
		}
    fmu->getBoolean              = (fGetBoolean)         getAdr(fmu, modelID, "fmiGetBoolean");
	if (!(fmu->getBoolean)) {
		return -1;
		}
    fmu->getString               = (fGetString)          getAdr(fmu, modelID, "fmiGetString");
	if (!(fmu->getString)) {
		return -1;
		}
    fmu->doStep			         = (fDoStep)             getAdr(fmu, modelID, "fmiDoStep");	
	if (!(fmu->doStep)) {
		return -1;
		}
    return 0; //success
	
}

static const char* fmiStatusToString(fmiStatus status){
    switch (status){
        case fmiOK:      return "ok";
        case fmiWarning: return "warning";
        case fmiDiscard: return "discard";
        case fmiError:   return "error";		
	    case fmiPending: return "pending";	
        default:         return "?";
    }
}

#define MAX_MSG_SIZE 1000
///////////////////////////////////////////////////////////////////////////////
/// Search a fmu for the given variable.
///
///\param fmu FMU.
///\param type Type of FMU variable.
///\param vr FMI value reference.
///\return NULL if not found or vr = fmiUndefinedValueReference
///////////////////////////////////////////////////////////////////////////////
static ScalarVariable* getSV(FMU* fmu, char type, fmiValueReference vr) {
  int i;
  Elm tp;
  ScalarVariable** vars = fmu->modelDescription->modelVariables;
  if (vr==fmiUndefinedValueReference) return NULL;
  switch (type) {
    case 'r': tp = elm_Real;    break;
    case 'i': tp = elm_Integer; break;
    case 'b': tp = elm_Boolean; break;
    case 's': tp = elm_String;  break;                
  }
  for (i=0; vars[i]; i++) {
    ScalarVariable* sv = vars[i];
    if (vr==getValueReference(sv) && tp==sv->typeSpec->type) 
      return sv;
  }
  return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Replace reference information in message.
/// E.g. #r1365# by variable name and ## by # in message
/// copies the result to buffer
///
///\param msg 
///\param buffer
///\param nBuffer
///\param fmu FMU
///////////////////////////////////////////////////////////////////////////////
static void replaceRefsInMessage(const char* msg, char* buffer, int nBuffer, FMU* fmu){
  int i=0; // position in msg
  int k=0; // position in buffer
  int n;
  char c = msg[i];
  while (c!='\0' && k < nBuffer) {
    if (c!='#') {
      buffer[k++]=c;
      i++;
      c = msg[i];
    }
    else {
      char* end = strchr(msg+i+1, '#');
      if (!end) {
        printf("unmatched '#' in '%s'\n", msg);
        buffer[k++]='#';
        break;
      }
      n = end - (msg+i);
      if (n==1) {
        // ## detected, output #
        buffer[k++]='#';
        i += 2;
        c = msg[i];
      }
      else {
        char type = msg[i+1]; // one of ribs
        fmiValueReference vr;
        int nvr = sscanf(msg+i+2, "%u", &vr);
        if (nvr==1) {
          // vr of type detected, e.g. #r12#
          ScalarVariable* sv = getSV(fmu, type, vr);
          const char* name = sv ? getName(sv) : "?";
          sprintf(buffer+k, "%s", name);
          k += strlen(name);
          i += (n+1);
          c = msg[i]; 
        }
        else {
          // could not parse the number
          printf("illegal value reference at position %d in '%s'\n", i+2, msg);
          buffer[k++]='#';
          break;
        }
      }
    }
  } // while
  buffer[k] = '\0';
}

///////////////////////////////////////////////////////////////////////////////
/// FMU logger
///
///\param c FMI component.
///\param instanceName FMI string.
///\param status FMI status.
///\param category FMI string. 
///\param message Message to be recorded.
///////////////////////////////////////////////////////////////////////////////
static void fmuLogger(fmiComponent c, fmiString instanceName, fmiStatus status,
               fmiString category, fmiString message, ...) {
  char msg[MAX_MSG_SIZE];
  char* copy;
  va_list argp;

  // Replace C format strings
	va_start(argp, message);
  vsprintf(msg, message, argp);

  // Replace e.g. ## and #r12#  
  copy = strdup(msg);
  replaceRefsInMessage(copy, msg, MAX_MSG_SIZE, &fmu);
  free(copy);
  
  // Print the final message
  if (!instanceName) instanceName = "?";
  if (!category) category = "?";
  printf("%s %s (%s): %s\n", fmiStatusToString(status), instanceName, category, msg);
}


static int error(const char* message){
    printf("%s\n", message);
    return -1;
}

////////////////////////////////////////////////////////////////
///  This method returns the fmi version number
///\param fmuWorkingFolder path to the FMU-working directory
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID  sizeFMU modelID
///\param fmiVersionNumber FMI version number
////////////////////////////////////////////////////////////////
fmiInteger getfmiEPlusVersion(fmiString fmuWorkingFolder, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID, fmiString fmiVersionNumber){
	
	char *trimfmuWorkingFolder;
	fmiString verID;
	fmiString trimmodelID;

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);
    
	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

        //load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) {
		sprintf(fmiVersionNumber, "%s", "Check FMU binaries folder and see whether libraries "
			"exist for the system architecture of the EnergyPlus version used. Also check whether the FMU has "
			"been exported for Co-Simulation. FMU for Model Exchange is not suported yet");
		return -1;
	}		

	// gets the modelID of the FMU
	verID = fmu.getVersion();
	strncpy(fmiVersionNumber, verID, strlen (verID)+ 1);

	// free trimmed working folder
	free (trimfmuWorkingFolder);
	// free trimmed modelID
	free (trimmodelID);
	return 0;	
}


////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to instantiate an FMU. It returns a pointer to the 
///  instantiate FMU
///
///\param fmuWorkingFolder path to the FMU-working directory
///\param timeOut communication timeout value in milli-seconds 
///\param visible flag to executes the FMU in windowless mode 
///\param interactive flag to execute the FMU in interactive mode
///\param loggingOn flag to enable or disable debug 
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID  sizeFMU modelID
///\param modelGUID FMU modelGUID
///\param sizemodelGUID size FMU modelGUID
////////////////////////////////////////////////////////////////
fmiComponent fmiEPlusInstantiateSlave(fmiString fmuWorkingFolder, 
         fmiReal *timeOut, fmiInteger *visible, 
		 fmiInteger *interactive, fmiInteger *loggingOn, 
		 fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
		 fmiInteger *sizemodelID, fmiString modelGUID, 
		 fmiInteger *sizemodelGUID) {
	
	char* trimfmuWorkingFolder;		 
    fmiString trimmodelID;
	fmiString trimmodelGUID;

	fmiBoolean visiBoolean;
	fmiBoolean interactBoolean;
	fmiBoolean loggOnBoolean;
	fmiComponent fmuInstance; 
    
    // define callbacks functions
    fmiCallbackFunctions callbacks; 
    callbacks.allocateMemory = calloc;
    callbacks.freeMemory = free;
	callbacks.logger = fmuLogger;
	 
	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	// allocate memory for the FMU-modelGUID withouth blanks
	trimmodelGUID = calloc(sizeof(char),*sizemodelGUID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelGUID, modelGUID, *sizemodelGUID);

	//map input to fmiBoolean variables
    if (*visible == 0)
		visiBoolean = fmiFalse;
	else
		visiBoolean = fmiTrue;
	
	if (*interactive == 0)
		interactBoolean = fmiFalse;
	else
		interactBoolean = fmiTrue;

	if (*loggingOn == 0)
		loggOnBoolean = fmiFalse;
	else
		loggOnBoolean = fmiTrue;

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE);

	// instantiate FMU
    fmuInstance = fmu.instantiateSlave(trimmodelID, trimmodelGUID, "", "", *timeOut, visiBoolean, 
		interactBoolean, callbacks, loggOnBoolean); 
    if (!fmuInstance) {
		printf("Error: failed to instantiate slave in fmiEPlusInstantiateSlave!\n");
		return NULL;
	}
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
    // deallocate memory for trimmed modelID
	free(trimmodelID);
	//deallocate memory for trimmed modelGUID
	free(trimmodelGUID);
    return fmuInstance;
}


////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to initialize an FMU. It returns an integer-value 
///  which indicates whether the initialization was successful 
///  or not
///
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance 
///\param tStart simulation starttime
///\param newStep flag to accept or reject timestep
///\param tStop simulation endtime 
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID size FMU modelID
////////////////////////////////////////////////////////////////
fmiStatus fmiEPlusInitializeSlave(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, fmiReal *tStart, 
	fmiInteger *newStep, fmiReal *tStop, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID) {

    char* trimfmuWorkingFolder;
	fmiString trimmodelID;

    fmiStatus fmiFlag;              
	fmiBoolean newStepBoolean;

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);
	
	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE);
	
	//map the newStep to fmiBoolean value
	if (*newStep == 0)
		newStepBoolean = fmiFalse;
	else
		newStepBoolean = fmiTrue;

    //initialize fmu; 
	fmiFlag = fmu.initializeSlave(*fmuInstance, *tStart, newStepBoolean, *tStop);
	if (fmiFlag > fmiWarning) {
      printf("Error: failed to initialize slave in fmiEPlusInitializeSlave!\n");
    return -1;
    }
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory for trimmed modelID
	free(trimmodelID);
	return fmiFlag;
}


////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to get real variables from the FMU. It returns  
///  the output variable from FMU 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance 
///\param valRef value reference
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID size FMU modelID
////////////////////////////////////////////////////////////////

fmiReal fmiEPlusGetReal(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, const fmiValueReference *valRef, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID) {
    
	char *trimfmuWorkingFolder;
	fmiString trimmodelID;

    fmiReal ry;					    
	int k;							
	fmiValueReference vr;
	ScalarVariable** vars; 

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

    //load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder,trimmodelID, &fmu)) exit(EXIT_FAILURE); 
    
	// loading the model variables
	vars = fmu.modelDescription->modelVariables;

	// get real value from fmu
	for (k=0; vars[k]; k++) {	
		 ScalarVariable* sv = vars[k];
		 ScalarVariable* svTemp = vars [k];
		 if (getAlias(sv)!=enu_noAlias) continue;
	     if (getCausality(sv) != enu_output) continue; // only get output variable
		 vr = getValueReference(sv);
		 if (vr == *valRef){	
		   svTemp = vars[k];
		   vr = getValueReference(svTemp);
		   fmu.getReal(*fmuInstance, &vr, 1, &ry);
		 }
	 }
	//// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory for trimmed modelID
	free(trimmodelID);
	return ry;
	
}


////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to get integer variables from the FMU. It returns  
///  the output variable from FMU 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance 
///\param valRef value reference
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID size FMU modelID
////////////////////////////////////////////////////////////////
fmiInteger fmiEPlusGetInteger(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, const fmiValueReference *valRef, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID) {

    char *trimfmuWorkingFolder;
    fmiString trimmodelID;
    
	fmiInteger iy;					// add integer variables output
	int k;							// add a counter for variables
	fmiValueReference vr;			// Zuo: add it to get value reference for variables
	ScalarVariable** vars;

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

    // allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE); 

	// loading the model variables
	vars = fmu.modelDescription->modelVariables;

	//get integer value from fmu
	for (k=0; vars[k]; k++) {
		 ScalarVariable* sv = vars[k];
		 ScalarVariable* svTemp = vars [k];
		 if (getAlias(sv)!=enu_noAlias) continue;
	     if (getCausality(sv) != enu_output) continue; // only get output variable
		 vr = getValueReference(sv);
		 if (vr == *valRef){
		   svTemp = vars[k];
		   vr = getValueReference(svTemp);
		   fmu.getInteger(*fmuInstance, &vr, 1, &iy); 
		 }
	 }
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);

	// deallocate memory for trimmed modelID
	free(trimmodelID);
	return iy;
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to set real variables to the FMU. It returns an  
///  integer which indicates whether input has been set or not 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance 
///\param valRef value reference
///\param inpVal input value
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID size FMU modelID
////////////////////////////////////////////////////////////////
fmiStatus fmiEPlusSetReal(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, const fmiValueReference *valRef, 
	fmiReal *inpVal, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID) {
    
	char *trimfmuWorkingFolder;
	fmiString trimmodelID;

    fmiStatus fmiFlag;              
    fmiReal rx;					
	int k;							
	ScalarVariable** vars; 
	fmiValueReference vr;			

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);
	
	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	// loading the model variables
	vars = fmu.modelDescription->modelVariables;

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE);
	
	//set real value in fmu
	for (k=0; vars[k]; k++) {
		 ScalarVariable* sv = vars[k];
		 ScalarVariable* svTemp = vars [k];
         if (getAlias(sv)!=enu_noAlias) continue;
	     if (getCausality(sv) != enu_input) continue; // only set input variable
		 vr = getValueReference(sv);
		 if (vr == *valRef){
		   svTemp = vars[k];
		   vr = getValueReference(svTemp);
		   rx = *inpVal;
		   fmiFlag = fmu.setReal(*fmuInstance, &vr, 1, &rx); 
		 }
	 }

	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory for trimmed modelID
	free(trimmodelID);
    return fmiFlag;
	
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to set integer variables to the FMU. It returns an  
///  integer which indicates whether input has been set or not 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance 
///\param valRef value reference
///\param inpVal input value
///\param sizefmuWorkingFolder size of FMU-working directory
///\param modelID FMU modelID
///\param sizemodelID  size FMU modelID
////////////////////////////////////////////////////////////////
fmiStatus fmiEPlusSetInteger(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, const fmiValueReference *valRef, 
	fmiInteger *inpVal, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID) {
    
	char *trimfmuWorkingFolder;
	fmiString trimmodelID;

    fmiStatus fmiFlag;               
    fmiInteger ix;
	int k;							

	ScalarVariable** vars;
	fmiValueReference vr;			

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

    // allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE);

	// loading the model variables
	vars = fmu.modelDescription->modelVariables;

	//set integer value in fmu
	for (k=0; vars[k]; k++) {
		 ScalarVariable* sv = vars[k];
		 ScalarVariable* svTemp = vars [k];
		 if (getAlias(sv)!=enu_noAlias) continue;
	     if (getCausality(sv) != enu_input) continue; // only set input variable
		 vr = getValueReference(sv);
		 if (vr == *valRef){
		   svTemp = vars[k];
		   vr = getValueReference(svTemp);
		   ix = *inpVal;
		   fmiFlag = fmu.setInteger(*fmuInstance, &vr, 1, &ix);
		 }
	 }
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory for trimmed modelID
	free(trimmodelID);
    return fmiFlag;
}


////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to get value reference of FMU- input variable by Name.  
///  It returns the value reference of the given variable 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param variableName FMU-variable name 
///\param sizefmuWorkingFolder size of FMU-working directory
///\param sizeVariableName size of FMU-variable name
////////////////////////////////////////////////////////////////
fmiInteger getValueReferenceByNameFMUInputVariables(
	fmiString fmuWorkingFolder, fmiString variableName, 
	fmiInteger *sizefmuWorkingFolder, 
fmiInteger *sizeVariableName) {
    
	char* xmlPath;  
	char *trimfmuWorkingFolder;
	char *trimVariableName;

	ModelDescription* md;                
	fmiInteger valueRef; 

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	// allocate memory for the FMU-variable trimmed
	trimVariableName = calloc(sizeof(char),*sizeVariableName +1 );

    //write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);
	//write FMU-variable withouth blanks
	strncpy(trimVariableName, variableName, *sizeVariableName);

   	// allocate memory for xml-path to FMU, NULL if not found
    xmlPath = calloc(sizeof(char), *sizefmuWorkingFolder + strlen(XML_FILE) + 1);
	
	// write path to the FMU
    sprintf(xmlPath, "%s%s", trimfmuWorkingFolder, XML_FILE);

	// get the model description of the FMU
	md = parse(xmlPath);
	if (!md) {
		printf("Error: failed to get the modelDescription in fmigetValueReferenceByName!\n");
		return -1;
	}

	// check whether the variable exists in the FMU
	if (getVariableByName(md, trimVariableName)== NULL){
		printf("Error: get variable by name failed in fmigetValueReferenceByName. "
			"Please check input variables and modelDescription file again.");
		return -1;
	}
	else
	{
		valueRef = getValueReference(getVariableByName(md, trimVariableName));
		if (!valueRef) {
			printf("Error: could not get value by reference in fmigetValueReferenceByName. "
				"Please check input variables and modelDescription file again");
			return -999;
		}
		if (getCausality(getVariableByName(md, trimVariableName)) != enu_input) {
			printf("Error: This is not an FMU input variable. "
				"Please check input variables and modelDescription file again");
			return -1;
		}
	}
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory FMU-variable name trimmed
	free(trimVariableName);
	// deallocate xmlPath
	free(xmlPath);
    return valueRef;
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to get value reference of FMU- output variable by Name.  
///  It returns the value reference of the given variable 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param variableName FMU-variable name 
///\param sizefmuWorkingFolder size of FMU-working directory
///\param sizeVariableName size of FMU-variable name
////////////////////////////////////////////////////////////////
fmiInteger getValueReferenceByNameFMUOutputVariables(
	fmiString fmuWorkingFolder, fmiString variableName, 
	fmiInteger *sizefmuWorkingFolder, 
	fmiInteger *sizeVariableName) {
    
	char* xmlPath;  
	char *trimfmuWorkingFolder;
	char *trimVariableName;

	ModelDescription* md;                
	fmiInteger valueRef; 

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	// allocate memory for the FMU-variable trimmed
	trimVariableName = calloc(sizeof(char),*sizeVariableName +1 );

    //write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);
	//write FMU-variable withouth blanks
	strncpy(trimVariableName, variableName, *sizeVariableName);

   	// allocate memory for xml-path to FMU, NULL if not found
    xmlPath = calloc(sizeof(char), *sizefmuWorkingFolder + strlen(XML_FILE) + 1);
	
	// write path to the FMU
    sprintf(xmlPath, "%s%s", trimfmuWorkingFolder, XML_FILE);

	// get the model description of the FMU
	md = parse(xmlPath);
	if (!md) {
		printf("Error: failed to get the modelDescription in fmigetValueReferenceByName!\n");
		return -1;
	}

	// check whether the variable exists in the FMU
	if (getVariableByName(md, trimVariableName)== NULL){
		printf("Error: get variable by name failed in fmigetValueReferenceByName. "
			"Please check output variables and modelDescription file again.");
		return -1;
	}
	else
	{
		valueRef = getValueReference(getVariableByName(md, trimVariableName));
		if (!valueRef) {
			printf("Error: could not get value by reference in fmigetValueReferenceByName. "
				"Please check output variables and modelDescription file again");
			return -999;
		}
		if (getCausality(getVariableByName(md, trimVariableName)) != enu_output) {
			printf("Error: This is not an FMU output variable. "
				"Please check output variables and modelDescription file again");
			return -1;
		}
	}
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory FMU-variable name trimmed
	free(trimVariableName);
	// deallocate xmlPath
	free(xmlPath);
    return valueRef;
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to do the step in FMU. It returns an integer  
///  which indicates whether the step was successfully done 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance
///\param curCommPoint current communication intervall 
///\param newStep flag to accept or reject step
///\param commStepSize size communication timestep
///\param sizefmuWorkingFolder size of FMU-working folder
///\param modelID FMU modelID
///\param sizemodelID size FMU modelID
////////////////////////////////////////////////////////////////
fmiStatus fmiEPlusDoStep(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, fmiReal *curCommPoint, 
	fmiReal *commStepSize, fmiInteger *newStep, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID) {
	
	char *trimfmuWorkingFolder;
	fmiString trimmodelID; 

	fmiBoolean newStepBoolean;
    fmiStatus fmiFlag;              

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
    
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// map the newStep to the fmiBoolean value
	if (*newStep == 0)
		newStepBoolean = fmiFalse;
	else
		newStepBoolean = fmiTrue;

	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE); 

	fmiFlag = fmu.doStep(*fmuInstance, *curCommPoint, *commStepSize, newStepBoolean);

	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory for trimmed modelID
	free(trimmodelID);
	return fmiFlag;
}

// This function is an interface to the function in Fortran that is needed to free the FMU.
// It is called at the end of the co-simulaton and should also free the dll used for the
// co-simulation.
////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to free FMU. It returns an integer  
///  which indicates whether the FMU was successfully free 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance
///\param sizefmuWorkingFolder size of FMU-working folder
///\param modelID FMU modelID
///\param sizemodelID  size FMU modelID
////////////////////////////////////////////////////////////////

fmiStatus fmiEPlusFreeSlave(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID) {
	
	char *trimfmuWorkingFolder;
    fmiString trimmodelID;

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE); 

    // free slave in fmu
	fmu.freeSlaveInstance (*fmuInstance);

    // deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);

	// deallocate memory for trimmed modelID
	free(trimmodelID);
    // free modelDescription
	freeElement (fmu.modelDescription);
	return 0;
}


// This function is an interface to the function in Fortran that is needed to reset the FMU
////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to reset FMU. It returns an integer  
///  which indicates whether the FMU was successfully reset 
///  
///\param fmuWorkingFolder path to the FMU-working directory
///\param fmuInstance FMU-instance
///\param sizefmuWorkingFolder size of FMU-working folder
///\param modelID FMU modelID
///\param sizemodelID  sizeFMU modelID
////////////////////////////////////////////////////////////////
fmiStatus fmiEPlusResetSlave(fmiString fmuWorkingFolder, 
	fmiComponent *fmuInstance, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID) {
	
	char *trimfmuWorkingFolder;
    fmiString trimmodelID;
	fmiStatus fmiFlag;
	
	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// allocate memory for the FMU-modelId withouth blanks
	trimmodelID = calloc(sizeof(char),*sizemodelID + 1);
	//write modelID withouth blanks
	strncpy(trimmodelID, modelID, *sizemodelID);

	//load lib by specifying path to the binaries
	if (loadLib(trimfmuWorkingFolder, trimmodelID, &fmu)) exit(EXIT_FAILURE); 

	// reset slave in fmu
	fmiFlag = fmu.resetSlaveInstance (*fmuInstance);
	
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory for trimmed modelID
	free(trimmodelID);
	return fmiFlag;
}


////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to add //fmus/ to the current working folder 
///  of EnergyPlus . It returns an integer  
///  which indicates whether the step was successfully done 
///  
///\param fmuWorkingFolder FMU output working folder
///\param fmuWorkingFolder FMU-working folder
///\param sizefmuWorkingFolder size of FMU-working folder
////////////////////////////////////////////////////////////////

fmiInteger addFMURootFolderName(fmiString fmuOutputWorkingFolder, 
	fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder) {
	char *trimfmuWorkingFolder;
	char * trimfmuWorkingFolderWithRoot;

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
    
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

    trimfmuWorkingFolderWithRoot = calloc(sizeof(char),*sizefmuWorkingFolder + strlen (FMU_ROOT_DIR) + 1);
	sprintf(trimfmuWorkingFolderWithRoot, "%s%s", trimfmuWorkingFolder, FMU_ROOT_DIR);

	//write fmuOutputWorkingFolder 
	strncpy(fmuOutputWorkingFolder, trimfmuWorkingFolderWithRoot, *sizefmuWorkingFolder + strlen (FMU_ROOT_DIR) + 1);
	
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory FMU-working folder with root
	free(trimfmuWorkingFolderWithRoot);
	return 0;	
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to get the path to the binaries of FMu instance 
///  It returns an integer  
///  which indicates whether the routine completes successfully
///  
///\param trimfmuOutputWorkingFolder_wLiB FMU output working folder 
/// with path to binaries
///\param fmuWorkingFolder FMU-working folder
///\param sizefmuWorkingFolder size of FMU-working folder
///\param fmumodelID FMU-modelID
///\param sizefmumodelID size of FMU-modelID
////////////////////////////////////////////////////////////////

fmiInteger addLibPathCurrentWorkingFolder(
	fmiString trimfmuOutputWorkingFolder_wLiB, 
	fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, 
	fmiString fmumodelID, fmiInteger *sizefmumodelID) {
	char *trimfmuWorkingFolder;
	char *trimfmumodelID;
	char * librPath_w32;
	char * librPath_w64;
	struct stat st;
	fmiInteger len_LibPath;
	fmiBoolean bRes_w32;
	fmiBoolean bRes_w64;

	char * librPath_l32;
	char * librPath_l64;
	fmiBoolean bRes_l32;
	fmiBoolean bRes_l64;
	 
	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
    
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

	// allocate memory for the FMU-modelID trimmed
	trimfmumodelID = calloc(sizeof(char),*sizefmumodelID + 1);
    
	//write FMU-modelID withouth blanks
	strncpy(trimfmumodelID, fmumodelID, *sizefmumodelID);

	#ifdef _MSC_VER
		len_LibPath = strlen(trimfmuWorkingFolder) + strlen(BIN_WIN) + strlen (trimfmumodelID) + strlen (LIB_EXT);
		librPath_w32 = calloc(sizeof(char), len_LibPath + 1);
		
		// write the path to the binaries for Windows 32 bit
		sprintf(librPath_w32, "%s%s%s%s", trimfmuWorkingFolder, BIN_WIN32, trimfmumodelID, LIB_EXT); 
		// check whether the dlls for Windows 32 bit exist in the binaries folder 
		bRes_w32 = (stat(librPath_w32, &st) == 0);
	
		// write the path to the binaries for Windows 64 bit
		librPath_w64 = calloc(sizeof(char), len_LibPath + 1);
		sprintf(librPath_w64, "%s%s%s%s", trimfmuWorkingFolder, BIN_WIN64, trimfmumodelID, LIB_EXT); 
		// check whether the dlls for Windows 32 bit exist in the binaries folder 
		bRes_w64 = (stat(librPath_w64, &st) == 0);
		
		// check whether we have folders for Windows 32 and Windows 64 bit
		if (bRes_w32 && bRes_w64) { 
			if (OperSys == 1){ 
				strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_w32, strlen(librPath_w32));
			}
			else
			{
				strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_w64, strlen(librPath_w64));
			}
		}
		// check whether we just have folder for Windows 32 bit
		else if (bRes_w32 && !bRes_w64){
			strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_w32, strlen(librPath_w32));
		}
		// check whether we just have folder for Windows 64 bit
		else if (!bRes_w32 && bRes_w64){
			strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_w64, strlen(librPath_w64));
		}
		else {
			printf("Error: FMU does not contain binaries folder for this operating system.");
			return -1;
		}
	#endif

	#if __linux__ 
		len_LibPath = strlen(trimfmuWorkingFolder) + strlen(BIN_LIN) + strlen (trimfmumodelID) + strlen (LIB_EXT);
		librPath_l32 = calloc(sizeof(char), len_LibPath + 1);
		
		// write the path to the binaries for Windows 32 bit
		sprintf(librPath_l32, "%s%s%s%s", trimfmuWorkingFolder, BIN_LIN32, trimfmumodelID, LIB_EXT); 
		// check whether the so for Linux 32 bit exist in the binaries folder 
		bRes_l32 = (stat(librPath_l32, &st) == 0);
	
		// write the path to the binaries for Windows 64 bit
		librPath_l64 = calloc(sizeof(char), len_LibPath + 1);
		sprintf(librPath_l64, "%s%s%s%s", trimfmuWorkingFolder, BIN_LIN64, trimfmumodelID, LIB_EXT); 
		// check whether the so for Linux 64 bit exist in the binaries folder 
		bRes_l64 = (stat(librPath_l64, &st) == 0);
		
		// check whether we have folders for Linux 32 and Linux 64 bit
		if (bRes_l32 && bRes_l64) { 
			if (OperSys == 3){ 
				strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_l32, strlen(librPath_l32));
			}
			else
			{
				strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_l64, strlen(librPath_l64));
			}
		}
		// check whether we just have folder for Linux 32 bit
		else if (bRes_l32 && !bRes_l64){
			strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_l32, strlen(librPath_l32));
		}
		// check whether we just have folder for Linux 64 bit
		else if (!bRes_l32 && bRes_l64){
			strncpy(trimfmuOutputWorkingFolder_wLiB, librPath_l64, strlen(librPath_l64));
		}
		else {
			printf("Error: FMU does not contain binaries folder for this operating system.");
			return -1;
			}
	#endif
	
	// deallocate memory FMU-working folder trimmed
	free(trimfmuWorkingFolder);
	// deallocate memory FMU-modelID
	free(trimfmumodelID);
	
	if (WINDOWS) {
	  // deallocate memory for path for binaries for windows 32 bit
	  free(librPath_w32);
	  // deallocate memory for path for binaries for windows 64 bit
	  free(librPath_w64);
	}

	#if __linux__ 
	  // deallocate memory for path for binaries for windows 32 bit
	  free(librPath_l32);
	  // deallocate memory for path for binaries for windows 64 bit
	  free(librPath_l64);
	#endif

	return 0;	
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to get the modelID and the modelGUID of the FMU.
///  It returns an integer  
///  which indicates whether the routine was successfully
///  
///\param fmuWorkingFolder FMU output working folder
///\param sizefmuWorkingFolder size of FMU-working folder
///\param modelID FMU-modelID
///\param modelGUID FMU-modelGUID
////////////////////////////////////////////////////////////////

fmiInteger model_ID_GUID(fmiString fmuWorkingFolder, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiString modelGUID) {
	
	char* xmlPath;
	char * trimfmuWorkingFolder;
	fmiString mID;
	fmiString mGUID;

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);
	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);
    
    xmlPath = calloc(sizeof(char), strlen(trimfmuWorkingFolder) + strlen(XML_FILE) + 1);
	// write path to the FMU
    sprintf(xmlPath, "%s%s", trimfmuWorkingFolder, XML_FILE);
    
	// parse the xml-file and getModelDescription
    fmu.modelDescription = parse(xmlPath); 
    // check whether modelDescription exists or not
	if (!fmu.modelDescription) {
		printf("Error: failed to get the modelDescription in fmiGetModelID!\n");
		return -1;
	}
	// gets the modelID of the FMU
	mID = getModelIdentifier(fmu.modelDescription);
	strncpy(modelID, mID, strlen (mID)+ 1);

	if (!modelID) {
		printf("Error: failed to get modelID in fmiGetModelID!\n");
		return -1;
	}

    // get the model GUID of the FMU
	mGUID = getString(fmu.modelDescription, att_guid);
	strncpy(modelGUID, mGUID, strlen (mGUID)+ 1);
	 
	if (!modelGUID) {
		printf("Error: failed to get modelGUID in fmiGetModelGUID!\n");
		return -1;
	}
	// deallocate xmlPath
	free (xmlPath);
	// free trimmed working folder
	free (trimfmuWorkingFolder);
	return 0;	
}


// This function is an interface to the function in Fortran that is used to parse the FMU
////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to unpack FMUs . It returns an integer  
///  which indicates whether the step was successfully done 
///  
///\param fmuName FMU-fileName
///\param fmuOutputWorkingFolder FMU-working folder
///\param sizefmuName size of FMU-Name trimmed
///\param sizefmuOutputWorkingFolder size of FMU-working folder trimmed
////////////////////////////////////////////////////////////////
fmiInteger fmiEPlusUnpack(fmiString fmuName, 
	fmiString fmuOutputWorkingFolder, 
	fmiInteger *sizefmuName, fmiInteger *sizefmuOutputWorkingFolder) {
	
	char * trimfmuOutputWorkingFolder;
	char * trimfmuName;
	int retVal;
 
    // allocate memory for the FMU-Name trimmed
	trimfmuName = calloc(sizeof(char),*sizefmuName + 1);

	// allocate memory for the FMU-working folder trimmed
	trimfmuOutputWorkingFolder = calloc(sizeof(char),*sizefmuOutputWorkingFolder + 1);

	//write fmuName withouth blanks
	strncpy(trimfmuName, fmuName, *sizefmuName);

	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuOutputWorkingFolder, fmuOutputWorkingFolder, *sizefmuOutputWorkingFolder);

	// unpack FMU in the working folder
	retVal = unpackminizip (trimfmuName, trimfmuOutputWorkingFolder);

	if (retVal != 0) {
		printf("Error: failed to unpack FMU in fmiEPlusUnpack!\n");
		return -1;
	}

    // deallocate memory FMU-Name
	free(trimfmuName);
	// deallocate memory FMU-working folder
	free(trimfmuOutputWorkingFolder);
    return 0;	
}

// This function is an interface to the function in Fortran that is used to parse the FMU
////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  needed to unpack FMUs . It returns an integer  
///  which indicates whether the step was successfully done 
///  
///\param fmuOutputWorkingFolder FMU-working folder
///\param sizefmuOutputWorkingFolder size of FMU-working folder trimmed
////////////////////////////////////////////////////////////////
fmiInteger fmiEPlusDelete(fmiString fmuOutputWorkingFolder, 
	fmiInteger *sizefmuOutputWorkingFolder) {
	int retVal;
	char * trimfmuOutputWorkingFolder;

	// allocate memory for the FMU-working folder trimmed
	trimfmuOutputWorkingFolder = calloc(sizeof(char),*sizefmuOutputWorkingFolder + 1);

	//write fmuWorkingFolder withouth blanks
	strncpy(trimfmuOutputWorkingFolder, fmuOutputWorkingFolder, *sizefmuOutputWorkingFolder);

	// delete FMU working folder
	retVal = delete(trimfmuOutputWorkingFolder);
	if (retVal != 0) {
		printf("Error: failed to delete FMU in fmiEPlusDelete!\n");
		return -1;
	}

	// deallocate memory FMU-working folder
	free(trimfmuOutputWorkingFolder);
    return 0;	

}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  It returns the number of input variables in the  
///  corresponding fmu instance. 
///  
///\param fmuWorkingFolder FMU-working folder
///\param sizefmuWorkingFolder size of FMU-working folder
////////////////////////////////////////////////////////////////
fmiInteger getNumInputVariablesInFMU(fmiString fmuWorkingFolder, 
	fmiInteger *sizefmuWorkingFolder){
	int i, j;
	void **list;
	int num_input = 0;

    char* xmlPath;  
	char *trimfmuWorkingFolder;
    ModelDescription* md;                

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);

    //write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

   	// allocate memory for xml-path to FMU, NULL if not found
    xmlPath = calloc(sizeof(char), *sizefmuWorkingFolder + strlen(XML_FILE) + 1);
	
	// write path to the FMU
    sprintf(xmlPath, "%s%s", trimfmuWorkingFolder, XML_FILE);

	// get the model description of the FMU
	md = parse(xmlPath);
	if (!md) {
		printf("Error: failed to get the modelDescription in fmigetValueReferenceByName!\n");
		return -1;
	}

	for(i=0; i<md->n; i+=2)
		if(!strcmp(md->attributes[i], "modelIdentifier"));

	list = (void **)md->modelVariables;
	if (list)
		for(j=0; list[j]; j++)
		{
			Element* e = (Element*)list[j];
			Enu val = enu_none;

			for(i=0; i<e->n; i+=2)
			{
				if(!strcmp(e->attributes[i], "causality"))
					val = checkEnumValue2(e->attributes[i+1]);
			}

           // get number of input variables   
			if(val == enu_input)
			{
				num_input = num_input + 1;
			}
    }
		// deallocate xml Path
		free (xmlPath);
		// deallocate trimfmuWorkingFolder
		free (trimfmuWorkingFolder);
		return num_input;
}

////////////////////////////////////////////////////////////////
///  This method is an interface to the function in Fortran 
///  It returns the number of output variables in the  
///  corresponding fmu instance. 
///  
///\param fmuWorkingFolder FMU-working folder
///\param sizefmuWorkingFolder size of FMU-working folder
////////////////////////////////////////////////////////////////
fmiInteger getNumOutputVariablesInFMU(fmiString fmuWorkingFolder, 
	fmiInteger *sizefmuWorkingFolder){
	int i, j;
	void **list;
	int num_output = 0;

    char* xmlPath;  
	char *trimfmuWorkingFolder;
    ModelDescription* md;                

	// allocate memory for the FMU-working folder trimmed
	trimfmuWorkingFolder = calloc(sizeof(char),*sizefmuWorkingFolder + 1);

    //write fmuWorkingFolder withouth blanks
	strncpy(trimfmuWorkingFolder, fmuWorkingFolder, *sizefmuWorkingFolder);

   	// allocate memory for xml-path to FMU, NULL if not found
    xmlPath = calloc(sizeof(char), *sizefmuWorkingFolder + strlen(XML_FILE) + 1);
	
	// write path to the FMU
    sprintf(xmlPath, "%s%s", trimfmuWorkingFolder, XML_FILE);

	// get the model description of the FMU
	md = parse(xmlPath);
	if (!md) {
		printf("Error: failed to get the modelDescription in fmigetValueReferenceByName!\n");
		return -1;
	}

	for(i=0; i<md->n; i+=2)
		if(!strcmp(md->attributes[i], "modelIdentifier"));

	list = (void **)md->modelVariables;
	if (list)
		for(j=0; list[j]; j++)
		{
			Element* e = (Element*)list[j];
			Enu val = enu_none;

			for(i=0; i<e->n; i+=2)
			{
				if(!strcmp(e->attributes[i], "causality"))
					val = checkEnumValue2(e->attributes[i+1]);
			}

           // get number of input variables   
			if(val == enu_output)
			{
				num_output = num_output + 1;
			}
    }
		// deallocate xml Path
		free (xmlPath);
		// deallocate trimfmuWorkingFolder
		free (trimfmuWorkingFolder);
		return num_output;
}

#else
  fmiComponent fmiEPlusInstantiateSlave(fmiString fmuWorkingFolder, 
         fmiReal *timeOut, fmiInteger *visible, fmiInteger *interactive, 
		 fmiInteger *loggingOn, fmiInteger *sizefmuWorkingFolder, 
		 fmiString modelID, fmiInteger *sizemodelID, fmiString modelGUID, 
		 fmiInteger *sizemodelGUID) {
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiStatus fmiEPlusInitializeSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  fmiReal *tStart, fmiInteger *newStep, fmiReal *tStop, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiReal fmiEPlusGetReal(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiInteger fmiEPlusGetInteger(fmiString fmuWorkingFolder, fmiComponent *fmuInstance,
	  const fmiValueReference *valRef, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiStatus fmiEPlusSetReal(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiReal *inpVal, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiStatus fmiEPlusSetInteger(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiInteger *inpVal, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiStatus fmiEPlusDoStep(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  fmiReal *curCommPoint, fmiReal *commStepSize, fmiInteger *newStep, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiStatus fmiEPlusFreeSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiStatus fmiEPlusResetSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  } 

  fmiInteger fmiEPlusUnpack(fmiString fmuName, fmiString fmuOutputWorkingFolder, 
	  fmiInteger *sizefmuName, fmiInteger *sizefmuOutputWorkingFolder){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  } 

  fmiInteger fmiEPlusDelete(fmiString fmuOutputWorkingFolder, fmiInteger *sizefmuOutputWorkingFolder){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  } 

  fmiInteger getValueReferenceByNameFMUInputVariables(fmiString fmuWorkingFolder, 
	  fmiString variableName, fmiInteger *sizefmuWorkingFolder, fmiInteger *sizeVariableName){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiInteger getValueReferenceByNameFMUOutputVariables(fmiString fmuWorkingFolder, 
	  fmiString variableName, fmiInteger *sizefmuWorkingFolder, fmiInteger *sizeVariableName){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiInteger model_ID_GUID(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiString modelGUID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiInteger addFMURootFolderName(fmiString fmuOutputWorkingFolder, fmiString fmuWorkingFolder, 
	  fmiInteger *sizefmuWorkingFolder){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }
  
  fmiInteger getNumInputVariablesInFMU(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

   fmiInteger getNumOutputVariablesInFMU(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiInteger getfmiEPlusVersion(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID, fmiString fmiVersionNumber){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }

  fmiInteger addLibPathCurrentWorkingFolder(fmiString trimfmuOutputWorkingFolder_wLiB, 
	  fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, fmiString fmumodelID, fmiInteger *sizefmumodelID){
			 printf("Error: FunctionalMock-up Unit for co-simulation is currently only supported on Windows");
			 exit(EXIT_FAILURE);
  }
#endif
