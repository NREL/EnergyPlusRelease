/* ------------------------------------------------------------------------- 
///////////////////////////////////////////////////////
/// \file   main.h
///
/// \author Thierry Stephane Nouidui,
///         Simulation Research Group, 
///         LBNL,
///         TSNouidui@lbl.gov
///
/// \date   2011-10-11
///
/// This files contains the definition of all 
/// energyplus functions that are needed to mapped 
/// to fmiFunctions

/// This file is based on main.h that is copyrighted by
/// QTronic GmbH and that is distributed under the BSD license. 
/// Function types for all FMU functions and a struct to wrap a FMU dll. 
/// Copyright 2010 QTronic GmbH. All rights reserved. 
/// -------------------------------------------------------------------------
 */

#ifndef main_h
#define main_h

// Use windows.h only for Windows 
#ifdef _MSC_VER
#include <windows.h>
#define WINDOWS 1
#else
#define WINDOWS 0
#define HANDLE void *
/* See http://www.yolinux.com/TUTORIALS/LibraryArchives-StaticAndDynamic.html */
#include <sys/stat.h> // for creating dirs on Linux
#endif

#include "fmiModelFunctions.h"
#include "xml_parser_cosim.h"
#include "eplusModelFunctions.h"

typedef const char* (*fGetTypesPlatform)();	
typedef const char* (*fGetVersion)();
typedef fmiComponent (*fInstantiateSlave)(fmiString instanceName, fmiString GUID,					
											fmiString fmuLocation, fmiString mimeType, fmiReal timeout,		
											fmiBoolean visible, fmiBoolean interactive,					
											fmiCallbackFunctions functions, fmiBoolean loggingOn);		
typedef void      (*fFreeSlaveInstance)  (fmiComponent c);				
typedef fmiStatus (*fResetSlaveInstance)  (fmiComponent c);				
typedef fmiStatus (*fSetDebugLogging)    (fmiComponent c, fmiBoolean loggingOn);
typedef fmiStatus (*fSetReal)   (fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiReal    value[]);
typedef fmiStatus (*fSetInteger)(fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiInteger value[]);
typedef fmiStatus (*fSetBoolean)(fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiBoolean value[]);
typedef fmiStatus (*fSetString) (fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiString  value[]);
typedef fmiStatus (*fInitializeSlave)(fmiComponent c, fmiReal tStart, fmiBoolean StopTimeDefined, fmiReal tStop);	

typedef fmiStatus (*fGetReal)   (fmiComponent c, const fmiValueReference vr[], size_t nvr, fmiReal    value[]);
typedef fmiStatus (*fGetInteger)(fmiComponent c, const fmiValueReference vr[], size_t nvr, fmiInteger value[]);
typedef fmiStatus (*fGetBoolean)(fmiComponent c, const fmiValueReference vr[], size_t nvr, fmiBoolean value[]);
typedef fmiStatus (*fGetString) (fmiComponent c, const fmiValueReference vr[], size_t nvr, fmiString  value[]);

typedef fmiStatus (*fDoStep) (fmiComponent c, fmiReal currentCommunicationPoint, fmiReal communicationStepSize, 
	fmiBoolean newStep); // Zuo: add for co-sim

typedef struct {
    ModelDescription* modelDescription;
    HANDLE dllHandle;	
    fGetTypesPlatform getTypesPlatform;				
    fGetVersion getVersion;			
    fInstantiateSlave instantiateSlave;				
    fFreeSlaveInstance freeSlaveInstance;			
    fResetSlaveInstance resetSlaveInstance;			
    fSetDebugLogging setDebugLogging;
    fSetReal setReal;
    fSetInteger setInteger;
    fSetBoolean setBoolean;
    fSetString setString;
    fInitializeSlave initializeSlave;				
    fGetReal getReal;
    fGetInteger getInteger;
    fGetBoolean getBoolean;
    fGetString getString;
	fDoStep doStep;	

} FMU;

fmiComponent fmiEPlusInstantiateSlave(fmiString fmuWorkingFolder, 
         fmiReal *timeOut, fmiInteger *visible, fmiInteger *interactive, fmiInteger *loggingOn, 
		 fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID, 
		 fmiString modelGUID,	fmiInteger *sizemodelGUID);

fmiStatus fmiEPlusInitializeSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	fmiReal *tStart, fmiInteger *newStep, fmiReal *tStop, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID);

fmiReal fmiEPlusGetReal(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	const fmiValueReference *valRef, fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID);

fmiInteger fmiEPlusGetInteger(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	const fmiValueReference *valRef, fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	fmiInteger *sizemodelID);

fmiStatus fmiEPlusSetReal(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	const fmiValueReference *valRef, fmiReal *inpVal, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID);

fmiStatus fmiEPlusSetInteger(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	const fmiValueReference *valRef, fmiInteger *inpVal, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID);

fmiStatus fmiEPlusDoStep(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	fmiReal *curCommPoint, fmiReal *commStepSize, fmiInteger *newStep, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID);

fmiStatus fmiEPlusFreeSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID);

fmiStatus fmiEPlusResetSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID); 

fmiInteger fmiEPlusUnpack(fmiString fmuName, fmiString fmuOutputWorkingFolder, 
	fmiInteger *sizefmuName, fmiInteger *sizefmuOutputWorkingFolder); 

fmiInteger addLibPathCurrentWorkingFolder(fmiString trimfmuOutputWorkingFolder_wLiB, 
	fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, fmiString fmumodelID, 
	fmiInteger *sizefmumodelID);

fmiInteger fmiEPlusDelete(fmiString fmuOutputWorkingFolder, fmiInteger *sizefmuOutputWorkingFolder); 

fmiInteger getValueReferenceByNameFMUInputVariables(fmiString fmuWorkingFolder, 
	fmiString variableName, fmiInteger *sizefmuWorkingFolder, fmiInteger *sizeVariableName);

fmiInteger getValueReferenceByNameFMUOutputVariables(fmiString fmuWorkingFolder, 
	fmiString variableName, fmiInteger *sizefmuWorkingFolder, fmiInteger *sizeVariableName);

fmiInteger model_ID_GUID(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiString modelGUID);

fmiInteger addFMURootFolderName(fmiString fmuOutputWorkingFolder, fmiString fmuWorkingFolder, 
	fmiInteger *sizefmuWorkingFolder);

fmiInteger getNumInputVariablesInFMU(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder);

fmiInteger getNumOutputVariablesInFMU(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder);

fmiInteger getfmiEPlusVersion(fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, 
	fmiString modelID, fmiInteger *sizemodelID, fmiString fmiVersionNumber);

fmiInteger checkOperatingSystem(fmiString errorMessage);

#endif // main_h

