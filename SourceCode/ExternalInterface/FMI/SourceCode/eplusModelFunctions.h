#ifndef eplusModelFunctions_h
#define eplusModelFunctions_h

///////////////////////////////////////////////////////
/// \file   eplusModelFunctions.h
///
///
/// \author Thierry Stephane Nouidui,
///         Simulation Research Group, 
///         LBNL,
///         TSNouidui@lbl.gov
///
/// \date   2011-11-11
///
/// This header file defines the energyplus functions 
/// that mapped to fmiFunctions and will be exported
/// in .dll
///////////////////////////////////////////////////////

#include "fmiModelTypes.h"
#include <stdlib.h>
/* Export fmi functions on Windows */
#ifdef _MSC_VER
#define DllExport __declspec( dllexport )
#define DllImport __declspec( dllexport )

#else
#define DllExport
#endif

/* make sure all compiler use the same alignment policies for structures */
#ifdef WIN32
#pragma pack(push,8)
#endif

/* Version number */
#define fmiVersion "1.0"

/* reset alignment policy to the one set before reading this file */
#ifdef WIN32
#pragma pack(pop)
#endif

/***************************************************

EnergyPlus Functions for FMI for Co-Simulation

****************************************************/

  DllExport fmiComponent fmiEPlusInstantiateSlave(fmiString fmuWorkingFolder, 
         fmiReal *timeOut, fmiInteger *visible, fmiInteger *interactive, fmiInteger *loggingOn,
		 fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID, fmiString modelGUID,	 
		 fmiInteger *sizemodelGUID);

  DllExport fmiStatus fmiEPlusInitializeSlave(fmiString fmuWorkingFolder, 
	  fmiComponent *fmuInstance, fmiReal *tStart, fmiInteger *newStep, fmiReal *tStop, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID);

  DllExport fmiReal fmiEPlusGetReal(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	  fmiInteger *sizemodelID);

  DllExport fmiInteger fmiEPlusGetInteger(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	  fmiInteger *sizemodelID);

  DllExport fmiStatus fmiEPlusSetReal(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiReal *inpVal, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID);

  DllExport fmiStatus fmiEPlusSetInteger(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  const fmiValueReference *valRef, fmiInteger *inpVal, fmiInteger *sizefmuWorkingFolder, 
	  fmiString modelID, fmiInteger *sizemodelID);

  DllExport fmiStatus fmiEPlusDoStep(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  fmiReal *curCommPoint, fmiReal *commStepSize, fmiInteger *newStep, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID);

  DllExport fmiStatus fmiEPlusFreeSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance,
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID);

  DllExport fmiStatus fmiEPlusResetSlave(fmiString fmuWorkingFolder, fmiComponent *fmuInstance, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiInteger *sizemodelID); 

  DllExport fmiInteger fmiEPlusUnpack(fmiString fmuName, fmiString fmuOutputWorkingFolder, 
	  fmiInteger *sizefmuName, fmiInteger *sizefmuOutputWorkingFolder); 

  DllExport fmiInteger fmiEPlusDelete(fmiString fmuOutputWorkingFolder, 
	  fmiInteger *sizefmuOutputWorkingFolder); 

  DllExport fmiInteger getValueReferenceByNameFMUInputVariables(fmiString fmuWorkingFolder, 
	  fmiString variableName, fmiInteger *sizefmuWorkingFolder, fmiInteger *sizeVariableName);

  DllExport fmiInteger getValueReferenceByNameFMUOutputVariables(fmiString fmuWorkingFolder, 
	  fmiString variableName, fmiInteger *sizefmuWorkingFolder, fmiInteger *sizeVariableName);

  DllExport fmiInteger model_ID_GUID(fmiString fmuWorkingFolder, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, fmiString modelGUID);

  DllExport fmiInteger addFMURootFolderName(fmiString fmuOutputWorkingFolder, 
	  fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder);
  
  DllExport fmiInteger getNumInputVariablesInFMU(fmiString fmuWorkingFolder, 
	  fmiInteger *sizefmuWorkingFolder);

  DllExport fmiInteger getNumOutputVariablesInFMU(fmiString fmuWorkingFolder, 
	  fmiInteger *sizefmuWorkingFolder);

  DllExport fmiInteger getfmiEPlusVersion(fmiString fmuWorkingFolder, 
	  fmiInteger *sizefmuWorkingFolder, fmiString modelID, 
	  fmiInteger *sizemodelID, fmiString fmiVersionNumber);

  DllExport fmiInteger addLibPathCurrentWorkingFolder(fmiString trimfmuOutputWorkingFolder_wLiB, 
	  fmiString fmuWorkingFolder, fmiInteger *sizefmuWorkingFolder, fmiString fmumodelID, fmiInteger *sizefmumodelID);

  DllExport fmiInteger checkOperatingSystem(fmiString errorMessage);


#endif // eplusModelFunctions_h
