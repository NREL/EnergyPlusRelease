# This CMake file was specially created to build the Core EnergyPlus tool
# This file is a compilation of the CMake files used to build the release

Project(EnergyPlus)

# configuration

cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)

IF(CMAKE_Fortran_COMPILER MATCHES "gfortran")
	SET(CMAKE_Fortran_FLAGS -ffree-line-length-none)
ENDIF(CMAKE_Fortran_COMPILER MATCHES "gfortran")

# if using intel compilers use all intell
# ie icc icpc and ifort
IF(CMAKE_Fortran_COMPILER MATCHES "ifort")
	SET( CMAKE_EXE_LINKER_FLAGS -static-intel )
	# hack to keep -static-intel from being overrode
	SET( CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS '' )
	SET(CMAKE_Fortran_FLAGS "-fp-model strict" )
ENDIF(CMAKE_Fortran_COMPILER MATCHES "ifort")

SET( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fmodules )

FILE(GLOB ep_src ./SourceCode/*.f90)

SET( sql_src 
	./SourceCode/SQLite/SourceCode/sqlite3.c
	./SourceCode/SQLite/SourceCode/sqlite3.h
	./SourceCode/SQLite/SourceCode/SQLiteCRoutines.c
	./SourceCode/SQLite/SourceCode/SQLiteCRoutines.h
)

SET( USE_SQLIGHT TRUE CACHE BOOL "Use SQLITE?" )
SET( USE_DELIGHT FALSE CACHE BOOL "Use Delight?" )

foreach(f ${ep_src}) 
	IF( ${f}  MATCHES ".*DElightManagerF.f90" )
		IF(NOT USE_DELIGHT)
			LIST( REMOVE_ITEM ep_src ${f} )
		ENDIF(NOT USE_DELIGHT)
	ENDIF( ${f}  MATCHES ".*DElightManagerF.f90" )
	
	IF( ${f}  MATCHES ".*SQLiteFortranRoutines.f90" )
		IF(NOT USE_SQLIGHT)
			LIST( REMOVE_ITEM ep_src ${f} )
		ENDIF(NOT USE_SQLIGHT)
	ENDIF( ${f}  MATCHES ".*SQLiteFortranRoutines.f90" )
	IF( ${f}  MATCHES ".*SQLiteFortranRoutines_NO.f90" )
		IF(USE_SQLIGHT)
			LIST( REMOVE_ITEM ep_src ${f} )
		ENDIF(USE_SQLIGHT)
	ENDIF( ${f}  MATCHES ".*SQLiteFortranRoutines_NO.f90" )

	
	IF( ${f}  MATCHES ".*CVFOnlyRoutines.f90" )
		LIST( REMOVE_ITEM ep_src ${f} )
	ENDIF( ${f}  MATCHES ".*CVFOnlyRoutines.f90" )
endforeach(f)

SOURCE_GROUP( sqlite FILES ${sql_src} )
SOURCE_GROUP( energyplus FILES ${ep_src} )

ADD_EXECUTABLE( energyplus ${ep_src} ${sql_src} )
TARGET_LINK_LIBRARIES( energyplus pthread )


