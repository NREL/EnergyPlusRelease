MODULE DataStringGlobals      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for string variables used in parsing
          ! "pieces" of EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! None!--This module is USEd by other modules; it should not USE anything.

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          CHARACTER(len=*), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖØÙÚÛÜİ'
          CHARACTER(len=*), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïğñòóôõöøùúûüı'
          CHARACTER(len=*), PARAMETER :: AccentedUpperCase='ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖØÙÚÛÜİ'
          CHARACTER(len=*), PARAMETER :: AccentedLowerCase='àáâãäåæçèéêëìíîïğñòóôõöøùúûüı'
          CHARACTER(len=*), PARAMETER :: AllCase=&
       'àáâãäåæçèéêëìíîïğñòóôõöøùúûüıÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖØÙÚÛÜİABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
#ifdef WINDOWS
          CHARACTER(len=1),  PARAMETER :: pathChar='\'
          CHARACTER(len=1),  PARAMETER :: altpathChar='/'
#elif defined LINUX
          CHARACTER(len=1),  PARAMETER :: pathChar='/'
          CHARACTER(len=1),  PARAMETER :: altpathChar='\'
#elif defined MAC
          CHARACTER(len=1),  PARAMETER :: pathChar='/'
          CHARACTER(len=1),  PARAMETER :: altpathChar='\'
#else
          CHARACTER(len=1),  PARAMETER :: pathChar='\'  ! default to Windows
          CHARACTER(len=1),  PARAMETER :: altpathChar='/'
#endif
          INTEGER,           PARAMETER :: PathLimit=255
          CHARACTER(len=1),  PARAMETER :: CharComma=CHAR(44) !comma
          CHARACTER(len=1),  PARAMETER :: CharSemicolon=CHAR(59) !comma
          CHARACTER(len=1),  PARAMETER :: CharTab=CHAR(9)  !tab
          CHARACTER(len=1),  PARAMETER :: CharSpace=CHAR(32) !space

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          CHARACTER(len=PathLimit)    :: ProgramPath=' '           ! Path for Program from Energy+.ini
          CHARACTER(len=PathLimit)    :: CurrentWorkingFolder=' '  ! Current working directory for run
          CHARACTER(len=PathLimit+15) :: FullName=' '              ! Full name of file to open, including path
          CHARACTER(len=120) :: IDDVerString=' '                   ! Version information from the IDD (line 1)
          CHARACTER(len=120) :: VerString='EnergyPlus, Version 8.1'      ! String that represents version information
          CHARACTER(len=5)   :: MatchVersion='8.1.0' ! String to be matched by Version object
          CHARACTER(len=40)  :: CurrentDateTime=' '       ! For printing current date and time at start of run


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

END MODULE DataStringGlobals
