MODULE DELIGHTMANAGERF

    ! MODULE INFORMATION
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   August 2003
    !       MODIFIED       January 2004
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS MODULE:

    ! Do nothing except show fatal error when calls are made to DElight subroutines.
    ! Fatal errors will only be generated when a user include DElight objects in the input IDF.
    ! This module replaces DElightManagerF.f90 when building EnergyPlus without the DElight LIB and DLL.

    ! METHODOLOGY EMPLOYED:

    !
    ! REFERENCES:


    ! OTHER NOTES:



    ! USE STATEMENTS:
    ! <use statements for data only modules>
    USE DataPrecisionGlobals
    USE DataDElight

    IMPLICIT NONE         ! Enforce explicit typing of all variables

    PUBLIC GenerateDElightDaylightCoefficients
    PUBLIC DElightDaylightCoefficients
    PUBLIC DElightElecLtgCtrl
    PUBLIC DElightFreeMemory
    PUBLIC DElightOutputGenerator
    PUBLIC DElightInputGenerator

    ! MODULE VARIABLE DECLARATIONS:

    CONTAINS

    ! SUBROUTINE SPECIFICATIONS FOR MODULE DElightManagerF
    ! MODULE SUBROUTINES:

SUBROUTINE GenerateDElightDaylightCoefficients (dBldgLat, iErrorFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to provide an envelop to the DElightDaylightCoefficients
          ! routine (and remove IEEE calls from main EnergyPlus core routines).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowFatalError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) :: dBldgLat
  INTEGER iErrorFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    CALL ShowFatalError('DElight is not available in this version')

  RETURN

END SUBROUTINE GenerateDElightDaylightCoefficients

SUBROUTINE  DElightDaylightCoefficients (dBldgLat, iErrorFlag)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   January 2004
    !       MODIFIED       February 2004 - Remove ProgramPath StringLength arguments
    !                       RJH - Jul 2004 - Add error flag parameter to match new DElight call
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine does nothing but generate an error message when calls are made.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64) :: dBldgLat
    INTEGER iErrorFlag

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    CALL ShowFatalError('DElight is not available in this version')

    return

END SUBROUTINE DElightDaylightCoefficients

SUBROUTINE DElightElecLtgCtrl (iNameLength, cZoneName, dBldgLat, &
                            dHISKF, dHISUNF, dCloudFraction, dSOLCOSX, dSOLCOSY, dSOLCOSZ, &
                            pdPowerReducFac, iErrorFlag)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   January 2004
    !       MODIFIED       RJH - Jul 2004 - Add error flag parameter to match new DElight call
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine does nothing but generate an error message when calls are made.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER iNameLength
    CHARACTER(len=*) cZoneName
!    REAL(r64) rBldgLat
    REAL(r64) :: dBldgLat
    REAL(r64) :: dHISKF
    REAL(r64) :: dHISUNF
    REAL(r64) :: dCloudFraction
    REAL(r64) :: dSOLCOSX
    REAL(r64) :: dSOLCOSY
    REAL(r64) :: dSOLCOSZ
    REAL(r64) :: pdPowerReducFac
    INTEGER iErrorFlag

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    CALL ShowFatalError('DElight is not available in this version')

    return

END SUBROUTINE DElightElecLtgCtrl

SUBROUTINE DElightFreeMemory ()

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   January 2004
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine does nothing but generate an error message when calls are made.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    return

END SUBROUTINE DElightFreeMemory

SUBROUTINE DElightOutputGenerator (iOutputFlag)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   January 2004
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine does nothing but generate an error message when calls are made.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER iOutputFlag

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    return

END SUBROUTINE DElightOutputGenerator

SUBROUTINE DElightInputGenerator

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   January 2004
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine does nothing but generate an error message when calls are made.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    CALL ShowFatalError('DElight is not available in this version')

    return

END SUBROUTINE DElightInputGenerator

SUBROUTINE SetupDElightOutput4EPlus

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   February 2004
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine does nothing but generate an error message when calls are made.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    CALL ShowFatalError('DElight is not available in this version')

    return

END SUBROUTINE SetupDElightOutput4EPlus

FUNCTION ReplaceBlanksWithUnderscores(InputString) RESULT (ResultString)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Robert J. Hitchcock
    !       DATE WRITTEN   August 2003
    !       MODIFIED       From MakeUPPERCase function by Linda K. Lawrie
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:

    ! METHODOLOGY EMPLOYED:

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataGlobals

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


    ! FUNCTION ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: InputString     ! Input String
    CHARACTER(len=MaxNameLength) ResultString       ! Result String

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ResultString=TRIM(InputString)

    RETURN

END FUNCTION ReplaceBlanksWithUnderscores

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

END MODULE DELIGHTMANAGERF





