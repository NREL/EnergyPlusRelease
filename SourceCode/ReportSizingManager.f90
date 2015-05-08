MODULE ReportSizingManager

          ! Module containing the routines dealing with the <module_name>

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie<author>
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Provide module interface for ReportSizingOutput

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

CONTAINS

SUBROUTINE ReportSizingOutput(CompType,CompName,VarDesc,VarValue,UsrDesc,UsrValue)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Fred Buhl
    !       DATE WRITTEN   Decenber 2001
    !       MODIFIED       August 2008, Greg Stark
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes one item of sizing data to the "eio" file..

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY : OutputFileInits
    USE OutputReportPredefined
    USE General, ONLY: RoundSigDigits
    USE SQLiteProcedures
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: CompType  ! the type of the component
    CHARACTER(len=*), INTENT(IN) :: CompName  ! the name of the component
    CHARACTER(len=*), INTENT(IN) :: VarDesc   ! the description of the input variable
    REAL(r64), INTENT(IN)        :: VarValue  ! the value from the sizing calculation
    CHARACTER(len=*), INTENT(IN),OPTIONAL :: UsrDesc   ! the description of a user-specified variable
    REAL(r64), INTENT(IN), OPTIONAL       :: UsrValue  ! the value from the user for the desc item

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.

    IF (MyOneTimeFlag) THEN
        WRITE(OutputFileInits, 990)
        MyOneTimeFlag = .FALSE.
    END IF

    WRITE (OutputFileInits, 991) TRIM(CompType), TRIM(CompName), TRIM(VarDesc), TRIM(RoundSigDigits(VarValue,5))
    !add to tabular output reports
    CALL AddCompSizeTableEntry(CompType,CompName,VarDesc,VarValue)

    IF (PRESENT(UsrDesc) .and. PRESENT(UsrValue)) THEN
      WRITE (OutputFileInits, 991) TRIM(CompType), TRIM(CompName), TRIM(UsrDesc), TRIM(RoundSigDigits(UsrValue,5))
      CALL AddCompSizeTableEntry(CompType,CompName,UsrDesc,UsrValue)
    ELSEIF (PRESENT(UsrDesc) .or. PRESENT(UsrValue)) THEN
      CALL ShowFatalError('ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both.')
    ENDIF

    ! add to SQL output
    IF (WriteOutputToSQLite) CALL AddSQLiteComponentSizingRecord(CompType, CompName, VarDesc, VarValue)

    990 FORMAT('! <Component Sizing Information>, Component Type, Component Name, ', &
        'Input Field Description, Value')
    991 FORMAT(' Component Sizing Information, ',A,', ',A,', ',A,', ',A)

    RETURN

END SUBROUTINE ReportSizingOutput

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

END MODULE ReportSizingManager

