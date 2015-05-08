MODULE ExternalInterface

          ! Module containing the routines dealing with the BCVTB interface

          ! MODULE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   5Jan2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Do nothing except show fatal error when an instance is made of an ExternalInterface
          ! object.
          ! This module replaces ExternalInterface.f90 when building EnergyPlus without the bcvtb LIB and DLL.
          ! This should only be done during development. The official EnergyPlus release needs to be
          ! compiled with ExternalInterface.f90, and linked to a dummy bcvtb LIB and DLL.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! http://simulationresearch.lbl.gov/bcvtb

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
LOGICAL, PUBLIC :: haveExternalInterfaceBCVTB = .false.

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ExternalInterface:
  PUBLIC  ExternalInterfaceExchangeVariables
  INTEGER, PUBLIC                         :: NumExternalInterfaces = 0        ! Number of ExternalInterface objects
  PUBLIC  CloseSocket

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ExternalInterfaceExchangeVariables

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   5Jan2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Exchanges variables between EnergyPlus and the BCVTB socket.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE InputProcessor, ONLY: GetNumObjectsFound
USE DataInterfaces, ONLY: ShowFatalError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: GetInputFlag = .true.       ! First time, input is "gotten"
  INTEGER      :: NumExternalInterfaces = 0   ! Number of ExternalInterface objects

  IF (GetInputFlag) THEN
    NumExternalInterfaces = GetNumObjectsFound('ExternalInterface')
    GetInputFlag=.false.
    IF ( NumExternalInterfaces > 0 ) THEN
           CALL ShowFatalError('ExternalInterface is not available in this version.')
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE ExternalInterfaceExchangeVariables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE CloseSocket(FlagToWriteToSocket)
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Michael Wetter
  !       DATE WRITTEN   5Jan2010
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine does nothing, but it is needed since EnergyPlus
  ! may call CloseSocket when it terminates.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! USE STATEMENTS:
  ! na

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: FlagToWriteToSocket  ! flag to write to the socket

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  RETURN

END SUBROUTINE CloseSocket
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

END MODULE ExternalInterface

