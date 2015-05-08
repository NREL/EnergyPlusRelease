MODULE GlobalNames

          ! Module containing the routines dealing with matching and assuring that
          ! various component types are unique by name (e.g. Chillers).

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module allows for verification of uniqueness (by name) across
          ! certain component names (esp. Chillers, Boilers)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals
  USE InputProcessor, ONLY: FindItemInList, MakeUPPERCase
  USE DataInterfaces

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE ComponentNameData
    CHARACTER(len=MaxNameLength) :: CompType = ' '  ! Component Type
    CHARACTER(len=MaxNameLength) :: CompName = ' '  ! Component Name (user supplied)
  END TYPE ComponentNameData

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumChillers=0
  INTEGER :: NumBoilers=0
  INTEGER :: NumBaseboards=0
  INTEGER :: NumCoils=0
  INTEGER :: CurMaxChillers=0
  INTEGER :: CurMaxBoilers=0
  INTEGER :: CurMaxBaseboards=0
  INTEGER :: CurMaxCoils=0
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:)  :: ChillerNames
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:)  :: BoilerNames
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:)  :: BaseboardNames
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:)  :: CoilNames

          ! SUBROUTINE SPECIFICATIONS FOR MODULE GlobalNames:
PUBLIC  VerifyUniqueBoilerName
PUBLIC  VerifyUniqueBaseboardName
PUBLIC  VerifyUniqueChillerName
PUBLIC  VerifyUniqueCoilName
CONTAINS

SUBROUTINE VerifyUniqueChillerName(TypeToVerify,NameToVerify,ErrorFound,StringToDisplay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine verifys that a new name will be unique in the list of
          ! chillers.  If not found in the list, it is added before returning.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)               :: TypeToVerify
  CHARACTER(len=*), INTENT(IN)               :: NameToVerify
  LOGICAL, INTENT(OUT)                       :: ErrorFound
  CHARACTER(len=*), INTENT(IN)               :: StringToDisplay

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Found
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:) :: TempChillerNames

  ErrorFound=.false.
  Found=0
  IF (NumChillers > 0)   &
      Found=FindItemInList(NameToVerify,ChillerNames%CompName,NumChillers)
  IF (Found /= 0) THEN
    CALL ShowSevereError(TRIM(StringToDisplay)//', duplicate name='//TRIM(NameToVerify)//  &
              ', Chiller Type="'//TRIM(ChillerNames(Found)%CompType)//'".')
    CALL ShowContinueError('...Current entry is Chiller Type="'//trim(TypeToVerify)//'".')
    ErrorFound=.true.
  ELSEIF (NumChillers == 0) THEN
    CurMaxChillers=4
    ALLOCATE(ChillerNames(CurMaxChillers))
    NumChillers=NumChillers+1
    ChillerNames(NumChillers)%CompType=MakeUPPERCase(TypeToVerify)
    ChillerNames(NumChillers)%CompName=NameToVerify
  ELSEIF (NumChillers == CurMaxChillers) THEN
    ALLOCATE(TempChillerNames(CurMaxChillers+4))
    TempChillerNames(1:CurMaxChillers)=ChillerNames(1:CurMaxChillers)
    DEALLOCATE(ChillerNames)
    CurMaxChillers=CurMaxChillers+4
    ALLOCATE(ChillerNames(CurMaxChillers))
    ChillerNames=TempChillerNames
    DEALLOCATE(TempChillerNames)
    NumChillers=NumChillers+1
    ChillerNames(NumChillers)%CompType=MakeUPPERCase(TypeToVerify)
    ChillerNames(NumChillers)%CompName=NameToVerify
  ELSE
    NumChillers=NumChillers+1
    ChillerNames(NumChillers)%CompType=MakeUPPERCase(TypeToVerify)
    ChillerNames(NumChillers)%CompName=NameToVerify
  ENDIF

  RETURN

END SUBROUTINE VerifyUniqueChillerName

SUBROUTINE VerifyUniqueBaseboardName(TypeToVerify,NameToVerify,ErrorFound,StringToDisplay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine verifys that a new name will be unique in the list of
          ! Baseboards.  If not found in the list, it is added before returning.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)               :: TypeToVerify
  CHARACTER(len=*), INTENT(IN)               :: NameToVerify
  LOGICAL, INTENT(OUT)                       :: ErrorFound
  CHARACTER(len=*), INTENT(IN)               :: StringToDisplay

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Found
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:) :: TempBaseboardNames

  ErrorFound=.false.
  Found = 0

  IF (NumBaseboards > 0)   &
        Found=FindItemInList(NameToVerify,BaseboardNames%CompName,NumBaseboards)

  IF (Found /= 0) THEN
    CALL ShowSevereError(TRIM(StringToDisplay)//', duplicate name='//TRIM(NameToVerify)//  &
              ', Baseboard Type="'//TRIM(BaseboardNames(Found)%CompType)//'".')
    CALL ShowContinueError('...Current entry is Baseboard Type="'//trim(TypeToVerify)//'".')
    ErrorFound=.true.
  ELSEIF (NumBaseboards == 0) THEN
    CurMaxBaseboards=4
    ALLOCATE(BaseboardNames(CurMaxBaseboards))
    NumBaseboards=NumBaseboards+1
    BaseboardNames(NumBaseboards)%CompType=TypeToVerify
    BaseboardNames(NumBaseboards)%CompName=NameToVerify
  ELSEIF (NumBaseboards == CurMaxBaseboards) THEN
    ALLOCATE(TempBaseboardNames(CurMaxBaseboards+4))
    TempBaseboardNames(1:CurMaxBaseboards)=BaseboardNames(1:CurMaxBaseboards)
    DEALLOCATE(BaseboardNames)
    CurMaxBaseboards=CurMaxBaseboards+4
    ALLOCATE(BaseboardNames(CurMaxBaseboards))
    BaseboardNames=TempBaseboardNames
    DEALLOCATE(TempBaseboardNames)
    NumBaseboards=NumBaseboards+1
    BaseboardNames(NumBaseboards)%CompType=TypeToVerify
    BaseboardNames(NumBaseboards)%CompName=NameToVerify
  ELSE
    NumBaseboards=NumBaseboards+1
    BaseboardNames(NumBaseboards)%CompType=TypeToVerify
    BaseboardNames(NumBaseboards)%CompName=NameToVerify
  ENDIF

  RETURN

END SUBROUTINE VerifyUniqueBaseboardName

SUBROUTINE VerifyUniqueBoilerName(TypeToVerify,NameToVerify,ErrorFound,StringToDisplay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine verifys that a new name will be unique in the list of
          ! Boilers.  If not found in the list, it is added before returning.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)               :: TypeToVerify
  CHARACTER(len=*), INTENT(IN)               :: NameToVerify
  LOGICAL, INTENT(OUT)                       :: ErrorFound
  CHARACTER(len=*), INTENT(IN)               :: StringToDisplay

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Found
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:) :: TempBoilerNames

  ErrorFound=.false.
  Found = 0
  IF (NumBoilers > 0)   &
        Found=FindItemInList(NameToVerify,BoilerNames%CompName,NumBoilers)

  IF (Found /= 0) THEN
    CALL ShowSevereError(TRIM(StringToDisplay)//', duplicate name='//TRIM(NameToVerify)//  &
              ', Boiler Type="'//TRIM(BoilerNames(Found)%CompType)//'".')
    CALL ShowContinueError('...Current entry is Boiler Type="'//trim(TypeToVerify)//'".')
    ErrorFound=.true.
  ELSEIF (NumBoilers == 0) THEN
    CurMaxBoilers=4
    ALLOCATE(BoilerNames(CurMaxBoilers))
    NumBoilers=NumBoilers+1
    BoilerNames(NumBoilers)%CompType=TypeToVerify
    BoilerNames(NumBoilers)%CompName=NameToVerify
  ELSEIF (NumBoilers == CurMaxBoilers) THEN
    ALLOCATE(TempBoilerNames(CurMaxBoilers+4))
    TempBoilerNames(1:CurMaxBoilers)=BoilerNames(1:CurMaxBoilers)
    DEALLOCATE(BoilerNames)
    CurMaxBoilers=CurMaxBoilers+4
    ALLOCATE(BoilerNames(CurMaxBoilers))
    BoilerNames=TempBoilerNames
    DEALLOCATE(TempBoilerNames)
    NumBoilers=NumBoilers+1
    BoilerNames(NumBoilers)%CompType=TypeToVerify
    BoilerNames(NumBoilers)%CompName=NameToVerify
  ELSE
    NumBoilers=NumBoilers+1
    BoilerNames(NumBoilers)%CompType=TypeToVerify
    BoilerNames(NumBoilers)%CompName=NameToVerify
  ENDIF

  RETURN

END SUBROUTINE VerifyUniqueBoilerName

SUBROUTINE VerifyUniqueCoilName(TypeToVerify,NameToVerify,ErrorFound,StringToDisplay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine verifys that a new name will be unique in the list of
          ! Coils.  If not found in the list, it is added before returning.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)               :: TypeToVerify
  CHARACTER(len=*), INTENT(IN)               :: NameToVerify
  LOGICAL, INTENT(OUT)                       :: ErrorFound
  CHARACTER(len=*), INTENT(IN)               :: StringToDisplay

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Found
  TYPE (ComponentNameData), ALLOCATABLE, DIMENSION(:) :: TempCoilNames

  ErrorFound=.false.
  Found=0
  IF (NumCoils > 0)   &
      Found=FindItemInList(NameToVerify,CoilNames%CompName,NumCoils)
  IF (Found /= 0) THEN
    CALL ShowSevereError(TRIM(StringToDisplay)//', duplicate name='//TRIM(NameToVerify)//  &
              ', Coil Type="'//TRIM(CoilNames(Found)%CompType)//'"')
    CALL ShowContinueError('...Current entry is Coil Type="'//trim(TypeToVerify)//'".')
    ErrorFound=.true.
  ELSEIF (NumCoils == 0) THEN
    CurMaxCoils=4
    ALLOCATE(CoilNames(CurMaxCoils))
    NumCoils=NumCoils+1
    CoilNames(NumCoils)%CompType=MakeUPPERCase(TypeToVerify)
    CoilNames(NumCoils)%CompName=NameToVerify
  ELSEIF (NumCoils == CurMaxCoils) THEN
    ALLOCATE(TempCoilNames(CurMaxCoils+4))
    TempCoilNames(1:CurMaxCoils)=CoilNames(1:CurMaxCoils)
    DEALLOCATE(CoilNames)
    CurMaxCoils=CurMaxCoils+4
    ALLOCATE(CoilNames(CurMaxCoils))
    CoilNames=TempCoilNames
    DEALLOCATE(TempCoilNames)
    NumCoils=NumCoils+1
    CoilNames(NumCoils)%CompType=MakeUPPERCase(TypeToVerify)
    CoilNames(NumCoils)%CompName=NameToVerify
  ELSE
    NumCoils=NumCoils+1
    CoilNames(NumCoils)%CompType=MakeUPPERCase(TypeToVerify)
    CoilNames(NumCoils)%CompName=NameToVerify
  ENDIF

  RETURN

END SUBROUTINE VerifyUniqueCoilName

!
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

END MODULE GlobalNames

