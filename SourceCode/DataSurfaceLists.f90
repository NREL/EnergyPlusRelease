MODULE DataSurfaceLists    ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module contains type definitions and variables
          ! associated with Radiant System Surface Groups.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowContinueError, ShowFatalError

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

          ! DERIVED TYPE DEFINITIONS:
  TYPE SurfaceListData
    CHARACTER(len=MaxNameLength)                            :: Name           =' '  ! Name of the surface list
    INTEGER                                                 :: NumOfSurfaces  =0    ! Number of surfaces in the list
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfName             ! Surfaces named in the list
    INTEGER,                      ALLOCATABLE, DIMENSION(:) :: SurfPtr              ! Location of surfaces in Surface derived type
    REAL(r64),                    ALLOCATABLE, DIMENSION(:) :: SurfFlowFrac         ! Fraction of mass flow/length for a surface
  END TYPE SurfaceListData

  TYPE SlabListData
    CHARACTER(len=MaxNameLength)                            :: Name           =' '  ! Name of the surface list
    INTEGER                                                 :: NumOfSurfaces  =0    ! Number of surfaces in the list
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfName             ! Surfaces named in the list
    INTEGER,                      ALLOCATABLE, DIMENSION(:) :: SurfPtr              ! Location of surfaces in Surface derived type
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneName             ! Zone named in the list
    INTEGER,                      ALLOCATABLE, DIMENSION(:) :: ZonePtr              ! Location of Zone in Surface derived type
    REAL(r64),                    ALLOCATABLE, DIMENSION(:) :: CoreDiameter         ! Fraction of mass flow/length for a surface
    REAL(r64),                    ALLOCATABLE, DIMENSION(:) :: CoreLength           ! Fraction of mass flow/length for a surface
    REAL(r64),                    ALLOCATABLE, DIMENSION(:) :: CoreNumbers          ! Fraction of mass flow/length for a surface
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SlabInNodeName       ! Zone named in the list
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SlabOutNodeName      ! Zone named in the list
  END TYPE SlabListData



          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  TYPE(SurfaceListData), ALLOCATABLE, DIMENSION(:) :: SurfList
  TYPE(SlabListData), ALLOCATABLE, DIMENSION(:) :: SlabList

  INTEGER :: NumOfSurfaceLists        =0   ! Number of surface lists in the user input file
  INTEGER :: NumOfSurfListVentSlab    =0   ! Number of surface lists in the user input file
  LOGICAL :: SurfaceListInputsFilled   = .FALSE. ! Set to TRUE after first pass through air loop

!  CHARACTER(len=*), PARAMETER :: CurrentModuleObject = ' '
  ! SUBROUTINE SPECIFICATIONS FOR MODULE DataSurfaceLists
PRIVATE GetSurfaceListsInputs
PUBLIC  GetNumberOfSurfaceLists
PUBLIC  GetNumberOfSurfListVentSlab
CONTAINS

SUBROUTINE GetSurfaceListsInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the surface lists for the Radiant System Surface Groups input.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, GetObjectDefMaxArgs, VerifyName
  USE DataHeatBalance,          ONLY : Zone, Construct
  USE DataGlobals,              ONLY : NumOfZones
  USE General,                  ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject1 = 'ZoneHVAC:LowTemperatureRadiant:SurfaceGroup'
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject2 = 'ZoneHVAC:VentilatedSlab:SlabGroup'
  REAL(r64),        PARAMETER :: FlowFractionTolerance = 0.0001d0 ! Smallest deviation from unity for the sum of all fractions
  REAL(r64),        PARAMETER :: SurfListMinFlowFrac   = 0.001d0  ! Minimum allowed flow fraction (to avoid divide by zero)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  INTEGER                         :: MaxAlphas  ! Maximum number of alphas for these input keywords
  INTEGER                         :: MaxNumbers ! Maximum number of numbers for these input keywords
  INTEGER                         :: NameConflict ! Used to see if a surface name matches the name of a surface list (not allowed)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers    ! Numeric items for object
  INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumArgs    ! Unused variable that is part of a subroutine call
  INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
  REAL(r64)                       :: SumOfAllFractions   ! Summation of all of the fractions for splitting flow (must sum to 1)
  INTEGER                         :: SurfNum    ! DO loop counter for surfaces
  INTEGER                         :: ZoneForSurface  ! Zone number that a particular surface is attached to
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER                         :: Item
  LOGICAL                         :: ErrorsFound
  INTEGER                         :: IOStatus
  INTEGER                         ::  AlphaArray
  INTEGER                         :: NumArray
  INTEGER                         :: SrfList
  LOGICAL                         :: IsNotOK
  LOGICAL                         :: IsBlank

          ! Obtain all of the user data related to surface lists.  Need to get
          ! this before getting the radiant system or ventilated slab data.

  ErrorsFound=.false.
  NumOfSurfaceLists     = GetNumObjectsFound(CurrentModuleObject1)
  NumOfSurfListVentSlab = GetNumObjectsFound(CurrentModuleObject2)

  ALLOCATE(SurfList(NumOfSurfaceLists))
  ALLOCATE(SlabList(NumOfSurfListVentSlab))

  IF (NumOfSurfaceLists > 0) THEN

    CALL GetObjectDefMaxArgs(CurrentModuleObject1,NumArgs,MaxAlphas,MaxNumbers)
    ALLOCATE(Alphas(MaxAlphas))
    Alphas=' '
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.false.
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(Numbers(MaxNumbers))
    Numbers=0.0d0
    ALLOCATE(cNumericFields(MaxNumbers))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(MaxNumbers))
    lNumericBlanks=.false.



    DO Item = 1, NumOfSurfaceLists

      CALL GetObjectItem(CurrentModuleObject1,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(Alphas(1),SurfList%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject1)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) Alphas(1)='xxxxx'
      ENDIF

      SurfList(Item)%Name          = Alphas(1)
      SurfList(Item)%NumOfSurfaces = NumAlphas - 1

      NameConflict = FindItemInList(SurfList(Item)%Name,Surface%Name,TotSurfaces)
      IF (NameConflict > 0) THEN  ! A surface list has the same name as a surface--not allowed
        CALL ShowSevereError(TRIM(CurrentModuleObject1)//' = '//TRIM(SurfList(Item)%Name)//  &
                             ' has the same name as a surface; this is not allowed.')
        ErrorsFound = .TRUE.
      END IF

      IF (SurfList(Item)%NumOfSurfaces < 1) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject1)//' = '//TRIM(SurfList(Item)%Name)//  &
                             ' does not have any surfaces listed.')
        ErrorsFound = .TRUE.
      ELSE
        ALLOCATE(SurfList(Item)%SurfName(SurfList(Item)%NumOfSurfaces))
        ALLOCATE(SurfList(Item)%SurfPtr(SurfList(Item)%NumOfSurfaces))
        ALLOCATE(SurfList(Item)%SurfFlowFrac(SurfList(Item)%NumOfSurfaces))
      END IF

      SumOfAllFractions = 0.0d0
      DO SurfNum = 1, SurfList(Item)%NumOfSurfaces
        SurfList(Item)%SurfName(SurfNum)     = Alphas(SurfNum+1)
        SurfList(Item)%SurfPtr(SurfNum)      = FindIteminList(Alphas(SurfNum+1),Surface%Name,TotSurfaces)
        IF (SurfList(Item)%SurfPtr(SurfNum) == 0) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(SurfNum+1))//' in '//TRIM(CurrentModuleObject1)//' statement not found = '//  &
                               TRIM(SurfList(Item)%SurfName(SurfNum)))
          ErrorsFound = .TRUE.
        ELSE  ! Make sure that all of the surfaces are located in the same zone
          Surface(SurfList(Item)%SurfPtr(SurfNum))%PartOfVentSlabOrRadiantSurface=.true.
          IF (SurfNum == 1) THEN
            ZoneForSurface = Surface(SurfList(Item)%SurfPtr(SurfNum))%Zone
          ENDIF
          IF (SurfNum > 1) THEN
            IF (ZoneForSurface /= Surface(SurfList(Item)%SurfPtr(SurfNum))%Zone) THEN
              CALL ShowSevereError('Not all surfaces in same zone for '//TRIM(CurrentModuleObject1)//' = '//  &
                                   TRIM(SurfList(Item)%Name))
              ErrorsFound = .TRUE.
            END IF
          END IF
        END IF
        SurfList(Item)%SurfFlowFrac(SurfNum) = Numbers(SurfNum)
        IF (SurfList(Item)%SurfFlowFrac(SurfNum) < SurfListMinFlowFrac) THEN
              CALL ShowSevereError('The Flow Fraction for Surface '//TRIM(SurfList(Item)%SurfName(SurfNum))// &
                                   ' in Surface Group '//TRIM(SurfList(Item)%Name)//' is too low' )
              CALL ShowContinueError('Flow fraction of '//TRIM(RoundSigDigits(SurfList(Item)%SurfFlowFrac(SurfNum),6 )) &
                                     //' is less than minimum criteria = '//TRIM(RoundSigDigits(SurfListMinFlowFrac,6)))
              CALL ShowContinueError('Zero or extremely low flow fractions are not allowed. ' &
                                    // 'Remove this surface from the surface group or combine small surfaces together.')
              ErrorsFound = .TRUE.
        END IF
        SumOfAllFractions = SumOfAllFractions + SurfList(Item)%SurfFlowFrac(SurfNum)
      END DO

      IF (ABS(SumOfAllFractions-1.0d0) > FlowFractionTolerance) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject1)//' flow fractions do not add up to unity for '//  &
                             TRIM(SurfList(Item)%Name))
        ErrorsFound = .TRUE.
      END IF

    END DO

    DEALLOCATE(Alphas)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(Numbers)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(lNumericBlanks)

    IF (ErrorsFound) CALL ShowSevereError(trim(CurrentModuleObject1)//' errors found getting input. Program will terminate.')
  END IF

  IF (NumOfSurfListVentSlab > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject2,NumArgs,MaxAlphas,MaxNumbers)
    ALLOCATE(Alphas(MaxAlphas))
    Alphas=' '
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.false.
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(Numbers(MaxNumbers))
    Numbers=0.0d0
    ALLOCATE(cNumericFields(MaxNumbers))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(MaxNumbers))
    lNumericBlanks=.false.

    DO Item = 1, NumOfSurfListVentSlab

      CALL GetObjectItem(CurrentModuleObject2,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(Alphas(1),SlabList%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject2)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) Alphas(1)='xxxxx'
      ENDIF

      SlabList(Item)%Name          = Alphas(1)
      SlabList(Item)%NumOfSurfaces = ((NumAlphas-1)/4)

      NameConflict = FindItemInList(SlabList(Item)%Name,Surface%Name,TotSurfaces)
      IF (NameConflict > 0) THEN  ! A surface list has the same name as a surface--not allowed
        CALL ShowSevereError(TRIM(CurrentModuleObject2)//' = '//TRIM(SlabList(Item)%Name)//  &
                             ' has the same name as a slab; this is not allowed.')
        ErrorsFound = .TRUE.
      END IF

      IF (SlabList(Item)%NumOfSurfaces < 1) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject2)//' = '//TRIM(SlabList(Item)%Name)//  &
                             ' does not have any slabs listed.')
        ErrorsFound = .TRUE.
      ELSE
        ALLOCATE(SlabList(Item)%ZoneName(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%ZonePtr(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%SurfName(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%SurfPtr(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%CoreDiameter(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%CoreLength(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%CoreNumbers(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%SlabInNodeName(SlabList(Item)%NumOfSurfaces))
        ALLOCATE(SlabList(Item)%SlabOutNodeName(SlabList(Item)%NumOfSurfaces))

      END IF

      AlphaArray =2
      NumArray   =1
      DO SurfNum = 1, SlabList(Item)%NumOfSurfaces
        SlabList(Item)%ZoneName(SurfNum)     = Alphas(AlphaArray)
        SlabList(Item)%ZonePtr  = FindIteminList(Alphas(AlphaArray),Zone%Name,NumOfZones)
        IF (SlabList(Item)%ZonePtr(SurfNum) == 0) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(AlphaArray+1))//' in '//TRIM(CurrentModuleObject2)//' Zone not found = '//  &
                               TRIM(SlabList(Item)%SurfName(SurfNum)))
          ErrorsFound = .TRUE.
        END IF

        SlabList(Item)%SurfName(SurfNum)     = Alphas(AlphaArray+1)
        SlabList(Item)%SurfPtr(SurfNum)      = FindIteminList(Alphas(AlphaArray+1),Surface%Name,TotSurfaces)
        IF (SlabList(Item)%SurfPtr(SurfNum) == 0) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(AlphaArray+1))//' in '//TRIM(CurrentModuleObject2)//' statement not found = '//  &
                               TRIM(SlabList(Item)%SurfName(SurfNum)))
          ErrorsFound = .TRUE.

        END IF
        DO SrfList=1,NumOfSurfaceLists
          NameConflict = FindItemInList(SlabList(Item)%SurfName(SurfNum),SurfList(SrfList)%SurfName,SurfList(SrfList)%NumOfSurfaces)
          IF (NameConflict > 0) THEN  ! A slab list includes a surface on a surface list--not allowed
            CALL ShowSevereError(TRIM(CurrentModuleObject2)//'="'//TRIM(SlabList(Item)%Name)//  &
                                 '", invalid surface specified.')
            CALL ShowContinueError('Surface="'//trim(SlabList(Item)%SurfName(SurfNum))//  &
               '" is also on a Surface List.')
            CALL ShowContinueError(trim(CurrentModuleObject1)//'="'//trim(SurfList(SrfList)%Name)//'" has this surface also.')
            CALL ShowContinueError('A surface cannot be on both lists. The models cannot operate correctly.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDDO
        Surface(SlabList(Item)%SurfPtr(SurfNum))%PartOfVentSlabOrRadiantSurface=.true.

        SlabList(Item)%CoreDiameter(SurfNum)     = Numbers(NumArray)
        SlabList(Item)%CoreLength(SurfNum)       = Numbers(NumArray+1)
        SlabList(Item)%CoreNumbers(SurfNum)      = Numbers(NumArray+2)
        SlabList(Item)%SlabInNodeName(SurfNum)   = Alphas(AlphaArray+2)
        SlabList(Item)%SlabOutNodeName(SurfNum)  = Alphas(AlphaArray+3)
        AlphaArray=2*(SurfNum+1)+2*((SurfNum+1)-1)
        NumArray = 2* SurfNum+(SurfNum+1)
      END DO
    END DO

    DEALLOCATE(Alphas)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(Numbers)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(lNumericBlanks)

    IF (ErrorsFound) CALL ShowSevereError(trim(CurrentModuleObject2)//' errors found getting input. Program will terminate.')

  END IF

  IF (ErrorsFound) CALL ShowFatalError('GetSurfaceListsInputs: Program terminates due to preceding conditions.')

RETURN

END SUBROUTINE GetSurfaceListsInputs

FUNCTION GetNumberOfSurfaceLists() RESULT(NumberOfSurfaceLists)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Acts as a target for outside routines to make sure data is gotten before using.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: NumberOfSurfaceLists

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (.not. SurfaceListInputsFilled) THEN
    CALL GetSurfaceListsInputs
    SurfaceListInputsFilled=.true.
  ENDIF

  NumberOfSurfaceLists=NumOfSurfaceLists
  RETURN

END FUNCTION GetNumberOfSurfaceLists


FUNCTION GetNumberOfSurfListVentSlab() RESULT(NumberOfSurfListVentSlab)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Acts as a target for outside routines to make sure data is gotten before using.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: NumberOfSurfListVentSlab

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (.not. SurfaceListInputsFilled) THEN
    CALL GetSurfaceListsInputs
    SurfaceListInputsFilled=.true.
  ENDIF

  NumberOfSurfListVentSlab=NumOfSurfListVentSlab

  RETURN

END FUNCTION GetNumberOfSurfListVentSlab


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

END MODULE DataSurfaceLists
