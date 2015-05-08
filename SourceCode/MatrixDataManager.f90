MODULE MatrixDataManager

          ! Module containing the routines dealing with Matrix input objects and services

          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Process user input for Matrix: input data objects
          ! Provide central services for other routines to access
          ! matrix input data.

          ! METHODOLOGY EMPLOYED:
          ! Basic calls to InputProcessor, series of simple get and set routines

          ! REFERENCES:
          ! none

          ! OTHER NOTES:
          ! first implemented for complex fenestration

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, &
                       ShowContinueError

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
!INTEGER, PARAMETER :: OneDimensional = 1
INTEGER, PARAMETER :: TwoDimensional = 2
!INTEGER, PARAMETER :: ThreeDimensional = 3
CHARACTER(len=*), PARAMETER :: Blank = ' '
          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
TYPE MatrixDataStruct
  CHARACTER(len=MaxNameLength)             :: Name  = Blank ! Matrix Name
  INTEGER                                  :: MatrixType   =0 !
  INTEGER, DIMENSION(:), ALLOCATABLE       :: Dim  ! matrix dimensions
  !REAL(r64), DIMENSION(:), ALLOCATABLE     :: Mat1D ! hold data if one dimensional
  REAL(r64), DIMENSION(:,:), ALLOCATABLE   :: Mat2D ! hold data if two dimensional
  !REAL(r64), DIMENSION(:,:,:), Allocatable :: Mat3D ! hold data if three dimensional

END TYPE MatrixDataStruct

TYPE(MatrixDataStruct), ALLOCATABLE, DIMENSION(:) :: MatData

INTEGER   :: NumMats ! number of matracies in input file

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:


! todo, flush out the following routines, see CurveManager for patterns
PRIVATE GetMatrixInput  !read in Matrix:TwoDimensional
PUBLIC MatrixIndex
PUBLIC Get2DMatrix
PUBLIC Get2DMatrixDimensions
!PUBLIC GetMatrixValue
!PUBLIC GetMatrixCheck
!PUBLIC GetMatrixType
!PUBLIC GetMatrixMinMaxValues
!PUBLIC SetMatrixOutputMinMaxValues
!PUBLIC GetMatrixName

CONTAINS

SUBROUTINE GetMatrixInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get input for Matrix objects

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumTwoDimMatrix ! count of Matrix:TwoDimension objects
  INTEGER :: MatIndex  ! do loop counter
  INTEGER :: MatNum    ! index management
  INTEGER :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER :: IOStatus   ! Used in GetObjectItem
  LOGICAL :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL :: IsNotOK              ! Flag to verify name
  LOGICAL :: IsBlank              ! Flag for blank name
  INTEGER :: NumRows
  INTEGER :: NumCols
  INTEGER :: NumElements
  INTEGER :: ElementNum
  INTEGER :: RowIndex
  INTEGER :: ColIndex

  cCurrentModuleObject = 'Matrix:TwoDimension'
  NumTwoDimMatrix = GetNumObjectsFound(cCurrentModuleObject)

  NumMats = NumTwoDimMatrix

  ALLOCATE(MatData(NumMats))

  MatNum = 0
  DO MatIndex=1, NumTwoDimMatrix
    CALL GetObjectItem(cCurrentModuleObject, MatIndex, cAlphaArgs, NumAlphas, rNumericArgs,NumNumbers, IOStatus, &
             NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    MatNum = MatNum + 1
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1), MatData%Name, MatNum - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    MatData(MatNum)%Name = cAlphaArgs(1)
    NumRows = Floor(rNumericArgs(1))
    NumCols = Floor(rNumericArgs(2))
    NumElements = NumRows * NumCols

    ! test
    IF (NumElements < 1) THEN
      CALL ShowSevereError('GetMatrixInput: for '//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Check '//TRIM(cNumericFieldNames(1))//' and '//TRIM(cNumericFieldNames(2))// &
                              ' total number of elements in matrix must be 1 or more')
      ErrorsFound=.true.
    ENDIF
    IF ((NumNumbers - 2) < NumElements) THEN
      CALL ShowSevereError('GetMatrixInput: for '//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Check input, total number of elements does not agree with '&
                              //TRIM(cNumericFieldNames(1))//' and '//TRIM(cNumericFieldNames(2)))
      ErrorsFound=.true.
    ENDIF
    ALLOCATE(MatData(MatNum)%Mat2D(NumRows, NumCols)) ! This is standard order for a NumRows X NumCols matrix
    ALLOCATE(MatData(MatNum)%Dim(2))
    MatData(MatNum)%MatrixType = TwoDimensional
    MatData(MatNum)%Dim(1) = NumRows
    MatData(MatNum)%Dim(2) = NumCols
    RowIndex = 1
    ColIndex = 1
    DO ElementNum=1, NumElements
      RowIndex = (ElementNum -1)/NumCols + 1
      ColIndex = Mod((ElementNum -1), NumCols) + 1
      MatData(MatNum)%Mat2D(RowIndex, ColIndex) = rNumericArgs(ElementNum + 2)  !Matrix is read in row-by-row
                    !Note: this is opposite to usual FORTRAN array storage
    ENDDO

  ENDDO


  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetMatrixInput: Errors found in Matrix objects. Preceding condition(s) cause termination.')
  ENDIF

  RETURN

END SUBROUTINE GetMatrixInput

FUNCTION MatrixIndex(MatrixName) RESULT (MatrixIndexPtr)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Return integer index or pointer to MatData structure array

          ! METHODOLOGY EMPLOYED:
          ! inputs name of matrix and returns integer index
          ! currently uses FindItemInList which is case sensitive

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)  :: MatrixName
  INTEGER :: MatrixIndexPtr !Function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE  :: GetInputFlag = .true.  ! First time, input is "gotten"

  IF (GetInputFlag) THEN
    CALL GetMatrixInput
    GetInputFlag = .FALSE.
  ENDIF

  IF (NumMats > 0) THEN
    MatrixIndexPtr = FindItemInList(MatrixName, MatData(1:NumMats)%Name, NumMats)
  ELSE
    MatrixIndexPtr = 0
  ENDIF

  RETURN

END FUNCTION MatrixIndex

SUBROUTINE Get2DMatrix(Index, Mat2D)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! pass matrix to calling routine

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Index ! pointer index to location in MatData
  REAL(r64), Dimension(:,:), INTENT(OUT) :: Mat2D

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (Index > 0) THEN ! protect hard crash
    Mat2D = MatData(Index)%Mat2D
  ELSE
    ! do nothing (?) throw dev error

  ENDIF

  RETURN

END SUBROUTINE Get2DMatrix

SUBROUTINE Get2DMatrixDimensions(Index, NumRows, NumCols)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Index ! pointer index to location in MatData
  INTEGER, INTENT(OUT) :: NumRows
  INTEGER, INTENT(OUT) :: NumCols
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  IF (Index > 0) Then
    NumRows = MatData(Index)%Dim(1)
    NumCols = MatData(Index)%Dim(2)
  ELSE
    ! do nothing (?) throw dev error?

  ENDIF

  RETURN

END SUBROUTINE Get2DMatrixDimensions


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
END MODULE MatrixDataManager


