MODULE DataSurfaceColors

          ! Module containing the data dealing with the coloring of surfaces for
          ! various outputs (such as DXF)

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Aug 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Contain the data for surface colors and user settings for DXF and possibly
          ! other surface reporting.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
integer, parameter :: NumColors=15
integer, parameter :: ColorNo_Text = 1
integer, parameter :: ColorNo_Wall = 2
integer, parameter :: ColorNo_Window = 3
integer, parameter :: ColorNo_GlassDoor = 4
integer, parameter :: ColorNo_Door = 5
integer, parameter :: ColorNo_Floor = 6
integer, parameter :: ColorNo_Roof = 7
integer, parameter :: ColorNo_ShdDetBldg = 8
integer, parameter :: ColorNo_ShdDetFix = 9
integer, parameter :: ColorNo_ShdAtt = 10
integer, parameter :: ColorNo_PV = 11
integer, parameter :: ColorNo_TDDDome = 12
integer, parameter :: ColorNo_TDDDiffuser = 13
integer, parameter :: ColorNo_DaylSensor1 = 14
integer, parameter :: ColorNo_DaylSensor2 = 15

integer, parameter, dimension(NumColors) :: defaultcolorno=    &
     (/ 3,   & ! text
       43,   & ! wall
      143,   & ! window
      143,   & ! glassdoor
       45,   & ! door
        8,   & ! floor
       15,   & ! roof
      195,   & ! detached building shade (moves with building)
        9,   & ! detached building fixed
       13,   & ! attached building shading
      174,   & ! PV
      143,   & ! TDD:Dome
      143,   & ! TDD:Diffuser
       10,   & ! Daylight Sensor 1
        5/)    ! Daylight Sensor 2

character(len=*), parameter, dimension(NumColors) :: colorkeys=   &
     (/'Text                     ',    &
       'Walls                    ',    &
       'Windows                  ',    &
       'GlassDoors               ',    &
       'Doors                    ',    &
       'Roofs                    ',    &
       'Floors                   ',    &
       'DetachedBuildingShades   ',    &
       'DetachedFixedShades      ',    &
       'AttachedBuildingShades   ',    &
       'Photovoltaics            ',    &
       'TubularDaylightDomes     ',    &
       'TubularDaylightDiffusers ',    &
       'DaylightReferencePoint1  ', &
       'DaylightReferencePoint2  '/)

integer, parameter, dimension(NumColors) :: colorkeyptr=    &
     (/ColorNo_Text,       &
       ColorNo_Wall,       &
       ColorNo_Window,     &
       ColorNo_GlassDoor,  &
       ColorNo_Door,       &
       ColorNo_Floor,      &
       ColorNo_Roof,       &
       ColorNo_ShdDetBldg, &
       ColorNo_ShdDetFix,  &
       ColorNo_ShdAtt,     &
       ColorNo_PV,         &
       ColorNo_TDDDome,    &
       ColorNo_TDDDiffuser,&
       ColorNo_DaylSensor1,&
       ColorNo_DaylSensor2 /)

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
integer, dimension(NumColors) :: DXFcolorno=defaultcolorno

          ! SUBROUTINE SPECIFICATIONS FOR MODULE:

PUBLIC  MatchAndSetColorTextString
PUBLIC  SetUpSchemeColors

CONTAINS

FUNCTION MatchAndSetColorTextString(String,SetValue,ColorType) RESULT(WasSet)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem ! case insensitive Find

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  character(len=*), intent(in) :: String   ! string to be matched
  integer, intent(in)          :: SetValue ! value to be used for the color
  character(len=*), intent(in), optional :: ColorType  ! for now, must be DXF
  logical :: WasSet

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  integer :: found

  WasSet=.false.
  found=FindItem(String,colorkeys,NumColors)

  if (found /= 0) then
    if (present(colortype)) then
      if (colortype == 'DXF') then
        DXFcolorno(colorkeyptr(found))=SetValue
        WasSet=.true.
      else
      endif
    else
      DXFcolorno(colorkeyptr(found))=SetValue
      WasSet=.true.
    endif
  endif

  RETURN

END FUNCTION MatchAndSetColorTextString

SUBROUTINE SetUpSchemeColors(SchemeName,ColorType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine resets the colorno array(s) with the entered scheme name as
          ! required for reporting.

          ! METHODOLOGY EMPLOYED:
          ! This routine first sets the color arrays to default.  Then, attempts to find the
          ! scheme name in the Input File.  If found, processes that scheme and sets colors.
          ! Worst case: the colors remain as default.  Note -- this allocates and deallocates
          ! the alphas and numerics required to process the Report:SurfaceColorScheme object.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength
  USE DataInterfaces, ONLY: ShowWarningError
  USE InputProcessor, ONLY: GetObjectItemNum, GetObjectItem, GetObjectDefMaxArgs

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SchemeName
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ColorType

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='OutputControl:SurfaceColorScheme'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer :: numAlphas
  integer :: numNumbers
  integer :: numptr
  integer :: numargs
  integer :: status
  character(len=maxnamelength), dimension(:), allocatable :: cAlphas
  character(len=maxnamelength), dimension(:), allocatable :: cAlphafields
  character(len=maxnamelength), dimension(:), allocatable :: cNumericfields
  logical, dimension(:), allocatable :: lAlphablanks
  logical, dimension(:), allocatable :: lNumericblanks
  real(r64), dimension(:), allocatable :: rNumerics

  DXFcolorno=defaultcolorno
! first see if there is a scheme name
  numptr=GetObjectItemNum(CurrentModuleObject,SchemeName)


  if (numptr > 0) then

    CALL GetObjectDefMaxArgs(CurrentModuleObject,numargs,numAlphas,numNumbers)

    allocate(cAlphas(NumAlphas))
    allocate(cAlphafields(NumAlphas))
    allocate(lAlphablanks(NumAlphas))
    allocate(rNumerics(numNumbers))
    allocate(cNumericfields(numNumbers))
    allocate(lNumericBlanks(numNumbers))

    cAlphas(1:numAlphas)=' '
    rNumerics(1:numNumbers)=0.0d0

    CALL GetObjectItem(CurrentModuleObject,numptr,cAlphas,numAlphas,rNumerics,numNumbers,status,  &
                   AlphaBlank=lAlphablanks,NumBlank=lNumericblanks,  &
                   AlphaFieldnames=cAlphafields,NumericFieldNames=cNumericfields)
    do numargs=1,numNumbers
      numptr=rNumerics(numargs)  ! set to integer
      if (lNumericblanks(numargs)) then
        if (.not. lAlphablanks(numargs+1)) then
          CALL ShowWarningError('SetUpSchemeColors: '//TRIM(cAlphafields(1))//'='//TRIM(SchemeName)//  &
             ', '//TRIM(cAlphafields(numargs+1))//'='//TRIM(cAlphas(numargs+1))//  &
            ', '//TRIM(cNumericfields(numargs))//' was blank.  Default color retained.')
        endif
        cycle
      endif
      if (.not. MatchAndSetColorTextString(cAlphas(numargs+1),numptr,ColorType)) then
        CALL ShowWarningError('SetUpSchemeColors: '//TRIM(cAlphafields(1))//'='//TRIM(SchemeName)//  &
           ', '//TRIM(cAlphafields(numargs+1))//'='//TRIM(cAlphas(numargs+1))//  &
           ', is invalid.  No color set.')
      endif
    enddo

    deallocate(cAlphas)
    deallocate(cAlphafields)
    deallocate(lAlphablanks)
    deallocate(rNumerics)
    deallocate(cNumericfields)
    deallocate(lNumericBlanks)

  else
    CALL ShowWarningError('SetUpSchemeColors: Name='//TRIM(SchemeName)//  &
       ' not on input file. Default colors will be used.')
  endif

  RETURN

END SUBROUTINE SetUpSchemeColors


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

END MODULE DataSurfaceColors

