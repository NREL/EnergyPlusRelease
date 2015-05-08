MODULE DataOutputs

          ! Module containing the data and routines dealing with prescanning for
          ! requested output variables to limit the number being processed in OutputProcessor
          ! Also any input counts (such as autosize counts/records that are used
          ! by later program modules.

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2010
          !       MODIFIED       April 2011; to include autosize counts
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! The module contains structure for output variables that are used in a small number of modules.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Data Only modules are Public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: NumMonthlyReports=62
CHARACTER(len=*), PARAMETER, DIMENSION(NumMonthlyReports) :: MonthlyNamedReports=  &
    (/'ZONECOOLINGSUMMARYMONTHLY                          ',   &
      'ZONEHEATINGSUMMARYMONTHLY                          ',   &
      'ZONEELECTRICSUMMARYMONTHLY                         ',   &
      'SPACEGAINSMONTHLY                                  ',   &
      'PEAKSPACEGAINSMONTHLY                              ',   &
      'SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY            ',   &
      'ENERGYCONSUMPTIONELECTRICITYNATURALGASMONTHLY      ',   &
      'ENERGYCONSUMPTIONELECTRICITYGENERATEDPROPANEMONTHLY',   &
      'ENERGYCONSUMPTIONDIESELFUELOILMONTHLY              ',   &
      'ENERGYCONSUMPTIONDISTRICTHEATINGCOOLINGMONTHLY     ',   &
      'ENERGYCONSUMPTIONCOALGASOLINEMONTHLY               ',   &
      'ENERGYCONSUMPTIONOTHERFUELSMONTHLY                 ',   &
      'ENDUSEENERGYCONSUMPTIONELECTRICITYMONTHLY          ',   &
      'ENDUSEENERGYCONSUMPTIONNATURALGASMONTHLY           ',   &
      'ENDUSEENERGYCONSUMPTIONDIESELMONTHLY               ',   &
      'ENDUSEENERGYCONSUMPTIONFUELOILMONTHLY              ',   &
      'ENDUSEENERGYCONSUMPTIONCOALMONTHLY                 ',   &
      'ENDUSEENERGYCONSUMPTIONPROPANEMONTHLY              ',   &
      'ENDUSEENERGYCONSUMPTIONGASOLINEMONTHLY             ',   &
      'ENDUSEENERGYCONSUMPTIONOTHERFUELSMONTHLY           ',   &
      'PEAKENERGYENDUSEELECTRICITYPART1MONTHLY            ',   &
      'PEAKENERGYENDUSEELECTRICITYPART2MONTHLY            ',   &
      'ELECTRICCOMPONENTSOFPEAKDEMANDMONTHLY              ',   &
      'PEAKENERGYENDUSENATURALGASMONTHLY                  ',   &
      'PEAKENERGYENDUSEDIESELMONTHLY                      ',   &
      'PEAKENERGYENDUSEFUELOILMONTHLY                     ',   &
      'PEAKENERGYENDUSECOALMONTHLY                        ',   &
      'PEAKENERGYENDUSEPROPANEMONTHLY                     ',   &
      'PEAKENERGYENDUSEGASOLINEMONTHLY                    ',   &
      'PEAKENERGYENDUSEOTHERFUELSMONTHLY                  ',   &
      'SETPOINTSNOTMETWITHTEMPERATURESMONTHLY             ',   &
      'COMFORTREPORTSIMPLE55MONTHLY                       ',   &
      'UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY     ',   &
      'OCCUPANTCOMFORTDATASUMMARYMONTHLY                  ',   &
      'CHILLERREPORTMONTHLY                               ',   &
      'TOWERREPORTMONTHLY                                 ',   &
      'BOILERREPORTMONTHLY                                ',   &
      'DXREPORTMONTHLY                                    ',   &
      'WINDOWREPORTMONTHLY                                ',   &
      'WINDOWENERGYREPORTMONTHLY                          ',   &
      'WINDOWZONESUMMARYMONTHLY                           ',   &
      'WINDOWENERGYZONESUMMARYMONTHLY                     ',   &
      'AVERAGEOUTDOORCONDITIONSMONTHLY                    ',   &
      'OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY             ',   &
      'OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY             ',   &
      'OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY             ',   &
      'OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY            ',   &
      'OUTDOORGROUNDCONDITIONSMONTHLY                     ',   &
      'WINDOWACREPORTMONTHLY                              ',   &
      'WATERHEATERREPORTMONTHLY                           ',   &
      'GENERATORREPORTMONTHLY                             ',   &
      'DAYLIGHTINGREPORTMONTHLY                           ',   &
      'COILREPORTMONTHLY                                  ',   &
      'PLANTLOOPDEMANDREPORTMONTHLY                       ',   &
      'FANREPORTMONTHLY                                   ',   &
      'PUMPREPORTMONTHLY                                  ',   &
      'CONDLOOPDEMANDREPORTMONTHLY                        ',   &
      'ZONETEMPERATUREOSCILLATIONREPORTMONTHLY            ',   &
      'AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY              ',   &
      'AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY                 ',   &
      'AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY             ',   &
      'MECHANICALVENTILATIONLOADSMONTHLY                  '/)


          ! DERIVED TYPE DEFINITIONS:
TYPE OutputReportingVariables    ! Linked list of variables and keys
  CHARACTER(len=MaxNameLength) :: Key     =' '  ! could be a key or "*"  (upper case)
  CHARACTER(len=MaxNameLength) :: VarName =' '  ! variable name (upper case)
  INTEGER                      :: Previous=0    ! Pointer to Previous of same variable name
  INTEGER                      :: Next    =0    ! Pointer to Next of same variable name
END TYPE

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: MaxConsideredOutputVariables=0       ! Max Array size for OutputVariable pre-scanned
INTEGER :: NumConsideredOutputVariables=0       ! Number of variables - pre-scanned, allowed for output
TYPE (OutputReportingVariables), ALLOCATABLE, DIMENSION(:) :: OutputVariablesForSimulation
TYPE (OutputReportingVariables), ALLOCATABLE, DIMENSION(:) :: TempOutputVariablesForSimulation
INTEGER :: iNumberOfRecords                     ! Number of records in input
INTEGER :: iNumberOfDefaultedFields             ! number of defaulted fields
INTEGER :: iTotalFieldsWithDefaults             ! number of fields that can be defaulted
INTEGER :: iNumberOfAutosizedFields             ! number of autosized fields
INTEGER :: iTotalAutoSizableFields              ! number of fields that can be autosized
INTEGER :: iNumberOfAutoCalcedFields            ! number of autocalculated fields
INTEGER :: iTotalAutoCalculatableFields         ! number of fields that can be autocalculated

PRIVATE DOSameString
PRIVATE DOMakeUPPERCase

CONTAINS

FUNCTION FindItemInVariableList(KeyedValue,VariableName) RESULT(InVariableList)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up a key and variable name value and determines if they are
          ! in the list of required variables for a simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: KeyedValue
  CHARACTER(len=*), INTENT(IN) :: VariableName
  LOGICAL                      :: InVariableList


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found
  INTEGER :: Item

  InVariableList=.false.
  Found=0
  DO Item=1,NumConsideredOutputVariables
    IF (.not. DOSameString(VariableName,OutputVariablesForSimulation(Item)%VarName)) CYCLE
    Found=Item
    EXIT
  ENDDO
  IF (Found /= 0) THEN
    IF (DOSameString(KeyedValue,OutputVariablesForSimulation(Found)%Key) .or.  &
        OutputVariablesForSimulation(Found)%Key == '*')   THEN
      InVariableList=.true.
    ELSE
      DO WHILE (Found /= 0)
        Found=OutputVariablesForSimulation(Found)%Next
        IF (Found /= 0) THEN
          IF (DOSameString(KeyedValue,OutputVariablesForSimulation(Found)%Key) .or.  &
              OutputVariablesForSimulation(Found)%Key == '*')   THEN
            InVariableList=.true.
            EXIT
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  RETURN

END FUNCTION FindItemInVariableList


LOGICAL FUNCTION DOSameString(TestString1,TestString2)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function returns true if the two strings are equal (case insensitively)

          ! METHODOLOGY EMPLOYED:
          ! Make both strings uppercase.  Do internal compare.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
   CHARACTER(len=*), INTENT(IN) :: TestString1  ! First String to Test
   CHARACTER(len=*), INTENT(IN) :: TestString2  ! Second String to Test


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (LEN_TRIM(TestString1) /= LEN_TRIM(TestString2)) THEN
    DOSameString=.false.
  ELSEIF (TestString1 == TestString2) THEN
    DOSameString=.true.
  ELSE
    DOSameString=DOMakeUPPERCase(TestString1) == DOMakeUPPERCase(TestString2)
  ENDIF

  RETURN

END FUNCTION DOSameString

FUNCTION DOMakeUPPERCase(InputString) RESULT (ResultString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function returns the Upper Case representation of the InputString.

          ! METHODOLOGY EMPLOYED:
          ! Uses the Intrinsic SCAN function to scan the lowercase representation of
          ! characters (DataStringGlobals) for each character in the given string.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
   CHARACTER(len=*), INTENT(IN) :: InputString    ! Input String
   CHARACTER(len=len(InputString)) ResultString ! Result String, string is limited to
                                                  ! MaxInputLineLength because of PowerStation Compiler
                                                  ! otherwise could say (CHARACTER(len=LEN(InputString))


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER i                  ! Loop Counter
  INTEGER :: CurCharVal

    ResultString=InputString

    do i = 1, LEN_TRIM(InputString)
      curCharVal = ICHAR(InputString(i:i))
      SELECT CASE (curCharVal)
        CASE (97:122,224:255) !lowercase ASCII and accented characters
          ResultString(i:i) = CHAR(curCharVal-32)
        CASE DEFAULT
      END SELECT
    end do
!       ! first check for normal lowercase char, then normal uppercase char
!       if(InputString(i:i) >= "a" .and. InputString(i:i) <= "z") then
!          ResultString(i:i) = achar(iachar(InputString(i:i)) - 32)
!       else if (InputString(i:i) >= "A" .and. InputString(i:i) <= "Z") then
!          cycle !ResultString(i:i) = InputString(i:i)  ! leave as is
!       else ! now see if it's an accented char that needs uppercaseing
!          Pos=SCAN(AccentedLowerCase,InputString(i:i))
!          if (Pos /= 0) THEN
!             ResultString(i:i)=AccentedUpperCase(Pos:Pos)
!          ELSE
!             cycle !ResultString(i:i) = InputString(i:i)
!          ENDIF
!       end if
!    end do

!    do i = 1, LEN_TRIM(InputString)
!       ! first check for normal lowercase char, then normal uppercase char
!       if(InputString(i:i) >= "a" .and. InputString(i:i) <= "z") then
!          ResultString(i:i) = achar(iachar(InputString(i:i)) - 32)
!       else if (InputString(i:i) >= "A" .and. InputString(i:i) <= "Z") then
!          cycle !ResultString(i:i) = InputString(i:i)  ! leave as is
!       else ! now see if it's an accented char that needs uppercaseing
!          Pos=SCAN(AccentedLowerCase,InputString(i:i))
!          if (Pos /= 0) THEN
!             ResultString(i:i)=AccentedUpperCase(Pos:Pos)
!          ELSE
!             cycle !ResultString(i:i) = InputString(i:i)
!          ENDIF
!       end if
!    end do
!
!    ResultString=TRIM(ResultString)

!  ResultString=Blank
!  Pos=SCAN(InputString,LowerCase)
!  IF (POS /= 0) THEN
!    LengthInputString=LEN_TRIM(InputString)
!    DO Count=1,LengthInputString
!      Pos=SCAN(LowerCase,InputString(Count:Count))
!      IF (Pos /= 0) THEN
!        ResultString(Count:Count)=UpperCase(Pos:Pos)
!      ELSE
!        ResultString(Count:Count)=InputString(Count:Count)
!      ENDIF
!    END DO
!    ResultString=TRIM(ResultString)
!  ELSE
!    ! String already in Upper Case
!    ResultString=TRIM(InputString)
!  ENDIF

  RETURN

END FUNCTION DOMakeUPPERCase

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

END MODULE DataOutputs

