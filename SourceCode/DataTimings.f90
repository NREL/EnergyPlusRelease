#ifdef EP_NO_Timings
#undef EP_Timings
#endif

#include "Timer.h"

MODULE DataTimings      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for data and routines for timing within EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataSystemVariables, ONLY: tabchar, DeveloperFlag

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxTimingStringLength=250 ! string length for timing string array

          ! DERIVED TYPE DEFINITIONS
  TYPE timings
    CHARACTER(len=MaxTimingStringLength) :: Element=' '
    REAL(r64)                            :: rstartTime=0.0d0
    REAL(r64)                            :: currentTimeSum=0.0d0
    INTEGER                              :: calls=0
  END TYPE

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  TYPE(timings), ALLOCATABLE, DIMENSION(:) :: Timing
  INTEGER :: NumTimingElements=0
  INTEGER :: MaxTimingElements=0
  real(r64) :: dailyWeatherTime, dailyExteriorEnergyUseTime, dailyHeatBalanceTime
  real(r64) :: hbdailyInit,hbdailyOutSurf,hbdailyInSurf,hbdailyHVAC,hbdailyRep
  real(r64) :: clockrate
  LOGICAL :: lprocessingInputTiming = .false.
  LOGICAL :: lmanageSimulationTiming = .false.
  LOGICAL :: lcloseoutReportingTiming = .false.

  ! Following for calls to routines
#ifdef EP_Count_Calls
  INTEGER :: NumShadow_Calls = 0
  INTEGER :: NumShadowAtTS_Calls = 0
  INTEGER :: NumClipPoly_Calls = 0
  INTEGER :: NumInitSolar_Calls = 0
  INTEGER :: NumAnisoSky_Calls = 0
  INTEGER :: NumDetPolyOverlap_Calls = 0
  INTEGER :: NumCalcPerSolBeam_Calls = 0
  INTEGER :: NumDetShadowCombs_Calls = 0
  INTEGER :: NumIntSolarDist_Calls = 0
  INTEGER :: NumIntRadExchange_Calls = 0
  INTEGER :: NumIntRadExchangeZ_Calls = 0
  INTEGER :: NumIntRadExchangeMain_Calls = 0
  INTEGER :: NumIntRadExchangeOSurf_Calls = 0
  INTEGER :: NumIntRadExchangeISurf_Calls = 0
  INTEGER :: NumMaxInsideSurfIterations = 0
  INTEGER :: NumCalcScriptF_Calls = 0
#endif

CONTAINS

SUBROUTINE epStartTime(ctimingElementstring)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Implement a timing scheme using start-stop (ref: epStopTime) that will help
          ! developers pinpoint problems.

          ! METHODOLOGY EMPLOYED:
          ! structure similar to recurring error structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
#if defined (_OPENMP) && defined(TIMER_OMP_GET_WTIME)
  use omp_lib ! only here for OMP timer
#endif

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ctimingElementstring

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(timings), ALLOCATABLE, DIMENSION(:) :: tempTiming  ! used for reallocate.
  INTEGER :: loop  ! testing if already in structure
  INTEGER :: found ! indicator for element

#ifdef EP_NO_Timings
  RETURN
#endif
#ifdef EP_Timings
  IF (NumTimingElements == 0) THEN
    MaxTimingElements=250
    ALLOCATE(Timing(MaxTimingElements))
  ELSEIF (NumTimingElements == MaxTimingElements) THEN
    ALLOCATE(tempTiming(MaxTimingElements+250))
    tempTiming(1:MaxTimingElements)=Timing(1:MaxTimingElements)
    DEALLOCATE(Timing)
    MaxTimingElements=MaxTimingElements+250
    ALLOCATE(Timing(MaxTimingElements))
    Timing(1:MaxTimingElements)=tempTiming(1:MaxTimingElements)
    DEALLOCATE(tempTiming)
  ENDIF

  found=0
  DO loop=1,NumTimingElements
    IF (Timing(loop)%Element /= ctimingElementstring) cycle
    found=loop
  ENDDO

  IF (found == 0) THEN
    NumTimingElements=NumTimingElements+1
    Timing(NumTimingElements)%Element = ctimingElementstring
    found=NumTimingElements
  ENDIF

  TSTART(Timing(found)%rstartTime)
  Timing(found)%calls=Timing(found)%calls+1

  RETURN
#endif

END SUBROUTINE epStartTime

SUBROUTINE epStopTime(ctimingElementstring,printit,wprint)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Implement a timing scheme using start-stop (ref: epStartTime) that will help
          ! developers pinpoint problems.

          ! METHODOLOGY EMPLOYED:
          ! structure similar to recurring error structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

#if defined (_OPENMP) && defined(TIMER_OMP_GET_WTIME)
  use omp_lib ! only here for OMP timer
#endif
  USE DataInterfaces, ONLY: ShowFatalError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ctimingElementstring
  LOGICAL, INTENT(IN), OPTIONAL :: printit   ! true if it should be printed here.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: wprint  ! only needed (and assumed, if printit is true)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: loop  ! testing if already in structure
  INTEGER :: found ! indicator for element
  REAL(r64) :: stoptime

#ifdef EP_NO_Timings
  RETURN
#endif
#ifdef EP_Timings
  found=0
  DO loop=1,NumTimingElements
    IF (Timing(loop)%Element /= ctimingElementstring) cycle
    found=loop
  ENDDO

  IF (found == 0) THEN
    CALL ShowFatalError('epStopTime: No element='//trim(ctimingElementstring))
  ENDIF

  TSTOP(stoptime)
  Timing(found)%currentTimeSum=Timing(found)%currentTimeSum+(stoptime-Timing(found)%rstartTime)

  IF (PRESENT(printit)) THEN
    IF (printit) THEN
      SELECT CASE(wprint)
        CASE('PRINT_TIME0')
          write(*,'(a80,f16.4)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIME1')
          write(*,'(a70,f16.4)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIME2')
          write(*,'(a60,f10.4)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIME2i')
          write(*,'(a56,i4,f10.4)') ctimingElementstring,Timing(found)%calls,Timing(found)%currentTimeSum
        CASE('PRINT_TIME3')
          write(*,'(a50,f10.4)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIME3i')
          write(*,'(a46,i4,f10.4)') ctimingElementstring,Timing(found)%calls,Timing(found)%currentTimeSum
        CASE('PRINT_TIME4')
          write(*,'(a40,f10.4)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIMEX')
          write(*,'(a100,f16.6)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINTES')
          write(*,'(a80,es22.15)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIME_AF')
          write(*,'(a55,10x,f16.4)') ctimingElementstring, stoptime-Timing(found)%rstartTime
        CASE('PRINT_TIME_AIF')
          write(*,'(a55,i10,f16.4)') ctimingElementstring,Timing(found)%calls,Timing(found)%currentTimeSum
        CASE DEFAULT
          write(*,*) trim(ctimingElementstring),Timing(found)%currentTimeSum
      END SELECT
    ENDIF
!could not cover:
!#define PRINT_TIME_AIIF(a, i1, i2, t) write(*,'(a55,i10,i10,f16.4)') a, i1, i2, t
!#define PRINT_TIME_AIIIF(a, i1, i2, i3, t) write(*,'(a55,i10,i10,i10,f16.55)') a, i1, i2, i3, t
  ENDIF

  RETURN
#endif

END SUBROUTINE epStopTime

SUBROUTINE epSummaryTimes(TimeUsed_CPUTime)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Print summary of timings from timing scheme using start-stop (ref: epStartTime, epStopTime) that will help
          ! developers pinpoint problems.

          ! METHODOLOGY EMPLOYED:
          ! structure similar to recurring error structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) :: TimeUsed_CPUTime

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER loop
  INTEGER :: EchoInputFile
  INTEGER, EXTERNAL :: FindUnitNumber

#ifdef EP_NO_Timings
  RETURN
#endif
#ifdef EP_Timings
  EchoInputFile=FindUnitNumber('eplusout.audit')
  WRITE(EchoInputFile,fmta) 'Timing Element'//tabchar//'# calls'//tabchar//'Time {s}'//tabchar//'Time {s} (per call)'

  DO loop=1,NumTimingElements
    IF (Timing(loop)%calls > 0) THEN
      WRITE(EchoInputFile,fmta) trim(Timing(loop)%Element)//tabchar//trim(RoundSigDigits(Timing(loop)%calls))//  &
         tabchar//trim(RoundSigDigits(Timing(loop)%currentTimeSum,3))//tabchar//  &
         trim(RoundSigDigits(Timing(loop)%currentTimeSum/REAL(Timing(loop)%calls,r64),3))
    ELSE
      WRITE(EchoInputFile,fmta) trim(Timing(loop)%Element)//tabchar//trim(RoundSigDigits(Timing(loop)%calls))//  &
         tabchar//trim(RoundSigDigits(Timing(loop)%currentTimeSum,3))//tabchar//  &
         trim(RoundSigDigits(-999.0d0,3))
    ENDIF
  ENDDO
  WRITE(EchoInputFile,fmta) 'Time from CPU_Time'//tabchar//trim(RoundSigDigits(TimeUsed_CPUTime,3))

  RETURN

#endif

END SUBROUTINE epSummaryTimes

FUNCTION epGetTimeUsed(ctimingElementstring) RESULT(totalTimeUsed)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provides outside function to getting time used on a particular element

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError
  USE DataErrorTracking, ONLY: AbortProcessing

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ctimingElementstring
  REAL(r64) :: totalTimeUsed

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: loop  ! testing if already in structure
  INTEGER :: found ! indicator for element

  found=0
  DO loop=1,NumTimingElements
    IF (Timing(loop)%Element /= ctimingElementstring) cycle
    found=loop
  ENDDO

  IF (found == 0 .and. .not. AbortProcessing) THEN
    CALL ShowFatalError('epGetTimeUsed: No element='//trim(ctimingElementstring))
  ELSE
    CALL ShowSevereError('epGetTimeUsed: No element='//trim(ctimingElementstring))
  ENDIF

  totalTimeUsed = Timing(found)%currentTimeSum

  RETURN

END FUNCTION epGetTimeUsed

FUNCTION epGetTimeUsedperCall(ctimingElementstring) RESULT(averageTimeUsed)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provides outside function to getting time used on a particular element
          ! per Call.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError
  USE DataErrorTracking, ONLY: AbortProcessing

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ctimingElementstring
  REAL(r64) :: averageTimeUsed

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: loop  ! testing if already in structure
  INTEGER :: found ! indicator for element

  found=0
  DO loop=1,NumTimingElements
    IF (Timing(loop)%Element /= ctimingElementstring) cycle
    found=loop
  ENDDO

  IF (found == 0) THEN
    CALL ShowFatalError('epGetTimeUsedperCall: No element='//trim(ctimingElementstring))
  ELSE
    CALL ShowSevereError('epGetTimeUsedperCall: No element='//trim(ctimingElementstring))
  ENDIF

  IF (Timing(found)%calls > 0) THEN
    averageTimeUsed = Timing(found)%currentTimeSum/REAL(Timing(found)%calls,r64)
  ELSE
    averageTimeUsed = -999.0d0
  ENDIF

  RETURN

END FUNCTION epGetTimeUsedperCall

FUNCTION eptime() RESULT(calctime)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! An alternative method for timing (to CPU_TIME) is to call the standard
          ! System_Clock routine.  This is a standard alternative to CPU_TIME.
          ! According to Intel documentation, the "count_rate" may differ depending on
          ! the size of the integer to receive the output.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) :: calctime  ! calculated time based on "count" and "count_rate"

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER(i32) :: icount

  CALL system_clock(count=icount)

  calctime=real(icount,r64)/clockrate  ! clockrate is set by main program.

  RETURN

END FUNCTION eptime

FUNCTION epElapsedTime() RESULT(calctime)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! An alternative method for timing elapsed times is to call the standard
          ! Date_And_Time routine and set the "time".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) :: calctime  ! calculated time based on hrs, minutes, seconds, milliseconds

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER(i32) :: clockvalues(8)
               !value(1)   Current year
               !value(2)   Current month
               !value(3)   Current day
               !value(4)   Time difference with respect to UTC in minutes (0-59)
               !value(5)   Hour of the day (0-23)
               !value(6)   Minutes (0-59)
               !value(7)   Seconds (0-59)
               !value(8)   Milliseconds (0-999)

  CALL date_and_time(values=clockvalues)
  calctime = clockvalues(5)*3600.d0+clockvalues(6)*60.d0+clockvalues(7)+clockvalues(8)/1000.d0

  RETURN

END FUNCTION epElapsedTime

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

END MODULE DataTimings
