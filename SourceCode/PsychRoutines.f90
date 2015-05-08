#ifdef EP_nocache_Psychrometrics
#undef EP_cache_PsyTwbFnTdbWPb
#undef EP_cache_PsyPsatFnTemp
#else
#define EP_cache_PsyTwbFnTdbWPb
#define EP_cache_PsyPsatFnTemp
#endif
#define EP_psych_errors
! the following defines would only be used in special instances and not for release.
!!!!#define EP_psych_stats
!!!!#define generatetestdata
Module Psychrometrics
 ! Module containing the Psychometric simulation routines

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1998
          !       MODIFIED       February 2010
          !       RE-ENGINEERED  Jan 2004: Rahul Chillar

          ! PURPOSE OF THIS MODULE:
          ! This module provides a repository for the psychrometric routines.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! Todo after 2.2 release:
          ! remove restriction on Max(W, 1d-5)
          ! more research on hfg calc

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
#ifdef EP_psych_errors
  USE DataGlobals
  USE DataEnvironment
  USE DataInterfaces
#endif

  ! Use Statements for other routines
#ifdef EP_psych_errors
  USE General, ONLY: TrimSigDigits
#endif

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
                                    ! call for recurring errors
      INTEGER, PARAMETER :: iPsyTdpFnTdbTwbPb     =1
      INTEGER, PARAMETER :: iPsyRhFnTdbWPb        =2
      INTEGER, PARAMETER :: iPsyTwbFnTdbWPb       =3
      INTEGER, PARAMETER :: iPsyTwbFnTdbWPb2      =14
      INTEGER, PARAMETER :: iPsyTwbFnTdbWPb3      =15  ! convergence
      INTEGER, PARAMETER :: iPsyVFnTdbWPb         =4
      INTEGER, PARAMETER :: iPsyWFnTdpPb          =5
      INTEGER, PARAMETER :: iPsyWFnTdbH           =6
      INTEGER, PARAMETER :: iPsyWFnTdbTwbPb       =7
      INTEGER, PARAMETER :: iPsyWFnTdbTwbPb2      =16
      INTEGER, PARAMETER :: iPsyWFnTdbRhPb        =8
      INTEGER, PARAMETER :: iPsyPsatFnTemp        =9
      INTEGER, PARAMETER :: iPsyTsatFnHPb         =10
      INTEGER, PARAMETER :: iPsyTsatFnPb          =11
      INTEGER, PARAMETER :: iPsyTsatFnPb2         =17 ! iterations
      INTEGER, PARAMETER :: iPsyRhFnTdbRhov       =12
      INTEGER, PARAMETER :: iPsyRhFnTdbRhovLBnd0C =13
      INTEGER, PARAMETER :: iPsyTwbFnTdbWPb_cache =18
      INTEGER, PARAMETER :: iPsyPsatFnTemp_cache  =19
      INTEGER, PARAMETER :: NumPsychMonitors=19 ! Parameterization of Number of psychrometric routines that
#ifdef EP_psych_stats
      CHARACTER(len=*), PARAMETER, DIMENSION(NumPsychMonitors) :: PsyRoutineNames=      &
              (/'PsyTdpFnTdbTwbPb      ',  &  ! 1
                'PsyRhFnTdbWPb         ',  &  ! 2
                'PsyTwbFnTdbWPb        ',  &  ! 3
                'PsyVFnTdbWPb          ',  &  ! 4
                'PsyWFnTdpPb           ',  &  ! 5
                'PsyWFnTdbH            ',  &  ! 6
                'PsyWFnTdbTwbPb        ',  &  ! 7
                'PsyWFnTdbRhPb         ',  &  ! 8
                'PsyPsatFnTemp         ',  &  ! 9
                'PsyTsatFnHPb          ',  &  ! 10
                'PsyTsatFnPb           ',  &  ! 11
                'PsyRhFnTdbRhov        ',  &  ! 12
                'PsyRhFnTdbRhovLBnd0C  ',  &  ! 13
                'PsyTwbFnTdbWPb        ',  &  ! 14 - HR
                'PsyTwbFnTdbWPb        ',  &  ! 15 - max iter
                'PsyWFnTdbTwbPb        ',  &  ! 16 - HR
                'PsyTsatFnPb           ',  &  ! 17 - max iter
                'PsyTwbFnTdbWPb_cache  ',  &  ! 18 - PsyTwbFnTdbWPb_raw (raw calc)
                'PsyPsatFnTemp_cache   '/)    ! 19 - PsyPsatFnTemp_raw (raw calc)

      LOGICAL, PARAMETER, DIMENSION(NumPsychMonitors) :: PsyReportIt=   &
      (/.true.,  & ! PsyTdpFnTdbTwbPb     1
        .true.,  & ! PsyRhFnTdbWPb        2
        .true.,  & ! PsyTwbFnTdbWPb       3
        .true.,  & ! PsyVFnTdbWPb         4
        .true.,  & ! PsyWFnTdpPb          5
        .true.,  & ! PsyWFnTdbH           6
        .true.,  & ! PsyWFnTdbTwbPb       7
        .true.,  & ! PsyWFnTdbRhPb        8
        .true.,  & ! PsyPsatFnTemp        9
        .true.,  & ! PsyTsatFnHPb         10
        .true.,  & ! PsyTsatFnPb          11
        .true.,  & ! PsyRhFnTdbRhov       12
        .true.,  & ! PsyRhFnTdbRhovLBnd0C 13
        .false., & ! PsyTwbFnTdbWPb       14 - HR
        .false., & ! PsyTwbFnTdbWPb       15 - max iter
        .false., & ! PsyWFnTdbTwbPb       16 - HR
        .false., & ! PsyTsatFnPb          17 - max iter
        .true.,  & ! PsyTwbFnTdbWPb_cache 18 - PsyTwbFnTdbWPb_raw (raw calc)
        .true./)   ! PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
#endif

#ifndef EP_psych_errors
      REAL(r64), PARAMETER :: KelvinConv=273.15d0
#endif

#ifdef EP_cache_PsyTwbFnTdbWPb
  integer, parameter :: twbcache_size=1024*1024
  integer, parameter :: twbprecision_bits=20

          ! DERIVED TYPE DEFINITIONS:
  TYPE :: cached_twb_t
    INTEGER(i64) :: iTdb  = 0
    INTEGER(i64) :: iW    = 0
    INTEGER(i64) :: iPb   = 0
    REAL(r64) :: Twb  = 0.0d0
  END TYPE
#endif
#ifdef EP_cache_PsyPsatFnTemp
  integer, parameter :: psatcache_size=1024*1024
  integer, parameter :: psatprecision_bits=24 !28  !24  !32

          ! DERIVED TYPE DEFINITIONS:
  TYPE :: cached_psat_t
    INTEGER(i64) :: iTdb  = -1000
    REAL(r64) :: Psat = 0.0d0
  END TYPE
#endif

          ! MODULE VARIABLE DECLARATIONS:
          ! na


          ! MODULE VARIABLE DEFINITIONS:
      CHARACTER(len=150) String
      LOGICAL :: ReportErrors=.true.
      INTEGER, DIMENSION(NumPsychMonitors) ::  iPsyErrIndex = NumPsychMonitors*0 ! Number of times error occurred
#ifdef EP_psych_stats
      INTEGER(i64), DIMENSION(NumPsychMonitors) :: NumTimesCalled=NumPsychMonitors*0
      INTEGER, DIMENSION(NumPsychMonitors) :: NumIterations =NumPsychMonitors*0
#endif
#ifdef EP_cache_PsyTwbFnTdbWPb
      TYPE(cached_twb_t), ALLOCATABLE, DIMENSION(:)  :: cached_Twb !DIMENSION(0:twbcache_size)
#endif
#ifdef EP_cache_PsyPsatFnTemp
      TYPE(cached_psat_t), ALLOCATABLE, DIMENSION(:) :: cached_Psat  !DIMENSION(0:psatcache_size)
#endif


! Subroutine Specifications for the Module
PUBLIC  PsyRhoAirFnPbTdbW
PUBLIC  PsyCpAirFnWTdb
PUBLIC  PsyHfgAirFnWTdb
PUBLIC  PsyHgAirFnWTdb
PUBLIC  PsyTdpFnTdbTwbPb
PUBLIC  PsyTdpFnWPb
PUBLIC  PsyHFnTdbW
PUBLIC  PsyHFnTdbRhPb
PUBLIC  PsyTdbFnHW
PUBLIC  PsyRhovFnTdbRh
PUBLIC  PsyRhovFnTdbRhLBnd0C
PUBLIC  PsyRhovFnTdbWPb
PUBLIC  PsyRhFnTdbRhov
PUBLIC  PsyRhFnTdbRhovLBnd0C
PUBLIC  PsyRhFnTdbWPb
PUBLIC  PsyTwbFnTdbWPb
PUBLIC  PsyVFnTdbWPb
PUBLIC  PsyWFnTdpPb
PUBLIC  PsyWFnTdbH
PUBLIC  PsyWFnTdbTwbPb
PUBLIC  PsyWFnTdbRhPb
Public  PsyPsatFnTemp
PUBLIC  PsyTsatFnHPb
PRIVATE PsyTsatFnPb
PUBLIC  CPCW
PUBLIC  CPHW
PUBLIC  RhoH2O
PUBLIC  ShowPsychrometricSummary
PUBLIC  InitializePsychRoutines

CONTAINS

SUBROUTINE InitializePsychRoutines

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes some variables for PsychRoutines

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

#ifdef EP_cache_PsyTwbFnTdbWPb
  ALLOCATE(cached_Twb(0:twbcache_size))
#endif
#ifdef EP_cache_PsyPsatFnTemp
  ALLOCATE(cached_Psat(0:psatcache_size))
#endif

  RETURN

END SUBROUTINE InitializePsychRoutines

SUBROUTINE ShowPsychrometricSummary

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provides a Psychrometric summary report to the audit file.
          ! Maybe later to the .eio file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
#ifdef EP_psych_stats
  INTEGER :: EchoInputFile  ! found unit number for "eplusout.audit"
  INTEGER, EXTERNAL :: FindUnitNumber
  INTEGER :: Loop
  REAL(r64) :: AverageIterations
  CHARACTER(len=32) :: istring

  EchoInputFile=FindUnitNumber('eplusout.audit')
  IF (EchoInputFile == 0) RETURN
  IF (ANY(NumTimesCalled>0)) THEN
    WRITE(EchoInputFile,fmta) 'RoutineName,#times Called,Avg Iterations'
    DO Loop=1,NumPsychMonitors
      IF (.not. PsyReportIt(Loop)) CYCLE
      write(istring,*) NumTimesCalled(Loop)
      istring=adjustl(istring)
      IF (NumIterations(Loop) > 0) THEN
        AverageIterations=REAL(NumIterations(Loop),r64)/REAL(NumTimesCalled(Loop),r64)
        WRITE(EchoInputFile,fmta) trim(PsyRoutineNames(Loop))//','//trim(istring)//','//trim(RoundSigDigits(AverageIterations,2))
      ELSE
        WRITE(EchoInputFile,fmta) trim(PsyRoutineNames(Loop))//','//trim(istring)
      ENDIF
    ENDDO
  ENDIF
#endif

  RETURN

END SUBROUTINE ShowPsychrometricSummary

function PsyRhoAirFnPbTdbW(pb,tdb,dw,calledfrom)  result(rhoair)

          ! FUNCTION INFORMATION:
          !       AUTHOR         G. S. Wright
          !       DATE WRITTEN   June 2, 1994
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides density of air as a function of barometric
          ! pressure, dry bulb temperature, and humidity ratio.

          ! METHODOLOGY EMPLOYED:
          ! ideal gas law
          !    universal gas const for air 287 J/(kg K)
          !    air/water molecular mass ratio 28.9645/18.01534

          ! REFERENCES:
          ! Wylan & Sontag, Fundamentals of Classical Thermodynamics.
          ! ASHRAE handbook 1985 Fundamentals, Ch. 6, eqn. (6),(26)

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: pb     ! barometric pressure (Pascals)
      REAL(r64), intent(in)  :: tdb    ! dry bulb temperature (Celsius)
      REAL(r64), intent(in)  :: dw      ! humidity ratio (kgWater/kgDryAir)
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)         :: rhoair ! result=> density of air

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) w  ! humidity ratio

      w=MAX(dw,1.0d-5)
      rhoair = pb/(287.d0*(tdb+KelvinConv)*(1.d0+1.6077687d0*w))
#ifdef EP_psych_errors
      if (rhoair < 0.0d0) then
        CALL ShowSevereError('PsyRhoAirFnPbTdbW: RhoAir (Density of Air) is calculated <= 0 ['// &
             trim(RoundSigDigits(rhoair,5))//'].')
        CALL ShowContinueError('pb =['//trim(RoundSigDigits(pb,2))//'], tdb=['//trim(RoundSigDigits(tdb,2))//  &
             '], w=['//trim(RoundSigDigits(dw,7))//'].')
        if (present(calledfrom)) then
          CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
        else
          CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
        endif
        CALL ShowFatalError('Program terminates due to preceding condition.')
      endif
#endif

  return
end function PsyRhoAirFnPbTdbW


function PsyCpAirFnWTdb(dw,T,calledfrom) result(cpa)

          ! FUNCTION INFORMATION:
          !       AUTHOR         J. C. VanderZee
          !       DATE WRITTEN   Feb. 1994
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the heat capacity of air {J/kg-C} as function of humidity ratio.

          ! METHODOLOGY EMPLOYED:
          ! take numerical derivative of PsyHFnTdbW function

          ! REFERENCES:
          ! see PsyHFnTdbW ref. to ASHRAE Fundamentals
          ! USAGE:  cpa = PsyCpAirFnWTdb(w,T)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: dw    ! humidity ratio {kgWater/kgDryAir}
      REAL(r64), intent(in)  :: T    ! input temperature {Celsius}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)         :: cpa  ! result => heat capacity of air {J/kg-C}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) h1  ! PsyHFnTdbW result of input parameters
      REAL(r64) tt  ! input temperature (T) + .1
      REAL(r64) h2  ! PsyHFnTdbW result of input humidity ratio and tt
      REAL(r64) w  ! humidity ratio

      REAL(r64), SAVE :: dwSave = -100.0d0
      REAL(r64), SAVE :: Tsave = -100.0d0
      REAL(r64), SAVE :: cpaSave = -100.0d0

      !check if last call had the same input and if it did just use the
      !saved output.
      IF (Tsave .EQ. T) THEN
        IF (dwSave .EQ. dw)  THEN
          cpa = cpaSave
          RETURN
        END IF
      END IF

      w=MAX(dw,1.0d-5)
      h1 = PsyHFnTdbW(T,w,calledfrom)
      tt = T + 0.1d0
      h2 = PsyHFnTdbW(tt,w,calledfrom)
      cpa = (h2-h1)/0.1d0

      !save values for next call
      dwSave = dw
      Tsave = T
      cpaSave = cpa
  return
end function PsyCpAirFnWTdb


function PsyHfgAirFnWTdb(w,T,calledfrom) result(hfg)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May, 2001
          !       MODIFIED       June, 2002
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides latent energy of air as function of humidity ratio and temperature.

          ! METHODOLOGY EMPLOYED:
          ! calculates hg and then hf and the difference is Hfg.

          ! REFERENCES:
          ! see ASHRAE Fundamentals Psychrometric Chapter
          ! USAGE:  hfg = PsyHfgAirFnWTdb(w,T)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: w    ! humidity ratio {kgWater/kgDryAir} !unused1208
      REAL(r64), intent(in)  :: T    ! input temperature {Celsius}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)         :: hfg  ! result => heat of vaporization for moist air {J/kg}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) :: hg  ! enthalpy of the gas
      REAL(r64) :: hf  ! enthalpy of the fluid
!      INTEGER,SAVE :: b0cerrcount=0
      REAL(r64) :: Temperature    ! input temperature {Celsius} - corrected for >= 0C

! This formulation currently does not use W since it returns results that are in J/kg and the
!  amount of energy is on a per unit of moisture basis.

      Temperature=MAX(T,0.0d0)
!      Temperature=T
      hg = 2500940.0d0 + 1858.95d0*Temperature
      hf = 4180.0d0*Temperature
      hfg = hg - hf
!4/8/08 - pending comments      hfg = hg

  return

end function PsyHfgAirFnWTdb


function PsyHgAirFnWTdb(w,T,calledfrom) result(hg)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May, 2001
          !       MODIFIED       June, 2002
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides latent energy of the moisture as a gas in the air as
          ! function of humidity ratio and temperature.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! see ASHRAE Fundamentals Psychrometric Chapter
          ! USAGE:  hg = PsyHgAirFnWTdb(w,T)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: w   ! humidity ratio {kgWater/kgDryAir} !unused1208
      REAL(r64), intent(in)  :: T   ! input temperature {Celsius}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)         :: hg  ! enthalpy of the gas {units?}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

! This formulation currently does not use W since it returns results that are in J/kg and the
!  amount of energy is on a per unit of moisture basis.

      hg = 2500940.0d0 + 1858.95d0*T

  return
end function PsyHgAirFnWTdb




FUNCTION PsyTdpFnTdbTwbPb(TDB,TWB,PB,calledfrom) RESULT(TDP)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the dew-point temperature {C} from dry-bulb, wet-bulb and pressure.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: TDB  ! dry-bulb temperature {C}
      REAL(r64), intent(in)  :: TWB  ! wet-bulb temperature {C}
      REAL(r64), intent(in)  :: PB   ! barometric pressure (N/M**2) {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)         :: TDP  ! result=> dew-point temperature {C}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) W  ! humidity ratio
      !
!                          calculate dew point temperature

      W=PsyWFnTdbTwbPb(TDB,TWB,PB,calledfrom)
      W=MAX(W,1.0d-5)

      TDP=PsyTdpFnWPb(W,PB,calledfrom)

#ifdef EP_psych_stats
      NumTimesCalled(iPsyTdpFnTdbTwbPb)=NumTimesCalled(iPsyTdpFnTdbTwbPb)+1
#endif

!                                      VALIDITY TEST.
      IF (TDP > TWB) THEN
#ifdef EP_psych_errors
        IF (TDP > TWB+0.1d0) THEN
          IF (.not. WarmupFlag) THEN    ! Display error message
            IF (iPsyErrIndex(iPsyTdpFnTdbTwbPb) == 0) THEN
              CALL ShowWarningMessage('Calculated Dew Point Temperature being reset (PsyTdpFnTdbTwbPb)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              String=' Dry-bulb='//TRIM(TrimSigDigits(TDB,2))//' Wet-Bulb (WB)= '//TRIM(TrimSigDigits(TWB,2))// &
                      ' Pressure= '//TRIM(TrimSigDigits(PB,2))//' Humidity Ratio='//TRIM(TrimSigDigits(W,3))
              CALL ShowContinueError(TRIM(String))
              String=' Calculated Dew Point Temperature (DPT)= '//TRIM(TrimSigDigits(TDP,2))//  &
                    '; Since DPT > WB, DPT will be set to WB'
              Call ShowContinueError(TRIM(String))
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Calculated Dew Point Temperature being reset (PsyTdpFnTdbTwbPb)', &
              iPsyErrIndex(iPsyTdpFnTdbTwbPb),ReportMinOf=TDP,ReportMaxOf=TDP,ReportMinUnits='C',ReportMaxUnits='C')
          ENDIF

          TDP=TWB

        ELSE
          TDP=TWB
        ENDIF
#endif
        TDP=TWB
      ENDIF

      ! TDP is the result

  RETURN

END FUNCTION PsyTdpFnTdbTwbPb


FUNCTION PsyTdpFnWPb(W,PB,calledfrom) RESULT(TDP)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the dew-point temperature {C} from humidity ratio and pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P.99, EQN 22

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: W    ! humidity ratio
      REAL(r64), intent(in)  :: PB   ! barometric pressure (N/M**2) {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)         :: TDP  ! result=> dew-point temperature {C}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) PDEW  ! pressure at dew point temperature
      REAL(r64) W0    ! limited humidity ratio

      W0=MAX(W,1.0d-5)
      PDEW=PB*W0/(0.62198d0+W0)
      TDP=PsyTsatFnPb(PDEW,calledfrom)

  RETURN
END FUNCTION PsyTdpFnWPb


FUNCTION PsyHFnTdbW(TDB,dW,calledfrom) RESULT (H)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the enthalpy {J/kg} from dry-bulb temperature and humidity ratio.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in)  :: TDB   ! dry-bulb temperature {C}
      REAL(r64), intent(in)  :: dW    ! humidity ratio
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)         :: H    ! enthalpy {J/kg}


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) W  ! humidity ratio

!                           calculate enthalpy

      W=MAX(dW,1.0d-5)
      H=1.00484d3*TDB+W*(2.50094d6+1.85895d3*TDB)

  RETURN
END FUNCTION PsyHFnTdbW


FUNCTION PsyHFnTdbRhPb(TDB,RH,PB,calledfrom) RESULT(H)

          ! FUNCTION INFORMATION:
          !       AUTHOR         J. C. VanderZee
          !       DATE WRITTEN   Feb. 1994
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides air enthalpy from temperature and relative humidity.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
          !   by using functions PsyWFnTdbRhPb and PsyHFnTdbW

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB  ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: RH   ! relative humidity value (0.0 - 1.0)
      REAL(r64), intent(in) :: PB   ! barometric pressure (N/M**2) {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: H    ! result=> enthalpy {J/kg}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) W   ! humidity ratio

      W = PsyWFnTdbRhPb(TDB,RH,PB)
      W=MAX(W,1.0d-5)
      H = PsyHFnTdbW(TDB,W)

  RETURN
END FUNCTION PsyHFnTdbRhPb


FUNCTION PsyTdbFnHW(H,dW,calledfrom) RESULT(TDB)

          ! FUNCTION INFORMATION:
          !       AUTHOR         J. C. VanderZee
          !       DATE WRITTEN   Feb. 1994
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides air temperature from enthalpy and humidity ratio.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
          !   by inverting function PsyHFnTdbW

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: H    ! enthalpy {J/kg}
      REAL(r64), intent(in) :: dW    ! humidity ratio
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: TDB  ! result=> dry-bulb temperature {C}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) W   ! humidity ratio

      W=MAX(dW,1.0d-5)
      TDB = (H - 2.50094d6 * W)/(1.00484d3 + 1.85895d3*W)

  RETURN
END FUNCTION PsyTdbFnHW


FUNCTION PsyRhovFnTdbRh(Tdb,RH,calledfrom) RESULT(RhoV)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. J. Liesen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
          !                      Function is continuous over temperature spectrum
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the Vapor Density in air as a
          ! function of dry bulb temperature, and Relative Humidity.

          ! METHODOLOGY EMPLOYED:
          ! ideal gas law
          ! Universal gas const for water vapor 461.52 J/(kg K)

          ! REFERENCES:
          ! ASHRAE handbook 1993 Fundamentals, ??
          ! Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
          ! values from PsyRhFnTdbWPb


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: RH     ! relative humidity value (0.0-1.0)
      REAL(r64), intent(in) :: Tdb    ! dry-bulb temperature {C}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: RhoV   ! Vapor density in air

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
        REAL(r64) :: pws  ! saturation pressure for Tdb

    pws=PsyPsatFnTemp(Tdb,'PsyRhovFnTdbRh')

    RhoV = (pws*RH)/(461.52d0*(Tdb+KelvinConv))

  return
end function PsyRhovFnTdbRh

FUNCTION PsyRhovFnTdbRhLBnd0C(Tdb,RH,calledfrom) RESULT(RhoV)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. J. Liesen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Name change to signify derivation and temperatures were used
          !                      with 0C as minimum; LKL January 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the Vapor Density in air as a
          ! function of dry bulb temperature, and Relative Humidity.

          ! METHODOLOGY EMPLOYED:
          ! ideal gas law
          ! Universal gas const for water vapor 461.52 J/(kg K)

          ! REFERENCES:
          ! ASHRAE handbook 1993 Fundamentals,

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: RH     ! relative humidity value (0.0-1.0)
      REAL(r64), intent(in) :: Tdb    ! dry-bulb temperature {C}
      character(*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: RhoV   ! Vapor density in air

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      RhoV = RH/(461.52d0*(Tdb+KelvinConv))*Exp(23.7093d0-4111.0d0/ &
                ((Tdb+KelvinConv)-35.45d0))

  return
end function PsyRhovFnTdbRhLBnd0C

FUNCTION PsyRhovFnTdbWPb(Tdb,dW,PB,calledfrom) RESULT(RhoV)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. J. Liesen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the Vapor Density in air as a
          ! function of dry bulb temperature, Humidity Ratio, and Barometric Pressure.

          ! METHODOLOGY EMPLOYED:
          ! ideal gas law
          ! Universal gas const for water vapor 461.52 J/(kg K)

          ! REFERENCES:
          ! ASHRAE handbook 1993 Fundamentals,

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: Tdb    ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: dW      ! humidity ratio
      REAL(r64), intent(in) :: Pb     ! Barometric Pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: RhoV   ! Vapor density in air

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) W   ! humidity ratio

      W=MAX(dW,1.0d-5)
      RhoV =W*PB/(461.52d0*(Tdb+KelvinConv)*(W+0.62198d0))

  return
end function PsyRhovFnTdbWPb


FUNCTION PsyRhFnTdbRhov(Tdb,Rhovapor,calledfrom) RESULT(RHValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. J. Liesen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
          !                      Function is continuous over temperature spectrum
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the Relative Humidity in air as a
          ! function of dry bulb temperature and Vapor Density.

          ! METHODOLOGY EMPLOYED:
          ! ideal gas law
          ! Universal gas const for water vapor 461.52 J/(kg K)

          ! REFERENCES:
          ! ASHRAE handbook 1993 Fundamentals,
          ! Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
          ! values from PsyRhFnTdbWPb

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: Tdb     ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: Rhovapor  ! vapor density in air {kg/m3}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: RHValue ! relative humidity value (0.0-1.0)
      REAL(r64)        :: pws     ! saturation pressure for Tdb

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

      If(Rhovapor <= 0.0d0) Then
        RHValue = 0.0d0
      Else
        pws=PsyPsatFnTemp(Tdb,'PsyRhFnTdbRhov')
        RHValue =Rhovapor*461.52d0*(Tdb+KelvinConv)/pws
      End If

#ifdef EP_psych_stats
      NumTimesCalled(iPsyRhFnTdbRhov)=NumTimesCalled(iPsyRhFnTdbRhov)+1
#endif

!                   VALIDITY TEST
      IF (RHValue < 0.0d0 .or. RHValue > 1.0d0) THEN
        IF (RHValue > 1.0d0) THEN
#ifdef EP_psych_errors
          IF (RHValue > 1.01d0) THEN
            IF (.not. WarmupFlag) THEN
              IF (iPsyErrIndex(iPsyRhFnTdbRhov) == 0) THEN
                String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//  &
                       ' Rhovapor= '//TRIM(TrimSigDigits(Rhovapor,3))// &
                       ' Calculated Relative Humidity [%]= '//TRIM(TrimSigDigits(RHValue*100.d0,2))
                CALL ShowWarningMessage('Calculated Relative Humidity out of range (PsyRhFnTdbRhov) ')
                if (present(calledfrom)) then
                  CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
                else
                  CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
                endif
                CALL ShowContinueError(TRIM(String))
                CALL ShowContinueError('Relative Humidity being reset to 100.0 %')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd('Calculated Relative Humidity out of range (PsyRhFnTdbRhov)',   &
                 iPsyErrIndex(iPsyRhFnTdbRhov),ReportMinOf=RHValue*100.d0,ReportMaxOf=RHValue*100.d0,  &
                 ReportMinUnits='%',ReportMaxUnits='%')
            ENDIF
          ENDIF
#endif
          RHValue=1.0d0
        ELSE    ! RHValue < 0.0
#ifdef EP_psych_errors
          IF (RHValue < -0.05d0) THEN
            IF (.not. WarmupFlag) THEN
              IF (iPsyErrIndex(iPsyRhFnTdbRhov) == 0) THEN
                String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//  &
                       ' Rhovapor= '//TRIM(TrimSigDigits(Rhovapor,3))// &
                       ' Calculated Relative Humidity [%]= '//TRIM(TrimSigDigits(RHValue*100.d0,2))
                CALL ShowWarningMessage('Calculated Relative Humidity out of range (PsyRhFnTdbRhov) ')
                if (present(calledfrom)) then
                  CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
                else
                  CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
                endif
                CALL ShowContinueError(TRIM(String))
                CALL ShowContinueError('Relative Humidity being reset to 1%')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd('Calculated Relative Humidity out of range (PsyRhFnTdbRhov)',   &
                 iPsyErrIndex(iPsyRhFnTdbRhov),ReportMinOf=RHValue*100.d0,ReportMaxOf=RHValue*100.d0,  &
                 ReportMinUnits='%',ReportMaxUnits='%')
            ENDIF
          ENDIF
#endif
          RHValue=.01d0
        ENDIF
      ENDIF    ! RHValue in proper range
 return
end function PsyRhFnTdbRhov

FUNCTION PsyRhFnTdbRhovLBnd0C(Tdb,Rhovapor,calledfrom) RESULT(RHValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. J. Liesen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Name change to signify derivation and temperatures were used
          !                      with 0C as minimum; LKL January 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the Relative Humidity in air as a
          ! function of dry bulb temperature and Vapor Density.

          ! METHODOLOGY EMPLOYED:
          ! ideal gas law
          ! Universal gas const for water vapor 461.52 J/(kg K)

          ! REFERENCES:
          ! ASHRAE handbook 1993 Fundamentals,

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: Tdb     ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: Rhovapor  ! vapor density in air {kg/m3}
      character(*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: RHValue ! relative humidity value (0.0-1.0)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
!      integer,save :: b0cerrcount=0

      If(Rhovapor <= 0.0d0) Then
        RHValue = 0.0d0
      Else
        RHValue =Rhovapor*461.52d0*(Tdb+KelvinConv)*Exp(-23.7093d0+4111.0d0/ &
                ((Tdb+KelvinConv)-35.45d0))
      End If

#ifdef EP_psych_stats
      NumTimesCalled(iPsyRhFnTdbRhovLBnd0C)=NumTimesCalled(iPsyRhFnTdbRhovLBnd0C)+1
#endif

!                   VALIDITY TEST
      IF (RHValue < 0.0d0 .or. RHValue > 1.0d0) THEN
        IF (RHValue > 1.0d0) THEN
#ifdef EP_psych_errors
          IF (RHValue > 1.01d0) THEN
            IF (.not. WarmupFlag) THEN
              IF (iPsyErrIndex(iPsyRhFnTdbRhovLBnd0C) == 0) THEN
                String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//  &
                       ' Rhovapor= '//TRIM(TrimSigDigits(Rhovapor,3))// &
                       ' Calculated Relative Humidity [%]= '//TRIM(TrimSigDigits(RHValue*100.d0,2))
                CALL ShowWarningMessage('Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C) ')
                if (present(calledfrom)) then
                  CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
                else
                  CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
                endif
                CALL ShowContinueError(TRIM(String))
                CALL ShowContinueError('Relative Humidity being reset to 100.0%')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd('Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C)',   &
                 iPsyErrIndex(iPsyRhFnTdbRhovLBnd0C),ReportMinOf=RHValue*100.d0,ReportMaxOf=RHValue*100.d0,  &
                 ReportMinUnits='%',ReportMaxUnits='%')
            ENDIF
          ENDIF
#endif
          RHValue=1.0d0
        ELSE    ! RHValue < 0.0
#ifdef EP_psych_errors
          IF (RHValue < -0.05d0) THEN
            IF (.not. WarmupFlag) THEN
              IF (iPsyErrIndex(iPsyRhFnTdbRhovLBnd0C) == 0) THEN
                String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//  &
                       ' Rhovapor= '//TRIM(TrimSigDigits(Rhovapor,3))// &
                       ' Calculated Relative Humidity [%]= '//TRIM(TrimSigDigits(RHValue*100.d0,2))
                CALL ShowWarningMessage('Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C) ')
                if (present(calledfrom)) then
                  CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
                else
                  CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
                endif
                CALL ShowContinueError(TRIM(String))
                CALL ShowContinueError('Relative Humidity being reset to 1%')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd('Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C)',   &
                 iPsyErrIndex(iPsyRhFnTdbRhovLBnd0C),ReportMinOf=RHValue*100.d0,ReportMaxOf=RHValue*100.d0,  &
                 ReportMinUnits='%',ReportMaxUnits='%')
            ENDIF
          ENDIF
#endif
          RHValue=.01d0
        ENDIF
      ENDIF    ! RHValue in proper range
 return
end function PsyRhFnTdbRhovLBnd0C

FUNCTION PsyRhFnTdbWPb(TDB,dW,PB,calledfrom) RESULT(RHValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Nov 1988
          !       MODIFIED       Aug 1989, Michael J. Witte
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the relative humidity value (0.0-1.0) as a result of
          ! dry-bulb temperature, humidity ratio and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK FUNDAMENTALS 1985, P6.12, EQN 10,21,23

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB   ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: dW     ! humidity ratio
      REAL(r64), intent(in) :: PB    ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: RHValue ! relative humidity value (0.0-1.0)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) U    ! Degree of Saturation
      REAL(r64) PWS  ! Pressure -- saturated for pure water
      REAL(r64) W   ! humidity ratio

        IF (PRESENT(calledfrom)) THEN
          PWS=PsyPsatFnTemp(TDB,calledfrom)
        ELSE
          PWS=PsyPsatFnTemp(TDB,'PsyRhFnTdbWPb')
        ENDIF

  !                   Find Degree Of Saturation
        W=MAX(dW,1.0d-5)
        U=W/(0.62198d0*PWS/(PB-PWS))
  !                   Calculate The Relative Humidity
        RHValue=U/(1.0d0-(1.0d0-U)*(PWS/PB))
  !
#ifdef EP_psych_stats
      NumTimesCalled(iPsyRhFnTdbWPb)=NumTimesCalled(iPsyRhFnTdbWPb)+1
#endif

  !                   VALIDITY TEST
        IF (RHValue < 0.0d0 .or. RHValue > 1.0d0) THEN
          IF (RHValue > 1.0d0) THEN
#ifdef EP_psych_errors
            IF (RHValue > 1.01d0) THEN
              IF (.not. WarmupFlag) THEN
                IF (iPsyErrIndex(iPsyRhFnTdbWPb) == 0) THEN
                  String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//  &
                         ' Humidity Ratio= '//TRIM(TrimSigDigits(W,3))// &
                         ' Calculated Relative Humidity [%]= '//TRIM(TrimSigDigits(RHValue*100.d0,2))
                  CALL ShowWarningMessage('Calculated Relative Humidity out of range (PsyRhFnTdbWPb) ')
                  if (present(calledfrom)) then
                    CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
                  else
                    CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
                  endif
                  CALL ShowContinueError(TRIM(String))
                  CALL ShowContinueError('Relative Humidity being reset to 100.0%')
                ENDIF
                CALL ShowRecurringWarningErrorAtEnd('Calculated Relative Humidity out of range (PsyRhFnTdbWPb)',   &
                  iPsyErrIndex(iPsyRhFnTdbWPb),ReportMinOf=RHValue*100.d0,ReportMaxOf=RHValue*100.d0,  &
                  ReportMinUnits='%',ReportMaxUnits='%')
              ENDIF
            ENDIF
#endif
            RHValue=1.0d0
          ELSE    ! RHValue < 0.0
#ifdef EP_psych_errors
            IF (RHValue < -0.05d0) THEN
              IF (.not. WarmupFlag) THEN
                IF (iPsyErrIndex(iPsyRhFnTdbWPb) == 0) THEN
                  String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//  &
                         ' Humidity Ratio= '//TRIM(TrimSigDigits(W,3))// &
                         ' Calculated Relative Humidity [%]= '//TRIM(TrimSigDigits(RHValue*100.d0,2))
                  CALL ShowWarningMessage('Calculated Relative Humidity out of range (PsyRhFnTdbWPb) ')
                  if (present(calledfrom)) then
                    CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
                  else
                    CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
                  endif
                  CALL ShowContinueError(TRIM(String))
                  CALL ShowContinueError('Relative Humidity being reset to 1%')
                ENDIF
                CALL ShowRecurringWarningErrorAtEnd('Calculated Relative Humidity out of range (PsyRhFnTdbWPb)',   &
                  iPsyErrIndex(iPsyRhFnTdbWPb),ReportMinOf=RHValue*100.d0,ReportMaxOf=RHValue*100.d0,  &
                  ReportMinUnits='%',ReportMaxUnits='%')
              ENDIF
            ENDIF
#endif
            RHValue=.01d0
          ENDIF
        ENDIF    ! RHValue in proper range

     ! RHValue is the result

    RETURN
END FUNCTION PsyRhFnTdbWPb

#ifdef EP_cache_PsyTwbFnTdbWPb
FUNCTION PsyTwbFnTdbWPb(Tdb,W,Pb,calledfrom)  RESULT (Twb_result)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie/Amir Roth
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provide a "cache" of results for the given arguments and wetbulb (twb) output result.

          ! METHODOLOGY EMPLOYED:
          ! Use grid shifting and masking to provide hash into the cache. Use Equivalence to
          ! make Fortran ignore "types".

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), intent(in) :: Tdb    ! dry-bulb temperature {C}
  REAL(r64), intent(in) :: W      ! humidity ratio
  REAL(r64), intent(in) :: Pb     ! barometric pressure {Pascals}
  character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)        :: Twb_result    ! result=> Temperature Wet-Bulb {C}

          ! FUNCTION PARAMETER DEFINITIONS:
  integer(i64), parameter :: Grid_Shift=(64 - 12 - twbPrecision_bits)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  integer(i64) :: Tdb_tag, W_tag, Pb_tag
  integer(i64) :: hash
  real(r64) :: Tdb_tag_r, w_tag_r, Pb_tag_r

#ifdef EP_psych_stats
      NumTimesCalled(iPsyTwbFnTdbWPb_cache)=NumTimesCalled(iPsyTwbFnTdbWPb_cache)+1
#endif


  Tdb_tag = Transfer(Tdb,Tdb_tag)
  W_tag = Transfer(W,W_tag)
  Pb_tag = Transfer(Pb,Pb_tag)

  !Both Intel & GNU are happy to use .iand.
  Tdb_tag = ISHFT(Tdb_tag, -Grid_shift)
  W_tag = ISHFT(W_tag, -Grid_shift)
  Pb_tag = ISHFT(Pb_tag, -Grid_shift)
  hash = IAND(IEOR(Tdb_tag,IEOR(W_tag,Pb_tag)), INT(twbcache_size - 1,i64))

  IF (cached_Twb(hash)%iTdb /= Tdb_tag .OR. cached_Twb(hash)%iW /= W_tag .OR. cached_Twb(hash)%iPb /= Pb_tag) THEN
    cached_Twb(hash)%iTdb = Tdb_tag
    cached_Twb(hash)%iW = W_tag
    cached_Twb(hash)%iPb = Pb_tag

    Tdb_tag_r = Transfer(ISHFT(Tdb_tag, Grid_shift), Tdb_tag_r)
    W_tag_r = Transfer(ISHFT(W_tag, Grid_shift), W_tag_r)
    Pb_tag_r = Transfer(ISHFT(Pb_tag, Grid_shift), Pb_tag_r)

    IF (PRESENT(calledfrom)) THEN
      cached_Twb(hash)%Twb = PsyTwbFnTdbWPb_raw(Tdb_tag_r, W_tag_r, Pb_tag_r,calledfrom)
    ELSE
      cached_Twb(hash)%Twb = PsyTwbFnTdbWPb_raw(Tdb_tag_r, W_tag_r, Pb_tag_r)
    ENDIF
  END IF

!  Twbresult_last = cached_Twb(hash)%Twb
!  Twb_result = Twbresult_last
  Twb_result = cached_Twb(hash)%Twb

  RETURN

END FUNCTION PsyTwbFnTdbWPb


FUNCTION PsyTwbFnTdbWPb_raw(TDB,dW,Patm,calledfrom) RESULT(TWB)
#else
FUNCTION PsyTwbFnTdbWPb(TDB,dW,Patm,calledfrom) RESULT(TWB)
#endif

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  Dec 2003; Rahul Chillar
          !                      2011; as time saving measure, cache some values.

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the wet-bulb temperature from dry-bulb temperature,
          ! humidity ratio and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! Uses an Iterative procedure to calculate WetBulbTemperature

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: Iterate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB    ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: dW      ! humidity ratio
      REAL(r64), intent(in) :: Patm   ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: TWB    ! result=> Temperature Wet-Bulb {C}

          ! FUNCTION PARAMETER DEFINITIONS:
      Integer, Parameter :: itmax=100 ! Maximum No of Iterations
      REAL(r64) :: convTol=0.0001d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64):: tBoil        ! Boiling temperature of water at given pressure
      REAL(r64), SAVE  :: last_Patm=-99999.d0   ! barometric pressure {Pascals}  (last)
      REAL(r64), SAVE  :: last_tBoil=-99999.d0       ! Boiling temperature of water at given pressure (last)
      REAL(r64):: newW         ! Humidity ratio calculated with wet bulb guess
      REAL(r64):: W            ! Humidity ratio entered and corrected as necessary
      REAL(r64):: ResultX      ! ResultX is the final Iteration result passed back to the calling routine
      REAL(r64):: WBT          ! Current Value of WetBulbTemperature
      REAL(r64):: error        ! Deviation of dependent variable in iteration
      REAL(r64):: X1           ! Independent variable in ITERATE
      REAL(r64):: Y1           ! Dependent variable in ITERATE
      REAL(r64):: Wstar        !Humidity  ratio as a function of Sat Press of Wet Bulb
      REAL(r64):: PSatstar     !Saturation pressure at wet bulb temperature
      Integer:: iter      ! Iteration counter
      Integer:: icvg      ! Iteration convergence flag
      LOGICAL FlagError   ! set when errors should be flagged

#ifdef EP_psych_stats
      NumTimesCalled(iPsyTwbFnTdbWPb)=NumTimesCalled(iPsyTwbFnTdbWPb)+1
#endif

         ! CHECK TDB IN RANGE.
      FlagError=.false.
#ifdef EP_psych_errors
      IF (TDB <= -100.0d0 .or. TDB >= 200.0d0) THEN
        IF (.not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyTwbFnTdbWPb) == 0) THEN
            CALL ShowWarningMessage('Temperature out of range [-100. to 200.] (PsyTwbFnTdbWPb)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            CALL ShowContinueError(' Input Temperature='//TRIM(TrimSigDigits(TDB,2)))
            FlagError=.true.
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('Temperature out of range [-100. to 200.] (PsyTwbFnTdbWPb)',   &
            iPsyErrIndex(iPsyTwbFnTdbWPb),ReportMinOf=TDB,ReportMaxOf=TDB,ReportMinUnits='C',ReportMaxUnits='C')
        ENDIF
      ENDIF
#endif

      W=dW
      IF (W < 0.0d0) THEN
#ifdef EP_psych_errors
        IF (W <= -.0001d0) THEN
          IF (.not. WarmupFlag) THEN
            IF (iPsyErrIndex(iPsyTwbFnTdbWPb2) == 0) THEN
              String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//' Humidity Ratio= '//TRIM(TrimSigDigits(W,3))//  &
                                ' Pressure= '//TRIM(TrimSigDigits(Patm,2))
              CALL ShowWarningMessage('Entered Humidity Ratio invalid (PsyTwbFnTdbWPb)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              CALL ShowContinueError(TRIM(String))
              String='Humidity Ratio= '//TRIM(TrimSigDigits(W,4))
              CALL ShowContinueError(TRIM(String)//' ... Humidity Ratio set to .00001')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Entered Humidity Ratio invalid (PsyTwbFnTdbWPb)',   &
              iPsyErrIndex(iPsyTwbFnTdbWPb2),ReportMinOf=W,ReportMaxOf=W,ReportMinUnits='[]',ReportMaxUnits='[]')
          ENDIF
        ENDIF
#endif
        W=1.d-5
      ENDIF

       ! Initial temperature guess at atmospheric pressure
      IF (PRESENT(calledfrom)) THEN
        IF (Patm /= last_Patm) THEN
          tBoil = PsyTsatFnPb(Patm,calledfrom)
          last_Patm=Patm
          last_tBoil=tBoil
        ELSE
          tBoil = last_tBoil
        ENDIF
      ELSE
        IF (Patm /= last_Patm) THEN
          tBoil = PsyTsatFnPb(Patm,'PsyTwbFnTdbWPb')
          last_Patm=Patm
          last_tBoil=tBoil
        ELSE
          tBoil = last_tBoil
        ENDIF
      ENDIF

       ! Set initial guess of WetBulbTemp=Entering Dry Bulb Temperature
      WBT = TDB

       ! Initializing  value for iter
      iter=0

       ! Begin iteration loop
      DO iter = 1,itmax

         ! Assigning a value to WBT
        IF (WBT .GE. (tBoil-0.09d0) ) WBT = tBoil-0.1d0

         ! Determine the saturation pressure for wet bulb temperature
        IF (PRESENT(calledfrom)) THEN
          PSatstar=PsyPsatFnTemp(WBT,calledfrom)
        ELSE
          PSatstar=PsyPsatFnTemp(WBT,'PsyTwbFnTdbWPb')
        ENDIF

         ! Determine humidity ratio for given saturation pressure
        Wstar=0.62198d0*PSatstar/(Patm-PSatstar)

         ! Calculate new humidity ratio and determine difference from known
         ! humidity ratio which is wStar calculated earlier
        newW=((2501.0d0-2.381d0*WBT)*Wstar-(TDB-WBT)) /          &
          (2501.0d0+1.805d0*TDB-4.186d0*WBT)

         ! Check error, if not satisfied, calculate new guess and iterate
        error = W-newW

         ! Using Iterative Procedure to Calculate WetBulb
        Call ITERATE(ResultX,convTol,WBT,error,X1,Y1,iter,icvg)
        WBT=ResultX

        ! If converged, leave iteration loop.
        IF (icvg .EQ. 1) Exit

        ! Error Trap for the Discontinious nature of PsyPsatFnTemp function (Sat Press Curve) at ~0 Deg C.
        IF((PSatstar > 611.000d0) .and. (PSatstar < 611.25d0) .and. (abs(error) <= 0.0001d0) .and. (iter > 4)) Exit

      End Do  ! End of Iteration Loop

#ifdef EP_psych_stats
      NumIterations(iPsyTwbFnTdbWPb)=NumIterations(iPsyTwbFnTdbWPb)+iter
#endif

        ! Wet bulb temperature has not converged after maximum specified
        ! iterations. Print error message, set return error flag, and RETURN
#ifdef EP_psych_errors
      IF (iter > itmax) THEN
        IF (.not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyTwbFnTdbWPb3) == 0) THEN
            CALL ShowWarningMessage('WetBulb not converged after '//TRIM(TrimSigDigits(iter))// ' iterations(PsyTwbFnTdbWPb)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            CALL ShowContinueError(' Input Temperature = '//TRIM(TrimSigDigits(TDB,2)))
            CALL ShowContinueError(' Input Humidity Ratio= '//TRIM(TrimSigDigits(W,6)))
            CALL ShowContinueError(' Input Pressure = '//TRIM(TrimSigDigits(Patm,2)))
            FlagError=.true.
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('WetBulb not converged after max iterations(PsyTwbFnTdbWPb)',   &
              iPsyErrIndex(iPsyTwbFnTdbWPb3))
        ENDIF
      ENDIF
#endif

       !Result is Temperature Wet Bulb
      TWB=WBT

#ifdef EP_psych_errors
      IF (FlagError) THEN
       CALL ShowContinueError(' Resultant Temperature= '//TRIM(TrimSigDigits(WBT,2)))
      ENDIF
#endif

       ! If (TempWetBulb)>(Dry Bulb Temp) , Setting (TempWetBulb)=(DryBulbTemp).
      IF (TWB .GT. TDB)  Then
        TWB = TDB
      End IF

#ifdef generatetestdata
   write(outputfiledebug,*) Tdb,dW,Patm,Twb
#endif

   RETURN

#ifdef EP_cache_PsyTwbFnTdbWPb
END FUNCTION PsyTwbFnTdbWPb_raw
#else
END FUNCTION PsyTwbFnTdbWPb
#endif


FUNCTION PsyVFnTdbWPb(TDB,dW,PB,calledfrom) RESULT(V)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the specific volume from dry-bulb temperature,
          ! humidity ratio and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 28

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB    ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: dW      ! humidity ratio
      REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: V      ! result=> specific volume {m3/kg}


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) w  ! humidity ratio

      w=MAX(dW,1.0d-5)
      V=1.59473d2*(1.d0+1.6078d0*W)*(1.8d0*TDB+492.d0)/PB

#ifdef EP_psych_stats
      NumTimesCalled(iPsyVFnTdbWPb)=NumTimesCalled(iPsyVFnTdbWPb)+1
#endif

!                                      VALIDITY TEST.
      IF (V < 0.0d0) THEN
#ifdef EP_psych_errors
        IF (V <= -.01d0) THEN
          IF (.not. WarmupFlag) THEN
            IF (iPsyErrIndex(iPsyVFnTdbWPb) == 0) THEN
              String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//' Humidity Ratio= '//TRIM(TrimSigDigits(W,3))//  &
                                ' Pressure= '//TRIM(TrimSigDigits(PB,2))
              CALL ShowWarningMessage('Calculated Specific Volume out of range (PsyVFnTdbWPb)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              CALL ShowContinueError(TRIM(String))
              String='Calculated Volume= '//TRIM(TrimSigDigits(V,3))
              CALL ShowContinueError(TRIM(String)//' ... Since Calculated Volume < 0.0, it is set to .83')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Calculated Specific Volume out of range (PsyVFnTdbWPb)',   &
              iPsyErrIndex(iPsyVFnTdbWPb),ReportMinOf=V,ReportMaxOf=V,ReportMinUnits='m3/kg',ReportMaxUnits='m3/kg')
          ENDIF
        ENDIF
        V=0.83d0
#endif
      ENDIF

  ! V is the result

  RETURN
END FUNCTION PsyVFnTdbWPb


FUNCTION PsyWFnTdpPb(TDP,PB,calledfrom) RESULT(W)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the humidity ratio from dew-point temperature
          ! and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDP    ! dew-point temperature {C}
      REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: W      ! result=> humidity ratio

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) PDEW ! saturation pressure at dew-point temperature {Pascals}

#ifdef EP_psych_stats
      NumTimesCalled(iPsyWFnTdpPb)=NumTimesCalled(iPsyWFnTdpPb)+1
#endif

      IF (PRESENT(calledfrom)) THEN
        PDEW=PsyPsatFnTemp(TDP,calledfrom)
      ELSE
        PDEW=PsyPsatFnTemp(TDP,'PsyWFnTdpPb')
      ENDIF
      W=PDEW*0.62198d0/(PB-PDEW)
!                                      VALIDITY TEST.
      IF (W < 0.0d0) THEN
#ifdef EP_psych_errors
        IF (W <= -.0001d0) THEN
          IF (.not. WarmupFlag) THEN
            IF (iPsyErrIndex(iPsyWFnTdpPb) == 0) THEN
              String=' Dew-Point= '//TRIM(TrimSigDigits(TDP,2))//' Pressure= '//TRIM(TrimSigDigits(PB,2))
              CALL ShowWarningMessage('Calculated Humidity Ratio invalid (PsyWFnTdpPb)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              CALL ShowContinueError(TRIM(String))
              String='Calculated Humidity Ratio= '//TRIM(TrimSigDigits(W,4))
              CALL ShowContinueError(TRIM(String)//' ... Humidity Ratio set to .00001')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Entered Humidity Ratio invalid (PsyWFnTdpPb)',   &
              iPsyErrIndex(iPsyWFnTdpPb),ReportMinOf=W,ReportMaxOf=W,ReportMinUnits='[]',ReportMaxUnits='[]')
          ENDIF
        ENDIF
#endif
        W=1.d-5
      ENDIF

      ! W is the result

  RETURN
END FUNCTION PsyWFnTdpPb


FUNCTION PsyWFnTdbH(TDB,H,calledfrom, SuppressWarnings) RESULT(W)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the humidity ratio from dry-bulb temperature
          ! and enthalpy.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB    ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: H      ! enthalpy {J/kg}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      LOGICAL, INTENT(IN), OPTIONAL :: SuppressWarnings     ! if calling function is calculating an intermediate state 
      REAL(r64)        :: W      ! result=> humidity ratio

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL  :: ReportWarnings

  ReportWarnings = .TRUE.

  IF (PRESENT(SuppressWarnings)) THEN
    IF (SuppressWarnings) ReportWarnings = .FALSE.
  ENDIF

!CP-------- here is 1.2, 1200., 1.004, or 1004.  --------
      W=(H-1.00484d3*TDB)/(2.50094d6+1.85895d3*TDB)
!
#ifdef EP_psych_stats
      NumTimesCalled(iPsyWFnTdbH)=NumTimesCalled(iPsyWFnTdbH)+1
#endif

!                                      VALIDITY TEST.
      IF (W < 0.0d0) THEN
#ifdef EP_psych_errors
        IF (W < -.0001d0) THEN
          IF (.not. WarmupFlag .AND.  ReportWarnings) THEN
            IF (iPsyErrIndex(iPsyWFnTdbH) == 0) THEN
              String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//' Enthalpy= '//TRIM(TrimSigDigits(H,3))
              CALL ShowWarningMessage('Calculated Humidity Ratio invalid (PsyWFnTdbH)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              CALL ShowContinueError(TRIM(String))
              String='Calculated Humidity Ratio= '//TRIM(TrimSigDigits(W,4))
              CALL ShowContinueError(TRIM(String)//' ... Humidity Ratio set to .00001')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Calculated Humidity Ratio invalid (PsyWFnTdbH)',   &
              iPsyErrIndex(iPsyWFnTdbH),ReportMinOf=W,ReportMaxOf=W,ReportMinUnits='[]',ReportMaxUnits='[]')
          ENDIF
        ENDIF
#endif
        W=1.d-5
      ENDIF

      ! W is the result

  RETURN
END FUNCTION PsyWFnTdbH


FUNCTION PsyWFnTdbTwbPb(TDB,TWBin,PB,calledfrom) RESULT(W)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the humidity ratio from dry-bulb temperature,
          ! wet-bulb temperature and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQ 22,35

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB    ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: TWBin  ! wet-bulb temperature {C}
      REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: W      ! result=> humidity ratio

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) PWET  ! Pressure at wet-bulb temperature {Pascals}
      REAL(r64) WET   ! Humidity ratio at wet-bulb temperature
      REAL(r64) TWB   ! test wet-bulb temperature


#ifdef EP_psych_stats
      NumTimesCalled(iPsyWFnTdbTwbPb)=NumTimesCalled(iPsyWFnTdbTwbPb)+1
#endif

!                                      VALIDITY CHECK.
      TWB=TWBin
      IF (TWB > TDB) THEN
#ifdef EP_psych_errors
        IF (TWB > (TDB+0.01d0)) THEN
          IF (ReportErrors .and. .not. WarmupFlag) THEN
            IF (iPsyErrIndex(iPsyWFnTdbTwbPb) == 0) THEN
              String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//' Pressure= '//TRIM(TrimSigDigits(PB,2))
              CALL ShowWarningMessage('Given Wet Bulb Temperature invalid (PsyWFnTdbTwbPb)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              CALL ShowContinueError(TRIM(String))
              String='Calculated Wet-Bulb= '//TRIM(TrimSigDigits(TWB,2))
              CALL ShowContinueError(TRIM(String)//' ... Since Dry Bulb < Wet Bulb, Wet Bulb set = to Dry Bulb')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Given Wet Bulb Temperature invalid (PsyWFnTdbTwbPb)',   &
              iPsyErrIndex(iPsyWFnTdbTwbPb),ReportMinOf=TWB,ReportMaxOf=TWB,ReportMinUnits='C',ReportMaxUnits='C')
          ENDIF
        ENDIF
#endif
        TWB=TDB
      ENDIF
!                                      CALCULATION.
      IF (PRESENT(calledfrom)) THEN
        PWET=PsyPsatFnTemp(TWB,calledfrom)
      ELSE
        PWET=PsyPsatFnTemp(TWB,'PsyWFnTdbTwbPb')
      ENDIF
      WET=0.62198d0*PWET/(PB-PWET)

      W=((2501.0d0-2.381d0*TWB)*WET-(TDB-TWB)) /                  &
       (2501.0d0+1.805d0*TDB-4.186d0*TWB)
!                                      VALIDITY CHECK.
      IF (W < 0.0d0) THEN
#ifdef EP_psych_errors
        IF (ReportErrors .and. .not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyWFnTdbTwbPb2) == 0) THEN
            String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//' Wet-Bulb= '//TRIM(TrimSigDigits(TWB,2))//  &
                   ' Pressure= '//TRIM(TrimSigDigits(PB,2))
            CALL ShowWarningMessage('Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            CALL ShowContinueError(TRIM(String))
            String='Calculated Humidity Ratio= '//TRIM(TrimSigDigits(W,4))//', will recalculate Humidity Ratio'
            CALL ShowContinueError(TRIM(String)//' using Relative Humidity .01% (and Dry-Bulb and Pressure as shown)')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)',   &
            iPsyErrIndex(iPsyWFnTdbTwbPb2),ReportMinOf=W,ReportMaxOf=W,ReportMinUnits='[]',ReportMaxUnits='[]')
        ENDIF
#endif
        W=PsyWFnTdbRhPb(TDB,.0001d0,PB,calledfrom)
      ENDIF

  ! W is the result

  RETURN
END FUNCTION PsyWFnTdbTwbPb


FUNCTION PsyWFnTdbRhPb(TDB,RH,PB,calledfrom) RESULT(W)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the humidity ratio from dry-bulb temperature,
          ! relative humidty (value) and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: TDB    ! dry-bulb temperature {C}
      REAL(r64), intent(in) :: RH     ! relative humidity value (0.0-1.0)
      REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: W      ! result=> humidity ratio

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) PDRY  ! Pressure at dry-bulb temperature {Pascals}
      REAL(r64) PDEW  ! Pressure at dew-point temperature {Pascals}

#ifdef EP_psych_stats
      NumTimesCalled(iPsyWFnTdbRhPb)=NumTimesCalled(iPsyWFnTdbRhPb)+1
#endif

      IF (PRESENT(calledfrom)) THEN
        PDRY=PsyPsatFnTemp(TDB,calledfrom)
      ELSE
        PDRY=PsyPsatFnTemp(TDB,'PsyWFnTdbRhPb')
      ENDIF

      PDEW=RH*PDRY

      !Numeric error check when the temperature and RH values cause Pdew to equal or exceed
      !barometric pressure which is physically impossible. An approach limit of 1000 pascals
      !was chosen to keep the numerics stable as the denominator approaches 0.
      If((PB-PDEW) <= 1000.0d0) Then
         W=PDEW*0.62198d0/1000.0d0
      Else
         W=PDEW*0.62198d0/(PB-PDEW)
      End If

!                                      THIS EQUATION IN SI UNIT IS FROM
!                                      VALIDITY TEST.
!                                      ASHRAE HANDBOOK OF FUNDAMENTALS
!                                      PAGE 99  EQUATION 22
      IF (W < 1.0d-5) THEN
#ifdef EP_psych_errors
        IF (W <= -.0001d0) THEN
          IF (.not. WarmupFlag) THEN
            IF (iPsyErrIndex(iPsyWFnTdbRhPb) == 0) THEN
              String=' Dry-Bulb= '//TRIM(TrimSigDigits(TDB,2))//' Relative Humidity [%]= '//  &
                     TRIM(TrimSigDigits(RH*100.d0,2))//' Pressure= '//TRIM(TrimSigDigits(PB,2))
              CALL ShowWarningMessage('Calculated Humidity Ratio is invalid (PsyWFnTdbRhPb)')
              if (present(calledfrom)) then
                CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
              else
                CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
              endif
              CALL ShowContinueError(TRIM(String))
              String='Calculated Humidity Ratio= '//TRIM(TrimSigDigits(W,4))
              CALL ShowContinueError(TRIM(String)//' ... Humidity Ratio set to .00001')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd('Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)',   &
              iPsyErrIndex(iPsyWFnTdbRhPb),ReportMinOf=W,ReportMaxOf=W,ReportMinUnits='[]',ReportMaxUnits='[]')
          ENDIF
        ENDIF
#endif
        W=1.d-5
      ENDIF

   ! W is the result

  RETURN
END FUNCTION PsyWFnTdbRhPb


#ifdef EP_cache_PsyPsatFnTemp
FUNCTION PsyPsatFnTemp(T,calledfrom) RESULT(Pascal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provide a "cache" of results for the given argument (T) and pressure (Pascal) output result.

          ! METHODOLOGY EMPLOYED:
          ! Use grid shifting and masking to provide hash into the cache. Use Equivalence to
          ! make Fortran ignore "types".

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), intent(in) :: T      ! dry-bulb temperature {C}
  character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
  REAL(r64)             :: Pascal ! result=> saturation pressure {Pascals}

          ! FUNCTION PARAMETER DEFINITIONS:
  integer(i64), parameter :: Grid_Shift=(64 - 12 - psatPrecision_bits)
!  integer(i64), parameter :: Grid_Mask=NOT(ISHFT(1_i64, Grid_Shift)-1)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  integer(i64) :: Tdb_tag
  integer(i64) :: hash
  real(r64) :: Tdb_tag_r

#ifdef EP_psych_stats
      NumTimesCalled(iPsyPsatFnTemp_cache)=NumTimesCalled(iPsyPsatFnTemp_cache)+1
#endif

  Tdb_tag=Transfer(T,Tdb_tag)

  !Both Intel & GNU are happy to use .iand.
  Tdb_tag = ISHFT(Tdb_tag, -Grid_Shift)
  hash = IAND(Tdb_tag, INT(psatcache_size - 1,i64))

  IF (cached_psat(hash)%iTdb /= Tdb_tag) THEN
    cached_Psat(hash)%iTdb = Tdb_tag
    Tdb_tag_r = Transfer(ISHFT(Tdb_tag, Grid_shift), Tdb_tag_r)

    IF (PRESENT(calledfrom)) THEN
      cached_Psat(hash)%Psat = PsyPsatFnTemp_raw(Tdb_tag_r,calledfrom)
    ELSE
      cached_Psat(hash)%Psat = PsyPsatFnTemp_raw(Tdb_tag_r)
    ENDIF
  END IF

  Pascal = cached_Psat(hash)%Psat

  RETURN

END FUNCTION PsyPsatFnTemp


FUNCTION PsyPsatFnTemp_raw(T,calledfrom) RESULT(Pascal)
#else
FUNCTION PsyPsatFnTemp(T,calledfrom) RESULT(Pascal)
#endif
          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       NA
          !       RE-ENGINEERED  Nov 2003; Rahul Chillar

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the saturation pressure as a function of temperature.

          ! METHODOLOGY EMPLOYED:
          ! Hyland & Wexler Formulation, range -100C to 200C

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 2005, Chap 6 (Psychrometrics), Eqn 5 & 6.
          ! Compared to Table 3 values (August 2007) with average error of 0.00%, max .30%,
          ! min -.39%.  (Spreadsheet available on request - Lawrie).

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: T      ! dry-bulb temperature {C}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: Pascal ! result=> saturation pressure {Pascals}

          ! FUNCTION PARAMETER DEFINITIONS:
      REAL(r64), PARAMETER   :: C1 = -5674.5359d0      ! Coefficient for TKel < KelvinConvK
      REAL(r64), PARAMETER   :: C2 =  6.3925247d0      ! Coefficient for TKel < KelvinConvK
      REAL(r64), PARAMETER   :: C3 = -0.9677843d-2     ! Coefficient for TKel < KelvinConvK
      REAL(r64), PARAMETER   :: C4 =  0.62215701d-6    ! Coefficient for TKel < KelvinConvK
      REAL(r64), PARAMETER   :: C5 =  0.20747825d-8    ! Coefficient for TKel < KelvinConvK
      REAL(r64), PARAMETER   :: C6 = -0.9484024d-12    ! Coefficient for TKel < KelvinConvK
      REAL(r64), PARAMETER   :: C7 =  4.1635019d0      ! Coefficient for TKel < KelvinConvK

      REAL(r64), PARAMETER   :: C8  =  -5800.2206d0    ! Coefficient for TKel >= KelvinConvK
      REAL(r64), PARAMETER   :: C9  =   1.3914993d0    ! Coefficient for TKel >= KelvinConvK
      REAL(r64), PARAMETER   :: C10 =  -0.048640239d0  ! Coefficient for TKel >= KelvinConvK
      REAL(r64), PARAMETER   :: C11 =   0.41764768d-4  ! Coefficient for TKel >= KelvinConvK
      REAL(r64), PARAMETER   :: C12 =  -0.14452093d-7  ! Coefficient for TKel >= KelvinConvK
      REAL(r64), PARAMETER   :: C13 =   6.5459673d0    ! Coefficient for TKel >= KelvinConvK
#ifdef EP_IF97
      !Table 34 in IF97
      REAL(r64), PARAMETER   :: N1  =  0.11670521452767d04
      REAL(r64), PARAMETER   :: N2  = -0.72421316703206d06
      REAL(r64), PARAMETER   :: N3  = -0.17073846940092d02
      REAL(r64), PARAMETER   :: N4  =  0.12020824702470d05
      REAL(r64), PARAMETER   :: N5  = -0.32325550322333d07
      REAL(r64), PARAMETER   :: N6  =  0.14915108613530d02
      REAL(r64), PARAMETER   :: N7  = -0.48232657361591d04
      REAL(r64), PARAMETER   :: N8  =  0.40511340542057d06
      REAL(r64), PARAMETER   :: N9  = -0.23855557567849d0
      REAL(r64), PARAMETER   :: N10 =  0.65017534844798d03
#endif

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64)   :: Tkel  ! Dry-bulb in REAL(r64) for function passing
#ifdef EP_IF97
      REAL(r64)   :: phi   ! IF97 equation 29b
      REAL(r64)   :: phi2  ! phi squared
      REAL(r64)   :: A     ! IF97 equation 30
      REAL(r64)   :: B     ! IF97 equation 30
      REAL(r64)   :: C     ! IF97 equation 30
#endif


#ifdef EP_psych_stats
      NumTimesCalled(iPsyPsatFnTemp)=NumTimesCalled(iPsyPsatFnTemp)+1
#endif

       ! CHECK T IN RANGE.
#ifdef EP_psych_errors
      IF (T <= -100.0d0 .or. T >= 200.0d0) THEN
        IF (.not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyPsatFnTemp) == 0) THEN
            CALL ShowWarningMessage('Temperature out of range [-100. to 200.] (PsyPsatFnTemp)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            CALL ShowContinueError(' Input Temperature='//TRIM(TrimSigDigits(T,2)))
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('Temperature out of range [-100. to 200.] (PsyPsatFnTemp)',   &
            iPsyErrIndex(iPsyPsatFnTemp),ReportMinOf=T,ReportMaxOf=T,ReportMinUnits='C',ReportMaxUnits='C')
        ENDIF
      ENDIF
#endif

       ! Convert temperature from Centigrade to Kelvin.
      TKel = T + KelvinConv

       ! If below freezing, calculate saturation pressure over ice.
      IF ((TKel .LT. KelvinConv) .AND. (TKel .GE. 173.15d0)) THEN
       Pascal = EXP(C1/TKel+C2+TKel*(C3+TKel*(C4+TKel*(C5+C6*TKel)))+C7*LOG(TKel))

       ! If below -100C,set value of Pressure corresponding to Saturation Temperature of -100C.
      ELSE IF ((TKel .Lt. 173.15d0)) THEN
         Pascal = 0.0017d0

       ! If above freezing, calculate saturation pressure over liquid water.
      ELSE IF ((TKel .GE. KelvinConv) .AND. (TKel .LE. 473.15d0)) THEN
#ifndef EP_IF97
         Pascal = EXP(C8/TKel+C9+TKel*(C10+TKel*(C11+TKel*C12))+C13*LOG(TKel))
#else
!         !IF97 equations
         phi =  TKel + n9 /(TKel - n10)
         phi2 = phi * phi
         A =      phi2 + n1 * phi + n2
         B = n3 * phi2 + n4 * phi + n5
         C = n6 * phi2 + n7 * phi + n8
         Pascal = 1000000.d0 * ((2.d0 * C) / (-B + SQRT(B**2 - 4.d0 * A * C)))**4
#endif
       ! If above 200C, set value of Pressure corresponding to Saturation Temperature of 200C.
      ELSE IF ((TKel .GT. 473.15d0)) THEN
         Pascal = 1555000.0d0

      ELSE
         ! bad temperature.  Use 0.0 C
#ifdef EP_psych_errors
         CALL ShowSevereError('PsyPsatFnTemp -- Bad input temperature='//TRIM(TrimSigDigits(T,2)))
         if (present(calledfrom)) then
           CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
         else
           CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
         endif
         CALL ShowFatalError(' Program terminates due to preceding conditions.')
#else
         STOP 'PsyPsatFnTemp'
#endif
      ENDIF


  RETURN

#ifdef EP_cache_PsyPsatFnTemp
END FUNCTION PsyPsatFnTemp_raw
#else
END FUNCTION PsyPsatFnTemp
#endif

FUNCTION PsyTsatFnHPb(H,PB,calledfrom) RESULT(T)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       MODIFIED       July 2003; LKL -- peg min/max values (outside range of functions)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the saturation temperature from the enthalpy
          ! and barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: H      ! enthalpy {J/kg}
      REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: T      ! result=> saturation temperature {C}

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) T1  ! APPROXIMATE SATURATION TEMPERATURE (C)
      REAL(r64) T2  ! APPROXIMATE SATURATION TEMPERATURE (C)
      REAL(r64) TN  ! NEW ASSUMED SATURATION TEMPERATURE (C)
      REAL(r64) H1  ! APPROXIMATE ENTHALPY (J/KG)
      REAL(r64) H2  ! APPROXIMATE ENTHALPY (J/KG)
      REAL(r64) Y1  ! ERROR IN ENTHALPY
      REAL(r64) Y2  ! ERROR IN ENTHALPY
      INTEGER  IterCount
      REAL(r64) HH  ! temporary enthalpy (calculation) value
      LOGICAL FlagError  ! Set when errors should be flagged
      REAL(r64)      :: Hloc ! local value of H


!                                      CHECK H IN RANGE.
      HH = H + 1.78637d4

      IF (H >= 0.0d0) THEN
        Hloc = MAX(0.00001d0,H)
      ELSE IF (H < 0.0d0) THEN
        Hloc = MIN(-.00001d0,H)
      END IF

#ifdef EP_psych_stats
      NumTimesCalled(iPsyTsatFnHPb)=NumTimesCalled(iPsyTsatFnHPb)+1
#endif

      FlagError=.false.
#ifdef EP_psych_errors
      IF (HH <= -4.24D4 .or. HH >= 4.5866D7) THEN
        IF (.not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyTsatFnHPb) == 0) THEN
            CALL ShowWarningMessage('Enthalpy out of range (PsyTsatFnHPb)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            String=' Enthalpy='//TRIM(TrimSigDigits(HH,5))//' Pressure= '//TRIM(TrimSigDigits(PB,2))
            CALL ShowContinueError(TRIM(String))
            FlagError=.true.
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('Enthalpy out of range (PsyTsatFnHPb)',   &
            iPsyErrIndex(iPsyTsatFnHPb),ReportMinOf=HH,ReportMaxOf=HH,ReportMinUnits='J/kg',ReportMaxUnits='J/kg')
        ENDIF
      ENDIF
#endif
!
      IF (HH > 7.5222d4) GO TO 20
      IF (HH > 2.7297d4) GO TO 60
      IF (HH > -6.7012d2) GO TO 50
      IF (HH > -2.2138d4) GO TO 40
      IF (HH < -4.24d4) HH=-4.24d4   ! Peg to minimum
      GO TO 30
   20 CONTINUE
      IF (HH < 1.8379d5) GO TO 70
      IF (HH < 4.7577d5) GO TO 80
      IF (HH < 1.5445d6) GO TO 90
      IF (HH < 3.8353d6) GO TO 100
      IF (HH > 4.5866d7) HH=4.5866d7   ! Peg to maximum
      GO TO 110
!
!                                      TEMP. IS FROM -60 C  TO  -40 C
   30 CONTINUE
      T=F6(HH,-19.44d0,8.53675d-4,-5.12637d-9,-9.85546d-14,-1.00102d-18,-4.2705d-24)
      GO TO 120
!                                      TEMP. IS FROM -40 C  TO  -20 C
   40 CONTINUE
      T=F6(HH,-1.94224d1,8.5892d-4,-4.50709d-9,-6.19492d-14,8.71734d-20,8.73051d-24)
      GO TO 120
!                                      TEMP. IS FROM -20 C  TO    0 C
   50 CONTINUE
      T=F6(HH,-1.94224d1,8.59061d-4,-4.4875d-9,-5.76696d-14,7.72217d-19,3.97894d-24)
      GO TO 120
!                                      TEMP. IS FROM   0 C  TO   20 C
   60 CONTINUE
      T=F6(HH,-2.01147d1,9.04936d-4,-6.83305d-9,2.3261d-14,7.27237d-20,-6.31939d-25)
      GO TO 120
!                                      TEMP. IS FROM  20 C  TO   40 C
   70 CONTINUE
      T=F6(HH,-1.82124d1,8.31683d-4,-6.16461d-9,3.06411d-14,-8.60964d-20,1.03003d-25)
      GO TO 120
!                                      TEMP. IS FROM  40 C  TO   60 C
   80 CONTINUE
      T=F6(HH,-1.29419d0,3.88538d-4,-1.30237d-9,2.78254d-15,-3.27225d-21,1.60969d-27)
      GO TO 120
!                                      TEMP. IS FROM   60 C TO   80 C
   90 CONTINUE
      T=F6(HH,2.39214d1,1.27519d-4,-1.52089d-10,1.1043d-16,-4.33919d-23,7.05296d-30)
      GO TO 120
!                                      TEMP. IS FROM   80 C TO   90 C
  100 CONTINUE
      T=F6(HH,4.88446d1,3.85534d-5,-1.78805d-11,4.87224d-18,-7.15283d-25,4.36246d-32)
      GO TO 120
!                                      TEMP. IS FROM   90 C TO  100C
  110 CONTINUE
      T=F7(HH,7.60565d11,5.80534d4,-7.36433d-3,5.11531d-10,-1.93619d-17,3.70511d-25, -2.77313d-33)
!                                      IF THE BAROMETRIC PRESSURE IS
!                                      EQUAL TO 1.0133E5 , SATURATION
!                                      TEMP. IS CALCULATED BY ABOVE EQUA
!                                      OTHERWISE TEMP. IS COMPUTED BY
!                                      FOLLOWING ITERATION METHOD
  120 CONTINUE
#ifdef EP_psych_errors
      IF (FlagError) THEN
        CALL ShowContinueError(' Initial Resultant Temperature= '//TRIM(TrimSigDigits(T,2)))
      ENDIF
#endif
      IF (ABS(PB-1.0133d5)/1.0133d5 <= 0.01d0) GO TO 170
      IterCount=0
      T1=T
      H1=PsyHFnTdbW(T1,PsyWFnTdbTwbPb(T1,T1,PB))
      Y1=H1-Hloc
      IF (ABS(Y1/Hloc) <= 0.1d-4) GO TO 140
      T2=T1*0.9d0
  130 IterCount=IterCount+1
      H2=PsyHFnTdbW(T2,PsyWFnTdbTwbPb(T2,T2,PB))
      Y2=H2-Hloc
      IF (ABS(Y2/Hloc) <= 0.1d-4) GO TO 150
      IF (Y2 == Y1) GO TO 150
      TN=T2-Y2/(Y2-Y1)*(T2-T1)
      IF (IterCount > 30) GO TO 160
      T1=T2
      T2=TN
      Y1=Y2
      GO TO 130
  140 CONTINUE
      T=T1
      GO TO 170
  150 CONTINUE
      T=T2
      GO TO 170
  160 CONTINUE
#ifdef EP_psych_errors
      IF (FlagError) THEN
        CALL ShowSevereError('Temperature did not converge (PsyTsatFnHPb)')
        if (present(calledfrom)) then
          CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
        else
          CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
        endif
        String=' Enthalpy='//TRIM(TrimSigDigits(HH,5))//' Pressure= '//TRIM(TrimSigDigits(PB,2))
        CALL ShowContinueError(TRIM(String)//' Last T='//TRIM(TrimSigDigits(T,2)))
      ENDIF
#endif
  170 CONTINUE

!   result is T

  RETURN

  CONTAINS

      REAL(r64) FUNCTION F6(X,A0,A1,A2,A3,A4,A5)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3,A4,A5

      F6=A0+X*(A1+X*(A2+X*(A3+X*(A4+X*A5))))

      RETURN
      END FUNCTION F6

      REAL(r64) FUNCTION F7(X,A0,A1,A2,A3,A4,A5,A6)
      IMPLICIT NONE
      REAL(r64) X,A6
      REAL(r64) A0,A1,A2,A3,A4,A5

      F7=(A0+X*(A1+X*(A2+X*(A3+X*(A4+X*(A5+X*A6))))))/1.0D10

      RETURN
      END FUNCTION F7


END FUNCTION PsyTsatFnHPb


FUNCTION PsyTsatFnPb(Press,calledfrom) RESULT(Temp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George Shih
          !       DATE WRITTEN   May 1976
          !       RE-ENGINEERED  Dec 2003; Rahul Chillar

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the saturation temperature from barometric pressure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! 1989 ASHRAE Handbook - Fundamentals
          ! Checked against 2005 HOF, Chap 6, Table 3 (using pressure in, temperature out) with
          ! good correlation from -60C to 160C

          ! USE STATEMENTS:
  USE General, ONLY: Iterate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: Press   ! barometric pressure {Pascals}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages)
      REAL(r64)        :: Temp    ! result=> saturation temperature {C}

          ! FUNCTION PARAMETER DEFINITIONS:
      Integer, Parameter :: itmax = 50 ! Maximum number of iterations
      REAL(r64) :: convTol=0.0001d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      LOGICAL FlagError     ! set when errors should be flagged
      REAL(r64),SAVE:: Press_Save=-99999.d0
      REAL(r64),SAVE:: tSat_Save =-99999.d0
      REAL(r64):: tSat           ! Water temperature guess
      REAL(r64):: pSat           ! Pressure corresponding to temp. guess
      REAL(r64):: error          ! Deviation of dependent variable in iteration
      REAL(r64):: X1             ! Previous value of independent variable in ITERATE
      REAL(r64):: Y1             ! Previous value of dependent variable in ITERATE
      REAL(r64):: ResultX        ! ResultX is the final Iteration result passed back to the calling routine
      Integer :: iter       ! Iteration counter
      Integer :: icvg       ! Iteration convergence flag


#ifdef EP_psych_stats
      NumTimesCalled(iPsyTsatFnPb)=NumTimesCalled(iPsyTsatFnPb)+1
#endif

          ! Check press in range.
      FlagError=.false.
#ifdef EP_psych_errors
      IF (Press <= 0.0017d0 .or. Press >= 1555000.d0) THEN
        IF (.not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyTsatFnPb) == 0) THEN
            CALL ShowWarningMessage('Pressure out of range (PsyTsatFnPb)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            CALL ShowContinueError(' Input Pressure= '//TRIM(TrimSigDigits(Press,2)))
            FlagError=.true.
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('Pressure out of range (PsyTsatFnPb)',   &
            iPsyErrIndex(iPsyTsatFnPb),ReportMinOf=Press,ReportMaxOf=Press,ReportMinUnits='Pa',ReportMaxUnits='Pa')
        ENDIF
      ENDIF
#endif
      IF (Press == Press_save) THEN
        Temp=tSat_Save
        RETURN
      ENDIF
      Press_save=Press

         ! Uses an iterative process to determine the saturation temperature at a given
         ! pressure by correlating saturated water vapor as a function of temperature.

         ! Initial guess of boiling temperature
      tSat = 100.0d0
      iter = 0

      ! If above 1555000,set value of Temp corresponding to Saturation Pressure of 1555000 Pascal.
      IF (Press>= 1555000.d0)Then
         tSat= 200.0d0
      ! If below 0.0017,set value of Temp corresponding to Saturation Pressure of 0.0017 Pascal.
      Else IF(Press<=0.0017d0)Then
         tSat= -100.0d0

      ! Setting Value of PsyTsatFnPb= 0C, due to non-continuous function for Saturation Pressure at 0C.
      Else IF((Press > 611.000d0) .and. (Press < 611.25d0))Then
         tSat= 0.0d0

      Else
      ! Iterate to find the saturation temperature
      ! of water given the total pressure

      ! Set iteration loop parameters
         ! make sure these are initialized
         DO iter = 1,itmax

            ! Calculate saturation pressure for estimated boiling temperature
            IF (PRESENT(calledfrom)) THEN
              pSat = PsyPsatFnTemp(tSat,calledfrom)
            ELSE
              pSat = PsyPsatFnTemp(tSat,'PsyTsatFnPb')
            ENDIF

            !Compare with specified pressure and update estimate of temperature
            error = Press - pSat
            Call ITERATE (ResultX,convTol,tSat,error,X1,Y1,iter,icvg)
            tSat = ResultX
            !If converged leave loop iteration
            IF (icvg .EQ. 1) Exit

            ! Water temperature not converged, repeat calculations with new
            ! estimate of water temperature

         End Do

         ! Saturation temperature has not converged after maximum specified
         ! iterations. Print error message, set return error flag, and RETURN

      End IF  !End If for the Pressure Range Checking

#ifdef EP_psych_stats
      NumIterations(iPsyTsatFnPb)=NumIterations(iPsyTsatFnPb)+iter
#endif

#ifdef EP_psych_errors
      IF (iter > itmax) THEN
        IF (.not. WarmupFlag) THEN
          IF (iPsyErrIndex(iPsyTsatFnPb2) == 0) THEN
            CALL ShowWarningMessage('Saturation Temperature not converged after '//TRIM(TrimSigDigits(iter))//  &
               ' iterations (PsyTsatFnPb)')
            if (present(calledfrom)) then
              CALL ShowContinueErrorTimeStamp(' Routine='//trim(calledfrom)//',')
            else
              CALL ShowContinueErrorTimeStamp(' Routine=Unknown,')
            endif
            CALL ShowContinueError(' Input Pressure= '//TRIM(TrimSigDigits(Press,2)))
            FlagError=.true.
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('Saturation Temperature not converged after max iterations (PsyTsatFnPb)',   &
            iPsyErrIndex(iPsyTsatFnPb2),ReportMinOf=tSat,ReportMaxOf=tSat,ReportMinUnits='C',ReportMaxUnits='C')
        ENDIF
      ENDIF
#endif

      ! Result is SatTemperature
      Temp = tSat
      tSat_Save=tSat


#ifdef EP_psych_errors
      IF (FlagError) THEN
       CALL ShowContinueError(' Resultant Temperature= '//TRIM(TrimSigDigits(Temp,2)))
      ENDIF
#endif

  RETURN

END FUNCTION PsyTsatFnPb


FUNCTION CPCW(Temperature,calledfrom) RESULT(SpecHeatCW)

          ! FUNCTION INFORMATION:
          !       AUTHOR         RUSSELL D. TAYLOR
          !       DATE WRITTEN   April 1992

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the specific heat of chilled water. CPCW (J/Kg/k)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

      Implicit NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: Temperature !unused1208
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: SpecHeatCW  ! result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

         SpecHeatCW=4180.d0

      RETURN
END FUNCTION CPCW

FUNCTION CPHW(Temperature,calledfrom) RESULT(SpecHeatHW)

          ! FUNCTION INFORMATION:
          !       AUTHOR         RUSSELL D. TAYLOR
          !       DATE WRITTEN   April 1992

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the specific heat of hot water. CPHW (J/Kg/k)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

      Implicit NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), intent(in) :: Temperature !unused1208
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: SpecHeatHW ! result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

         SpecHeatHW=4180.d0

      RETURN
END FUNCTION CPHW


FUNCTION RhoH2O(TB,calledfrom) RESULT(RhoResult)

          ! FUNCTION INFORMATION:
          !       AUTHOR         SIGSTEINN P. GRETARSSON
          !       DATE WRITTEN   April 1992

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides the density of water at a specific temperature.

          ! METHODOLOGY EMPLOYED:
          !     Density of water [kg/m3]
          !     (RANGE: KelvinConv - 423.15 DEG. K) (convert to C first)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

      IMPLICIT NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), INTENT(in) :: TB  ! Dry bulb temperature. {C}
      character(len=*), intent(in), optional :: calledfrom  ! routine this function was called from (error messages) !unused1208
      REAL(r64)        :: RhoResult

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

        RhoResult=1000.1207d0+8.3215874d-04*TB-4.929976d-03*TB**2+8.4791863d-06*TB**3

      RETURN
END FUNCTION RhoH2O

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


End Module Psychrometrics
