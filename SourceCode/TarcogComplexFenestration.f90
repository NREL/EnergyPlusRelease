module TARCOGGassesParams

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   August/2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Keeps common data used by gasses and tarcog routines

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataPrecisionGlobals

  implicit none

  public

  !Max number of gasses
  integer, parameter :: maxgas = 10

  !Standards:
  integer, parameter :: ISO15099    = 1     ! standard = ISO15099
  integer, parameter :: EN673       = 2     ! standard = EN 673 / ISO 10292 Declared
  integer, parameter :: EN673Design = 3     ! standard = EN 673 / ISO 10292 Design

  integer, parameter :: MinStandard = 1     ! minimum index for standard
  integer, parameter :: MaxStandard = 3     ! maximum index for standard

  !real(r64), parameter :: pi       = 3.14159265358979323846d0
  !real(r64), parameter :: UniversalGasConst = 8314.462175d0 !(J/mol*K)
  real(r64), parameter :: alpha1 = 0.5d0 !accomodation coefficient for low pressure gas calculations
  real(r64), parameter :: alpha2 = 0.5d0 !accomodation coefficient for low pressure gas calculations
  real(r64), parameter :: InputDataTolerance = 1.0d-7 !coefficient used for input data tolerance in case for displaying error message

  !real(r64) :: gcon(maxgas,3), gvis(maxgas,3), gcp(maxgas,3), grho(maxgas,3), wght(maxgas)

  ! Gas properties (ISO 15099 - Regression constants from Annex B):
  !DATA gcon / 2.873e-3,   2.285e-3,  9.443e-4,  4.538e-4,  0,0,0,0,0,0, &
  !          & 7.760e-5,   5.149e-5,  2.826e-5,  1.723e-5,  0,0,0,0,0,0, &
  !          & 0.0,        0.0,       0.0,       0.0,       0,0,0,0,0,0/
  !DATA gvis / 3.723e-6,   3.379e-6,  2.213e-6,  1.069e-6,  0,0,0,0,0,0, &
  !          & 4.940e-8,   6.451e-8,  7.777e-8,  7.414e-8,  0,0,0,0,0,0, &
  !          &  0.0,        0.0,       0.0,       0.0,       0,0,0,0,0,0/
  !DATA gcp  / 1.002737e3, 0.521929e3,0.248091e3,0.158340e3,0,0,0,0,0,0, &
  !          & 1.2324e-2,  0,         0,         0,         0,0,0,0,0,0, &
  !          & 0,          0,         0,         0,         0,0,0,0,0,0/

  !  Mollecular weights (ISO 15099 - from Annex B):
  !DATA wght / 28.97,      39.948,    83.8,      131.3,     0,0,0,0,0,0/

  !SAVE gcon, gvis, gcp, grho, wght

!contains

  ! GetGasIndex - returns index of a gas (from ISO15099 gas properties list) based on its molecular weight
  !integer function GetGasIndex (molweight)

  !  real(r64) :: molweight
  !  integer :: i

  !  GetGasIndex = 0  ! unknown gas

  !  do i = 1, maxgas
  !    if (ABS(molweight-wght(i)).lt.1e-5) then
  !      GetGasIndex = i
  !      EXIT  ! exit loop
  !    end if
  !  end do


  !  return

  !end function GetGasIndex

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

end module TARCOGGassesParams

module TARCOGGasses90

          ! MODULE INFORMATION:
          !       AUTHOR         D. Charlie Curcija
          !       DATE WRITTEN   June/2000
          !       MODIFIED       (see revision history bellow)
          !       RE-ENGINEERED  na
          !
          !  Revision: 7.0.02  (November/8/2011), Simon Vidanovic
          !   - feature: Error message (string) return from gasses
          !
          !  Revision: 7.0.00  (September/6/2011), Simon Vidanovic
          !   - Introduction of vacuum coefficients and routine to calculate low gas pressure conductance
          !
          !  Revision: 6.3.09  (August/23/2011), Simon Vidanovic
          !   - Removed GetGasIndex function which could cause a double usage of gas coefficients
          !     and therefore introducing new bugs.

          ! PURPOSE OF THIS MODULE:
          ! A module containing functions for gas properties calculation

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! ISO15099, EN673

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataGlobals
  use TARCOGGassesParams

  implicit none

  public GASSES90
  public GassesLow
  !private doe2gas90

  contains

  subroutine GASSES90(tmean,iprop,frct,pres,nmix,xwght,xgcon,xgvis,xgcp,con,visc,dens,cp,pr,standard,nperr,ErrorMessage)

    ! Variables

    integer, intent(in) :: standard, nmix
    integer, dimension(maxgas), intent(in) :: iprop
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgcp, xgvis
    real(r64), intent(in) :: tmean, pres
    real(r64), dimension(maxgas), intent(in) :: frct

    integer, intent(out) :: nperr
    character(len=*), intent(inout) :: ErrorMessage
    real(r64), intent(out) :: con, visc, dens, cp, pr

    integer :: i, j;

    real(r64), dimension(maxgas) :: fvis, fcon, fdens, fcp
    real(r64), dimension(maxgas) :: kprime, kdblprm, mukpdwn, kpdown, kdpdown
    real(r64) :: molmix, cpmixm, kpmix, kdpmix, kmix, mumix
    real(r64) :: phimup, downer, psiup, psiterm, phikup, rhomix

    !Simon: TODO: this is used for EN673 calculations and it is not assigned properly. Check this
    real(r64), dimension(maxgas, 3) :: xgrho
    real(r64), dimension(maxgas, 3) :: grho

    !real(r64) gaslaw
    !DATA gaslaw /8314.51/   ! Molar gas constant in Joules/(kmol*K)

    ! SUBROUTINE PARAMETER DEFINITIONS:
    real(r64), parameter          :: ENpressure = 1.0d5    ! Gap gas pressure (Pa)
    real(r64), parameter          :: gaslaw = 8314.51d0    ! Molar gas constant (J/kMol-K)

    !!! Body of GASSES90

    con=0.0d0
    visc=0.0d0
    dens=0.0d0
    cp=0.0d0

    !Simon: remove this when assigned properly
!EPTeam???    xgrho = 0.0d0 !Objexx:Uninit Line added to protect against use uninitialized: Assuming this is incomplete/unused functionality
    grho = 0.0d0

    fcon(1)  = xgcon(iprop(1),1) + xgcon(iprop(1),2)*tmean + xgcon(iprop(1),3)*tmean**2
    fvis(1)  = xgvis(iprop(1),1) + xgvis(iprop(1),2)*tmean + xgvis(iprop(1),3)*tmean**2
    fcp(1)   = xgcp(iprop(1),1)  + xgcp(iprop(1),2)*tmean  + xgcp(iprop(1),3)*tmean**2
    ! Density using ideal gas law: rho=(presure*mol. weight)/(gas const*Tmean)
    fdens(1) = pres * xwght(iprop(1)) / (UniversalGasConst * tmean)
                ! Mollecular weights in kg/kmol
    if ((standard.eq.EN673).or.(standard.eq.EN673Design)) then
       !fdens(1) = xgrho(iprop(1),1) + xgrho(iprop(1),2)*tmean + xgrho(iprop(1),3)*tmean**2 !Objexx:Uninit xgrho is uninitialized
      fdens(i) = ENpressure*xwght(iprop(i))/(gaslaw*tmean)
    end if

    if (frct(1).eq.1.0d0) then   ! Single gas properties
      visc=fvis(1)      ! viscosity in kg/(m*s)
      con=fcon(1)       ! conductivity in W/(m*K)
      cp=fcp(1)         ! SpecIFic heat in J/(kg*K)
      dens=fdens(1)     ! density in kg/m^3
    else        ! Mixture properties
      molmix = frct(1)*xwght(iprop(1))       ! initialize equation 56
      cpmixm = molmix*fcp(1)            ! initialize equation 58
      kprime(1)  = 3.75d0 * UniversalGasConst / xwght(iprop(1)) * fvis(1)   ! equation 67
      kdblprm(1) = fcon(1)-kprime(1)        ! equation 67
      ! initialize sumations for eqns 60-66:
      mumix = 0.0d0
      kpmix = 0.0d0
      kdpmix = 0.0d0
      mukpdwn(1) = 1.0d0
      kpdown(1) = 1.0d0
      kdpdown(1) = 1.0d0
      do i = 2, nmix
        if (frct(i).eq.0.0d0) then
          nperr = 2011              ! error 2011: component fraction in a mixture is 0%
          ErrorMessage = 'Component fraction in mixture is 0%'
          return
        end if
        ! calculate properties of mixture constituents:
        fcon(i)  = xgcon(iprop(i),1) + xgcon(iprop(i),2)*tmean + xgcon(iprop(i),3)*tmean**2.0d0
        fvis(i)  = xgvis(iprop(i),1) + xgvis(iprop(i),2)*tmean + xgvis(iprop(i),3)*tmean**2.0d0
        fcp(i)   = xgcp(iprop(i),1)  + xgcp(iprop(i),2)*tmean  + xgcp(iprop(i),3)*tmean**2.0d0
        fdens(i) = pres * xwght(iprop(i)) / (UniversalGasConst * tmean)
        if ((standard.eq.EN673).or.(standard.eq.EN673Design)) then
          !fdens(i) = grho(iprop(i),1) + grho(iprop(i),2)*tmean + grho(iprop(i),3)*tmean**2.0d0
          fdens(1) = ENpressure*xwght(iprop(1))/(gaslaw*tmean)  ! Density using ideal gas law: rho=(presure*mol. weight)/(gas const*Tmean)
        end if
        molmix = molmix+frct(i)*xwght(iprop(i))      ! equation 56
        cpmixm = cpmixm+frct(i)*fcp(i)*xwght(iprop(i))   ! equation 58-59
        kprime(i)  = 3.75d0 * UniversalGasConst / xwght(iprop(i)) * fvis(i) ! equation 67
        kdblprm(i) = fcon(i)-kprime(i)      ! equation 68
        mukpdwn(i) = 1.0d0            ! initialize denominator of equation 60
        kpdown(i)  = 1.0d0            ! initialize denominator of equation 63
        kdpdown(i) = 1.0d0            ! initialize denominator of equation 65
      end do

      select case (standard)
        case (ISO15099)
          do  i = 1, nmix
            do  j = 1, nmix
              ! numerator of equation 61
              phimup = (1.0d0 + (fvis(i)/fvis(j))**0.5d0*(xwght(iprop(j))/xwght(iprop(i)))**0.25d0)**2.0d0

              ! denominator of equation 61, 64 and 66
              downer = 2.0d0 * sqrt(2.0d0) * (1.0d0+(xwght(iprop(i))/xwght(iprop(j))))**0.5d0

              ! calculate the denominator of equation 60
              if (i.ne.j) mukpdwn(i) = mukpdwn(i) + phimup/downer*frct(j)/frct(i)

              ! numerator of equation 64, psiterm is the multiplied term in backets
              psiup   = (1.0d0 + (kprime(i)/kprime(j))**0.5d0*(xwght(iprop(i))/xwght(iprop(j)))**0.25d0)**2.0d0

              psiterm = 1.0d0 + 2.41d0*(xwght(iprop(i))-xwght(iprop(j)))*(xwght(iprop(i))-0.142d0*xwght(iprop(j))) &
                                    / (xwght(iprop(i)) + xwght(iprop(j)))**2.0d0

              ! using the common denominator downer calculate the denominator for equation 63
              if (i.ne.j) kpdown(i) = kpdown(i) + psiup*psiterm/downer*frct(j)/frct(i)

              ! calculate the numerator of equation 66
              phikup = (1.0d0+(kprime(i)/kprime(j))**0.5d0 * (xwght(iprop(i))/xwght(iprop(j)))**0.25d0)**2.0d0

              ! using the common denominator downer calculat the denominator for equation 65
              if (i.ne.j) kdpdown(i) = kdpdown(i) + phikup/downer*frct(j)/frct(i)
            end do
            mumix = mumix + fvis(i)/mukpdwn(i)            ! equation 60
            kpmix = kpmix + kprime(i)/kpdown(i)           ! equation 63
            kdpmix = kdpmix + kdblprm(i)/kdpdown(i)       ! equation 65
          end do

          ! calculate the density of the mixture assuming an ideal gas:
          rhomix = pres * molmix / (UniversalGasConst * tmean)      ! equation 57
          kmix = kpmix + kdpmix                          ! equation 68-a

          ! final mixture properties:
          visc=mumix
          con=kmix
          dens=rhomix
          cp=cpmixm/molmix
        case (EN673, EN673Design)
          do  i=1,nmix
            con = con + fcon(i)*frct(i)
            visc = visc + fvis(i)*frct(i)
            dens = dens + fdens(i)*frct(i)
            cp = cp + fcp(i)*frct(i)
          end do
        case DEFAULT
          ! should never come here - unsupported standard
      end select

    end if

    pr = cp * visc / con    ! calculate the Prandtl number

  end subroutine GASSES90

  subroutine GassesLow(tmean, mwght, pressure, gama, cond, nperr, ErrorMessage)

    real(r64), intent(in) :: tmean, mwght, pressure, gama

    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage
    real(r64), intent(out) :: cond

    real(r64) :: alpha = 0.0d0
    real(r64) :: B = 0.0d0

    alpha = alpha1 * alpha2 / (alpha2 + alpha1*(1 - alpha2))

    if ((gama).eq.1) then
      nperr = 40 !supplied gamma coefficient is incorrect
      ErrorMessage = 'Supplied gamma coefficient is incorrect.'
      return
    end if

    B = alpha * (gama + 1)/(gama - 1) * (UniversalGasConst / (8 * pi * mwght * tmean)) ** 0.5d0

    cond = B * pressure

  end subroutine GassesLow

!  subroutine doe2gas90 (standard, iprop, frct, pres, nmix, con0, dcon, visc0, dvisc, dens0, ddens, pr0, dpr)
!    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!    !  calculate gas properties using old doe2 format                              !
!    !------------------------------------------------------------------------------!
!    !  iprop(i)  vector of gas identifiers (for max of w5cog.fi::maxgas gasses)
!    !  frct(i)   vector of fraction of gasses in a mixture (for max of w5cog.fi::maxgas gasses)
!    !  pres(i)   pressure (default: pres = 1e5)[N/m^2]
!    !  nmix(i)   number of gasses in a mixture
!    !  con0(o)   thermal conductivity @ mean temperature of 0 C[W/m-K]
!    !  dcon(o)   derivative of thermal conductivity wrt temperature x 10^5 [W/m-K^2 x 10^5]
!    !  visc0(o)  dynamic viscosity @ mean temperature of 0 C x 10^5 [kg/m-s x 10^5]
!    !  dvisc(o)  derivative of dynamic viscosity wrt temperature x 10^8 [kg/m-s-K x 10^8]
!    !  dens0(o)  density @ mean temperature of 0 C [kg/m^3]
!    !  ddens(o)  derivative of density wrt temperature [kg/m^3-K]
!    !  pr0(o)    Prandl number @ mean temperature of 0 C [ - ]
!    !  dpr(o)    derivative of Prandl number wrt temperature [ 1/K ]
!    !  nperr(o)  error flag (if component fraction in a mixture is 0%)
!    !
!    !**import:
!    !  w5cog.fi::maxgas
!    !
!    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
!    ! Variables
!
!    integer, intent(in) :: nmix, iprop(maxgas)
!    real(r64), intent(in) :: pres, frct(maxgas)
!
!    real(r64), intent(out) :: con0, dcon, visc0, dvisc, dens0, ddens, pr0, dpr
!
!    real(r64) :: con, visc, dens, cp, pr
!    integer :: standard, nperr
!    character*(2000) :: ErrMsg
!
!    call GASSES90(273.15d0, iprop, frct, pres,nmix, wght, gcon, gvis, gcp, con, visc, dens, cp, pr, standard, nperr, ErrMsg)
!
!    con0=con
!    visc0=visc*10**5
!    dens0=dens
!    pr0=pr
!
!    call GASSES90(283.15d0,iprop, frct, pres, nmix, wght, gcon, gvis, gcp, con, visc, dens, cp, pr, standard, nperr, ErrMsg)
!
!    dcon=(con-con0)/10*10**5
!    dvisc=(visc*10**5-visc0)/10*10**3
!    ddens=(dens-dens0)/10
!    dpr=(pr-pr0)/10
!
!  end subroutine doe2gas90

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

end module TARCOGGasses90

module TARCOGParams

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June/22/2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          !  Revision: 6.0.36  (June/22/2010)
          !   - Initial setup, extracted from TARCOG.fi
          !   - Moved Standards and MaxGas into GasParams module (gasses project)

          ! PURPOSE OF THIS MODULE:
          ! Module which contains common TARCOG parameters and constants

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataPrecisionGlobals

  implicit none

  public

  !real(r64), parameter :: StefanBoltzmannConst    = 5.6697e-8     ! Stefan-Boltzman constant (5.6697e-8 [W/m^2K^4])
  !real(r64), parameter :: GravityConstant = 9.807d0
  real(r64), parameter :: e = 2.718281828459d0
  !real(r64), parameter :: MaxHr = 100  ! used in iterations in case temperatures on surfaces reaches identical values
  real(r64), parameter :: DeflectionRelaxation = 0.005d0 ! Deflection relaxation parameter
  integer, parameter :: DeflectionMaxIterations = 400 !maximum number of deflection iterations
  real(r64), parameter :: DeflectionErrorMargin = 0.01d0 !maximum temperature difference on layers for deflection iterations
  !pi is moved to gasses since it is used there now
  !real(r64), parameter :: pi       = 3.14159265358979323846

  integer, parameter :: maxpan   = 100          ! maximum number of monolithic glazing layers (100)
  !integer, parameter :: maxlay   = 1000         ! maximum number of layers (including laminates) (1000)
  integer, parameter :: maxlay   = 100          ! maximum number of layers (including laminates) (100)
  integer, parameter :: MaxGap   = maxlay-1     !maximum number of gaps (between layers)
  integer, parameter :: maxlay1  = maxlay+1     ! maximum number of 'gaps', including in and out (maxlay+1)
  integer, parameter :: maxlay2  = maxlay*2     ! maximum number of glass surfaces (maxlay*2)
  integer, parameter :: maxlay3  = maxlay2+1    ! maximum number of ? (maxlay2+1)
  integer, parameter :: maxlay4  = maxlay*4     ! maximum number of ? (maxlay*4)
  integer, parameter :: maxslice = 100          ! maximum nuber of slices (100)

  !integer, parameter :: MaxThetaArray = 200     ! maximum number for theta array

  ! Debug flags
  integer, parameter :: noDebug = 0
  integer, parameter :: appendResultsToFile = 1
  integer, parameter :: resultsToNewFile = 2
  integer, parameter :: saveIntermediateResults = 3 !this will create new file

  integer, parameter :: minDebugFlag = 0
  integer, parameter :: maxDebugFlag = 3

  ! to keep info that certain file is not open for writing
  integer, parameter :: statusClosed = 0

  !  Layer types:
  integer, parameter :: SPECULAR   = 0
  integer, parameter :: VENETBLIND = 1
  integer, parameter :: WOVSHADE   = 2
  integer, parameter :: PERFORATED = 3
  integer, parameter :: DIFFSHADE  = 4
  integer, parameter :: BSDF = 5

  integer, parameter :: MinLayType = 0
  integer, parameter :: MaxLayType = 5

  !  Thermal models:
  integer, parameter :: THERM_MOD_ISO15099 = 0
  integer, parameter :: THERM_MOD_SCW      = 1
  integer, parameter :: THERM_MOD_CSM      = 2

  integer, parameter :: MinThermalMode = 0
  integer, parameter :: MaxThermalMode = 2

  integer, parameter :: NO_SupportPillar = 0
  integer, parameter :: YES_SupportPillar = 1

  !Deflection parameters
  integer, parameter :: NO_DEFLECTION_CALCULATION = 0
  integer, parameter :: DEFLECTION_CALC_TEMPERATURE = 1
  integer, parameter :: DEFLECTION_CALC_GAP_WIDTHS = 2

  !definition of parameters for deflection sum.  These parameters define maximum number of loop to which sum
  !will perform. By equation, these numbers will go to infinite and some test showed that going to nmax and mmax
  !values would produce enough precision
  integer, parameter :: mmax = 5 ! top m value for which "deflection sum" will be calculated
  integer, parameter :: nmax = 5 ! top n value for which "deflection sum" will be calculated

  !  CalcForcedVentilation flag:
  !  0 = Skip forced ventilation calc
  !  1 = Allow forced ventilation calc
  integer, parameter :: CalcForcedVentilation = 0

  !  Calculation outcome
  integer, parameter :: CALC_UNKNOWN      = 0
  integer, parameter :: CALC_OK           = 1
  integer, parameter :: CALC_DIVERGE      = 2
  integer, parameter :: CALC_OSC_OK       = 3

  integer, parameter :: NumOfIterations = 100

  ! Program will examine convergence parameter in each iteration.  That convergence parameter should decrease each time.
  ! In case that is not happening program will tolerate certain number of tries before declare convergence
  ! (or decrease relaxation parameter)
  integer, parameter :: NumOfTries = 5
  !integer, parameter :: NewtonIterations = 75 ! shows when to swith to Newton
  real(r64), parameter :: RelaxationStart = 0.6d0 ! Has to be between 0 and 1
  real(r64), parameter :: RelaxationDecrease = 0.1d0 ! Step for which relaxation parameter will decrease

  ! Convergence parameters
  real(r64), parameter :: tempCorrection = 1d-10 !used in case outside or inside temperature approaches tamb or troom
  real(r64), parameter :: ConvergenceTolerance = 1d-2 ! tolerance used within iterations

  ! Airflow iterations
  real(r64), parameter :: AirflowConvergenceTolerance = 1d-2
  real(r64), parameter :: AirflowRelaxationParameter = 0.9d0

  real(r64), parameter :: TemperatureQuessDiff = 1.0d0 ! in case outside and inside temperatures are identical

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

end module TARCOGParams

module TARCOGCommon

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June/22/2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          !  Revision: 6.0.36  (June/22/2010)
          !   - Initial setup, extracted from TARCOG.for

          ! PURPOSE OF THIS MODULE:
          ! A module which contains common TARCOG functions and subroutines

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataPrecisionGlobals

  implicit none

  public  IsShadingLayer
  public  LDSumMax
  public  LDSumMean
  public  MatrixQBalance
  public  pos
  public  EquationsSolver
  private ludcmp
  private lubksb

  contains

  logical function IsShadingLayer (layertype)

    use TARCOGParams

    integer :: layertype

    if ((layertype.eq.VENETBLIND).or.(layertype.eq.WOVSHADE).or.(layertype.eq.PERFORATED).or.(layertype.eq.BSDF)) then
      IsShadingLayer = .TRUE.
    else
      IsShadingLayer = .FALSE.
    end if
    return

  end function IsShadingLayer

  real(r64) function LDSumMax(Width, Height)
    ! LDSumMax function calculates sum part of equation for maximum deflection
    ! Width - glazing system width
    ! Height - glazing system height

    use DataGlobals, only: Pi
    use TARCOGParams
    !use TARCOGGassesParams

    real(r64), intent(in) :: Width
    real(r64), intent(in) :: Height
    integer :: i, j

    LDSumMax = 0.0d0
    do i = 1, mmax, 2
      do j = 1, nmax, 2
        LDSumMax = LDSumMax + (Sin(i*Pi/2) * Sin(j*Pi/2)) / (i*j*((i/Width)**2 + (j/Height)**2)**2)
      end do !do j = 1, nmax, 2
    end do !do i = 1, mmax, 2

    return
  end function LDSumMax

  real(r64) function LDSumMean(Width, Height)
    ! LDSumMean function calculates sum part of equation for mean deflection
    ! Width - glazing system width
    ! Height - glazing system height

    use DataGlobals, only: Pi
    use TARCOGParams
    !use TARCOGGassesParams

    real(r64), intent(in) :: Width
    real(r64), intent(in) :: Height
    integer :: i, j

    LDSumMean = 0.0d0
    do i = 1, mmax, 2
      do j = 1, nmax, 2
        LDSumMean = LDSumMean + 4 / (i**2 * j**2 * Pi**2 * ((i / Width)**2 + (j / Height)**2)**2)
      end do !do j = 1, nmax, 2
    end do !do i = 1, mmax, 2

    return
  end function LDSumMean

  subroutine matrixQBalance(nlayer, a, b, scon, thick, hcgas, hcout, hcin, asol, qv, &
        Tin, Tout, Gin, Gout, theta, tir, rir, emis)

    use DataGlobals, only: StefanBoltzmann
    use TARCOGParams

    integer, intent(in) :: nlayer
    real(r64), dimension(maxlay), intent(in) :: scon, thick
    real(r64), dimension(maxlay1), intent(in) :: hcgas
    real(r64), intent(in) :: hcout, hcin, Gin, Gout
    real(r64), dimension(maxlay2), intent(in) :: tir, rir, emis, theta

    real(r64), dimension(maxlay), intent(in) :: asol
    real(r64), dimension(maxlay1), intent(in) :: qv
    real(r64), intent(in) :: Tin, Tout

    real(r64), dimension(4*nlayer, 4*nlayer), intent(out) :: a
    real(r64), dimension(4*nlayer), intent(out) :: b

    ! local variables
    integer :: i, j, k, front, back

    do i=1, 4*nlayer
      b(i) = 0.0d0
      do j = 1, 4*nlayer
        a(i,j) = 0.0d0
      end do
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!  build matrix a   !!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Because of build matrix optimization all environmental heat transfer
    ! coefficients were stored in hgas array.  This means that hgas(1) is actually
    ! hout, while hgas(nlayer+1) is actually hin.  Same is valid for hcgas and
    ! hrgas arrays
    do i = 1, nlayer
      k = 4*i - 3
      front = 2*i - 1
      back = 2*i
      if (nlayer.ne.1) then
        if (i.ne.1) then
          a(k, k-3)   = -hcgas(i)
          a(k, k-1)   = -1.0d0
          a(k+1, k-3) = -hcgas(i)
          a(k+1, k-1) = -1.0d0
          a(k+2, k-1) = rir(front)
          a(k+3, k-1) = tir(front)
        end if
        if (i.ne.nlayer) then
          a(k, k+4)   = -hcgas(i+1)
          a(k, k+6)   = -1.0d0
          a(k+2, k+6) = tir(back)
          a(k+3, k+6) = rir(back)
        end if
      end if
      a(k, k)     = hcgas(i)
      a(k, k+1)   = hcgas(i+1)
      a(k, k+2)   = 1.0d0
      a(k, k+3)   = 1.0d0
      a(k+1, k)   = scon(i)/thick(i) + hcgas(i)
      a(k+1, k+1) = -scon(i)/thick(i)
      a(k+1, k+2) = 1.0d0
      a(k+2, k)   = emis(front) * StefanBoltzmann * theta(front)**3.0d0
      a(k+2, k+2) = -1.0d0
      a(k+3, k+1) = emis(back) * StefanBoltzmann * theta(back)**3.0d0
      a(k+3, k+3) = -1.0d0
    end do

    !build matrix b
    do i = 1, nlayer
      k = 4*i - 3
      front = 2*i - 1
      back = 2*i
      b(k) = asol(i) + 0.5d0 * qv(i) + 0.5d0 * qv(i+1)
      b(k+1)   = 0.5d0 * asol(i) + 0.5d0 * qv(i)
      if (i.eq.1) then
        b(k)   = b(k) + hcout*Tout + Gout
        b(k+1) = b(k+1) + hcout*Tout + Gout
        b(k+2) = b(k+2) - rir(front)*Gout
        b(k+3) = b(k+3) - tir(front)*Gout
      end if
      if (i.eq.(nlayer)) then
        b(k) = b(k) + hcin*Tin + Gin
        b(k+2) = b(k+2) - tir(back)*Gin
        b(k+3) = b(k+3) - rir(back)*Gin
      end if
    end do

  end subroutine matrixQBalance

  subroutine EquationsSolver(a,b,n,nperr,ErrorMessage)
    !***********************************************************************
    ! Purpose: solves the main system of energy balance equations
    !***********************************************************************
    ! Input:
    !   a - matrix, radiositied
    !   b - vector of known quantities
    !   n - ???
    ! Output:
    !   b - solutions
    !   nperr - error code

    use TARCOGParams

    integer, intent(in) :: n
    real(r64), dimension(n,n), intent(inout) :: a
    real(r64), dimension(n), intent(inout) :: b

    integer, intent(inout) :: nperr
    character(len=*), intent (inout) :: ErrorMessage

    integer :: indx(n)
    real(r64) :: d

    call ludcmp(a, n, indx, d, nperr, ErrorMessage)

    ! Exit on error
    if ((nperr.gt.0).and.(nperr.le.1000)) return

    call lubksb(a,n,indx,b)

  end subroutine EquationsSolver

  subroutine ludcmp(a,n,indx,d,nperr,ErrorMessage)
    integer, intent(in) :: n
    real(r64), intent(inout) :: d
    real(r64), dimension(n,n), intent(inout) :: a
    integer, dimension(n), intent(inout) :: indx
    integer, intent(out) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    integer, parameter :: NMAX = 500
    real(r64), parameter :: TINY = 1.0d-20

    integer :: i,imax,j,k
    real(r64) :: aamax, dum, sum
    real(r64),dimension(NMAX) :: vv

    d=1.0d0
    do i = 1, n
      aamax = 0.0d0
      do j = 1, n
        if (ABS(a(i,j)).gt.aamax) aamax=ABS(a(i,j))
      end do  ! j
      if (aamax.eq.0.0d0) then
        nperr = 13
        ErrorMessage = 'Singular matrix in ludcmp.'
        return
      end if
      vv(i) = 1.0d0/aamax
    end do  ! i

    do j = 1, n
      do i = 1, j-1
        sum = a(i,j)
        do k = 1,i-1
          sum = sum - a(i,k)*a(k,j)
        end do  ! k
        a(i,j) = sum
      end do  ! i
      aamax = 0.0d0
      do i = j, n
        sum = a(i,j)
        do k = 1, j-1
          sum = sum - a(i,k)*a(k,j)
        end do  ! k
        a(i,j) = sum
        dum = vv(i)*ABS(sum)
        if (dum.ge.aamax) then
          imax = i
          aamax = dum
        end if
      end do  ! i
      if (j.ne.imax) then
        do k = 1, n
          dum = a(imax,k)
          a(imax,k) = a(j,k)
          a(j,k)    = dum
        end do  ! k
        d = -d
        vv(imax) = vv(j)
      end if
      indx(j) = imax
      if (a(j,j).eq.0.0d0) a(j,j) = TINY
      if (j.ne.n) then
        dum=1.0d0/a(j,j)
        do i = j+1, n
          a(i,j) = a(i,j)*dum
        end do  ! i
      end if
    end do  ! j

  end subroutine ludcmp

  subroutine lubksb(a,n,indx,b)
    !***********************************************************************
    !
    !***********************************************************************
    integer, intent(in) :: n
    integer, dimension(n), intent(in) :: indx
    real(r64), dimension(n,n), intent(in) :: a
    real(r64), dimension(n), intent(inout) :: b

    integer ::i,ii,j,ll
    real(r64) ::sum

    ii = 0
    do i = 1, n
      ll = indx(i)
      sum = b(ll)
      b(ll) = b(i)
      if (ii.ne.0) then
        do j = ii, i-1
          sum = sum - a(i,j)*b(j)
        end do  ! j
      else if (sum.ne.0.0d0) then
        ii = i
      end if
      b(i) = sum
    end do  ! i

    do i = n, 1, -1
      sum = b(i)
      do j = i+1, n
        sum = sum - a(i,j)*b(j)
      end do  ! j
      b(i) = sum/a(i,i)
    end do  ! i

  end subroutine lubksb

  real(r64) function pos(x)
    !***********************************************************************
    !
    !***********************************************************************
    real(r64), intent(in) :: x

    pos = (x + ABS(x)) / 2.0d0

  end function pos

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

end module TARCOGCommon

module TARCOGOutput

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June/22/2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          !  Revision: 6.0.36  (June/22/2010)
          !   - Initial setup, extracted from TARCOG.for

          ! PURPOSE OF THIS MODULE:
          ! A module which contains debug dump subroutines

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataPrecisionGlobals
  use TarcogCommon
  use TARCOGGassesParams
  use TARCOGParams

  implicit none

  public WriteInputArguments
  public WriteModifiedArguments
  public WriteOutputArguments
  public WriteOutputEN673
  public WriteTARCOGInputFile
  public FinishDebugOutputFiles
  public PrepDebugFilesAndVariables

  public

  ! variables:
  !bi...Debug files handles:
  !character(len=1000) :: DebugDir
  character(len=1000) :: DBGD
  character(len=10) :: FileMode
  logical :: WriteDebugOutput
  integer :: DebugMode
  integer :: winID, iguID

  integer, external :: GetNewUnitNumber

  integer :: InArgumentsFile = statusClosed
  integer :: OutArgumentsFile = statusClosed
  integer :: WINCOGFile = statusClosed

  !Intermediate debug files
  integer :: IterationCSVFileNumber = statusClosed
  integer :: TarcogIterationsFileNumber = statusClosed

  character(len=1000)    :: IterationCSVName = 'IterationResults.csv'

  !integer, parameter :: IterationHHAT = 102
  !character*(1000)    :: IterationHHATName = 'IterationHHAT.csv'

  character(len=1024)    :: WinCogFileName = 'test.w7'
  !character*(1000)    :: SHGCFileName = 'test.w7'
  character(len=1024)    :: DebugOutputFileName = 'Tarcog.dbg'

  character(len=*), parameter :: VersionNumber = ' 7.0.15.00 '
  character(len=*), parameter :: VersionCompileDateCC = ' August 02, 2012'

  contains

  subroutine WriteInputArguments(tout, tind, trmin, &
                &  wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, ibc, hout, hin,  &
                &  standard, ThermalMod, SDScalar, height, heightt, width, tilt, totsol,   &
                &  nlayer, LayerType, thick, scon, asol, tir, emis, Atop, Abot, Al, Ar, Ah,  &
                &  SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve,  &
                &  nslice, LaminateA, LaminateB, sumsol, gap, vvent, tvent,  &
                &  presure, nmix, iprop, frct, xgcon, xgvis, xgcp, xwght)

    use DataGlobals, only: KelvinConv
    integer, intent(in) :: nlayer, iwd, isky
    real(r64), dimension(maxlay), intent(in) :: Atop, Abot, Al, Ar, Ah
    real(r64), dimension(maxlay), intent(in) :: SlatThick, SlatWidth, SlatAngle
    real(r64), dimension(maxlay), intent(in) :: SlatCond, SlatSpacing, SlatCurve
    real(r64), dimension(maxlay1), intent(in) :: vvent, tvent

    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), intent(in) :: fclr, width, hout, hin, SDscalar, height, heightt
    real(r64), intent(in) :: tout, tind, wso, wsi, dir, outir, tsky, esky, totsol, tilt, trmin
    real(r64), intent(in) :: VacuumPressure, VacuumMaxGapThickness

    real(r64), dimension(maxlay), intent(in) :: gap, thick, scon, asol
    real(r64), dimension(maxlay2), intent(in) :: tir, emis
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: presure
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(2), intent(in) :: ibc
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgvis, xgcp
    real(r64), dimension(maxgas), intent(in) :: xwght

    real(r64), dimension(maxlay), intent(in) :: LaminateA, LaminateB, sumsol

    integer, dimension(maxlay), intent(in) :: nslice
    integer, intent(in) :: standard, ThermalMod


    integer, dimension(8) :: DATE_TIME
    character(LEN = 12), dimension(3) :: real_CLOCK

    integer :: i, j, nperr

  !bi...Create debug file w/ Tarcog's input arguments:

  ! File is not open and nothing cannot be written
  if (InArgumentsFile == statusClosed) return

    call DATE_AND_TIME (real_CLOCK (1), real_CLOCK (2), real_CLOCK (3), DATE_TIME)

    write(InArgumentsFile,*)
  !  write(InArgumentsFile, 10001) VersionNumber, VersionCompileDateCC
    write(InArgumentsFile, 1001) DATE_TIME(1), DATE_TIME(2), DATE_TIME(3), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
    write(InArgumentsFile,*)

    if (winID.eq.-1) then
      write(InArgumentsFile, 1002) winID
    else
      write(InArgumentsFile, 1003) winID
    end if

    if (iguID.eq.-1) then
      write(InArgumentsFile, 1006) iguID
    else
      write(InArgumentsFile, 1007) iguID
    end if

    write(InArgumentsFile,*) '    Debug dir:     '//TRIM(DBGD)

    write(InArgumentsFile,*)
    write(InArgumentsFile, 1000)
    write(InArgumentsFile,*)
    write(InArgumentsFile, 1005)
    write(InArgumentsFile, 1010) tout,  tout - KelvinConv
    write(InArgumentsFile, 1015) tind,  tind - KelvinConv
    write(InArgumentsFile, 1020) trmin,  trmin - KelvinConv
    write(InArgumentsFile, 1030) wso
    if (iwd.eq.0) write(InArgumentsFile, 1032)    ! windward
    if (iwd.eq.1) write(InArgumentsFile, 1033)    ! leeward
    write(InArgumentsFile, 1035) wsi
    write(InArgumentsFile, 1040) dir
    write(InArgumentsFile, 1041) outir
    write(InArgumentsFile, 1045) isky
    write(InArgumentsFile, 1050) tsky,  tsky - KelvinConv
    write(InArgumentsFile, 1055) esky
    write(InArgumentsFile, 1060) fclr
    write(InArgumentsFile, 1061) VacuumPressure
    write(InArgumentsFile, 1062) VacuumMaxGapThickness
    write(InArgumentsFile, 1063) ibc(1)
    write(InArgumentsFile, 1065) hout
    write(InArgumentsFile, 1066) ibc(2)
    write(InArgumentsFile, 1068) hin

    if (standard.eq.ISO15099) write(InArgumentsFile, 1070) standard
    if (standard.eq.EN673) write(InArgumentsFile, 1071) standard
    if (standard.eq.EN673Design) write(InArgumentsFile, 1072) standard

    if (ThermalMod.eq.THERM_MOD_ISO15099) then
      write(InArgumentsFile, 10731) ThermalMod
      write(InArgumentsFile, 10740) SDScalar
    end if

    if (ThermalMod.eq.THERM_MOD_SCW) then
      write(InArgumentsFile, 10732) ThermalMod
      write(InArgumentsFile, 10740) SDScalar
    end if

    if (ThermalMod.eq.THERM_MOD_CSM) then
      write(InArgumentsFile, 10733) ThermalMod
      write(InArgumentsFile, 10740) SDScalar
    end if

  !    if (ThermalMod.eq.THERM_MOD_CSM)
  !        write(InArgumentsFile, 10740) SDScalar

    write(InArgumentsFile, *)

    write(InArgumentsFile, 1075)
    write(InArgumentsFile, 1076) height
    write(InArgumentsFile, 1077) heightt
    write(InArgumentsFile, 1078) width
    write(InArgumentsFile, 1079) tilt
    write(InArgumentsFile, 1080) totsol
    write(InArgumentsFile, 1081) nlayer
    write(InArgumentsFile, *)

    write(InArgumentsFile, 1089)
    do i = 1, nlayer
      select case (LayerType(i))
        case (DIFFSHADE)                           ! Diffuse Shade
          write(InArgumentsFile, 10806) i, LayerType(i)
        case (WOVSHADE)                             ! Woven Shade
          write(InArgumentsFile, 10805) i, LayerType(i)
        case (VENETBLIND)                           ! Venetian blind
         write(InArgumentsFile, 10804) i, LayerType(i)
        case (SPECULAR)                             ! Specular layer
          if (nslice(i).le.1) then
            write(InArgumentsFile, 10802) i, LayerType(i) ! Monolithic glass
          else
            write(InArgumentsFile, 10803) i, LayerType(i) ! Laminated layer
          end if
        case DEFAULT
          write(InArgumentsFile, 10809) i, LayerType(i)
      end select

      write(InArgumentsFile, 1090) thick(i)
      write(InArgumentsFile, 1091) scon(i)
      write(InArgumentsFile, 1092) asol(i)
      write(InArgumentsFile, 1093) tir(2*i-1)
      write(InArgumentsFile, 1094) emis(2*i-1)
      write(InArgumentsFile, 1095) emis(2*i)

      if (LayerType(i).eq.VENETBLIND) then ! SD layer
        write(InArgumentsFile, 1100) Atop(i)
        write(InArgumentsFile, 1101) Abot(i)
        write(InArgumentsFile, 1102) Al(i)
        write(InArgumentsFile, 1103) Ar(i)
        write(InArgumentsFile, 1105) Ah(i)

        write(InArgumentsFile, 11051) SlatThick(i)
        write(InArgumentsFile, 11052) SlatWidth(i)
        write(InArgumentsFile, 11053) SlatAngle(i)
        write(InArgumentsFile, 11054) SlatCond(i)
        write(InArgumentsFile, 11055) SlatSpacing(i)
        write(InArgumentsFile, 11056) SlatCurve(i)

  !bi...Input arguments correction patch:

  !     if (ApplyVenetianPatch.eq..TRUE.) then
  !      SlatThick(i) = Thick(i)
  !      SlatWidth(i) = SlatWidth(i) / 1000.0
  !      SlatCurve(i) = SlatCurve(i) / 1000.0
  !      SlatSpacing(i) = SlatSpacing(i) / 1000.0
  !      write(InArgumentsFile, *) 'After applying the patch:'
  !        write(InArgumentsFile, 11051) SlatThick(i)
  !        write(InArgumentsFile, 11052) SlatWidth(i)
  !        write(InArgumentsFile, 11053) SlatAngle(i)
  !        write(InArgumentsFile, 11054) SlatCond(i)
  !        write(InArgumentsFile, 11055) SlatSpacing(i)
  !        write(InArgumentsFile, 11056) SlatCurve(i)
  !     end if

  !bi...end Input arguments correction patch

      end if

      if (nslice(i).gt.1) then ! SD layer
        write(InArgumentsFile, 1085) nslice(i)
        write(InArgumentsFile, 1085) LaminateA(i)
        write(InArgumentsFile, 1085) LaminateB(i)
        write(InArgumentsFile, 1085) sumsol(i)
      end if
    end do  ! i - layers

    write(InArgumentsFile, *)

    write(InArgumentsFile, 1110)

    do i = 1, nlayer+1 ! loop through gaps:
      if ((i.gt.1).and.(i.le.nlayer))   write(InArgumentsFile, 1111) i-1
      if (i.eq.1)  write(InArgumentsFile, 11110)
      if (i.eq.nlayer+1)  write(InArgumentsFile, 11111)
      if ((i.gt.1).and.(i.le.nlayer))   write(InArgumentsFile, 1112) gap(i-1)
      write(InArgumentsFile, 1113) presure(i)
      if ((i.gt.1).and.(i.le.nlayer)) then
        write(InArgumentsFile, 1120) vvent(i)
      end if
      if ((i.gt.1).and.(i.le.nlayer)) then
        write(InArgumentsFile, 1121) tvent(i)
      end if
      write(InArgumentsFile, 1114) nmix(i)

      !if (mgas.eq.1) then ! call gasses by names:
      !  do  j = 1, nmix(i)
      !    if (iprop(i, j).eq.1) write(InArgumentsFile, 1115) iprop(i, j), 'Air,     ', 100*frct(i, j) ! Air
      !    if (iprop(i, j).eq.2) write(InArgumentsFile, 1115) iprop(i, j), 'Argon,   ', 100*frct(i, j) ! Argon
      !    if (iprop(i, j).eq.3) write(InArgumentsFile, 1115) iprop(i, j), 'Krypton, ', 100*frct(i, j) ! Krypton
      !    if (iprop(i, j).eq.4) write(InArgumentsFile, 1115) iprop(i, j), 'Xenon,   ', 100*frct(i, j) ! Xenon
      !  end do  ! j - mix loop
      !end if

      !if (mgas.eq.0) then ! show received gass properties:
        do j = 1, nmix(i)
          !if (iprop(i, j).eq.1) write(InArgumentsFile, 1115) iprop(i, j), ' ' 100*frct(i, j) ! Air
          write(InArgumentsFile, 1115) iprop(i, j), ' ', 100 * frct(i, j) ! gas
          !if (iprop(i, j).eq.2) write(InArgumentsFile, 1116) iprop(i, j), 100*frct(i, j) ! Argon
          !if (iprop(i, j).eq.3) write(InArgumentsFile, 1117) iprop(i, j), 100*frct(i, j) ! Krypton
          !if (iprop(i, j).eq.4) write(InArgumentsFile, 1118) iprop(i, j), 100*frct(i, j) ! Xenon
          write(InArgumentsFile, 1130) iprop(i, j), 100*frct(i, j)
          write(InArgumentsFile, 1131) xgcon(iprop(i, j), 1), xgcon(iprop(i, j), 2), xgcon(iprop(i, j), 3)
          write(InArgumentsFile, 1132) xgvis(iprop(i, j), 1), xgvis(iprop(i, j), 2), xgvis(iprop(i, j), 3)
          write(InArgumentsFile, 1133) xgcp(iprop(i, j), 1), xgcp(iprop(i, j), 2), xgcp(iprop(i, j), 3)
          write(InArgumentsFile, 1134) xwght(iprop(i, j))
        end do  ! - j - one mix
      !end if  ! MGAS = 1 - "table" gasses
    end do ! i - gas loop

    write(InArgumentsFile, *)
    write(InArgumentsFile, 1198)

    !close(InArgumentsFile)

    !!!!!!!!!!!!!!!!!!!
    !!!
    !!! Formats:
    !!!
    !!!!!!!!!!!!!!!!!!!

    10001  format('TARCOG v.', A, 'compiled ', A)
    !1000  format('TARCOG input arguments list - ',I4,'-',I2.2,'-',I2.2, ', ', I2.2,':',I2.2,':',I2.2)
    1000  format('TARCOG input arguments:')
    1001  format('TARCOG debug output, ',I4,'-',I2.2,'-',I2.2, ', ', I2.2,':',I2.2,':',I2.2)
    1002  format('     WindowID:', I8, '  - Not specified')
    1003  format('     WindowID:', I8, ' ')
    1006  format('     IGUID:   ', I8, '  - Not specified')
    1007  format('     IGUID:   ', I8, ' ')

    1005  format('Simulation parameters:')
    !1010  format('  Tout       =  ', F10.6,  ' - Outdoor temperature [K]')
    1010  format('  Tout       =  ', F10.6, ' K ( ',F7.3, ' deg C) - Outdoor temperature')
    !1015  format('  Tin        =  ', F10.6,  ' - Indoor temperature [K]')
    1015  format('  Tint       =  ', F10.6, ' K ( ',F7.3, ' deg C) - Indoor temperature')

    1014  format('Adjusted input arguments:')
    1013  format(' Gass coefficients:')
    !1016  format('  Trmout     =  ', F10.6,  ' - Outdoor mean radiant temperature [K]')
    1016  format('  Trmout     =  ', F10.6, ' K ( ',F7.3, ' deg C) - Outdoor mean radiant temp.')

    1017  format('  Gout       =  ', F10.6,  ' ')
    1018  format('  Gin        =  ', F10.6,  ' ')
    1019  format('  Ebsky      =  ', F10.6,  ' ')
    10191  format('  Ebroom     =  ', F10.6,  ' ')

    !1020  format('  Trmin      =  ', F10.6,  ' - Indoor mean radiant temperature [K]')
    1020  format('  Trmin      =  ', F10.6, ' K ( ',F7.3, ' deg C) - Indoor mean radiant temp.')
    1030  format('  wso        =  ', F7.3,  '    - Outdoor wind speed [m/s]')
    1032  format('  iwd        =    0        - Wind direction - windward')
    1033  format('  iwd        =    1        - Wind direction - leeward')
    1035  format('  wsi        =  ', F7.3,  '    - Indoor forced air speed [m/s]')
    1040  format('  dir        = ', F8.3,  '    - Direct solar radiation [W/m^2]')
    1041  format('  outir       = ', F8.3,  '    - IR radiation [W/m^2]')
    1045  format('  isky       =  ', I3,  '        - Flag for handling tsky, esky')
    !1050  format('  tsky       =  ', F10.6,  ' - Night sky temperature [K]')
    1050  format('  tsky           =  ', F10.6, ' K ( ',F7.3, ' deg C) - Night sky temperature')
    1055  format('  esky           =  ', F7.3,  '    - Effective night sky emmitance')
    1060  format('  fclr           =  ', F7.3,  '    - Fraction of sky that is clear')
    1061  format('  VacuumPressure =  ', F7.3,  '    - maximum allowed gas pressure to be considered as vacuum')
    1062  format('  VacuumMaxGapThickness =  ', F7.3,  '    - maximum allowed vacuum gap thickness with support pillar')
    1063  format('  ibc(1)         =  ', I3,  '        - Outdoor BC switch')
    1065  format('  hout           =  ', F9.5,  '  - Outdoor film coeff. [W/m^2-K]')
    1066  format('  ibc(2)         =  ', I3,  '        - Indoor BC switch')
    1068  format('  hin            =  ', F9.5,  '  - Indoor film coeff. [W/m^2-K]')

    1070  format('  standard   =  ', I3,  '        - ISO 15099 calc. standard')
    1071  format('  standard   =  ', I3,  '        - EN 673/ISO 10292 Declared calc. standard')
    1072  format('  standard   =  ', I3,  '        - EN 673/ISO 10292 Design calc. standard')

    10731  format('  ThermalMod =  ', I3,  '        - ISO15099 thermal model')
    10732  format('  ThermalMod =  ', I3,  '        - Scaled Cavity Width (SCW) thermal model')
    10733  format('  ThermalMod =  ', I3,  '        - Convective Scalar Model (CSM) thermal model')

    10740  format('  SDScalar =  ', F7.5,  '      - Factor of Venetian SD layer contribution to convection'// &
                      ' (only if ThermalModel = 2, otherwise ignored)')

    1075  format('IGU parameters:')
    1076  format('  height     =  ', F10.6,  ' - IGU cavity height [m]')
    1077  format('  heightt    =  ', F10.6,  ' - Total window height [m]')
    1078  format('  width      =  ', F10.6,  ' - Window width [m]')
    1079  format('  tilt       =  ', F7.3,  '    - Window tilt [deg]')
    1080  format('  totsol     =  ', F10.6,  ' - Total solar transmittance of IGU')
    1081  format('  nlayer     =  ', I3,  '        - Number of glazing layers')

    1089  format('IGU layers list:')

    10802  format(' Layer', I3, ' : ', I1, '              - Specular layer - Monolyhtic Glass')
    10803  format(' Layer', I3, ' : ', I1, '              - Laminated Glass')
    10804  format(' Layer', I3, ' : ', I1, '              - Venetian Blind')
    10805  format(' Layer', I3, ' : ', I1, '              - Woven Shade')
    10806  format(' Layer', I3, ' : ', I1, '              - Diffuse Shade')
    10809  format(' Layer', I3, ' : ', I1, '              - UNKNOWN TYPE!')

    1085  format('    nslice     = ', I3,    '          - Number of slices')
    1086  format('    LaminateA  = ', F12.8,  ' - A coeff.')
    1087  format('    LaminateB  = ', F12.8,  ' - B coeff.')
    1088  format('    sumsol     = ', F12.8,  ' - Absorbed solar energy [W/m^2]')

    1090  format('    thick   = ', F10.6,  '   - Thickness [m]')
    1091  format('    scon    = ', F10.6,  '   - Thermal conductivity [W/m-K]')
    1092  format('    asol    = ', F12.8,  ' - Absorbed solar energy [W/m^2]')
    1093  format('    tir     = ', F12.8,  ' - IR transmittance')
    1094  format('    emis1   = ', F10.6,  '   - IR outdoor emissivity')
    1095  format('    emis2   = ', F10.6,  '   - IR indoor emissivity')
    1100  format('    Atop    = ', F10.6,  '   - Top opening area [m^2]')
    1101  format('    Abot    = ', F10.6,  '   - Bottom opening area [m^2]')
    1102  format('    Al      = ', F10.6,  '   - Left opening area [m^2]')
    1103  format('    Ar      = ', F10.6,  '   - Right opening area [m^2]')
    1105  format('    Ah      = ', F10.6,  '   - Total area of holes [m^2]')

    11051  format('    SlatThick   = ', F10.6,  '   - Slat thickness [m]')
    11052  format('    SlatWidth   = ', F10.6,  '   - Slat width [m]')
    11053  format('    SlatAngle   = ', F10.6,  '   - Slat tilt angle [deg]')
    11054  format('    SlatCond    = ', F10.6,  '   - Conductivity of the slat material [W/m.K]')
    11055  format('    SlatSpacing = ', F10.6,  '   - Distance between slats [m]')
    11056  format('    SlatCurve   = ', F10.6,  '   - Curvature radius of the slat [m]')

    1110  format('IGU Gaps:')
    1111  format(' Gap ', I2,  ':')
    11110  format(' Outdoor space:')
    11111  format(' Indoor space:')

    1112  format('    gap        = ', F12.5,  ' - Gap width [m]')
    1113  format('    presure    = ', F12.5,  ' - Gas pressure [N/m^2]')
    1114  format('    nmix       = ', I6,  '       - Num. of gasses in a gas mix')
    1115  format('      Gas ', I1, ':     ',A,'     ', F6.2,' %')

    !1115  format('      Gas ', I1, ':     Air,     ', F6.2,' %')
    !1116  format('      Gas ', I1, ':     Argon,   ', F6.2,' %')
    !1117  format('      Gas ', I1, ':     Krypron, ', F6.2,' %')
    !1118  format('      Gas ', I1, ':     Xenon,   ', F6.2,' %')

    1120  format('    vvent      = ', F12.5,  ' - Forced ventilation speed [m/s]')
    1121  format('    tvent      = ', F12.5,  ' - Temperature in connected gap [K]')

    1130  format('      Gas mix coefficients - gas ', i1, ', ', F6.2,' %')
    1131  format('        gcon   = ', F11.6, ', ', F11.6, ', ', F11.6,  ' - Conductivity')
    1132  format('        gvis   = ', F11.6, ', ', F11.6, ', ', F11.6,  ' - Dynamic viscosity')
    1133  format('        gcp    = ', F11.6, ', ', F11.6, ', ', F11.6,  ' - Spec.heat @ const.P')
    1134  format('        wght   = ', F11.6,  '                           - Molecular weight')

    1198  format('=====  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====')
    !1199  format('-----  *****  -----  *****  -----  *****  -----  *****  -----  *****  -----')

  end subroutine

  subroutine WriteModifiedArguments(InArgumentsFile, DBGD, esky, trmout, trmin, ebsky, ebroom, Gout, Gin, &
                                    &  nlayer, LayerType, nmix, frct, thick, scon, gap, xgcon, xgvis, xgcp, xwght)

    use DataGlobals, only: KelvinConv

    integer, intent(in) :: InArgumentsFile
    integer, intent(in) :: nlayer
    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), intent(in) :: esky, trmout, trmin, ebsky, ebroom, Gout, Gin
    real(r64), dimension(maxlay), intent(in) :: thick, scon
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgvis, xgcp
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(MaxGap), intent(in) :: gap
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct

    character(len=1000), intent(in) :: DBGD

    integer i, j, nperr


    !open(unit=InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode, &
    !        &  form='formatted', iostat=nperr)
    !if (nperr.ne.0)  open(unit=InArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode, &
    !        &  form='formatted', iostat=nperr)
    write(InArgumentsFile, *)
    write(InArgumentsFile, 1014)
    write(InArgumentsFile, *)
    write(InArgumentsFile, 1055) esky
    write(InArgumentsFile, 1016) trmout,  trmout - KelvinConv
    write(InArgumentsFile, 1020) trmin,    trmin - KelvinConv
    write(InArgumentsFile, 1019) ebsky
    write(InArgumentsFile, 10191) ebroom
    write(InArgumentsFile, 1017) Gout
    write(InArgumentsFile, 1018) Gin
    write(InArgumentsFile, *)

    do i = 1, nlayer
      if (LayerType(i).eq.VENETBLIND) then ! SD layer
        write(InArgumentsFile, 1084) i, LayerType(i)
        write(InArgumentsFile, 1090) thick(i)
        write(InArgumentsFile, 1091) scon(i)
      end if
    end do
    write(InArgumentsFile, *)

    write(InArgumentsFile, 1013)
    do i = 1, nlayer+1 ! loop through gaps:
      if ((i.gt.1).and.(i.le.nlayer))  write(InArgumentsFile, 1111) i-1
      if ((i.gt.1).and.(i.le.nlayer))  write(InArgumentsFile, 1112) gap(i-1)
      if (i.eq.1)   write(InArgumentsFile, 11110)
      if (i.eq.nlayer+1)  write(InArgumentsFile, 11111)
  !    write(InArgumentsFile, 1111) i-1
      do j = 1, nmix(i)
        write(InArgumentsFile, 1130) j, 100*frct(i, j)
        write(InArgumentsFile, 1131) xgcon(j, 1), xgcon(j, 2), xgcon(j, 3)
        write(InArgumentsFile, 1132) xgvis(j, 1), xgvis(j, 2), xgvis(j, 3)
        write(InArgumentsFile, 1133) xgcp(j, 1), xgcp(j, 2), xgcp(j, 3)
        write(InArgumentsFile, 1134) xwght(j)
      end do  ! j - gas mix
    end do ! i - gaps
    write(InArgumentsFile, *)
    write(InArgumentsFile, 1198)
    !close(InArgumentsFile)

    !!!!!!!!!!!!!!!!!!!
    !!!
    !!! Formats:
    !!!
    !!!!!!!!!!!!!!!!!!!

    1014  format('Adjusted input arguments:')
    1013  format(' Gass coefficients:')
    1016  format('  Trmout     =  ', F10.6, ' K ( ',F7.3, ' deg C) - Outdoor mean radiant temp.')
    1017  format('  Gout       =  ', F10.6,  ' ')
    1018  format('  Gin        =  ', F10.6,  ' ')
    1019  format('  Ebsky      =  ', F10.6,  ' ')
    10191  format('  Ebroom     =  ', F10.6,  ' ')

    1020  format('  Trmin      =  ', F10.6, ' K ( ',F7.3, ' deg C) - Indoor mean radiant temp.')
    1055  format('  esky       =  ', F7.3,  '    - Effective night sky emmitance')

    1084  format(' Layer', I3, ' : ', I1, '              - Venetian Blind')
    1090  format('    thick   = ', F10.6,  '   - Thickness [m]')
    1091  format('    scon    = ', F10.6,  '   - Thermal conductivity [W/m-K]')

    1130  format('      Gas mix coefficients - gas ', i1, ', ', F6.2,' %')
    1131  format('        gcon   = ', F11.6, ', ', F11.6, ', ', F11.6,  ' - Conductivity')
    1132  format('        gvis   = ', F11.6, ', ', F11.6, ', ', F11.6,  ' - Dynamic viscosity')
    1133  format('        gcp    = ', F11.6, ', ', F11.6, ', ', F11.6,  ' - Spec.heat @ const.P')
    1134  format('        wght   = ', F11.6,  '                           - Molecular weight')

    1110  format('IGU Gaps:')
    1111  format(' Gap ', I2,  ':')
    1112  format(' Gap width: ', F11.8)
    11110  format(' Outdoor space:')
    11111  format(' Indoor space:')
    1198  format('=====  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====')

  end subroutine

  subroutine WriteOutputArguments(OutArgumentsFile, DBGD, nlayer, tamb, q, qv, qcgas, qrgas, theta, vfreevent, vvent, &
                                    &   Keff, ShadeGapKeffConv, troom, ufactor, shgc, sc, hflux, shgct, &
                                    &   hcin, hrin, hcout, hrout, Ra, Nu, LayerType, &
                                    &   Ebf, Ebb, Rf, Rb, Ebsky, Gout, Ebroom, Gin, &
                                    &   ShadeEmisRatioIn, ShadeEmisRatioOut, ShadeHcRatioIn, ShadeHcRatioOut, &
                                    &   HcUnshadedIn, HcUnshadedOut, hcgas, hrgas, AchievedErrorTolerance, NumOfIter)

    use DataGlobals, only: KelvinConv

    integer, intent(inout) :: OutArgumentsFile
    integer, intent(in) :: nlayer
    real(r64), intent(in) :: tamb, troom, shgct, hcin, hrin,hcout, hrout
    real(r64), dimension(maxlay3), intent(in) :: q
    real(r64), dimension(maxlay1), intent(in) :: qv
    real(r64), dimension(maxlay1), intent(in) :: qcgas
    real(r64), dimension(maxlay1), intent(in) :: qrgas
    integer, dimension(maxlay), intent(in) :: LayerType
    integer, intent(in) :: NumOfIter
    real(r64), intent(in) :: ufactor,sc,hflux,shgc
    real(r64), dimension(maxlay2), intent(in) :: theta
    real(r64), dimension(maxlay), intent(in) :: hcgas, hrgas
    real(r64), dimension(maxlay1), intent(in) :: vfreevent
    real(r64), dimension(maxlay1), intent(in) :: vvent

    real(r64), dimension(maxlay), intent(in) :: Ra, Nu

    real(r64), intent(in) :: ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn
    real(r64), intent(in) :: HcUnshadedOut, HcUnshadedIn
    real(r64), dimension(maxlay), intent(in) :: Keff
    real(r64), dimension(MaxGap), intent(in) :: ShadeGapKeffConv

    real(r64), intent(in) ::  Ebsky, Gout, Ebroom, Gin
    real(r64), dimension(maxlay), intent(in) ::  Ebb, Ebf, Rb, Rf

    real(r64), intent(in) :: AchievedErrorTolerance

    character(len=1000), intent(in) :: DBGD

    integer, dimension(8) :: DATE_TIME
    character(len = 12), dimension(3) :: real_CLOCK


    integer i, nperr

    !open(unit=OutArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode, &
    !      & form='formatted', iostat=nperr)
    !if (nperr.ne.0)  open(unit=OutArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode, &
    !      & form='formatted', iostat=nperr)
    call DATE_AND_TIME (real_CLOCK (1), real_CLOCK (2), real_CLOCK (3), DATE_TIME)
    write(OutArgumentsFile,*)
    write(OutArgumentsFile, 2000) DATE_TIME(1), DATE_TIME(2), DATE_TIME(3), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2350)
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2105) tamb,    tamb - KelvinConv
    write(OutArgumentsFile,2180) q(1)

    !bi  Write out layer properties:
    do i = 1, nlayer
      !        write(OutArgumentsFile, 2110) 2*i-1, theta(2*i-1), theta(2*i-1)-273.15
      select case (LayerType(i))
        case (SPECULAR)                             ! Specular layer
          write(OutArgumentsFile, 2110) 2*i-1, theta(2*i-1), theta(2*i-1) - KelvinConv
          write(OutArgumentsFile, 2190) i, q(2*i)
          write(OutArgumentsFile, 2110) 2*i, theta(2*i), theta(2*i) - KelvinConv
        case (VENETBLIND)                             ! Venetian blind
          write(OutArgumentsFile, 2111) 2*i-1, theta(2*i-1), theta(2*i-1) - KelvinConv
          write(OutArgumentsFile, 2195) i, q(2*i), i, ShadeGapKeffConv(i)
          write(OutArgumentsFile, 2111) 2*i, theta(2*i), theta(2*i) - KelvinConv
        case (WOVSHADE)                             ! Venetian blind
          write(OutArgumentsFile, 2112) 2*i-1, theta(2*i-1), theta(2*i-1) - KelvinConv
          write(OutArgumentsFile, 2195) i, q(2*i), i, ShadeGapKeffConv(i)
          write(OutArgumentsFile, 2112) 2*i, theta(2*i), theta(2*i) - KelvinConv
        case (DIFFSHADE)                             ! Venetian blind
          write(OutArgumentsFile, 2110) 2*i-1, theta(2*i-1), theta(2*i-1) - KelvinConv
          write(OutArgumentsFile, 2190) i, q(2*i)
          write(OutArgumentsFile, 2110) 2*i, theta(2*i), theta(2*i) - KelvinConv
        case DEFAULT
          write(OutArgumentsFile, 2110) 2*i-1, theta(2*i-1), theta(2*i-1) - KelvinConv
          write(OutArgumentsFile, 2199) i, q(2*i)
          write(OutArgumentsFile, 2110) 2*i, theta(2*i), theta(2*i) - KelvinConv
      end select

  !    write(OutArgumentsFile, 2110) 2*i, theta(2*i), theta(2*i)-273.15

      !bi  Write out gap properties:
      if (i.ne.nlayer) then
        write(OutArgumentsFile, 2300) i, q(2*i + 1)
        write(OutArgumentsFile, 2320) i, qv(i+1)
        if (vvent(i+1).eq.0) then
          write(OutArgumentsFile, 2321) i, vfreevent(i+1), i, Keff(i)
        else
          if (i > 1) then
            write(OutArgumentsFile, 2321) i, vvent(i+1), i, Keff(i-1) !Objexx:BoundsViolation Keff(i-1) @ i=1
          end if
        end if
        write(OutArgumentsFile, 2322) i, qcgas(i+1), i, qrgas(i+1)
  !      write(OutArgumentsFile, 2323) i, Keff(i)
        !write(OutArgumentsFile, 2310) i, qprim(2*i + 1)
      else
        write(OutArgumentsFile, 2210) q(2*i + 1)
      end if
    end do  ! i - layers

    write(OutArgumentsFile,2115) troom,    troom - KelvinConv

    write(OutArgumentsFile,*)

    !Simon: Write energy balances on layer surfaces
    write(OutArgumentsFile, 4350)
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,4205) Ebsky, Gout
    write(OutArgumentsFile,*)

    do i = 1, nlayer
      select case (LayerType(i))
        case (SPECULAR)                             ! Specular layer
          write(OutArgumentsFile, 4110) i, Ebf(i), i, Rf(i)
          write(OutArgumentsFile, 4111)
          write(OutArgumentsFile, 4190)
          write(OutArgumentsFile, 4121)
          write(OutArgumentsFile, 4120) i, Ebb(i), i, Rb(i)
        case (VENETBLIND)                             ! Venetian blind
          write(OutArgumentsFile, 4112) i, Ebf(i), i, Rf(i)
          write(OutArgumentsFile, 4113)
          write(OutArgumentsFile, 4190)
          write(OutArgumentsFile, 4123)
          write(OutArgumentsFile, 4122) i, Ebb(i), i, Rb(i)
        case (WOVSHADE)                             ! Venetian blind
          write(OutArgumentsFile, 4114) i, Ebf(i), i, Rf(i)
          write(OutArgumentsFile, 4115)
          write(OutArgumentsFile, 4190)
          write(OutArgumentsFile, 4125)
          write(OutArgumentsFile, 4124) i, Ebb(i), i, Rb(i)
        case (DIFFSHADE)
          write(OutArgumentsFile, 4116) i, Ebf(i), i, Rf(i)
          write(OutArgumentsFile, 4117)
          write(OutArgumentsFile, 4190)
          write(OutArgumentsFile, 4127)
          write(OutArgumentsFile, 4126) i, Ebb(i), i, Rb(i)
        case DEFAULT
          write(OutArgumentsFile, 4110) i, Ebf(i), i, Rf(i)
          write(OutArgumentsFile, 4111)
          write(OutArgumentsFile, 4190)
          write(OutArgumentsFile, 4121)
          write(OutArgumentsFile, 4120) i, Ebb(i), i, Rb(i)
        end select
        write(OutArgumentsFile,*)
    end do

    write(OutArgumentsFile,4215) Ebroom, Gin

    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2351)
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2120) ufactor
    write(OutArgumentsFile,2130) shgc
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2132) sc
    write(OutArgumentsFile, 2170) hflux
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2131) shgct
    write(OutArgumentsFile,*)
    write(OutArgumentsFile,2140) hcin, hrin, hcin+hrin
    write(OutArgumentsFile,2150) hcout, hrout, hcout+hrout

    write(OutArgumentsFile,*)
    do i=1,nlayer-1
      write(OutArgumentsFile,2155) i,Ra(i),i,Nu(i)
    end do
    write(OutArgumentsFile,*)
    write(OutArgumentsFile, 2330) ShadeEmisRatioIn, ShadeEmisRatioOut
    write(OutArgumentsFile, 2331) ShadeHcRatioIn, ShadeHcRatioOut
    write(OutArgumentsFile, 2332) HcUnshadedIn, HcUnshadedOut

    write(OutArgumentsFile,*)
    do i=2,nlayer
      write(OutArgumentsFile,2160) i, hcgas(i), i, hrgas(i)
    end do

    write(OutArgumentsFile,*)
    write(OutArgumentsFile, '("  Error Tolerance = ", e12.6)') AchievedErrorTolerance

    write(OutArgumentsFile,*)
    write(OutArgumentsFile, '("  Number of Iterations = ", i6)') NumOfIter

    !  write(OutArgumentsFile, *)
    !  write(OutArgumentsFile, 3333) flux_nonsolar, qeff

    !close(OutArgumentsFile)


    !!!!!!!!!!!!!!!!!!!
    !!!
    !!! Formats:
    !!!
    !!!!!!!!!!!!!!!!!!!

    2000  format('TARCOG calculation results - ',I4,'-',I2.2,'-',I2.2, ', ', I2.2,':',I2.2,':',I2.2)
    !2101  format(' SHGC =   ',F8.6,6x,' SHGC_OLD = ',F8.6,2x,' SC = ',F8.6)
    !2110  format(' Theta(',I3,') = ',F12.6)
    !2111  format(/'Pane #:', I3/)
    !2112  format('Number of panes: ',I2)
    !2113  format('    Thetaslice(',I3,',',I3') = ',F12.6)
    2120  format('  Ufactor  = ',F12.6)
    2130  format('  SHGC     = ',F12.6)
    2131  format('  SHGC_OLD = ',F12.6)
    2132  format('  SC       = ',F12.6)


    2140  format('  hcin  = ',F10.6,3x,'hrin  = ',F10.6,3x,'hin  = ',F10.6)
    2150  format('  hcout = ',F10.6,3x,'hrout = ',F10.6,3x,'hout = ',F10.6)
    2155  format('  Ra(',I1,') =',F15.6,'        Nu(',I1,') =',F12.6)
    2160  format('  hcgas(',I1,') =',F15.6,'      hrgas(',I1,') =',F24.6)

    2165  format('  rhum  =',F15.6,'        rhout =',F12.6)
    2170  format('  hflux    = ',F12.6)


    !2105  format('                                            Tamb =',F11.6)
    2105  format('                                            Tamb =',F11.6, ' K ( ',F7.3, ' deg C)')
    !2110  format('  ----------------- ------------------   Theta',I2,' =',F11.6)
    2110  format('  ----------------- ------------------   Theta',I2,' =',F11.6, ' K ( ',F7.3, ' deg C)')
    2111  format('  \\\\\\\\\\\\\\\\\ \\\\\\\\\\\\\\\\\\   Theta',I2,' =',F11.6, ' K ( ',F7.3, ' deg C)')
    2112  format('  +++++++++++++++++ ++++++++++++++++++   Theta',I2,' =',F11.6, ' K ( ',F7.3, ' deg C)')
    2113  format('  ooooooooooooooooo oooooooooooooooooo   Theta',I2,' =',F11.6, ' K ( ',F7.3, ' deg C)')
    !2115  format('                                           Troom =',F11.6)
    2115  format('                                           Troom =',F11.6, ' K ( ',F7.3, ' deg C)')

    2180  format('           qout =',    F12.5)
    2190  format('  |     qpane', i2,' =',  F12.5,'        |')
    !2190  format('  |       qpane', i2,' =',  F11.6,'       |         keffc', i2,' =',  F11.6)
    !2195  format('  |////   qpane', i2,' =',  F11.6,'   ////|')
    2195  format('  |     qpane', i2,' =',  F12.5,'        |         keffc', i2,' =',  F11.6)
    2199  format('  |      qlayer', i2,' =',  F12.5,'       |')
    2210  format('            qin =',    F11.6)
    2300  format('            q', i2,' =',  F12.5)
    2310  format('        qprim',  i2,' =',  F12.5)
    2320  format('           qv', i2,' =',  F12.5) !,  '  ( airspeed = ', F12.6, ' )')
    2321  format('       airspd', i2,' =',  F12.5, '    keff', i2,' =', F12.5)
    2322  format('           qc', i2,' =',  F12.5, '      qr', i2,' =', F12.5)

    !2323  format('         keff', i2,' =',  F12.5)

    2330  format('  ShadeEmisRatioIn  =',F11.6,'        ShadeEmisRatioOut =',F11.6)
    2331  format('  ShadeHcRatioIn    =',F11.6,'        ShadeHcRatioOut   =',F11.6)
    2332  format('  HcUnshadedIn      =',F11.6,'        HcUnshadedOut     =',F11.6)

    2340  format('  ')
    2350  format('Heat Flux Flow and Temperatures of Layer Surfaces:')
    2351  format('Basic IGU properties:')

    2220  format('  he = ',F8.4,',',3x,'hi = ',F8.4)
    2230  format('  hg',I2,' =',E15.6,'      hr',I2,' =',E15.6,'      hs',I2,' =',E15.6)

    3333  format('Flux (non-solar pass): ',F12.6, ' ; Flux per W7: ', F12.6)

    4205  format('  Ebsky =',F11.6, ' [W/m2], Gout =',F11.6, ' [W/m2]')
    4215  format('  Ebroom =',F11.6, ' [W/m2], Gin  =',F11.6, ' [W/m2]')

    4110  format('  Ef', I2,' =', F11.6, ' [W/m2], Rf', I2,' =', F11.6, ' [W/m2]')
    4111  format('  ----------------- ------------------')
    4112  format('  Ef', I2,' =', F11.6, ' [W/m2], Rf', I2,' =', F11.6, ' [W/m2]')
    4113  format('  \\\\\\\\\\\\\\\\\ \\\\\\\\\\\\\\\\\\')
    4114  format('  Ef', I2,' =', F11.6, ' [W/m2], Rf', I2,' =', F11.6, ' [W/m2]')
    4115  format('  +++++++++++++++++ ++++++++++++++++++')
    4116  format('  Ef', I2,' =', F11.6, ' [W/m2], Rf', I2,' =', F11.6, ' [W/m2]')
    4117  format('  ooooooooooooooooo oooooooooooooooooo')

    4120  format('  Eb', I2,' =', F11.6, ' [W/m2], Rb', I2,' =', F11.6, ' [W/m2]')
    4121  format('  ----------------- ------------------')
    4122  format('  Eb', I2,' =', F11.6, ' [W/m2], Rb', I2,' =', F11.6, ' [W/m2]')
    4123  format('  \\\\\\\\\\\\\\\\\ \\\\\\\\\\\\\\\\\\')
    4124  format('  Eb', I2,' =', F11.6, ' [W/m2], Rb', I2,' =', F11.6, ' [W/m2]')
    4125  format('  +++++++++++++++++ ++++++++++++++++++')
    4126  format('  Eb', I2,' =', F11.6, ' [W/m2], Rb', I2,' =', F11.6, ' [W/m2]')
    4127  format('  ooooooooooooooooo oooooooooooooooooo')

    4190  format('  |                     |')

    4350  format('Energy balances on Layer Surfaces:')


  end subroutine

  subroutine WriteOutputEN673(OutArgumentsFile, DBGD, nlayer, ufactor, hout, hin, Ra, Nu, hg, hr, hs, nperr)

    integer, intent(inout) :: OutArgumentsFile
    integer, intent(in) :: nlayer
    character(len=*), intent(in) :: DBGD
    real(r64), intent(in) :: ufactor, hout, hin
    real(r64), dimension(maxlay), intent(in) :: Ra, Nu
    real(r64), dimension(maxlay), intent(in) :: hg, hr, hs

    integer, intent(inout) :: nperr
    !character*(*), intent (inout) :: ErrorMessage


    integer, dimension(8) :: DATE_TIME
    character(len = 12), dimension(3) :: real_CLOCK

    integer :: i

    !open(unit=OutArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode,  &
    !      &  form='formatted', iostat=nperr)
    !if (nperr.ne.0)  open(unit=OutArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode,  &
    !      &  form='formatted', iostat=nperr)
    call DATE_AND_TIME (real_CLOCK (1), real_CLOCK (2), real_CLOCK (3), DATE_TIME)
    write(OutArgumentsFile, *)
    write(OutArgumentsFile, 2000) DATE_TIME(1), DATE_TIME(2), DATE_TIME(3), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
    write(OutArgumentsFile, *)

    write(OutArgumentsFile, *)
    write(OutArgumentsFile, 2351)
    write(OutArgumentsFile, *)
    write(OutArgumentsFile, 2120) ufactor
    write(OutArgumentsFile, *)
    write(OutArgumentsFile, 2220) hout, hin
    write(OutArgumentsFile, *)
    do i=1,nlayer-1
      write(OutArgumentsFile, 2155) i,Ra(i),i,Nu(i)
    end do
    write(OutArgumentsFile, *)
    do i=1,nlayer-1
      write(OutArgumentsFile, 2230) i, hg(i), i, hr(i), i, hs(i)
    end do
    !close(OutArgumentsFile)

    !!!!!!!!!!!!!!!!!!!
    !!!
    !!! Formats:
    !!!
    !!!!!!!!!!!!!!!!!!!

    2000  format('TARCOG calculation results - ',I4,'-',I2.2,'-',I2.2, ', ', I2.2,':',I2.2,':',I2.2)
    2351  format('Basic IGU properties:')
    2120  format('  Ufactor  = ',F12.6)
    2220  format('  he = ',F8.4,',',3x,'hi = ',F8.4)
    2155  format('  Ra(',I1,') =',F15.6,'        Nu(',I1,') =',F12.6)
    2230  format('  hg',I2,' =',E15.6,'      hr',I2,' =',E15.6,'      hs',I2,' =',E15.6)

  end subroutine

  subroutine WriteTARCOGInputFile( VerNum, tout, tind, trmin,  &
                                  &  wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, &
                                  &  CalcDeflection, Pa, Pini, Tini ,ibc, hout, hin,  &
                                  &  standard, ThermalMod, SDScalar, height, heightt, width, tilt, totsol,  &
                                  &  nlayer, LayerType, thick, scon, YoungsMod, PoissonsRat, asol, tir, emis, &
                                  &  Atop, Abot, Al, Ar, Ah, SupportPillar, PillarSpacing, PillarRadius, &
                                  &  SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve,  &
                                  &  nslice, gap, GapDef, vvent, tvent,  &
                                  &  presure, nmix, iprop, frct, xgcon, xgvis, xgcp, xwght, gama)

    use TARCOGGassesParams

    integer, intent(in) :: iwd
    integer, intent(in) :: isky, CalcDeflection
    real(r64), dimension(maxlay), intent(in) :: Atop, Abot, Al, Ar, Ah
    real(r64), dimension(maxlay), intent(in) :: SlatThick, SlatWidth, SlatAngle
    real(r64), dimension(maxlay), intent(in) :: SlatCond, SlatSpacing, SlatCurve
    real(r64), dimension(maxlay1), intent(in) :: vvent, tvent
    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), intent(in) :: width, fclr, SDScalar, height, heightt
    real(r64), intent(in) :: tout, tind, wso, wsi, dir, outir, tsky, esky, totsol, tilt, hout, hin,trmin
    real(r64), intent(in) :: VacuumPressure, VacuumMaxGapThickness, Pa, Pini, Tini
    real(r64), dimension(maxlay), intent(in) :: gap, thick, scon, asol, YoungsMod, PoissonsRat
    real(r64), dimension(MaxGap), intent(in) :: GapDef
    real(r64), dimension(maxlay2), intent(in) :: tir, emis
    real(r64), dimension(maxlay1,maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: presure
    integer, dimension(maxlay1), intent(in) :: nmix
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon
    real(r64), dimension(maxgas, 3), intent(in) :: xgvis
    real(r64), dimension(maxgas, 3), intent(in) :: xgcp
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(maxgas), intent(in) :: gama

    !Support Pillars
    integer, dimension(maxlay), intent(in) :: SupportPillar     ! Shows whether or not gap have support pillar
                                                                !   0 - does not have support pillar
                                                                !   1 - have support pillar
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing   ! Pillar spacing for each gap (used in case there is support pillar)
    real(r64), dimension(maxlay), intent(in) :: PillarRadius    ! Pillar radius for each gap (used in case there is support pillar)


    integer, intent(in) :: nlayer
    integer, dimension(2), intent(in) :: ibc

    integer, dimension(maxlay), intent(in) :: nslice
    integer, intent(in) :: standard, ThermalMod

    integer :: i, j
    integer :: NumOfProvGasses

    character(len=10), intent(in) ::  VerNum

    integer, dimension(8) :: DATE_TIME
    character(len=12), dimension(3) :: real_CLOCK

    integer :: nperr

    character(len=1024) :: dynFormat = ''

      !open(unit=WINCogFile,  file=TRIM(DBGD)//TRIM(WinCogFileName),  status='unknown', access=FileMode, &
      !       &  form='formatted', iostat=nperr)
      !if (nperr.ne.0) open(unit=WINCogFile,  file=TRIM(WinCogFileName),  status='unknown', access=FileMode, &
      !                      & form='formatted', iostat=nperr)
    !else
    !  open(unit=WINCogFile,  file=TRIM(DBGD)//TRIM(SHGCFileName),  status='unknown', access=FileMode, &
    !         & form='formatted', iostat=nperr)
    !  if (nperr.ne.0) open(unit=WINCogFile,  file=TRIM(SHGCFileName),  status='unknown', access=FileMode, &
    !                        & form='formatted', iostat=nperr)
    !end if

  !bi...Create WINCOG input file using Tarcog's input arguments:

  !bi...Write the header:

    call DATE_AND_TIME (real_CLOCK (1), real_CLOCK (2), real_CLOCK (3), DATE_TIME)

    write(WINCogFile,112)
    write(WINCogFile,111)
    write(WINCogFile, 1001) DATE_TIME(1), DATE_TIME(2), DATE_TIME(3), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
    write(WINCogFile, 10001) VerNum   !, VerDat
    write(WINCogFile,111)

    if (winID.eq.-1) then
      write(WINCogFile, 1002) winID
    else
      write(WINCogFile, 1003) winID
    end if
    if (iguID.eq.-1) then
      write(WINCogFile, 1006) iguID
    else
      write(WINCogFile, 1007) iguID
    end if

    write(WINCogFile, 1008) nlayer
    write(WINCogFile,111)
    write(WINCogFile,112)

  !bi...Write main body:

    write(WINCogFile,113)
    write(WINCogFile,200)
    write(WINCogFile,113)
    write(WINCogFile,210)
    write(WINCogFile, 1010) nlayer, 2, standard, ThermalMod, CalcDeflection, SDScalar, VacuumPressure, VacuumMaxGapThickness

    write(WINCogFile,113)
    write(WINCogFile,300)
    write(WINCogFile,113)
    write(WINCogFile,310)
    write(WINCogFile, 1020) tout, tind, wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, trmin, Pa, Pini, Tini

    !if (mgas.eq.0) then
      NumOfProvGasses = 0
      do while (xwght(NumOfProvGasses + 1).ne.0)
        NumOfProvGasses = NumOfProvGasses + 1
      end do
      write(WINCogFile,113)
      write(WINCogFile, 2000)
      write(WINCogFile,113)
      write(WINCogFile, 2011)
      write(WINCogFile, 2010) NumOfProvGasses
      do i = 1, NumOfProvGasses
        write(WINCogFile, 2021)
        write(WINCogFile, 2020) xwght(i)
        write(WINCogFile, 2031)
        write(WINCogFile, 2030) (xgcon(i, j),j=1,3)
        write(WINCogFile, 2032)
        write(WINCogFile, 2030) (xgvis(i, j),j=1,3)
        write(WINCogFile, 2033)
        write(WINCogFile, 2030) (xgcp(i, j),j=1,3)
        write(WINCogFile, 2034)
        write(WINCogFile, 2020) gama(i)
      end do !i = 1, NumProvGasses
    !end if

    write(WINCogFile,113)
    write(WINCogFile,400)
    write(WINCogFile,113)
    write(WINCOGFile,410)
    write(WINCogFile, 1030) totsol, tilt, height, heightt, width

    !write(WINCogFile,500)
    !write(WINCogFile,*) SlatLength, SlatSpacing, SlatAngle, &
    !    CurvatureRadius, SubdivisionNumber, Rf, Rb, T, Ef_IR, Eb_IR, T_IR, &
    !    NumThetas, SOLMethod, FIRMethod, SOLhemCalc, SOLdifCalc
    !write(WINCogFile,*) '    0.016, 0.012, 45, 0.0, 5, 0, 0.70, 0.40, 0.00, 0.90, 0.90, 0.00, 1, 1, 1, 1'
    !write(WINCogFile,117)
    !write(WINCogFile,*) '    0.003, 0.01, 0.8, 0.6, 1, 1, 1'
    !write(WINCogFile,118)
    !write(WINCogFile,*) '    1'
    !write(WINCogFile,*) '    0'
    !write(WINCogFile,*) '    1'
    !write(WINCogFile,*) '    0'

    write(WINCogFile,113)
    write(WINCogFile,600)
    write(WINCogFile,113)
    write(WINCogFile,610)
    write(WINCogFile, 1040) ibc(1), hout, presure(1), 1, 1, 1.0, vvent(1), tvent(1)

    write(WINCogFile,700)

    do i = 1, nlayer
      write(WINCogFile,113)
      if (LayerType(i).eq.SPECULAR) then
        write(WINCogFile, 1060) i
      else if (LayerType(i).eq.VENETBLIND) then
        write(WINCogFile, 1061) i
      else if (LayerType(i).eq.WOVSHADE) then
        write(WINCogFile, 1062) i
      else if (LayerType(i).eq.DIFFSHADE) then
        write(WINCogFile, 1063) i
      else
        write(WINCogFile, 1064) i
      end if
      write(WINCogFile,113)

      write(WINCogFile, 1050)
      write(WINCogFile, 1051) scon(i), asol(i), thick(i), emis(2*i-1), emis(2*i), tir(2*i-1), YoungsMod(i), &
                                PoissonsRat(i), LayerType(i), nslice(i)

      if (IsShadingLayer(LayerType(i))) then
        write(WINCogFile, 1052)
        write(WINCogFile, 1053) Atop(i), Abot(i), Al(i), Ar(i), Ah(i)
      end if

      if (LayerType(i).eq.VENETBLIND) then
        write(WINCogFile, 1054)
        write(WINCogFile, 1055) SlatThick(i), SlatWidth(i), SlatAngle(i), SlatCond(i), SlatSpacing(i), SlatCurve(i)
      end if

      if (i.lt.nlayer) then
        write(WINCogFile,113)
        write(WINCogFile, 1049) i
        write(WINCogFile,113)
        write(WINCogFile, 1048)
        !build dynamic formating for various gas line possibilities
        dynFormat = '("    ", F24.12, ", ", F24.12, ", ", f24.12, ", ", i1, ", "'
        do j = 1, nmix(i+1)
          dynFormat = TRIM(dynFormat)//', i1, ", "'
        end do !j = 1, nmix(i+1)
        do j = 1, nmix(i+1)
          dynFormat = TRIM(dynFormat)//', f24.12, ", "'
        end do !j = 1, nmix(i+1)
        dynFormat = TRIM(dynFormat)//', f24.12, ", ", f24.12, ", ", i1)'
        write(WINCogFile, dynFormat) gap(i), GapDef(i), presure(i+1), nmix(i+1), (iprop(i+1, j), j=1,nmix(i+1)), &
          (frct(i+1, j), j=1,nmix(i+1)), vvent(i+1), tvent(i+1), SupportPillar(i)
        if (SupportPillar(i).eq.YES_SupportPillar) then
          write(WINCogFile, 1034)
          write(WINCogFile, 1035) PillarSpacing(i), PillarRadius(i)
        end if !if (SupportPillar(i).eq.YES_SupportPillar) then
      end if
    end do  !  i - layers

    write(WINCogFile,113)
    write(WINCogFile,800)
    write(WINCogFile,113)

    write(WINCogFile,810)
    write(WINCogFile, 1040) ibc(2), hin, presure(nlayer+1), 1, 1, 1.0, vvent(nlayer+1), tvent(nlayer+1)

    write(WINCogFile,113)
    write(WINCogFile,900)
    write(WINCogFile,113)
    !  write(WINCogFile, 1198)
    write(WINCogFile,*)

    !close(WINCogFile)

    !!!!!!!!!!!!!!!!!!!
    !!!
    !!! Formats:
    !!!
    !!!!!!!!!!!!!!!!!!!

    111  format('*')
    112  format('* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *')
    113 format('*------------------------------------------------------------')
    200  format('* General options:')
    210 format('* <nlayer, debug, standard, ThermalMod, CalcDeflection, SDScalar, VacuumPressure, VacuumMaxGapThickness>')
    300  format('* Environmental settings:')
    310 format('* <tout, tind, wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, trmin, Pa, Pini, Tini>')
    400  format('* Overall IGU properties:')
    410 format('* <totsol, tilt, height, heightt, width>')
    600  format('* Outdoor environment:')
    610 format('* <ibc(1), hout, presure(1), 1, 1, 1.0, vvent(1), tvent(1)>')
    700  format('* IGU definition:')
    800  format('* Indoor environment:')
    810 format('* <ibc(2), hin, presure(nlayer+1), 1, 1, 1.0, vvent(nlayer+1), tvent(nlayer+1)>')
    900  format('* End file')

    10001  format('* created by TARCOG v. ', A) ! , 'compiled ', A)
    1001  format('* TARCOG debug output for WinCOG, ',I4,'-',I2.2,'-',I2.2, ', ', I2.2,':',I2.2,':',I2.2)
    1002  format('*     WindowID:   ', I8, '  - Not specified')
    1003  format('*     WindowID:   ', I8, ' ')
    1006  format('*     IGUID:      ', I8, '  - Not specified')
    1007  format('*     IGUID:      ', I8, ' ')
    1008  format('*     Num Layers: ', I8, ' ')

    !  General:
    1010  format('    ',I1,', ',I1,', ',I1,', ',I1,', ',I1,', ',F24.12, ', ', F24.12, ', ', F24.12)
    1020  format('    ',F24.12,', ',F24.12,', ',F24.12,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',I1,', ',F24.12,', ', &
                        F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)
    1030  format('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)
    1031  format('    ',F24.12,', ',F24.12,', ',I3,', ',F24.12,', ',I3,', ',I3,', ',F24.12,', ',F24.12,', ',F24.12,', ', &
                        F24.12,', ',F24.12,', ',F24.12,', ',I2,', ',I2,', ',I2,', ',I2)
    1034  format('* <PillarSpacing(i), PillarRadius(i)')
    1035  format('    ',F24.12,', ',F24.12)

    !  Gaps/environment:
    1040  format('    ',I1,', ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12)
    1048  format('* <gap(i), GapDef(i), presure(i+1), nmix(i+1), (iprop(i+1, j), j=1,nmix(i+1)), (frct(i+1, j), '//&
                  'j=1,nmix(i+1)), vvent(i), tvent(i), SupportPillar(i)>')
    1049  format('* Gap ',I1,':')
    1041  format('    ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12)
    1042  format('    ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)
    1043  format('    ',F24.12,', ',F24.12,', ',I1,', ',I1,', ',I1,', ',I1,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ', &
                        F24.12)

    !  Layers:
    1050  format('* <scon(i), asol(i), thick(i), emis(2*i-1), emis(2*i), tir(2*i-1), YoungsMod(i),'// &
                  ' PoissonsRat(i), LayerType(i), nslice(i)>')
    1051  format('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',I1,', ',I1)
    1052  format('* <Atop(i), Abot(i), Al(i), Ar(i), Ah(i)>')
    1053  format('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)
    1054  format('* <SlatThick(i), SlatWidth(i), SlatAngle(i), SlatCond(i), SlatSpacing(i), SlatCurve(i)>')
    1055  format('    ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12,', ',F24.12)

    1060  format('* Layer ',I1,' - specular-glass:')
    1061  format('* Layer ',I1,' - venetian blind:')
    1062  format('* Layer ',I1,' - woven shade:')
    1063  format('* Layer ',I1,' - diffuse shade:')
    1064  format('* Layer ',I1,' - ???:')

    2000 format('* Gas coefficients information')
    2010  format('    ',I2)
    2011  format('* <NumberOfGasses>')
    !2020  format('    ',F12.6)
    2020  format('    ',ES12.6)
    2021  format('* <MolecularWeight>')
    2030  format('    ',ES12.6,', ',ES12.6,', ',ES12.6)
    2031  format('* <gconA, gconB, gconC>')
    2032  format('* <gvisA, gvisB, gvisC>')
    2033  format('* <gcpA, gcpB, gcpC>')
    2034  format('* <Gamma>')


    1198  format(' *************************************************')

  end subroutine

  subroutine FinishDebugOutputFiles(nperr)

    integer, intent(in) :: nperr
    integer :: ferr

    if (WriteDebugOutput) then
      !open(unit=OutArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access='append',  &
      !      &  form='formatted', iostat=ferr)
      !if (ferr.ne.0) open(unit=OutArgumentsFile,  file=DebugOutputFileName,  status='unknown', access='append',  &
      !      &  form='formatted', iostat=ferr)

      write(OutArgumentsFile,*)
      if ((nperr.gt.0).and.(nperr.lt.1000)) then
        write(OutArgumentsFile, 2362) nperr
      else if ((nperr.ge.1000)) then
          write(OutArgumentsFile, 2361) nperr
        else
          write(OutArgumentsFile, 2360) nperr
      end if

      write(OutArgumentsFile,*)
      write(OutArgumentsFile,1199)
      write(OutArgumentsFile,1199)

      !close(OutArgumentsFile)
    end if ! debug

    ! Close debug files
    if (InArgumentsFile /= statusClosed) then
      close(InArgumentsFile)
      InArgumentsFile = statusClosed
      OutArgumentsFile = statusClosed ! This is same is InArgumentsFile
    end if

    if (WINCOGFile /= statusClosed) then
      close(WINCOGFile)
      WINCOGFile = statusClosed
    end if

    if (IterationCSVFileNumber /= statusClosed) then
      close(IterationCSVFileNumber)
      IterationCSVFileNumber = statusClosed
    end if

    if (TarcogIterationsFileNumber /= statusClosed) then
      close(TarcogIterationsFileNumber)
      TarcogIterationsFileNumber = statusClosed
    end if

    !!!!!!!!!!!!!!!!!!!
    !!!
    !!! Formats:
    !!!
    !!!!!!!!!!!!!!!!!!!

    2360  format('TARCOG status: ', I3, ' - Normal termination.')
    2361  format('TARCOG status: ', I3, ' - Warning!')
    2362  format('TARCOG status: ', I3, ' - Error!')

    1199  format('#####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####')

  end subroutine FinishDebugOutputFiles

  subroutine PrepDebugFilesAndVariables(Debug_dir, Debug_file, Debug_mode, win_ID, igu_ID, nperr)

    integer, intent(in) :: Debug_mode
    character(len=*), intent(in) :: Debug_dir
    character(len=*), intent(in) :: Debug_file
    integer, intent(in) :: win_ID, igu_ID
    integer, intent(inout) :: nperr

    character :: LastPathChar
    integer :: LastPathCharIndex

    DBGD = TRIM(Debug_dir)

    LastPathCharIndex = LEN_TRIM(Debug_dir)
    if (LastPathCharIndex > 0) then
      LastPathChar = Debug_Dir(LastPathCharIndex:LastPathCharIndex)
      if (LastPathChar.ne.'/') DBGD = TRIM(Debug_dir)//'/'
      if ((LastPathChar.eq.'/').and.(LastPathCharIndex.eq.1)) DBGD = '';
    end if

    !DebugDir = Debug_dir
    DebugMode = Debug_mode
    winID = win_ID
    iguID = igu_ID

    !setup file names if file name is provided, otherwise keep default
    if (Trim(Debug_file).ne.'') then
      WinCogFileName = Trim(Debug_file)//'.w7'
      !SHGCFileName = Trim(Debug_file)//'_SHGC.w7'
      DebugOutputFileName = Trim(Debug_file)//'.dbg'
    end if

    !bi...Write debug output files - if debug flag > 0:

    WriteDebugOutput = .FALSE.
    if ((Debug_mode.gt.minDebugFlag).and.(Debug_mode.le.maxDebugFlag)) then

      WriteDebugOutput = .TRUE.
      if (Debug_mode.eq.appendResultsToFile) FileMode = 'APPEND'
      if ((Debug_mode.eq.resultsToNewFile).or.(Debug_mode.eq.saveIntermediateResults)) FileMode = 'SEQUENTIAL'

      InArgumentsFile = GetNewUnitNumber()
!      open(newunit=InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode,  &
!              &  form='formatted', iostat=nperr)
      open(InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode,  &
              &  form='formatted', iostat=nperr)

!      if (nperr.ne.0)  open(newunit=InArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode,  &
!              &  form='formatted', iostat=nperr)
      if (nperr.ne.0)  open(InArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode,  &
              &  form='formatted', iostat=nperr)

      OutArgumentsFile = InArgumentsFile

      WINCOGFile = GetNewUnitNumber()
!      open(newunit=WINCogFile,  file=TRIM(DBGD)//TRIM(WinCogFileName),  status='unknown', access=FileMode, &
!             &  form='formatted', iostat=nperr)
      open(WINCogFile,  file=TRIM(DBGD)//TRIM(WinCogFileName),  status='unknown', access=FileMode, &
             &  form='formatted', iostat=nperr)
!      if (nperr.ne.0) open(newunit=WINCogFile,  file=TRIM(WinCogFileName),  status='unknown', access=FileMode, &
!                            & form='formatted', iostat=nperr)
      if (nperr.ne.0) open(WINCogFile,  file=TRIM(WinCogFileName),  status='unknown', access=FileMode, &
                            & form='formatted', iostat=nperr)

      if (Debug_mode == saveIntermediateResults) then
        TarcogIterationsFileNumber = GetNewUnitNumber()
!        open(newunit=TarcogIterationsFileNumber,  file=TRIM(DBGD)//'TarcogIterations.dbg',  status='unknown', access='APPEND',  &
!              &  form='formatted', iostat=nperr)
        open(TarcogIterationsFileNumber,  file=TRIM(DBGD)//'TarcogIterations.dbg',  status='unknown', access='APPEND',  &
              &  form='formatted', iostat=nperr)

!        if (nperr.ne.0)  open(newunit=TarcogIterationsFileNumber, file='TarcogIterations.dbg',status='unknown', access='APPEND',  &
!              &  form='formatted', iostat=nperr)
        if (nperr.ne.0)  open(TarcogIterationsFileNumber, file='TarcogIterations.dbg',status='unknown', access='APPEND',  &
              &  form='formatted', iostat=nperr)

        IterationCSVFileNumber = GetNewUnitNumber()
!        open(newunit=IterationCSVFileNumber,  file=TRIM(DBGD)//Trim(IterationCSVName),  status='unknown', access='APPEND',  &
!              &  form='formatted', iostat=nperr)
        open(IterationCSVFileNumber,  file=TRIM(DBGD)//Trim(IterationCSVName),  status='unknown', access='APPEND',  &
              &  form='formatted', iostat=nperr)

!        if (nperr.ne.0)  open(newunit=IterationCSVFileNumber,  file=Trim(IterationCSVName),  status='unknown', access='APPEND',  &
!              &  form='formatted', iostat=nperr)
        if (nperr.ne.0)  open(IterationCSVFileNumber,  file=Trim(IterationCSVName),  status='unknown', access='APPEND',  &
              &  form='formatted', iostat=nperr)
      end if
   end if

  end subroutine

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

end module TARCOGOutput

module TARCOGArgs

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June/22/2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          !  Revision: 6.0.36  (June/22/2010)
          !   - Initial setup, extracted from TARCOG.for

          ! PURPOSE OF THIS MODULE:
          ! A module which contains common functions for error checking and
          !    preparation of arguments and intermediate variables

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataGlobals, only: Pi, StefanBoltzmann
  use TARCOGOutput
  use TARCOGParams

  implicit none

  public ArgCheck
  public GoAhead
  public PrepVariablesISO15099

  contains

  integer function ArgCheck(nlayer,iwd,tout,tind,trmin,wso,wsi,dir,outir,isky,tsky,esky,fclr,VacuumPressure, &
            VacuumMaxGapThickness, CalcDeflection, Pa, Pini, Tini, &
            gap,GapDef,thick,scon,YoungsMod,PoissonsRat,tir,emis,totsol,  &
            tilt,asol,height,heightt,width,presure,iprop,frct,xgcon,xgvis,xgcp,xwght,gama,nmix,  &
            SupportPillar, PillarSpacing, PillarRadius, &
            hin,hout, ibc,Atop,Abot,Al,Ar,Ah, SlatThick, SlatWidth, SlatAngle, &
            SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB, &
            sumsol, standard, ThermalMod, SDScalar, ErrorMessage)


    !!! INPUTS:

    !!! General:
    integer, intent(in) :: nlayer
    real(r64), intent(in) :: width, height, heightt
    integer, intent(in) :: standard
    integer, intent(in) :: ThermalMod
    character(len=*), intent(inout) :: ErrorMessage

    !!! Environment related:
    real(r64), intent(in) ::  tout,tind,trmin, wso, wsi, dir, outir, tsky, esky, fclr, totsol, tilt
    real(r64), intent(in) ::  VacuumPressure, VacuumMaxGapThickness
    integer, intent(in) :: iwd, isky, CalcDeflection
    integer, dimension(2), intent(in) :: ibc

      !!! Layers:
    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), dimension(maxlay), intent(in) ::  asol
    real(r64), dimension(maxlay2), intent(in) :: tir, emis
    !!! Venetians:
    real(r64), dimension(maxlay), intent(in) ::  Atop, Abot, Al, Ar, Ah
    real(r64), dimension(maxlay), intent(in) ::  SlatThick, SlatWidth, SlatAngle
    real(r64), dimension(maxlay), intent(in) ::  SlatCond, SlatSpacing, SlatCurve
    real(r64), dimension(maxlay1), intent(in) ::  vvent, tvent

    !!! Laminates:
    integer, dimension(maxlay), intent(in) :: nslice
    real(r64), dimension(maxlay), intent(in) :: LaminateA, LaminateB, sumsol

    !!! Gaps:
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxlay1,maxgas), intent(in) ::  frct
    real(r64), dimension(maxlay1), intent(in) ::  presure
    real(r64), dimension(maxgas, 3), intent(in) ::  xgcon, xgvis, xgcp
    real(r64), dimension(maxgas), intent(in) ::  xwght, gama
    real(r64), intent(in) :: SDScalar

    !Deflection
    real(r64), intent(in) :: Pa
    real(r64), intent(in) :: Pini
    real(r64), intent(in) :: Tini

    !Support Pillars
    integer, dimension(maxlay), intent(in) :: SupportPillar    ! Shows whether or not gap have support pillar
                                                               !   0 - does not have support pillar
                                                               !   1 - have support pillar
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing  ! Pillar spacing for each gap (used in case there is support pillar)
    real(r64), dimension(maxlay), intent(in) :: PillarRadius   ! Pillar radius for each gap (used in case there is support pillar)


    !!!! INPUTS/OUTPUTS:
    real(r64), intent(in) :: scon(maxlay), YoungsMod(maxlay), PoissonsRat(maxlay)
    real(r64), dimension(maxlay), intent(in) :: thick, gap
    real(r64), dimension(MaxGap), intent(in) :: GapDef
    real(r64), intent(inout) :: hin, hout

    integer :: i
    character(len=3) :: a

  !bi...Write debug output files - if debug flag = 1:

    if (WriteDebugOutput) then

      call WriteInputArguments(tout, tind, trmin,  wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, &
                            VacuumMaxGapThickness, ibc, hout, hin,  &
                            standard, ThermalMod, SDScalar, height, heightt, width, tilt, totsol, nlayer,  &
                            LayerType, thick, scon, asol, tir, emis, Atop, Abot, Al, Ar, Ah,  &
                            SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve,  &
                            nslice, LaminateA, LaminateB, sumsol, gap, vvent, tvent,  &
                            presure, nmix, iprop, frct, xgcon, xgvis, xgcp, xwght)

      call WriteTARCOGInputFile(VersionNumber, tout, tind, trmin, &
                            &   wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, &
                            &   CalcDeflection, Pa, Pini, Tini, ibc, hout, hin,   &
                            &   standard, ThermalMod, SDScalar, height, heightt, width, tilt, totsol, nlayer,  &
                            &   LayerType, thick, scon, YoungsMod, PoissonsRat, asol, tir, emis, Atop, Abot, Al, Ar, Ah,  &
                            &   SupportPillar, PillarSpacing, PillarRadius, &
                            &   SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve,  &
                            &   nslice, gap, GapDef, vvent, tvent,  &
                            &   presure, nmix, iprop, frct, xgcon, xgvis, xgcp, xwght, gama)

    end if ! if debug=1 - write dbg output file


    !bi...assume All OK
    ArgCheck = 0

    !dr...check for error messages
    if (nlayer.lt.1) then
      ArgCheck = 17
      ErrorMessage = 'Number of layers must be >0.'
      return
    end if

    if ((standard.lt.MinStandard).or.(standard.gt.MaxStandard)) then
      ArgCheck = 28
      ErrorMessage = 'Invalid code for standard.'
      return
    end if

    if ((ThermalMod.lt.MinThermalMode).or.(ThermalMod.gt.MaxThermalMode)) then
      ArgCheck = 29
      ErrorMessage = 'Invalid code for thermal mode.'
      return
    end if

    if ((iwd.ne.0).and.(iwd.ne.1)) then
      ArgCheck = 18
      ErrorMessage = 'Wind direction can be windward (=0) or leeward (=1).'
      return
    end if

    if ((fclr.lt.0.0d0).or.(fclr.gt.1.0d0)) then
      ArgCheck = 19
      ErrorMessage = 'Fraction of sky that is clear can be in range between 0 and 1.'
      return
    end if

    do i=1, nlayer - 1
      if (gap(i).le.0.0d0) then
        ArgCheck = 20
        write(a,'(i3)') i
        ErrorMessage = 'Gap width is less than (or equal to) zero. Gap #'//trim(a)
        return
      end if
    end do

    do i=1, nlayer
      if (thick(i).le.0.0d0) then
        ArgCheck = 21
        write(a,'(i3)') i
        ErrorMessage = 'Layer width is less than (or equal to) zero. Layer #'//trim(a)
        return
      end if
      if ((i.lt.nlayer).and.(LayerType(i).eq.VENETBLIND).and.(LayerType(i+1).eq.VENETBLIND)) then
        ArgCheck = 37
        ErrorMessage = 'Cannot handle two consecutive venetian blinds.'
        return
      end if
      if ((i.lt.nlayer).and.(LayerType(i).eq.WOVSHADE).and.(LayerType(i+1).eq.WOVSHADE)) then
        ArgCheck = 43
        ErrorMessage = 'Cannot handle two consecutive woven shades.'
        return
      end if
      if ((i.lt.nlayer).and.(LayerType(i).eq.VENETBLIND).and.(LayerType(i+1).eq.WOVSHADE)) then
        ArgCheck = 44
        ErrorMessage = 'Cannot handle consecutive venetian blind and woven shade.'
        return
      end if
      if ((i.lt.nlayer).and.(LayerType(i).eq.WOVSHADE).and.(LayerType(i+1).eq.VENETBLIND)) then
        ArgCheck = 44
        ErrorMessage = 'Cannot handle consecutive venetian blind and woven shade.'
        return
      end if
      !Deflection cannot be calculated with IGU containing shading layer. This error check is to be
      !removed once that extension is programmed
      if ((CalcDeflection.gt.0.0d0).and.(LayerType(i).ne.SPECULAR)) then
        ArgCheck = 42
        ErrorMessage = 'Cannot calculate deflection with IGU containing shading devices.'
        return
      end if
    end do

    if (height.le.0.0d0) then
      ArgCheck = 23
      ErrorMessage = 'IGU cavity height must be greater than zero.'
      return
    end if

    if (heightt.le.0.0d0) then
      ArgCheck = 24
      ErrorMessage = 'Total window height must be greater than zero.'
      return
    end if

    if (width.le.0.0d0) then
      ArgCheck = 25
      ErrorMessage = 'Window width must be greater than zero.'
      return
    end if

    if ((SDScalar.lt.0.0d0).or.(SDScalar.gt.1.0d0)) then
      ArgCheck = 30
      ErrorMessage = 'SDscalar is out of range (<0.0 or >1.0).'
      return
    end if

  !bi...Check layers and update Venetian blinds properties:
    do i=1, nlayer
      if (scon(i).le.0.0d0) then
        ArgCheck = 26
        write(a,'(i3)') i
        ErrorMessage = 'Layer '//trim(a)//' has conductivity whcih is less or equal to zero.'
        return
      end if

      if ((LayerType(i).lt.MinLayType).or.(LayerType(i).gt.MaxLayType)) then
        ArgCheck = 22
        write(a,'(i3)') i
        ErrorMessage = 'Incorrect layer type for layer #'//trim(a)//'.  Layer type can either be 0 (glazing layer),' // &
              '1 (Venetian blind), 2 (woven shade), 3 (perforated), 4 (diffuse shade) or 5 (bsdf).'
        return
      end if

  !bi...TEMPORARY! Don't allow CSW and CSM method for outdoor and indoor SD layers
      if ( (IsShadingLayer(LayerType(1))).and.((ThermalMod.eq.THERM_MOD_SCW).or.(ThermalMod.eq.THERM_MOD_CSM)) ) then
        ArgCheck = 39
        ErrorMessage = 'CSM and SCW thermal models cannot be used for outdoor and indoor SD layers.'
        return
      end if
      if ( (IsShadingLayer(LayerType(nlayer))).and.((ThermalMod.eq.THERM_MOD_SCW) &
            .or.(ThermalMod.eq.THERM_MOD_CSM)) ) then
        ArgCheck = 39
        ErrorMessage = 'CSM and SCW thermal models cannot be used for outdoor and indoor SD layers.'
        return
      end if

      if (LayerType(i).eq.VENETBLIND) then   ! Venetian blind specific:
        if (SlatThick(i).le.0) then
          ArgCheck = 31
          write(a,'(i3)') i
          ErrorMessage = 'Invalid slat thickness (must be >0). Layer #'//trim(a)
          return
        end if
        if (SlatWidth(i).le.0.0d0) then
          ArgCheck = 32
          write(a,'(i3)') i
          ErrorMessage = 'Invalid slat width (must be >0). Layer #'//trim(a)
          return
        end if
        if ((SlatAngle(i).lt.-90.0d0).or.(SlatAngle(i).gt.90.0d0)) then
          ArgCheck = 33
          write(a,'(i3)') i
          ErrorMessage = 'Invalid slat angle (must be between -90 and 90). Layer #'//trim(a)
          return
        end if
        if (SlatCond(i).le.0.0d0) then
          ArgCheck = 34
          write(a,'(i3)') i
          ErrorMessage = 'Invalid conductivity of slat material (must be >0). Layer #'//trim(a)
          return
        end if
        if (SlatSpacing(i).le.0.0d0) then
          ArgCheck = 35
          write(a,'(i3)') i
          ErrorMessage = 'Invalid slat spacing (must be >0). Layer #'//trim(a)
          return
        end if
        if ( (SlatCurve(i).ne.0.0d0).and.(abs(SlatCurve(i)).le.(SlatWidth(i) / 2.0d0)) ) then
          ArgCheck = 36
          write(a,'(i3)') i
          ErrorMessage = 'Invalid curvature radius (absolute value must be >SlatWidth/2, or 0 for flat slats). Layer #'//trim(a)
          return
        end if

      end if  !  LayerType is Venetian

    end do  ! Layers...

    do i=1, nlayer + 1
      if (presure(i).lt.0.0d0) then
        ArgCheck = 27
        write(a,'(i3)') i
        if ((i.eq.1).or.(i.eq.(nlayer + 1))) then
          ErrorMessage = 'One of enviroments (inside or outside) has pressure which is less than zero.'
        else
          ErrorMessage = 'One of gaps has pressure which is less than zero. Gap #'//trim(a)
        end if
        return
      end if
    end do

    return

  !bi...Debug output:
  !      open(unit=18,  file='iprop.dbg',  status='unknown', access='APPEND',
  !  2            form='formatted', iostat=nperr)
  !    write(18,5555) 'Iprop1:', iprop(1, 1), iprop(1, 2), iprop (1, 3)
  !    write(18,5555) 'Iprop2:', iprop(2, 1), iprop(2, 2), iprop (2, 3)
  !    write(18,5555) 'Iprop3:', iprop(3, 1), iprop(3, 2), iprop (3, 3)
  !5555  format(A, I3, I3, I3)
  !    close(18)

  end function

  subroutine PrepVariablesISO15099(nlayer, tout, tind, trmin, isky, outir, tsky, esky, fclr, gap, thick, scon, tir, emis,  &
          &  tilt, hin, hout, ibc, SlatThick, SlatWidth, SlatAngle, SlatCond, LayerType,   &
          &  ThermalMod, SDScalar, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn,  &
          & Keff, ShadeGapKeffConv, sc, shgc, ufactor, flux, LaminateAU, sumsolU, sol0,  &
          & hint, houtt, trmout, ebsky, ebroom, Gout, Gin, rir, vfreevent, nperr, ErrorMessage)

    integer, intent(in) :: nlayer
    integer, intent(in) :: ThermalMod

    !!! Environment related:
    real(r64), intent(in) ::  tout,tind, tsky, fclr, tilt
    integer, intent(in) :: isky
    integer, dimension(2), intent(in) :: ibc
    real(r64), intent(in) :: outir !IR radiance of window's exterior/interior surround (W/m2)

      !!! Layers:
    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), dimension(maxlay2), intent(in) ::  tir, emis

    !!! Venetians:
    real(r64), dimension(maxlay), intent(in) ::  SlatThick, SlatWidth, SlatAngle, SlatCond

    real(r64), intent(in) :: SDScalar

    !!!! INPUTS/OUTPUTS:
    real(r64), intent(inout) :: trmin, esky
    real(r64), dimension(maxlay), intent(inout) :: scon, thick
    real(r64), dimension(MaxGap), intent(inout) :: gap
    real(r64), intent(inout) :: hin, hout

    !!! OUTPUTS:
    integer, intent(out) :: nperr
    character (len=*), intent(inout) :: ErrorMessage
    real(r64), dimension(maxlay2), intent(out) :: rir
    real(r64), intent(out) :: ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn
    real(r64), intent(out) :: Gout, Gin, sc, shgc, ufactor, flux
    real(r64), dimension(maxlay1), intent(out) :: vfreevent
    real(r64), dimension(maxlay), intent(out) :: LaminateAU, sumsolU, sol0
    real(r64), intent(out) :: hint, houtt, trmout, ebsky, ebroom
    real(r64), dimension(maxlay), intent(out) :: Keff
    real(r64), dimension(MaxGap), intent(out) :: ShadeGapKeffConv

    integer :: i, k, k1
    real(r64) :: tiltr, Rsky, Fsky, Fground, e0
    character(len=3) :: a

    !! Initialize variables:

    !! Scalars:
    ShadeEmisRatioOut = 1.0d0
    ShadeEmisRatioIn  = 1.0d0
    ShadeHcRatioOut   = 1.0d0
    ShadeHcRatioIn    = 1.0d0

    !! re-initialize iteration parameters:
    sc = 0.0d0
    shgc = 0.0d0
    ufactor = 0.0d0
    flux = 0.0d0

    !! Vectors:
    LaminateAU = 0.0d0
    sumsolU    = 0.0d0
    vfreevent  = 0.0d0
    sol0 = 0.0d0
    !bi...    Clear keff, keffc elements:
    Keff = 0.0d0
    ShadeGapKeffConv = 0.0d0

    ! Adjust shading layer properties
    do i=1, nlayer
      if (LayerType(i).eq.VENETBLIND) then
        scon(i)  = SlatCond(i)
        if (ThermalMod.eq.THERM_MOD_SCW) then
          !bi...the idea here is to have glass-to-glass width the same as before scaling
          !bi...TODO: check for outdoor and indoor blinds! SCW model is only applicable to in-between SDs!!!
          thick(i) = SlatWidth(i) * cos(SlatAngle(i) * Pi / 180.0d0)
          if ( i > 1 ) gap(i-1)=gap(i-1) + (1.0d0-SDScalar)/2.0d0 * thick(i) !Objexx:BoundsViolation gap(i-1) @ i=1: Added if condition
!EPTeam - see above line          gap(i-1)=gap(i-1) + (1.0d0-SDScalar)/2.0d0 * thick(i)
          gap(i)=gap(i) + (1.0d0-SDScalar)/2.0d0 * thick(i)
          thick(i) = SDScalar*thick(i)
          if (thick(i).lt.SlatThick(i))  thick(i) = SlatThick(i)
        else if ((ThermalMod.eq.THERM_MOD_ISO15099).or.(ThermalMod.eq.THERM_MOD_CSM)) then
          thick(i) = SlatThick(i)
        end if
      end if  ! Venetian
    end do


    hint = hin
    houtt = hout
    tiltr = tilt*2.0d0*pi/360.0d0         ! convert tilt in degrees to radians

    ! external radiation term
    select case (isky)
      case (3)
        Gout = outir
        Trmout = (Gout/StefanBoltzmann)**(0.25d0)
      case (2)                            ! effective clear sky emittance from swinbank (SPC142/ISO15099 equations 131, 132, ...)
        Rsky = 5.31d-13*Tout**6
        esky = Rsky/(StefanBoltzmann*Tout**4)        ! check esky const, also check what esky to use when tsky input...
      case (1)
        esky = tsky**4/tout**4
      case (0)                              ! for isky=0 it is assumed that actual values for esky and Tsky are specified
        esky = esky*tsky**4/tout**4
      case DEFAULT
        nperr = 1                        ! error 2010: isky can be: 0(esky,Tsky input), 1(Tsky input), or 2(Swinbank model)
        return
      end select

    !Simon: In this case we do not need to recalculate Gout and Trmout again
    if (isky.ne.3) then
      Fsky = (1.0d0+cos(tiltr))/2.0d0
      Fground = 1.0d0 - Fsky
      e0 = Fground + (1.0d0-fclr)*Fsky + Fsky*fclr*esky
      !  Trmout = Tout * e0**0.25

      !bi   Set mean radiant temps for fixed combined film coef. case:

      if (ibc(1).eq.1) then ! outside BC - fixed combined film coef.
        Trmout = Tout
      else
        Trmout = Tout * e0**0.25d0
      end if

      Gout = StefanBoltzmann * Trmout**4
    end if !if (isky.ne.3) then

    Ebsky = Gout

    !     Ebsky=sigma*Tout**4.
    !
    ! As of 6/1/01 The expression for Ebsky is different in the current ISO 15099
    ! (Ebsky=sigma*Tout**4) because equations 32 and 33 specify Tout and Tind as reference
    ! outdoor and indoor temperatures, but I think that they should be Tne and Tni
    ! (environmental temps).  Therefore, Ebsky becomes the same as Gout.
    !
    ! Inside (room) radiation
    !     Ebroom = sigma*tind**4.
    ! See comment above about Ebsky

    if (ibc(2).eq.1) then ! inside BC - fixed combined film coef.
      Trmin = Tind
    end if

    Gin = StefanBoltzmann * trmin**4.0d0
    Ebroom = Gin

    ! calculate ir reflectance:
    do k = 1, nlayer
      k1 = 2*k - 1
      rir(k1)   = 1 - tir(k1) - emis(k1)
      rir(k1+1) = 1 - tir(k1) - emis(k1+1)
      if ((tir(k1).lt.0.0d0).or.(tir(k1).gt.1.0d0).or.(tir(k1+1).lt.0.0d0).or.(tir(k1+1).gt.1.0d0)) then
        nperr = 4
        write(a, '(i3)') k
        ErrorMessage = 'Layer transmissivity is our of range (<0 or >1). Layer #'//trim(a)
        return
      end if
      if ((emis(k1).lt.0.0d0).or.(emis(k1).gt.1.0d0).or.(emis(k1+1).lt.0.0d0).or.(emis(k1+1).gt.1.0d0)) then
        nperr = 14
        write(a, '(i3)') k
        ErrorMessage = 'Layer emissivity is our of range (<0 or >1). Layer #'//trim(a)
        return
      end if
      if ((rir(k1).lt.0.0d0).or.(rir(k1).gt.1.0d0).or.(rir(k1+1).lt.0.0d0).or.(rir(k1+1).gt.1.0d0)) then
        nperr = 3
        write(a, '(i3)') k
        ErrorMessage = 'Layer reflectivity is our of range (<0 or >1). Layer #'//trim(a)
        return
      end if
    end do

  end subroutine PrepVariablesISO15099

  logical function GoAhead(nperr)

    integer, intent(in) :: nperr

    if (((nperr.gt.0).and.(nperr.lt.1000)).or.((nperr.gt.2000).and.(nperr.lt.3000))) then
      GoAhead = .FALSE.  ! error
    else
      GOAhead = .TRUE.   ! all OK, or a warning
    end if

    return

  end function

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

end module TARCOGArgs

module TARCOGDeflection

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   October/22/2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          !  Revision: 7.0.02  (October 22, 2011)
          !   - Initial setup

          ! PURPOSE OF THIS MODULE:
          ! A module which contains functions for deflection calculation

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataGlobals
  !use TARCOGGassesParams
  use TARCOGParams
  use TARCOGCommon

  implicit none

  public  PanesDeflection
  private DeflectionTemperatures
  private DeflectionWidths

  contains

  subroutine PanesDeflection(DeflectionStandard, W, H, nlayer, Pa, Pini, Tini, PaneThickness, NonDeflectedGapWidth, &
                          DeflectedGapWidthMax, DeflectedGapWidthMean, PanelTemps, YoungsMod, PoissonsRat, LayerDeflection, &
                          nperr, ErrorMessage)
    !***********************************************************************
    ! PanesDeflection - calculates deflection of panes and recalculate gap
    !                   widths at maximal point of deflection
    !***********************************************************************

    !INPUT
    integer, intent(in) :: DeflectionStandard
    integer, intent(in) :: nlayer
    real(r64), intent(in) :: W, H
    real(r64), dimension(maxlay2), intent(in) :: PanelTemps
    real(r64), dimension(MaxGap), intent(in) :: NonDeflectedGapWidth
    real(r64), dimension(maxlay), intent(in) :: PaneThickness
    real(r64), dimension(maxlay), intent(in) :: YoungsMod, PoissonsRat
    real(r64), intent(in) :: Pa, Pini, Tini

    !OUTPUT
    real(r64), dimension(maxlay), intent(inout) :: LayerDeflection !Objexx:ArgIN Changed IN to INOUT: Arg used and modified
    real(r64), dimension(MaxGap), intent(inout) :: DeflectedGapWidthMax
    real(r64), dimension(MaxGap), intent(out) :: DeflectedGapWidthMean
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    !Localy used
    real(r64), dimension(maxlay) :: DCoeff
    integer :: i

    i = 0
    ! first calculate D coefficients since that will be necessary for any of selected standards
    do i = 1, nlayer
      DCoeff(i) = YoungsMod(i) * PaneThickness(i)**3.0d0 / (12 * (1 - PoissonsRat(i)**2.0d0))
    end do

    select case (DeflectionStandard)
      case (NO_DEFLECTION_CALCULATION)
        return
      case (DEFLECTION_CALC_TEMPERATURE)
        call DeflectionTemperatures(nlayer, W, H, Pa, Pini, Tini, NonDeflectedGapWidth, DeflectedGapWidthMax, &
                               DeflectedGapWidthMean, PanelTemps, DCoeff, LayerDeflection, nperr, ErrorMessage)
      case (DEFLECTION_CALC_GAP_WIDTHS)
        call DeflectionWidths(nlayer, W, H, DCoeff, NonDeflectedGapWidth, DeflectedGapWidthMax, DeflectedGapWidthMean, &
                               LayerDeflection)
      case default
        return
    end select

    return

  end subroutine PanesDeflection

  subroutine DeflectionTemperatures(nlayer, W, H, Pa, Pini, Tini, NonDeflectedGapWidth, DeflectedGapWidthMax, &
                             DeflectedGapWidthMean, PanelTemps, DCoeff, LayerDeflection, nperr, ErrorMessage)
    !***********************************************************************************************************
    ! DeflectionTemperatures - calculates deflection of panes and recalculate gap
    !                          widths at maximal point of deflection based on gap pressures and temperatures
    !***********************************************************************************************************
    !INPUT
    integer, intent(in) :: nlayer
    real(r64), intent(in) :: W, H
    real(r64), dimension(maxlay2), intent(in) :: PanelTemps
    real(r64), dimension(MaxGap), intent(in) :: NonDeflectedGapWidth
    real(r64), dimension(maxlay) :: DCoeff
    real(r64), intent(in) :: Pa, Pini, Tini

    !OUTPUT
    real(r64), dimension(maxlay), intent(inout) :: LayerDeflection !Objexx:ArgIN Changed IN to INOUT: Arg used and modified
    real(r64), dimension(MaxGap), intent(out) :: DeflectedGapWidthMax
    real(r64), dimension(MaxGap), intent(out) :: DeflectedGapWidthMean
    integer, intent(inout) :: nperr
    character (len=*), intent(inout) :: ErrorMessage

    !localy used
    real(r64), dimension(maxlay) :: DPressure  !delta pressure at each glazing layer
    real(r64), dimension(MaxGap) :: Vini
    real(r64), dimension(MaxGap) :: Vgap
    real(r64), dimension(MaxGap) :: Pgap
    real(r64), dimension(MaxGap) :: Tgap
    real(r64) :: MaxLDSum, MeanLDSum, Ratio
    integer :: i, j

    i = 0
    j = 0
    Ratio = 0.0d0
    MeanLDSum = 0.0d0
    MaxLDSum = 0.0d0

    !calculate Vini for each gap
    do i = 1, nlayer - 1
      Vini(i) = NonDeflectedGapWidth(i) * W * H
    end do !do i = 1, nlayer

    MaxLDSum = LDSumMax(W, H)
    MeanLDSum = LDSumMean(W, H)
    Ratio = MeanLDSum / MaxLDSum

    !calculate Vgap for each gap
    do i = 1, nlayer - 1
      Vgap(i) = Vini(i) + W * H * Ratio * (LayerDeflection(i) - LayerDeflection(i+1))
    end do !do i = 1, nlayer

    !calculate Tgap for each gap
    do i = 1, nlayer - 1
      j = 2 * i
      Tgap(i) = (PanelTemps(j) + PanelTemps(j + 1)) / 2
    end do !do i = 1, nlayer

    do i = 1, nlayer - 1
      Pgap(i) = Pini*Vini(i)*Tgap(i)/(Tini*Vgap(i))
    end do !do i = 1, nlayer

    DPressure(1) = Pgap(1) - Pa
    if (nlayer.gt.1) then
      DPressure(nlayer) = Pa - Pgap(nlayer - 1)
    end if

    do i = 2, nlayer - 1
      DPressure(i) = Pgap(i) - Pgap(i - 1)
    end do !do i = 1, nlayer

    do i = 1, nlayer
      LayerDeflection(i) = LayerDeflection(i) + DeflectionRelaxation * MaxLDSum * 16 * DPressure(i) / (Pi**6 * DCoeff(i))
    end do

    do i = 1, nlayer - 1
      DeflectedGapWidthMax(i) = NonDeflectedGapWidth(i) + LayerDeflection(i) - LayerDeflection(i+1)
      if (DeflectedGapWidthMax(i) < 0.0d0) then
        nperr = 2001 !glazing panes collapsed
        ErrorMessage = 'Glazing panes collapsed'
      end if
    end do

    do i = 1, nlayer - 1
      DeflectedGapWidthMean(i) = NonDeflectedGapWidth(i) + Ratio * (DeflectedGapWidthMax(i) - NonDeflectedGapWidth(i))
    end do

    return

  end subroutine DeflectionTemperatures

  subroutine DeflectionWidths(nlayer, W, H, DCoeff, NonDeflectedGapWidth, DeflectedGapWidthMax, DeflectedGapWidthMean, &
                    LayerDeflection)
    !INPUT
    integer, intent(in) :: nlayer
    real(r64), intent(in) :: W, H
    real(r64), dimension(MaxGap), intent(in) :: NonDeflectedGapWidth
    real(r64), dimension(MaxGap), intent(in) :: DeflectedGapWidthMax
    real(r64), dimension(maxlay) :: DCoeff

    !OUTPUT
    real(r64), dimension(maxlay), intent(out) :: LayerDeflection
    real(r64), dimension(MaxGap), intent(out) :: DeflectedGapWidthMean
    !integer, intent(inout) :: nperr
    !character*(*) :: ErrorMessage

    !LOCALS
    integer :: i, j
    real(r64) :: nominator, denominator, SumL
    real(r64) :: MaxLDSum, MeanLDSum, Ratio

    i = 0
    j = 0
    Ratio = 0.0d0
    MeanLDSum = 0.0d0
    MaxLDSum = 0.0d0

    nominator = 0.0d0
    do i = 1, nlayer - 1
      SumL = 0.0d0
      do j = i, nlayer - 1
        SumL = SumL + NonDeflectedGapWidth(j) - DeflectedGapWidthMax(j)
      end do
      nominator = nominator + SumL * DCoeff(i)
    end do

    denominator = 0.0d0
    do i = 1, nlayer
      denominator = denominator + DCoeff(i)
    end do

    LayerDeflection(nlayer) = nominator / denominator

    do i = nlayer - 1, 1, -1
      LayerDeflection(i) = DeflectedGapWidthMax(i) - NonDeflectedGapWidth(i) + LayerDeflection(i + 1)
    end do

    MaxLDSum = LDSumMax(W, H)
    MeanLDSum = LDSumMean(W, H)
    Ratio = MeanLDSum / MaxLDSum

    do i = 1, nlayer - 1
      DeflectedGapWidthMean(i) = NonDeflectedGapWidth(i) + Ratio * (DeflectedGapWidthMax(i) - NonDeflectedGapWidth(i))
    end do

    return
  end subroutine DeflectionWidths

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

end module TARCOGDeflection

module TarcogShading

          ! MODULE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June/22/2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          !  Revision: 6.0.36  (June/22/2010)
          !   - Initial setup, extracted from TARCOG.for

          ! PURPOSE OF THIS MODULE:
          ! Module which contains subroutines used for handling shading
          ! device layers according to ISO15099

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use TARCOGGassesParams
  use TARCOGGasses90
  use TARCOGParams

  implicit none

  public  shading
  private forcedventilation
  private shadingin
  private shadingedge

  contains

  subroutine shading(theta, gap, hgas, hcgas, hrgas, frct, iprop, pressure, nmix, xwght, xgcon, xgvis, xgcp, nlayer, width, &
                  height, angle, Tout, Tin, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Tgaps, qv, nperr, &
                  ErrorMessage, vfreevent)
  !**************************************************************************************************************
  !  Input:
  !
  ! theta   Vector of average temperatures
  !  gap      Vector of gap widths (maxlay) [m]
  !  hgas    Convective part of gap effective conductivity
  !  frct    Fraction of gasses in a mixture (maxlay1,maxgas)
  !  iprop    Vector of gas identifers (maxlay1,maxgas)
  !  pressure  Vector of gas pressures [N/m^2]
  !  nmix    Vector of number of gasses for each mixture (maxgas=10)
  !  nlayer  Number of glazing layers
  !  width    IGU cavity width [m]
  !  height  IGU cavity height [m]
  !  angle    Window angle [degrees]
  !  Tout    Outdoor temperature [K]
  !  Tin      Indoor temperature [K]
  !  Atop    Opening between top of shading device and top of glazing cavity [m^2]
  !  Abot    Opening between bottom of shading device and bottom of glazing cavity [m^2]
  !  Al      Opening between left of shading device and left end of glazing cavity [m^2]
  !  Ar      Opening between right of shading device and right end of glazing cavity [m^2]
  !  Ah      Total area holes in the shading device [m^2]
  !  LayerType    Vector of layer types (0 - glazing; 1 - shading)
  !  Ebf      Vector of emissive power of the front surface (# of layers)
  !
  !  Input/Output:
  !
  !  Tgaps    Vector of gap temperatures [K]
  !
  !  Output:
  !
  !  qv      Vector of heat transfer to the gap by vetilation [W/m^2]
  !  hhatv    Vector of all film coefficients for vented cavities (maxlay3)
  !  hcv      Vector of surface-to-air heat transfer coefficients by condction/convection for vented cavities [W/(m^2*K)]
  !  Ebgap    Vector of emissive power of the vented cavities (maxlay3)
  !  nperr    Error flag
  ! vfreevent   Vector of free ventilation velocities in gaps
  !**************************************************************************************************************

    use TarcogCommon

    real(r64), dimension(maxlay), intent(in) :: Atop, Abot, Al, Ar, Ah
    !real(r64), intent(in) :: Ebf(maxlay)
    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), intent(in) :: width, height, angle, Tout, Tin
    real(r64), dimension(maxlay1), intent(in) :: vvent, tvent
    real(r64), dimension(maxlay2), intent(in) :: theta
    real(r64), dimension(MaxGap), intent(in) :: gap
    real(r64), dimension(maxlay1), intent(inout) :: hgas, hcgas, hrgas
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: pressure
    integer, dimension(maxlay1), intent(in) :: nmix
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, intent(in) :: nlayer
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgvis, xgcp

    real(r64), dimension(maxlay1), intent(out) :: qv
    real(r64), dimension(maxlay1), intent(out) :: vfreevent

    real(r64), dimension(maxlay1), intent(inout) :: Tgaps
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage


    real(r64) :: Atops, Abots, Als, Ars, Ahs, press1, press2, s1, s2, s, hcvs
    real(r64) :: qvs, hc, hc1, hc2
    real(r64), dimension(maxgas) :: frct1, frct2
    real(r64) :: speed, Tav, Tgap, Temp
    real(r64) :: speed1, speed2, Tav1, Tav2, Tgap1, Tgap2, hcv1, hcv2, qv1, qv2

    integer :: i, j, k
    integer :: nmix1, nmix2
    integer, dimension(maxgas) :: iprop1, iprop2

    ! init vectors:
    qv = 0.0d0
    !hhatv = 0.0
    !Ebgap = 0.0
    !hcv = 0.0

    !main loop:
    do i = 1, nlayer
      k = 2*i + 1
      !if (LayerType(i).eq.VENETBLIND) then
      if (IsShadingLayer(LayerType(i))) then
        !dr.........set Shading device geometry
        Atops = Atop(i)
        Abots = Abot(i)
        Als   = Al(i)
        Ars   = Ar(i)
        Ahs   = Ah(i)

        !dr.....setting gas properies for two adjacent gaps (or enviroment)
        nmix1 = nmix(i)
        nmix2 = nmix(i+1)
        press1 = pressure(i)
        press2 = pressure(i+1)
        do j = 1, maxgas
          iprop1(j) = iprop(i, j)
          iprop2(j) = iprop(i+1, j)
          frct1(j) = frct(i, j)
          frct2(j) = frct(i+1,j)
        end do  ! j

        !dr.......shading on outdoor side
        if (i.eq.1) then
          s = gap(1)
          hc = hcgas(2)
          !Tenv = tvent(1)
          Tav = (theta(2) + theta(3)) / 2.0d0
          Tgap = Tgaps(2)

          !bi......use Tout as temp of the air at inlet
          call shadingedge(iprop1, frct1, press1, nmix1, iprop2, frct2,  press2, nmix2, xwght, xgcon, xgvis, xgcp, &
                            Atops, Abots, Als, Ars, Ahs, s, height, width, angle, vvent(2), hc, Tout, Tav, Tgap, hcvs, qvs, &
                            nperr, ErrorMessage, speed)

          ! exit on error
          if ((nperr.gt.0).and.(nperr.lt.1000))  return

          Tgaps(2) = Tgap
          !Ebgap(3) = sigma * Tgap ** 4

          hcgas(2)   = hcvs / 2.0d0
          hgas(2) = hcgas(2) + hrgas(2)
          qv(2)    = qvs

          !bi.........Add free ventilation velocity
          vfreevent(2) = speed
        end if !if (i.eq.1) then

        !dr.......shading on indoor side
        if (i.eq.nlayer) then
          if (nlayer > 1) then
            s = gap(nlayer - 1) !Objexx:BoundsViolation gap(nlayer - 1) @ nlayer=1
            Tav = (theta(2 * nlayer - 1) + theta(2 * nlayer - 2)) / 2.0d0 !Objexx:BoundsViolation theta(2 * nlayer - 2) @ nlayer=1
          else
            s = 0.0d0
            Tav = 273.15d0
          end if
          hc = hcgas(nlayer)
          !Tenv = tvent(nlayer + 1)

          Tgap = Tgaps(nlayer)

          !bi.........use Tin as temp of the air at inlet
          call shadingedge(iprop2, frct2, press2, nmix2, iprop1, frct1,  press1, nmix1, xwght, xgcon, xgvis, xgcp, &
                    Atops, Abots, Als, Ars, Ahs, s, height, width, angle, vvent(nlayer), hc, Tin, Tav, Tgap, hcvs, qvs, &
                    nperr, ErrorMessage, speed)

          ! exit on error
          if ((nperr.gt.0).and.(nperr.lt.1000))  return

          Tgaps(nlayer) = Tgap
          hcgas(nlayer)   = hcvs / 2.0d0
          hgas(nlayer) = hcgas(nlayer) + hrgas(nlayer)
          qv(nlayer)    = qvs

          !bi.........Add free ventilation velocity
          vfreevent(i) = speed
        end if !if (i.eq.nlayer) then

        !dr.......shading between glass layers
        if ((i.gt.1).and.(i.lt.nlayer)) then
          !dr.........average temperatures
          Tav1  = (theta(2*i - 2) + theta(2*i - 1)) / 2.0d0
          Tav2  = (theta(2*i) + theta(2*i + 1)) / 2.0d0
          Tgap1 = Tgaps(i)
          Tgap2 = Tgaps(i + 1)

          hc1   = hcgas(i)
          hc2   = hcgas(i+1)
          if (i.gt.1) s1    = gap(i - 1)
          s2  = gap(i)

          !speed1 = vvent(i)
          !speed2 = vvent(i+1)

          if ((CalcForcedVentilation.ne.0).and.((vvent(i).ne.0).or.(vvent(i+1).ne.0))) then
            call forcedventilation(iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, s1, height, hc1, &
                    & vvent(i), tvent(i), Temp, Tav1, hcv1, qv1, nperr, ErrorMessage)
            call forcedventilation(iprop2, frct2, press2, nmix1, xwght, xgcon, xgvis, xgcp, s2, height, hc1, &
                    & vvent(i+1), tvent(i+1), Temp, Tav2, hcv2, qv2, nperr, ErrorMessage)
          else
            call shadingin(iprop1, frct1, press1, nmix1, iprop2, frct2,  press2, nmix2, xwght, xgcon, xgvis, xgcp, &
                            Atops, Abots, Als, Ars, Ahs, s1, s2, height, width, angle, hc1, hc2, speed1, speed2, &
                            Tgap1, Tgap2, Tav1, Tav2, hcv1, hcv2, qv1, qv2, nperr, ErrorMessage)
          end if

          ! exit on error
          if ((nperr.gt.0).and.(nperr.lt.1000))  return

          !if (vvent(i).gt.0) then !not implemented for inside shadin yet
          !  nperr = 1006
          !  ErrorMessage = 'Forced ventilation not implemented for internal SD layers.'
          !  return
          !end if

          hcgas(i)   = hcv1 / 2.0d0
          hcgas(i+1) = hcv2 / 2.0d0
          hgas(i) = hcgas(i) + hrgas(i)
          hgas(i+1) = hcgas(i+1) + hrgas(i+1)
          qv(i)      = qv1
          qv(i+1)    = qv2
          Tgaps(i)         = Tgap1
          Tgaps(i + 1)     = Tgap2
          !bi.........Add free ventilation velocity
          vfreevent(i) = speed1
          vfreevent(i+1) = speed2
        end if !if ((i.gt.1).and.(i.lt.nlayer)) then
      end if  !if (LayerType(i).eq.SHADING) then
    end do

  end subroutine

  subroutine forcedventilation(iprop, frct, press, nmix, xwght, xgcon, xgvis, xgcp, s, H, hc, &
        & forcedspeed, Tinlet, Toutlet, Tav, hcv, qv, nperr, ErrorMessage)
  !**************************************************************************************************************
  !  Input:
  !
  !  iprop      Vector of gas identifiers
  !  frct      Fraction of gasses in a mixture
  !  nmix      Number of gasses in a mixture
  !  press      Pressure in mixture
  !  s1        Gap width [m]
  !  H          IGU cavity height [m]
  !  L          IGU cavity width [m]
  !  hc        Convective/conductive coefficient for non-vented gap
  !  Tav        Average temperature of gap surfaces
  ! Tinlet    Temperature of inlet air
  !
  !  Output:
  !
  !  hcv    Convective/conductive coefficient for vented gap
  !  qv    Heat transfer to the gap by vetilation [W/m^2]
  !  nperr      Error flag
  ! ErrorMessage string containing error message
  !**************************************************************************************************************
    integer, dimension(maxgas), intent(in) :: iprop
    integer, intent(in) :: nmix
    real(r64), dimension(maxgas), intent(in) :: frct
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgvis, xgcp
    real(r64), intent(in) :: press, s, H
    real(r64), intent(in) :: hc, Tav, forcedspeed, Tinlet

    real(r64), intent(out) :: hcv, qv, Toutlet
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64) :: H0, dens, cp, pr, con, visc

    call gasses90(Tav, iprop, frct, press, nmix, xwght, xgcon, xgvis, xgcp, con, visc, dens, cp, pr, 1, nperr, ErrorMessage)

    H0 = (dens * cp * s * forcedspeed) / (4.0d0 * hc + 8.0d0 * forcedspeed)

    Toutlet = Tav - (Tav - Tinlet)* (e**(-H/H0))

    qv = - dens * cp * forcedspeed * s * (Toutlet - Tinlet) / H

    !Need to calculate surface-to-air convection heat transfer coefficient.  This is needed later to calculate layer
    !to gap thermal resistance
    hcv = 2.0d0 * hc + 4.0d0 * forcedspeed

  end subroutine forcedventilation

  subroutine shadingin(iprop1, frct1, press1, nmix1, iprop2, frct2,  press2, nmix2, xwght, xgcon, xgvis, xgcp,  &
                        &  Atop, Abot, Al, Ar, Ah, s1, s2, H, L, angle, hc1, hc2,  &
                        &  speed1, speed2, Tgap1, Tgap2, Tav1, Tav2, hcv1, hcv2, qv1, qv2, nperr, ErrorMessage)
  !**************************************************************************************************************
  !  Input:
  !
  !  iprop1      Vector of gas identifiers
  !  frct1      Fraction of gasses in a mixture
  !  nmix1      Number of gasses in a mixture
  !  press1      Pressure in mixture
  !  iprop2      Vector of gas identifiers
  !  frct2      Fraction of gasses in a mixture
  !  nmix2      Number of gasses in a mixture
  !  press2      Pressure in mixture
  !  Atop      Opening between top of shading device and top of glazing cavity [m^2]
  !  Abot      Opening between bottom of shading device and bottom of glazing cavity [m^2]
  !  Al        Opening between left of shading device and left end of glazing cavity [m^2]
  !  Ar        Opening between right of shading device and right end of glazing cavity [m^2]
  !  Ah        Total area holes in the shading device [m^2]
  !  s1, s2      Gap width [m]
  !  H        IGU cavity height [m]
  !  L        IGU cavity width [m]
  !  angle      Window angle [degrees]
  !  hc1, hc2    Convective/conductive coefficient for non-vented gap
  !  Tav1, Tav2    Average temperature of gap surfaces
  !
  !  Output:
  !
  !  Tgap1, Tgap2  Temperature of vented gap
  !  hcv1, hcv2    Convective/conductive coefficient for vented gap
  !  qv1, qv2    Heat transfer to the gap by vetilation [W/m^2]
  !  speed1, speed2  Air/gas velocities in gaps around SD layer
  !  nperr      Error flag
  !**************************************************************************************************************
    use DataGlobals, only: KelvinConv

    integer, dimension(maxgas), intent(in) :: iprop1, iprop2
    integer, intent(in) :: nmix1, nmix2
    real(r64), dimension(maxgas), intent(in) :: frct1, frct2
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgvis, xgcp
    real(r64), intent(in) :: press1, press2, Al, Ar, Ah, s1, s2, H, L, angle
    real(r64), intent(inout) :: Atop, Abot
    real(r64), intent(in) :: hc1, hc2, Tav1, Tav2

    real(r64), intent(inout) :: Tgap1, Tgap2 !Objexx:ArgIN Changed IN to INOUT: These are used and modified
    real(r64), intent(out) :: hcv1, hcv2, qv1, qv2, speed1, speed2
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage


    real(r64) ::  A, A1, A2, B1, B2, C1, C2, D1, D2
    real(r64) ::  Zin1, Zin2, Zout1, Zout2
    real(r64) ::  A1eqin, A1eqout, A2eqin, A2eqout
    real(r64) ::  T0, tilt
    real(r64) ::  dens0, visc0, con0, pr0, cp0
    real(r64) ::  dens1, visc1, con1, pr1, cp1
    real(r64) ::  dens2, visc2, con2, pr2, cp2
    real(r64) ::  Tup, Tdown
    real(r64) ::  H01, H02, beta1, beta2, alpha1, alpha2, P1, P2
    real(r64) ::  qsmooth

    ! iteration parameters
    integer :: iter
    real(r64) :: TGapOld1, TGapOld2, Temp1, Temp2
    logical :: converged

    TGapOld1 = 0.0d0
    TGapOld2 = 0.0d0
    tilt = pi/180 * (angle - 90)
    T0 = 0.0d0 + KelvinConv
    A1eqin = 0.0d0
    A2eqout = 0.0d0
    A1eqout = 0.0d0
    A2eqin = 0.0d0
    P1 = 0.0d0
    P2 = 0.0d0

    call gasses90(T0, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con0, visc0, dens0, cp0, pr0, 1, &
                    nperr, ErrorMessage)

    ! exit on error:
    if ((nperr.gt.0).and.(nperr.lt.1000))  return

  !dr...check for error messages
    if ((Tgap1*Tgap2).eq.0) then
      nperr = 15
      ErrorMessage = 'Temperature of vented gap must be greater than 0 [K].'
      return
    end if

    if ((Atop + Abot).eq.0) then
  !    nperr = 16
  !    return
      Atop = 0.000001d0
      Abot = 0.000001d0
    end if

    converged = .false.
    iter = 0
    do while (.not. converged)
      iter = iter + 1
      call gasses90(Tgap1, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con1, visc1, dens1, cp1, pr1, 1, &
                    nperr, ErrorMessage)
      call gasses90(Tgap2, iprop2, frct2, press2, nmix2, xwght, xgcon, xgvis, xgcp, con2, visc2, dens2, cp2, pr2, 1, &
                    nperr, ErrorMessage)

      !  A = dens0 * T0 * GravityConstant * abs(cos(tilt)) * abs(Tgap1 - Tgap2) / (Tgap1 * Tgap2)

      !bi...Bug fix #00005:
      A = dens0 * T0 * GravityConstant * H * ABS(COS(tilt)) * ABS(Tgap1 - Tgap2) / (Tgap1 * Tgap2)

      if (A == 0.0d0) then
        qv1 = 0.0d0
        qv2 = 0.0d0
        speed1 = 0.0d0
        speed2 = 0.0d0
        hcv1 = 2.0d0 * hc1
        hcv2 = 2.0d0 * hc2
        return
      endif

      B1 = dens1 / 2
      B2 = (dens2 / 2) * ((s1 / s2) ** 2)

      C1 = 12 * visc1 * H / (s1 ** 2)
      C2 = 12 * visc2 * (H / (s2 ** 2)) * (s1 / s2)

      if (Tgap1 >= Tgap2) then
        A1eqin = Abot + 0.5d0 * Atop * (Al + Ar + Ah) / (Abot + Atop)
        A2eqout = Abot + 0.5d0 * Atop * (Al + Ar + Ah) / (Abot + Atop)
        A1eqout = Atop + 0.5d0 * Abot * (Al + Ar + Ah) / (Abot + Atop)
        A2eqin = Atop + 0.5d0 * Abot * (Al + Ar + Ah) / (Abot + Atop)
      else if (Tgap1 < Tgap2) then
        A1eqout = Abot + 0.5d0 * Atop * (Al + Ar + Ah) / (Abot + Atop)
        A2eqin = Abot + 0.5d0 * Atop * (Al + Ar + Ah) / (Abot + Atop)
        A1eqin = Atop + 0.5d0 * Abot * (Al + Ar + Ah) / (Abot + Atop)
        A2eqout = Atop + 0.5d0 * Abot * (Al + Ar + Ah) / (Abot + Atop)
      end if

      Zin1 = ((s1 * L / (0.6d0 * A1eqin)) - 1.0d0) ** 2
      Zin2 = ((s2 * L / (0.6d0 * A2eqin)) - 1.0d0) ** 2
      Zout1 = ((s1 * L / (0.6d0 * A1eqout)) - 1.0d0) ** 2
      Zout2 = ((s2 * L / (0.6d0 * A2eqout)) - 1.0d0) ** 2

      D1 = (dens1 / 2.0d0) * (Zin1 + Zout1)
      D2 = (dens2 / 2.0d0) * ((s1 / s2) ** 2) * (Zin2 + Zout2)

      A1 = B1 + D1 + B2 + D2
      A2 = C1 + C2

      speed1 = (SQRT((A2 ** 2) + ABS(4.0d0 * A * A1)) - A2) / (2.0d0 * A1)
      speed2 = speed1 * s1 / s2

      H01 = (dens1 * cp1 * s1 * speed1) / (4.0d0 * hc1 + 8.0d0 * speed1)
      H02 = (dens2 * cp2 * s2 * speed2) / (4.0d0 * hc2 + 8.0d0 * speed2)

      if ((H01 /= 0.0d0).and.(H02 /= 0.0d0)) then
        P1 = - H / H01
        P2 = - H / H02
      end if

      beta1 = e ** P1
      beta2 = e ** P2

      alpha1 = 1.0d0 - beta1
      alpha2 = 1.0d0 - beta2

      if (Tgap1 > Tgap2) then
        Tup = (alpha1 * Tav1 + beta1 * alpha2 * Tav2) / (1.0d0 - beta1 * beta2)
        Tdown = alpha2 * Tav2 + beta2 * Tup
      else if (Tgap2 >= Tgap1) then
        Tdown = (alpha1 * Tav1 + beta1 * alpha2 * Tav2) / (1.0d0 - beta1 * beta2)
        Tup = alpha2 * Tav2 + beta2 * Tdown
      end if

      TGapOld1 = Tgap1
      TGapOld2 = Tgap2

      if (Tgap1 > Tgap2) then
        Temp1 = Tav1 - (H01 / H) * (Tup - Tdown)
        Temp2 = Tav2 - (H02 / H) * (Tdown - Tup)
      else if (Tgap2 >= Tgap1) then
        Temp1 = Tav1 - (H01 / H) * (Tdown - Tup)
        Temp2 = Tav2 - (H02 / H) * (Tup - Tdown)
      end if

      Tgap1 = AirflowRelaxationParameter * Temp1 + (1.0d0 - AirflowRelaxationParameter) * TGapOld1
      Tgap2 = AirflowRelaxationParameter * Temp2 + (1.0d0 - AirflowRelaxationParameter) * TGapOld2

      converged = .false.
      if ((abs(Tgap1 - TGapOld1) < AirflowConvergenceTolerance) .or. (iter >= NumOfIterations)) then
        if (abs(Tgap2 - TGapOld2) < AirflowConvergenceTolerance) then
          converged = .true.
        end if
      endif

    end do

    hcv1 = 2.0d0 * hc1 + 4.0d0 * speed1
    hcv2 = 2.0d0 * hc2 + 4.0d0 * speed2

    if (Tgap2 >= Tgap1) then
      qv1 = - dens1 * cp1 * speed1 * s1 * L * (Tdown - Tup) / (H * L)
      qv2 = - dens2 * cp2 * speed2 * s2 * L * (Tup - Tdown) / (H * L)
    else if (Tgap2 < Tgap1) then
      qv1 =  dens1 * cp1 * speed1 * s1 * L * (Tdown - Tup) / (H * L)
      qv2 =  dens2 * cp2 * speed2 * s2 * L * (Tup - Tdown) / (H * L)
    end if

  !  write(*, *) Tup-Tdown
  !  write(*, 998) Tup - Tdown, qv1, qv2

  !998  format(f15.9, f15.9, f15.9)

  !bi..  testing - velocities output file
  !bi      open(unit = 33, file = 'velocities.out', status='unknown', form='formatted', iostat = er)

  !bi  write(33, 987) speed1

    qsmooth = (abs(qv1) + abs(qv2)) / 2.0d0

    if (qv1 > 0.0d0) then
      qv1 = qsmooth
      qv2 = - qsmooth
    else
      qv1 = - qsmooth
      qv2 = qsmooth
    end if

  987   format(' V = ', F8.6)

  end subroutine

  subroutine shadingedge(iprop1, frct1, press1, nmix1, iprop2, frct2,  press2, nmix2, xwght, xgcon, xgvis, xgcp, &
                  Atop, Abot, Al, Ar, Ah, s, H, L, angle, forcedspeed, hc, Tenv, Tav, Tgap, hcv, qv, nperr, ErrorMessage, speed)
  !**************************************************************************************************************
  !  Input:
  !
  !  iprop1      Vector of gas identifiers
  !  frct1      Fraction of gasses in a mixture
  !  nmix1      Number of gasses in a mixture
  !  press1      Pressure in mixture
  !  iprop2      Vector of gas identifiers
  !  frct2      Fraction of gasses in a mixture
  !  nmix2      Number of gasses in a mixture
  !  press2      Pressure in mixture
  !  Atop      Opening between top of shading device and top of glazing cavity [m^2]
  !  Abot      Opening between bottom of shading device and bottom of glazing cavity [m^2]
  !  Al        Opening between left of shading device and left end of glazing cavity [m^2]
  !  Ar        Opening between right of shading device and right end of glazing cavity [m^2]
  !  Ah        Total area holes in the shading device [m^2]
  !  s        Gap width [m]
  !  H        IGU cavity height [m]
  !  L        IGU cavity width [m]
  !  angle      Window angle [degrees]
  !  forcedspeed    Speed of forced ventilation [m/s]
  !  hc        Convective/conductive coefficient for non-vented gap
  !  Tenv      Enviromental temperature
  !  Tav        Average temperature of gap surfaces
  !
  !  Output:
  !
  !  Tgap      Temperature of vented gap
  !  hcv        Convective/conductive coefficient for vented gap
  !  qv        Heat transfer to the gap by vetilation [W/m^2]
  !  nperr      Error flag
  !  speed      Air velocity
  !**************************************************************************************************************
    use DataGlobals, only: KelvinConv

    integer, dimension(maxgas), intent(in) :: iprop1, iprop2
    integer, intent(in) :: nmix1, nmix2
    real(r64), dimension(maxgas), intent(in) :: frct1, frct2
    real(r64), dimension(maxgas), intent(in) :: xwght
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon, xgvis, xgcp
    real(r64), intent(in) :: press1, press2, Al, Ar, s, H, L, angle, hc, Tenv, Tav
    real(r64), intent(inout) :: Atop, Abot, Ah

    real(r64), intent(inout) :: Tgap !Objexx:ArgIN Changed IN to INOUT: Arg used and modified
    real(r64), intent(out) :: hcv, qv, speed
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64) :: A, A1, A2, B1, C1, D1
    real(r64) :: Zin1, Zout1
    real(r64) :: A1eqin, A1eqout
    real(r64) :: T0, tilt
    real(r64) :: dens0, visc0, con0, pr0, cp0
    !real(r64) :: dens1, visc1, con1, pr1, cp1
    real(r64) :: dens2, visc2, con2, pr2, cp2
    real(r64) :: Tgapout, forcedspeed
    real(r64) :: H0, P, beta

    ! iteration parameters
    integer :: iter
    real(r64) :: TGapOld
    logical :: converged

    tilt = pi/180.0d0 * (angle - 90.0d0)
    T0 = 0.0d0 + KelvinConv

    call gasses90(T0, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con0, visc0, dens0, cp0, pr0, 1, &
                    nperr, ErrorMessage)
    !call gasses90(Tenv, iprop1, frct1, press1, nmix1, xwght, xgcon, xgvis, xgcp, con1, visc1, dens1, cp1, pr1, 1, &
    !                nperr, ErrorMessage)

    ! exit on error:
    if ((nperr.gt.0).and.(nperr.lt.1000))  return

    !dr...check for error messages
    if ((Tgap*Tenv).eq.0.0d0) then
      nperr = 15
      ErrorMessage = 'Temperature of vented air must be greater then 0 [K].'
      return
    end if

    if ((Atop + Abot).eq.0) then
  !    nperr = 16
  !    return
      Atop = 0.000001d0
      Abot = 0.000001d0
    end if
    if ((Ah+Al+Ar).eq.0.0d0) then
      Ah = 0.000001d0
    end if

    converged = .false.
    iter = 0
    do while (.not. converged)
      iter = iter + 1
      call gasses90(Tgap, iprop2, frct2, press2, nmix2, xwght, xgcon, xgvis, xgcp, con2, visc2, dens2, cp2, pr2, 1, &
                      nperr, ErrorMessage)

      if ((nperr.gt.0).and.(nperr.lt.1000))  return

    !  A = dens0 * T0 * gravity * abs(cos(tilt)) * abs(Tgap - Tenv) / (Tgap * Tenv)

    !bi...Bug fix #00005:
      A = dens0 * T0 * GravityConstant * H * abs(cos(tilt)) * abs(Tgap - Tenv) / (Tgap * Tenv)
    !  A = dens0 * T0 * GravityConstant * H * abs(cos(tilt)) * (Tgap - Tenv) / (Tgap * Tenv)

      B1 = dens2 / 2
      C1 = 12.0d0 * visc2 * H / (s ** 2.0d0)

      if (Tgap > Tenv) then
        A1eqin = Abot + 0.5d0 * Atop * (Al + Ar + Ah) / (Abot + Atop)
        A1eqout = Atop + 0.5d0 * Abot * (Al + Ar + Ah) / (Abot + Atop)
      else if (Tgap <= Tenv) then
        A1eqout = Abot + 0.5d0 * Atop * (Al + Ar + Ah) / (Abot + Atop)
        A1eqin = Atop + 0.5d0 * Abot * (Al + Ar + Ah) / (Abot + Atop)
      end if

      Zin1 = ((s * L / (0.6d0 * A1eqin)) - 1) ** 2.0d0
      Zout1 = ((s * L / (0.6d0 * A1eqout)) - 1) ** 2.0d0

      D1 = (dens2 / 2.0d0) * (Zin1 + Zout1)

      A1 = B1 + D1
      A2 = C1

    !dr...recalculate speed if forced speed exist
    !bi...skip forced vent for now
    !  if (forcedspeed.ne.0) then
      if ((forcedspeed.ne.0.0d0).and.(CalcForcedVentilation.ne.0)) then
        speed = forcedspeed
      else
        speed = (SQRT((A2 ** 2) + ABS(4.0d0 * A * A1)) - A2) / (2.0d0 * A1)
    !  speed = abs((sqrt((A2 ** 2) + (4 * A * A1)) - A2) / (2 * A1))
      end if

      TGapOld = Tgap

      ! Speed is zero when environment temperature is equal to average layer temperatures
      ! For example, this can happen when inside and outside temperatures are equal
      if (speed /= 0.0d0) then
        H0 = (dens2 * cp2 * s * speed) / (4.0d0 * hc + 8.0d0 * speed)

        P = - H / H0
        beta = e ** P
        Tgapout = Tav - (Tav - Tenv) * beta
        Tgap = Tav - (H0 / H) * (Tgapout - Tenv)
      else
        Tgapout = Tav
        Tgap = Tav
      end if

      converged = .false.
      if ((abs(Tgap - TGapOld) < AirflowConvergenceTolerance) .or. (iter >= NumOfIterations)) then
        converged = .true.
      end if

      !if (iter > NumOfIterations) then
      !  converged = .true.
      !end if

    end do

  !bi...Test output:
  !  write(*,101) tenv, tgap, tgapout
  !101  format(f15.9, f15.9, f15.9)

    hcv = 2.0d0 * hc + 4.0d0 * speed

    qv = dens2 * cp2 * speed * s * L * (Tenv - Tgapout) / (H * L)

  end subroutine

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

end module TarcogShading

module ThermalEN673Calc

          ! MODULE INFORMATION:
          !       AUTHOR         D. Charlie Curcija
          !       DATE WRITTEN   July/2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Calculate thermal properties of IGU according to EN673 standard

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use TARCOGGassesParams
  use TARCOGGasses90
  use TARCOGParams

  implicit none

  public  Calc_EN673
  public  EN673ISO10292
  private linint
  private solar_EN673

  contains

  subroutine Calc_EN673(standard, nlayer, tout, tind, gap, thick, scon, emis, totsol,  &
          tilt, dir, asol, presure, iprop, frct, nmix, xgcon,xgvis,xgcp,xwght, theta, ufactor, hcin, &
          hin, hout, shgc, nperr, ErrorMessage, ibc, hg, hr, hs, Ra,Nu)

    use TARCOGArgs, ONLY : GoAhead
    use TarcogOutput

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!! function attributes:

    !!! INPUTS:

    !!! General:
    integer, intent(in) :: nlayer, standard

    !!! Environment related:
    real(r64), intent(in) ::  tout, tind, totsol, tilt, dir

    integer, dimension(2), intent(in) :: ibc

    !!! Layers:
    real(r64), dimension(maxlay2), intent(in) ::  emis
    real(r64), dimension(maxlay), intent(in) ::  asol


    !!! Gaps:
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxlay1, maxgas), intent(in) ::  frct
    real(r64), dimension(maxlay1), intent(in) ::  presure
    real(r64), dimension(maxgas, 3), intent(in) ::  xgcon, xgvis, xgcp
    real(r64), dimension(maxgas), intent(in) ::  xwght


    !!!! INPUTS/OUTPUTS:
    real(r64), dimension(maxlay), intent(inout) :: scon, thick
    real(r64), dimension(MaxGap), intent(inout) :: gap
    real(r64), intent(inout) :: hin, hout

    !!! OUTPUTS:
      !!! Overall:
    integer, intent(out) :: nperr
    character(len=*), intent(inout) :: ErrorMessage
    real(r64), intent(out) :: ufactor
    real(r64), intent(out) :: shgc
    real(r64), intent(out) :: hcin

    !!! Layers:
    real(r64), dimension(maxlay2), intent(out) :: theta

    !!! Gaps:
    real(r64), dimension(maxlay), intent(out) :: hg, hr, hs
    real(r64), dimension(maxlay), intent(out) :: Ra, Nu

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    real(r64), dimension(maxlay3) :: rs
    real(r64) :: rtot, sft

    !call  propcon90(standard, mgas, gcon, gvis, gcp, grho, wght, nperr)
    rtot=0.0d0
    sft=0.0d0
    if (GoAhead(nperr)) then
      call  EN673ISO10292(nlayer, tout, tind, emis, gap, thick, scon, tilt, iprop, frct,xgcon,xgvis,xgcp,xwght, &
                              presure, nmix, theta,standard, hg, hr, hs, hin, hout, hcin, ibc, rs, ufactor, Ra, Nu, nperr, &
                              ErrorMessage)

      if (GoAhead(nperr)) then
        rtot = 1.0d0/ufactor
        call  solar_EN673(dir, totsol, rtot, rs, nlayer, asol, sft, standard, nperr, ErrorMessage)
        if (GoAhead(nperr)) then
          shgc=sft
          if (WriteDebugOutput) call WriteOutputEN673(OutArgumentsFile, DBGD, nlayer, ufactor, hout, hin, Ra, Nu, hg, hr, hs, nperr)
        end if  ! GoAhead after solar
      end if  ! GoAhead after EN673ISO10292
    end if  ! GopAhead after propcon90

  end subroutine Calc_EN673

  subroutine EN673ISO10292(nlayer, tout, tind, emis, gap, thick, scon, tilt, iprop, frct, xgcon, xgvis, xgcp, xwght, &
      presure, nmix, theta, standard, hg, hr, hs, hin, hout, hcin, ibc, rs, ufactor, Ra, Nu, nperr, ErrorMessage)

    real(r64), intent(in) :: tout, tind
    integer, intent(in) :: standard
    integer, intent(in) :: nlayer
    real(r64), dimension(MaxGap), intent(in) :: gap
    real(r64), dimension(maxlay), intent(in) :: thick ,scon
    real(r64), dimension(maxlay2), intent(in) :: emis
    real(r64), dimension(maxlay1,maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: presure
    real(r64), dimension(maxgas, 3), intent(in) ::  xgcon, xgvis, xgcp
    real(r64), dimension(maxgas), intent(in) ::  xwght
    real(r64), dimension(maxlay3), intent(out) :: rs
    integer, dimension(maxlay1), intent(in) :: nmix
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(2), intent(in) :: ibc
    real(r64), intent(in) :: tilt
    real(r64), intent(in) :: hout

    real(r64), dimension(maxlay2), intent(out) :: theta
    real(r64), intent(inout) :: hin !Objexx:ArgIN Changed IN to INOUT: Arg used and modified
    real(r64), intent(out) :: hcin, ufactor
    real(r64), dimension(maxlay), intent(out) :: hr, hs, hg
    integer, intent(out) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    !dr...internal variables
    real(r64) :: Tm, diff, Rg
    real(r64), dimension(maxlay1) :: dT
    integer :: i, j, iter
    real(r64) :: dens, visc, con, cp, pr
    real(r64), dimension(maxlay) :: Gr, Nu, Ra
    real(r64) :: A, n, hrin, sumRs, sumRsold

    real(r64), parameter :: eps = 1.0d-4  ! set iteration accuracy

    real(r64), dimension(maxgas) :: frctg
    integer, dimension(maxgas) :: ipropg

!EPTeam    ! Initialization
!EPTeam - comment out until proven    con = 0.0d0 !Objexx:Uninit Line added to eliminate chance of use uninitialized

  !jel..hrin is 4.4 for standard clear glass:
    if ((emis(2*nlayer).lt.0.85d0).and.(emis(2*nlayer).gt.0.83d0)) then
      hrin =  4.4d0
    else
      hrin =  4.4d0 * emis(2*nlayer) / 0.837d0
  !       hrin = 4.4 * emis(2*nlayer) / 0.84  !old formula
    end if

    if (ibc(1).ne.1) then
      nperr = 38
      ErrorMessage = 'Boundary conditions for EN673 can be combined hout for outdoor and either convective (hcin) '//  &
         'or combined (hin) for indoor.  Others are not supported currently.'
      return
    end if

    if (ibc(2).eq.1) then
      CONTINUE
    else if (ibc(2).eq.2) then
      hcin = hin
      hin = hcin + hrin
    else
      nperr = 39
      ErrorMessage = 'CSM and SCW thermal models cannot be used for outdoor and indoor SD layers.'
      return
    end if

    rs(1) = 1.0d0/hout
    rs(2*nlayer+1) = 1.0d0/hin

    Tm = 283.0d0
    iter = 1
    sumRs = 0.0d0
    Rg = 0.0d0

    !bi Init vectors:
    Gr = 0.0d0
    Nu = 0.0d0
    Ra = 0.0d0
    con = 0.0d0

    do i = 1, nlayer
      rs(2*i) = thick(i)/scon(i) ! thermal resistance of each glazing layer
      Rg = Rg + rs(2*i) ! cumulative thermal resistance of glazing layers
    end do

    if (nlayer.eq.1) then ! Calc U-Factor and glazing temperature for simgle glazing and return
      ufactor = 1.0d0 / (1.0d0 / hin + 1.0d0 / hout + Rg)
      theta(1) = ufactor*(tind-tout)/hout+tout
      theta(2) = tind - ufactor*(tind-tout)/hin
      return
    else
      if (tind.gt.tout) then
        !dr...linear interpolation for gas conductance coefficients
        if (tilt.eq.0.0d0) then
          A = 0.16d0
          n = 0.28d0
        else if ((tilt.gt.0.0d0).and.(tilt.lt.45.0d0)) then
          call  linint(0.0d0, 45.0d0, 0.16d0, 0.1d0, tilt, A)
          call  linint(0.0d0, 45.0d0, 0.28d0, 0.31d0, tilt, n)
        else if (tilt.eq.45.0d0) then
          A = 0.10d0
          n = 0.31d0
        else if ((tilt.gt.45.0d0).and.(tilt.lt.90.0d0)) then
          call  linint(45.0d0, 90.0d0, 0.1d0, 0.035d0, tilt, A)
          call  linint(45.0d0, 90.0d0, 0.31d0, 0.38d0, tilt, n)
        else if (tilt.eq.90) then
          A = 0.035d0
          n = 0.38d0
        end if  ! tilt

        !c   gas constants
        !    open(unit=18,  file='gas.dbg',  status='unknown', access='APPEND',
        !  2            form='formatted', iostat=nperr)
        !    write(18,*) 'New calc'
        do i = 1, nlayer - 1
          !22222  format('Gas #', I3, ' : Dens=', F9.7, ' Visc=', F12.9, ' Cond=', F9.7, ' Cp=', F9.7)
          !   write(18, 22222) iprop(i+1, j), tempDens, gvis(iprop(i+1,j), 1), gcon(iprop(i+1,j), 1), gcp(iprop(i+1,j), 1)
          dT(i) = 15.0d0 / (nlayer - 1)  ! set initial temperature distribution
          do j=1, nmix(i+1)
            ipropg(j) = iprop(i+1,j)
            frctg(j) = frct(i+1,j)
          end do
          call  gasses90(Tm, ipropg, frctg, presure(i+1), nmix(i+1), xwght, xgcon, xgvis, xgcp, &
                          con, visc, dens, cp, pr, standard, nperr, ErrorMessage)
          Gr(i) = (GravityConstant * gap(i)**3.0d0 * dT(i) * dens**2) / (Tm * visc**2)
          Ra(i) = Gr(i) * pr
          Nu(i) = A*Ra(i)**n
          if (Nu(i).lt.1.0d0) then
            Nu(i) = 1.0d0
          end if
          hg(i) = Nu(i) * con / gap(i)
        end do  ! gaps
      else
        do  i = 1, nlayer - 1
          Nu(i)   = 1.0d0
          hg(i) = Nu(i) * con / gap(i) !Objexx:Uninit con is uninitialized: Initialization added above: With con=0 this line is pointless
        end do
      end if
      do i = 1, nlayer - 1
        hr(i) = 4.0d0 * StefanBoltzmann * (1.0d0 / emis(2*i) + 1.0d0 / emis(2*i+1) - 1.0d0)**(-1.0d0) * Tm**3
        hs(i) = hg(i) + hr(i)
        rs(2*i+1) = 1.0d0/hs(i) ! Thermal resistance of each gap
        sumRs = sumRs + rs(2*i+1)
      end do
        !    write(18,*) '------'
        !    close(18)

      ufactor = 1.0d0 / (1.0d0 / hin + 1.0d0 / hout + sumRs + Rg)
      theta(1) = ufactor*(tind-tout)/hout+tout
      theta(2*nlayer) = tind - ufactor*(tind-tout)/hin
      do i=2,nlayer
        theta(2*i-2) = ufactor*(tind-tout)*thick(1)/scon(1)+theta(2*i-3)
        theta(2*i-1) = ufactor*(tind-tout)/hs(i-1) + theta(2*i-2)
      end do  ! end of first iteration

  !bi More iterations:
      DO
        sumRsold = sumRs
        sumRs = 0.0d0

        if ((standard.eq.EN673).and.(nlayer.eq.2)) then
          return  ! If EN673 declared values path and glazing has 2 layers, end claculations and return
        else
          if (tind.gt.tout) then
            do  i=1,nlayer - 1
              dT(i) = 15.0d0 * (1.0d0 / hs(i)) / sumRsold  ! updated temperature distribution
              if (standard.eq.EN673) then
                Tm = 283.0d0
              else
                Tm = (theta(2*i) + theta(2*i+1))/2.0d0
              end if
              do j=1, nmix(i+1)
                ipropg(j) = iprop(i+1,j)
                frctg(j) = frct(i+1,j)
              end do  ! j, gas mix
              call  gasses90(Tm, ipropg, frctg, presure(i+1), nmix(i+1), xwght, xgcon, xgvis, xgcp, &
                              con, visc, dens, cp, pr, standard, nperr, ErrorMessage)
              Gr(i) = (GravityConstant * gap(i)**3.0d0 * dT(i) * dens**2.0d0) / (Tm * visc**2.0d0)
              Ra(i) = Gr(i) * pr
              Nu(i) = A*Ra(i)**n
              if (Nu(i).lt.1.0d0) then
                Nu(i) = 1.0d0
              end if
              hg(i) = Nu(i) * con / gap(i)
            end do  ! i, gaps
          else
            do i = 1, nlayer-1
              Nu(i)   = 1.0d0
              hg(i) = Nu(i) * con / gap(i) !Objexx:Uninit con may be uninitialized: Initialization added above: With con=0 this line is pointless
            end do
          end if   ! tind > tout
        end if

        do i = 1, nlayer - 1
      !      hr(i) = 4 * sigma * (1/emis(2*i) + 1/emis(2*i+1) - 1)**(-1) * Tm**3
          hs(i) = hg(i) + hr(i)
          rs(2*i+1) = 1.0d0 / hs(i) ! Thermal resistance of each gap
          sumRs = sumRs + rs(2*i+1)
        end do
        ufactor = 1.0d0 / (1.0d0 / hin + 1.0d0 / hout + sumRs + Rg)
        theta(1) = ufactor * (tind - tout) / hout + tout
        theta(2*nlayer) = tind - ufactor*(tind-tout)/hin
        do i=2,nlayer
          theta(2*i-2) = ufactor * (tind - tout) * thick(1) / scon(1) + theta(2*i-3)
          theta(2*i-1) = ufactor * (tind - tout) / hs(i-1) + theta(2*i-2)
        end do
        iter = iter + 1  ! end of next iteration
        diff = abs(sumRs - sumRsold)
        !bi: perhaps we should also limit No. of iterations?
        if (diff.lt.eps) EXIT  ! tolerance was met - exit loop
      end do  ! remaining iterations
    end if

    !dr...END OF ITERATIONS
    return

  end subroutine

  subroutine linint(x1, x2, y1, y2, x, y)

    real(r64), intent(in) :: x1, x2, y1, y2, x
    real(r64), intent(out) :: y

    y = (y2 - y1) / (x2 - x1) * (x - x1) + y1 !Objexx:DivZero Should protect against divide by zero

    return

  end subroutine

  subroutine solar_EN673(dir, totsol, rtot, rs, nlayer, absol, sf, standard, nperr, ErrorMessage)
  !***********************************************************************
  !   This subroutine calculates the shading coefficient for a window.
  !***********************************************************************
  !  Inputs:
  !    absol     array of absorped fraction of solar radiation in lites
  !    totsol    total solar transmittance
  !    rtot      total thermal resistance of window
  !    rs        array of thermal resistances of each gap and layer
  !    layer     number of layers
  !  Outputs:
  !    sf        solar gain of space
  !

    integer, intent(in) :: nlayer, standard
    real(r64), dimension(maxlay3), intent(in) :: rs
    real(r64), dimension(maxlay), intent(in) :: absol
    real(r64), intent(in) :: totsol, rtot, dir

    real(r64), intent(out) :: sf
    integer, intent(out) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    integer :: i, j
    real(r64) :: fract, flowin

    !!!!!!!!!!!!!!!!!!!!

    fract = 0.0d0
    flowin = 0.0d0
    sf = 0.0d0

    ! evaluate inward flowing fraction of absorbed radiation:
    if ((standard.eq.EN673).or.(standard.eq.EN673Design)) then
      if (nlayer.eq.1) then
        fract = dir*absol(1)*(rs(1)*rs(3))/(rs(1)*(rs(1) + rs(3)))
      else
        flowin = (rs(1) + 0.5d0 * rs(2)) / rtot
        fract = dir * absol(1) * rs(10)
        do i=2, nlayer
          j = 2 * i
          flowin = flowin + (0.5d0 * (rs(j-2) + 0.5d0 * rs(j)) + rs(j-1)) / rtot
          fract = fract + absol(i) * flowin
        end do
        fract = fract + dir * absol(nlayer) * rs(2*nlayer) / 2.0d0
      end if
    else
      nperr = 28
      ErrorMessage = 'Invalid code for standard.'
      return
    end if

    sf = totsol+fract       ! add inward fraction to directly transmitted fraction

  end subroutine

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
end module ThermalEN673Calc

module ThermalISO15099Calc
  !***********************************************************************
  ! ThermalISO15099Calc: a TARCOG module
  !
  !    module For Calculation of Thermal Performance Indices For Center
  !     of Glass According to ISO 15099
  !
  ! History of Revisions:
  !
  !  Revision: 6.0.36  (June/22/2010)
  !   - Initial setup, extracted and refactored from TARCOG.for
  !
  !***********************************************************************

          ! MODULE INFORMATION:
          !       AUTHOR         D. Charlie Curcija
          !       DATE WRITTEN   July/2000
          !       MODIFIED       na
          !       RE-ENGINEERED  March/27/2012, Simon Vidanovic

          !  Revision: 7.0.13  (March/27/2012), Simon Vidanovic
          !   - feature: New set of equaitons is set instead of hhat coefficents and new approach to solution which improves
          !               speed and stability.  Note that this solution does not include laminates

          ! PURPOSE OF THIS MODULE:
          ! Module For Calculation of Thermal Performance Indices For Center
          !  of Glass According to ISO 15099

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use DataPrecisionGlobals
  use TARCOGGassesParams
  use TARCOGParams
  use TARCOGArgs
  use TARCOGOutput
  use TARCOGGasses90
  use TarcogShading

  implicit none

  public  Calc_ISO15099
  private film
  private therm1d
  private guess
  private TemperaturesFromEnergy
  private solarISO15099
  private resist
  private hatter
  private filmi
  private filmg
  private filmPillar
  private nusselt
  !private picard
  private adjusthhat
  private storeIterationResults
  private CalculateFuncResults

  contains

  subroutine film(tex,tw,ws,iwd,hcout,ibc)
    !***********************************************************************
    ! purpose - to find outdoor film coeff
    !***********************************************************************
    ! Inputs -
    !   tex - outdoor air temp [k]
    !   tw - outside surface temp
    !   ws - wind speed [m/s]
    !   iwd - wind direction [0 - windward; 1 - leeward]
    ! Outputs
    !   hcout - convective film coeff [w m-2 k-1]

    real(r64), intent(in) :: tex, tw, ws
    integer, intent(in) :: iwd, ibc
    real(r64), intent(out) :: hcout

    real(r64), parameter :: conv = 5.6783d0

    real(r64) :: vc, acoef, bexp

    ! calculation of convection component of exterior film coefficient using the :
    select case (ibc)
      case (0)    !ISO 15099
        hcout = 4.0d0 + 4.0d0 * ws
      case (-1)   ! old ASHRAE SPC142 correlation
        if (iwd .eq. 0) then    ! windward
          if (ws.gt.2.0d0) then
            vc = 0.25d0 * ws
          else
            vc = 0.5d0
          end if
        else        ! leeward
          vc = 0.3d0 + 0.05d0 * ws
        end if
        hcout = 3.28d0 * ((vc)**0.605d0)
        hcout = conv*hcout     ! convert to metric
      case (-2)  ! Yazdanian-Klems correlation:
        if (iwd .eq. 0) then    ! windward
          acoef = 2.38d0
          bexp = 0.89d0
        else        ! leeward
          acoef = 2.86d0
          bexp = 0.617d0
        end if
        hcout = ((0.84d0*(tw-tex)**0.33d0)**2+(acoef*ws**bexp)**2)**0.5d0
      case (-3)  ! Kimura correlation (Section 8.4.2.3 in ISO 15099-2001):
        if (iwd .eq. 0) then    ! windward
          if (ws.gt.2.0d0) then
            vc = 0.25d0 * ws
          else
            vc = 0.5d0 * ws
          end if
        else        ! leeward
          vc = 0.3d0 + 0.05d0*ws
        end if
        hcout = 4.7d0 + 7.6d0 * vc
    end select

  end subroutine film

  subroutine Calc_ISO15099(nlayer,iwd,tout,tind,trmin,wso,wsi,dir,outir,isky,tsky,esky,fclr,VacuumPressure,VacuumMaxGapThickness, &
          & gap,thick,scon,tir,emis,totsol,  &
          &  tilt,asol,height,heightt,width,presure,iprop,frct,xgcon,xgvis,xgcp,xwght,gama,nmix,  &
          & SupportPillar, PillarSpacing, PillarRadius, &
          &  theta,q,qv,ufactor,sc,hflux,hcin,hcout,hrin,hrout,hin,hout,hcgas,hrgas,shgc,nperr,ErrorMessage, &
          &  shgct,tamb,troom,ibc,Atop,Abot,Al,Ar,Ah, SlatThick, SlatWidth, SlatAngle, &
          &  SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB, &
          &  sumsol, Ra,Nu, ThermalMod, Debug_mode, &
          &  ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, &
          &  HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, SHGCCalc, NumOfIterations)

    !!! function attributes:

    !!! INPUTS:

    !!! General:
    integer, intent(in) :: nlayer
    real(r64), intent(in) :: width, height, heightt
    integer, intent(in) :: ThermalMod
    integer, intent(in) :: SHGCCalc      ! SHGC calculation switch:
                                          !    0 - do not perform SHGC calculations
                                          !    1 - perform SHGC calculations
    integer, intent(in) :: Debug_mode ! Switch for debug output files:
                                        !    0 - don't create debug output files
                                        !    1 - append results to existing debug output file
                                        !    2 - store results in new debug output file
                                        !   3 - save in-between results (in all iterations) to existing debug file

    !!! Environment related:
    real(r64), intent(in) ::  wso, wsi, dir, outir, tsky, fclr, VacuumPressure, VacuumMaxGapThickness, totsol, tilt
    real(r64), intent(inout) :: tout, tind
    integer, intent(in) :: iwd, isky
    integer, dimension(2), intent(in) :: ibc
    integer, intent(out) :: NumOfIterations

      !!! Layers:
    integer, dimension(maxlay), intent(in) :: LayerType
    real(r64), dimension(maxlay2), intent(in) ::  tir, emis
    real(r64), dimension(maxlay), intent(in) ::  asol

    !!! Venetians:
    real(r64), dimension(maxlay), intent(in) ::  Atop, Abot, Al, Ar, Ah
    real(r64), dimension(maxlay), intent(in) ::  SlatThick, SlatWidth, SlatAngle
    real(r64), dimension(maxlay), intent(in) ::  SlatCond, SlatSpacing, SlatCurve
    real(r64), dimension(maxlay1), intent(in) ::  vvent, tvent

    !!! Laminates:
    integer, dimension(maxlay), intent(in) :: nslice
    real(r64), dimension(maxlay), intent(in) :: LaminateA(maxlay), LaminateB(maxlay), sumsol(maxlay)

    !!! Gaps:
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxlay1, maxgas), intent(in) ::  frct
    real(r64), dimension(maxlay1), intent(in) ::  presure
    real(r64), dimension(maxgas,3), intent(in) :: xgcon, xgvis, xgcp
    real(r64), dimension(maxgas), intent(in) :: xwght, gama

    integer, dimension(maxlay), intent(in) :: SupportPillar    ! Shows whether or not gap have support pillar
                                                               ! 0 - does not have support pillar
                                                               ! 1 - have support pillar
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing  ! Pillar spacing for each gap (used in case there is support pillar)
    real(r64), dimension(maxlay), intent(in) :: PillarRadius   ! Pillar radius for each gap (used in case there is support pillar)

    real(r64), intent(in) :: SDScalar

    !!!! INPUTS/OUTPUTS:
    real(r64), intent(inout) :: esky, trmin
    real(r64), dimension(maxlay), intent(inout) :: scon, thick
    real(r64), dimension(MaxGap), intent(inout) :: gap
    real(r64), intent(inout) :: hin, hout

    !!! OUTPUTS:
      !!! Overall:
    integer, intent(out) :: nperr
    character(len=*), intent(inout) :: ErrorMessage
    real(r64), intent(out) :: ufactor, sc, hflux
    real(r64), intent(out) :: shgc, shgct
    real(r64), intent(out) :: hcin, hrin, hcout, hrout, tamb, troom

    !!! Layers:
    real(r64), dimension(maxlay2), intent(inout) :: theta
    real(r64), dimension(maxlay3), intent(out) :: q
    real(r64), dimension(maxlay1), intent(out) :: qv
    real(r64), dimension(maxlay), intent(out) :: Keff

    !!! Gaps:
    real(r64), dimension(maxlay1), intent(out) :: hcgas, hrgas
    real(r64), dimension(maxlay), intent(out) :: Ra, Nu

    !!! Shading related:
    real(r64), intent(out) :: ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn
    real(r64), intent(out) :: HcUnshadedOut, HcUnshadedin
    real(r64), dimension(MaxGap), intent(out) :: ShadeGapKeffConv

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Variables

    real(r64), dimension(maxlay2) :: thetas, rir
    real(r64), dimension(maxlay1) :: hcgass, hrgass
    real(r64), dimension(maxlay3) :: rs=0.0d0

  !  real(r64) :: grho(maxgas,3)
    real(r64), dimension(maxlay3) :: qs
    real(r64), dimension(maxlay1) :: qvs
    real(r64), dimension(maxlay) :: LaminateAU, sumsolU, sol0
    real(r64) :: shgct_NOSD,trmout

    real(r64) ::  Gout, Gin, AchievedErrorTolerance, AchievedErrorToleranceSolar
    integer :: NumOfIter, NumOfIterSolar

    real(r64) ::  tgg, qc1, qc2, qcgg
    real(r64), dimension(maxlay1) ::  qcgas, qcgaps, qrgas, qrgaps

    real(r64) ::  ShadeHcModifiedOut, ShadeHcModifiedIn

    !real(r64) :: xgrho(maxgas, 3)   !!!!!!!!!!!!!!!!!1


    !cbi...Variables for "unshaded" run:

    logical :: NeedUnshadedRun
    integer :: nlayer_NOSD
    real(r64) ::  AchievedErrorTolerance_NOSD
    integer :: NumOfIter_NOSD
    real(r64), dimension(maxlay) ::  Atop_NOSD, Abot_NOSD, Al_NOSD, Ar_NOSD, Ah_NOSD
    real(r64), dimension(maxlay) ::  SlatThick_NOSD, SlatWidth_NOSD, SlatAngle_NOSD
    real(r64), dimension(maxlay) ::  SlatCond_NOSD, SlatSpacing_NOSD, SlatCurve_NOSD
    real(r64), dimension(maxlay1) ::  vvent_NOSD, tvent_NOSD, qv_NOSD
    real(r64), dimension(maxlay3) ::  q_NOSD
    real(r64) ::  hin_NOSD, flux_NOSD, hcin_NOSD, hrin_NOSD, hcout_NOSD, hrout_NOSD
    real(r64) ::  tamb_NOSD, troom_NOSD
    integer, dimension(maxlay) :: LayerType_NOSD
    real(r64) ::  ufactor_NOSD,sc_NOSD,hflux_NOSD,shgc_NOSD,hout_NOSD
    real(r64), dimension(maxlay) ::  gap_NOSD, thick_NOSD, scon_NOSD
    real(r64), dimension(maxlay2) :: emis_NOSD, rir_NOSD, tir_NOSD
    real(r64), dimension(maxlay2) :: theta_NOSD
    real(r64), dimension(maxlay1, maxgas) :: frct_NOSD
    integer, dimension(maxlay1, maxgas) :: iprop_NOSD
    integer, dimension(maxlay1) :: nmix_NOSD
    real(r64), dimension(maxlay1) ::  presure_NOSD
    real(r64), dimension(maxlay1) ::  hcgas_NOSD, hrgas_NOSD
    !real(r64) ::  rs_NOSD(maxlay3)!,sol(maxlay)
    real(r64), dimension(maxlay) ::  LaminateA_NOSD, LaminateB_NOSD, sumsol_NOSD
    real(r64), dimension(maxlay) ::  Ra_NOSD, Nu_NOSD
    real(r64) ::  ShadeEmisRatioOut_NOSD, ShadeEmisRatioIn_NOSD
    real(r64) ::  ShadeHcRatioOut_NOSD, ShadeHcRatioIn_NOSD
    real(r64) ::  ShadeHcModifiedOut_NOSD, ShadeHcModifiedIn_NOSD
    real(r64), dimension(maxlay) ::  Ebb, Ebf, Rb, Rf
    real(r64), dimension(maxlay) ::  Ebbs, Ebfs, Rbs, Rfs
    real(r64), dimension(maxlay) ::  Ebb_NOSD, Ebf_NOSD, Rb_NOSD, Rf_NOSD

    real(r64), dimension(MaxGap) ::  ShadeGapKeffConv_NOSD
    real(r64), dimension(maxlay1) ::  qcgas_NOSD, Keff_NOSD, qrgas_NOSD
    integer, dimension(maxlay) :: nslice_NOSD
    real(r64), dimension(maxlay1) ::  vfreevent_NOSD

    integer :: FirstSpecularLayer, LastSpecularLayer

    real(r64), dimension(maxlay1) ::  vfreevent

    !cbi...Other variables:
    real(r64) ::  flux, hint, houtt, Ebsky, Ebroom
    integer :: i, j
    integer :: OriginalIndex, UnshadedDebug
    real(r64) ::  rtot=0.0d0
    real(r64) ::  sft=0.0d0
    real(r64) ::  hcins=0.0d0
    real(r64) ::  hrins=0.0d0
    real(r64) ::  hins=0.0d0
    real(r64) ::  hcouts=0.0d0
    real(r64) ::  hrouts=0.0d0
    real(r64) ::  houts=0.0d0
    real(r64) ::  ufactors=0.0d0
    real(r64) ::  fluxs=0.0d0
    real(r64) ::  qeff=0.0d0
    real(r64) ::  flux_nonsolar=0.0d0

    AchievedErrorTolerance = 0.0d0
    AchievedErrorToleranceSolar = 0.0d0
    AchievedErrorTolerance_NOSD = 0.0d0

    call PrepVariablesISO15099(nlayer, tout, tind, trmin, isky, outir, tsky, esky, fclr, gap, thick, scon, tir, emis,  &
          &  tilt, hin, hout, ibc, SlatThick, SlatWidth, SlatAngle, SlatCond, LayerType,   &
          &  ThermalMod, SDScalar, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn,  &
          & Keff, ShadeGapKeffConv, sc, shgc, ufactor, flux, LaminateAU, sumsolU, sol0,  &
          & hint, houtt, trmout, ebsky, ebroom, Gout, Gin, rir, vfreevent, nperr, ErrorMessage)

    !No option to take hardcoded variables.  All gas coefficients are now passed from outside.
    !if (GoAhead(nperr)) call propcon90(ISO15099,mgas,xgcon,xgvis,xgcp,xgrho,xwght,nperr)

    ! exit on error
    if (.not.(GoAhead(nperr))) return

    !bi...Write intermediate results to output file:
    if (WriteDebugOutput)  then
      call WriteModifiedArguments(InArgumentsFile, DBGD, esky, trmout, trmin, ebsky, ebroom, Gout, Gin, &
                                    nlayer, LayerType, nmix, frct, thick, scon, gap, xgcon, xgvis, xgcp, xwght)
    end if

  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !
  !     This is "solar radiation" pass
  !
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  !This is main calculation in case UFactor calculations will not be performed
    if ((dir.gt.0.0d0).or.(SHGCCalc.eq.0)) then
      ! call therm1d to calculate heat flux with solar radiation

      call therm1d(nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, dir, Ebsky, Gout, Trmout, Trmin, &
                    Ebroom, Gin, tir, rir, emis, gap, thick, scon, tilt, asol, height, heightt, width, iprop, frct, presure, &
                    nmix, xwght, xgcon, xgvis, xgcp, gama, SupportPillar, PillarSpacing, PillarRadius, theta, q, qv, flux,  &
                    hcin, hrin, hcout, hrout, hin, hout, hcgas, hrgas, ufactor, nperr, ErrorMessage, tamb, troom, ibc, &
                    Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Ra, Nu, vfreevent, qcgas, qrgas, Ebf, Ebb, Rf, Rb, &
                    ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcModifiedOut, ShadeHcModifiedIn, &
                    ThermalMod, Debug_mode, AchievedErrorToleranceSolar, NumOfIterSolar)

      NumOfIterations = NumOfIterSolar
      !exit on error:

      if (nlayer.gt.1) then
        do i=1, nlayer-1
          Keff(i)   = gap(i)   * q(2*i+1) / (theta(2*i+1) - theta(2*i))
          if (IsShadingLayer(LayerType(i))) then
            Keff(i)   = gap(i)   * q(2*i+1) / (theta(2*i+1) - theta(2*i))
          end if
          if (IsShadingLayer(LayerType(i+1))) then
            Keff(i)   = gap(i)   * q(2*i+1) / (theta(2*i+1) - theta(2*i))
          end if
        end do
      end if

      if (.not.(GoAhead(nperr))) return

      !No need to store results in case of non-ufactor run
      if ((SHGCCalc.gt.0).and.(dir.gt.0.0d0)) then
        call solarISO15099(totsol, rtot, rs, nlayer, asol, sft)
        shgct = sft
        shgct_NOSD = 0.0d0
        hcins=hcin
        hrins=hrin
        hins=hin
        hcouts=hcout
        hrouts=hrout
        houts=hout
        ufactors=ufactor
        fluxs=flux
        do i=1,nlayer
          thetas(2*i-1)=theta(2*i-1)
          thetas(2*i)=theta(2*i)
          Ebbs(i) = Ebb(i)
          Ebfs(i) = Ebf(i)
          Rbs(i) = Rb(i)
          Rfs(i) = Rf(i)
          qs(2*i - 1) = q(2*i - 1)
          qs(2*i) = q(2*i)
          !qprims(2*i - 1) = qprim(2*i - 1)
          !qprims(2*i) = qprim(2*i)
          qvs(2*i - 1) = qv(2*i - 1)
          qvs(2*i) = qv(2*i)
          hcgass(i)=hcgas(i)
          hrgass(i)=hrgas(i)
          qrgaps(i)=qrgas(i)
          qcgaps(i)=qcgas(i)
        end do
    !    CHECK THIS!
        qs(2*nlayer+1) = q(2*nlayer+1)
      end if !if (UFactorCalc.gt.0) then

    end if

    !No solar radiation pass is not needed to be calculated
    !if ((SHGCCalc.gt.0).or.(dir.eq.0)) then
    if (SHGCCalc.gt.0) then

        !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        !
        !      This is "no solar radiation" pass
        !
        !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          hin=hint
          hout=houtt

          ! call therm1d to calculate heat flux without solar radiation
          call therm1d(nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, &
                        0.0d0, Ebsky, Gout, Trmout, Trmin, Ebroom, Gin, tir, rir, emis, gap, thick, scon,  &
                        tilt, sol0, height, heightt, width, iprop, frct, presure, nmix, xwght, xgcon, xgvis, xgcp, gama, &
                        SupportPillar, PillarSpacing, PillarRadius, &
                        theta, q, qv, flux, hcin, hrin, hcout, hrout, hin, hout, hcgas, hrgas, ufactor, nperr, ErrorMessage, &
                        tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Ra, Nu, vfreevent, qcgas, qrgas,  &
                        Ebf, Ebb, Rf, Rb, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcModifiedOut, ShadeHcModifiedIn, &
                        ThermalMod, Debug_mode, AchievedErrorTolerance, NumOfIter)

          NumOfIterations = NumOfIter

          !exit on error:
          if (.not.(GoAhead(nperr))) return

          !bi...Keep hcout, hcin in case this is an unshaded system:
          HcUnshadedOut = hcout
          HcUnshadedIn  = hcin

          !bi...do an Unshaded run if necessary (Uvalue/Winter conditions):
          !bi...Prepare variables for UNSHADED (NO SD) run:

          NeedUnshadedRun = .FALSE.
          FirstSpecularLayer = 1
          LastSpecularLayer = nlayer
          nlayer_NOSD = nlayer
          if (IsShadingLayer(LayerType(1))) then
            nlayer_NOSD = nlayer_NOSD - 1
            FirstSpecularLayer = 2
            NeedUnshadedRun = .TRUE.
          end if

        !  if (LayerType(nlayer).eq.VENETBLIND) then
          if (IsShadingLayer(LayerType(nlayer))) then
            nlayer_NOSD = nlayer_NOSD-1
            LastSpecularLayer = nlayer-1
            NeedUnshadedRun = .TRUE.
          end if

          ! no unshaded run for now
          NeedUnshadedRun = .false.
          !bi...Set outdoor & indoor gas properties:
          if (NeedUnshadedRun) then
            nmix_NOSD(1)    = nmix(1)
            presure_NOSD(1) = presure(1)
            nmix_NOSD(nlayer_NOSD+1)  = nmix(nlayer+1)
            presure_NOSD(nlayer_NOSD+1) = presure(nlayer+1)
            do j = 1, nmix(1)
              iprop_NOSD(1, j) = iprop(1, j)
              frct_NOSD(1, j)  = frct(1, j)
            end do
            do j = 1, nmix(nlayer_NOSD+1)
              iprop_NOSD(nlayer_NOSD+1, j) = iprop(nlayer+1, j)
              frct_NOSD(nlayer_NOSD+1, j)  = frct(nlayer+1, j)
            end do
            do i = 1, nlayer_NOSD
              OriginalIndex = FirstSpecularLayer + i - 1
              Atop_NOSD(i) = Atop(OriginalIndex)
              Abot_NOSD(i) = Abot(OriginalIndex)
              Al_NOSD(i) = Al(OriginalIndex)
              Ar_NOSD(i) = Ar(OriginalIndex)
              Ah_NOSD(i) = Ah(OriginalIndex)

              SlatThick_NOSD(i) = SlatThick(OriginalIndex)
              SlatWidth_NOSD(i) = SlatWidth(OriginalIndex)
              SlatAngle_NOSD(i) = SlatAngle(OriginalIndex)
              SlatCond_NOSD(i)  = SlatCond(OriginalIndex)
              SlatSpacing_NOSD(i) = SlatSpacing(OriginalIndex)
              SlatCurve_NOSD(i) = SlatCurve(OriginalIndex)

              !cbi...    TO do when Forced Ventilation is implemented: take care of appropriate arguments!!!
              !
              !      vvent_NOSD
              !      tvent_NOSD
              !

              LayerType_NOSD(i) = LayerType(OriginalIndex)

              thick_NOSD(i)    = thick(OriginalIndex)
              scon_NOSD(i)     = scon(OriginalIndex)
              tir_NOSD(2*i-1)  = tir(2*OriginalIndex-1)
              emis_NOSD(2*i-1) = emis(2*OriginalIndex-1)
              emis_NOSD(2*i)   = emis(2*OriginalIndex)
              rir_NOSD(2*i-1)  = rir(2*OriginalIndex-1)
              rir_NOSD(2*i)    = rir(2*OriginalIndex)

              gap_NOSD(i)        = gap(OriginalIndex)

              if (i < nlayer_NOSD) then
                nmix_NOSD(i+1)     = nmix(OriginalIndex+1)
                presure_NOSD(i+1)  = presure(OriginalIndex+1)
                do j = 1, nmix_NOSD(i+1)
                  iprop_NOSD(i+1,j)  = iprop(OriginalIndex+1, j)
                  frct_NOSD(i+1,j)   = frct(OriginalIndex+1, j)
                end do
              end if

              LaminateA_NOSD(i)  = LaminateA(OriginalIndex)
              LaminateB_NOSD(i)  = LaminateB(OriginalIndex)
              sumsol_NOSD(i)     = sumsol(OriginalIndex)

              nslice_NOSD(i)  = nslice(OriginalIndex)

            end do

            !    This is UNSHADED pass - no solar radiation:
            hin_NOSD=hint
            hout_NOSD=houtt

            !Simon: Removed unshaded debug output for now
            UnshadedDebug = 0
            if (WriteDebugOutput.and.(UnshadedDebug.eq.1)) then
              FileMode = 'APPEND'
              ! InArgumentsFile should already be open
              !open(unit=InArgumentsFile,  file=TRIM(DBGD)//DebugOutputFileName,  status='unknown', access=FileMode,  &
              !  &    form='formatted', iostat=nperr)

              !if (nperr.ne.0)  open(unit=InArgumentsFile,  file=DebugOutputFileName,  status='unknown', access=FileMode, &
              !  &   form='formatted', iostat=nperr)
              write(InArgumentsFile, *)
              write(InArgumentsFile, *) 'UNSHADED RUN:'
              write(InArgumentsFile, *)
              !close(InArgumentsFile)

              call WriteInputArguments(tout, tind, trmin,  wso, iwd, wsi, dir, outir, isky, tsky, esky, fclr, &
                                        VacuumPressure, VacuumMaxGapThickness, ibc, hout_NOSD, hin_NOSD,  &
                                        ISO15099, ThermalMod, SDScalar, height, heightt, width, tilt, totsol,  &
                                        nlayer_NOSD, LayerType_NOSD, thick_NOSD, scon_NOSD, asol, tir_NOSD, emis_NOSD,  &
                                        Atop_NOSD, Abot_NOSD, Al_NOSD, Ar_NOSD, Ah_NOSD,  &
                                        SlatThick_NOSD, SlatWidth_NOSD, SlatAngle_NOSD,  &
                                        SlatCond_NOSD, SlatSpacing_NOSD, SlatCurve_NOSD,  &
                                        nslice_NOSD, LaminateA_NOSD, LaminateB_NOSD, sumsol_NOSD,  &
                                        gap_NOSD, vvent_NOSD, tvent_NOSD,  &
                                        presure_NOSD, nmix_NOSD, iprop_NOSD, frct_NOSD,  &
                                        xgcon, xgvis, xgcp, xwght)

            end if ! end if UnshadedDebug = 1

        !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        !
        !      This is "Unshaded, No solar radiation" pass
        !
        !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            ! call therm1d to calculate heat flux with solar radiation
            call therm1d(nlayer_NOSD, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, &
                          0.0d0, Ebsky, Gout, Trmout, Trmin, Ebroom, Gin, tir_NOSD, rir_NOSD, emis_NOSD, gap_NOSD, thick_NOSD, &
                          scon_NOSD, tilt, sol0, height, heightt, width, iprop_NOSD, frct_NOSD, presure_NOSD, nmix_NOSD, &
                          xwght, xgcon, xgvis, xgcp, gama, SupportPillar, PillarSpacing, PillarRadius, &
                          theta_NOSD, q_NOSD, qv_NOSD, flux_NOSD,  &
                          hcin_NOSD, hrin_NOSD, hcout_NOSD, hrout_NOSD, hin_NOSD, hout_NOSD,  &
                          hcgas_NOSD, hrgas_NOSD, ufactor_NOSD, nperr, ErrorMessage, tamb_NOSD, troom_NOSD, ibc,  &
                          Atop_NOSD, Abot_NOSD, Al_NOSD, Ar_NOSD, Ah_NOSD, vvent_NOSD, tvent_NOSD,  &
                          LayerType_NOSD, Ra_NOSD, Nu_NOSD, vfreevent_NOSD, qcgas_NOSD, qrgas_NOSD,  &
                          Ebf_NOSD, Ebb_NOSD, Rf_NOSD, Rb_NOSD, &
                          ShadeEmisRatioOut_NOSD, ShadeEmisRatioIn_NOSD, ShadeHcModifiedOut_NOSD, ShadeHcModifiedIn_NOSD,  &
                          ThermalMod, Debug_mode, AchievedErrorTolerance_NOSD, NumOfIter_NOSD)

            NumOfIterations = NumOfIter_NOSD
            ! exit on error
            if (.not.(GoAhead(nperr))) return

            !bi...  Keep these values:
            HcUnshadedOut = hcout_NOSD
            HcUnshadedIn  = hcin_NOSD

            ShadeHcRatioOut = ShadeHcModifiedOut / HcUnshadedOut
            ShadeHcRatioIn  = ShadeHcModifiedIn  / HcUnshadedIn


            !bi...unshaded results:
            if (WriteDebugOutput.and.(UnshadedDebug.eq.1)) then
              call WriteOutputArguments(OutArgumentsFile, DBGD, nlayer_NOSD, tamb, q_NOSD, qv_NOSD, qcgas_NOSD,  &
                                        qrgas_NOSD, theta_NOSD, vfreevent_NOSD, vvent_NOSD, Keff_NOSD, ShadeGapKeffConv_NOSD, &
                                        troom_NOSD, ufactor_NOSD, shgc_NOSD, sc_NOSD, hflux_NOSD, shgct_NOSD,  &
                                        hcin_NOSD, hrin_NOSD, hcout_NOSD, hrout_NOSD,   &
                                        Ra_NOSD, Nu_NOSD, LayerType_NOSD, &
                                        Ebf_NOSD, Ebb_NOSD, Rf_NOSD, Rb_NOSD, Ebsky, Gout, Ebroom, Gin,  &
                                        ShadeEmisRatioIn_NOSD, ShadeEmisRatioOut_NOSD,  &
                                        ShadeHcRatioIn_NOSD, ShadeHcRatioOut_NOSD,  &
                                        hcin_NOSD, hcout_NOSD, hcgas_NOSD, hrgas_NOSD, AchievedErrorTolerance_NOSD, NumOfIter_NOSD)
            end if ! end if UnshadedDebug = 1
          end if ! end if NeedUnshadedRun...


          !bi Set T6-related quantities keff, keffc: (using non-solar pass results)
          if (nlayer.gt.1) then
            do i=1, nlayer-1
              Keff(i)   = gap(i)   * q(2*i+1) / (theta(2*i+1) - theta(2*i))
              if (IsShadingLayer(LayerType(i))) then
                Keff(i)   = gap(i)   * q(2*i+1) / (theta(2*i+1) - theta(2*i))
              end if
              if (IsShadingLayer(LayerType(i+1))) then
                Keff(i)   = gap(i)   * q(2*i+1) / (theta(2*i+1) - theta(2*i))
              end if
              if (IsShadingLayer(LayerType(i))) then
                !Keff(i)   = gap(i)   * qprim(2*i+1) / (theta(2*i+1) - theta(2*i))
                if ((i.gt.1).and.(i.lt.nlayer)) then
                  tgg = gap(i-1) + gap(i) + thick(i)
                  qc1 = qcgas(i-1)
                  qc2 = qcgas(i)
                  qcgg = (qc1 + qc2) / 2.0d0
                  ShadeGapKeffConv(i) = tgg * qcgg / (theta(2*i+1) - theta(2*i-2))
                end if
              end if
            end do
          end if

    end if !if (UFactorCalc.ne.0) then

    !bi...  For debugging purposes:
    qeff = ufactor * ABS(tout - tind)
    flux_nonsolar = flux

    if ((SHGCCalc.gt.0).and.(dir.gt.0.0d0)) then
      shgc=totsol-(fluxs-flux)/dir
      sc=shgc/0.87d0
      hcin=hcins
      hrin=hrins
      hin=hins
      hcout=hcouts
      hrout=hrouts
      hout=houts
      flux=fluxs ! <--- ???
      do i=1,nlayer
        theta(2*i-1)=thetas(2*i-1)
        theta(2*i)=thetas(2*i)
        Ebb(i) = Ebbs(i)
        Ebf(i) = Ebfs(i)
        Rb(i) = Rbs(i)
        Rf(i) = Rfs(i)
        q(2*i - 1) = qs(2*i - 1)
        q(2*i) = qs(2*i)
        !qprim(2*i - 1) = qprims(2*i - 1)
        !qprim(2*i) = qprims(2*i)
        qv(2*i - 1) = qvs(2*i - 1)
        qv(2*i) = qvs(2*i)
        hcgas(i)=hcgass(i)
        hrgas(i)=hrgass(i)
        qcgas(i) = qcgaps(i)
        qrgas(i) = qrgaps(i)
        AchievedErrorTolerance = AchievedErrorToleranceSolar
        NumOfIter = NumOfIterSolar
      end do

      ! bi    CHECK THIS!
       q(2*nlayer+1) = qs(2*nlayer+1)
    end if

    hflux = flux        ! save flux value for output table

    !bi...  Write results to debug output file:
    if (WriteDebugOutput) then
         call WriteOutputArguments(OutArgumentsFile, DBGD, nlayer, tamb, q, qv, qcgas, qrgas, theta, vfreevent, vvent,  &
                                 &   Keff, ShadeGapKeffConv, troom, ufactor, shgc, sc, hflux, shgct,  &
                                 &   hcin, hrin, hcout, hrout, Ra, Nu, LayerType,  &
                                 &   Ebf, Ebb, Rf, Rb, Ebsky, Gout, Ebroom, Gin, &
                                 &   ShadeEmisRatioIn, ShadeEmisRatioOut, ShadeHcRatioIn, ShadeHcRatioOut,  &
                                 &   HcUnshadedIn, HcUnshadedOut, hcgas, hrgas, AchievedErrorTolerance, NumOfIter)
      end if  ! if WriteDebugOutput.eq.true - writing output file

  end subroutine Calc_ISO15099

  subroutine therm1d(nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, &
                      dir, Ebsky, Gout, Trmout, Trmin, Ebroom, Gin, tir, rir, emis,  &
                      gap, thick, scon, tilt, asol, height, heightt, width, &
                      iprop, frct, presure, nmix, wght, gcon, gvis, gcp, gama,  &
                      SupportPillar, PillarSpacing, PillarRadius, &
                      theta, q, qv, flux, hcin, hrin, hcout, hrout, hin, hout, hcgas, hrgas, ufactor, nperr, ErrorMessage,  &
                      tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType,  &
                      Ra, Nu, vfreevent, qcgas, qrgas, Ebf, Ebb, Rf, Rb, &
                      ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcModifiedOut, ShadeHcModifiedIn,  &
                      ThermalMod, Debug_mode, AchievedErrorTolerance, TotalIndex)
    !********************************************************************************
    ! Main subroutine for calculation of 1-D heat transfer in the center of glazing.
    !********************************************************************************
    ! Inputs
    !
    !   nlayer    number of solid layers
    !   iwd   wind direction
    !   tout  outside temp in k
    !   tind  inside temp in k
    !   wso   wind speed in m/s
    !   wsi   inside forced air speed m/s
    !   Ebsky     ir flux from outside
    !   Gout  back facing radiosity from outside
    !   Trmout    Mean outdoor radiant temperature
    !   Trmin     Mean indoor radiant temperature
    !   Ebroom    ir flux from room
    !   Gin   front facing radiosity from room
    !   tir   ir transmittance of each layer
    !   rir   ir reflectance of each surface
    !   emis  ir emittances of each surface
    !   gap   array of gap widths in meters
    !   thick     thickness of glazing layers (m)
    !   scon  Vector of conductivities of 'glazing' layers
    !   tilt  Window tilt (deg). vert: tilt=90, hor out up: tilt=0, hor out down: tilt=180
    !   sol   absorbed solar energy for each layer in w/m2
    !   height    glazing cavity height
    !   heightt
    !   iprop
    !   frct
    !   presure
    !   nmix  vector of number of gasses in a mixture for each gap
    !   hin  convective indoor film coefficient (if non-zero hin input)
    !   hout     convective outdoor film coeff. (if non-zero hout input)
    !
    ! outputs
    !
    !   theta     temp distribution in k
    !   flux  net heat flux between room and window
    !   rtot  overall thermal resistance
    !   rs    ?
    !   hcin  convective indoor film coeff.
    !   hrin  radiative part of indoor film coeff.
    !   hcout     convective outdoor film coeff.
    !   hrout     radiative part of outdoor film coeff.
    !   hin   convective indoor film coefficient
    !   hout  convective outdoor film coeff.
    !   ufactor   overall u-factor
    !   qcgap     vector of convective/conductive parts of flux in gaps
    !   qrgap     vector of radiative parts of flux in gaps
    !   nperr
    ! *Inactives**
    !   wa - window azimuth (degrees, clockwise from south)
    !   hgas  matrix of gap film coefficients
    ! Locals
    !   Ebb   Vector
    !   Ebf   Vector
    !   Rb    Vector
    !   Rf    Vector
    !   a     Array
    !   b     Array
    !   hhat  Vector
    !   err   iteration tolerance
    !   dtmax     max temp dfference after iteration
    !   index     iteration step
    integer, intent(in) :: nlayer, iwd, ThermalMod
    integer, intent(in) :: Debug_mode ! Switch for debug output files:
                                        !    0 - don't create debug output files
                                        !    1 - append results to existing debug output file
                                        !    2 - store results in new debug output file
                                        !   3 - save in-between results (in all iterations) to existing debug file
    integer, dimension(maxlay), intent(in) :: LayerType
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    integer, dimension(2), intent(in) :: ibc
    real(r64), intent(in) :: dir, wso, wsi, VacuumPressure, VacuumMaxGapThickness, Gout, Trmout, Trmin, Gin
    real(r64), intent(inout) :: tout, tind
    integer, intent(out) :: TotalIndex
    real(r64), dimension(MaxGap), intent(in) :: gap
    real(r64), dimension(maxlay), intent(in) :: thick
    real(r64), dimension(maxlay2), intent(in) :: rir, emis, tir
    real(r64), dimension(maxlay), intent(in) :: asol, scon
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: presure
    real(r64), dimension(maxgas), intent(in) :: wght, gama
    real(r64), dimension(maxgas, 3), intent(in) :: gcon, gvis, gcp
    real(r64), intent(in) :: tilt, height, heightt
    real(r64), dimension(maxlay), intent(in) :: Atop, Abot, Al, Ar, Ah
    real(r64), dimension(maxlay1), intent(in) :: vvent, tvent
    integer, dimension(maxlay), intent(in) :: SupportPillar
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing, PillarRadius

    real(r64), intent(inout) :: Ebroom, Ebsky

    ! real(r64), intent(in) :: sumsol(maxlay)
    !integer, intent(in) :: nslice(maxlay)

    real(r64), intent(out) :: flux
    real(r64), dimension(maxlay3), intent(out) :: q
    real(r64), dimension(maxlay1), intent(out) :: qv, qcgas, qrgas
    real(r64), dimension(maxlay), intent(out) :: Ra, Nu
    real(r64), dimension(maxlay1), intent(out) :: vfreevent
    real(r64), intent(out) :: ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcModifiedOut, ShadeHcModifiedIn
    real(r64), intent(out) :: ufactor, hcin
    real(r64), dimension(maxlay1), intent(out) :: hcgas, hrgas
    real(r64), intent(inout) :: Tamb, Troom, hin, hout, hcout
    real(r64), dimension(maxlay2), intent(inout) :: theta
    real(r64), intent(inout) :: hrin, hrout
    real(r64), dimension(maxlay), intent(inout) :: Ebb, Ebf, Rb, Rf
    real(r64), intent(out) :: AchievedErrorTolerance
    integer, intent(inout) :: nperr
    character(len=*) :: ErrorMessage

    real(r64) :: width, glsyswidth
    !real(r64) :: Ebbold(maxlay), Ebfold(maxlay), Rbold(maxlay), Rfold(maxlay)
    !real(r64) :: rs(maxlay3)
    real(r64), dimension(4*nlayer, 4*nlayer) :: a
    real(r64), dimension(4*nlayer) :: b
    real(r64), dimension(maxlay1) :: hgas
    !real(r64) :: hhatv(maxlay3),hcv(maxlay3), Ebgap(maxlay3), Tgap(maxlay1)
    real(r64), dimension(maxlay1) :: Tgap

    !real(r64) ::  alpha
    integer ::   maxiter

    real(r64) ::  qr_gap_out, qr_gap_in

    real(r64), dimension(2*nlayer) :: told

    ! Simon: parameters used in case of JCFN iteration method
    !real(r64) :: Dvector(maxlay4) ! store diagonal matrix used in JCFN iterations
    real(r64), allocatable :: FRes(:) ! store function results from current iteration
    real(r64), allocatable :: FResOld(:) ! store function results from previous iteration
    real(r64), allocatable :: FResDiff(:) ! save difference in results between iterations
    real(r64), allocatable :: Radiation(:) ! radiation on layer surfaces.  used as temporary storage during iterations

    real(r64), allocatable :: x(:) ! temporary vector for storing results (theta and Radiation).  used for easier handling
    real(r64), allocatable :: dX(:) ! difference in results
    real(r64), allocatable :: Jacobian(:,:) ! diagonal vector for jacobian comuptation-free newton method
    real(r64), allocatable :: DRes(:) ! used in jacobian forward-difference approximation

    ! This is used to store matrix before equation solver.  It is important because solver destroys
    ! content of matrices
    real(r64), allocatable :: LeftHandSide(:,:)
    real(r64), allocatable :: RightHandSide(:)

    ! Simon: Keep best achieved convergence
    real(r64) :: prevDifference, Relaxation
    real(r64), allocatable :: RadiationSave(:)
    real(r64), allocatable :: thetaSave(:)
    integer :: currentTry

    integer ::   LayerTypeSpec(maxlay)
    integer ::   SDLayerIndex

    integer ::   CSMFlag
    integer :: i, j, k
    real(r64) :: curDifference
    integer :: index
    integer :: curTempCorrection

    real(r64) :: qc_gap_out, qcgapout2, hc_modified_out, qc_gap_in, hc_modified_in

    integer :: CalcOutcome

    logical :: iterationsFinished ! To mark whether or not iterations are finished
    logical :: saveIterationResults
    logical :: updateGapTemperature
    !logical :: TurnOnNewton

    SDLayerIndex = -1

    ! Allocate arrays
    if (.not. allocated(FRes)) allocate(FRes(1:4*nlayer))
    if (.not. allocated(FResOld)) allocate(FResOld(1:4*nlayer))
    if (.not. allocated(FResDiff)) allocate(FResDiff(1:4*nlayer))
    if (.not. allocated(Radiation)) allocate(Radiation(1:2*nlayer))
    if (.not. allocated(RadiationSave)) allocate(RadiationSave(1:2*nlayer))
    if (.not. allocated(thetaSave)) allocate(thetaSave(1:2*nlayer))
    if (.not. allocated(X)) allocate(X(1:4*nlayer))
    if (.not. allocated(dX)) allocate(dX(1:4*nlayer))
    if (.not. allocated(Jacobian)) allocate(Jacobian(1:4*nlayer, 1:4*nlayer))
    if (.not. allocated(DRes)) allocate(DRes(1:4*nlayer))

    if (.not. allocated(LeftHandSide)) allocate(LeftHandSide(1:4*nlayer, 1:4*nlayer))
    if (.not. allocated(RightHandSide)) allocate(RightHandSide(1:4*nlayer))

    dX = 0.0d0

    ! Simon: This is set to zero until it is resolved what to do with modifier
    ShadeHcModifiedOut = 0.0d0
    !BuffIndex = 0
    CSMFlag = 0
    CalcOutcome = CALC_UNKNOWN
    curTempCorrection = 0
    AchievedErrorTolerance = 0.0d0
    curDifference = 0.0d0
    !TurnOnNewton = .true.
    currentTry = 0
    index = 0
    TotalIndex = 0
    iterationsFinished = .false.
    qv=0.0d0
    Ebb = 0.0d0
    Ebf = 0.0d0
    Rb = 0.0d0
    Rf = 0.0d0
    a = 0.0d0
    b = 0.0d0

    !Dvector = 0.0
    FRes = 0.0d0
    FResOld = 0.0d0
    FResDiff = 0.0d0
    Radiation = 0.0d0
    Relaxation = RelaxationStart
    !alpha = PicardRelaxation

    maxiter = NumOfIterations

    !call MarkVentilatedGaps(nlayer, isVentilated, LayerType, vvent)

    if (Debug_mode == saveIntermediateResults) then
      saveIterationResults = .true.
    else
      saveIterationResults = .false.
    end if

    !call guess(tout, tind, nlayer, gap, thick, glsyswidth, theta, Ebb, Ebf, Tgap)

    do i=1, nlayer
      k = 2*i
      Radiation(k) = Ebb(i)
      Radiation(k-1) = Ebf(i)
      told(k-1) = 0.0d0
      told(k) = 0.0d0
      !told(k-1) = theta(k-1)
      !told(k) = theta(k)
    end do

    !bi...Set LayerTypeSpec array - need to treat venetians AND woven shades as glass:
    if (ThermalMod == THERM_MOD_CSM) then
      do i = 1, nlayer
        if (IsShadingLayer(LayerType(i))) then
          LayerTypeSpec(i) = 0
          SDLayerIndex = i
        else
          LayerTypeSpec(i) = LayerType(i)
        end if
      end do
    end if

    !first store results before iterations begin
    if (saveIterationResults) then
      call storeIterationResults(nlayer, index, theta, Trmout, Tamb, Trmin, Troom, Ebsky, Ebroom, &
            hcin, hcout, hrin, hrout, hin, hout, Ebb, Ebf, Rb, Rf, nperr)
    end if

    Tgap(1) = tout
    Tgap(nlayer+1) = tind
    do i =2, nlayer
      Tgap(i) = (theta(2*i-1) + theta(2*i-2) ) / 2
    end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!! MAIN ITERATION LOOP
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do while (.not.(iterationsFinished))

      do i=1, 2*nlayer
        if (theta(i).lt.0) then
          theta(i) = 1.0d0 * i
        end if
      end do

      !do i=1,nlayer+1
      !  if (i == 1) then
      !    Tgap(i) = tout
      !  else if (i == nlayer+1) then
      !    Tgap(i) = tind
      !  else
      !    Tgap(i) = (theta(2*i-2) + theta(2*i-1)) / 2.0d0
      !  end if
      !end do

      ! skip updating gap temperatures for shading devices. Gap temperature in that case is not simply average
      ! between two layer temperatures
      do i =2, nlayer
        updateGapTemperature = .false.
        if ((.not.(IsShadingLayer(LayerType(i-1)))).and.(.not.(IsShadingLayer(LayerType(i))))) then
          updateGapTemperature = .true.
        end if
        if (updateGapTemperature) then
          Tgap(i) = (theta(2*i-1) + theta(2*i-2) ) / 2
        end if
      end do

      ! evaluate convective/conductive components of gap
      call hatter(nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, Ebsky, Tamb, Ebroom, Troom, &
                    gap, height, heightt, scon, tilt, theta, Tgap, Radiation, Trmout, Trmin,  &
                    iprop, frct, presure, nmix, wght, gcon, gvis, gcp, gama, &
                    SupportPillar, PillarSpacing, PillarRadius, &
                    hgas, hcgas, hrgas, hcin, hcout, hin, hout,  &
                    index, ibc, nperr, ErrorMessage, hrin, hrout, Ra, Nu)

      ! exit on error
      if (.not.(GoAhead(nperr))) return

      !bi...Override hhat values near SHADING DEVICE layer(s), but only for CSM thermal model:
      if ((ThermalMod.eq.THERM_MOD_CSM).and.(SDLayerIndex.gt.0)) then
        ! adjust hhat values
        !call adjusthhat(SDLayerIndex, ibc, tout, tind, nlayer, theta, wso, wsi, iwd, height, heightt, tilt,  &
        !               &  thick, gap, hout, hrout, hin, hrin, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, &
        !               index, SDScalar, Ebf, Ebb, hgas, hhat, nperr, ErrorMessage)
        !do i = 1, maxlay3
          !hhatv(i) = 0.0
          !Ebgap(i) = 0.0
          !qv(i)    = 0.0
          !hcv(i)   = 0.0
        !end do
        call matrixQBalance(nlayer, a, b, scon, thick, hcgas, hcout, hcin, asol, qv, &
            Tind, Tout, Gin, Gout, theta, tir, rir, emis)
      else
        !bi...There are no Venetian layers, or ThermalMod is not CSM, so carry on as usual:
        call shading(theta, gap, hgas, hcgas, hrgas, frct, iprop, presure, nmix, wght, gcon, gvis, gcp, nlayer, &
          width, height, tilt, tout, tind, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Tgap, qv, nperr, &
          ErrorMessage, vfreevent)

        ! exit on error
        if (.not.(GoAhead(nperr))) return

        call matrixQBalance(nlayer, a, b, scon, thick, hcgas, hcout, hcin, asol, qv, &
            Tind, Tout, Gin, Gout, theta, tir, rir, emis)

      end if !  end if

      FResOld = FRes

      ! Pack results in one array
      do i = 1, nlayer
        k=4*i-3
        j=2*i-1

        x(k) = theta(j)
        x(k+1) = theta(j+1)
        x(k+2) = Radiation(j)
        x(k+3) = Radiation(j+1)
      end do

      call CalculateFuncResults(nlayer, a, b, x, FRes)

      FResDiff = FRes - FResOld

      !if (TurnOnNewton) then
      !  do i = 1, 4*nlayer
      !    temp = x(i)
      !    h = ConvergenceTolerance * abs(x(i))
      !    if (h == 0) then
      !      h = ConvergenceTolerance
      !    end if
      !    x(i) = temp + h
      !    h = x(i) - temp ! trick to reduce finite precision error
      !    call CalculateFuncResults(nlayer, a, b, x, DRes, nperr, ErrorMessage)
      !    do j = 1, 4*nlayer
      !      Jacobian(j,i) = (DRes(j) - FRes(j)) / h
      !    end do
      !    x(i) = temp
      !  end do
      !end if

      !if (TurnOnNewton) then
      !  LeftHandSide = Jacobian
      !  RightHandSide = -FRes
      !else
        LeftHandSide = a
        RightHandSide = b
      !end if
      call EquationsSolver(LeftHandSide, RightHandSide, 4*nlayer, nperr, ErrorMessage)

      !if (TurnOnNewton) then
      !  dx = RightHandSide
      !end if

      ! Simon: This is much better, but also much slower convergence criteria.  Think of how to make this flexible and allow
      ! user to change this from outside (through argument passing)
      !curDifference = abs(FRes(1))
      !do i = 2, 4*nlayer
        !curDifference = max(curDifference, abs(FRes(i)))
        !curDifference = curDifference + abs(FRes(i))
      !end do

      curDifference = abs(theta(1) - told(1))
      !curDifference = abs(FRes(1))
      do i = 2, 2*nlayer
      !do i = 2, 4*nlayer
        curDifference = max(curDifference, abs(theta(i) - told(i)))
        !curDifference = MAX(abs(FRes(i)), curDifference)
      end do

      do i=1, nlayer
        k=4*i-3
        j=2*i-1
        !if (TurnOnNewton) then
        !  theta(j) = theta(j) + Relaxation*dx(k)
        !  theta(j+1) = theta(j+1) + Relaxation*dx(k+1)
        !  Radiation(j) = Radiation(j) + Relaxation*dx(k+2)
        !  Radiation(j+1) = Radiation(j+1) + Relaxation*dx(k+3)
        !else
        !  dX(k) = RightHandSide(k) - theta(j)
        !  dX(k+1) = RightHandSide(k + 1) - theta(j+1)
        !  dX(k+2) = RightHandSide(k + 2) - Radiation(j)
        !  dX(k+3) = RightHandSide(k + 3) - Radiation(j+1)
        told(j) = theta(j)
        told(j+1) = theta(j+1)
        theta(j) = (1 - Relaxation) * theta(j) + Relaxation * RightHandSide(k)
        theta(j+1) = (1 - Relaxation) * theta(j+1) + Relaxation * RightHandSide(k+1)
        Radiation(j) = (1 - Relaxation) * Radiation(j) + Relaxation * RightHandSide(k+2)
        Radiation(j+1) = (1 - Relaxation) * Radiation(j+1) + Relaxation * RightHandSide(k+3)
        !end if
      end do

      ! it is important not to update gaps around shading layers since that is already calculated by
      ! shading routines
      do i=1, nlayer+1
        updateGapTemperature = .true.
        if ((i.eq.1).or.(i.eq.nlayer+1)) then
          ! update gap array with interior and exterior temperature
          updateGapTemperature = .true.
        else
          ! update gap temperature only if gap on both sides
          updateGapTemperature = .false.
          if ((.not.(IsShadingLayer(LayerType(i-1)))).and.(.not.(IsShadingLayer(LayerType(i))))) then
            updateGapTemperature = .true.
          end if
        end if
        j = 2*(i-1)
        if (updateGapTemperature) then
          if (i.eq.1) then
            Tgap(1) = tout
          else if (i.eq.(nlayer+1)) then
            Tgap(i) = tind
          else
            Tgap(i) = (theta(j) + theta(j+1) ) / 2
          end if
        end if
      end do

      !and store results during iterations
      if (saveIterationResults) then
        call storeIterationResults(nlayer, index + 1, theta, Trmout, Tamb, Trmin, Troom, Ebsky, Ebroom, &
          hcin, hcout, hrin, hrout, hin, hout, Ebb, Ebf, Rb, Rf, nperr)
      end if

      if (.not.(GoAhead(nperr))) return

      prevDifference = curDifference

      if ((index == 0).or.(curDifference < AchievedErrorTolerance)) then
        AchievedErrorTolerance = curDifference
        currentTry = 0
        do i=1,2*nlayer
          RadiationSave(i) = Radiation(i)
          thetaSave(i) = theta(i)
        end do
      else
        ! This is case when program solution diverged
        currentTry = currentTry + 1
        if (currentTry >= NumOfTries) then
          currentTry = 0
          do i = 1, 2*nlayer
            Radiation(i) = RadiationSave(i)
            theta(i) = thetaSave(i)
          end do
          !if (.not.TurnOnNewton) then
          !  TurnOnNewton = .true.
          !else
            Relaxation = Relaxation - RelaxationDecrease
            TotalIndex = TotalIndex + index
            index = 0
            ! Start from best achieved convergence
            if (Relaxation <= 0.0d0) then ! cannot continue with relaxation equal to zero
              iterationsFinished = .true.
            end if
           ! TurnOnNewton = .true.
          !end if ! if (.not.TurnOnNewton) then
        end if ! f (currentTry == NumOfTries) then
      end if

      ! Chek if results were found:
      if (curDifference < ConvergenceTolerance) then
        CalcOutcome = CALC_OK
        TotalIndex = TotalIndex + index
        iterationsFinished = .true.
      end if

      if (index >= maxiter) then
        Relaxation = Relaxation - RelaxationDecrease
        TotalIndex = TotalIndex + index
        index = 0
        !TurnOnNewton = .true.

        ! Start from best achieved convergence
        do i = 1, 2*nlayer
          Radiation(i) = RadiationSave(i)
          theta(i) = thetaSave(i)
        end do
        if (Relaxation <= 0.0d0) then ! cannot continue with relaxation equal to zero
          iterationsFinished = .true.
        end if
      end if

      index = index + 1
    end do

    ! Get results from closest iteration and store it
    if (CalcOutcome == CALC_OK) then
      do i=1,2*nlayer
        Radiation(i) = RadiationSave(i)
        theta(i) = thetaSave(i)
      end do

      do i =2, nlayer
        updateGapTemperature = .false.
        if ((.not.(IsShadingLayer(LayerType(i-1)))).and.(.not.(IsShadingLayer(LayerType(i))))) then
          updateGapTemperature = .true.
        end if

        if (updateGapTemperature) then
          Tgap(i) = (theta(2*i-1) + theta(2*i-2) ) / 2
        end if
      end do

      ! Simon: It is important to recalculate coefficients from most accurate run
      call hatter(nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, Ebsky, Tamb, Ebroom, Troom, &
                    gap, height, heightt, scon, tilt, theta, Tgap, Radiation, Trmout, Trmin,  &
                    iprop, frct, presure, nmix, wght, gcon, gvis, gcp, gama, &
                    SupportPillar, PillarSpacing, PillarRadius, hgas, hcgas, hrgas, hcin, hcout, hin, hout,  &
                    index, ibc, nperr, ErrorMessage, hrin, hrout, Ra, Nu)

      call shading(theta, gap, hgas, hcgas, hrgas, frct, iprop, presure, nmix, wght, gcon, gvis, gcp, nlayer, &
        width, height, tilt, tout, tind, Atop, Abot, Al, Ar, Ah, vvent, tvent, LayerType, Tgap, qv, nperr, &
        ErrorMessage, vfreevent)
    end if

    if (CalcOutcome.eq.CALC_UNKNOWN) then
        ErrorMessage = 'Tarcog failed to converge'
        nperr = 2  ! error 2: failed to converge...
    end if

    ! Get radiation results first
    !if (curEquationsApproach.eq.eaQBalance) then
    do i=1, nlayer
      k=2*i-1
      Rf(i) = Radiation(k)
      Rb(i) = Radiation(k+1)
      Ebf(i) = StefanBoltzmann*theta(k)**4
      Ebb(i) = StefanBoltzmann*theta(k+1)**4
    end do
    !end if

    ! Finishing calcs:
    call resist(nlayer, Trmout, Tout, Trmin, tind, hcgas, hrgas, Theta, q,  &
             &   qv, LayerType, thick, scon, ufactor, flux,  &
             &   qcgas, qrgas)

    !bi...  Set T6-related quantities - ratios for modified epsilon, hc for modelling external SDs:
    !    (using non-solar pass results)
    if ((dir.eq.0.0d0).and.(nlayer.gt.1)) then

      qr_gap_out = Rf(2)  - Rb(1)
      qr_gap_in  = Rf(nlayer) - Rb(nlayer-1)

      if (IsShadingLayer(LayerType(1))) then
        ShadeEmisRatioOut = qr_gap_out / ( emis(3) * StefanBoltzmann * (theta(3)**4 - Trmout**4) )
        !qc_gap_out = qprim(3) - qr_gap_out
        !qcgapout2 = qcgas(1)
        !Hc_modified_out = (qc_gap_out / (theta(3) - tout))
        !ShadeHcModifiedOut = Hc_modified_out
      end if

      if (IsShadingLayer(LayerType(nlayer))) then
        ShadeEmisRatioIn  = qr_gap_in / ( emis(2*nlayer-2) * StefanBoltzmann * (Trmin**4 - theta(2*nlayer-2)**4) )
        qc_gap_in = q(2*nlayer-1) - qr_gap_in
        Hc_modified_in = (qc_gap_in / (Tind - theta(2*nlayer-2)))
        ShadeHcModifiedIn = Hc_modified_in
      end if
    end if ! IF dir = 0

    if (allocated(FRes)) deallocate(FRes)
    if (allocated(FResOld)) deallocate(FResOld)
    if (allocated(FResDiff)) deallocate(FResDiff)
    if (allocated(Radiation)) deallocate(Radiation)
    if (allocated(RadiationSave)) deallocate(RadiationSave)
    if (allocated(thetaSave)) deallocate(thetaSave)
    if (allocated(X)) deallocate(X)
    if (allocated(dX)) deallocate(dX)
    if (allocated(Jacobian)) deallocate(Jacobian)
    if (allocated(DRes)) deallocate(DRes)

    if (allocated(LeftHandSide)) deallocate(LeftHandSide)
    if (allocated(RightHandSide)) deallocate(RightHandSide)

    !do i=1, nlayer-1
    !  if (((LayerType(i).eq.VENETBLIND)  &
    !      &  .and.(ThermalMod.ne.THERM_MOD_CSM))  &
    !      &  .or.(LayerType(i).eq.WOVSHADE)) then
    !    !hcgas(i+1)=hcv(2*i+1)
    !  else
    !    !hcgas(i+1)=hgas(i+1)
    !  end if
    !end do

    1111  format('Outdoor: ', F9.6,' ;  alt2: ', F9.6, ' ; alt3: ', F9.6, ' ; alt4: ', F9.6)
    1112  format('Indoor:  ', F9.6,' ;  alt2: ', F9.6, ' ; alt3: ', F9.6, ' ; alt4: ', F9.6)

    !110   format(' Theta(',I1,') = ',F12.6)
    !111   format(' T(',I1,')=',F15.9)
    !112  format(' ',A3,' =',F15.9)

  end subroutine therm1d

  subroutine guess(tout, tind, nlayer, gap, thick, width, theta, Ebb, Ebf, Tgap)
    !***********************************************************************
    ! purpose - initializes temperature distribution assuming
    !   a constant temperature gradient across the window
    !***********************************************************************
    ! Input
    !   tout    outdoor air temperature (k)
    !   tind     indoor air temperature (k)
    !   nlayer  number of solid layers in window output
    !   gap     thickness of gas gaps (m)
    !   thick   thickness of glazing layers (m)
    ! Output
    !   width   total width of the glazing system
    !   theta   array of surface temps starting from outdoor layer (k)
    !   Ebb     vector of emissive power (?) of the back surface (# of layers)
    !   Ebf     vector of emissive power (?) of the front surface (# of layers)
    ! Locals
    !   x   Vector of running width
    !   delta   delta T per unit length


    integer, intent(in) :: nlayer
    real(r64), intent(in) :: tout, tind
    real(r64), dimension(MaxGap), intent(in) :: gap
    real(r64), dimension(maxlay), intent(in) :: thick

    real(r64), intent(out) :: width
    real(r64), dimension(maxlay), intent(out) :: Ebb, Ebf
    real(r64), dimension(maxlay1), intent(out) :: Tgap
    real(r64), dimension(maxlay2), intent(out) :: theta

    real(r64), dimension(maxlay2) :: x
    real(r64) :: delta
    integer :: i, j, k

    x(1) = 0.001d0
    x(2) = x(1)+thick(1)

    do i = 2, nlayer
      j = 2*i - 1
      k = 2*i
      x(j) = x(j-1) + gap(i-1)
      x(k) = x(k-1) + thick(i)
    end do

    width = x(nlayer*2)+0.01d0
    delta = (tind-tout)/width

    if (delta.eq.0.0d0) then
      delta = TemperatureQuessDiff/width
    end if

    do i=1, nlayer
      j = 2*i
      theta(j-1) = tout + x(j-1)*delta
      theta(j) = tout + x(j)*delta
      Ebf(i) = StefanBoltzmann * theta(j-1)**4
      Ebb(i) = StefanBoltzmann * theta(j)**4
    end do

    do i =1, nlayer + 1
      if (i.eq.1) then
        Tgap(1) = tout
      else if (i.eq.(nlayer + 1)) then
        Tgap(nlayer + 1) = tind
      else
        Tgap(i) = (theta(2*i-1) + theta(2*i-2) ) / 2
      end if
    end do

  end subroutine guess

  subroutine TemperaturesFromEnergy(theta, Tgap, Ebf, Ebb, nlayer, nperr, ErrorMessage)
    !***********************************************************************
    ! this subroutine computes the new temperature distribution
    !***********************************************************************


    integer, intent(in) :: nlayer
    real(r64), dimension(maxlay), intent(in) :: Ebf, Ebb

    real(r64), dimension(maxlay2), intent(inout) :: theta
    real(r64), dimension(maxlay1), intent(inout) :: Tgap

    !real(r64), intent(out) :: dtmax
    !integer, intent(out) :: MaxIndex

    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64), dimension(maxlay2) :: told
    integer ::   i, j


    !dtmax = 0.0
    !MaxIndex = 0

    ! first check for energy values. They cannot be negative because power to 0.25
    ! will crash application
    do i = 1, nlayer
      if ((Ebf(i).lt.0).and.(Ebb(i).lt.0)) then
        nperr = 2 !this is flag for convergence error
        ErrorMessage = 'Tarcog failed to converge.'
        return ! stop execution
      end if
    end do

    do i=1, nlayer
      j = 2*i
      told(j)    = theta(j)
      told(j-1)  = theta(j-1)
      theta(j-1) = (Ebf(i)/StefanBoltzmann)**0.25d0
      theta(j)   = (Ebb(i)/StefanBoltzmann)**0.25d0
      if (i.ne.1) then
        Tgap(i) = (theta(j-1) + theta(j-2)) / 2
      end if
    end do

  end subroutine TemperaturesFromEnergy

  subroutine solarISO15099(totsol, rtot, rs, nlayer, absol, sf)
    !***********************************************************************
    !   This subroutine calculates the shading coefficient for a window.
    !***********************************************************************
    !  Inputs:
    !    absol     array of absorped fraction of solar radiation in lites
    !    totsol    total solar transmittance
    !    rtot  total thermal resistance of window
    !    rs    array of thermal resistances of each gap and layer
    !    layer     number of layers
    !     dir  direct solar radiation
    !  Outputs:
    !    sf    solar gain of space

    real(r64), intent(in) :: totsol, rtot
    real(r64), dimension(maxlay), intent(in) :: absol
    real(r64), dimension(maxlay3), intent(in) :: rs
    integer, intent(in) :: nlayer
    real(r64), intent(out) :: sf

    real(r64) :: flowin, fract
    integer :: i, j

    fract = 0.0d0
    flowin = 0.0d0
    sf = 0.0d0

    if (rtot == 0.0d0) then
      return
    end if

    ! evaluate inward flowing fraction of absorbed radiation:
    flowin = (rs(1) + 0.5d0 * rs(2)) / rtot
    fract = absol(1) * flowin

    do i=2, nlayer
      j = 2*i
      flowin = flowin + (0.5d0 * (rs(j-2) + rs(j)) + rs(j-1)) / rtot
      fract = fract + absol(i) * flowin
    end do
    sf = totsol+fract   ! add inward fraction to directly transmitted fraction

  end subroutine solarISO15099

  subroutine resist(nlayer, Trmout, Tout, Trmin, tind, hcgas, hrgas, Theta,  &
                      &  qlayer, qv, LayerType, thick, scon,  &
                      &  ufactor, flux, qcgas, qrgas)
    !***********************************************************************
    ! subroutine to calculate total thermal resistance of the glazing system
    !***********************************************************************

    integer, intent(in) :: nlayer, LayerType(maxlay)
    real(r64), intent(in) :: Trmout, Tout, Trmin, tind
    real(r64), dimension(maxlay), intent(in) :: thick, scon
    real(r64), dimension(maxlay2), intent(inout) :: theta
    real(r64), dimension(maxlay1), intent(in) :: qv

    real(r64), intent(out) :: ufactor, flux
    real(r64), dimension(maxlay1), intent(in)  :: hcgas, hrgas
    real(r64), dimension(maxlay1), intent(out) :: qcgas, qrgas
    real(r64), dimension(maxlay3), intent(out) :: qlayer

    integer :: i

    !R_tot = 0.0

    ! Simon: calculation of heat flow through gaps and layers as well as ventilation speed and heat flow
    ! are kept just for reporting purposes.  U-factor calculation is performed by calculating heat flow transfer
    ! at indoor layer

    !calculate heat flow for external and internal environments and gaps
    do i = 1, nlayer+1
      if (i.eq.1) then
        qcgas(i) = hcgas(i)*(Theta(2*i-1) - Tout)
        qrgas(i) = hrgas(i)*(Theta(2*i-1) - Trmout)
        qlayer(2*i-1) = qcgas(i) + qrgas(i)
    !    rs(2*i-1) = 1/hgas(i)
      else if (i.eq.(nlayer+1)) then
        qcgas(i) = hcgas(i)*(Tind - Theta(2*i-2))
        qrgas(i) = hrgas(i)*(Trmin - Theta(2*i-2))
        qlayer(2*i-1) = qcgas(i) + qrgas(i)
    !    rs(2*i-1) = 1/hgas(i)
      else
        qcgas(i) = hcgas(i)*(Theta(2*i-1) - Theta(2*i-2))
        qrgas(i) = hrgas(i)*(Theta(2*i-1) - Theta(2*i-2))
        qlayer(2*i-1) = qcgas(i) + qrgas(i)
    !    rs(2*i-1) = 1/hgas(i)
      end if
    end do

    !.....Calculate thermal resistances for glazing layers:
    do i = 1, nlayer
    !  rs(2*i) = thick(i)/scon(i)
      qlayer(2*i) = scon(i) / thick(i) * (Theta(2*i)-Theta(2*i-1))
    end do

    !R_tot = 0.0

    !do i = 1, 2*nlayer+1
    !  R_tot = R_tot + rs(i)
    !end do

    ! U factor:
    !ufactor = 1./R_tot

    flux = qlayer(2*nlayer+1)
    if (IsShadingLayer(LayerType(nlayer))) then
      flux = flux + qv(nlayer)
    end if

    ufactor = 0.0d0
    if (Tind /= Tout) then
      ufactor = flux / (Tind - Tout)
    end if

  end subroutine

  subroutine hatter(nlayer, iwd, tout, tind, wso, wsi, VacuumPressure, VacuumMaxGapThickness, Ebsky, tamb, Ebroom, troom, &
                      gap, height, heightt, scon, tilt, theta, Tgap, Radiation, Trmout, Trmin, &
                      iprop, frct, presure, nmix, wght, gcon, gvis, gcp, gama, SupportPillar, PillarSpacing, PillarRadius, &
                      hgas, hcgas, hrgas, hcin, hcout, hin, hout, index, ibc, nperr, ErrorMessage, hrin, hrout, Ra, Nu )
    !***********************************************************************
    !  This subroutine calculates the array of conductances/film coefficients used to model convection.  The conductances/film
    !  coefficients are calculated as functions of temperature defined with the usual variable h and THEN are converted into an
    !  equivalent value interms of the black body emittance based on the surface
    !***********************************************************************
    ! Inputs
    !   nlayer   number of solid layers
    !   iwd  wind direction
    !   tout     outside temp in k
    !   tind  inside temp in k
    !   wso  wind speed in m/s
    !   wsi  inside forced air speed m/s
    !   Ebsky    ir flux from outside
    !   Ebroom   ir flux from room
    !   Gout     radiosity (ir flux) of the combined environment (sky+ground)
    !   Gin
    !   gap  vector of gap widths in meters
    !   height   IGU cavity height
    !   heightt
    !   thick    glazing layer thickness
    !   scon   Vector of conductivities of each glazing layer
    !   tilt   Window tilt (in degrees)
    !   theta  Vector of average temperatures
    !   Ebb
    !   Ebf
    !   iprop    array of gap mixtures
    !   frct     vector of mixture fractions
    !   presure
    !   hin   Indoor Indoor combined film coefficient (if non-zero)
    !   hout  Outdoor combined film coefficient (if non-zero)
    !   nmix  vector of number of gasses in a mixture for each gap
    ! Ouputs
    !   hhat     vector of all film coefficients (maxlay3)
    !   hgas     vector of gap 'film' coeff.
    !   hcin  Indoor convective surface heat transfer coefficient
    !   hcout     Outdoor convective heat transfer coeff
    !   hrin    Indoor radiative surface heat transfer coefficient
    !   hrout   Outdoor radiative surface heat transfer coefficient
    !   hin   Indoor combined film coefficient
    !   hout  Outdoor combined film coefficient
    !   index    iteration step
    !   ibc
    !
    ! Inactives**
    !   wa - window azimuth (degrees, clockwise from south)
    !

    integer, intent(in) :: nlayer, index, iwd
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    integer, dimension(2), intent(in) :: ibc
    real(r64), dimension(maxgas), intent(in) :: wght, gama
    real(r64), dimension(maxgas, 3), intent(in) :: gcon, gvis, gcp
    real(r64), intent(in) :: tout, tind, wso, wsi, height, heightt, tilt, Trmout, Trmin, VacuumPressure, VacuumMaxGapThickness
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxlay), intent(in) :: scon
    real(r64), dimension(maxlay1), intent(in) :: presure
    real(r64), dimension(maxlay2), intent(inout) :: theta, Radiation
    real(r64), dimension(maxlay1), intent(in) :: Tgap
    real(r64), intent(inout) :: Ebsky, Ebroom
    real(r64), dimension(MaxGap), intent(in) :: gap
    integer, intent(in) :: SupportPillar(maxlay)
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing, PillarRadius
    real(r64), intent(in) ::  hin, hout
    real(r64), dimension(maxlay), intent(inout) :: Ra, Nu
    real(r64), intent(inout) :: hrin, hrout, hcin, hcout
    real(r64), dimension(maxlay1), intent(out):: hgas, hcgas, hrgas
    real(r64), intent(out) :: troom, tamb
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    integer :: i, k, nface
    !character*(3) :: a

  !  common    /props/ gcon(maxgas,3),gvis(maxgas,3),gcp(maxgas,3),grho(maxgas,3),wght(maxgas)

    ! evaluate convective/conductive components of gap grashof number, thermal conductivity and their derivatives:
    nface = 2*nlayer

    !do i = 1, nlayer
    !  j=2*i

    !  if ((Ebb(i)-Ebf(i)).eq.0) then
    !    theta(j) = theta(j) + tempCorrection
    !    Ebb(i) = sigma * (theta(j) ** 4)
    !  end if
    !  hhat(j) = scon(i)/thick(i) * (theta(j)-theta(j-1))/(Ebb(i)-Ebf(i))

      !dr.....caluclate for laminate procedure
    !  if (nslice(i).gt.1) then
    !    if ((LaminateB(i).ne.0).and.((Ebb(i)-Ebf(i)).ne.0)) then
    !      hhat(j) = (theta(j)-theta(j-1))/(LaminateB(i) * (Ebb(i)-Ebf(i)))
    !    end if
    !  end if
      !if (hhat(j).lt.0) then
      !  nperr = 6
      !  write(a, '(i3)') i
      !  ErrorMessage = 'Heat transfer coefficient based on emissive power in glazing layer is less than zero. Layer #'//trim(a)
      !  return
      !end if
    !end do

    call filmg(tilt, theta, Tgap, nlayer, height, gap, iprop, frct, VacuumPressure, presure, nmix, &
        wght, gcon, gvis, gcp, gama, hcgas, Ra, Nu, nperr, ErrorMessage)

    if (.not.(GoAhead(nperr))) then
      return
    end if

    !this is adding influence of pillar to hgas
    call filmPillar(SupportPillar, scon, PillarSpacing, PillarRadius, nlayer, gap, hcgas, VacuumMaxGapThickness, &
                      nperr, ErrorMessage)

    if (.not.(GoAhead(nperr))) then
      return
    end if

    ! adjust radiation coefficients
    !hrgas = 0.0d0
    do i=2, nlayer
      k = 2*i-1
      !if ((theta(k)-theta(k-1)) == 0) then
      !  theta(k-1) = theta(k-1) + tempCorrection
      !end if
      if ((theta(k)-theta(k-1)) /= 0) then
        hrgas(i) = (Radiation(k) - Radiation(k-1))/(theta(k)-theta(k-1))
      end if

      hgas(i) = hcgas(i) + hrgas(i)
    end do

    ! convective indoor film coeff:
    if (ibc(2).le.0) then
      call filmi(tind, theta(nface), nlayer, tilt, wsi, heightt, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, hcin, ibc(2), &
                  nperr, ErrorMessage)
    else if (ibc(2).eq.1) then
      hcin = hin-hrin
    !Simon: First iteration is with index = 0 and that means it should reenter iteration with whatever is provided as input
    !else if (ibc(2).eq.2.and.index.eq.1) then
    else if ((ibc(2).eq.2).and.(index.eq.0)) then
      hcin=hin
    end if
    if (hcin.lt.0) then
      nperr = 8
      ErrorMessage = 'Hcin is less then zero.'
      return
    end if

    hcgas(nlayer+1) = hcin
    !hrin = 0.95*(Ebroom - Radiation(2*nlayer))/(Trmin-theta(2*nlayer))+0.05*hrin
    hrin = (Ebroom - Radiation(2*nlayer))/(Trmin-theta(2*nlayer))
    !if ((Theta(2*nlayer) - Trmin).ne.0) then
    !  hrin =  sigma * emis(2*nlayer) * (Theta(2*nlayer)**4 - Trmin**4)/(Theta(2*nlayer) - Trmin)
    !else
    !  Theta(2*nlayer) = Theta(2*nlayer) + tempCorrection
    !  hrin =  sigma * emis(2*nlayer) * (Theta(2*nlayer)**4 - Trmin**4)/(Theta(2*nlayer) - Trmin)
    !end if
    hrgas(nlayer+1) = hrin
    !hgas(nlayer+1)  = hcgas(nlayer+1) + hrgas(nlayer+1)
    troom = (hcin*tind+hrin*trmin)/(hcin+hrin)

    ! convective outdoor film coeff:
    if (ibc(1).le.0) then
      call film(tout,theta(1),wso,iwd,hcout,ibc(1))
    else if (ibc(1).eq.1) then
      hcout = hout-hrout
    !Simon: First iteration is with index = 0 and that means it should reenter iteration with whatever is provided as input
    !else if (ibc(1).eq.2.and.index.eq.1) then
    else if ((ibc(1).eq.2).and.(index.eq.0)) then
      hcout=hout
    end if
    if (hcout.lt.0) then
      nperr = 9
      ErrorMessage = 'Hcout is less than zero.'
      return
    end if

    hcgas(1) = hcout
    hrout = (Radiation(1) - Ebsky)/(theta(1)-Trmout)
    !if ((Theta(1) - Trmout).ne.0) then
    !  hrout = sigma * emis(1) * (Theta(1)**4 - Trmout**4)/(Theta(1) - Trmout)
    !else
    !  Theta(1) = Theta(1) + tempCorrection
    !  hrout = sigma * emis(1) * (Theta(1)**4 - Trmout**4)/(Theta(1) - Trmout)
    !end if
    hrgas(1) = hrout
    !hgas(1)  = hrout + hcout
    tamb = (hcout*tout+hrout*trmout)/(hcout+hrout)

  end subroutine hatter

  subroutine filmi(tair,t,nlayer,tilt,wsi,height,iprop,frct,presure,nmix,wght,gcon,gvis,gcp,hcin,ibc,nperr,ErrorMessage)
    !***********************************************************************
    !  purpose to evaluate heat flux at indoor surface of window using still air correlations (Curcija and Goss 1993)
    !  found in SPC142 equations 5.43 - 5.48.
    !***********************************************************************
    ! Input
    !   tair - room air temperature
    !   t - inside surface temperature
    !   nlayer  number of glazing layers
    !   tilt - the tilt of the glazing in degrees
    !   wsi - room wind speed (m/s)
    !   height - window height
    !   iprop
    !   frct
    !   presure
    !   nmix  vector of number of gasses in a mixture for each gap
    ! Output
    !   hcin - indoor convecive heat transfer coeff

    ! If there is forced air in the room than use SPC142 corelation 5.49 to calculate the room side film coefficient.

    real(r64), intent(in) :: tair, t, tilt, wsi, height
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: presure
    real(r64), dimension(maxgas), intent(in) :: wght
    real(r64), dimension(maxgas, 3), intent(in) :: gcon, gvis, gcp
    integer, intent(in) :: nlayer, ibc
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), intent(out) :: hcin
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64), dimension(maxgas) :: frcti
    integer :: j
    integer, dimension(maxgas) :: ipropi
    real(r64) :: tiltr, tmean, delt, con, visc, dens, cp, pr, gr, RaCrit, RaL, Gnui

    if (wsi .gt. 0.0d0) then  ! main IF
      select case (ibc)
        case (0)
          hcin = 4.0d0 + 4.0d0 * wsi
        case (-1)
          hcin = 5.6d0 + 3.8d0 * wsi   ! SPC142 correlation
          return
      end select
    else       ! main IF - else
      tiltr = tilt*2.0d0*pi/360.0d0     ! convert tilt in degrees to radians
      tmean = tair + 0.25d0 * (t - tair)
      delt = ABS(tair-t)

      do j=1, nmix(nlayer+1)
        ipropi(j) = iprop(nlayer+1,j)
        frcti(j) = frct(nlayer+1,j)
      end do

      call gasses90(tmean, ipropi, frcti, presure(nlayer+1), nmix(nlayer+1), wght, gcon, gvis, gcp, con, visc, dens, cp, pr, &
                      ISO15099, nperr, ErrorMessage)

      !   Calculate grashoff number:
      !   The grashoff number is the Rayleigh Number (equation 5.29) in SPC142 divided by the Prandtl Number (prand):
      gr = GravityConstant * height**3 * delt*dens**2 / (tmean*visc**2)

      RaL = gr*pr
      !   write(*,*)' RaCrit,RaL,gr,pr '
      !   write(*,*) RaCrit,RaL,gr,pr

      if ((0.0d0.le.tilt).and.(tilt.lt.15.0d0)) then      ! IF no. 1
        Gnui = 0.13d0 * RaL**(1.0d0/3.0d0)
      else if ((15.0d0.le.tilt).and.(tilt.le.90.0d0)) then
        !   if the room air is still THEN use equations 5.43 - 5.48:
        RaCrit = 2.5d5 * (EXP(0.72d0 * tilt) / SIN(tiltr))**0.2d0
        if (RaL.le.RaCrit)  THEN           ! IF no. 2
          Gnui = 0.56d0 * (RaL * SIN(tiltr))**0.25d0
          ! write(*,*) ' Nu ', Gnui
        else
          !Gnui = 0.13*(RaL**0.3333 - RaCrit**0.3333) + 0.56*(RaCrit*sin(tiltr))**0.25
          Gnui = 0.13d0 * (RaL**(1.0d0/3.0d0) - RaCrit**(1.0d0/3.0d0)) + 0.56d0 * (RaCrit*sin(tiltr))**0.25d0
        end if              ! end if no. 2
      else if ((90.0d0.lt.tilt).and.(tilt.le.179.0d0)) then
        Gnui = 0.56d0 * (RaL*SIN(tiltr))**0.25d0
      else if ((179.0d0.lt.tilt).and.(tilt.le.180.0d0)) then
        Gnui = 0.58d0 * RaL**(1/3.0d0)
      end if                   ! end if no. 1
      !   write(*,*) ' RaL   ', RaL, '   RaCrit', RaCrit
      !   write(*,*)'   Nusselt Number   ',Gnui

      hcin = Gnui * (con/height)
  !   hin = 1.77*(abs(t-tair))**0.25

    end if  ! end main IF

  end subroutine filmi

  subroutine filmg(tilt, theta, Tgap, nlayer, height, gap, iprop, frct, VacuumPressure, presure, &
                    nmix, wght, gcon, gvis, gcp, gama, hcgas, Rayleigh, Nu, nperr, ErrorMessage)
    !***********************************************************************
    ! sobroutine to calculate effective conductance of gaps
    !***********************************************************************
    ! Inputs:
    !   tilt  window angle (deg)
    !   theta     vector of surface temperatures [K]
    !   nlayer    total number of glazing layers
    !   height    glazing cavity height
    !   gap   vector of gap widths [m]
    !   iprop
    !   frct
    !   presure
    !   nmix  vector of number of gasses in a mixture for each gap
    ! Output:
    !   hgas  vector of gap coefficients
    !   nperr     error code
    ! Locals:
    !   gr    gap grashof number
    !   con   gap gas conductivity
    !   visc  dynamic viscosity @ mean temperature [g/m*s]
    !   dens  density @ mean temperature [kg/m^3]
    !   cp    specific heat @ mean temperature [J/g*K]
    !   pr    gap gas Prandtl number
    !   tmean     average film temperature
    !   delt  temperature difference

    real(r64), intent(in) :: tilt, height, VacuumPressure
    real(r64), dimension(maxlay2), intent(in) :: theta
    real(r64), dimension(maxlay1), intent(in) :: Tgap
    real(r64), dimension(MaxGap), intent(in) :: gap
    real(r64), dimension(maxlay1), intent(in) :: presure
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxgas), intent(in) :: wght, gama
    real(r64), dimension(maxgas, 3), intent(in) :: gcon, gvis, gcp
    integer, intent(in) :: nlayer
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxlay), intent(out) :: Rayleigh, Nu
    real(r64), dimension(maxlay1), intent(out) :: hcgas
    integer, intent (inout) :: nperr
    character(len=*), intent (inout) :: ErrorMessage

    real(r64) :: con, visc, dens, cp, pr, delt, tmean, ra, asp, gnu
    real(r64), dimension(maxgas) :: frctg
    integer :: ipropg(maxgas), i, j, k, l

    hcgas = 0.0d0

    do i=1, nlayer-1
      j = 2*i
      k = j+1
      ! determine the gas properties of each gap:
      !tmean = (theta(j)+theta(k))/2.
      tmean = Tgap(i+1) ! Tgap(1) is exterior environment
      delt = ABS(theta(j)-theta(k))
      ! Temperatures should not be equal. This can happen in initial temperature guess before iterations started
      if (delt == 0.0d0) delt = 1.0d-6
      do l=1, nmix(i+1)
        ipropg(l) = iprop(i+1,l)
        frctg(l) = frct(i+1,l)
      end do

      if (presure(i+1).gt.VacuumPressure) then
        call gasses90(tmean, ipropg, frctg, presure(i+1), nmix(i+1), wght, gcon, gvis, gcp, con, visc, dens, cp, pr, &
                        ISO15099, nperr, ErrorMessage)

        ! Calculate grashoff number:
        ! The grashoff number is the Rayleigh Number (equation 5.29) in SPC142 divided by the Prandtl Number (prand):
        ra = GravityConstant * gap(i)**3 * delt* cp * dens**2 / (tmean*visc*con)
        Rayleigh(i) = ra
        ! write(*,*) 'height,gap(i),asp',height,gap(i),asp
        !asp = 1
        !if (gap(i).ne.0) then
        asp = height / gap(i)
        !end if
        ! determine the Nusselt number:
        call nusselt(tilt, ra, asp, gnu, nperr, ErrorMessage)

        Nu(i) = gnu
        ! calculate effective conductance of the gap
        hcgas(i+1) = con/gap(i)*gnu

        ! write(*,*)'theta(j),theta(k),j,k',j,theta(j),k,theta(k)
        ! write(*,*)'Nusselt,Rayleigh,Prandtl,hgas(k),k'
        ! write(*,*) gnu,gr*pr,pr,hgas(k),k
      else !low pressure calculations
        call GassesLow(tmean, wght(iprop(i+1, 1)), presure(i+1), gama(iprop(i+1, 1)), con, nperr, ErrorMessage)
        hcgas(i+1) = con
      end if !if (pressure(i+1).gt.VacuumPressure) then
    end do
  end subroutine filmg

  subroutine filmPillar(SupportPillar, scon, PillarSpacing, PillarRadius, nlayer, gap, hcgas, VacuumMaxGapThickness, &
                          nperr, ErrorMessage)
    !***********************************************************************
    ! subroutine to calculate effective conductance of support pillars
    !***********************************************************************

    integer, dimension(maxlay), intent(in) :: SupportPillar     ! Shows whether or not gap have support pillar
                                                                !   0 - does not have support pillar
                                                                !   1 - have support pillar

    real(r64), dimension(maxlay), intent(in) :: scon            ! Conductivity of glass layers
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing   ! Pillar spacing for each gap (used in case there is support pillar)
    real(r64), dimension(maxlay), intent(in) :: PillarRadius    ! Pillar radius for each gap (used in case there is support pillar)

    real(r64), intent(in) :: VacuumMaxGapThickness

    real(r64), dimension(MaxGap), intent(in) :: gap
    integer, intent(in) :: nlayer
    real(r64), dimension(maxlay1), intent(inout) :: hcgas
    integer, intent (inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64) :: cpa = 0.0d0
    real(r64) :: aveGlassConductivity = 0.0d0
    integer :: i = 0
    integer :: k = 0
    character(len=12) :: a, b

    do i=1, nlayer-1
      k = 2*i + 1
      if (SupportPillar(i).eq.YES_SupportPillar) then
!lkl        if (gap(i).gt.(VacuumMaxGapThickness + InputDataTolerance)) then
!lkl          nperr = 1007 !support pillar is not necessary for wide gaps (calculation will continue)
!lkl          write(a, '(f12.6)') VacuumMaxGapThickness
!lkl          write(b, '(i3)') i
!lkl          ErrorMessage = 'Gap width is more than '//trim(a)//' and it contains support pillar. Gap #'//trim(b)
!lkl        end if  !if (gap(i).gt.VacuumMaxGapThickness) then

        !Average glass conductivity is taken as average from both glass surrounding gap
        aveGlassConductivity = (scon(i) + scon(i+1)) / 2;

        cpa = 2.0d0 * aveGlassConductivity * PillarRadius(i) / ((PillarSpacing(i) ** 2) * &
          (1.0d0 + 2.0d0 * gap(i) / (pi * PillarRadius(i))))

        !It is important to add on prevoius values caluculated for gas
        hcgas(i+1) = hcgas(i+1) + cpa
      end if !if (SupportPillar(i).eq.YES_SupportPillar) then

    end do

  end subroutine filmPillar

  subroutine nusselt(tilt, ra, asp, gnu, nperr, ErrorMessage)
    !***********************************************************************
    ! purpose to calculate nusselt modulus for air gaps (ISO15099)
    !***********************************************************************
    ! Input
    !   tilt   tilt in degrees
    !   ra     rayleigh number
    !   asp    Aspect ratio
    !
    ! Output
    !   gnu    nusselt number
    !   nperr

    real(r64), intent(in) :: tilt, ra, asp
    real(r64), intent(out) :: gnu
    integer, intent(inout) :: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64) :: subNu1, subNu2, subNu3, Nu1, Nu2, G, Nu60, Nu90, tiltr

    subNu1 = 0.0d0
    subNu2 = 0.0d0
    subNu3 = 0.0d0
    Nu1 = 0.0d0
    Nu2 = 0.0d0
    Nu90 = 0.0d0
    Nu60 = 0.0d0
    G = 0.0d0
    tiltr = tilt*2.0d0*pi/360.0d0     ! convert tilt in degrees to radians
    if ((tilt.ge.0.0d0).and.(tilt.lt.60.0d0)) then                 !ISO/DIS 15099 - chapter 5.3.3.1
      subNu1 = 1.0d0 - 1708.0d0 / (ra * cos(tiltr))
      subNu1 = pos(subNu1)
      subNu2 = 1.0d0 - (1708.0d0 * (sin(1.8d0 * tiltr)) ** 1.6d0) / (ra * cos(tiltr))
      subNu3 = ((ra * cos(tiltr) / 5830.0d0) ** (1.0d0/3.0d0)) - 1.0d0
      subNu3 = pos(subNu3)
      gnu = 1.0d0 + 1.44d0 * subNu1 * subNu2 + subNu3                         !equation 42
      if (ra.ge.1.0d5) then
        nperr = 1001    ! Rayleigh number is out of range
        ErrorMessage = 'Rayleigh number out of range in Nusselt num. calc. for gaps (angle between 0 and 60 deg).'
      end if
      if (asp.le.20.0d0) then
        nperr = 1002    ! Aspect Ratio is out of range
        ErrorMessage = 'Aspect Ratio out of range in Nusselt num. calc. for gaps (angle between 0 and 60 deg).'
      end if
    else if (tilt.eq.60.0d0) then                               !ISO/DIS 15099 - chapter 5.3.3.2
      G = 0.5d0 / ((1.0d0 + (ra / 3160.0d0) ** 20.6d0) ** 0.1d0)                      !equation 47
      Nu1 = (1.0d0 + ((0.0936d0 * ra ** 0.314d0)/(1.0d0 + G)) ** 7) ** (0.1428571d0)  !equation 45
      Nu2 = (0.104d0 + 0.175d0 / asp) * (ra ** 0.283d0)                       !equation 46
      gnu = Max(Nu1, Nu2)                                               !equation 44
    else if ((tilt.gt.60.0d0).and.(tilt.lt.90.0d0)) then            !ISO/DIS 15099 - chapter 5.3.3.3
      if ((ra.gt.100.0d0).and.(ra.lt.2.0d7).and.(asp.gt.5.0d0).and.(asp.lt.100.0d0)) then
        G = 0.5d0 / ((1.0d0 + (ra / 3160.0d0) ** 20.6d0) ** 0.1d0)                     !equation 47
        Nu1 = (1.0d0 + ((0.0936d0 * ra ** 0.314d0)/(1.0d0 + G)) ** 7) ** (0.1428571d0) !equation 45
        Nu2 = (0.104d0 + 0.175d0 / asp) * (ra ** 0.283d0)                      !equation 46
        Nu60 = Max(Nu1, Nu2)                                             !equation 44
        Nu2 = 0.242d0 * (ra / asp) ** 0.272d0                                !equation 52
        if (ra.gt.5.0d4) then
          Nu1 = 0.0673838d0 * ra ** (1.0d0/3.0d0)                                !equation 49
        else if ((ra.gt.1.0d4).and.(ra.le.5.0d4)) then
          Nu1 = 0.028154d0 * ra ** 0.4134d0                                  !equation 50
        else if (ra.le.1.0d4) then
          Nu1 = 1.0d0 + 1.7596678d-10 * ra ** 2.2984755d0                      !equation 51
        end if
      else if (ra.le.100.0d0) then
        G = 0.5d0 / ((1.0d0 + (ra / 3160.0d0) ** 20.6d0) ** 0.1d0)                      !equation 47
        Nu1 = (1.0d0 + ((0.0936d0 * ra ** 0.314d0)/(1.0d0 + G)) ** 7) ** (0.1428571d0)  !equation 45
        Nu2 = (0.104d0 + 0.175d0 / asp) * (ra ** 0.283d0)                       !equation 46
        Nu60 = Max(Nu1, Nu2)                                              !equation 44
        Nu2 = 0.242d0 * (ra / asp) ** 0.272d0                                 !equation 52
        Nu1 = 1.0d0 + 1.7596678d-10 * ra ** 2.2984755d0                         !equation 51
        nperr = 1003   ! Rayleigh number is less than 100
        ErrorMessage = 'Rayleigh number is less than 100 in Nusselt number calculations for gaps '//  &
           '(angle between 60 and 90 degrees).'
      else if (ra.gt.2.0d7) then
        G = 0.5d0 / ((1.0d0 + (ra / 3160.0d0) ** 20.6d0) ** 0.1d0)                      !equation 47
        Nu1 = (1.0d0 + ((0.0936d0 * ra ** 0.314d0)/(1.0d0 + G)) ** 7) ** (0.1428571d0)  !equation 45
        Nu2 = (0.104d0 + 0.175d0 / asp) * (ra ** 0.283d0)                       !equation 46
        Nu60 = Max(Nu1, Nu2)                                              !equation 44
        Nu2 = 0.242d0 * (ra / asp) ** 0.272d0                                 !equation 52
        Nu1 = 0.0673838d0 * ra ** (1.0d0/3.0d0)                                   !equation 49
        nperr = 1004   ! Rayleigh number is great from 2e7
        ErrorMessage = 'Rayleigh number is greater than 2e7 in Nusselt number calculations for gaps'// &
                          ' (angle between 60 and 90 degrees).'
      else if ((asp.le.5.0d0).or.(asp.ge.100.0d0)) then
        G = 0.5d0 / ((1.0d0 + (ra / 3160.0d0) ** 20.6d0) ** 0.1d0)                      !equation 47
        Nu1 = (1.0d0 + ((0.0936d0 * ra ** 0.314d0)/(1.0d0 + G)) ** 7) ** (0.1428571d0)  !equation 45
        Nu2 = (0.104d0 + 0.175d0 / asp) * (ra ** 0.283d0)                       !equation 46
        Nu60 = Max(Nu1, Nu2)                                              !equation 44
        Nu2 = 0.242d0 * (ra / asp) ** 0.272d0                                 !equation 52
        if (ra.gt.5.0d4) then
          Nu1 = 0.0673838d0 * ra ** (1.0d0/3.0d0)                                 !equation 49
        else if ((ra.gt.1.0d4).and.(ra.le.5.0d4)) then
          Nu1 = 0.028154d0 * ra ** 0.4134d0                                   !equation 50
        else if (ra.le.1.0d4) then
          Nu1 = 1.0d0 + 1.7596678d-10 * ra ** 2.2984755d0                       !equation 51
        end if
        nperr = 1005 ! Aspect Ratio is out of range
        ErrorMessage = 'Aspect Ratio is out of range in Nusselt number calculations for gaps (angle between 60 and 90 degrees).'
      end if
      Nu90 = Max(Nu1, Nu2)                                  !equation 48
      gnu = ((Nu90 - Nu60) / (90.0d0 - 60.0d0)) * (tilt - 60.0d0) + Nu60  !linear interpolation between 60 and 90 degrees
    else if (tilt.eq.90.0d0) then                                !ISO/DIS 15099 - chapter 5.3.3.4
      Nu2 = 0.242d0 * (ra / asp) ** 0.272d0                     !equation 52
      if (ra.gt.5.0d4) then
        Nu1 = 0.0673838d0 * (ra ** (1.0d0/3.0d0))             !equation 49
      else if ((ra.gt.1.0d4).and.(ra.le.5.0d4)) then
        Nu1 = 0.028154d0 * ra ** 0.4134d0                       !equation 50
        !Nu1 = 0.028154 * ra ** 0.414d0                       !equation 50 - DISCONTINUITY CORRECTED
      else if (ra.le.1.0d4) then
        Nu1 = 1.0d0 + 1.7596678d-10 * ra ** 2.2984755d0           !equation 51
      end if
      gnu = Max(Nu1, Nu2)                                   !equation 48
    else if ((tilt.gt.90.0d0).and.(tilt.le.180.0d0)) then
      Nu2 = 0.242d0 * (ra / asp) ** 0.272d0                     !equation 52
      if (ra.gt.5.0d4) then
        Nu1 = 0.0673838d0 * ra ** (1.0d0/3.0d0)               !equation 49
      else if ((ra.gt.1.0d4).and.(ra.le.5.0d4)) then
        Nu1 = 0.028154d0 * ra ** 0.4134d0                       !equation 50
      else if (ra.le.1.0d4) then
        Nu1 = 1.0d0 + 1.7596678d-10 * ra ** 2.2984755d0           !equation 51
      end if
      gnu = Max(Nu1, Nu2)                                   !equation 48
      gnu = 1.0d0 + (gnu - 1.0d0) * SIN(tiltr)                      !equation 53
    else
      nperr = 10    !error flag: angle is out of range
      ErrorMessage = 'Window tilt angle is out of range.'
    return
    end if

  end subroutine nusselt

!  subroutine picard(nlayer, alpha, Ebb, Ebf, Rf, Rb, Ebbold, Ebfold, Rfold, Rbold)
!
!    integer, intent(in) :: nlayer
!    real(r64), intent(in) :: alpha
!    real(r64), intent(in) :: Ebbold(maxlay), Ebfold(maxlay), Rbold(maxlay), Rfold(maxlay)
!    real(r64), intent(inout) :: Ebb(maxlay), Ebf(maxlay), Rb(maxlay), Rf(maxlay)
!
!    integer :: i
!
!    do i=1,nlayer
!      Ebb(i) = alpha * Ebb(i) + (1-alpha) * Ebbold(i)
!      Ebf(i) = alpha * Ebf(i) + (1-alpha) * Ebfold(i)
!      Rb(i) = alpha * Rb(i) + (1-alpha) * Rbold(i)
!      Rf(i) = alpha * Rf(i) + (1-alpha) * Rfold(i)
!    end do
!
!    return
!  end subroutine picard

  subroutine adjusthhat(SDLayerIndex, ibc, tout, tind, nlayer, theta, wso, wsi, iwd, height, heightt, tilt,  &
                     thick, gap, hout, hrout, hin, hrin, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, &
                     index, SDScalar, Ebf, Ebb, hgas, hhat, nperr, ErrorMessage)

    !********************************************************************
    !  Modifies hhat, hgas coefficients around SD layers
    !********************************************************************

    integer, intent(in) :: SDLayerIndex, nlayer, iwd, index
    integer, dimension(2), intent(in) :: ibc
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop
    integer, dimension(maxlay1), intent(in) :: nmix
    real(r64), dimension(maxgas), intent(in) :: wght
    real(r64), dimension(maxgas, 3), intent(in) :: gcon, gvis, gcp
    real(r64), intent(in) :: tout, tind, wso, wsi, height, heightt, tilt
    real(r64), dimension(maxlay2), intent(in) :: theta
    real(r64), dimension(maxlay), intent(in) :: thick
    real(r64), dimension(MaxGap), intent(in) :: gap
    real(r64), intent(in) :: hout, hrout, hin, hrin, SDScalar
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct
    real(r64), dimension(maxlay1), intent(in) :: presure
    real(r64), dimension(maxlay), intent(in) :: Ebf, Ebb
    real(r64), dimension(maxlay), intent(inout) :: hgas
    real(r64), dimension(maxlay3), intent(inout) :: hhat
    integer, intent(inout):: nperr
    character(len=*), intent(inout) :: ErrorMessage

    real(r64) :: hc_NOSD, hc_0, hc_1, hc_alpha, hhat_alpha
    real(r64) :: hc_1_1, hc_1_2, hc_alpha1, hc_alpha2, hhat_alpha1, hhat_alpha2
    real(r64), dimension(maxgas) :: frctg
    integer :: i, j, k, l
    integer, dimension(maxgas) :: ipropg
    real(r64) :: tmean, con, visc, dens, cp, pr, delt, gap_NOSD, rayl, asp, gnu


    !bi...  Step 1: Calculate hc as if there was no SD here
    if (SDLayerIndex.eq.1) then
      !car    SD is the first layer (outdoor)
      ! calc hc_0 as hcout:
      ! convective outdoor film coeff:
      if (ibc(1).le.0) then
        call film(tout,theta(3),wso,iwd,hc_NOSD,ibc(1))
      else if (ibc(1).eq.1) then
        hc_NOSD = hout-hrout
      else if ((ibc(1).eq.2).and.(index.eq.1)) then
        hc_NOSD=hout
      end if
      if (hc_NOSD.lt.0) then
        nperr = 9
        ErrorMessage = 'Hcout is out of range.'
        return
      end if
      hc_0 = hc_NOSD*(theta(3)-tout)/(theta(3)-theta(2))
      hc_1 = hgas(2)
      hc_alpha = SDScalar*(hc_1 - hc_0) + hc_0
      hhat_alpha = hc_alpha*(theta(3)-theta(2)) / (Ebf(2)-Ebb(1))

      hgas(2) = hc_alpha
      hhat(3) = hhat_alpha
    else if (SDLayerIndex.eq.nlayer) then
      !car    SD is the last layer (indoor)
      ! calc hc_0 as hcin:
      ! convective indoor film coeff:
      if (ibc(2).le.0) then
        call filmi(tind, theta(2*nlayer-2), nlayer, tilt, wsi, heightt, iprop, frct, presure, nmix, wght, gcon, gvis, gcp, &
                    hc_NOSD, ibc(2), nperr, ErrorMessage)
      else if (ibc(2).eq.1) then
        hc_NOSD = hin-hrin
      else if (ibc(2).eq.2.and.index.eq.1) then
        hc_NOSD = hin
      end if
      if (hc_NOSD.lt.0) then
        nperr = 8
        ErrorMessage = 'Hcin is out of range.'
        return
      end if
      !    hgas(2*nlayer+1) = hcin
      hc_0 = hc_NOSD*(tind-theta(2*nlayer-2))/(theta(2*nlayer-1)-theta(2*nlayer-2))
      hc_1 = hgas(nlayer+1)
      hc_alpha = SDScalar*(hc_1 - hc_0) + hc_0
      hhat_alpha = hc_alpha*(theta(2*nlayer-1) - theta(2*nlayer-2)) / (Ebf(nlayer)-Ebb(nlayer-1))

      hgas(nlayer+1) = hc_alpha
      hhat(2*nlayer-1) = hhat_alpha
    else
      !car     SD is in between glazing
      !  calc hc_NOSD as hcgas:
      j = 2*SDLayerIndex-2
      k = j+3
      ! determine the gas properties for this "gap":
      tmean = (theta(j)+theta(k))/2.0d0
      delt = abs(theta(j)-theta(k))
      i = SDLayerIndex
      do l=1, nmix(i+1)
        ipropg(l) = iprop(i+1,l)
        frctg(l) = frct(i+1,l)
      end do
      call gasses90(tmean, ipropg, frctg, presure(i+1), nmix(i+1), wght, gcon, gvis, gcp, con, visc, dens, cp, pr, &
                    ISO15099, nperr, ErrorMessage)
      gap_NOSD = gap(SDLayerIndex-1) + gap(SDLayerIndex) + thick(SDLayerIndex)
      ! determine the Rayleigh number:
      rayl = GravityConstant * gap_NOSD**3 * delt* cp * dens**2 / (tmean*visc*con)
      asp = height / gap_NOSD
      ! determine the Nusselt number:
      call nusselt(tilt, rayl, asp, gnu, nperr, ErrorMessage)
      ! calculate effective conductance of the gap
      hc_NOSD = con/gap_NOSD*gnu
      i = SDLayerIndex
      j = 2 * i
      !car     changed hc interpolation with temperatures by inverting ratios
      !car  hc_0_1 = hc_NOSD*abs((theta(2*i-1) - theta(2*i-2))/(theta(2*i+1) - theta(2*i-2)))
      !car  hc_0_2 = hc_NOSD*abs((theta(2*i+1) - theta(2*i))/(theta(2*i+1) - theta(2*i-2)))
      hc_1_1 = hgas(i)
      hc_1_2 = hgas(i+1)

      !car    changed how SDscalar interpolation is done.  Instead of hc_0_1 and hc_0_2, hc_NOSD is used
      hc_alpha1 = SDScalar*(hc_1_1 - hc_NOSD) + hc_NOSD
      hc_alpha2 = SDScalar*(hc_1_2 - hc_NOSD) + hc_NOSD

      hhat_alpha1 = hc_alpha1*(theta(j-1) - theta(j-2)) / (Ebf(i)-Ebb(i-1))
      hhat_alpha2 = hc_alpha2*(theta(j+1) - theta(j)) / (Ebf(i+1)-Ebb(i))

      hgas(i) = hc_alpha1
      hgas(i+1) = hc_alpha2
      hhat(j-1) = hhat_alpha1
      hhat(j+1) = hhat_alpha2
    end if

  end subroutine adjusthhat

  subroutine storeIterationResults(nlayer, index, theta, Trmout, Tamb, Trmin, Troom, Ebsky, Ebroom, &
              hcin, hcout, hrin, hrout, hin, hout, Ebb, Ebf, Rb, Rf, nperr)

    use DataGlobals, only: KelvinConv

    integer, intent(inout) :: nperr
    !character*(*), intent(inout) :: ErrorMessage
    integer, intent(in) :: nlayer, index
    real(r64), dimension(maxlay), intent(in) :: theta
    real(r64), intent(in) :: Trmout, Tamb, Trmin, Troom
    real(r64), intent(in) :: Ebsky, Ebroom
    real(r64), intent(in) :: hcin, hcout, hrin, hrout, hin, hout
    real(r64), dimension(maxlay), intent(in) :: Ebb, Ebf, Rb, Rf

    !localy used
    character(len=1024) :: dynFormat = ''
    character(len=3) :: a
    integer :: i

    !open(unit=InArgumentsFile,  file=TRIM(DBGD)//'TarcogIterations.dbg',  status='unknown', access='APPEND',  &
    !          &  form='formatted', iostat=nperr)

    !if (nperr.ne.0)  open(unit=InArgumentsFile,  file='TarcogIterations.dbg',  status='unknown', access='APPEND',  &
    !          &  form='formatted', iostat=nperr)

    !open(unit=IterationCSV,  file=TRIM(DBGD)//Trim(IterationCSVName),  status='unknown', access='APPEND',  &
    !          &  form='formatted', iostat=nperr)

    !if (nperr.ne.0)  open(unit=IterationCSV,  file=Trim(IterationCSVName),  status='unknown', access='APPEND',  &
    !          &  form='formatted', iostat=nperr)

    !open(unit=IterationHHAT,  file=TRIM(DBGD)//Trim(IterationHHATName),  status='unknown', access='APPEND',  &
    !          &  form='formatted', iostat=nperr)

    !if (nperr.ne.0)  open(unit=IterationHHAT,  file=Trim(IterationHHATName),  status='unknown', access='APPEND',  &
    !          &  form='formatted', iostat=nperr)

    !write(a,1000) index
    write(TarcogIterationsFileNumber, '("*********************************************************************************'// &
        &'****************")')
    write(TarcogIterationsFileNumber, '("Iteration number: ", i5)') index

    write(TarcogIterationsFileNumber, '("Trmin = ", f8.4)') Trmin - KelvinConv
    write(TarcogIterationsFileNumber, '("Troom = ", f12.6)') Troom - KelvinConv
    write(TarcogIterationsFileNumber, '("Trmout = ", f8.4)') Trmout - KelvinConv
    write(TarcogIterationsFileNumber, '("Tamb = ", f12.6)') Tamb - KelvinConv

    write(TarcogIterationsFileNumber, '("Ebsky = ", f8.4)') Ebsky
    write(TarcogIterationsFileNumber, '("Ebroom = ", f8.4)') Ebroom

    write(TarcogIterationsFileNumber, '("hcin = ", f8.4)') hcin
    write(TarcogIterationsFileNumber, '("hcout = ", f8.4)') hcout
    write(TarcogIterationsFileNumber, '("hrin = ", f8.4)') hrin
    write(TarcogIterationsFileNumber, '("hrout = ", f8.4)') hrout
    write(TarcogIterationsFileNumber, '("hin = ", f8.4)') hin
    write(TarcogIterationsFileNumber, '("hout = ", f8.4)') hout

    !Write headers for Ebb and Ebf
    do i = 1, 2*nlayer

      write(a,1000) (i + 1)/2 !this is just to simulate correct integer in brackets
      if (i.eq.1) then
        dynFormat = '("'
      end if
      if (mod(i, 2).eq.1) then
        dynFormat = TRIM(dynFormat)//'Ebf('//a//')'
      else
        dynFormat = TRIM(dynFormat)//'Ebb('//a//')'
      end if
      if (i.eq.2*nlayer) then
        dynFormat = TRIM(dynFormat)//'")'
      else
        dynFormat = TRIM(dynFormat)//'==='
      end if
    end do
    write(TarcogIterationsFileNumber, dynFormat)

    !write Ebb and Ebf
    do i = 1, 2*nlayer

      if (i.eq.1) then
        dynFormat = '(  '
      end if
      dynFormat = TRIM(dynFormat)//'f16.8'
      if (i.eq.2*nlayer) then
        dynFormat = TRIM(dynFormat)//')'
      else
        dynFormat = TRIM(dynFormat)//',"   " '
      end if
    end do
    write(TarcogIterationsFileNumber, dynFormat) (Ebf(i), Ebb(i), i=1,nlayer)

    !Write headers for Rb and Rf
    do i = 1, 2*nlayer

      write(a,1000) (i + 1)/2 !this is just to simulate correct integer in brackets
      if (i.eq.1) then
        dynFormat = '("'
      end if
      if (mod(i, 2).eq.1) then
        dynFormat = TRIM(dynFormat)//'Rf('//a//')'
      else
        dynFormat = TRIM(dynFormat)//'Rb('//a//')'
      end if
      if (i.eq.2*nlayer) then
        dynFormat = TRIM(dynFormat)//'")'
      else
        dynFormat = TRIM(dynFormat)//'==='
      end if
    end do
    write(TarcogIterationsFileNumber, dynFormat)

    !write Rb and Rf
    do i = 1, 2*nlayer

      if (i.eq.1) then
        dynFormat = '(  '
      end if
      dynFormat = TRIM(dynFormat)//'f16.8'
      if (i.eq.2*nlayer) then
        dynFormat = TRIM(dynFormat)//')'
      else
        dynFormat = TRIM(dynFormat)//',"   " '
      end if
    end do
    write(TarcogIterationsFileNumber, dynFormat) (Rf(i), Rb(i), i=1,nlayer)

    !Write header for temperatures
    do i = 1, 2*nlayer

      write(a,1000) i
      if (i.eq.1) then
        dynFormat = '("'
      end if
      dynFormat = TRIM(dynFormat)//'theta('//a//')'
      if (i.eq.(2*nlayer)) then
        dynFormat = TRIM(dynFormat)//'")'
      else
        dynFormat = TRIM(dynFormat)//'=='
      end if
    end do
    write(TarcogIterationsFileNumber, dynFormat)


    !write temperatures
    do i = 1, 2*nlayer

      if (i.eq.1) then
        dynFormat = '(  '
      end if
      dynFormat = TRIM(dynFormat)//'f8.4'
      if (i.eq.(2*nlayer)) then
        dynFormat = TRIM(dynFormat)//')'
      else
        dynFormat = TRIM(dynFormat)//',"   " '
      end if
    end do
    write(TarcogIterationsFileNumber, dynFormat) (theta(i) - KelvinConv, i=1,2*nlayer)

    !close(TarcogIterationsFileNumber)

    !write results in csv file
    if (index.eq.0) then
      dynFormat = '("  '
      do i = 1, 2*nlayer
        write(a,1000) i !this is just to simulate correct integer in brackets
        if (i.ne.2*nlayer) then
          dynFormat = trim(dynFormat)//'theta('//a//'),'
        else
          dynFormat = trim(dynFormat)//'theta('//a//')'
        end if
      end do
      dynFormat = trim(dynFormat)//'")'
      write(IterationCSVFileNumber, dynFormat)
    end if
    do i = 1, 2*nlayer

      if (i.eq.1) then
        dynFormat = '(  '
      end if
      dynFormat = TRIM(dynFormat)//'f8.4'
      if (i.eq.(2*nlayer)) then
        dynFormat = TRIM(dynFormat)//')'
      else
        dynFormat = TRIM(dynFormat)//',",   " '
      end if
    end do
    write(IterationCSVFileNumber, dynFormat) (theta(i) - KelvinConv, i=1,2*nlayer)

    !close(IterationCSVFileNumber)


  1000  format(I3)
  end subroutine storeIterationResults

  subroutine CalculateFuncResults(nlayer, a, b, x, FRes)
    !calculate balance equations by using temperature solution and estimates stores error in FRes
    !real(r64), intent(in) :: theta(maxlay2)
    !real(r64), intent(in) :: R(maxlay2)  ! Radiation on layer surfaces
    integer, intent(in) :: nlayer
    real(r64), dimension(4*nlayer, 4*nlayer), intent(in) :: a
    real(r64), dimension(4*nlayer), intent(in) :: b, x

    !integer, intent(out) :: nperr
    !character*(*), intent(out) :: ErrorMessage
    real(r64), dimension(4*nlayer), intent(inout) :: FRes

    !Local variables
    integer :: i, j

    do i=1, 4*nlayer
      FRes(i) = - b(i)
      do j=1, 4*nlayer
        FRes(i) = FRes(i) + a(i,j) * x(j)
      end do
    end do !do i=1, 4*nlayer

  end subroutine CalculateFuncResults

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

end module ThermalISO15099Calc

module TARCOGMain
          ! TARCOG: Thermal Analysis Routine for Center of Glazing

          ! MODULE INFORMATION:
          !       AUTHOR         D. Charlie Curcija
          !       DATE WRITTEN   July 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  (see information bellow)
          !
          !  Revision: 7.0.13  (March/27/2012), Simon Vidanovic
          !   - feature: New set of equaitons is set instead of hhat coefficents and new approach to solution which improves
          !               speed and stability.  Note that this solution does not include laminates
          !
          !  Revision: 7.0.12  (March/06/2012), Simon Vidanovic
          !   - feature: Additional state for isky introduced.  Tarcog now can accept IR radiance from external source.
          !
          !  Revision: 7.0.11  (January/04/2012), Simon Vidanovic
          !   - imrovements/bug fixes: Several items merged from Windows tarcog version into this one:
          !      - bug fix: Correct foramtting for VacuumMaxGapThickness when program writes input file
          !      - improvement: Gamma coefficient is now written in scientific notation (needed for correct output file generation)
          !      - imporvement: Gap data are now saved with higer precision to wincog input file (test purposes, debug mode only)
          !      - bug fix: Gap temperatures are recalculated within iterations (for thermally driven and forced ventilation)
          !
          !  Revision: 7.0.10  (December/15/2011), Simon Vidanovic
          !   - imrovement: Flag for performing SHGC calculations
          !
          !  Revision: 7.0.09  (November/15/2011), Simon Vidanovic
          !   - imrovement: Added error message tolerance (This is necessary to handle error messages in correct way)
          !
          !  Revision: 7.0.08  (November/15/2011), Simon Vidanovic
          !   - bug fix: Fixed program crashing when warrning message 1007 occured (output could not fit in string)
          !   - feature: relaxation parameter changed
          !
          !  Revision: 7.0.07  (November/08/2011), Simon Vidanovic
          !   - feature: Error message (as string) is now return from tarcog
          !
          !  Revision: 7.0.06  (November/07/2011), Simon Vidanovic
          !   - bug fix: Error report now actually use passed VacuumMaxGapThickness value
          !
          !  Revision: 7.0.05  (November/07/2011), Simon Vidanovic
          !   - bug fix: Troom and Tamb are not passed out of hhat routine after recalculation is performed.
          !              This will cause differences in calculation of U-factor
          !   - feature: VacuumMaxGapThickness is added to list of input paramters
          !
          !  Revision: 7.0.04  (November/03/2011), Simon Vidanovic
          !   - bug fix: one of debug files did not update properly
          !
          !  Revision: 7.0.03  (November/01/2011), Simon Vidanovic
          !   - tarcog will now exit if error code is in range from 2000 to 3000
          !   - tarcog now accepts file name which is template for debug output files creation
          !   - temperature correction added in case temperatures on the layers are equal
          !     which in case of energy calculation will give division with zero
          !   - iteration results now can be saved in the file (just for debugging purposes)
          !
          !  Revision: 7.0.02  (October/10/2011), Simon Vidanovic
          !   - Deflection calculations implemented.
          !
          !  Revision: 7.0.01  (September/23/2011), Simon Vidanovic
          !   - Support pillars implemented.
          !
          !  Revision: 7.0.00  (August/23/2011), Simon Vidanovic
          !   - Added comments to input arguments; Fixed bug in Nu calculation for angle between 60 and 90 degrees.
          !
          !  Revision: 6.0.36  (June/22/2010)
          !   - Converted to F95; refactoring.
          !
          !  Revision: 6.0.35  (June/4/2010)
          !   - Fixed a few potential bugs
          !
          !  Revision: 6.0.34  (May/26/2009)
          !   - Updated creation of W6 debug file (added ClosedBlind flag)
          !
          !  Revision: 6.0.33  (March/03/2008)
          !   - Fixed a bug in arguments checking.
          !   - Added creation of WINCOG input file in debug mode.
          !
          !  Revision: 6.0.32  (February/29/2008)
          !   - Applied CSM thermal model to Woven Shades layers.
          !   - Introduced a new error message (#39)
          !
          !  Revision: 6.0.31  (February/15/2008)
          !   - EN673 Design and Declared standards added/fixed (it used to be EN673 and ISO 10292).
          !
          !  Revision: 6.0.30  (May/05/2007)
          !   - Scalar model (CSM) fixed.
          !
          !  Revision: 6.0.29  (September/14/2006)
          !   - Fixed a bug in CSM calculation that affected some cases.
          !
          !  Revision: 6.0.28  (September/05/2006)
          !   - Woven Shade layer type introduced. These layers will be treated
          !    the same way as Venetian Blind layers.
          !
          !  Revision: 6.0.27  (August/24/2006)
          !   - Implemented new thermal model (thermal model 2 - Convection Scalar Model)
          !   - Added new input argument - SDScalar:
          !      0.0  - No SD (in terms of convection)
          !      1.0  - Closed SD (SD treated as a 'regular' specular layer)
          !      between 0 and 1 - cobination of No SD and Closed SD cases
          !   - A bug in unshaded run (remapping gas properties in removal of indoor SD layer) was fixed
          !   - TARCOG error codes have been updated (error codes 30 and 37 added)
          !
          !  Revision: 6.0.26  (May/31/2006)
          !   - Hard-coded Xenon gas properties for EN673 were updated.
          !   - hrin equation in EN673 routine was updated.
          !
          !  Revision: 6.0.25  (March/29/2006)
          !   - Bug fixes in EN673/ISO10292 procedure:
          !    .values of gas properties for EN673 and ISO10292 procedure can now be passed to TARCOG
          !     via A coefficients in gvis, gcon and gcp matrices and wght array
          !    .gas mixture buid-up limited to number of gasses used in the mix
          !    .dT array is now updated correctly after each iteration
          !    .hrin formula fixed
          !   - Bug fix (checking of slat tilt angle - negative values are now allowed)
          !
          !  Revision: 6.0.24  (November/25/2005)
          !   - Code responsible for ETR calculation (in U factor calculation)
          !    has been redesigned and cleared of all bugs.
          !
          !  Revision: 6.0.23  (November/24/2005)
          !   - Bug fix (wrong U factor results for IGUs with one glass + indoor SD)
          !   - Bug fix (wrong results for Hc modification ratios)
          !    when fixed H or fixed Hc model is used.
          !   - Allowed DiffuseShade as layer type (does not affect calculation algorithm).
          !   - hflux value has been updated - includes contribution from freely ventilated air
          !    around indoor SD layer (this affects SHGC value)
          !   - Debug file has been updated:
          !      . qr is shown in each gap,
          !      . hcgas values have been updated to include contribution from freely ventilated air
          !        around SD layers (this affects output argument as well),
          !      . hrgas values have been introduced in debug file.
          !
          !  Revision: 6.0.22  (November/04/2005)
          !   - Added an internal "unshaded" calculation run for glazing systems with
          !     outdoor and/or indoor SD layer(s). This run is needed for proper calculation
          !     of Hc modification ratios.
          !   - Added two new output arguments needed for connection with T6:
          !      HcUnshadedOut,
          !      HcUnshadeIn.
          !   - Changed the way ShadeHcRatioOut and ShadeHcRatioIn are caculated.
          !
          !  Revision: 6.0.21  (October/28/2005)
          !   - Fixed another serious bug in new routine for calculation of U factor.
          !
          !  Revision: 6.0.20  (October/20/2005)
          !   - Fixed a bug in new calculation of U factor.
          !   - Fixed a bug in formulas for calculation of SD thickness.
          !   - Forced ventilation calc has been disabled (since v6.0.19)
          !
          !  Revision: 6.0.19  (October/19/2005)
          !   - New input arguments added:
          !      SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, ThermalMod.
          !   - Argument gltype renamed to LayerType.
          !   - Thermal_model_1 implemented
          !   - U factor calculation has been updated.
          !   - Description of arguments has been updated.
          !   - Forced ventilation calc has been disabled.
          !
          !  Revision: 6.0.18  (September/16/2005)
          !   - Changed Tvent for outdoor and indoor SD layers to Tout and Tin, respectivelly.
          !   - Keff is now calculated for each gap.
          !
          !  Revision: 6.0.17  (September/08/2005)
          !   - Fixed a bug involving Al, Ar & Ah values (patch for a case of Al+Ar+Ah = 0).
          !
          !  Revision: 6.0.16  (September/07/2005)
          !   - Added new output arguments needed for connection with T6:
          !      ShadeEmisRatioOut,
          !      ShadeEmisRatioIn,
          !      ShadeHcRatioOut,
          !      ShadeHcRatioIn,
          !      Keff,
          !      ShadeGapKeffConv
          !
          !  Revision: 6.0.15  (August/30/2005)
          !   - Removed CHDIR call; used filepath//filename instead.
          !
          !  Revision: 6.0.14  (August/26/2005)
          !   - New arguments introduced:
          !      Debug_dir  - character array: target directory for debug output
          !      Window_ID  - integer: window ID (from W6)
          !      IGU_ID  - integer: IGU ID (from W6)
          !
          !  Revision: 6.0.13  (August/19/2005)
          !   - Bug fix #9 - allows calculations for Atop=Abot=0, by setting both values to 1e-6
          !
          !  Revision: 6.0.12  (August/12/2005)
          !   - Minor change in Picard coefficients for vented gaps - MaxIter cannot
          !     be less than 800. This solves issue with theta buffer in Therm1d.
          !   - Implemented creation of TARCOG debug file w/ i/o arguments/results (Tarcog.dbg)
          !     (fetaure #8); debug file will be created depending on Debug_mode switch:
          !      Debug_mode = 0 : debug file will not be created
          !      Debug_mode = 1 : debug file will be appended
          !      Debug_mode = 2 : new debug file will be created
          !   - Bug fix #7- qin result corrected in SHGC run.
          !
          !  Revision: 6.0.11  (July/15/2005)
          !   - Bug fix #4: an issue in original formulas that causes Therm1d
          !     to fail to converge for in certain cases. Changes were made in Therm1d
          !     that allow regular calculation outcome in these cases.
          !
          !  Revision: 6.0.10  (June/3/2005)
          !   - Bug fix #00005: added IGU Height in term A in ShadingIn and ShadingEdge
          !     procedures. This term is used in calculations of free vent air velocity
          !
          !  Revision: 6.0.09  (May/16/2005)
          !   - Bug fix #00001: for fixed combined coef. BC -> Trmout = Tout, Trmin = Tind
          !
          !  Revision: 6.0.08  (May/12/2005)
          !   - Debug flag introduced in Tarcog, Therm1d and dtemp procedures:
          !     debug info is stored in two files: tarcog.dbg and temps.out
          !
          !  Revision: 6.0.07  (April/12/2005)
          !   - Fixed a bug (#00002) in calculation of
          !     Picard method coefficients, in Therm1d procedure:
          !     fixes SD ventilation velocity related calcs.
          !
          !  Revision: 6.0.06  (October/22/2004)
          !   - Fixed a bug in velocity calculations
          !
          !  Revision: 6.0.05  (October/01/2004)
          !   - Changed name from w5cog to TARCOG
          !   - Changed version numbering
          !
          !---------------------------------------------
          !
          !  Revision: 5.3.105  (September/07/04)
          !
          !   - Changed argument 'standard' from string to integer
          !
          !  Revision: 5.3.02.03  (January/16/04)
          !
          !   - Slice temperatures calculated
          !   - Write out Nusselt and Rayleigh (in debug mode)
          !   - sol converted to asol in input format
          !
          !  Revision: 5.3.02.02  (11/14/03)
          !
          !   - Implemented CEN Standard
          !
          !  Revision: 5.3.02.01  (11/06/03)
          !
          !   - implemented Laminate procedure
          !
          !  Revision: 5.3.01  (09/22/03)
          !
          !   - fixed bug in Tgap initial calculation
          !   - reduce dimension of Tgap
          !
          !  Revision: 5.3.00  (08/25/03)
          !
          !   - make code more readable and rename version
          !
          !  Revision: 6.0.07  (08/10/03)
          !
          !   - implemented arrays for ventilation temperature and speed
          !   - repaired equation 121 in ISO (instead of cos(tilt) now is abs(cos(tilt)))
          !   - implemented input field for forced ventilation velocity (for all cases)
          !   - fixed bug in shading edge: coefficients B1, C1 and D1 repaired to calcluate value for gap temperature (not enviroment)
          !   - fixed bug in routine shading edge: characteristic Height is calculated for gap (not for enviroment)
          !   - fixed bug in proccesing error messages (this was produced slightly wrong in some cases results)
          !
          !  Revision: 6.0.06 rv  (04/22/03)
          !
          !   - write out heat flux calculation for shading device panes
          !   - qv smoothing: make vented heat flux same in adjacent gaps (gaps are divided by shading pane)
          !   - fixed bug in routine "shadingin": make that vented heat flow direction is calculated correct, according
          !     to temeratures in adjecent gaps
          !
          !  Revision: 6.0.05  (04/14/03)
          !
          !   - implemented "Picard" method in iteration loop
          !   - number of iterations is increased to 1000
          !   - alpha coefficient is set to 0.01 (Picard method)
          !
          !  Revision: 6.0.04  (03/03/03)
          !
          !   - implemented forced ventilation for indoor and outdoor shading devices
          !   - maximum number of iterations is increased to 100
          !
          !  Revision: 6.0.03 (02/25/03)
          !
          !   - fixed bug in error message for "nlayer<1"
          !   - error messages update
          !   - repaired U-value calculation and Tgap1, Tgap2 calculation for shading inside
          !   - shading devices implemented
          !   - error messages and warnings updated and tolerance is decreased to 1e-5
          !   - write out heat fluxes
          !   - fixed bug in hrin and hrout calculation
          !   - fixed bug in nusselt number for gap calculation for angle > 90 degrees
          !    and new error messages are added
          !
          !pr  Revision: 5.13 (01/16/02)
          !pr
          !pr   - pi value updated
          !pr
          !  Revision: 5.12 (10/27/01)
          !
          !   - deleted extra lines in hatter (just cleanup)
          !
          !  Revision: 5.11 (10/23/01)
          !
          !   - fixed bug in reporting SHGC
          !
          !pr  Revision: 5.10 (10/15/01) [Not included here - under testing]
          !pr
          !pr  - updated gas properties for Xenon (for EN673 standard)
          !
          !pr  Revision: 5.09 (10/12/01) [Not included here - under testing]
          !pr
          !pr  - implemented EN673 standard
          !
          !  Revision: 5.08 (10/11/01)
          !
          !   - corrected reporting for solar conditions
          !   - corrected array declaration for nmix in filmg
          !
          !  Revision: 5.07 (9/21/01) [not included here - under testing]
          !
          !   - implemented Laminate procedure
          !
          !pr  Revision: 5.06(09/18/00) [not included here - under testing]
          !pr
          !pr  - implemented CEN algorithms
          !
          ! Revised: June, 2001
          !
          !   - further implementation of ISO 15099
          !     * new def. of shgc
          !     * revised gas constants
          !     * implemented mean radiant temperature on indoor side
          !     *
          !   - cleaned bugs
          !     * fixed film coefficient didn't work correctly
          !   - removed condensation calculations (part of main w5 code now)
          !   - implemented environmental temperatures
          !
          ! Revised: January, 2001
          !
          !   - revised input format
          !   - streamlined code
          !   - implemented dll's
          !
          ! Revised: July 2000 (Major revision of the new code)
          !
          !   - checked for accuracy, cleaned bunch of bugs
          !   - implemented gas mixtures
          !
          ! Initial update of the old "therm" (WINDOW 4.1) code: Around 1998/99

          ! PURPOSE OF THIS MODULE:
          !   Module For Calculation of Thermal Performance Indices For Center
          !     of Glass According to ISO 15099/ASHRAE SPC142, ISO10292, and EN673

          ! METHODOLOGY EMPLOYED:
          !  Standard ISO 15099/ASHRAE SPC142, ISO10292 and EN673

          ! REFERENCES:
          ! ISO 15099/ASHRAE SPC142, ISO10292, EN673, Tarcog technical documentation

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

  use TarcogOutput
  use TARCOGGassesParams
  use TarcogShading
  use TARCOGArgs
  use ThermalISO15099Calc
  use ThermalEN673Calc
  use TARCOGDeflection

  implicit none

  public TARCOG90

  contains

  subroutine TARCOG90(nlayer, iwd, tout, tind, trmin, wso, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, &
              VacuumMaxGapThickness,  CalcDeflection, Pa, Pini, Tini, gap, GapDefMax, thick, scon, YoungsMod, PoissonsRat, &
              tir, emis, totsol, tilt, asol, height, heightt, width, &
              presure, iprop, frct, xgcon, xgvis, xgcp, xwght, gama, nmix,  &
              SupportPillar, PillarSpacing, PillarRadius, &
              theta, LayerDef, q, qv, ufactor, sc, hflux, hcin, hcout, hrin, hrout, hin, hout, hcgas, hrgas, shgc, nperr, &
              ErrorMessage, shgct, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, &
              SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB, &
              sumsol, hg, hr, hs, he, hi, Ra, Nu, standard, ThermalMod, Debug_mode, Debug_dir, Debug_file, win_ID, igu_ID, &
              ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, &
              HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, SHGCCalc, NumOfIterations)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!! function attributes:

    !!! INPUTS:

    !!! General:
    integer, intent(in) :: nlayer  !Number of layers (glass + SD)
    real(r64), intent(in) :: width  ! Window width
    real(r64), intent(in) :: height  ! IGU cavity height
    real(r64), intent(in) :: heightt  ! Window height
    integer, intent(in) :: standard  ! Calculation standard switch:
                                      !    1 - ISO15099
                                      !    2 - ISO10292
                                      !    3 - EN673
    integer, intent(in) :: ThermalMod  ! Thermal model:
                                        !    0 - ISO15099
                                        !    1 - Thermal model 1
                                        !    2 - Thermal model 2 (not implemented)
    integer, intent(in) :: Debug_mode ! Switch for debug output files:
                                        !    0 - don't create debug output files
                                        !    1 - append results to existing debug output file
                                        !    2 - store results in new debug output file
                                        !   3 - save in-between results (in all iterations) to existing debug file
    integer, intent(in) :: SHGCCalc      ! SHGC calculation switch:
                                          !    0 - do not perform SHGC calculations
                                          !    1 - perform SHGC calculations
    character(len=*), intent(in) :: Debug_dir  ! Target directory for debug files
    character(len=*), intent(in) :: Debug_file ! File name template for debug files
    character(len=*), intent(inout) :: ErrorMessage ! To store error message from tarcog execution
    integer, intent(in) :: win_ID  ! ID of window (passed by W6)
    integer, intent(in) :: igu_ID  ! ID of the IGU (passed by W6)

    !!! Environment related:
    real(r64), intent(inout) ::  tout    ! Outdoor temperature [K]
    real(r64), intent(inout) ::  tind    ! Indoor temperature [K]
    real(r64), intent(in) ::  wso    ! Outdoor wind speed [m/s]
    real(r64), intent(in) ::  wsi    ! Inside forced air speed [m/s]
    real(r64), intent(in) ::  dir    ! Direct solar radiation [W/m2]
    real(r64), intent(in) ::  outir  ! IR radiance of window's exterior surround [W/m2]
    real(r64), intent(in) ::  tsky    ! Night sky temperature [K]
    real(r64), intent(in) ::  fclr    ! Fraction of sky that is clear
    real(r64), intent(in) ::  VacuumPressure !maximal pressure for gas to be considered as vacuum
    real(r64) ::  VacuumMaxGapThickness !maximum allowed thickness without producing warning message
    real(r64), intent(in) ::  totsol  ! Total solar transmittance of the IGU
    real(r64), intent(in) ::  tilt    ! Window tilt [degrees]
    integer, intent(in) :: iwd  ! Wind direction:
                                  !    0 - windward
                                  !    1 - leeward
    integer, intent(in) :: isky  ! Flag for sky temperature(Tsky) and sky emittance(esky)
                                  !    0 - both Tsky and Esky are specified
                                  !    1 - Tsky specified; esky = 1
                                  !    2 - Swinbank model for effective sky emittance
                                  !    3 - IR radiance is provided from external source
    !integer, intent(in) :: mgas  ! Flag for gas property constants:
                                  !    0 - gas constants supplied (through gcon, gvis and gcp arrays)
                                  !    1 - use internal constants; when internal then first index
                                  !        in gcon, gciv and gcp is:
                                  !          1 - Air
                                  !          2 - Argon
                                  !          3 - Krypton
                                  !          4 - Xenon
    integer, dimension(2), intent(in):: ibc ! Vector of boundary condition flags (ibc(1) - outdoor, ibc(2) - indoor
                                            !    0 - h to be calculated
                                            !    1 - combined film coefficient h prescribed
                                            !    2 - convective film coefficient (hc) prescibed
                                            ! Also used in old algorithms for calculating h, accessible through negative
                                            ! values for flags:
                                            !    -1 - old SPC142 correlation
                                            !    -2 - Klems-Yazdanian correlation (applicable to outdoor only)
                                            !    -3 - Kimura correlation (applicable to outdoor only)

    !!! Layers:
    integer, dimension(maxlay), intent(in) :: LayerType      ! Glazing layer type flag
                                                             !    0 - Specular layer
                                                             !    1 - Venetian blind (SD)
                                                             !    2 - Woven shade (SD) (not implemented)
                                                             !    3 - Diffuse shade
    real(r64), dimension(maxlay2), intent(in) ::  tir        ! Vector of IR transmittances of each surface
    real(r64), dimension(maxlay2), intent(in) ::  emis       ! Vector of IR emittances of each surface
    real(r64), dimension(maxlay), intent(in) ::  asol        ! Vector of Absorbed solar energy fractions for each layer

    !!! Venetians:
    real(r64), dimension(maxlay), intent(in) ::  Atop        ! Vector with areas of top openings – between SD layers and top of
                                                             ! glazing cavity [m2]
    real(r64), dimension(maxlay), intent(in) ::  Abot        ! Vector with areas of bottom openings – between SD layers and
                                                             ! bottom of glazing cavity [m2]
    real(r64), dimension(maxlay), intent(in) ::  Al          ! Vector with areas of left-hand side openings – between SD layers and
                                                             ! left end of glazing cavity [m2]
    real(r64), dimension(maxlay), intent(in) ::  Ar          ! Vector of areas of right-hand side openings – between SD layers and
                                                             ! right end of glazing cavity [m2]
    real(r64), dimension(maxlay), intent(in) ::  Ah          ! Vector of total areas of holes for each SD [m2]
    real(r64), dimension(maxlay), intent(in) ::  SlatThick   ! Thickness of the slat material [m]
    real(r64), dimension(maxlay), intent(in) ::  SlatWidth   ! Slat width [m]
    real(r64), dimension(maxlay), intent(in) ::  SlatAngle   ! Slat tilt angle [deg]
    real(r64), dimension(maxlay), intent(in) ::  SlatCond    ! Conductivity of the slat material [W/m.K]
    real(r64), dimension(maxlay), intent(in) ::  SlatSpacing ! Distance between slats [m]
    real(r64), dimension(maxlay), intent(in) ::  SlatCurve   ! Curvature radius of the slat [m]
    real(r64), dimension(maxlay1), intent(in) ::  vvent      ! Vector of velocities for forced ventilation, for each gap, and for
                                                             ! outdoor and indoor environment [m/s]
    real(r64), dimension(maxlay1), intent(in) ::  tvent      ! Vector of temperatures of ventilation gas for forced ventilation,
                                                             ! for each gap, and for outdoor and indoor environment

    !!! Laminates:
    integer, dimension(maxlay), intent(in) :: nslice         ! Vector of numbers of slices in a laminated glazing layers
                                                             ! (0 – monolithic layer)
    real(r64), dimension(maxlay), intent(in) :: LaminateA    ! Left-hand side array for creating slice equations
    real(r64), dimension(maxlay), intent(in) :: LaminateB    ! Right-hand side array for creating slice equations
    real(r64), dimension(maxlay), intent(in) :: sumsol       ! Array of absorbed solar energy fractions for each laminated
                                                             ! glazing layer [W/m2]

    !!! Gaps:
    integer, dimension(maxlay1, maxgas), intent(in) :: iprop  ! Matrix of gas codes – see mgas definition
    integer, dimension(maxlay1), intent(in) :: nmix           ! Vector of number of gasses in gas mixture of each gap
    real(r64), dimension(maxlay1, maxgas), intent(in) :: frct ! Matrix of mass percentages in gap mixtures
    real(r64), dimension(maxlay1), intent(in) :: presure      ! Vector of gas pressures in gaps [N/m2]
    real(r64), dimension(maxgas, 3), intent(in) :: xgcon      ! Matrix of constants for gas conductivity calc
                                                              !  (A, B, C for max of 10 gasses)
    real(r64), dimension(maxgas, 3), intent(in) :: xgvis      ! Matrix of constants for gas dynamic viscosity calc
                                                              !  (A, B, C for max of 10 gasses)
    real(r64), dimension(maxgas, 3), intent(in) :: xgcp       ! Matrix of constants for gas specific heat calc at constant pressure
                                                              !  (A, B, C for max of 10 gasses)
    real(r64), dimension(maxgas), intent(in) :: xwght         ! Vector of Molecular weights for gasses
    real(r64), dimension(maxgas), intent(in) :: gama          ! Vector of spefic heat ration for low pressure calc

    integer, dimension(maxlay), intent(in) :: SupportPillar   ! Shows whether or not gap have support pillar
                                                              !   0 - does not have support pillar
                                                              !   1 - have support pillar
    real(r64), dimension(maxlay), intent(in) :: PillarSpacing ! Pillar spacing for each gap (used in case there is support pillar)
    real(r64), dimension(maxlay), intent(in) :: PillarRadius  ! Pillar radius for each gap (used in case there is support pillar)

    real(r64), intent(in) :: SDScalar  ! Factor of Venetian SD layer contribution to convection
                                       ! (used in conjunction with Thermal Model 2; otherwise, this value is ignored by TARCOG)
                                       ! – real(r64) value between 0 (SD contribution to convection is neglected) and
                                       !  1 (SD treated as “closed” – as if it is a glass layer with thermal
                                       !  properties of SD slat material)

    !Deflection
    integer, intent(in) :: CalcDeflection   ! Deflection calculation flag:
                                            !    0 - no deflection calculations
                                            !    1 - perform deflection calculation (input is Pressure/Temp)
                                            !    2 - perform deflection calculation (input is measured deflection)
    real(r64), intent(in) :: Pa             ! Atmospheric (outside/inside) pressure (used onlu if CalcDeflection = 1)
    real(r64), intent(in) :: Pini           ! Initial presssure at time of fabrication (used only if CalcDeflection = 1)
    real(r64), intent(in) :: Tini           ! Initial temperature at time of fabrication (used only if CalcDeflection = 1)
    real(r64), dimension(MaxGap), intent(inout) :: GapDefMax  ! Vector of gap widths in deflected state. It will be used as input
                                                                ! if CalcDeflection = 2. In case CalcDeflection = 1 it will return
                                                                ! recalculated gap widths. [m]
    real(r64), dimension(maxlay), intent(in) :: YoungsMod   ! Youngs Modulus coefficients used in deflection calculations
    real(r64), dimension(maxlay), intent(in) :: PoissonsRat ! Poissons Ratio coefficients used in deflection calculations

    !!!! INPUTS/OUTPUTS:
    real(r64), intent(inout) :: trmin                     ! Indoor mean radiant temperature [K]
    real(r64), intent(inout) :: esky                      ! Effective night sky emittance
    real(r64), dimension(maxlay), intent(inout) :: scon   ! Vector of conductivities of each glazing layer  [W/mK]
    real(r64), dimension(maxlay), intent(inout) :: thick  ! Vector of glazing layer thicknesses [m]
    real(r64), dimension(maxlay), intent(inout) :: gap    ! Vector of gap widths [m]
    real(r64), intent(inout) :: hin                       ! Indoor combined film coefficient (if non-zero) [W/m2K]
    real(r64), intent(inout) :: hout                      ! Outdoor combined film coefficient (if non-zero) [W/m2K]

      !!! OUTPUTS:
      !!! Overall:
    integer, intent(out) :: nperr        ! Error code
    integer, intent(out) :: NumOfIterations  ! Number of iterations for reacing solution
    real(r64), intent(out) :: ufactor    ! Center of glass U-value [W/m2 K]
    real(r64), intent(out) :: sc         ! Shading Coefficient
    real(r64), intent(out) :: hflux      ! Net heat flux between room and window [W/m2]
    real(r64), intent(out) :: shgc       ! Solar heat gain coefficient – per ISO 15099
    real(r64), intent(out) :: shgct      ! Solar heat gain coefficient – per old procedure
    real(r64), intent(out) :: he         ! External heat transfer coefficient [W/m2 K] – EN673 and ISO 10292 procedure
    real(r64), intent(out) :: hi         ! Internal heat transfer coefficient [W/m2 K] – EN673 and ISO 10292 procedure
    real(r64), intent(out) :: hcin       ! Indoor convective surface heat transfer coefficient  [W/m2 K]
    real(r64), intent(out) :: hrin       ! Indoor radiative surface heat transfer coefficient [W/m2 K]
    real(r64), intent(out) :: hcout      ! Outdoor convective surface heat transfer coefficient [W/m2 K]
    real(r64), intent(out) :: hrout      ! Outdoor radiative surface heat transfer coefficient [W/m2 K]
    real(r64), intent(out) :: tamb       ! Outdoor environmental temperature [K]
    real(r64), intent(out) :: troom      ! Indoor environmental temperature [K]

    !!! Layers:
    real(r64), dimension(maxlay2), intent(inout) :: theta ! Vector of average temperatures of glazing surfaces [K]
    real(r64), dimension(maxlay3), intent(out) :: q     ! Vector of various heat fluxes [W/m2]
                                                        ! depending on element index:
                                                        !    1 - qout (heat flux from outer-most glazing surface to outdoor space)
                                                        !    2*i = qpane(i) (heat flux through i-th glazing layer
                                                        !    2*i-1 = qgap(i) (heat flux from i-th glazing cavity to indoor-faced
                                                        !                      surface of the adjacent glazing layer)
                                                        !    2*nlayer + 1 = qin (heat flux from indoor space to inner-most glazing
                                                        !                        surface)

    real(r64), dimension(maxlay1), intent(out) :: qv       ! Vector of heat fluxes to each gap by ventillation [W/m2]
    real(r64), dimension(maxlay), intent(out) :: Keff      ! Vector of keff values for gaps [W/m.K]
    real(r64), dimension(maxlay), intent(out) :: LayerDef  ! Vector of layers deflection. [m]

    !!! Gaps:
    real(r64), dimension(maxlay1), intent(out) :: hcgas   ! Convective part of gap effective conductivity (including in and out)
    real(r64), dimension(maxlay1), intent(out) :: hrgas   ! Radiative part of gap effective conductivity (including in and out)
    real(r64), dimension(maxlay), intent(out) :: hg       ! Gas conductance of the glazing cavity [W/m2 K]
                                                          !      – EN673 and ISO 10292 procedure
    real(r64), dimension(maxlay), intent(out) :: hr       ! Radiation conductance of the glazing cavity [W/m2 K]
                                                          !      – EN673 and ISO 10292 procedure
    real(r64), dimension(maxlay), intent(out) :: hs       ! Thermal conductance of the glazing cavity [W/m2 K]
                                                          !      – EN673 and ISO 10292 procedure
    real(r64), dimension(maxlay), intent(out) :: Ra       ! Vector of Rayleigh numbers, for each gap
    real(r64), dimension(maxlay), intent(out) :: Nu       ! Vector of Nusselt numbers, for each gap

    !!! Shading related:
    real(r64), intent(out) :: ShadeEmisRatioOut   ! Ratio of modified to glass emissivity at the outermost glazing surface
    real(r64), intent(out) :: ShadeEmisRatioIn    ! Ratio of modified to glass emissivity at the innermost glazing surface
    real(r64), intent(out) :: ShadeHcRatioOut     ! Ratio of modified to unshaded Hc at the outermost glazing surface
    real(r64), intent(out) :: ShadeHcRatioIn      ! Ratio of modified to unshaded Hc at the innermost glazing surface
    real(r64), intent(out) :: HcUnshadedOut       ! Hc value at outermost glazing surface of an unshaded subsystem [W/m2 K]
    real(r64), intent(out) :: HcUnshadedin        ! Hc value at innermost glazing surface of an unshaded subsystem [W/m2 K]
    real(r64), dimension(MaxGap), intent(out) :: ShadeGapKeffConv  ! Vector of convective keff values for areas above/below
                                                                     !  SD layers [W/m.K]

    ! temporary variables stored between deflection iterations because tarcog need to produce result with exact same
    ! input variables
    real(r64) :: eskyTemp
    real(r64) :: trminTemp
    real(r64) :: hinTemp
    real(r64) :: houtTemp
    real(r64), dimension(maxlay) :: sconTemp
    real(r64), dimension(maxlay) :: thickTemp

    !real(r64), dimension(maxlay) ::  sol ! Vector of Absorbed solar energy for each layer [W/m2] = dir*asol

    !Internaly used
    logical :: converged = .false.  !used for convergence check in case of deflection calculations
    real(r64), dimension(maxlay2) :: told
    real(r64), dimension(MaxGap) :: CurGap
    real(r64), dimension(MaxGap) :: GapDefMean
    real(r64) :: dtmax
    integer :: i
    integer :: counter

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!! Body of TARCOG90

    he = 0.0d0
    hi = 0.0d0
    hcin = 0.0d0
    hrin = 0.0d0
    hcout = 0.0d0
    hrout = 0.0d0
    LayerDef = 0.0d0
    dtmax = 0.0d0
    i = 0
    counter = 0
    eskyTemp = 0.0d0
    trminTemp = 0.0d0
    hinTemp = 0.0d0
    houtTemp = 0.0d0
    ErrorMessage = 'Normal Termination'

    !sol = 0.0d0
    !if (dir.ne.0) then
    !  do i= 1, nlayer
    !    sol(i) = dir * asol(i)
    !  end do
    !end if

    do i = 1, nlayer - 1
      CurGap(i) = gap(i)
    end do

    !  Prepare common debug variables:
    call PrepDebugFilesAndVariables(Debug_dir, Debug_file, Debug_Mode, win_ID, igu_ID, nperr)

    ! Check input arguments:
    nperr = ArgCheck(nlayer,iwd,tout,tind,trmin,wso,wsi,dir,outir,isky,tsky,esky,fclr,VacuumPressure, VacuumMaxGapThickness, &
          & CalcDeflection, Pa, Pini, Tini, &
          & gap,GapDefMax,thick,scon,YoungsMod,PoissonsRat,tir,emis,totsol,  &
          &  tilt,asol,height,heightt,width,presure,iprop,frct,xgcon,xgvis,xgcp,xwght,gama,nmix,  &
          & SupportPillar, PillarSpacing, PillarRadius, &
          &  hin,hout, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, &
          &  SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB, &
          &  sumsol, standard, ThermalMod, SDScalar,ErrorMessage)

    ! in case of provided deflected gap widths just store deflected widhts before temperatures calculation
    ! deflections in this case do not depend of temperatures and it should be calculated before to avoid
    ! one extra call of temperatures calculations
    if (CalcDeflection.eq.DEFLECTION_CALC_GAP_WIDTHS) then
      call PanesDeflection(CalcDeflection, width, height, nlayer, Pa, Pini, Tini, thick, gap, &
          GapDefMax, GapDefMean, theta, YoungsMod, PoissonsRat, LayerDef, nperr, ErrorMessage)
      do i = 1, nlayer - 1
        CurGap(i) = GapDefMean(i)
      end do !do i = 1, nlayer - 1
    end if

    ! in case of deflection calculation for temperature & pressure input some variables needs to be stored because
    ! Calc_ISO15099 and EN673 routines will change them and for deflection recalculation everything needs to be
    ! called in same way except for changed gap widths
    if (CalcDeflection.eq.DEFLECTION_CALC_TEMPERATURE) then
      eskyTemp = esky
      trminTemp = trmin
      hinTemp = hin
      houtTemp = hout
      sconTemp = scon
      thickTemp = thick
    end if

    if (GoAhead(nperr)) then

      select case (standard)
        case (ISO15099)
          call Calc_ISO15099(nlayer,iwd,tout,tind,trmin,wso,wsi,dir,outir,isky,tsky,esky,fclr,VacuumPressure, &
                    VacuumMaxGapThickness, CurGap,thick,scon,tir,emis,totsol,  &
                    tilt,asol,height,heightt,width,presure,iprop,frct,xgcon,xgvis,xgcp,xwght,gama,nmix,  &
                    SupportPillar, PillarSpacing, PillarRadius, &
                    theta,q,qv,ufactor,sc,hflux,hcin,hcout,hrin,hrout,hin,hout,hcgas,hrgas,shgc,nperr,ErrorMessage, &
                    shgct,tamb,troom,ibc,Atop,Abot,Al,Ar,Ah, SlatThick, SlatWidth, SlatAngle, &
                    SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB, &
                    sumsol, Ra,Nu, ThermalMod, Debug_mode, &
                    ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, &
                    HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, SHGCCalc, NumOfIterations)
        case (EN673, EN673Design)
          call Calc_EN673(standard, nlayer, tout, tind, CurGap, thick, scon, emis, totsol,  &
                  &  tilt, dir, asol, presure, iprop, frct, nmix, xgcon, xgvis, xgcp, xwght, &
                  & theta, ufactor, hcin, hin, hout, shgc, nperr, ErrorMessage, ibc, hg, hr, hs, Ra,Nu)
        case default
      end select

    end if

    !Deflection calculations in case of temperature & pressure inputs
    if (GoAhead(nperr)) then
      if (.not.(GoAhead(nperr))) then
        return
      end if

      if (CalcDeflection.eq.DEFLECTION_CALC_TEMPERATURE) then
        converged = .false.
        do while (.not.(converged))
          call PanesDeflection(CalcDeflection, width, height, nlayer, Pa, Pini, Tini, thick, gap, &
            GapDefMax, GapDefMean, theta, YoungsMod, PoissonsRat, LayerDef, nperr, ErrorMessage)

          if (.not.(GoAhead(nperr))) then
            return
          end if

          !store temperatures before new calculations are performed. This is necessary in order to determine
          do i=1, 2*nlayer
            told(i)    = theta(i)
          end do !do i=1, 2*nlayer

          !before calling thermal calculations, return back old variables
          esky  = eskyTemp
          trmin = trminTemp
          hin   = hinTemp
          hout  = houtTemp
          scon  = sconTemp
          thick = thickTemp

          !after performed deflection recalculate temperatures with new gap widths
          select case (standard)
            case (ISO15099)
            call Calc_ISO15099(nlayer,iwd,tout,tind,trmin,wso,wsi,dir,outir,isky,tsky,esky,fclr,VacuumPressure, &
                        VacuumMaxGapThickness, GapDefMean,thick,scon,tir,emis,totsol,  &
                        tilt,asol,height,heightt,width,presure,iprop,frct,xgcon,xgvis,xgcp,xwght,gama,nmix,  &
                        SupportPillar, PillarSpacing, PillarRadius, &
                        theta,q,qv,ufactor,sc,hflux,hcin,hcout,hrin,hrout,hin,hout,hcgas,hrgas,shgc,nperr,ErrorMessage, &
                        shgct,tamb,troom,ibc,Atop,Abot,Al,Ar,Ah, SlatThick, SlatWidth, SlatAngle, &
                        SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB, &
                        sumsol, Ra,Nu, ThermalMod, Debug_mode, &
                        ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, &
                        HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, SHGCCalc, NumOfIterations)
            case (EN673, EN673Design)
              call Calc_EN673(standard, nlayer, tout, tind, GapDefMean, thick, scon, emis, totsol,  &
                      &  tilt, dir, asol, presure, iprop, frct, nmix, xgcon, xgvis, xgcp, xwght, &
                      & theta, ufactor, hcin, hin, hout, shgc, nperr, ErrorMessage, ibc, hg, hr, hs, Ra, Nu)
            case default
          end select !select case (standard)

          if (.not.(GoAhead(nperr))) then
            return
          end if

          !calc error
          dtmax = 0.0d0
          do i=1, 2*nlayer
            dtmax = abs(told(i) - theta(i))
          end do !do i=1, 2*nlayer

          if (dtmax < DeflectionErrorMargin) then
            converged = .true.
          end if
          counter = counter + 1

          if (counter > DeflectionMaxIterations) then
            converged = .true.
            nperr = 41  ! Deflection calculations failed to converge
            ErrorMessage = 'Deflection calculations failed to converge'
          end if
        end do !do while (.not.(converged))
      end if ! if ((CalcDeflection.eq.DEFLECTION_CALC_TEMPERATURE).or.(CalcDeflection.eq.DEFLECTION_CALC_GAP_WIDTHS)) then
    end if  ! if (GoAhead(nperr)) then

    call FinishDebugOutputFiles(nperr)

  end subroutine TARCOG90
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

end module TARCOGMain
