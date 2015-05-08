MODULE DataConversions      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for conversion unit variables which
          ! strictly "should not be needed" in EnergyPlus but may be used in a few
          ! places (e.g. Conduction Transfer Functions).  We have adopted the "international
          ! table" for conversions.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Original Reference: DARCOM P 706-470, Engineering Design Handbook,
          ! Metric Conversion Guide, July 1976.
          ! Federal Standard 376B, January 27, 1993.  Preferred metric units
          ! for general use by the Federal Government.
          ! ASHRAE has similar recommendations.

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
!REAL(r64), PARAMETER:: CFC     =4.184D0            ! Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
REAL(r64), PARAMETER:: CFC     =4.1868D0           ! Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
!  above is listed in July 1976 publication as "International Table"
REAL(r64), PARAMETER:: CFL     =0.3048D0           ! Length:         FT * CFL = M
REAL(r64), PARAMETER:: CFM     =0.45359237D0       ! Mass:           LB * CFM = KG
!REAL(r64), PARAMETER:: CFP     =249.082D0          ! Pressure:       IN-H2O * CFP = N/M**2
! above is listed in July 1976 publication as in-water at 39.2 deg F
REAL(r64), PARAMETER:: CFP     =248.84D0           ! Pressure:       IN-H2O * CFP = N/M**2
!  above is listed in July 1976 publication as in-water at 60 deg F
REAL(r64), PARAMETER:: DELTMP  =-32.D0             ! Temperature:    (F + DELTMP) * CFT = C
REAL(r64), PARAMETER:: CFA     =CFL*CFL            ! Area:           FT**2 * CFA = M**2
REAL(r64), PARAMETER:: CFT     =5.D0/9.D0          ! Temperature:    R * CFT = K
REAL(r64), PARAMETER:: CFV     =CFA*CFL            ! Volume:         FT**3 * CFV = M**3
REAL(r64), PARAMETER:: CFE     =CFC*CFM*CFT/3.6D0  ! Energy:         BTU * CFE = W-HR
REAL(r64), PARAMETER:: CFD     =CFM/CFV            ! Density:        LB/FT**3 * CFD = KG/M**3
REAL(r64), PARAMETER:: CFH     =CFC*CFT            ! Enthalpy:       BTU/LB * CFH = J/KG
REAL(r64), PARAMETER:: CFK     =CFE/(CFL*CFT)      ! Conductivity:   BTU/(HR*FT*R) * CFK = W/(M*K)
REAL(r64), PARAMETER:: CFMF    =CFM/3600.D0        ! Mass Flow:      LB/HR * CFMF = KG/SEC
REAL(r64), PARAMETER:: CFQ     =CFE                ! Power:          BTU/HR * CFQ = W
REAL(r64), PARAMETER:: CFU     =CFK/CFL            ! U-Value:        BTU/(HR*FT**2*R) * CFU = W/(M**2*K)
                                                          ! Note:  R-Value = 1/U-Value
REAL(r64), PARAMETER:: CFS     =CFL/60.D0          ! Speed:          FT/MIN * CFS = M/SEC
REAL(r64), PARAMETER:: CFVF    =CFV/60.D0          ! Volume Flow:    FT**3/MIN * CFVF = M**3/SEC
REAL(r64), PARAMETER:: CFHF    =CFQ/CFA            ! Heat Flux:      BTU/(HR*FT**2) * CFHF = W/M**2
REAL(r64), PARAMETER:: CFTMP   =DELTMP             ! Temperature:    Same as DELTMP

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


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

END MODULE DataConversions
