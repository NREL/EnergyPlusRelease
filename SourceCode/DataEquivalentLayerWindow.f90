MODULE DataWindowEquivalentLayer
          ! MODULE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module for equivalent layer window model.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC

! CFSTY: Complex Fenestration System
INTEGER, PARAMETER :: CFSMAXNL = 6  ! max # of glaze or shade layers
! Long-wave (aka LW or thermal) layer properties
TYPE CFSLWP
    REAL(r64) :: EPSLF       =0.0d0    ! thermal emittance, front (outside) side
    REAL(r64) :: EPSLB       =0.0d0    ! thermal emittance, back (inside) side
    REAL(r64) :: TAUL       =0.0d0    ! thermal transmittance (same value for front or back)
END TYPE CFSLWP
! Short wave (aka SW or solar) layer properties
TYPE CFSSWP
    REAL(r64) :: RHOSFBB   =0.0d0    ! Solar reflectance, BEAM-BEAM, front (outside) side (any angle of incidence)
    REAL(r64) :: RHOSBBB   =0.0d0    ! Solar reflectance, BEAM-BEAM, back (inside) side (any angle of incidence)
    REAL(r64) :: TAUSFBB   =0.0d0    ! Solar transmittance, BEAM-BEAM, any angle of incidence
                                     ! radiation incident from front (outside)
    REAL(r64) :: TAUSBBB   =0.0d0    ! Solar transmittance, BEAM-BEAM, any angle of incidence
                                     !    radiation incident from back (inside)
    REAL(r64) :: RHOSFBD   =0.0d0    ! Solar reflectance, BEAM-DIFFUSE, front (outside) side
                                     !    BEAM-DIFFUSE, any angle of incidence
    REAL(r64) :: RHOSBBD   =0.0d0    ! Solar reflectance, BEAM-DIFFUSE, back (inside) side
                                     !    any angle of incidence
    REAL(r64) :: TAUSFBD   =0.0d0    ! Solar transmittance, BEAM-DIFFUSE, front (outside) side
                                !    any angle of incidence
    REAL(r64) :: TAUSBBD   =0.0d0    ! Solar transmittance, BEAM-DIFFUSE, any angle of incidence
    REAL(r64) :: RHOSFDD   =0.0d0    ! Solar reflectance, DIFFUSE-DIFFUSE, front (outside) side
    REAL(r64) :: RHOSBDD   =0.0d0    ! Solar reflectance, DIFFUSE-DIFFUSE, back (inside) side
    REAL(r64) :: TAUS_DD   =0.0d0    ! Solar transmittance, DIFFUSE-DIFFUSE
                                !    (same value for radiation incident from front or back)
END TYPE CFSSWP
! "black" room (no reflection)
TYPE( CFSSWP),SAVE :: SWP_ROOMBLK = CFSSWP  &
   ( 0.0d0,  &  ! Solar reflectance, BEAM-BEAM, front
     0.0d0,  &  ! Solar reflectance, BEAM-BEAM, back
     0.0d0,  &  ! Solar transmittance, BEAM-BEAM, front
     0.0d0,  &  ! Solar transmittance, BEAM-BEAM, back
     0.0d0,  &  ! Solar reflectance, BEAM-DIFFUSE, front
     0.0d0,  &  ! Solar reflectance, BEAM-DIFFUSE, back
     0.0d0,  &  ! Solar transmittance, BEAM-DIFFUSE, front
     0.0d0,  &  ! Solar transmittance, BEAM-DIFFUSE, back
     0.0d0,  &  ! Solar reflectance, DIFFUSE-DIFFUSE, front
     0.0d0,  &  ! Solar reflectance, DIFFUSE-DIFFUSE, back
     0.0d0)     ! Solar transmittance, DIFFUSE-DIFFUSE
! Layer information
TYPE CFSLAYER
    CHARACTER(len=MaxNameLength)::Name =' '! ID of layer
    INTEGER       :: LTYPE    =0 ! layer type (see ltyXXX above)
    INTEGER       :: iGZS     =0 ! re spectral glazing
                                 !   = GZSTbl idx of LTYPE=ltyGZS (spectral glazing)
                                 !   else 0
    ! material properties
                                 !  ltyGLAZE, ltyROLLB: as measured
    TYPE( CFSSWP) :: SWP_MAT     !  ltyGZS: derived from GSZ file data
    TYPE( CFSLWP) :: LWP_MAT     !  ltyVBxxx = slat properties (diffuse only)
                                 !   short wave (solar)
                                 !   long wave (thermal)

                                 ! equivalent layer properties (see FinalizeCFSLAYER())
                                 !  = diff + direct-normal properties for pseudo flat layer
    TYPE( CFSSWP) :: SWP_EL      !  ltyGLAZE, ltyGZS, ltyROLLB: same as _MAT
    TYPE( CFSLWP) :: LWP_EL      !  ltyVBxxx: see VB_xxx()
                                 !   short wave (solar)
                                 !   long wave (thermal)

    ! Shade Geometry (Slat, Drape, Insect Screen)
    REAL(r64)     :: S         =0.0d0 ! spacing
                                 !    VB: slat spacing, m, >0
                                 !    PD: rectangular pleat spacing, m >0
                                 !    IS: wire center-to-center spacing (pitch), m, >0
                                 !    else unused
    REAL(r64)     :: W         =0.0d0 ! width
                                 !    VB: slat tip-to-tip (chord width), m, >0
                                 !        if crown > 0, W < slat flattened width
                                 !    PD: pleat depth, m >= 0
                                 !    IS: wire diameter, m, >0, <S
    REAL(r64)     :: C         =0.0d0 ! crown
                                 !    VB: slat crown, m >=0 if used
                                 !    crown assume upward for ltyVBHOR else unused
    REAL(r64)     :: PHI_DEG =0.0d0 ! Angle
                                 !    VB: slat angle, degrees (-90 <= PHI_DEG <= 90)
                                 !        ltyVBHOR: + = front-side slat tip below horizontal
                                 !        ltyVBVER: + = front-side slat tip is counter-
                                 !                     clockwise from normal (viewed from above)
                                 !    else unused
    ! shade control method
    INTEGER       :: CNTRL    =0 !    VB: lscNONE:   PHI_DEG not changed
                                 !        lscVBPROF: PHI_DEG = profile angle (max gain)
                                 !        lscVBNOBM: exclude beam (max visibility w/o beam)
                                 !                   PHI_DEG altered to just exclude beam
                                 !                   PHI_DEG = 20 if diffuse only
END TYPE CFSLAYER
! Gap Gas Properties
TYPE CFSFILLGAS
    CHARACTER(len=MaxNameLength) ::Name=' ' ! Gas Type (AIR, ARGON, XENON, KRYPTON, CUSTOM)
    !Gas Conductivity: K = AK + BK*T + CK*T*T
    REAL(r64):: AK            =0.0d0    !   conductivity coeff constant term,  (W/m-K)
    REAL(r64):: BK             =0.0d0    !   conductivity coeff of T term, (W/m-K2)
    REAL(r64):: CK              =0.0d0  !   conductivity coeff of T^2 term, (W/m-K^3)
    ! Gas Specific heat: CP = ACP + BCP*T + CCP*T*T
    REAL(r64):: ACP              =0.0d0  ! specific heat constant term, (J/kg-K)
    REAL(r64):: BCP              =0.0d0  ! specific heat coeff of T term, (J/kg-K^2)
    REAL(r64):: CCP              =0.0d0  ! specific heat coeff of T^2 term, (J/kg-K^3)
    !Gas Viscosity: Visc = AVISC + BVISC*T + CVISC*T*T
    REAL(r64):: AVISC         =0.0d0    ! viscosity constant term, (N-sec/m2)
    REAL(r64):: BVISC         =0.0d0  ! viscosity coeff of T term, (N-sec/m2-K)
    REAL(r64):: CVISC         =0.0d0  ! viscosity coeff of T^2 term, (N-sec/m2-K^2)
    REAL(r64):: MHAT          =0.0d0    ! apparent molecular weight of gas
END TYPE CFSFILLGAS
! Gap information
TYPE CFSGAP
    CHARACTER(len=MaxNameLength):: Name=' ' ! Gap layer name
    INTEGER          :: GTYPE  =0   ! gap type (gtyXXX above)
    REAL(r64)        :: TAS    =0.0d0 ! actual surface-surface gap thickness, mm (always > 0)
                                    !   VB: minimum tip-surface distance (slats normal to CFS plane)
    REAL(r64)        :: TAS_EFF=0.0d0 ! effective gap thickness, mm (always > 0)
                                    !   if either adjacent layer is VB adjusted
                                    !   slat angle and convective behavior
                                    !   else = TAS
    TYPE( CFSFILLGAS):: FG          ! fill gas properties (see above)
    REAL(r64)        :: RHOGAS =0.0d0 ! fill gas density (kg/m3)
END TYPE CFSGAP
! Equivalent Layer Window Constructon
TYPE CFSTY
    CHARACTER(len=MaxNameLength):: Name=' '! ID (Fenestration Name)
    INTEGER                     :: NL  =0  ! number of layers
    TYPE(CFSLAYER)  :: L( CFSMAXNL)        ! layer array, L(1) is outside layer
    TYPE(CFSGAP)    :: G(CFSMAXNL-1)       ! gap array, G(1) is outside-most, betw L(1) and L(2)
    LOGICAL         :: ISControlled    = .FALSE. ! CFS is not controlled, or has no controlled VB layer
END TYPE CFSTY
! CFSLAYER: layer types
INTEGER, PARAMETER :: ltyNONE   = 0    ! unused / empty layer
INTEGER, PARAMETER :: ltyGLAZE  = 1    ! glazing layer i.e, purely specular
INTEGER, PARAMETER :: ltyDRAPE  = 2    ! pleated drapes/curtains
INTEGER, PARAMETER :: ltyROLLB  = 3    ! roller blind
INTEGER, PARAMETER :: ltyVBHOR  = 4    ! venetian blinds - horizontal
INTEGER, PARAMETER :: ltyVBVER  = 5    ! venetian blinds - vertical
INTEGER, PARAMETER :: ltyINSCRN = 6    ! insect screen
INTEGER, PARAMETER :: ltyROOM   = 7    ! indoor space and/or make no adjustment
INTEGER, PARAMETER :: ltyGZS    = 8    ! glazing with spectral data (read from aux file)
! index for solar arrays
INTEGER, PARAMETER :: isDIFF = 1
INTEGER, PARAMETER :: isBEAM = 2
! Defined CFSLayers and CFSs
TYPE( CFSLAYER),    ALLOCATABLE,       DIMENSION(:) :: CFSLayers
TYPE( CFSTY),       ALLOCATABLE,       DIMENSION(:) :: CFS
TYPE( CFSGAP),      ALLOCATABLE,       DIMENSION(:) :: CFSGaps
INTEGER :: TotWinEquivLayerConstructs = 0 ! Number of constructions with Window equivalent Layer

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

END MODULE DataWindowEquivalentLayer
