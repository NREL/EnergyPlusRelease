MODULE HeatBalanceHAMTManager


  ! MODULE INFORMATION:
  !       AUTHOR      Phillip Biddulph
  !       DATE WRITTEN   June 2008
  !       MODIFIED
  !       Bug fixes to make sure HAMT can cope with data limits  ! PDB August 2009
  !       RE-ENGINEERED
  !


  ! PURPOSE OF THIS MODULE:
  ! Calculate, record and report the one dimentional heat and moisture transfer
  ! through a surface given the material composition of the building surface and
  ! the external and internal Temperatures and Relative Humidities.

  ! METHODOLOGY EMPLOYED:
  ! Each surface is split into "cells", where all characteristics are initiallised.
  ! Cells are matched and links created in the initialisation routine.
  ! The internal and external "surfaces" of the surface are virtual cells to allow for the
  ! input of heat and vapor via heat transfer coefficients, radiation,
  ! and vapor transfer coefficients
  ! Uses Forward (implicit) finite difference alogorithm. Heat transfer is caclulated first,
  ! with the option of including the latent heat, then liquid and vapor transfer. The process is ittereated.
  ! Once the temperatures have converged the internal surface
  ! temperature and vapor densities are passed back to EnergyPlus.

  ! Temperatures and relative humidities are updated once EnergyPlus has checked that
  ! the zone temperatures have converged.

  ! REFERENCES:
  ! K?zel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components.
  ! One- and two-dimensional calculation using simple parameters. IRB Verlag 1995
  !
  ! Holman, J.P. (2002) Heat Transfer, Ninth Edition. McGraw-Hill
  !
  ! Winterton, R.H.S. (1997) Heat Transfer. (Oxford Chemistry Primers; 50) Oxford University Press
  !
  ! Kumar Kumaran, M. (1996) IEA ANNEX 24, Final Report, Volume 3


  ! USE STATEMENTS:

  USE DataGlobals
  USE DataMoistureBalance
  USE DataSurfaces
  USE DataHeatBalSurface, ONLY: QRadSWOutAbs,QRadSWInAbs,NetLWRadToSurf,MaxSurfaceTempLimit,MaxSurfaceTempLimitBeforeFatal, &
                                MinSurfaceTempLimit,MinSurfaceTempLimitBeforeFatal
  USE DataHeatBalance
  USE Psychrometrics
  USE DataHeatBalFanSys, ONLY: Mat,QHtRadSysSurf, QHWBaseboardSurf, QSteamBaseboardSurf, QElecBaseboardSurf
  USE DataEnvironment, ONLY: SkyTemp,  SunIsUp, OutBaroPress, OutEnthalpy, IsRain
  USE DataInterfaces


  IMPLICIT NONE ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: ittermax=150 ! Maximum Number of itterations
  INTEGER, PARAMETER ::  adjmax=6    ! Maximum Number of Adjacent Cells

  REAL(r64), PARAMETER :: wdensity=1000.d0   ! Density of water kg.m-3
  REAL(r64), PARAMETER :: wspech=4180.d0     ! Specific Heat Capacity of Water J.kg-1.K-1 (at 20C)
  REAL(r64), PARAMETER :: whv=2489000.d0     ! Evaporation enthalpy of water J.kg-1
  REAL(r64), PARAMETER :: convt=0.002d0     ! Temperature convergence limit
  REAL(r64), PARAMETER :: qvplim=100000.d0   ! Maximum latent heat W
  REAL(r64), PARAMETER :: rhmax=1.01d0      ! Maximum RH value

  ! DERIVED TYPE DEFINITIONS:
  TYPE :: subcell

     INTEGER :: matid=-1                  ! Material Id Number
     INTEGER :: sid=-1                    ! Surface Id Number
     REAL(r64) :: Qadds=0.0d0            ! Additional sources of heat
     REAL(r64) :: density=-1.0d0         ! Density
     REAL(r64) :: wthermalc=0.0d0        ! Moisture Dependant Thermal Conductivity
     REAL(r64) :: spech=0.0d0            ! Specific Heat capacity
     REAL(r64) :: htc=-1.0d0             ! Heat Transfer Coefficient
     REAL(r64) :: vtc=-1.0d0             ! Vapor Transfer Coefficient
     REAL(r64) :: mu=-1.0d0              ! Vapor Diffusion resistance Factor
     REAL(r64) :: volume=0.0d0           ! Cell Volume
     REAL(r64) :: temp=0.0d0
     REAL(r64) :: tempp1=0.0d0
     REAL(r64) :: tempp2=0.0d0
     REAL(r64) :: wreport=0.0d0          ! Water content for reporting
     REAL(r64) :: water=0.0d0            ! Water Content of cells
     REAL(r64) :: vp=0.0d0               ! Vapor Pressure
     REAL(r64) :: vpp1=0.0d0             ! Vapor Pressure
     REAL(r64) :: vpsat=0.0d0            ! Saturation Vapor Pressure
     REAL(r64) :: rh=0.1d0
     REAL(r64) :: rhp1=0.1d0
     REAL(r64) :: rhp2=0.1d0             ! Relative Humidity
     REAL(r64) :: rhp=10.d0              ! cell relative humidity (percent - reporting)
     REAL(r64) :: dwdphi=-1.0d0                  ! Moisture storage capacity
     REAL(r64) :: dw=-1.0d0                      ! Liquid transport Coefficient
     REAL(r64), DIMENSION(3) :: origin=0.0d0 ! Cell origin. The geometric centre of the cell.
     REAL(r64), DIMENSION(3) :: length=0.0d0 ! Cell lengths
     REAL(r64), DIMENSION(6) :: overlap=0.0d0 ! Area of overlap
     REAL(r64), DIMENSION(6) :: dist=0.0d0   ! distance between cell origins
     INTEGER, DIMENSION(6) :: adjs=0
     INTEGER, DIMENSION(6) :: adjsl=0
  END TYPE subcell

  TYPE (subcell), DIMENSION(:), ALLOCATABLE :: cells


  ! MODULE VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(:), ALLOCATABLE :: firstcell
  INTEGER, DIMENSION(:), ALLOCATABLE :: lastcell
  INTEGER, DIMENSION(:), ALLOCATABLE :: Extcell
  INTEGER, DIMENSION(:), ALLOCATABLE :: ExtRadcell
  INTEGER, DIMENSION(:), ALLOCATABLE :: ExtConcell
  INTEGER, DIMENSION(:), ALLOCATABLE :: ExtSkycell
  INTEGER, DIMENSION(:), ALLOCATABLE :: ExtGrncell
  INTEGER, DIMENSION(:), ALLOCATABLE :: Intcell
  INTEGER, DIMENSION(:), ALLOCATABLE :: IntConcell

  REAL(r64), DIMENSION(:), ALLOCATABLE :: watertot
  REAL(r64), DIMENSION(:), ALLOCATABLE :: surfrh
  REAL(r64), DIMENSION(:), ALLOCATABLE :: surfextrh
  REAL(r64), DIMENSION(:), ALLOCATABLE :: surftemp
  REAL(r64), DIMENSION(:), ALLOCATABLE :: surfexttemp
  REAL(r64), DIMENSION(:), ALLOCATABLE :: surfvp

  REAL(r64), DIMENSION(:), ALLOCATABLE :: extvtc  ! External Surface vapor transfer coefficient
  REAL(r64), DIMENSION(:), ALLOCATABLE :: intvtc  ! Internal Surface Vapor Transfer Coefficient
  LOGICAL, DIMENSION(:), ALLOCATABLE :: extvtcflag  ! External Surface vapor transfer coefficient flag
  LOGICAL, DIMENSION(:), ALLOCATABLE :: intvtcflag  ! Internal Surface Vapor Transfer Coefficient flag
  LOGICAL, DIMENSION(:), ALLOCATABLE :: MyEnvrnFlag ! Flag to reset surface properties.



  REAL(r64) :: deltat=0.0d0            ! time step in seconds

  INTEGER :: TotCellsMax=0         ! Maximum number of cells per material

  LOGICAL :: latswitch=.false. ! latent heat switch,
  LOGICAL :: rainswitch=.false. ! rain switch,


  ! SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceHAMTManager:

  PUBLIC ManageHeatBalHAMT
  PUBLIC UpdateHeatBalHAMT

  PRIVATE GetHeatBalHAMTInput
  PRIVATE InitHeatBalHAMT
  PRIVATE CalcHeatBalHAMT

CONTAINS
  SUBROUTINE ManageHeatBalHAMT(SurfNum,TempSurfInTmp,TempSurfOutTmp)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Manages the Heat and Moisture Transfer calculations.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(In) :: SurfNum
    REAL(r64), INTENT(inout) :: TempSurfInTmp
    REAL(r64), INTENT(inout) :: TempSurfOutTmp


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    LOGICAL,SAVE :: OneTimeFlag = .TRUE.

    IF(OneTimeFlag)THEN
       OneTimeFlag=.FALSE.
       CALL DisplayString('Initialising Heat and Moisture Transfer Model')
       CALL GetHeatBalHAMTInput
       CALL InitHeatBalHAMT
    ENDIF

    CALL CalcHeatBalHAMT(SurfNum,TempSurfInTmp,TempSurfOutTmp)

  END SUBROUTINE ManageHeatBalHAMT

  SUBROUTINE GetHeatBalHAMTInput

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! gets input for the HAMT model

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE InputProcessor


    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: cHAMTObject1='MaterialProperty:HeatAndMoistureTransfer:Settings'
    CHARACTER(len=*), PARAMETER :: cHAMTObject2='MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm'
    CHARACTER(len=*), PARAMETER :: cHAMTObject3='MaterialProperty:HeatAndMoistureTransfer:Suction'
    CHARACTER(len=*), PARAMETER :: cHAMTObject4='MaterialProperty:HeatAndMoistureTransfer:Redistribution'
    CHARACTER(len=*), PARAMETER :: cHAMTObject5='MaterialProperty:HeatAndMoistureTransfer:Diffusion'
    CHARACTER(len=*), PARAMETER :: cHAMTObject6='MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity'
    CHARACTER(len=*), PARAMETER :: cHAMTObject7='SurfaceProperties:VaporCoefficients'

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


    CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: AlphaArray
    CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: cAlphaFieldNames
    CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: cNumericFieldNames

    LOGICAL,DIMENSION(:),ALLOCATABLE :: lAlphaBlanks
    LOGICAL,DIMENSION(:),ALLOCATABLE :: lNumericBlanks

    REAL(r64),DIMENSION(:),ALLOCATABLE ::  NumArray

    REAL(r64) :: dumrh
    REAL(r64) :: dumdata
    REAL(r64) :: avdata

    INTEGER :: MaxNums
    INTEGER :: MaxAlphas
    INTEGER :: NumParams
    INTEGER :: NumNums
    INTEGER :: NumAlphas
    INTEGER :: status
    INTEGER :: matid
    INTEGER :: iso
    INTEGER :: Numid
    INTEGER :: suc
    INTEGER :: red
    INTEGER :: mu
    INTEGER :: tc
!unused1208    INTEGER :: sid
    INTEGER :: HAMTitems
    INTEGER :: item
    INTEGER :: ii
    INTEGER :: jj
    INTEGER :: vtcsid

    LOGICAL :: avflag
    LOGICAL :: isoerrrise
    LOGICAL :: ErrorsFound

    ALLOCATE(watertot(TotSurfaces))
    ALLOCATE(surfrh(TotSurfaces))
    ALLOCATE(surfextrh(TotSurfaces))
    ALLOCATE(surftemp(TotSurfaces))
    ALLOCATE(surfexttemp(TotSurfaces))
    ALLOCATE(surfvp(TotSurfaces))

    ALLOCATE(firstcell(TotSurfaces))
    ALLOCATE(lastcell(TotSurfaces))
    ALLOCATE(Extcell(TotSurfaces))
    ALLOCATE(ExtRadcell(TotSurfaces))
    ALLOCATE(ExtConcell(TotSurfaces))
    ALLOCATE(ExtSkycell(TotSurfaces))
    ALLOCATE(ExtGrncell(TotSurfaces))
    ALLOCATE(Intcell(TotSurfaces))
    ALLOCATE(IntConcell(TotSurfaces))

    ALLOCATE(extvtc(TotSurfaces))
    ALLOCATE(intvtc(TotSurfaces))
    ALLOCATE(extvtcflag(TotSurfaces))
    ALLOCATE(intvtcflag(TotSurfaces))
    ALLOCATE(MyEnvrnFlag(TotSurfaces))

    extvtc=-1.0d0
    intvtc=-1.0d0
    extvtcflag=.FALSE.
    intvtcflag=.FALSE.
    MyEnvrnFlag=.TRUE.

    latswitch=.TRUE.
    rainswitch=.TRUE.

    MaxAlphas=0
    MaxNums=0
    CALL GetObjectDefMaxArgs(cHAMTObject1,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
    CALL GetObjectDefMaxArgs(cHAMTObject2,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
    CALL GetObjectDefMaxArgs(cHAMTObject3,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
    CALL GetObjectDefMaxArgs(cHAMTObject4,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
    CALL GetObjectDefMaxArgs(cHAMTObject5,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
    CALL GetObjectDefMaxArgs(cHAMTObject6,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
    CALL GetObjectDefMaxArgs(cHAMTObject7,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)

    ErrorsFound=.false.

    ALLOCATE(AlphaArray(MaxAlphas))
    AlphaArray=' '
    ALLOCATE(cAlphaFieldNames(MaxAlphas))
    cAlphaFieldNames=' '
    ALLOCATE(cNumericFieldNames(MaxNums))
    cNumericFieldNames=' '
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0d0
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.false.
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.false.

    HAMTitems=GetNumObjectsFound(cHAMTObject1)  ! MaterialProperty:HeatAndMoistureTransfer:Settings
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject1,item,AlphaArray,NumAlphas,NumArray,NumNums,status,   &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       matid=FindItemInList(AlphaArray(1),Material%Name,TotMaterials)

       IF (matid == 0) THEN
          CALL ShowSevereError(cHAMTObject1//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF
       IF (Material(matid)%ROnly) THEN
          CALL ShowWarningError(cHAMTObject1//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is defined as an R-only value material.')
          CYCLE
       ENDIF

       Material(matid)%Porosity=NumArray(1)
       Material(matid)%iwater=NumArray(2)

    ENDDO

    HAMTitems=GetNumObjectsFound(cHAMTObject2) ! MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject2,item,AlphaArray,NumAlphas,NumArray,NumNums,status,  &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       matid=FindItemInList(AlphaArray(1),Material%Name,TotMaterials)

       IF (matid == 0) THEN
          CALL ShowSevereError(cHAMTObject2//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF
       IF (Material(matid)%ROnly) THEN
          CALL ShowWarningError(cHAMTObject2//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is defined as an R-only value material.')
          CYCLE
       ENDIF

       Numid=1

       Material(matid)%niso=INT(NumArray(Numid))

       DO iso=1,Material(matid)%niso
          Numid=Numid+1
          Material(matid)%isorh(iso)=NumArray(Numid)
          Numid=Numid+1
          Material(matid)%isodata(iso)=NumArray(Numid)
       ENDDO

       Material(matid)%niso=Material(matid)%niso+1
       Material(matid)%isorh(Material(matid)%niso)=rhmax
       Material(matid)%isodata(Material(matid)%niso)=Material(matid)%Porosity*wdensity

       Material(matid)%niso=Material(matid)%niso+1
       Material(matid)%isorh(Material(matid)%niso)=0.0d0
       Material(matid)%isodata(Material(matid)%niso)=0.0d0
    ENDDO



    ! check the isotherm
    DO matid=1,TotMaterials
       IF(Material(matid)%niso>0)THEN
          ! - First sort
          DO jj=1,Material(matid)%niso-1
             DO ii=jj+1,Material(matid)%niso
                IF(Material(matid)%isorh(jj)>Material(matid)%isorh(ii))THEN

                   dumrh=Material(matid)%isorh(jj)
                   dumdata=Material(matid)%isodata(jj)

                   Material(matid)%isorh(jj)=Material(matid)%isorh(ii)
                   Material(matid)%isodata(jj)=Material(matid)%isodata(ii)

                   Material(matid)%isorh(ii)=dumrh
                   Material(matid)%isodata(ii)=dumdata


                ENDIF
             ENDDO
          ENDDO
          !- Now make sure the data rises
          isoerrrise=.FALSE.
          DO ii=1,100
             avflag=.TRUE.
             DO jj=1,Material(matid)%niso-1
                IF(Material(matid)%isodata(jj)>Material(matid)%isodata(jj+1))THEN
                   isoerrrise=.TRUE.
                   avdata=(Material(matid)%isodata(jj)+Material(matid)%isodata(jj+1))/2.0d0
                   Material(matid)%isodata(jj)=avdata
                   Material(matid)%isodata(jj+1)=avdata
                   avflag=.FALSE.
                ENDIF
             ENDDO
             IF(avflag)EXIT
          ENDDO
          IF(isoerrrise)THEN
             CALL ShowWarningError(cHAMTObject2//' data not rising - Check material '//TRIM(Material(matid)%Name))
             Call ShowContinueError('Isotherm data has been fixed, and the simulation continues.')
          ENDIF
       ENDIF
    ENDDO


    HAMTitems=GetNumObjectsFound(cHAMTObject3)  ! MaterialProperty:HeatAndMoistureTransfer:Suction
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject3,item,AlphaArray,NumAlphas,NumArray,NumNums,status,  &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       matid=FindItemInList(AlphaArray(1),Material%Name,TotMaterials)

       IF (matid == 0) THEN
          CALL ShowSevereError(cHAMTObject3//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF
       IF (Material(matid)%ROnly) THEN
          CALL ShowWarningError(cHAMTObject3//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is defined as an R-only value material.')
          CYCLE
       ENDIF

       Numid=1

       Material(matid)%nsuc=NumArray(Numid)
       DO suc=1,Material(matid)%nsuc
          Numid=Numid+1
          Material(matid)%sucwater(suc)=NumArray(Numid)
          Numid=Numid+1
          Material(matid)%sucdata(suc)=NumArray(Numid)
       ENDDO

       Material(matid)%nsuc=Material(matid)%nsuc+1
       Material(matid)%sucwater(Material(matid)%nsuc)=Material(matid)%isodata(Material(matid)%niso)
       Material(matid)%sucdata(Material(matid)%nsuc)=Material(matid)%sucdata(Material(matid)%nsuc-1)

    ENDDO


    HAMTitems=GetNumObjectsFound(cHAMTObject4)  ! MaterialProperty:HeatAndMoistureTransfer:Redistribution
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject4,item,AlphaArray,NumAlphas,NumArray,NumNums,status,  &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       matid=FindItemInList(AlphaArray(1),Material%Name,TotMaterials)
       IF (matid == 0) THEN
          CALL ShowSevereError(cHAMTObject4//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF
       IF (Material(matid)%ROnly) THEN
          CALL ShowWarningError(cHAMTObject4//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is defined as an R-only value material.')
          CYCLE
       ENDIF
       Numid=1

       Material(matid)%nred=NumArray(Numid)
       DO red=1,Material(matid)%nred
          Numid=Numid+1
          Material(matid)%redwater(red)=NumArray(Numid)
          Numid=Numid+1
          Material(matid)%reddata(red)=NumArray(Numid)
       ENDDO

       Material(matid)%nred=Material(matid)%nred+1
       Material(matid)%redwater(Material(matid)%nred)=Material(matid)%isodata(Material(matid)%niso)
       Material(matid)%reddata(Material(matid)%nred)=Material(matid)%reddata(Material(matid)%nred-1)

    ENDDO


    HAMTitems=GetNumObjectsFound(cHAMTObject5)  ! MaterialProperty:HeatAndMoistureTransfer:Diffusion
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject5,item,AlphaArray,NumAlphas,NumArray,NumNums,status,  &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       matid=FindItemInList(AlphaArray(1),Material%Name,TotMaterials)
       IF (matid == 0) THEN
          CALL ShowSevereError(cHAMTObject5//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF
       IF (Material(matid)%ROnly) THEN
          CALL ShowWarningError(cHAMTObject5//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is defined as an R-only value material.')
          CYCLE
       ENDIF

       Numid=1

       Material(matid)%nmu=NumArray(Numid)
       IF(Material(matid)%nmu>0)THEN
          DO mu=1,Material(matid)%nmu
             Numid=Numid+1
             Material(matid)%murh(mu)=NumArray(Numid)
             Numid=Numid+1
             Material(matid)%mudata(mu)=NumArray(Numid)
          ENDDO

          Material(matid)%nmu=Material(matid)%nmu+1
          Material(matid)%murh(Material(matid)%nmu)=Material(matid)%isorh(Material(matid)%niso)
          Material(matid)%mudata(Material(matid)%nmu)=Material(matid)%mudata(Material(matid)%nmu-1)

       ENDIF
    ENDDO


    HAMTitems=GetNumObjectsFound(cHAMTObject6)  ! MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject6,item,AlphaArray,NumAlphas,NumArray,NumNums,status,  &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       matid=FindItemInList(AlphaArray(1),Material%Name,TotMaterials)
       IF (matid == 0) THEN
          CALL ShowSevereError(cHAMTObject6//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF
       IF (Material(matid)%ROnly) THEN
          CALL ShowWarningError(cHAMTObject6//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is defined as an R-only value material.')
          CYCLE
       ENDIF
       Numid=1

       Material(matid)%ntc=NumArray(Numid)
       IF(Material(matid)%ntc>0)THEN
          DO tc=1,Material(matid)%ntc
             Numid=Numid+1
             Material(matid)%tcwater(tc)=NumArray(Numid)
             Numid=Numid+1
             Material(matid)%tcdata(tc)=NumArray(Numid)
          ENDDO

          Material(matid)%ntc=Material(matid)%ntc+1
          Material(matid)%tcwater(Material(matid)%ntc)=Material(matid)%isodata(Material(matid)%niso)
          Material(matid)%tcdata(Material(matid)%ntc)=Material(matid)%tcdata(Material(matid)%ntc-1)

       ENDIF
    ENDDO

    ! Vapor Transfer coefficients
    HAMTitems=GetNumObjectsFound(cHAMTObject7)  ! SurfaceProperties:VaporCoefficients
    DO item=1,HAMTitems
       CALL GetObjectItem(cHAMTObject7,item,AlphaArray,NumAlphas,NumArray,NumNums,status,  &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

       vtcsid=FindItemInList(AlphaArray(1),Surface%Name,TotSurfaces)
       IF(vtcsid==0)THEN
          CALL ShowSevereError(cHAMTObject7//' '//trim(cAlphaFieldNames(1))//'="'//TRIM(AlphaArray(1))//  &
             '" is invalid (undefined).')
          CALL ShowContinueError('The basic material must be defined in addition to specifying '//  &
            'HeatAndMoistureTransfer properties.')
          ErrorsFound=.true.
          CYCLE
       ENDIF

       IF(TRIM(AlphaArray(2)) == 'YES')THEN
          extvtcflag(vtcsid)=.TRUE.
          extvtc(vtcsid)=NumArray(1)
       ENDIF

       IF(TRIM(AlphaArray(3)) == 'YES')THEN
          intvtcflag(vtcsid)=.TRUE.
          intvtc(vtcsid)=NumArray(2)
       ENDIF

    ENDDO

    DEALLOCATE(AlphaArray)
    DEALLOCATE(cAlphaFieldNames)
    DEALLOCATE(cNumericFieldNames)
    DEALLOCATE(NumArray)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(lNumericBlanks)

    IF (ErrorsFound) THEN
      CALL ShowFatalError('GetHeatBalHAMTInput: Errors found getting input.  Program terminates.')
    ENDIF

  END SUBROUTINE GetHeatBalHAMTInput

  SUBROUTINE InitHeatBalHAMT
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       B. Griffith, Aug 2012 for surface-specific algorithms
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! <description>

    ! METHODOLOGY EMPLOYED:
    ! <description>

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE General,        ONLY: TrimSigDigits, ScanForReports, RoundSigDigits



    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    ! na

    ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: adjdist=0.00005d0 ! Allowable distance between two cells, also used as limit on cell length
    CHARACTER(len=*), PARAMETER :: RoutineName='InitCombinedHeatAndMoistureFiniteElement: '


    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER :: ii
    INTEGER :: cid
    INTEGER :: cid1
    INTEGER :: cid2
    INTEGER :: sid
    INTEGER :: conid
    INTEGER :: lid
    INTEGER :: matid
    INTEGER :: did
    INTEGER :: adj1
    INTEGER :: adj2
    INTEGER :: errorCount
    INTEGER :: concell
    INTEGER :: MaterNum


    REAL(r64) :: runor
    REAL(r64) :: high1
    REAL(r64) :: low2
    REAL(r64) :: testlen
    REAL(r64) :: waterd ! water density
    LOGICAL   :: DoReport
    deltat=TimeStepZone*3600.0d0


    ! Check the materials information and work out how many cells are required.
    errorCount=0
    TotCellsMax=0
    DO sid=1,TotSurfaces
       IF (Surface(sid)%Class == SurfaceClass_Window) CYCLE
       IF (Surface(sid)%HeatTransferAlgorithm /= HeatTransferModel_HAMT) CYCLE
        conid=Surface(sid)%Construction
        IF (conid == 0) CYCLE
        DO lid=1,Construct(conid)%TotLayers
           matid=Construct(conid)%LayerPoint(lid)
           IF (Material(matid)%ROnly) THEN
              CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name)//  &
                 ' cannot contain R-only value materials.')
              CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'".')
              errorCount=errorCount+1
              CYCLE
           ENDIF

           IF(Material(matid)%nmu<0)THEN
              CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name))
              CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                ' does not have required Water Vapor Diffusion Resistance Factor (mu) data.')
              errorCount=errorCount+1
           ENDIF

           IF(Material(matid)%niso<0)THEN
              CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name))
              CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                ' does not have required isotherm data.')
              errorCount=errorCount+1
           ENDIF
           IF(Material(matid)%nsuc<0)THEN
              CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name))
              CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                ' does not have required liquid transport coefficient (suction) data.')
              errorCount=errorCount+1
           ENDIF
           IF(Material(matid)%nred<0)THEN
              CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name))
              CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                ' does not have required liquid transport coefficient (redistribution) data.')
              errorCount=errorCount+1
           ENDIF
           IF(Material(matid)%ntc<0)THEN
              IF(Material(matid)%Conductivity>0)THEN
                 CALL ShowWarningError(RoutineName//'Construction='//trim(Construct(conid)%Name))
                 CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                   ' does not have thermal conductivity data. Using fixed value.')
                 Material(matid)%ntc=2
                 Material(matid)%tcwater(1)=0.0d0
                 Material(matid)%tcdata(1)=Material(matid)%Conductivity
                 Material(matid)%tcwater(2)=Material(matid)%isodata(Material(matid)%niso)
                 Material(matid)%tcdata(2)=Material(matid)%Conductivity
              ELSE
                 CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name))
                 CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                  ' does not have required thermal conductivity data.')
                 errorCount=errorCount+1
              ENDIF
           ENDIF

           ! convert material water content to RH

           waterd=Material(matid)%iwater*Material(matid)%density
           CALL interp(Material(matid)%niso,Material(matid)%isodata,Material(matid)%isorh,&
                waterd,Material(matid)%irh)

           Material(matid)%divs=INT(Material(matid)%Thickness/Material(matid)%divsize)+Material(matid)%divmin
           IF(Material(matid)%divs>Material(matid)%divmax) THEN
              Material(matid)%divs=Material(matid)%divmax
           ENDIF
           ! Check length of cell - reduce number of divisions if neccessary
           DO
              testlen=Material(matid)%Thickness* &
               ((SIN(PI*(-1.0d0/REAL(Material(matid)%divs,r64))-PI/2.0d0)/2.0d0)-(SIN(-PI/2.0d0)/2.0d0))
              IF(testlen>adjdist)EXIT
              Material(matid)%divs= Material(matid)%divs-1
              IF(Material(matid)%divs<1)THEN
                 CALL ShowSevereError(RoutineName//'Construction='//trim(Construct(conid)%Name))
                 CALL ShowContinueError('Reference Material="'//TRIM(Material(matid)%Name)//'"'//  &
                   ' is too thin.')
                 errorCount=errorCount+1
                 EXIT
              ENDIF
           ENDDO
           TotCellsMax=TotCellsMax+Material(matid)%divs
        ENDDO
        TotCellsMax=TotCellsMax+7

    ENDDO



    IF(errorCount>0)THEN
       CALL ShowFatalError('CombinedHeatAndMoistureFiniteElement: Incomplete data to start solution, program terminates.')
    ENDIF


    ! Make the cells and initialise
    ALLOCATE(cells(TotCellsMax))
    DO ii=1,adjmax
       cells%adjs(ii)=-1
       cells%adjsl(ii)=-1
    ENDDO

    cid=0

    ! Set up surface cell structure
    DO sid=1,TotSurfaces
      IF (.not. Surface(sid)%HeatTransSurf) CYCLE
      IF (Surface(sid)%Class == SurfaceClass_Window) CYCLE
      IF (Surface(sid)%HeatTransferAlgorithm /= HeatTransferModel_HAMT) CYCLE
      ! Boundary Cells
      runor=-0.02d0
      ! Air Convection Cell
      cid=cid+1
      firstcell(sid)=cid
      ExtConcell(sid)=cid
      cells(cid)%rh=0.0d0
      cells(cid)%sid=sid
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)= cells(cid)%length(1)/2.0d0+runor

      ! Air Radiation Cell
      cid=cid+1
      ExtRadcell(sid)=cid
      cells(cid)%rh=0.0d0
      cells(cid)%sid=sid
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)= cells(cid)%length(1)/2.0d0+runor

      ! Sky Cell
      cid=cid+1
      ExtSkycell(sid)=cid
      cells(cid)%rh=0.0d0
      cells(cid)%sid=sid
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)= cells(cid)%length(1)/2.0d0+runor

      ! Ground Cell
      cid=cid+1
      ExtGrncell(sid)=cid
      cells(cid)%rh=0.0d0
      cells(cid)%sid=sid
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)= cells(cid)%length(1)/2.0d0+runor
      runor=runor+ cells(cid)%length(1)

      ! External Virtual Cell
      cid=cid+1
      Extcell(sid)=cid
      cells(cid)%rh=0.0d0
      cells(cid)%sid=sid
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)= cells(cid)%length(1)/2.0d0+runor
      runor=runor+ cells(cid)%length(1)


      ! Material Cells
      conid=Surface(sid)%Construction
      DO lid=1,Construct(conid)%TotLayers
         matid=Construct(conid)%LayerPoint(lid)

         DO did=1,Material(matid)%divs
            cid=cid+1

            cells(cid)%matid=matid
            cells(cid)%sid=sid

            cells(cid)%temp=Material(matid)%itemp
            cells(cid)%tempp1=Material(matid)%itemp
            cells(cid)%tempp2=Material(matid)%itemp

            cells(cid)%rh=Material(matid)%irh
            cells(cid)%rhp1=Material(matid)%irh
            cells(cid)%rhp2=Material(matid)%irh

            cells(cid)%density=Material(matid)%Density
            cells(cid)%spech=Material(matid)%SpecHeat

            ! Make cells smaller near the surface
            cells(cid)%length(1)=Material(matid)%Thickness* &
                 ((SIN(pi*(-REAL(did,r64)/REAL(Material(matid)%divs,r64))-pi/2.0d0)/2.0d0)- &
                 (SIN(pi*(-REAL(did-1,r64)/REAL(Material(matid)%divs,r64))-pi/2.0d0)/2.0d0))

            cells(cid)%origin(1)=runor+cells(cid)%length(1)/2.0d0
            runor=runor+cells(cid)%length(1)

            cells(cid)%volume=cells(cid)%length(1)*Surface(sid)%Area

         ENDDO
      ENDDO

      ! Interior Virtual Cell
      cid=cid+1
      Intcell(sid)=cid
      cells(cid)%sid=sid
      cells(cid)%rh=0.0d0
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)=cells(cid)%length(1)/2.0d0+runor
      runor=runor+ cells(cid)%length(1)

      ! Air Convection Cell
      cid=cid+1
      lastcell(sid)=cid
      IntConcell(sid)=cid
      cells(cid)%rh=0.0d0
      cells(cid)%sid=sid
      cells(cid)%length(1)=0.01d0
      cells(cid)%origin(1)= cells(cid)%length(1)/2.0d0+runor

    ENDDO


    ! Find adjacent cells.
    DO cid1=1,TotCellsMax
       DO cid2=1,TotCellsMax
          IF((cid1/=cid2).AND.(cells(cid1)%sid==cells(cid2)%sid))THEN
             high1=cells(cid1)%origin(1)+cells(cid1)%length(1)/2.0d0
             low2=cells(cid2)%origin(1)-cells(cid2)%length(1)/2.0d0
             IF(ABS(low2-high1)<adjdist)THEN
                adj1=0
                DO ii=1,adjmax
                   adj1=adj1+1
                   IF(cells(cid1)%adjs(adj1)==-1)EXIT
                END DO
                adj2=0
                DO ii=1,adjmax
                   adj2=adj2+1
                   IF(cells(cid2)%adjs(adj2)==-1)EXIT
                END DO
                cells(cid1)%adjs(adj1)=cid2
                cells(cid2)%adjs(adj2)=cid1

                cells(cid1)%adjsl(adj1)=adj2
                cells(cid2)%adjsl(adj2)=adj1

                sid=cells(cid1)%sid
                cells(cid1)%overlap(adj1)=Surface(sid)%Area
                cells(cid2)%overlap(adj2)=Surface(sid)%Area
                cells(cid1)%dist(adj1)=cells(cid1)%length(1)/2.0d0
                cells(cid2)%dist(adj2)=cells(cid2)%length(1)/2.0d0
             ENDIF
          ENDIF
       ENDDO
    ENDDO

    ! Reset surface virtual cell origins and volumes. Initialise report variables.
    WRITE(OutputFileInits,1966)
1966      FORMAT('! <HAMT cells>, Surface Name, Construction Name, Cell Numbers')
    WRITE(OutputFileInits,1965)
1965      FORMAT('! <HAMT origins>, Surface Name, Construction Name, Cell origins (m) ')
    !cCurrentModuleObject='MaterialProperty:HeatAndMoistureTransfer:*'
    DO sid=1,TotSurfaces
      IF (.not. Surface(sid)%HeatTransSurf) CYCLE
      IF (Surface(sid)%Class == SurfaceClass_Window) CYCLE
      IF (Surface(sid)%HeatTransferAlgorithm /= HeatTransferModel_HAMT) CYCLE
      cells(Extcell(sid))%origin(1)=cells(Extcell(sid))%origin(1)+cells(Extcell(sid))%length(1)/2.0d0
      cells(Intcell(sid))%origin(1)=cells(Intcell(sid))%origin(1)-cells(Intcell(sid))%length(1)/2.0d0
      cells(Extcell(sid))%volume=0.0d0
      cells(Intcell(sid))%volume=0.0d0
      watertot(sid)=0.0d0
      surfrh(sid)=0.0d0
      surfextrh(sid)=0.0d0
      surftemp(sid)=0.0d0
      surfexttemp(sid)=0.0d0
      surfvp(sid)=0.0d0
      CALL SetUpOutputVariable('HAMT Surface Average Water Content Ratio [kg/kg]',watertot(sid),  &
         'Zone','State',Surface(sid)%Name)
      CALL SetUpOutputVariable('HAMT Surface Inside Face Temperature [C]',surftemp(sid),  &
         'Zone','State',Surface(sid)%Name)
      CALL SetUpOutputVariable('HAMT Surface Inside Face Relative Humidity [%]',surfrh(sid),  &
         'Zone','State',Surface(sid)%Name)
      CALL SetUpOutputVariable('HAMT Surface Inside Face Vapor Pressure [Pa]',surfvp(sid),  &
         'Zone','State',Surface(sid)%Name)
      CALL SetUpOutputVariable('HAMT Surface Outside Face Temperature [C]',surfexttemp(sid),  &
         'Zone','State',Surface(sid)%Name)
      CALL SetUpOutputVariable('HAMT Surface Outside Face Relative Humidity [%]',surfextrh(sid),  &
         'Zone','State',Surface(sid)%Name)


      ! write cell origins to initilisation output file
      conid=Surface(sid)%Construction
      WRITE(OutputFileInits,1968) TRIM(Surface(sid)%Name),TRIM(Construct(conid)%Name),  &
         (concell, concell=1,Intcell(sid)-Extcell(sid)+1)
1968      FORMAT('HAMT cells, ',A,',',A,400(:,',',i4))
      WRITE(OutputFileInits,1967) TRIM(Surface(sid)%Name),TRIM(Construct(conid)%Name),  &
         cells(Extcell(sid):Intcell(sid))%origin(1)
1967      FORMAT('HAMT origins,',A,',',A,400(:,',',f10.7))

      concell=1
      DO cid=Extcell(sid),Intcell(sid)
         CALL SetUpOutputVariable( &
              'HAMT Surface Temperature Cell ' &
              //TRIM(TrimSigDigits(concell))//' [C]', &
              cells(cid)%temp,'Zone','State',Surface(sid)%Name)
         concell=concell+1
      ENDDO
      concell=1
      DO cid=Extcell(sid),Intcell(sid)
         CALL SetUpOutputVariable( &
              'HAMT Surface Water Content Cell ' &
              //TRIM(TrimSigDigits(concell))//' [kg/kg]', &
              cells(cid)%wreport,'Zone','State',Surface(sid)%Name)
         concell=concell+1
      ENDDO
      concell=1
      DO cid=Extcell(sid),Intcell(sid)
         CALL SetUpOutputVariable( &
              'HAMT Surface Relative Humidity Cell ' &
              //TRIM(TrimSigDigits(concell))//' [%]', &
              cells(cid)%rhp,'Zone','State',Surface(sid)%Name)
         concell=concell+1
      ENDDO
    ENDDO

    CALL ScanForReports('Constructions',DoReport,'Constructions')
    IF (DoReport) THEN

      WRITE(OutputFileInits,108)

      Do MaterNum=1,TotMaterials

         WRITE(OutputFileInits,111) Trim(Material(MaterNum)%Name),TRIM(RoundSigDigits(NominalR(MaterNum),4))

      end do

    End If

    108 FORMAT('! <Material Nominal Resistance>, Material Name,  Nominal R')
    111 FORMAT('Material Nominal Resistance',2(',',A))

  END SUBROUTINE InitHeatBalHAMT

  SUBROUTINE CalcHeatBalHAMT(sid,TempSurfInTmp,TempSurfOutTmp)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! To calculate the heat and moisture transfer through the surface

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE General, ONLY: RoundSigDigits
    USE DataSurfaces, ONLY: OtherSideCondModeledExt, OSCM

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(in) :: sid
    REAL(r64), INTENT(inout) :: TempSurfInTmp
    REAL(r64), INTENT(inout) :: TempSurfOutTmp


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TempSurfInP
    REAL(r64) :: RhoIn
    REAL(r64) :: RhoOut
    REAL(r64) :: torsum
    REAL(r64) :: oorsum
    REAL(r64) :: phioosum
    REAL(r64) :: phiorsum
    REAL(r64) :: vpoosum
    REAL(r64) :: vporsum
    REAL(r64) :: rhr1
    REAL(r64) :: rhr2
    REAL(r64) :: wcap
    REAL(r64) :: thermr1
    REAL(r64) :: thermr2
    REAL(r64) :: tcap
    REAL(r64) :: qvp
    REAL(r64) :: vaporr1
    REAL(r64) :: vaporr2
    REAL(r64) :: vpdiff
    REAL(r64) :: sumtp1
    REAL(r64) :: tempmax
    REAL(r64) :: tempmin

    INTEGER :: ii
    INTEGER :: matid
    INTEGER :: itter
    INTEGER :: cid
!unused1208    INTEGER :: cid1
    INTEGER :: adj
    INTEGER :: adjl


!    INTEGER, SAVE :: tempErrCount=0
    INTEGER, SAVE :: qvpErrCount=0
!    INTEGER, SAVE :: tempErrReport=0
    INTEGER, SAVE :: qvpErrreport=0
    REAL(r64) :: denominator


    IF(BeginEnvrnFlag .AND. MyEnvrnFlag(sid))THEN
       cells(Extcell(sid))%rh=0.0D0
       cells(Extcell(sid))%rhp1=0.0D0
       cells(Extcell(sid))%rhp2=0.0D0

       cells(Extcell(sid))%temp=10.0D0
       cells(Extcell(sid))%tempp1=10.0D0
       cells(Extcell(sid))%tempp2=10.0D0

       cells(Intcell(sid))%rh=0.0D0
       cells(Intcell(sid))%rhp1=0.0D0
       cells(Intcell(sid))%rhp2=0.0D0

       cells(Intcell(sid))%temp=10.0D0
       cells(Intcell(sid))%tempp1=10.0D0
       cells(Intcell(sid))%tempp2=10.0D0

       DO cid=Extcell(sid)+1,Intcell(sid)-1
          matid=cells(cid)%matid

          cells(cid)%temp=Material(matid)%itemp
          cells(cid)%tempp1=Material(matid)%itemp
          cells(cid)%tempp2=Material(matid)%itemp

          cells(cid)%rh=Material(matid)%irh
          cells(cid)%rhp1=Material(matid)%irh
          cells(cid)%rhp2=Material(matid)%irh
       ENDDO
       MyEnvrnFlag(sid)=.FALSE.
    ENDIF
    IF(.NOT. BeginEnvrnFlag)THEN
       MyEnvrnFlag(sid)=.TRUE.
    ENDIF

    ! Set all the boundary values
    cells(ExtRadcell(sid))%temp=TempOutsideAirFD(sid)
    cells(ExtConcell(sid))%temp=TempOutsideAirFD(sid)
    IF (Surface(sid)%ExtBoundCond == OtherSideCondModeledExt) THEN
     !CR8046 switch modeled rad temp for sky temp.
      cells(ExtSkycell(sid))%temp =  OSCM(Surface(sid)%OSCMPtr)%TRad
      cells(Extcell(sid))%Qadds= 0.0D0 ! eliminate incident shortwave on underlying surface
    ELSE
      cells(ExtSkycell(sid))%temp = SkyTemp

      cells(Extcell(sid))%Qadds=Surface(sid)%Area*QRadSWOutAbs(sid)

    ENDIF

    cells(ExtGrncell(sid))%temp=TempOutsideAirFD(sid)
    RhoOut=RhoVaporAirOut(sid)

    ! Special case when the surface is an internal mass
    IF (Surface(sid)%ExtBoundCond == sid) THEN
       cells(ExtConcell(sid))%temp= Mat(Surface(sid)%Zone)
       RhoOut=RhoVaporAirIn(sid)
    ENDIF

    RhoIn=RhoVaporAirIn(sid)

    cells(ExtRadcell(sid))%htc=HAirFD(sid)
    cells(ExtConcell(sid))%htc=HConvExtFD(sid)
    cells(ExtSkycell(sid))%htc=HSkyFD(sid)
    cells(ExtGrncell(sid))%htc=HGrndFD(sid)



    cells(IntConcell(sid))%temp=Mat(Surface(sid)%Zone)

    cells(IntConcell(sid))%htc=HConvInFD(sid)

    cells(Intcell(sid))%Qadds= &
         Surface(sid)%Area*(QRadSWInAbs(sid)+NetLWRadToSurf(sid)+QHtRadSysSurf(sid)+QHWBaseboardSurf(sid)+  &
                            QSteamBaseboardSurf(sid)+QElecBaseboardSurf(sid)+QRadThermInAbs(sid))
    ! Check, Is this per unit area or for the whole wall.
    !    cells(Intcell(sid))%Qadds=QRadSWInAbs(sid)+NetLWRadToSurf(sid)+QHtRadSysSurf(sid)+QRadThermInAbs(sid)


    cells(ExtConcell(sid))%rh=PsyRhFnTdbRhov(cells(ExtConcell(sid))%temp,RhoOut,'HAMT-Ext')
    cells(IntConcell(sid))%rh=PsyRhFnTdbRhov(cells(IntConcell(sid))%temp,RhoIn,'HAMT-Int')



    IF(cells(ExtConcell(sid))%rh>rhmax)THEN
       cells(ExtConcell(sid))%rh=rhmax
    ENDIF
    IF(cells(IntConcell(sid))%rh>rhmax)THEN
       cells(IntConcell(sid))%rh=rhmax
    ENDIF

! PDB August 2009 Start! Correction for when no vapour transfer coefficient have been defined.
    IF(extvtcflag(sid))THEN
       cells(ExtConcell(sid))%vtc=extvtc(sid)
    ELSE
       IF(cells(ExtConcell(sid))%rh>0)THEN
          cells(ExtConcell(sid))%vtc= &
               HMassConvExtFD(sid)*RhoOut/(PsyPsatFnTemp(TempOutsideAirFD(sid))*cells(ExtConcell(sid))%rh)
       ELSE
          cells(ExtConcell(sid))%vtc=10000.0d0
       ENDIF
    ENDIF


    IF(intvtcflag(sid))THEN
       cells(IntConcell(sid))%vtc=intvtc(sid)
       HMassConvInFD(sid)= &
            cells(IntConcell(sid))%vtc*PsyPsatFnTemp(Mat(Surface(sid)%Zone))*cells(IntConcell(sid))%rh/RhoIn
    ELSE
       IF(cells(IntConcell(sid))%rh>0)THEN
          cells(IntConcell(sid))%vtc= &
               HMassConvInFD(sid)*RhoIn/(PsyPsatFnTemp(Mat(Surface(sid)%Zone))*cells(IntConcell(sid))%rh)
       ELSE
          cells(IntConcell(sid))%vtc= 10000.0d0
       ENDIF
    ENDIF
! PDB August 2009 End

    ! Initialise
    DO cid=firstcell(sid),Extcell(sid)-1
       cells(cid)%tempp1=cells(cid)%temp
       cells(cid)%tempp2=cells(cid)%temp
       cells(cid)%rhp1=cells(cid)%rh
       cells(cid)%rhp2=cells(cid)%rh
    ENDDO
    DO cid=Intcell(sid)+1,lastcell(sid)
       cells(cid)%tempp1=cells(cid)%temp
       cells(cid)%tempp2=cells(cid)%temp
       cells(cid)%rhp1=cells(cid)%rh
       cells(cid)%rhp2=cells(cid)%rh
    ENDDO

    itter=0
    DO
       itter=itter+1
       ! Update Moisture values

       DO cid=firstcell(sid),lastcell(sid)
          matid=cells(cid)%matid
          cells(cid)%vp=RHTOVP(cells(cid)%rh,cells(cid)%temp)
          cells(cid)%vpp1=RHTOVP(cells(cid)%rhp1,cells(cid)%tempp1)
          cells(cid)%vpsat=PsyPsatFnTemp(cells(cid)%tempp1)
          IF(matid>0)THEN
             CALL interp(Material(matid)%niso,Material(matid)%isorh,Material(matid)%isodata,cells(cid)%rhp1, &
                  cells(cid)%water,cells(cid)%dwdphi)
             IF(IsRain.AND.rainswitch)THEN
                CALL interp(Material(matid)%nsuc,Material(matid)%sucwater,Material(matid)%sucdata,cells(cid)%water, &
                     cells(cid)%dw)
             ELSE
                CALL interp(Material(matid)%nred,Material(matid)%redwater,Material(matid)%reddata,cells(cid)%water, &
                     cells(cid)%dw)
             ENDIF
             CALL interp(Material(matid)%nmu,Material(matid)%murh,Material(matid)%mudata,cells(cid)%rhp1, &
                  cells(cid)%mu)
             CALL interp(Material(matid)%ntc,Material(matid)%tcwater,Material(matid)%tcdata,cells(cid)%water, &
                  cells(cid)%wthermalc)
          ENDIF
       ENDDO


       !Calculate Heat and Vapor resistances,
       DO cid=Extcell(sid),Intcell(sid)
          torsum=0.0D0
          oorsum=0.0D0
          vpdiff=0.0D0
          DO ii=1,adjmax
             adj=cells(cid)%adjs(ii)
             adjl=cells(cid)%adjsl(ii)
             IF(adj==-1)EXIT

             IF(cells(cid)%htc>0)THEN
                thermr1=1.0D0/(cells(cid)%overlap(ii)*cells(cid)%htc)
             ELSE IF(cells(cid)%matid>0)THEN
                thermr1=cells(cid)%dist(ii)/(cells(cid)%overlap(ii)*cells(cid)%wthermalc)
             ELSE
                thermr1=0.0D0
             ENDIF

             IF(cells(cid)%vtc>0)THEN
                vaporr1=1.0d0/(cells(cid)%overlap(ii)*cells(cid)%vtc)
             ELSE IF(cells(cid)%matid>0)THEN
                vaporr1=(cells(cid)%dist(ii)*cells(cid)%mu)/(cells(cid)%overlap(ii)*WVDC(cells(cid)%tempp1,OutBaroPress))
             ELSE
                vaporr1=0.0D0
             ENDIF

             IF(cells(adj)%htc>0)THEN
                thermr2=1.0D0/(cells(cid)%overlap(ii)*cells(adj)%htc)
             ELSE IF(cells(adj)%matid>0)THEN
                thermr2=cells(adj)%dist(adjl)/(cells(cid)%overlap(ii)*cells(adj)%wthermalc)
             ELSE
                thermr2=0.0D0
             ENDIF

             IF(cells(adj)%vtc>0)THEN
                vaporr2=1.0D0/(cells(cid)%overlap(ii)*cells(adj)%vtc)
             ELSE IF(cells(adj)%matid>0)THEN
                vaporr2=cells(adj)%mu*cells(adj)%dist(adjl)/(WVDC(cells(adj)%tempp1,OutBaroPress)*cells(cid)%overlap(ii))
             ELSE
                vaporr2=0.0D0
             ENDIF

             IF(thermr1+thermr2>0)THEN
                oorsum=oorsum+1.0D0/(thermr1+thermr2)
                torsum=torsum+cells(adj)%tempp1/(thermr1+thermr2)
             ENDIF
             IF(vaporr1+vaporr2>0)THEN
                vpdiff=vpdiff+(cells(adj)%vp-cells(cid)%vp)/(vaporr1+vaporr2)
             ENDIF


          ENDDO


          ! Calculate Heat Capacitance
          tcap=((cells(cid)%density*cells(cid)%spech+cells(cid)%water*wspech)*cells(cid)%volume)

          ! calculate the latent heat if wanted and check for divergence
          qvp=0.0D0
          IF((cells(cid)%matid>0).AND.(latswitch))THEN
             qvp=vpdiff*whv
          ENDIF
          IF(ABS(qvp)>qvplim)THEN
             IF(.NOT. WarmupFlag)THEN
                qvpErrCount=qvpErrCount+1
                IF(qvpErrCount < 16)THEN
                   CALL ShowWarningError('HeatAndMoistureTransfer: Large Latent Heat for Surface '//TRIM(Surface(sid)%Name))
                ELSE
                   CALL ShowRecurringWarningErrorAtEnd('HeatAndMoistureTransfer: Large Latent Heat Errors ',qvpErrReport)
                ENDIF
             ENDIF
             qvp=0.0D0
          ENDIF

          ! Calculate the temperature for the next time step
          cells(cid)%tempp1=(torsum+qvp+cells(cid)%Qadds+(tcap*cells(cid)%temp/deltat))/(oorsum+(tcap/deltat))
       ENDDO

       !Check for silly temperatures
       tempmax=MAXVAL(cells%tempp1)
       tempmin=MINVAL(cells%tempp1)
       IF(tempmax>MaxSurfaceTempLimit)THEN
          IF (.NOT. WarmupFlag)THEN
             IF (Surface(sid)%HighTempErrCount == 0) THEN
               CALL ShowSevereMessage('HAMT: Temperature (high) out of bounds ('//TRIM(RoundSigDigits(tempmax,2))//  &
                                 ') for surface='//TRIM(Surface(sid)%Name))
               CALL ShowContinueErrorTimeStamp(' ')
             ENDIF
             CALL ShowRecurringWarningErrorAtEnd('HAMT: Temperature Temperature (high) out of bounds; Surface='//  &
                TRIM(Surface(sid)%Name),  &
                Surface(sid)%HighTempErrCount,ReportMinOf=tempmax,ReportMinUnits='C',  &
                ReportMaxOf=tempmax,ReportMaxUnits='C')
          ENDIF
       ENDIF
       IF(tempmax>MaxSurfaceTempLimitBeforeFatal)THEN
          IF (.NOT. WarmupFlag)THEN
             CALL ShowSevereError('HAMT: HAMT: Temperature (high) out of bounds ( '//TRIM(RoundSigDigits(tempmax,2))//  &
                                 ') for surface='//TRIM(Surface(sid)%Name))
             CALL ShowContinueErrorTimeStamp(' ')
             CALL ShowFatalError('Program terminates due to preceding condition.')
          ENDIF
       ENDIF
       IF(tempmin<MinSurfaceTempLimit)THEN
          IF (.NOT. WarmupFlag)THEN
             IF (Surface(sid)%HighTempErrCount == 0) THEN
               CALL ShowSevereMessage('HAMT: Temperature (low) out of bounds ('//TRIM(RoundSigDigits(tempmin,2))//  &
                                 ') for surface='//TRIM(Surface(sid)%Name))
               CALL ShowContinueErrorTimeStamp(' ')
             ENDIF
             CALL ShowRecurringWarningErrorAtEnd('HAMT: Temperature Temperature (high) out of bounds; Surface='//  &
                TRIM(Surface(sid)%Name),  &
                Surface(sid)%HighTempErrCount,ReportMinOf=tempmin,ReportMinUnits='C',  &
                ReportMaxOf=tempmin,ReportMaxUnits='C')
          ENDIF
       ENDIF
       IF(tempmin<MinSurfaceTempLimitBeforeFatal)THEN
          IF (.NOT. WarmupFlag)THEN
             CALL ShowSevereError('HAMT: HAMT: Temperature (low) out of bounds ( '//TRIM(RoundSigDigits(tempmin,2))//  &
                                 ') for surface='//TRIM(Surface(sid)%Name))
             CALL ShowContinueErrorTimeStamp(' ')
             CALL ShowFatalError('Program terminates due to preceding condition.')
          ENDIF
       ENDIF


       ! Calculate the liquid and vapor resisitances
       DO cid=Extcell(sid),Intcell(sid)
          phioosum=0.0d0
          phiorsum=0.0d0
          vpoosum=0.0d0
          vporsum=0.0d0

          DO ii=1,adjmax
             adj=cells(cid)%adjs(ii)
             adjl=cells(cid)%adjsl(ii)
             IF(adj==-1)EXIT

             IF(cells(cid)%vtc>0)THEN
                vaporr1=1.0d0/(cells(cid)%overlap(ii)*cells(cid)%vtc)
             ELSE IF(cells(cid)%matid>0)THEN
                vaporr1=(cells(cid)%dist(ii)*cells(cid)%mu)/(cells(cid)%overlap(ii)*WVDC(cells(cid)%tempp1,OutBaroPress))
             ELSE
                vaporr1=0.0d0
             ENDIF

             IF(cells(adj)%vtc>0)THEN
                vaporr2=1.0d0/(cells(cid)%overlap(ii)*cells(adj)%vtc)
             ELSE IF(cells(adj)%matid>0)THEN
                vaporr2=(cells(adj)%dist(adjl)*cells(adj)%mu)/(cells(cid)%overlap(ii)*WVDC(cells(adj)%tempp1,OutBaroPress))
             ELSE
                vaporr2=0.0d0
             ENDIF
             IF(vaporr1+vaporr2>0)THEN
                vpoosum=vpoosum+1.0d0/(vaporr1+vaporr2)
                vporsum=vporsum+(cells(adj)%vpp1/(vaporr1+vaporr2))
             ENDIF

             IF((cells(cid)%dw>0).AND.(cells(cid)%dwdphi>0))THEN
                rhr1=cells(cid)%dist(ii)/(cells(cid)%overlap(ii)*cells(cid)%dw*cells(cid)%dwdphi)
             ELSE
                rhr1=0.0d0
             ENDIF
             IF((cells(adj)%dw>0).AND.(cells(adj)%dwdphi>0))THEN
                rhr2=cells(adj)%dist(adjl)/(cells(cid)%overlap(ii)*cells(adj)%dw*cells(adj)%dwdphi)
             ELSE
                rhr2=0.0d0
             ENDIF

             !             IF(rhr1+rhr2>0)THEN
             IF(rhr1*rhr2>0)THEN
                phioosum=phioosum+1.0d0/(rhr1+rhr2)
                phiorsum=phiorsum+(cells(adj)%rhp1/(rhr1+rhr2))
             ENDIF

          ENDDO

          ! Moisture Capacitance
          IF(cells(cid)%dwdphi>0.0d0)THEN
             wcap=cells(cid)%dwdphi*cells(cid)%volume
          ELSE
             wcap=0.0d0
          ENDIF

          ! Calculate the RH for the next time step
          denominator=(phioosum+vpoosum*cells(cid)%vpsat+wcap/deltat)
          if (denominator /= 0.0d0) then
            cells(cid)%rhp1=(phiorsum+vporsum+(wcap*cells(cid)%rh)/deltat)/denominator
          else
            call ShowSevereError('CalcHeatBalHAMT: demoninator in calculating RH is zero.  Check material properties for accuracy.')
            call ShowContinueError('...Problem occurs in Material="'//trim(Material(cells(cid)%MatID)%Name)//'".')
            call ShowFatalError('Program terminates due to preceding condition.')
          endif

          IF(cells(cid)%rhp1>rhmax)THEN
             cells(cid)%rhp1=rhmax
          ENDIF
       ENDDO

       !Check for convergence or too many itterations
       sumtp1=0.0d0
       DO cid=Extcell(sid),Intcell(sid)
          IF(sumtp1<ABS(cells(cid)%tempp2-cells(cid)%tempp1))THEN
             sumtp1=ABS(cells(cid)%tempp2-cells(cid)%tempp1)
          ENDIF
       ENDDO
       IF(sumtp1<convt)THEN
          EXIT
       ENDIF
       IF(itter>ittermax)THEN
          EXIT
       ENDIF
       DO cid=firstcell(sid),lastcell(sid)
          cells(cid)%tempp2=cells(cid)%tempp1
          cells(cid)%rhp2=cells(cid)%rhp1
       ENDDO
    ENDDO


    ! report back to CalcHeatBalanceInsideSurf
    TempSurfOutTmp=cells(Extcell(sid))%tempp1
    TempSurfInTmp=cells(Intcell(sid))%tempp1

    TempSurfInP=cells(Intcell(sid))%rhp1*PsyPsatFnTemp(cells(Intcell(sid))%tempp1)

    RhoVaporSurfIn(sid)=TempSurfInP/(461.52d0*(Mat(Surface(sid)%Zone)+KelvinConv))

  END SUBROUTINE CalcHeatBalHAMT
  SUBROUTINE UpdateHeatBalHAMT(sid)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! The zone heat balance equation has converged, so now the HAMT values are to be fixed
    ! ready for the next itteration.
    ! Fill all the report variables

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(in) :: sid


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: cid
    REAL(r64) :: watermass
    REAL(r64) :: matmass
!unused1208    REAL(r64), SAVE :: InOld=0.0D0
!unused1208    REAL(r64), SAVE :: OutOld=0.0D0


    !Update Temperatures and RHs. Calculate report variables
    matmass=0.0D0
    watermass=0.0D0
    DO cid=firstcell(sid),lastcell(sid)
       ! fix HAMT values for this surface
       cells(cid)%temp=cells(cid)%tempp1
       cells(cid)%rh=cells(cid)%rhp1
       cells(cid)%rhp=cells(cid)%rh*100.d0
       IF(cells(cid)%density>0.0d0)THEN
          cells(cid)%wreport=cells(cid)%water/cells(cid)%density
          watermass=watermass+(cells(cid)%water*cells(cid)%volume)
          matmass=matmass+(cells(cid)%density*cells(cid)%volume)
       ENDIF
    ENDDO

    watertot(sid)=0.0d0
    IF(matmass>0) watertot(sid)=watermass/matmass

    surfrh(sid)=100.0d0*cells(Intcell(sid))%rh
    surfextrh(sid)=100.0d0*cells(Extcell(sid))%rh
    surftemp(sid)=cells(Intcell(sid))%temp
    surfexttemp(sid)=cells(Extcell(sid))%temp
    surfvp(sid)=RHTOVP(cells(Intcell(sid))%rh,cells(Intcell(sid))%temp)

  END SUBROUTINE UpdateHeatBalHAMT

  SUBROUTINE interp(ndata,xx,yy,invalue,outvalue,outgrad)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! To find a value by searching an array and interpolating between two coordinates
    ! Also returns the gradient if required.

    ! METHODOLOGY EMPLOYED:
    ! Simple search

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(in) :: ndata
    REAL(r64), INTENT(in), DIMENSION(ndata) :: xx
    REAL(r64), INTENT(in), DIMENSION(ndata) :: yy
    REAL(r64), INTENT(in) :: invalue
    REAL(r64), INTENT(out) :: outvalue
    REAL(r64), INTENT(out), OPTIONAL :: outgrad


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    REAL(r64) :: xxlow
    REAL(r64) :: xxhigh
    REAL(r64) :: yylow
    REAL(r64) :: yyhigh
    REAL(r64) :: mygrad
    INTEGER :: step

    mygrad=0.0d0
    outvalue=0.0d0

    IF(ndata>1)THEN
       xxlow=xx(1)
       yylow=yy(1)
       DO step=2,ndata
          xxhigh=xx(step)
          yyhigh=yy(step)
          IF(invalue<=xxhigh)EXIT
          xxlow=xxhigh
          yylow=yyhigh
       ENDDO

       IF(xxhigh>xxlow)THEN
          mygrad=(yyhigh-yylow)/(xxhigh-xxlow)
          outvalue=(invalue-xxlow)*mygrad+yylow
! PDB August 2009 bug fix
        else if (abs(xxhigh-xxlow)<0.0000000001d0) then
          outvalue=yylow
      ENDIF
    ENDIF

    IF( PRESENT(outgrad)) THEN
       ! return gradient if required
       outgrad=mygrad
    ENDIF
    RETURN
  END SUBROUTINE interp

  REAL(r64) FUNCTION RHtoVP(RH,Temperature)
    ! FUNCTION INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! Convert Relative Humidity and Temperature to Vapor Pressure

    ! METHODOLOGY EMPLOYED:
    ! <description>

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:

    REAL(r64), INTENT(IN) :: RH
    REAL(r64), INTENT(IN) :: Temperature


    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:

    REAL(r64) :: VPSat

    VPSat=PsyPsatFnTemp(Temperature)

    RHtoVP=RH*VPsat

    RETURN
  END FUNCTION RHtoVP
  REAL(r64) FUNCTION WVDC(Temperature,ambp)
    ! FUNCTION INFORMATION:
    !       AUTHOR         Phillip Biddulph
    !       DATE WRITTEN   June 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! To calculate the Water Vapor Diffusion Coefficient in air
    ! using the temperature and ambient atmospheric pressor

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! K?zel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components.
    ! One- and two-dimensional calculation using simple parameters. IRB Verlag 1995

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Temperature
    REAL(r64), INTENT(IN) :: ambp


    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    ! na

    ! DERIVED TYPE DEFINITIONS:
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na
    !


    WVDC=(2.d-7*(Temperature+KelvinConv)**0.81d0)/ambp

    RETURN
  END FUNCTION WVDC

!
!                                 COPYRIGHT NOTICE
!
!     Portions Copyright ?University College London 2007.  All rights
!     reserved.
!
!     UCL LEGAL NOTICE
!     Neither UCL, members of UCL nor any person or organisation acting on
!     behalf of either:
!
!     A. Makes any warranty of representation, express or implied with
!        respect to the accuracy, completeness, or usefulness of the
!        information contained in this program, including any warranty of
!        merchantability or fitness of any purpose with respect to the
!        program, or that the use of any information disclosed in this
!        program may not infringe privately-owned rights, or
!
!     B. Assumes any liability with respect to the use of, or for any and
!        all damages resulting from the use of the program or any portion
!        thereof or any information disclosed therein.
!
!
!     NOTICE
!
!     Copyright © 1996-2013 The Board of Trustees of the University of
!     Illinois and The Regents of the University of California through
!     Ernest Orlando Lawrence Berkeley National Laboratory.  All rights
!     reserved.
!
!     Portions of the EnergyPlus software package have been developed and
!     copyrighted by other individuals, companies and institutions.  These
!     portions have been incorporated into the EnergyPlus software package
!     under license.  For a complete list of contributors, see "Notice"
!     located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting
!     on its behalf a paid-up, nonexclusive, irrevocable, worldwide license
!     in this data to reproduce, prepare derivative works, and perform
!     publicly and display publicly. Beginning five (5) years after
!     permission to assert copyright is granted, subject to two possible
!     five year renewals, the U.S. Government is granted for itself and
!     others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display
!     publicly, and to permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

END MODULE HeatBalanceHAMTManager
