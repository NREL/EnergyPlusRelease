! define this variable to get new code, commenting should yield original
#define SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

MODULE AirflowNetworkSolver


          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
          !       DATE WRITTEN   Jul. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS MODULE:
          ! This module is used to simulate airflows and pressures. The module is modified to
          ! meet requirements of EnergyPLus based on AIRNET, developed by
          ! National Institute of Standards and Technology (NIST).

          ! METHODOLOGY EMPLOYED:
          ! An airflow network approach is used. It consists of nodes connected by airflow elements.
          ! The Newton's method is applied to solve a sparse matrix. When a new solver is available, this
          ! module will be replaced or updated.

          ! REFERENCES:
          ! Walton, G. N., 1989, "AIRNET - A Computer Program for Building Airflow Network Modeling,"
          ! NISTIR 89-4072, National Institute of Standards and Technology, Gaithersburg, Maryland

          ! OTHER NOTES: none


          ! USE STATEMENTS:

    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: MaxNameLength, PI, DegToRadians, KelvinConv,rTinyValue
    USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError,  &
                           SetupOutputVariable, ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
    USE DataEnvironment, ONLY: StdBaroPress, OutBaroPress, OutDryBulbTemp, OutHumRat,  &
                               Latitude
    USE DataSurfaces, ONLY: Surface
    USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHFnTdbW
    USE DataAirflowNetwork


IMPLICIT NONE   ! Enforce explicit typing of all variables

INTEGER, PUBLIC  :: NetworkNumOfLinks = 0
INTEGER, PUBLIC  :: NetworkNumOfNodes = 0

PRIVATE ! Everything private unless explicitly made public

INTEGER, PARAMETER :: NrInt=20 ! Number of intervals for a large opening

! Common block AFEDAT
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AFECTL
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AFLOW2
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AFLOW
REAL(r64), ALLOCATABLE, DIMENSION(:) :: PS
REAL(r64), ALLOCATABLE, DIMENSION(:) :: PW


! Common block CONTRL
REAL(r64), PRIVATE :: PB=0.0d0
INTEGER, PRIVATE :: LIST=0


! Common block ZONL
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RHOZ
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SQRTDZ
REAL(r64), ALLOCATABLE, DIMENSION(:) :: VISCZ
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SUMAF
REAL(r64), ALLOCATABLE, DIMENSION(:) :: TZ              ! Temperature [C]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: WZ              ! Humidity ratio [kg/kg]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: PZ  ! Pressure [Pa]

! Other array variables
INTEGER, ALLOCATABLE, DIMENSION(:) :: ID
INTEGER, ALLOCATABLE, DIMENSION(:) :: IK
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AD
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AU

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
INTEGER, ALLOCATABLE, DIMENSION(:) :: newIK   !noel
REAL(r64), ALLOCATABLE, DIMENSION(:) :: newAU !noel
#endif

!REAL(r64), ALLOCATABLE, DIMENSION(:) :: AL
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SUMF
INTEGER :: Unit11=0
INTEGER :: Unit21=0

! Large opening variables
REAL(r64), ALLOCATABLE, DIMENSION(:) :: DpProf    ! Differential pressure profile for Large Openings [Pa]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RhoProfF  ! Density profile in FROM zone [kg/m3]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RhoProfT  ! Density profile in TO zone [kg/m3]
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: DpL     ! Array of stack pressures in link

PUBLIC SETSKY
PUBLIC AIRMOV
Public AllocateAirflowNetworkData
Public InitAirflowNetworkData


CONTAINS


SUBROUTINE AllocateAirflowNetworkData


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Aug. 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine allocates dynamic arrays for AirflowNetworkSolver.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER i,j,N
INTEGER, EXTERNAL :: GetNewUnitNumber  ! External  function to "get" a unit number

! Assume a network to simulate multizone airflow is a subset of the network to simulate air distribution system.
! Network array size is allocated based on the network of air distribution system.
! If multizone airflow is simulated only, the array size is allocated based on the multizone network.
          ! FLOW:
   NetworkNumOfLinks = AirflowNetworkNumOfLinks
   NetworkNumOfNodes = AirflowNetworkNumOfNodes

   ALLOCATE(AFECTL(NetworkNumOfLinks))
   ALLOCATE(AFLOW2(NetworkNumOfLinks))
   ALLOCATE(AFLOW(NetworkNumOfLinks))
   ALLOCATE(PW(NetworkNumOfLinks))
   ALLOCATE(PS(NetworkNumOfLinks))

   ALLOCATE(TZ(NetworkNumOfNodes))
   ALLOCATE(WZ(NetworkNumOfNodes))
   ALLOCATE(PZ(NetworkNumOfNodes))
   ALLOCATE(RHOZ(NetworkNumOfNodes))
   ALLOCATE(SQRTDZ(NetworkNumOfNodes))
   ALLOCATE(VISCZ(NetworkNumOfNodes))
   ALLOCATE(SUMAF(NetworkNumOfNodes))


   ALLOCATE(ID(NetworkNumOfNodes))
   ALLOCATE(IK(NetworkNumOfNodes+1))
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
   ALLOCATE(newIK(NetworkNumOfNodes+1))
#endif
   ALLOCATE(AD(NetworkNumOfNodes))
   ALLOCATE(SUMF(NetworkNumOfNodes))

   N = 0
   Do i=1,AirflowNetworkNumOfLinks
      j = AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum
      if (j .EQ. CompTypeNum_DOP) then
         n = n+1
      end if
   End do

   ALLOCATE(DpProf(N*(NrInt+2)))
   ALLOCATE(RhoProfF(N*(NrInt+2)))
   ALLOCATE(RhoProfT(N*(NrInt+2)))
   ALLOCATE(DpL(2,AirflowNetworkNumOfLinks))

   PB = 101325.0d0
!   LIST = 5
   LIST = 0
   If (LIST .GE. 1) then
      Unit21 = GetNewUnitNumber()
      open(Unit21,file='eplusADS.out')
   end if


   DO N=1,NetworkNumOfNodes
      ID(N) = N
   END DO
   DO I=1,NetworkNumOfLinks
      AFECTL(I) = 1.0d0
      AFLOW(I) = 0.0d0
      AFLOW2(I) = 0.0d0
   END DO


   DO I=1,NetworkNumOfNodes
      TZ(I) = AirflowNetworkNodeSimu(I)%TZ
      WZ(I) = AirflowNetworkNodeSimu(I)%WZ
      PZ(I) = AirflowNetworkNodeSimu(I)%PZ
   END DO

   ! Assign linkage values
   DO I=1,NetworkNumOfLinks
      PW(I) = 0.0d0
   END DO
   ! Write an ouput file used for AIRNET input
   if (LIST .GE. 5) then
     Unit11 = GetNewUnitNumber()
     open(Unit11,file='eplusADS.inp')
     DO i=1,NetworkNumOfNodes
        write(Unit11,901) i,AirflowNetworkNodeData(I)%NodeTypeNum,AirflowNetworkNodeData(I)%NodeHeight,TZ(I),PZ(I)
     end do
     write(Unit11,900) 0
     DO i=1,AirflowNetworkNumOfComps
        j = AirflowNetworkCompData(i)%TypeNum
        select case (AirflowNetworkCompData(i)%CompTypeNum)
           CASE (CompTypeNum_PLR) !'PLR'  Power law component
!              WRITE(Unit11,902) AirflowNetworkCompData(i)%CompNum,1,DisSysCompLeakData(j)%FlowCoef, &
!                  DisSysCompLeakData(j)%FlowCoef,DisSysCompLeakData(j)%FlowCoef,DisSysCompLeakData(j)%FlowExpo
           CASE (CompTypeNum_SCR) !'SCR'  Surface crack component
              WRITE(Unit11,902) AirflowNetworkCompData(i)%CompNum,1,MultizoneSurfaceCrackData(j)%FlowCoef, &
                  MultizoneSurfaceCrackData(j)%FlowCoef,MultizoneSurfaceCrackData(j)%FlowCoef,MultizoneSurfaceCrackData(j)%FlowExpo
           CASE (CompTypeNum_DWC) !'DWC' Duct component
!              WRITE(Unit11,902) AirflowNetworkCompData(i)%CompNum,2,DisSysCompDuctData(j)%L,DisSysCompDuctData(j)%D, &
!                               DisSysCompDuctData(j)%A,DisSysCompDuctData(j)%Rough
!              WRITE(Unit11,903) DisSysCompDuctData(i)%TurDynCoef,DisSysCompDuctData(j)%LamFriCoef, &
!                               DisSysCompDuctData(j)%LamFriCoef,DisSysCompDuctData(j)%InitLamCoef
!           CASE (CompTypeNum_CVF) ! 'CVF' Constant volume fan component
!              WRITE(Unit11,904) AirflowNetworkCompData(i)%CompNum,4,DisSysCompCVFData(j)%FlowRate
           CASE (CompTypeNum_EXF) ! 'EXF' Zone exhaust fan
              WRITE(Unit11,904) AirflowNetworkCompData(i)%CompNum,4,MultizoneCompExhaustFanData(j)%FlowRate
           CASE Default
        END SELECT
     end do
     write(Unit11,900) 0
     DO i=1,NetworkNumOfLinks
        write(Unit11,910) i,AirflowNetworkLinkageData(I)%NodeNums(1),AirflowNetworkLinkageData(I)%NodeHeights(1), &
                         AirflowNetworkLinkageData(I)%NodeNums(2),AirflowNetworkLinkageData(I)%NodeHeights(2), &
                         AirflowNetworkLinkageData(I)%CompNum,0,0
     end do
     write(Unit11,900) 0
   end if

   CALL SETSKY

   !SETSKY figures out the IK stuff -- which is why E+ doesn't allocate AU until here
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
!   ! only printing to screen, can be commented
!   print*, "SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS is defined"
!
!   write(*,'(2(a,i8))') "AllocateAirflowNetworkData: after SETSKY, allocating AU.  NetworkNumOfNodes=", &
!        NetworkNumOfNodes, " IK(NetworkNumOfNodes+1)= NNZE=", IK(NetworkNumOfNodes+1)
!   print*, " NetworkNumOfLinks=", NetworkNumOfLinks
!
   ! allocate same size as others -- this will be maximum  !noel
   ALLOCATE(newAU(IK(NetworkNumOfNodes+1)))
#endif

   ! noel, GNU says the AU is indexed above its upper bound
   !ALLOCATE(AU(IK(NetworkNumOfNodes+1)-1))
   ALLOCATE(AU(IK(NetworkNumOfNodes+1)))

900 Format(1X,i2)
901 Format(1X,2I4,4F9.4)
902 Format(1X,2I4,4F9.4)
903 Format(9X,4F9.4)
904 Format(1X,2I4,1F9.4)
910 Format(1X,I4,2(I4,F9.4),I4,2F4.1)

RETURN
END SUBROUTINE AllocateAirflowNetworkData

SUBROUTINE InitAirflowNetworkData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Aug. 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes variables for AirflowNetworkSolver.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   INTEGER I,N

      ! FLOW:
   DO N=1,NetworkNumOfNodes
      ID(N) = N
   END DO
   DO I=1,NetworkNumOfLinks
      AFECTL(I) = 1.0d0
      AFLOW(I) = 0.0d0
      AFLOW2(I) = 0.0d0
   END DO

   DO I=1,NetworkNumOfNodes
      TZ(I) = AirflowNetworkNodeSimu(I)%TZ
      WZ(I) = AirflowNetworkNodeSimu(I)%WZ
      PZ(I) = AirflowNetworkNodeSimu(I)%PZ
   END DO

RETURN

END SUBROUTINE InitAirflowNetworkData


      SUBROUTINE SETSKY
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   1998
          !       MODIFIED       Feb. 2006 (L. Gu) to meet requirements of AirflowNetwork
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the "IK" array describing the sparse matrix [A] in skyline
          !     form by using the location matrix.

          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! AIRNET


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! IK(K) - pointer to the top of column/row "K".
      INTEGER  I, J, K, L, M, N1, N2

      ! FLOW:
      ! Initialize "IK".
      DO I=1,NetworkNumOfNodes+1
         IK(I) = 0
      END DO
      ! Determine column heights.
      DO M=1,NetworkNumOfLinks
        J = AirflowNetworkLinkageData(M)%NodeNums(2)
        IF(J.EQ.0) CYCLE
        L = ID(J)
        I = AirflowNetworkLinkageData(M)%NodeNums(1)
        K = ID(I)
        N1 = ABS(L-K)
        N2 = MAX(K,L)
        IK(N2) = MAX(IK(N2),N1)
      END DO
      ! Convert heights to column addresses.
      J = IK(1)
      IK(1) = 1
      DO K=1,NetworkNumOfNodes
        I = IK(K+1)
        IK(K+1) = IK(K)+J
        J = I
      END DO

      RETURN
      END SUBROUTINE SETSKY


      SUBROUTINE AIRMOV
!
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is a driver for AIRNET to calculate nodal pressures and linkage airflows

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         INTEGER  I, M, N, ITER

      ! FLOW:
      ! Compute zone air properties.
      DO N=1,NetworkNumOfNodes
        RHOZ(N) = PsyRhoAirFnPbTdbW(StdBaroPress+PZ(N),TZ(N),WZ(N))
        if (AirflowNetworkNodeData(N)%ExtNodeNum > 0) then
           RHOZ(N) = PsyRhoAirFnPbTdbW(StdBaroPress+PZ(N),OutDryBulbTemp,OutHumRat)
           TZ(N) = OutDryBulbTemp
           WZ(N) = OutHumRat
        end if
        SQRTDZ(N) = SQRT(RHOZ(N))
        VISCZ(N) = 1.71432d-5+4.828d-8*TZ(N)
        IF(LIST.GE.2) WRITE(Unit21,903) 'D,V:',N,RHOZ(N),VISCZ(N)
      END DO
      IF(AirflowNetworkSimu%InitFlag.EQ.0) THEN
        DO N=1,NetworkNumOfNodes
          IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.0) PZ(N) = 0.0d0
        END DO
      END IF
      ! Compute stack pressures.
      DO I=1,NetworkNumOfLinks
        N = AirflowNetworkLinkageData(I)%NodeNums(1)
        M = AirflowNetworkLinkageData(I)%NodeNums(2)
        IF(AFLOW(I).GT.0.0d0) THEN
          PS(I) = 9.80d0*( RHOZ(N)*(AirflowNetworkNodeData(N)%NodeHeight-AirflowNetworkNodeData(M)%NodeHeight) &
                    +AirflowNetworkLinkageData(I)%NodeHeights(2)*(RHOZ(M)-RHOZ(N)) )
        ELSE IF(AFLOW(I).LT.0.0d0) THEN
          PS(I) = 9.80d0*( RHOZ(M)*(AirflowNetworkNodeData(N)%NodeHeight-AirflowNetworkNodeData(M)%NodeHeight) &
                     +AirflowNetworkLinkageData(I)%NodeHeights(1)*(RHOZ(M)-RHOZ(N)) )
        ELSE
          PS(I) = 4.90d0*( (RHOZ(N)+RHOZ(M))*(AirflowNetworkNodeData(N)%NodeHeight-AirflowNetworkNodeData(M)%NodeHeight) &
            + (AirflowNetworkLinkageData(I)%NodeHeights(1)+AirflowNetworkLinkageData(I)%NodeHeights(2))*(RHOZ(M)-RHOZ(N)))
        END IF
      END DO

      ! Calculate pressure field in a large opening
      CALL PSTACK

      CALL SOLVZP(IK,AD,AU,ITER)

      ! Report element flows and zone pressures.
      DO N=1,NetworkNumOfNodes
        SUMAF(N) = 0.0d0
      END DO
      IF(LIST.GE.1) WRITE(Unit21,900)
      DO I=1,NetworkNumOfLinks
        N = AirflowNetworkLinkageData(I)%NodeNums(1)
        M = AirflowNetworkLinkageData(I)%NodeNums(2)
        IF(LIST.GE.1) THEN
          WRITE(Unit21,901) 'Flow: ',I,N,M,AirflowNetworkLinkSimu(I)%DP,AFLOW(I),AFLOW2(I)
        END IF
        SUMAF(N) = SUMAF(N)-AFLOW(I)-AFLOW2(I)
        SUMAF(M) = SUMAF(M)+AFLOW(I)+AFLOW2(I)
      END DO
      DO N=1,NetworkNumOfNodes
        IF(LIST.GE.1) WRITE(Unit21,903) 'Room: ',N,PZ(N),SUMAF(N),TZ(N)
      END DO

   DO I=1,NetworkNumOfLinks
      if (AFLOW2(i) .ne. 0.0d0) then
         Continue
      end if
      if (AFLOW(i) > 0.0d0) then
         AirflowNetworkLinkSimu(I)%FLOW = AFLOW(I)
         AirflowNetworkLinkSimu(I)%FLOW2 = 0.0d0
      else
         AirflowNetworkLinkSimu(I)%FLOW = 0.0d0
         AirflowNetworkLinkSimu(I)%FLOW2 = -AFLOW(I)
      end if
      If (AirflowNetworkCompData(AirflowNetworkLinkageData(I)%CompNum)%CompTypeNum == CompTypeNum_HOP) Then
         if (AFLOW2(I) .ne. 0.0d0) then
            AirflowNetworkLinkSimu(I)%FLOW = AFLOW(I)+AFLOW2(I)
            AirflowNetworkLinkSimu(I)%FLOW2 = AFLOW2(I)
         end if
      end if
      if (AirflowNetworkLinkageData(I)%DetOpenNum > 0) then
         if (AFLOW2(I) .ne. 0.0d0) then
            AirflowNetworkLinkSimu(I)%FLOW = AFLOW(I)+AFLOW2(I)
            AirflowNetworkLinkSimu(I)%FLOW2 = AFLOW2(I)
         end if
      end if
      If (AirflowNetworkCompData(AirflowNetworkLinkageData(I)%CompNum)%CompTypeNum == CompTypeNum_SOP .AND. &
          AFLOW2(I) .ne. 0.0d0) then
        if (AFLOW(i) >= 0.0d0) then
         AirflowNetworkLinkSimu(I)%FLOW = AFLOW(I)
         AirflowNetworkLinkSimu(I)%FLOW2 = abs(AFLOW2(I))
        else
         AirflowNetworkLinkSimu(I)%FLOW = abs(AFLOW2(I))
         AirflowNetworkLinkSimu(I)%FLOW2 = -AFLOW(I)
        End If
      end if
   END DO

   DO I=1,NetworkNumOfNodes
      AirflowNetworkNodeSimu(I)%PZ = PZ(I)
   END DO

  900 FORMAT(/,11X,'i    n    m       DP',12x,'F1',12X,'F2')
  901 FORMAT(1X,A6,3I5,3F14.6)
  902 FORMAT(/,11X,'n       P',12x,'sumF')
  903 FORMAT(1X,A6,I5,3F14.6)
  907 FORMAT(/,' CPU seconds for ',A,F12.3)

      RETURN
      END SUBROUTINE AIRMOV


      SUBROUTINE SOLVZP(IK,AD,AU,ITER)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves zone pressures by modified Newton-Raphson iteration

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE General, ONLY: RoundSigDigits

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(INOUT)  :: IK(NetworkNumOfNodes+1) ! pointer to the top of column/row "K"
          INTEGER, INTENT(INOUT)  :: ITER  ! number of iterations
          !noel GNU says AU is being indexed beyound bounds
          !REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AD(NetworkNumOfNodes)  ! the main diagonal of [A] before and after factoring


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          !     NNZE   - number of nonzero entries in the "AU" array.
          !     LFLAG   - if = 1, use laminar relationship (initialization).
          !     I       - element number.
          !     N       - number of node/zone 1.
          !     M       - number of node/zone 2.
          !     F       - flows through the element (kg/s).
          !     DF      - partial derivatives:  DF/DP.
          !     NF      - number of flows, 1 or 2.
          !     SUMF    - sum of flows into node/zone.
          !     CCF     - current pressure correction (Pa).
          !     PCF     - previous pressure correction (Pa).
          !     CEF     - convergence enhancement factor.
!
          INTEGER  N, NNZE, NSYM, LFLAG, CONVG, ACCEL
          REAL(r64) PCF(NetworkNumOfNodes), CEF(NetworkNumOfNodes)
          REAL(r64) C, SSUMF, SSUMAF, ACC0, ACC1
          REAL(r64) CCF(NetworkNumOfNodes)

      ! FLOW:
      ACC1 = 0.0d0
      ACCEL = 0
      NSYM = 0
      NNZE = IK(NetworkNumOfNodes+1)-1
      IF(LIST.GE.2) WRITE(Unit21,*) 'Initialization',NetworkNumOfNodes,NetworkNumOfLinks,NNZE
      ITER = 0

      DO N=1,NetworkNumOfNodes
        PCF(N) = 0.0d0
        CEF(N) = 0.0d0
      END DO

      IF(AirflowNetworkSimu%InitFlag.NE.1) THEN
        ! Initialize node/zone pressure values by assuming only linear relationship between
        ! airflows and pressure drops.
        LFLAG = 1
        CALL FILJAC(NNZE,LFLAG)
        DO N=1,NetworkNumOfNodes
          IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.0) PZ(N) = SUMF(N)
        END DO
        ! Data dump.
        IF(LIST.GE.3) THEN
          CALL DUMPVD('AD:',AD,NetworkNumOfNodes,Unit21)
          CALL DUMPVD('AU:',AU,NNZE,Unit21)
          CALL DUMPVR('AF:',SUMF,NetworkNumOfNodes,Unit21)
        END IF
        ! Solve linear system for approximate PZ.
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        CALL FACSKY(newAU,AD,newAU,newIK,NetworkNumOfNodes,NSYM) !noel
        CALL SLVSKY(newAU,AD,newAU,PZ,newIK,NetworkNumOfNodes,NSYM) !noel
#else
        CALL FACSKY(AU,AD,AU,IK,NetworkNumOfNodes,NSYM)
        CALL SLVSKY(AU,AD,AU,PZ,IK,NetworkNumOfNodes,NSYM)
#endif
        IF(LIST.GE.2) CALL DUMPVD('PZ:',PZ,NetworkNumOfNodes,Unit21)
      END IF
      ! Solve nonlinear airflow network equations by modified Newton's method.

      DO WHILE (ITER .LT. AirflowNetworkSimu%MaxIteration)
        LFLAG = 0
        ITER = ITER+1
        IF(LIST.GE.2) WRITE(Unit21,*) 'Begin iteration ',ITER
        ! Set up the Jacobian matrix.
        CALL FILJAC(NNZE,LFLAG)
        ! Data dump.
        IF(LIST.GE.3) THEN
          CALL DUMPVR('SUMF:',SUMF,NetworkNumOfNodes,Unit21)
          CALL DUMPVR('SUMAF:',SUMAF,NetworkNumOfNodes,Unit21)
        END IF
        ! Check convergence.
        CONVG = 1
        SSUMF = 0.0d0
        SSUMAF = 0.0d0
        DO N=1,NetworkNumOfNodes
          SSUMF = SSUMF + ABS(SUMF(N))
          SSUMAF = SSUMAF + SUMAF(N)
          IF(CONVG.EQ.1) THEN
            IF(ABS(SUMF(N)).LE.AirflowNetworkSimu%AbsTol) CYCLE
            IF(ABS(SUMF(N)/SUMAF(N)).GT.AirflowNetworkSimu%RelTol) CONVG = 0
          END IF
        END DO
        ACC0 = ACC1
        IF(SSUMAF.GT.0.0d0) ACC1 = SSUMF / SSUMAF
        IF(CONVG.EQ.1 .AND. ITER.GT.1) RETURN
        IF(ITER.GE.AirflowNetworkSimu%MaxIteration) EXIT
        ! Data dump.
        IF(LIST.GE.3) THEN
          CALL DUMPVD('AD:',AD,NetworkNumOfNodes,Unit21)
          CALL DUMPVD('AU:',AU,NNZE,Unit21)
        END IF
        ! Solve AA * CCF = SUMF.
        DO N=1,NetworkNumOfNodes
          CCF(N) = SUMF(N)
        END DO
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        CALL FACSKY(newAU,AD,newAU,newIK,NetworkNumOfNodes,NSYM) !noel
        CALL SLVSKY(newAU,AD,newAU,CCF,newIK,NetworkNumOfNodes,NSYM) !noel
#else
        CALL FACSKY(AU,AD,AU,IK,NetworkNumOfNodes,NSYM)
        CALL SLVSKY(AU,AD,AU,CCF,IK,NetworkNumOfNodes,NSYM)
#endif
        ! Revise PZ (Steffensen iteration on the N-R correction factors to handle oscillating corrections).
        IF(ACCEL.EQ.1) THEN
          ACCEL = 0
        ELSE
          IF(ITER.GT.2 .AND. ACC1.GT.0.5d0*ACC0) ACCEL = 1
        END IF
        DO N=1,NetworkNumOfNodes
          IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.1) CYCLE
          CEF(N) = 1.0d0
          IF(ACCEL.EQ.1) THEN
            C = CCF(N)/PCF(N)
            IF(C.LT.AirflowNetworkSimu%ConvLimit) CEF(N) = 1.0/(1.0-C)
            C = CCF(N) * CEF(N)
          ELSE
!            IF (CCF(N) .EQ. 0.0) CCF(N)=TINY(CCF(N))  ! 1.0E-40
            IF (CCF(N) .EQ. 0.0d0) CCF(N)=rTinyValue     ! 1.0E-40 (Epsilon)
            PCF(N) = CCF(N)
            C = CCF(N)
          END IF
          IF(ABS(C).GT.AirflowNetworkSimu%MaxPressure) THEN
            CEF(N) = CEF(N)*AirflowNetworkSimu%MaxPressure/ABS(C)
            PZ(N) = PZ(N)-CCF(N)*CEF(N)
          ELSE
            PZ(N) = PZ(N)-C
          END IF
        END DO
        ! Data revision dump.
        IF(LIST.GE.2) THEN
          DO N=1,NetworkNumOfNodes
            IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.0) &
            WRITE(Unit21,901) ' Rev:',N,SUMF(N),CCF(N),CEF(N),PZ(N)
          END DO
        ENDIF
      END DO

      ! Error termination.
      CALL ShowSevereError('Too many iterations (SOLVZP) in Airflow Network simulation')
      AirflowNetworkSimu%ExtLargeOpeningErrCount = AirflowNetworkSimu%ExtLargeOpeningErrCount + 1
      if (AirflowNetworkSimu%ExtLargeOpeningErrCount < 2) then
        CALL ShowWarningError('AirflowNetwork: SOLVER, Changing values for initialization flag, Relative airflow convergence, ' &
         //'Absolute airflow convergence, Convergence acceleration limit or Maximum Iteration Number may solve the problem.')
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('..Iterations='//TRIM(RoundSigDigits(ITER))//', Max allowed='//  &
               TRIM(RoundSigDigits(AirflowNetworkSimu%MaxIteration)))
        CALL ShowFatalError('AirflowNetwork: SOLVER, The previous error causes termination.')
      else
        CALL ShowRecurringWarningErrorAtEnd('AirFlowNetwork: Too many iterations (SOLVZP) in AirflowNetwork ' &
                                            //' simulation continues.',AirflowNetworkSimu%ExtLargeOpeningErrIndex)
      end if

  901 FORMAT(A5,I3,2E14.6,0PF8.4,F24.14)
      RETURN
      END SUBROUTINE SOLVZP


      SUBROUTINE FILJAC(NNZE,LFLAG)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates matrices for solution of flows

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: NNZE   ! number of nonzero entries in the "AU" array.
          INTEGER, INTENT(IN)  :: LFLAG  ! if = 1, use laminar relationship (initialization).


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! I       - component number.
          ! N       - number of node/zone 1.
          ! M       - number of node/zone 2.
          ! F       - flows through the element (kg/s).
          ! DF      - partial derivatives:  DF/DP.
          ! NF      - number of flows, 1 or 2.
!
       INTEGER  I, J, M, N, FLAG, NF
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
       INTEGER LHK,JHK,JHK1, newsum,newh,Nzeros, ispan, thisIK !noel
       LOGICAL allZero ! noel
       LOGICAL, save :: firsttime=.true. ! noel
#endif
       REAL(r64)     X(4)
       REAL(r64) DP, F(2), DF(2)

      ! FLOW:
      DO N=1,NetworkNumOfNodes
        SUMF(N) = 0.0d0
        SUMAF(N) = 0.0d0
        IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.1) THEN
          AD(N) = 1.0d0
        ELSE
          AD(N) = 0.0d0
        END IF
      END DO
      DO N=1,NNZE
        AU(N) = 0.0d0
      END DO
!                              Set up the Jacobian matrix.
      DO I=1,NetworkNumOfLinks
        N = AirflowNetworkLinkageData(I)%NodeNums(1)
        M = AirflowNetworkLinkageData(I)%NodeNums(2)
        !!!! Check array of DP. DpL is used for multizone air flow calculation only
        !!!! and is not for forced air calculation
        if (I .GT. NumOfLinksMultiZone) then
          DP = PZ(N)-PZ(M)+PS(I)+PW(I)
        else
          DP = PZ(N)-PZ(M)+DpL(1,I)+PW(I)
        end if
        IF(LIST.GE.4) WRITE(Unit21,901) 'PS:',I,N,M,PS(I),PW(I),AirflowNetworkLinkSimu(I)%DP
        J = AirflowNetworkLinkageData(I)%CompNum
        Select Case (AirflowNetworkCompData(J)%CompTypeNum)
           Case (CompTypeNum_PLR) ! Distribution system crack component
              CALL AFEPLR(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_DWC) ! Distribution system duct component
              CALL AFEDWC(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_CVF) ! Distribution system constant volume fan component
              CALL AFECFR(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_FAN) ! Distribution system detailed fan component
              CALL AFEFAN(J,LFLAG,DP,I,N,M,F,DF,NF)
!           Case (CompTypeNum_CPF) ! not currently used in EnergyPlus code -- left for compatibility with AirNet
!              CALL AFECPF(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_DMP) ! Distribution system damper component
              CALL AFEDMP(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_ELR) ! Distribution system effective leakage ratio component
              CALL AFEELR(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_CPD) ! Distribution system constant pressure drop component
              CALL AFECPD(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_DOP) ! Detailed opening
              CALL AFEDOP(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_SOP) ! Simple opening
              CALL AFESOP(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_SCR) ! Surface crack component
              CALL AFESCR(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_SEL) ! Surface effective leakage ratio component
              CALL AFESEL(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_COI) ! Distribution system coil component
              CALL AFECOI(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_TMU) ! Distribution system terminal unit component
              CALL AFETMU(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_EXF) ! Exhaust fan component
              CALL AFEEXF(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_HEX) ! Distribution system heat exchanger component
              CALL AFEHEX(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case (CompTypeNum_HOP) ! Horizontal opening
              CALL AFEHOP(J,LFLAG,DP,I,N,M,F,DF,NF)
           Case Default
              CYCLE
        End Select
        AirflowNetworkLinkSimu(I)%DP = DP
        AFLOW(I) = F(1)
        AFLOW2(I) = 0.0d0
        if (AirflowNetworkCompData(J)%CompTypeNum .EQ. CompTypeNum_DOP) then
           AFLOW2(I) = F(2)
        end if
        if (AirflowNetworkCompData(J)%CompTypeNum .EQ. CompTypeNum_HOP) then
           AFLOW2(I) = F(2)
        end if
        IF(LIST.GE.3) WRITE(Unit21,901) ' NRi:',I,N,M,AirflowNetworkLinkSimu(I)%DP,F(1),DF(1)
        FLAG = 1
        IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.0) THEN
          FLAG = FLAG+1
          X(1) =  DF(1)
          X(2) = -DF(1)
          SUMF(N) = SUMF(N)+F(1)
          SUMAF(N) = SUMAF(N)+ABS(F(1))
        END IF
        IF(AirflowNetworkNodeData(M)%NodeTypeNum.EQ.0) THEN
          FLAG = FLAG+2
          X(4) =  DF(1)
          X(3) = -DF(1)
          SUMF(M) = SUMF(M)-F(1)
          SUMAF(M) = SUMAF(M)+ABS(F(1))
        END IF
        IF(FLAG.NE.1) CALL FILSKY(X,AirflowNetworkLinkageData(I)%NodeNums,IK,AU,AD,FLAG)
        IF(NF.EQ.1) CYCLE
        AFLOW2(I) = F(2)
        IF(LIST.GE.3) WRITE(Unit21,901) ' NRj:',I,N,M,AirflowNetworkLinkSimu(I)%DP,F(2),DF(2)
        FLAG = 1
        IF(AirflowNetworkNodeData(N)%NodeTypeNum.EQ.0) THEN
          FLAG = FLAG+1
          X(1) =  DF(2)
          X(2) = -DF(2)
          SUMF(N) = SUMF(N)+F(2)
          SUMAF(N) = SUMAF(N)+ABS(F(2))
        END IF
        IF(AirflowNetworkNodeData(M)%NodeTypeNum.EQ.0) THEN
          FLAG = FLAG+2
          X(4) =  DF(2)
          X(3) = -DF(2)
          SUMF(M) = SUMF(M)-F(2)
          SUMAF(M) = SUMAF(M)+ABS(F(2))
        END IF
        IF(FLAG.NE.1) CALL FILSKY(X,AirflowNetworkLinkageData(I)%NodeNums,IK,AU,AD,FLAG)
      END DO
  901 FORMAT(A5,3I3,4E16.7)

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

     ! After the matrix values have been set, we can look at them and see if any columns are filled with zeros.
     ! If they are, let's remove them from the matrix -- but only for the purposes of doing the solve.
     ! They way I do this is building a separate IK array (newIK) that simply changes the column heights.
     ! So the affected SOLVEs would use this newIK and nothing else changes.
     DO n=1,NetworkNumOfNodes+1
        newIK(n)=IK(n)
        !print*, " NetworkNumOfNodes  n=", n, " IK(n)=", IK(n)
     enddo

     newsum=IK(2)-IK(1) ! always 0?

     JHK=1
     DO n=2,NetworkNumOfNodes
        JHK1 = IK(n+1)  ! starts at IK(3)-IK(2)
        LHK = JHK1-JHK
        IF(LHK.LE.0) then
          newIK(n+1) = newIK(n)
          CYCLE
        endif
        !write(*,'(4(a,i8))') "n=", n, " ik=", ik(n), " JHK=", JHK, " LHK=", LHK

        ! is the entire column zero?  noel
        allZero=.True.
        DO i=0,LHK-1
           if (AU(JHK+i) .ne. 0.0d0) then
              allZero=.False.
              exit
           endif
        enddo

        newh=LHK
        if (allZero .eqv. .True.) then
           !print*, "allzero n=", n
           newh=0
        else
           !DO i=0,LHK-1
           !   write(*, '(2(a,i8),a, f15.3)') "  n=", n, " i=", i, " AU(JHK+i)=", AU(JHK+i)
           !enddo
        endif
        newIK(n+1) = newIK(n) + newh
        newsum = newsum + newh

        !do i = LHK-1,0, -1
        !   write(*, '(2(a,i8),a, f15.3)') "  n=", n, " i=", i, " AU(JHK+i)=", AU(JHK+i)
        !enddo
        JHK = JHK1
     enddo

     ! this is just a print to screen, is not necessary
!     if (firsttime) then
!        write(*, '(2(a,i8))') " After SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS: newsum=", newsum, " oldsum=", IK(NetworkNumOfNodes+1)
!        firsttime=.false.
!     endif


     ! Now fill newAU from AU, using newIK
     thisIK=1
     DO n=2,NetworkNumOfNodes
        thisIK = newIK(n)
        ispan = newIK(n+1) - thisIK

        IF(ispan.LE.0) CYCLE
        DO i=0,ispan-1
           newAU(thisIK+i) = AU(IK(n)+i)
        enddo

     enddo
#endif

      RETURN
      END SUBROUTINE FILJAC


      SUBROUTINE AFEPLR(J,LFLAG,PDROP,I,N,M,F,DF,NF)
!
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a power law resistance airflow component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     CDM, FL, FT
         REAL(r64)     RhozNorm, VisczNorm, expn, Ctl, coef
         INTEGER  CompNum

      ! FLOW:
      ! Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
      CompNum = AirflowNetworkCompData(J)%TypeNum
      RhozNorm = PsyRhoAirFnPbTdbW(101325.0d0,20.0d0,0.0d0)
      VisczNorm = 1.71432d-5+4.828d-8*20.0d0
      expn = DisSysCompLeakData(CompNum)%FlowExpo
      coef = DisSysCompLeakData(CompNum)%FlowCoef

      IF(PDROP.GE.0.0d0) THEN
         coef = coef/SQRTDZ(N)
      ELSE
         coef = Coef/SQRTDZ(M)
      END IF

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          ctl = (RhozNorm/RHOZ(N))**(expn-1.0d0)*(VisczNorm/VISCZ(N))**(2.0d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(N)/VISCZ(N)*ctl
        ELSE
          ctl = (RhozNorm/RHOZ(M))**(expn-1.0d0)*(VisczNorm/VISCZ(M))**(2.0d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(M)/VISCZ(M)*ctl
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction for laminar flow.
          ctl = (RhozNorm/RHOZ(N))**(expn-1.0d0)*(VisczNorm/VISCZ(N))**(2.0d0*expn-1.0d0)
          CDM = Coef*RHOZ(N)/VISCZ(N)*ctl
          FL = CDM*PDROP
          ! Flow in positive direction for turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = Coef*SQRTDZ(N)*SQRT(PDROP)
          ELSE
            FT = Coef*SQRTDZ(N)*(PDROP**expn)
          END IF
        ELSE
          ! Flow in negative direction for laminar flow
          CDM = Coef*RHOZ(M)/VISCZ(M)
          FL = CDM*PDROP
          ! Flow in negative direction for turbulent flow
          IF(expn .EQ. 0.5d0) THEN
            FT = -Coef*SQRTDZ(M)*SQRT(-PDROP)
          ELSE
            FT = -Coef*SQRTDZ(M)*(-PDROP)**expn
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(LIST.GE.4) WRITE(Unit21,901) ' plr: ',I,PDROP,FL,FT
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = FT*expn/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,I3,6X,4E16.7)
      RETURN
      END SUBROUTINE AFEPLR

      SUBROUTINE AFESCR(J,LFLAG,PDROP,I,N,M,F,DF,NF)
!
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a surface crack component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     CDM, FL, FT
         REAL(r64)     RhozNorm, VisczNorm, expn, Ctl, coef, Corr, VisAve, Tave, RhoCor
         INTEGER  CompNum

      ! FLOW:
      ! Crack standard condition from given inputs
      Corr = MultizoneSurfaceData(I)%Factor
      CompNum = AirflowNetworkCompData(J)%TypeNum
      RhozNorm = PsyRhoAirFnPbTdbW(MultizoneSurfaceCrackData(CompNum)%StandardP, &
                 MultizoneSurfaceCrackData(CompNum)%StandardT,MultizoneSurfaceCrackData(CompNum)%StandardW)
      VisczNorm = 1.71432d-5+4.828d-8*MultizoneSurfaceCrackData(CompNum)%StandardT

      expn = MultizoneSurfaceCrackData(CompNum)%FlowExpo
      VisAve = (VISCZ(N)+VISCZ(M))/2.0d0
      Tave = (TZ(N)+TZ(M))/2.0d0
      IF(PDROP.GE.0.0d0) THEN
         coef = MultizoneSurfaceCrackData(CompNum)%FlowCoef/SQRTDZ(N)*Corr
      ELSE
         coef = MultizoneSurfaceCrackData(CompNum)%FlowCoef/SQRTDZ(M)*Corr
      END IF

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          RhoCor = (TZ(N)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(N)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(N)/VISCZ(N)*ctl
        ELSE
          RhoCor = (TZ(M)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(M)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(M)/VISCZ(M)*ctl
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow.
          RhoCor = (TZ(N)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(N)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          CDM = Coef*RHOZ(N)/VISCZ(N)*ctl
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = Coef*SQRTDZ(N)*SQRT(PDROP)*ctl
          ELSE
            FT = Coef*SQRTDZ(N)*(PDROP**expn)*ctl
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow.
          RhoCor = (TZ(M)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(M)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          CDM = Coef*RHOZ(M)/VISCZ(M)*ctl
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = -Coef*SQRTDZ(M)*SQRT(-PDROP)*ctl
          ELSE
            FT = -Coef*SQRTDZ(M)*(-PDROP)**expn*ctl
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(LIST.GE.4) WRITE(Unit21,901) ' scr: ',I,PDROP,FL,FT
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = FT*expn/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,I3,6X,4E16.7)
      RETURN
      END SUBROUTINE AFESCR


      SUBROUTINE AFEDWC(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a duct/pipe component using Colebrook equation for the
          ! turbulent friction factor

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !     RE      - Reynolds number.
          !     FL      - friction factor for laminar flow.
          !     FT      - friction factor for turbulent flow.

         REAL(r64)     A0, A1, A2, B, C, D, EPS, S2, CDM, FL, FT, FTT, RE
         INTEGER  CompNum
         REAL(r64)     ed, ld, g, AA1
!
      DATA C,EPS / 0.868589d0, 0.001d0 /

      ! FLOW:
      CompNum = AirflowNetworkCompData(J)%TypeNum
      ed = DisSysCompDuctData(CompNum)%Rough/DisSysCompDuctData(CompNum)%D
      ld = DisSysCompDuctData(CompNum)%L/DisSysCompDuctData(CompNum)%D
      g  = 1.14d0 - 0.868589d0*LOG(ed)
      AA1 = g

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = (2.d0*RHOZ(N)*DisSysCompDuctData(CompNum)%A*DisSysCompDuctData(CompNum)%D)/ &
                  (VISCZ(N)*DisSysCompDuctData(CompNum)%InitLamCoef*ld)
        ELSE
          DF(1) = (2.d0*RHOZ(M)*DisSysCompDuctData(CompNum)%A*DisSysCompDuctData(CompNum)%D)/ &
                  (VISCZ(M)*DisSysCompDuctData(CompNum)%InitLamCoef*ld)
        END IF
        F(1) = -DF(1)*PDROP
        IF(LIST.GE.4) WRITE(Unit21,901) ' dwi:',I,DisSysCompDuctData(CompNum)%InitLamCoef,F(1),DF(1)
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow coefficient !=0
          IF(DisSysCompDuctData(CompNum)%LamFriCoef.GE.0.001d0) THEN
            A2 = DisSysCompDuctData(CompNum)%LamFriCoef/(2.0d0*RHOZ(N)*DisSysCompDuctData(CompNum)%A* &
                 DisSysCompDuctData(CompNum)%A)
            A1 = (VISCZ(N)*DisSysCompDuctData(CompNum)%LamDynCoef*ld)/ &
                 (2.0d0*RHOZ(N)*DisSysCompDuctData(CompNum)%A*DisSysCompDuctData(CompNum)%D)
            A0 = -PDROP
            CDM = SQRT(A1*A1-4.0d0*A2*A0)
            FL = (CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(N)*DisSysCompDuctData(CompNum)%A*DisSysCompDuctData(CompNum)%D)/ &
                  (VISCZ(N)*DisSysCompDuctData(CompNum)%LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = FL*DisSysCompDuctData(CompNum)%D/(VISCZ(N)*DisSysCompDuctData(CompNum)%A)
          IF(LIST.GE.4) WRITE(Unit21,901) ' dwl:',I,PDROP,FL,CDM,RE
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(2.d0*RHOZ(N)*PDROP)*DisSysCompDuctData(CompNum)%A
            FTT = S2 / SQRT(ld/g**2+DisSysCompDuctData(CompNum)%TurDynCoef)
            IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,S2,FTT,g
            DO
              FT = FTT
              B = (9.3d0*VISCZ(N)*DisSysCompDuctData(CompNum)%A)/(FT*DisSysCompDuctData(CompNum)%Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+DisSysCompDuctData(CompNum)%TurDynCoef)
              IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,B,FTT,g
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = FTT
          ELSE
            FT = FL
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow coefficient !=0
          IF(DisSysCompDuctData(CompNum)%LamFriCoef.GE.0.001d0) THEN
            A2 = DisSysCompDuctData(CompNum)%LamFriCoef/(2.d0*RHOZ(M)*DisSysCompDuctData(CompNum)%A* &
                 DisSysCompDuctData(CompNum)%A)
            A1 = (VISCZ(M)*DisSysCompDuctData(CompNum)%LamDynCoef*ld)/ &
                 (2.d0*RHOZ(M)*DisSysCompDuctData(CompNum)%A*DisSysCompDuctData(CompNum)%D)
            A0 = PDROP
            CDM = SQRT(A1*A1-4.0d0*A2*A0)
            FL = -(CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(M)*DisSysCompDuctData(CompNum)%A*DisSysCompDuctData(CompNum)%D)/ &
                  (VISCZ(M)*DisSysCompDuctData(CompNum)%LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = -FL*DisSysCompDuctData(CompNum)%D/(VISCZ(M)*DisSysCompDuctData(CompNum)%A)
          IF(LIST.GE.4) WRITE(Unit21,901) ' dwl:',I,PDROP,FL,CDM,RE
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(-2.d0*RHOZ(M)*PDROP)*DisSysCompDuctData(CompNum)%A
            FTT = S2 / SQRT(ld/g**2+DisSysCompDuctData(CompNum)%TurDynCoef)
            IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,S2,FTT,g
            DO
              FT = FTT
              B = (9.3d0*VISCZ(M)*DisSysCompDuctData(CompNum)%A)/(FT*DisSysCompDuctData(CompNum)%Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+ DisSysCompDuctData(CompNum)%TurDynCoef)
              IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,B,FTT,g
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = -FTT
          ELSE
            FT = FL
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = 0.5d0*FT/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,I3,6X,4E16.7)

      RETURN
      END SUBROUTINE AFEDWC

      SUBROUTINE AFESOP(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a Doorway airflow component using standard interface.
          ! A doorway may have two-way airflows. Heights measured relative to the bottom of the door.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !     DPMID   - pressure drop at mid-height of doorway.
          !     DRHO    - difference in air densities between rooms.
          !     Y       - height of neutral plane rel. to bottom of door (m).
          !     F0      - flow factor at the bottom of the door.
          !     FH      - flow factor at the top of the door.
          !     DF0     - derivative factor at the bottom of the door.
          !     DFH     - derivative factor at the top of the door.
!
         REAL(r64) DPMID
         REAL(r64)     C, DF0, DFH, DRHO, GDRHO, F0, FH, Y, SQRT2
         INTEGER  CompNum
         REAL(r64) FlowCoef,FlowExpo,MinRhoDiff,DischCoeff,Width,Height,OpenFactor
!
      DATA SQRT2 / 1.414214d0 /

      ! FLOW:
      CompNum = AirflowNetworkCompData(J)%TypeNum
      MinRhoDiff = MultizoneCompSimpleOpeningData(CompNum)%MinRhoDiff
      DischCoeff = MultizoneCompSimpleOpeningData(CompNum)%DischCoeff
      Width = MultizoneSurfaceData(i)%Width
      Height = MultizoneSurfaceData(i)%Height
      FlowCoef = MultizoneCompSimpleOpeningData(CompNum)%FlowCoef*2.0d0*(Width+Height)
      FlowExpo = MultizoneCompSimpleOpeningData(CompNum)%FlowExpo
      OpenFactor = MultizoneSurfaceData(I)%OpenFactor
      If (OpenFactor > 0.0d0) then
        Width = Width*OpenFactor
        If (Surface(MultizoneSurfaceData(I)%SurfNum)%Tilt .LT. 90.d0) then
          Height = Height*Surface(MultizoneSurfaceData(I)%SurfNum)%SinTilt
        End If
      End If

      IF(PDROP.GE.0.0d0) THEN
         FlowCoef = FlowCoef/SQRTDZ(N)
      ELSE
         FlowCoef = FlowCoef/SQRTDZ(M)
      END IF

      ! Add window multiplier with window close
      If (MultizoneSurfaceData(I)%Multiplier > 1.0d0) FlowCoef=FlowCoef*MultizoneSurfaceData(I)%Multiplier
      ! Add window multiplier with window open
      If (OpenFactor .gt. 0.0d0) then
        If (MultizoneSurfaceData(I)%Multiplier > 1.0d0) Width=Width*MultizoneSurfaceData(I)%Multiplier
      End If

      NF = 1
      DRHO = RHOZ(N)-RHOZ(M)
      GDRHO = 9.8d0*DRHO
      IF(LIST.GE.4) WRITE(Unit21,903) ' DOR:',I,N,M,PDROP,ABS(DRHO),MinRhoDiff
      IF(OpenFactor .eq. 0.0d0) THEN
        CALL GenericCrack(FlowCoef,FlowExpo,LFLAG,PDROP,N,M,F,DF,NF)
        Return
      end if
      IF(ABS(DRHO).LT.MinRhoDiff .OR. LFLAG.EQ.1) THEN
        DPMID = PDROP-0.5d0*Height*GDRHO
        ! Initialization or identical temps: treat as one-way flow.
        CALL GenericCrack(FlowCoef,FlowExpo,LFLAG,DPMID,N,M,F,DF,NF)
        IF(LIST.GE.4) WRITE(Unit21,900) ' Drs:',DPMID,F(1),DF(1)
      ELSE
        ! Possible two-way flow:
        Y = PDROP/GDRHO
        IF(LIST.GE.4) WRITE(Unit21,900) ' DrY:',PDROP,GDRHO,Y
        ! F0 = lower flow, FH = upper flow.
        C = SQRT2*Width*DischCoeff
        DF0 = C*SQRT(ABS(PDROP))/ABS(GDRHO)
!        F0 = 0.666667d0*C*SQRT(ABS(GDRHO*Y))*ABS(Y)
        F0 = (2.0d0/3.0d0)*C*SQRT(ABS(GDRHO*Y))*ABS(Y)
        DFH = C*SQRT(ABS((Height-Y)/GDRHO))
!        FH = 0.666667d0*DFH*ABS(GDRHO*(Height-Y))
        FH = (2.0d0/3.0d0)*DFH*ABS(GDRHO*(Height-Y))
        IF(LIST.GE.4) WRITE(Unit21,900) ' DrF:',F0,DF0,FH,DFH
        IF(Y.LE.0.0d0) THEN
          ! One-way flow (negative).
          IF(DRHO.GE.0.0d0) THEN
            F(1) = -SQRTDZ(M)*ABS(FH-F0)
            DF(1) = SQRTDZ(M)*ABS(DFH-DF0)
          ELSE
            F(1) =  SQRTDZ(N)*ABS(FH-F0)
            DF(1) = SQRTDZ(N)*ABS(DFH-DF0)
          END IF
          IF(LIST.GE.4) WRITE(Unit21,900) ' Dr1:',C,F(1),DF(1)
        ELSE IF(Y.GE.Height) THEN
          ! One-way flow (positive).
          IF(DRHO.GE.0.0d0) THEN
            F(1) =  SQRTDZ(N)*ABS(FH-F0)
            DF(1) = SQRTDZ(N)*ABS(DFH-DF0)
          ELSE
            F(1) = -SQRTDZ(M)*ABS(FH-F0)
            DF(1) = SQRTDZ(M)*ABS(DFH-DF0)
          END IF
          IF(LIST.GE.4) WRITE(Unit21,900) ' Dr2:',C,F(1),DF(1)
        ELSE
          ! Two-way flow.
          NF = 2
          IF(DRHO.GE.0.0d0) THEN
            F(1) = -SQRTDZ(M)*FH
            DF(1) = SQRTDZ(M)*DFH
            F(2) =  SQRTDZ(N)*F0
            DF(2) = SQRTDZ(N)*DF0
          ELSE
            F(1) =  SQRTDZ(N)*FH
            DF(1) = SQRTDZ(N)*DFH
            F(2) = -SQRTDZ(M)*F0
            DF(2) = SQRTDZ(M)*DF0
          END IF
          IF(LIST.GE.4) WRITE(Unit21,900) ' Dr3:',C,F(1),DF(1)
          IF(LIST.GE.4) WRITE(Unit21,900) ' Dr4:',C,F(2),DF(2)
        ENDIF
      END IF
  900 FORMAT(A5,9X,4E16.7)
  903 FORMAT(A5,3I3,4E16.7)

      RETURN
      END SUBROUTINE AFESOP


      SUBROUTINE AFECFR(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a constant flow rate airflow component -- using standard interface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE DataLoopNode, ONLY: Node
          USE DataAirLoop,  ONLY: LoopSystemOnMassFlowrate,LoopSystemOffMassFlowrate,LoopFanOperationMode,LoopCompCycRatio
          USE DataHVACGlobals, ONLY: FanType_SimpleOnOff, FanType_SimpleConstVolume, FanType_SimpleVAV

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          INTEGER, PARAMETER :: CycFanCycComp    = 1 ! fan cycles with compressor operation


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          INTEGER CompNum, k,k1
          REAL(r64) :: SumTermFlow  ! Sum of all Terminal flows [kg/s]
          REAL(r64) :: SumFracSuppLeak  ! Sum of all supply leaks as a fraction of supply fan flow rate
          Integer Node1, Node2

      ! FLOW:
      CompNum = AirflowNetworkCompData(J)%TypeNum

      NF = 1
      If (DisSysCompCVFData(CompNum)%FanTypeNum .eq. FanType_SimpleOnOff) then
        If (LoopFanOperationMode .EQ. CycFanCycComp .and. LoopSystemOnMassFlowrate .GT. 0.0d0) then
          F(1) = LoopSystemOnMassFlowrate
        else
          F(1) = Node(DisSysCompCVFData(CompNum)%InletNode)%MassFlowrate*DisSysCompCVFData(CompNum)%Ctrl
          If (MultiSpeedHPIndicator .EQ. 2) Then
            F(1) = LoopSystemOnMassFlowrate*LoopCompCycRatio+LoopSystemOffMassFlowrate*(1.0d0-LoopCompCycRatio)
          End If
        End If
      Else If (DisSysCompCVFData(CompNum)%FanTypeNum .eq. FanType_SimpleConstVolume) Then
        If (DisSysCompCVFData(CompNum)%FlowRate > 0) Then
          F(1) = DisSysCompCVFData(CompNum)%FlowRate*DisSysCompCVFData(CompNum)%Ctrl
        Else
          F(1) = Node(DisSysCompCVFData(CompNum)%InletNode)%MassFlowrate*DisSysCompCVFData(CompNum)%Ctrl
        End If
        If (MultiSpeedHPIndicator .EQ. 2) Then
          F(1) = LoopSystemOnMassFlowrate
        End If
      Else If (DisSysCompCVFData(CompNum)%FanTypeNum .eq. FanType_SimpleVAV) Then
        ! Check VAV termals with a damper
        SumTermFlow = 0.d0
        SumFracSuppLeak = 0.d0
        Do k=1,NetworkNumOfLinks
          If (AirflowNetworkLinkageData(k)%VAVTermDamper) Then
            k1 = AirflowNetworkNodeData(AirflowNetworkLinkageData(k)%NodeNums(1))%EPlusNodeNum
            If (Node(k1)%MassFlowRate .GT. 0.0d0) Then
              SumTermFlow = SumTermFlow + Node(k1)%MassFlowRate
            End If
          End If
          If (AirflowNetworkCompData(AirflowNetworkLinkageData(k)%CompNum)%CompTypeNum == CompTypeNum_ELR) then
            ! Calculate supply leak sensible losses
            Node1 = AirflowNetworkLinkageData(k)%NodeNums(1)
            Node2 = AirflowNetworkLinkageData(k)%NodeNums(2)
            if ((AirflowNetworkNodeData(Node2)%EPlusZoneNum > 0) .AND. (AirflowNetworkNodeData(Node1)%EPlusNodeNum == 0)) Then
              SumFracSuppLeak = SumFracSuppLeak + &
                DisSysCompELRData(AirflowNetworkCompData(AirflowNetworkLinkageData(k)%CompNum)%TypeNum)%ELR
            End If
          End If
        End Do
        F(1) = SumTermFlow/(1.d0-SumFracSuppLeak)
        VAVTerminalRatio = 0.d0
        If (F(1) .GT. DisSysCompCVFData(CompNum)%MaxAirMassFlowRate) Then
          VAVTerminalRatio = DisSysCompCVFData(CompNum)%MaxAirMassFlowRate/F(1)
          F(1) = DisSysCompCVFData(CompNum)%MaxAirMassFlowRate
        End If
      End If
      DF(1) = 0.0d0

      RETURN
      END SUBROUTINE AFECFR


      SUBROUTINE AFEFAN(JA,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a detailed fan component -- using standard interface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: JA    ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !     PRISE   - pressure rise (negative of pressure drop) (Pa).
!
         INTEGER  J, K, L
         REAL(r64)     DPDF, PRISE, BX, BY, CX, CY, CCY, DX, DY, TOL
         INTEGER  CompNum, NumCur
         REAL(r64)     FlowCoef, FlowExpo

      DATA TOL / 0.00001d0 /

      ! FLOW:
      CompNum = AirflowNetworkCompData(JA)%TypeNum
      NumCur = DisSysCompDetFanData(CompNum)%n
      FlowCoef = DisSysCompDetFanData(CompNum)%FlowCoef
      FlowExpo = DisSysCompDetFanData(CompNum)%FlowExpo

      NF = 1
      IF(AFECTL(I).LE.0.0d0) THEN
        ! Speed = 0; treat fan as resistance.
        CALL GenericCrack(FlowCoef,FlowExpo,LFLAG,PDROP,N,M,F,DF,NF)
        RETURN
      END IF
      ! Pressure rise at reference fan speed.
      IF(AFECTL(I).GE.DisSysCompDetFanData(CompNum)%TranRat) THEN
        PRISE = -PDROP*(DisSysCompDetFanData(CompNum)%RhoAir/RHOZ(N))/AFECTL(I)**2
      ELSE
        PRISE = -PDROP*(DisSysCompDetFanData(CompNum)%RhoAir/RHOZ(N))/ &
                (DisSysCompDetFanData(CompNum)%TranRat*AFECTL(I))
      END IF
      IF(LIST.GE.4) WRITE(Unit21,901) ' fan:',I,PDROP,PRISE,AFECTL(I),DisSysCompDetFanData(CompNum)%TranRat
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear approximation.
        F(1) = -DisSysCompDetFanData(CompNum)%Qfree*AFECTL(I)*(1.0d0-PRISE/DisSysCompDetFanData(CompNum)%Pshut)
        DPDF = -DisSysCompDetFanData(CompNum)%Pshut/DisSysCompDetFanData(CompNum)%Qfree
        IF(LIST.GE.4) WRITE(Unit21,901) ' fni:',JA,DisSysCompDetFanData(CompNum)%Qfree, &
                                               DisSysCompDetFanData(CompNum)%Pshut
      ELSE
        ! Solution of the fan performance curve.
        ! Determine curve fit range.
        J = 1
        K = 5*(J-1)+1
        BX = DisSysCompDetFanData(CompNum)%Coeff(K)
        BY = DisSysCompDetFanData(CompNum)%Coeff(K+1)+BX*(DisSysCompDetFanData(CompNum)%Coeff(K+2)+ &
          BX*(DisSysCompDetFanData(CompNum)%Coeff(K+3)+BX*DisSysCompDetFanData(CompNum)%Coeff(K+4)))-PRISE
        IF(BY.LT.0.0d0) CALL ShowFatalError('Out of range, too low in an AirflowNetwork detailed Fan')

        DO
          DX = DisSysCompDetFanData(CompNum)%Coeff(K+5)
          DY = DisSysCompDetFanData(CompNum)%Coeff(K+1)+DX*(DisSysCompDetFanData(CompNum)%Coeff(K+2)+ &
               DX*(DisSysCompDetFanData(CompNum)%Coeff(K+3)+DX*DisSysCompDetFanData(CompNum)%Coeff(K+5)))-PRISE
          IF(LIST.GE.4) WRITE(Unit21,901) ' fp0:',J,BX,BY,DX,DY
          IF(BY*DY.LE.0.0d0) EXIT
          J = J+1
          IF(J.GT.NumCur) &
            CALL ShowFatalError('Out of range, too high (FAN) in ADS simulation')
          K = K+5
          BX = DX
          BY = DY
        END DO
        ! Determine reference mass flow rate by false position method.
        L = 0
        CY = 0.0d0
   40     CONTINUE
          L = L+1
          IF(L.GT.100) CALL ShowFatalError('Too many iterations (FAN) in AirflowNtework simulation')
          CCY = CY
          CX = BX-BY*((DX-BX)/(DY-BY))
          CY = DisSysCompDetFanData(CompNum)%Coeff(K+1)+CX*(DisSysCompDetFanData(CompNum)%Coeff(K+2)+ &
             CX*(DisSysCompDetFanData(CompNum)%Coeff(K+3)+CX*DisSysCompDetFanData(CompNum)%Coeff(K+4)))-PRISE
          IF(BY*CY .EQ. 0.0d0) GOTO 90
          IF(BY*CY .GT. 0.0d0) GOTO 60
   50     DX = CX
          DY = CY
          IF(CY*CCY.GT.0.0d0) BY = 0.5d0*BY
          GO TO 70
   60     BX = CX
          BY = CY
          IF(CY*CCY.GT.0.0d0) DY = 0.5d0*DY
   70     CONTINUE
          IF(LIST.GE.4) WRITE(Unit21,901) ' fpi:',J,BX,CX,DX,BY,DY
          IF(DX-BX.LT.TOL*CX) GO TO 80
          IF(DX-BX.LT.TOL) GO TO 80
          GO TO 40
   80   CX = 0.5d0*(BX+DX)
   90   F(1) = CX
        DPDF = DisSysCompDetFanData(CompNum)%Coeff(K+2)+CX*(2.0d0*DisSysCompDetFanData(CompNum)%Coeff(K+3)+ &
               CX*3.0d0*DisSysCompDetFanData(CompNum)%Coeff(K+4))
      END IF
      ! Convert to flow at given speed.
      F(1) = F(1)*(RHOZ(N)/DisSysCompDetFanData(CompNum)%RhoAir)*AFECTL(I)
      ! Set derivative w/r pressure drop (-).
      IF(AFECTL(I).GE.DisSysCompDetFanData(CompNum)%TranRat) THEN
        DF(1) = -AFECTL(I)/DPDF
      ELSE
        DF(1) = -1.0d0/DPDF
      END IF
  901 FORMAT(A5,I3,5E14.6)

  999 RETURN
      END SUBROUTINE AFEFAN

      ! The above subroutine is not used. Leave it for the time being and revise later.

      SUBROUTINE AFECPF(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a constant power simple fan component -- using standard interface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

      ! FLOW:
      NF = 1
      IF(LFLAG.EQ.1) THEN
        F(1) = AFECTL(I)
        DF(1) = F(1)
      ELSE
        F(1) = -AFECTL(I)/PDROP
        DF(1) = -F(1)/PDROP
      END IF

      RETURN
      END SUBROUTINE AFECPF


! Leave it for the time being and revise later. Or drop this component ???????????

      SUBROUTINE AFEDMP(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a Controlled power law resistance airflow component (damper)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     C
         INTEGER  CompNum

      ! FLOW:
      ! Get component number
      CompNum = AirflowNetworkCompData(J)%TypeNum

      NF = 1
      C = AFECTL(I)
      IF(C.LT.DisSysCompDamperData(CompNum)%FlowMin) C = DisSysCompDamperData(CompNum)%FlowMin
      IF(C.GT.DisSysCompDamperData(CompNum)%FlowMax) C = DisSysCompDamperData(CompNum)%FlowMax
      C = DisSysCompDamperData(CompNum)%A0+C*(DisSysCompDamperData(CompNum)%A1+C* &
          (DisSysCompDamperData(CompNum)%A2+C*DisSysCompDamperData(CompNum)%A3))
      IF(LIST.GE.4) WRITE(Unit21,901) ' Dmp:',I,AFECTL(I),DisSysCompDamperData(CompNum)%FlowMin, &
                                      DisSysCompDamperData(CompNum)%FlowMax,C
      IF(LFLAG.EQ.1 .OR. ABS(PDROP).LE.DisSysCompDamperData(CompNum)%LTP) THEN
!                              Laminar flow.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = C*DisSysCompDamperData(CompNum)%LamFlow*RHOZ(N)/VISCZ(N)
        ELSE
          DF(1) = C*DisSysCompDamperData(CompNum)%LamFlow*RHOZ(M)/VISCZ(M)
        END IF
        F(1) = DF(1)*PDROP
      ELSE
!                              Turbulent flow.
        IF(PDROP.GE.0.0d0) THEN
          F(1) = C*DisSysCompDamperData(CompNum)%TurFlow*SQRTDZ(N)*PDROP**DisSysCompDamperData(CompNum)%FlowExpo
        ELSE
          F(1) = -C*DisSysCompDamperData(CompNum)%TurFlow*SQRTDZ(M)*(-PDROP)**DisSysCompDamperData(CompNum)%FlowExpo
        ENDIF
        DF(1) = F(1)*DisSysCompDamperData(CompNum)%FlowExpo/PDROP
      END IF
  901 FORMAT(A5,I3,6X,4E16.7)

      RETURN
      END SUBROUTINE AFEDMP


      SUBROUTINE AFESEL(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a Surface effective leakage area component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     CDM, FL, FT
         REAL(r64) FlowCoef,FlowExpo
         INTEGER CompNum

      ! FLOW:
      ! Get component properties
      CompNum  = AirflowNetworkCompData(J)%TypeNum
      FlowExpo = MultizoneSurfaceELAData(CompNum)%FlowExpo
      FlowCoef = MultizoneSurfaceELAData(CompNum)%ELA*MultizoneSurfaceELAData(CompNum)%DischCoeff*(2)**0.5d0* &
                 MultizoneSurfaceELAData(CompNum)%RefDeltaP**(0.5d0-FlowExpo)

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = FlowCoef*RHOZ(N)/VISCZ(N)
        ELSE
          DF(1) = FlowCoef*RHOZ(M)/VISCZ(M)
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow.
          CDM = FlowCoef*RHOZ(N)/VISCZ(N)
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(MultizoneSurfaceELAData(CompNum)%FlowExpo.EQ.0.5d0) THEN
            FT = FlowCoef*SQRTDZ(N)*SQRT(PDROP)
          ELSE
            FT = FlowCoef*SQRTDZ(N)*(PDROP**FlowExpo)
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow.
          CDM = FlowCoef*RHOZ(M)/VISCZ(M)
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(FlowExpo .EQ.0.5d0) THEN
            FT = -FlowCoef*SQRTDZ(M)*SQRT(-PDROP)
          ELSE
            FT = -FlowCoef*SQRTDZ(M)*(-PDROP)**FlowExpo
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(LIST.GE.4) WRITE(Unit21,901) ' plr: ',I,PDROP,FL,FT
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = FT*FlowExpo/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,I3,6X,4E16.7)

      RETURN
      END SUBROUTINE AFESEL


      SUBROUTINE AFEELR(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a Effective leakage ratio component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     CDM, FL, FT
         REAL(r64) FlowCoef
         INTEGER CompNum

      ! FLOW:
      ! Get component properties
      CompNum  = AirflowNetworkCompData(J)%TypeNum
      FlowCoef = DisSysCompELRData(CompNum)%ELR*DisSysCompELRData(CompNum)%FlowRate/RHOZ(N)* &
                 DisSysCompELRData(CompNum)%RefPres**(-DisSysCompELRData(CompNum)%FlowExpo)

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = FlowCoef*RHOZ(N)/VISCZ(N)
        ELSE
          DF(1) = FlowCoef*RHOZ(M)/VISCZ(M)
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow.
          CDM = FlowCoef*RHOZ(N)/VISCZ(N)
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(DisSysCompELRData(CompNum)%FlowExpo.EQ.0.5d0) THEN
            FT = FlowCoef*SQRTDZ(N)*SQRT(PDROP)
          ELSE
            FT = FlowCoef*SQRTDZ(N)*(PDROP**DisSysCompELRData(CompNum)%FlowExpo)
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow.
          CDM = FlowCoef*RHOZ(M)/VISCZ(M)
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(DisSysCompELRData(CompNum)%FlowExpo.EQ.0.5d0) THEN
            FT = -FlowCoef*SQRTDZ(M)*SQRT(-PDROP)
          ELSE
            FT = -FlowCoef*SQRTDZ(M)*(-PDROP)**DisSysCompELRData(CompNum)%FlowExpo
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(LIST.GE.4) WRITE(Unit21,901) ' plr: ',I,PDROP,FL,FT
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = FT*DisSysCompELRData(CompNum)%FlowExpo/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,I3,6X,4E16.7)

      RETURN
      END SUBROUTINE AFEELR


      SUBROUTINE AFECPD(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a Constant pressure drop component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(INOUT) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     Co
         INTEGER  CompNum,K

      ! FLOW:
      ! Get component properties
      ! A  = Cross section area [m2]
      ! DP = Pressure difference across the element [Pa]
      CompNum  = AirflowNetworkCompData(J)%TypeNum

      NF = 1
      if (PDROP .EQ. 0.0d0) then
         F(1) = SQRT(2.0d0*RHOZ(N))*DisSysCompCPDData(CompNum)%A*DisSysCompCPDData(CompNum)%DP**0.5d0
         DF(1) = 0.5d0*F(1)/DisSysCompCPDData(CompNum)%DP
      else
         DO k=1,NetworkNumOfLinks
            if (AirflowNetworkLinkageData(k)%NodeNums(2) .EQ. N) then
               F(1) = AFLOW(k)
               Exit
            end if
         end do
         PDROP = DisSysCompCPDData(CompNum)%DP
         PZ(M) = PZ(N) -PDROP
         Co=F(1)/DisSysCompCPDData(CompNum)%DP
         DF(1) = 10.d10
      end if

      RETURN
      END SUBROUTINE AFECPD

      SUBROUTINE AFECOI(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a coil component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     A0, A1, A2, B, C, D, EPS, S2, CDM, FL, FT, FTT, RE
         INTEGER  CompNum
         REAL(r64)     ed, ld, g, AA1, rough,area,InitLamCoef,LamDynCoef,LamFriCoef,TurDynCoef

      DATA C,EPS / 0.868589d0, 0.001d0 /
      DATA rough/0.0001d0/
      DATA InitLamCoef,LamDynCoef,LamFriCoef,TurDynCoef/128.0d0,64.0d0,0.0001d0,0.0001d0/

      ! FLOW:
      ! Get component properties
      CompNum = AirflowNetworkCompData(J)%TypeNum
      ed = Rough/DisSysCompDuctData(CompNum)%D
      area = (DisSysCompCoilData(CompNum)%D)**2*PI
      ld = DisSysCompCoilData(CompNum)%L/DisSysCompCoilData(CompNum)%D
      g  = 1.14d0 - 0.868589d0*LOG(ed)
      AA1 = g

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = (2.d0*RHOZ(N)*area*DisSysCompCoilData(CompNum)%D)/(VISCZ(N)*InitLamCoef*ld)
        ELSE
          DF(1) = (2.d0*RHOZ(M)*Area*DisSysCompCoilData(CompNum)%D)/(VISCZ(M)*InitLamCoef*ld)
        END IF
        F(1) = -DF(1)*PDROP
        IF(LIST.GE.4) WRITE(Unit21,901) ' dwi:',I,InitLamCoef,F(1),DF(1)
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow coefficient !=0
          IF(LamFriCoef.GE.0.001d0) THEN
            A2 = LamFriCoef/(2.0d0*RHOZ(N)*Area*Area)
            A1 = (VISCZ(N)*LamDynCoef*ld)/(2.d0*RHOZ(N)*Area*DisSysCompCoilData(CompNum)%D)
            A0 = -PDROP
            CDM = SQRT(A1*A1-4.d0*A2*A0)
            FL = (CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(N)*Area*DisSysCompCoilData(CompNum)%D)/(VISCZ(N)*LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = FL*DisSysCompCoilData(CompNum)%D/(VISCZ(N)*Area)
          IF(LIST.GE.4) WRITE(Unit21,901) ' dwl:',I,PDROP,FL,CDM,RE
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(2.d0*RHOZ(N)*PDROP)*Area
            FTT = S2 / SQRT(ld/g**2+TurDynCoef)
            IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,S2,FTT,g
            DO
              FT = FTT
              B = (9.3d0*VISCZ(N)*Area)/(FT*Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+TurDynCoef)
              IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,B,FTT,g
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = FTT
          ELSE
            FT = FL
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow coefficient !=0
          IF(LamFriCoef.GE.0.001d0) THEN
            A2 = LamFriCoef/(2.d0*RHOZ(M)*Area*Area)
            A1 = (VISCZ(M)*LamDynCoef*ld)/(2.d0*RHOZ(M)*Area*DisSysCompCoilData(CompNum)%D)
            A0 = PDROP
            CDM = SQRT(A1*A1-4.d0*A2*A0)
            FL = -(CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(M)*Area*DisSysCompCoilData(CompNum)%D)/(VISCZ(M)*LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = -FL*DisSysCompCoilData(CompNum)%D/(VISCZ(M)*Area)
          IF(LIST.GE.4) WRITE(Unit21,901) ' dwl:',I,PDROP,FL,CDM,RE
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(-2.d0*RHOZ(M)*PDROP)*Area
            FTT = S2 / SQRT(ld/g**2+TurDynCoef)
            IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,S2,FTT,g
            DO
              FT = FTT
              B = (9.3d0*VISCZ(M)*Area)/(FT*Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+TurDynCoef)
              IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,B,FTT,g
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = -FTT
          ELSE
            FT = FL
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = 0.5d0*FT/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,I3,6X,4E16.7)

      RETURN
      END SUBROUTINE AFECOI

      SUBROUTINE AFETMU(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a terminal unit component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE DataLoopNode, ONLY: Node

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     A0, A1, A2, B, C, D, EPS, S2, CDM, FL, FT, FTT, RE
         INTEGER  CompNum
         REAL(r64)     ed, ld, g, AA1, rough,Area,InitLamCoef,LamDynCoef,LamFriCoef,TurDynCoef

      DATA C,EPS / 0.868589d0, 0.001d0 /
      DATA rough/0.0001d0/
      DATA InitLamCoef,LamDynCoef,LamFriCoef,TurDynCoef/128.0d0,64.0d0,0.0001d0,0.0001d0/

      ! FLOW:
      ! Get component properties
      CompNum = AirflowNetworkCompData(J)%TypeNum
      ed = Rough/DisSysCompTermUnitData(CompNum)%D
      Area = (DisSysCompTermUnitData(CompNum)%D)**2*PI
      ld = DisSysCompTermUnitData(CompNum)%L/DisSysCompTermUnitData(CompNum)%D
      g  = 1.14d0 - 0.868589d0*LOG(ed)
      AA1 = g

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = (2.d0*RHOZ(N)*area*DisSysCompTermUnitData(CompNum)%D)/(VISCZ(N)*InitLamCoef*ld)
        ELSE
          DF(1) = (2.d0*RHOZ(M)*Area*DisSysCompTermUnitData(CompNum)%D)/(VISCZ(M)*InitLamCoef*ld)
        END IF
        F(1) = -DF(1)*PDROP
        IF(LIST.GE.4) WRITE(Unit21,901) ' dwi:',I,InitLamCoef,F(1),DF(1)
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow coefficient !=0
          IF(LamFriCoef.GE.0.001d0) THEN
            A2 = LamFriCoef/(2.d0*RHOZ(N)*Area*Area)
            A1 = (VISCZ(N)*LamDynCoef*ld)/(2.d0*RHOZ(N)*Area*DisSysCompTermUnitData(CompNum)%D)
            A0 = -PDROP
            CDM = SQRT(A1*A1-4.d0*A2*A0)
            FL = (CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(N)*Area*DisSysCompTermUnitData(CompNum)%D)/(VISCZ(N)*LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = FL*DisSysCompTermUnitData(CompNum)%D/(VISCZ(N)*Area)
          IF(LIST.GE.4) WRITE(Unit21,901) ' dwl:',I,PDROP,FL,CDM,RE
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(2.d0*RHOZ(N)*PDROP)*Area
            FTT = S2 / SQRT(ld/g**2+TurDynCoef)
            IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,S2,FTT,g
            DO
              FT = FTT
              B = (9.3d0*VISCZ(N)*Area)/(FT*Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+TurDynCoef)
              IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,B,FTT,g
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = FTT
          ELSE
            FT = FL
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow coefficient !=0
          IF(LamFriCoef.GE.0.001d0) THEN
            A2 = LamFriCoef/(2.d0*RHOZ(M)*Area*Area)
            A1 = (VISCZ(M)*LamDynCoef*ld)/(2.0d0*RHOZ(M)*Area*DisSysCompTermUnitData(CompNum)%D)
            A0 = PDROP
            CDM = SQRT(A1*A1-4.d0*A2*A0)
            FL = -(CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(M)*Area*DisSysCompTermUnitData(CompNum)%D)/(VISCZ(M)*LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = -FL*DisSysCompTermUnitData(CompNum)%D/(VISCZ(M)*Area)
          IF(LIST.GE.4) WRITE(Unit21,901) ' dwl:',I,PDROP,FL,CDM,RE
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(-2.d0*RHOZ(M)*PDROP)*Area
            FTT = S2 / SQRT(ld/g**2+TurDynCoef)
            IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,S2,FTT,g
            DO
              FT = FTT
              B = (9.3d0*VISCZ(M)*Area)/(FT*Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+TurDynCoef)
              IF(LIST.GE.4) WRITE(Unit21,901) ' dwt:',I,B,FTT,g
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = -FTT
          ELSE
            FT = FL
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = 0.5d0*FT/PDROP
        END IF
      END IF
      ! If damper, setup the airflows from nodal values calculated from teminal
      If (AirflowNetworkLinkageData(I)%VAVTermDamper) Then
        F(1) = Node(DisSysCompTermUnitData(CompNum)%DamperInletNode)%MassFlowrate
        If (VAVTerminalRatio .gt. 0.d0) Then
          F(1) = F(1)*VAVTerminalRatio
        End If
        DF(1) = 0.0d0
      End IF

!
  901 FORMAT(A5,I3,6X,4E16.7)

      RETURN
      END SUBROUTINE AFETMU

      SUBROUTINE AFEEXF(J,LFLAG,PDROP,I,N,M,F,DF,NF)
!
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 12/17/06
          !                      Revised for zone exhaust fan
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a surface crack component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE DataLoopNode,    ONLY: Node
          USE DataHVACGlobals, ONLY: VerySmallMassFlow

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     CDM, FL, FT
         REAL(r64)     RhozNorm, VisczNorm, expn, Ctl, coef, Corr, VisAve, Tave, RhoCor
         INTEGER  CompNum,InletNode

    ! FLOW:
    CompNum = AirflowNetworkCompData(J)%TypeNum
    InletNode = MultizoneCompExhaustFanData(CompNum)%InletNode
    If (Node(InletNode)%MassFlowRate > VerySmallMassFlow) then
      ! Treat the component as an exhaust fan
      NF = 1
      F(1) = Node(InletNode)%MassFlowRate
      DF(1) = 0.0d0
      RETURN
    Else
      ! Treat the component as a surface crack
      ! Crack standard condition from given inputs
      Corr = MultizoneSurfaceData(I)%Factor
      RhozNorm = PsyRhoAirFnPbTdbW(MultizoneCompExhaustFanData(CompNum)%StandardP, &
                 MultizoneCompExhaustFanData(CompNum)%StandardT,MultizoneCompExhaustFanData(CompNum)%StandardW)
      VisczNorm = 1.71432d-5+4.828d-8*MultizoneCompExhaustFanData(CompNum)%StandardT

      expn = MultizoneCompExhaustFanData(CompNum)%FlowExpo
      VisAve = (VISCZ(N)+VISCZ(M))/2.0d0
      Tave = (TZ(N)+TZ(M))/2.0d0
      IF(PDROP.GE.0.0d0) THEN
         coef = MultizoneCompExhaustFanData(CompNum)%FlowCoef/SQRTDZ(N)*Corr
      ELSE
         coef = MultizoneCompExhaustFanData(CompNum)%FlowCoef/SQRTDZ(M)*Corr
      END IF

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          RhoCor = (TZ(N)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(N)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(N)/VISCZ(N)*ctl
        ELSE
          RhoCor = (TZ(M)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(M)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(M)/VISCZ(M)*ctl
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow.
          RhoCor = (TZ(N)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(N)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          CDM = Coef*RHOZ(N)/VISCZ(N)*ctl
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = Coef*SQRTDZ(N)*SQRT(PDROP)*ctl
          ELSE
            FT = Coef*SQRTDZ(N)*(PDROP**expn)*ctl
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow.
          RhoCor = (TZ(M)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(M)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          CDM = Coef*RHOZ(M)/VISCZ(M)*ctl
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = -Coef*SQRTDZ(M)*SQRT(-PDROP)*ctl
          ELSE
            FT = -Coef*SQRTDZ(M)*(-PDROP)**expn*ctl
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(LIST.GE.4) WRITE(Unit21,901) ' scr: ',I,PDROP,FL,FT
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = FT*expn/PDROP
        END IF
      END IF
    End If
!
  901 FORMAT(A5,I3,6X,4E16.7)
      RETURN
      END SUBROUTINE AFEEXF

      SUBROUTINE AFEHEX(J,LFLAG,PDROP,I,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 1/18/09
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a heat exchanger component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     A0, A1, A2, B, C, D, EPS, S2, CDM, FL, FT, FTT, RE
         INTEGER  CompNum
         REAL(r64)     ed, ld, g, AA1, rough,area,InitLamCoef,LamDynCoef,LamFriCoef,TurDynCoef

      DATA C,EPS / 0.868589d0, 0.001d0 /
      DATA rough/0.0001d0/
      DATA InitLamCoef,LamDynCoef,LamFriCoef,TurDynCoef/128.0d0,64.0d0,0.0001d0,0.0001d0/

      ! FLOW:
      ! Get component properties
      CompNum = AirflowNetworkCompData(J)%TypeNum
      ed = Rough/DisSysCompHXData(CompNum)%D
      area = (DisSysCompHXData(CompNum)%D)**2*PI
      ld = DisSysCompHXData(CompNum)%L/DisSysCompHXData(CompNum)%D
      g  = 1.14d0 - 0.868589d0*LOG(ed)
      AA1 = g

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          DF(1) = (2.d0*RHOZ(N)*area*DisSysCompHXData(CompNum)%D)/(VISCZ(N)*InitLamCoef*ld)
        ELSE
          DF(1) = (2.d0*RHOZ(M)*Area*DisSysCompHXData(CompNum)%D)/(VISCZ(M)*InitLamCoef*ld)
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow coefficient !=0
          IF(LamFriCoef.GE.0.001d0) THEN
            A2 = LamFriCoef/(2.0d0*RHOZ(N)*Area*Area)
            A1 = (VISCZ(N)*LamDynCoef*ld)/(2.d0*RHOZ(N)*Area*DisSysCompHXData(CompNum)%D)
            A0 = -PDROP
            CDM = SQRT(A1*A1-4.d0*A2*A0)
            FL = (CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(N)*Area*DisSysCompHXData(CompNum)%D)/(VISCZ(N)*LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = FL*DisSysCompHXData(CompNum)%D/(VISCZ(N)*Area)
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.0d0) THEN
            S2 = SQRT(2.d0*RHOZ(N)*PDROP)*Area
            FTT = S2 / SQRT(ld/g**2+TurDynCoef)
            DO
              FT = FTT
              B = (9.3d0*VISCZ(N)*Area)/(FT*Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+TurDynCoef)
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = FTT
          ELSE
            FT = FL
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow coefficient !=0
          IF(LamFriCoef.GE.0.001d0) THEN
            A2 = LamFriCoef/(2.d0*RHOZ(M)*Area*Area)
            A1 = (VISCZ(M)*LamDynCoef*ld)/(2.d0*RHOZ(M)*Area*DisSysCompHXData(CompNum)%D)
            A0 = PDROP
            CDM = SQRT(A1*A1-4.d0*A2*A0)
            FL = -(CDM-A1)/(2.d0*A2)
            CDM = 1.0d0/CDM
          ELSE
            CDM = (2.d0*RHOZ(M)*Area*DisSysCompHXData(CompNum)%D)/(VISCZ(M)*LamDynCoef*ld)
            FL = CDM*PDROP
          END IF
          RE = -FL*DisSysCompHXData(CompNum)%D/(VISCZ(M)*Area)
          ! Turbulent flow; test when Re>10.
          IF(RE.GE.10.d0) THEN
            S2 = SQRT(-2.d0*RHOZ(M)*PDROP)*Area
            FTT = S2 / SQRT(ld/g**2+TurDynCoef)
            DO
              FT = FTT
              B = (9.3d0*VISCZ(M)*Area)/(FT*Rough)
              D = 1.0d0 + g*B
              g = g - (g-AA1+C*LOG(D))/(1.0d0+C*B/D)
              FTT = S2 / SQRT(ld/g**2+TurDynCoef)
              IF(ABS(FTT-FT)/FTT .LT. EPS) EXIT
            ENDDO
            FT = -FTT
          ELSE
            FT = FL
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = 0.5d0*FT/PDROP
        END IF
      END IF

      RETURN
      END SUBROUTINE AFEHEX

      SUBROUTINE AFEHOP(J,LFLAG,PDROP,I,N,M,F,DF,NF)
!
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Apr. 2009
          !       MODIFIED       na
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a horizontal opening component. The subroutine was
          ! developed based on the subroutine AFEPLR of AIRNET.

          ! METHODOLOGY EMPLOYED:
          ! Combine forced and buyancy airflows together with a cap

          ! REFERENCES:
          ! Cooper, L., 1989, "Calculation of the Flow Through a Horizontal Ceiling/Floor Vent,"
          ! NISTIR 89-4052, National Institute of Standards and Technology, Gaithersburg, MD

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN)     :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: I     ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT)    :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT)    :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          REAL(r64), PARAMETER :: PI = 3.14159265358979323846d0


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     RhozAver, expn, coef
         INTEGER  CompNum
         REAL(r64)     Width            ! Opening width
         REAL(r64)     Height           ! Opening height
         REAL(r64)     Fact             ! Opening factor
         REAL(r64)     Slope            ! Opening slope
         REAL(r64)     DischCoeff       ! Discharge coefficient
         REAL(r64)     fma12            ! massflow in direction "from-to" [kg/s]
         REAL(r64)     fma21            ! massflow in direction "to-from" [kg/s]
         REAL(r64)     dp1fma12         ! derivative d fma12 / d Dp [kg/s/Pa]
         REAL(r64)     dp1fma21         ! derivative d fma21 / d Dp [kg/s/Pa]
         REAL(r64)     PurgedP          ! Purge pressure [Pa]
         REAL(r64)     BuoFlow          ! Buoyancy flow rate [Pa]
         REAL(r64)     BuoFlowMax       ! Maximum buoyancy flow rate [Pa]
         REAL(r64)     dPBuoFlow        ! Derivative of buoyancy flow rate [kg/s/Pa]
         REAL(r64)     DH               ! Hydraulic diameter [m]
         REAL(r64)     Cshape           ! Shape factor [dimensionless]
         REAL(r64)     OpenArea         ! Opening area [m2]


      ! FLOW:
      ! Get information on the horizontal opening
      CompNum = AirflowNetworkCompData(J)%TypeNum
      RhozAver = (RHOZ(N)+RHOZ(M))/2.0d0
      Width = MultizoneSurfaceData(I)%Width
      Height = MultizoneSurfaceData(I)%Height
      fact = MultizoneSurfaceData(I)%OpenFactor
      expn = MultizoneCompHorOpeningData(CompNum)%FlowExpo
      coef = MultizoneCompHorOpeningData(CompNum)%FlowCoef
      slope = MultizoneCompHorOpeningData(CompNum)%slope
      DischCoeff = MultizoneCompHorOpeningData(CompNum)%DischCoeff
      Cshape = 0.942d0*Width/Height
      OpenArea = Width*Height*fact*sin(slope*PI/180.0d0)*(1.0d0+cos(slope*PI/180.0d0))
      DH = 4.0d0*(Width*Height)/2.0/(Width+Height)*fact

      ! Check which zone is higher

      IF(fact .eq. 0.0d0) THEN
        CALL GenericCrack(coef,expn,LFLAG,PDROP,N,M,F,DF,NF)
        Return
      end if


      fma12     = 0.0d0
      fma21     = 0.0d0
      dp1fma12  = 0.0d0
      dp1fma21  = 0.0d0
      BuoFlow   = 0.0d0
      dPBuoFlow = 0.0d0

      If (AirflowNetworkLinkageData(I)%NodeHeights(1) > AirflowNetworkLinkageData(I)%NodeHeights(2)) Then
      ! Node N is upper zone
        If (RHOZ(N) > RHOZ(M)) Then
          BuoFlowMax = RhozAver*0.055d0*SQRT(9.81*abs(RHOZ(N)-RHOZ(M))*DH**5/RhozAver)
          PurgedP = Cshape*Cshape*9.81d0*ABS(RHOZ(N)-RHOZ(M))*DH**5/(2.0d0*(OpenArea)**2)
          If (abs(PDROP) .LE. PurgedP) Then
            BuoFlow = BuoFlowMax*(1.0d0-abs(PDROP)/PurgedP)
            dPBuoFlow = BuoFlowMax/PurgedP
          End If
        End If
      Else
      ! Node M is upper zone
        If (RHOZ(N) < RHOZ(M)) Then
          BuoFlowMax = RhozAver*0.055d0*SQRT(9.81d0*abs(RHOZ(N)-RHOZ(M))*DH**5/RhozAver)
          PurgedP = Cshape*Cshape*9.81d0*ABS(RHOZ(N)-RHOZ(M))*DH**5/(2.0d0*(OpenArea)**2)
          If (abs(PDROP) .LE. PurgedP) Then
            BuoFlow = BuoFlowMax*(1.0d0-abs(PDROP)/PurgedP)
            dPBuoFlow = BuoFlowMax/PurgedP
          End If
        End If
      End If

      IF(PDROP.EQ.0.0d0) THEN
         fma12 = BuoFlow
         fma21 = BuoFlow
         dp1fma12 = 0.0d0
         dp1fma21 = 0.0d0
      ELSE IF (PDROP.GT.0.0d0) THEN
         fma12 = RHOZ(N)*OpenArea*fact*DischCoeff*SQRT(2.0d0*PDROP/RhozAver)+BuoFlow
         dp1fma12 = RHOZ(N)*OpenArea*DischCoeff/SQRT(2.0d0*PDROP*RhozAver)+dPBuoFlow
         If (BuoFlow .gt. 0.0d0) Then
           fma21 = BuoFlow
           dp1fma21 = dPBuoFlow
         End If
      ELSE ! PDROP.LT.0.0
         fma21 = RHOZ(M)*OpenArea*fact*DischCoeff*SQRT(2.0d0*ABS(PDROP)/RhozAver)+BuoFlow
         dp1fma21 = -RHOZ(M)*OpenArea*DischCoeff/SQRT(2.0d0*ABS(PDROP)*RhozAver)+dPBuoFlow
         If (BuoFlow .gt. 0.0d0) Then
           fma12 = BuoFlow
           dp1fma12 = dPBuoFlow
         End If
      END IF

      F(1) = fma12-fma21
      DF(1) = dp1fma12-dp1fma21
      F(2) = 0.0d0
      if (fma12 .NE. 0.0d0 .and. fma21 .NE. 0.0d0) then
        F(2) = fma21
      End if
      DF(2) = 0.0d0

      RETURN
      END SUBROUTINE AFEHOP

      SUBROUTINE GenericCrack(Coef,Expn,LFLAG,PDROP,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  This subroutine is revised from AFEPLR developed by George Walton, NIST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves airflow for a power law component

          ! METHODOLOGY EMPLOYED:
          ! Using Q=C(dP)^n

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          REAL(r64), INTENT(INOUT)  :: Coef     ! Flow coefficient
          REAL(r64), INTENT(IN)  :: Expn     ! Flow exponent
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)     CDM, FL, FT
         REAL(r64)     RhozNorm, VisczNorm, Ctl, VisAve, Tave, RhoCor

      ! FLOW:
      ! Calculate normal density and viscocity at Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
      RhozNorm = PsyRhoAirFnPbTdbW(101325.0d0,20.0d0,0.0d0)
      VisczNorm = 1.71432d-5+4.828d-8*20.0d0
      VisAve = (VISCZ(N)+VISCZ(M))/2.0d0
      Tave = (TZ(N)+TZ(M))/2.0d0
      IF(PDROP.GE.0.0d0) THEN
         coef = Coef/SQRTDZ(N)
      ELSE
         coef = Coef/SQRTDZ(M)
      END IF

      NF = 1
      IF(LFLAG.EQ.1) THEN
        ! Initialization by linear relation.
        IF(PDROP.GE.0.0d0) THEN
          RhoCor = (TZ(N)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(N)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(N)/VISCZ(N)*ctl
        ELSE
          RhoCor = (TZ(M)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(M)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          DF(1) = Coef*RHOZ(M)/VISCZ(M)*ctl
        END IF
        F(1) = -DF(1)*PDROP
      ELSE
        ! Standard calculation.
        IF(PDROP.GE.0.0d0) THEN
          ! Flow in positive direction.
          ! Laminar flow.
          RhoCor = (TZ(N)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(N)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          CDM = Coef*RHOZ(N)/VISCZ(N)*ctl
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = Coef*SQRTDZ(N)*SQRT(PDROP)*ctl
          ELSE
            FT = Coef*SQRTDZ(N)*(PDROP**expn)*ctl
          END IF
        ELSE
          ! Flow in negative direction.
          ! Laminar flow.
          RhoCor = (TZ(M)+KelvinConv)/(Tave+KelvinConv)
          ctl = (RhozNorm/RHOZ(M)/RhoCor)**(expn-1.0d0)*(VisczNorm/VisAve)**(2.d0*expn-1.0d0)
          CDM = Coef*RHOZ(M)/VISCZ(M)*ctl
          FL = CDM*PDROP
          ! Turbulent flow.
          IF(expn .EQ. 0.5d0) THEN
            FT = -Coef*SQRTDZ(M)*SQRT(-PDROP)*ctl
          ELSE
            FT = -Coef*SQRTDZ(M)*(-PDROP)**expn*ctl
          END IF
        END IF
        ! Select laminar or turbulent flow.
        IF(LIST.GE.4) WRITE(Unit21,901) ' generic crack: ',PDROP,FL,FT
        IF(ABS(FL).LE.ABS(FT)) THEN
          F(1) = FL
          DF(1) = CDM
        ELSE
          F(1) = FT
          DF(1) = FT*expn/PDROP
        END IF
      END IF
!
  901 FORMAT(A5,6X,4E16.7)
      RETURN
      END SUBROUTINE GenericCrack


      SUBROUTINE FACSKY(AU,AD,AL,IK,NEQ,NSYM)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  This subroutine is revised from FACSKY developed by George Walton, NIST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs L-U factorization of a skyline ordered matrix, [A]
          ! The algorithm has been restructured for clarity.
          ! Note dependence on compiler for optimizing the inner do loops.

          ! METHODOLOGY EMPLOYED:
          !     L-U factorization of a skyline ordered matrix, [A], used for
          !     solution of simultaneous linear algebraic equations [A] * X = B.
          !     No pivoting!  No scaling!  No warnings!!!
          !     Related routines:  SLVSKY, SETSKY, FILSKY.

          ! REFERENCES:
          !     Algorithm is described in "The Finite Element Method Displayed",
          !     by G. Dhatt and G. Touzot, John Wiley & Sons, New York, 1984.

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: IK(NetworkNumOfNodes+1) ! pointer to the top of column/row "K"
          INTEGER, INTENT(IN)  :: NEQ   ! number of equations
          INTEGER, INTENT(IN)  :: NSYM  ! symmetry:  0 = symmetric matrix, 1 = non-symmetric
          ! noel, GNU says the AU is indexed above its upper bound
          !REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AD(NetworkNumOfNodes)  ! the main diagonal of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AL(IK(NetworkNumOfNodes+1)-1) ! the lower triangle of [A] before and after factoring


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         INTEGER  JHK, JHK1, LHK, LHK1, IMIN, IMIN1
         INTEGER  JHJ, JHJ1, IC, I, J, K
         REAL(r64)  T1, T2, SDOT, SUMD

      ! FLOW:
      AD(1) = 1.0d0/AD(1)
      JHK = 1
      DO K=2,NEQ
        SUMD = 0.0d0
        JHK1 = IK(K+1)
        LHK = JHK1-JHK
        IF(LHK.GT.0) THEN
          LHK1 = LHK-1
          IMIN = K-LHK1
          IMIN1 = IMIN-1
          IF(NSYM.EQ.1) AL(JHK) = AL(JHK)*AD(IMIN1)
          IF(LHK1.NE.0) THEN
            JHJ = IK(IMIN)
            IF(NSYM.EQ.0) THEN
              DO J=1,LHK1
                JHJ1 = IK(IMIN+J)
                IC = MIN(J,JHJ1-JHJ)
                IF(IC.GT.0) THEN
                  SDOT = 0.0d0
                  DO I=0,IC-1
                    SDOT = SDOT+AU(JHJ1-IC+I)*AU(JHK+J-IC+I)
                  END DO
                  AU(JHK+J) = AU(JHK+J)-SDOT
                END IF
                JHJ = JHJ1
              END DO
            ELSE
              DO J=1,LHK1
                JHJ1 = IK(IMIN+J)
                IC = MIN(J,JHJ1-JHJ)
                SDOT = 0.0d0
                IF(IC.GT.0) THEN
                  DO I=0,IC-1
                    SDOT = SDOT+AL(JHJ1-IC+I)*AU(JHK+J-IC+I)
                  END DO
                  AU(JHK+J) = AU(JHK+J)-SDOT
                  SDOT = 0.0d0
                  DO I=0,IC-1
                    SDOT = SDOT+AU(JHJ1-IC+I)*AL(JHK+J-IC+I)
                  END DO
                END IF
                AL(JHK+J) = (AL(JHK+J)-SDOT)*AD(IMIN1+J)
                JHJ = JHJ1
              END DO
            END IF
!
          END IF
          IF(NSYM.EQ.0) THEN
            DO I=0,LHK1
              T1 = AU(JHK+I)
              T2 = T1*AD(IMIN1+I)
              AU(JHK+I) = T2
              SUMD = SUMD+T1*T2
            END DO
          ELSE
            DO I=0,LHK1
              SUMD = SUMD+AU(JHK+I)*AL(JHK+I)
            END DO
          END IF
        END IF
        If (AD(K)-SUMD .EQ. 0.d0) Then
          CALL ShowSevereError('AirflowNetworkSolver: L-U factorization in Subroutine FACSKY.')
          CALL ShowContinueError('The denominator used in L-U factorizationis equal to 0.0 at node = ' &
                                //TRIM(AirflowNetworkNodeData(K)%Name)//'.')
          CALL ShowContinueError('One possible cause is that this node may not be connected directly, or indirectly via airflow ' &
                                //'network connections ')
          CALL ShowContinueError('(e.g., AirflowNetwork:Multizone:SurfaceCrack, AirflowNetwork:Multizone:Component:' &
                                 // 'SimpleOpening, etc.), to an external')
          CALL ShowContinueError('node (AirflowNetwork:MultiZone:Surface).')
          CALL ShowContinueError('Please send your input file and weather file to EnergyPlus support/development team' &
                                 //' for further investigation.')
          CALL ShowFatalError('Preceding condition causes termination.')
        End If
        AD(K) = 1.0d0/(AD(K)-SUMD)
        JHK = JHK1
      END DO
!
      RETURN
      END SUBROUTINE FACSKY


      SUBROUTINE SLVSKY(AU,AD,AL,B,IK,NEQ,NSYM)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  This subroutine is revised from CLVSKY developed by George Walton, NIST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves simultaneous linear algebraic equations [A] * X = B
          ! using L-U factored skyline form of [A] from "FACSKY"

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: IK(NetworkNumOfNodes+1) ! pointer to the top of column/row "K"
          INTEGER, INTENT(IN)  :: NEQ   ! number of equations
          INTEGER, INTENT(IN)  :: NSYM  ! symmetry:  0 = symmetric matrix, 1 = non-symmetric
          ! noel, GNU says the AU is indexed above its upper bound
          !REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(IN) :: AU(IK(NetworkNumOfNodes+1)) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(IN) :: AD(NetworkNumOfNodes)  ! the main diagonal of [A] before and after factoring
          REAL(r64), INTENT(IN) :: AL(IK(NetworkNumOfNodes+1)-1) ! the lower triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: B(NetworkNumOfNodes) ! "B" vector (input); "X" vector (output).


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

      INTEGER  I, JHK, JHK1, K, LHK
      REAL(r64)  SDOT, T1

      ! FLOW:
      JHK = 1
      DO K=2,NEQ
        JHK1 = IK(K+1)
        LHK = JHK1-JHK
        IF(LHK.LE.0) CYCLE
        SDOT = 0.0d0
        IF(NSYM.EQ.0) THEN
          DO I=0,LHK-1
            SDOT = SDOT+AU(JHK+I)*B(K-LHK+I)
          END DO
        ELSE
          DO I=0,LHK-1
            SDOT = SDOT+AL(JHK+I)*B(K-LHK+I)
          END DO
        ENDIF
        B(K) = B(K)-SDOT
        JHK = JHK1
      END DO
!
      IF(NSYM.EQ.0) THEN
        DO K=1,NEQ
          B(K) = B(K)*AD(K)
        END DO
      END IF
!
      K = NEQ+1
      JHK1 = IK(K)
      DO WHILE (K .NE. 1)
        K = K-1
        IF(NSYM.EQ.1) B(K) = B(K)*AD(K)
        IF(K.EQ.1) EXIT
!        IF(K.EQ.1) RETURN
        JHK = IK(K)
        T1 = B(K)
        DO I=0,JHK1-JHK-1
          B(K-JHK1+JHK+I) = B(K-JHK1+JHK+I)-AU(JHK+I)*T1
        END DO
        JHK1 = JHK
      END DO

      RETURN
      END SUBROUTINE SLVSKY


      SUBROUTINE FILSKY(X,LM,IK,AU,AD,FLAG)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  This subroutine is revised from FILSKY developed by George Walton, NIST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine adds element array "X" to the sparse skyline matrix [A]

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: LM(2) ! location matrix
          INTEGER, INTENT(IN)  :: IK(NetworkNumOfNodes+1) ! pointer to the top of column/row "K"
          INTEGER, INTENT(IN)  :: FLAG  ! mode of operation
          REAL(r64), INTENT(IN)     :: X(4)  ! element array (row-wise sequence)
          ! noel, GNU says the AU is indexed above its upper bound
          !REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)) ! the upper triangle of [A] before and after factoring
          REAL(r64), INTENT(INOUT) :: AD(NetworkNumOfNodes)  ! the main diagonal of [A] before and after factoring


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         INTEGER  J, K, L

      ! FLOW:
      ! K = row number, L = column number.
      IF(FLAG.GT.1) THEN
        K = LM(1)
        L = LM(2)
        IF(FLAG.EQ.4) THEN
          AD(K) = AD(K)+X(1)
          IF(K.LT.L) THEN
            J = IK(L+1)-L+K
            AU(J) = AU(J)+X(2)
          ELSE
            J = IK(K+1)-K+L
            AU(J) = AU(J)+X(3)
          END IF
          AD(L) = AD(L)+X(4)
        ELSE IF(FLAG.EQ.3) THEN
          AD(L) = AD(L)+X(4)
        ELSE IF(FLAG.EQ.2) THEN
          AD(K) = AD(K)+X(1)
        END IF
      END IF
!
      RETURN
      END SUBROUTINE FILSKY


      SUBROUTINE DUMPVD(S,V,N,UOUT)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  This subroutine is revised from DUMPVD developed by George Walton, NIST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine prints the contents of the REAL(r64) "V" vector

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          CHARACTER(*), INTENT(IN):: S  ! Description
          REAL(r64), INTENT(IN)     :: V(*) ! Output values
          INTEGER, INTENT(IN)  :: N  ! Array size
          INTEGER, INTENT(IN)  :: UOUT ! Output file unit


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          INTEGER I

      ! FLOW:
      ! Write values for debug
      WRITE(UOUT,901) S
      WRITE(UOUT,902) (V(I),I=1,N)
  901 FORMAT(1X,A)
  902 FORMAT(1X,5E15.07)
!
      RETURN
      END SUBROUTINE DUMPVD


      SUBROUTINE DUMPVR(S,V,N,UOUT)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   Extracted from AIRNET
          !       MODIFIED       Lixing Gu, 2/1/04
          !                      Revised the subroutine to meet E+ needs
          !       MODIFIED       Lixing Gu, 6/8/05
          !
          !       RE-ENGINEERED  This subroutine is revised from DUMPVR developed by George Walton, NIST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine prints the contents of the REAL(r64) "V" vector

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          CHARACTER(*), INTENT(IN):: S  ! Description
          REAL(r64), INTENT(IN)     :: V(*) ! Output values
          INTEGER, INTENT(IN)  :: N  ! Array size
          INTEGER, INTENT(IN)  :: UOUT ! Output file unit


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          INTEGER I

      ! FLOW:
      WRITE(UOUT,901) S
      WRITE(UOUT,902) (V(I),I=1,N)
  901 FORMAT(1X,A)
  902 FORMAT(1X,5E15.07)
!
      RETURN
      END SUBROUTINE DUMPVR


      SUBROUTINE AFEDOP(J,LFLAG,PDROP,IL,N,M,F,DF,NF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  This subroutine is revised based on a vertical large opening subroutine from COMIS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates airflow and pressure of a detailed large opening component.

          ! METHODOLOGY EMPLOYED:
          ! Purpose:  This routine calculates the massflow and its derivative
          !       through a large opening in both flow directions. As input
          !       the density profiles RhoProfF/T are required aswell as the
          !       effective pressure difference profile DpProfNew, which is the
          !       sum of the stack pressure difference profile DpProf and the
          !       difference of the actual pressures at reference height. The
          !       profiles are calculated in the routine PresProfile.
          !       The massflow and its derivative are calculated for each
          !       interval representing a step of the pressure difference
          !       profile. The total flow and derivative are obtained by
          !       summation over the whole opening.
          !       The calculation is split into different cases representing
          !       different situations of the opening:
          !       - closed opening (opening factor = 0): summation of top and
          !         bottom crack (crack length = lwmax) plus "integration" over
          !         a vertically distributed crack of length (2*lhmax+lextra).
          !       - type 1: normal rectangular opening: "integration" over NrInt
          !         openings with width actlw and height actlh/NrInt
          !       - type 2: horizontally pivoted window: flow direction assumed
          !         strictly perpendicular to the plane of the opening
          !         -> "integration" over normal rectangular openings at top
          !         and bottom of LO plus a rectangular opening in series with two
          !         triangular openings in the middle of the LO (most general
          !         situation). The geometry is defined by the input parameters
          !         actlw(=lwmax), actlh, axisheight, opening angle.
          !       Assuming the massflow perpendicular to the opening plane in all
          !       cases the ownheightfactor has no influence on the massflow.

          ! REFERENCES:
          ! Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
          ! Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: J     ! Component number
          INTEGER, INTENT(IN)  :: LFLAG ! Initialization flag.If = 1, use laminar relationship
          REAL(r64), INTENT(IN) :: PDROP ! Total pressure drop across a component (P1 - P2) [Pa]
          INTEGER, INTENT(IN)  :: IL    ! Linkage number
          INTEGER, INTENT(IN)  :: N     ! Node 1 number
          INTEGER, INTENT(IN)  :: M     ! Node 2 number
          INTEGER, INTENT(OUT) :: NF    ! Number of flows, either 1 or 2
          REAL(r64), INTENT(OUT) :: F(2)  ! Airflow through the component [kg/s]
          REAL(r64), INTENT(OUT) :: DF(2) ! Partial derivative:  DF/DP

          ! SUBROUTINE PARAMETER DEFINITIONS:
          REAL(r64), PARAMETER :: RealMax=0.1d+37
          REAL(r64), PARAMETER :: RealMin=1d-37


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER  CompNum
      REAL(r64) Width,Height

      REAL(r64)  fma12    ! massflow in direction "from-to" [kg/s]
      REAL(r64)  fma21    ! massflow in direction "to-from" [kg/s]
      REAL(r64)  dp1fma12 ! derivative d fma12 / d Dp [kg/s/Pa]
      REAL(r64)  dp1fma21 ! derivative d fma21 / d Dp [kg/s/Pa]
      REAL(r64)  DpProfNew(NrInt+2) ! Differential pressure profile for Large Openings, taking into account fixed
                                           ! pressures and actual zone pressures at reference height
      REAL(r64) fact ! Actual opening factor
      REAL(r64) DifLim ! Limit for the pressure difference where laminarization takes place [Pa]
      REAL(r64) Cfact,FvVeloc

      REAL(r64) ActLh,ActLw,Lextra,Axishght,ActCD,Cs,expn,Type
      REAL(r64) Interval,fmasum,dfmasum,Prefact,EvalHghts(NrInt+2)
      REAL(r64) h2,h4,alpha,rholink,c1,c2
      REAL(r64) DpZeroOffset,area,WFact,HFact
      INTEGER i,Loc,iNum

      ! FLOW:
      ! Get component properties
      DifLim = 1.0d-4
      CompNum = AirflowNetworkCompData(J)%TypeNum
      Width = MultizoneSurfaceData(IL)%Width
      Height = MultizoneSurfaceData(IL)%Height
      fact = MultizoneSurfaceData(IL)%OpenFactor
      Loc = (AirflowNetworkLinkageData(IL)%DetOpenNum-1)*(NrInt+2)
      iNum = MultizoneCompDetOpeningData(CompNum)%NumFac
      ActCD=0.0d0

      if (iNum .eq. 2) then
        if (fact .le. MultizoneCompDetOpeningData(CompNum)%OpenFac2) then
          WFact = MultizoneCompDetOpeningData(CompNum)%WidthFac1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%WidthFac2-MultizoneCompDetOpeningData(CompNum)%WidthFac1)
          HFact = MultizoneCompDetOpeningData(CompNum)%HeightFac1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%HeightFac2-MultizoneCompDetOpeningData(CompNum)%HeightFac1)
          CFact = MultizoneCompDetOpeningData(CompNum)%DischCoeff1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%DischCoeff2-MultizoneCompDetOpeningData(CompNum)%DischCoeff1)
        Else
          CALL ShowFatalError('Open Factor is above the maximum input range for opening factors in ' &
                              //'AirflowNetwork:MultiZone:Component:DetailedOpening = ' &
                                   //TRIM(MultizoneCompDetOpeningData(CompNum)%Name))
        End if
      end if

      if (iNum .eq. 3) then
        if (fact .le. MultizoneCompDetOpeningData(CompNum)%OpenFac2) then
          WFact = MultizoneCompDetOpeningData(CompNum)%WidthFac1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%WidthFac2-MultizoneCompDetOpeningData(CompNum)%WidthFac1)
          HFact = MultizoneCompDetOpeningData(CompNum)%HeightFac1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%HeightFac2-MultizoneCompDetOpeningData(CompNum)%HeightFac1)
          CFact = MultizoneCompDetOpeningData(CompNum)%DischCoeff1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%DischCoeff2-MultizoneCompDetOpeningData(CompNum)%DischCoeff1)
        else if (fact .le. MultizoneCompDetOpeningData(CompNum)%OpenFac3) then
          WFact = MultizoneCompDetOpeningData(CompNum)%WidthFac2+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac2)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac3-MultizoneCompDetOpeningData(CompNum)%OpenFac2)* &
           (MultizoneCompDetOpeningData(CompNum)%WidthFac3-MultizoneCompDetOpeningData(CompNum)%WidthFac2)
          HFact = MultizoneCompDetOpeningData(CompNum)%HeightFac2+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac2)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac3-MultizoneCompDetOpeningData(CompNum)%OpenFac2)* &
           (MultizoneCompDetOpeningData(CompNum)%HeightFac3-MultizoneCompDetOpeningData(CompNum)%HeightFac2)
          CFact = MultizoneCompDetOpeningData(CompNum)%DischCoeff2+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac2)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac3-MultizoneCompDetOpeningData(CompNum)%OpenFac2)* &
           (MultizoneCompDetOpeningData(CompNum)%DischCoeff3-MultizoneCompDetOpeningData(CompNum)%DischCoeff2)
        Else
          CALL ShowFatalError('Open Factor is above the maximum input range for opening factors in ' &
                              //'AirflowNetwork:MultiZone:Component:DetailedOpening = ' &
                                   //TRIM(MultizoneCompDetOpeningData(CompNum)%Name))
        end if
      end if

      if (iNum .eq. 4) then
        if (fact .le. MultizoneCompDetOpeningData(CompNum)%OpenFac2) then
          WFact = MultizoneCompDetOpeningData(CompNum)%WidthFac1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%WidthFac2-MultizoneCompDetOpeningData(CompNum)%WidthFac1)
          HFact = MultizoneCompDetOpeningData(CompNum)%HeightFac1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%HeightFac2-MultizoneCompDetOpeningData(CompNum)%HeightFac1)
          CFact = MultizoneCompDetOpeningData(CompNum)%DischCoeff1+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac1)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac2-MultizoneCompDetOpeningData(CompNum)%OpenFac1)* &
           (MultizoneCompDetOpeningData(CompNum)%DischCoeff2-MultizoneCompDetOpeningData(CompNum)%DischCoeff1)
        else if (fact .le. MultizoneCompDetOpeningData(CompNum)%OpenFac3) then
          WFact = MultizoneCompDetOpeningData(CompNum)%WidthFac2+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac2)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac3-MultizoneCompDetOpeningData(CompNum)%OpenFac2)* &
           (MultizoneCompDetOpeningData(CompNum)%WidthFac3-MultizoneCompDetOpeningData(CompNum)%WidthFac2)
          HFact = MultizoneCompDetOpeningData(CompNum)%HeightFac2+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac2)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac3-MultizoneCompDetOpeningData(CompNum)%OpenFac2)* &
           (MultizoneCompDetOpeningData(CompNum)%HeightFac3-MultizoneCompDetOpeningData(CompNum)%HeightFac2)
          CFact = MultizoneCompDetOpeningData(CompNum)%DischCoeff2+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac2)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac3-MultizoneCompDetOpeningData(CompNum)%OpenFac2)* &
           (MultizoneCompDetOpeningData(CompNum)%DischCoeff3-MultizoneCompDetOpeningData(CompNum)%DischCoeff2)
        else if (fact .le. MultizoneCompDetOpeningData(CompNum)%OpenFac4) then
          WFact = MultizoneCompDetOpeningData(CompNum)%WidthFac3+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac3)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac4-MultizoneCompDetOpeningData(CompNum)%OpenFac3)* &
           (MultizoneCompDetOpeningData(CompNum)%WidthFac4-MultizoneCompDetOpeningData(CompNum)%WidthFac3)
          HFact = MultizoneCompDetOpeningData(CompNum)%HeightFac3+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac3)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac4-MultizoneCompDetOpeningData(CompNum)%OpenFac3)* &
           (MultizoneCompDetOpeningData(CompNum)%HeightFac4-MultizoneCompDetOpeningData(CompNum)%HeightFac3)
          CFact = MultizoneCompDetOpeningData(CompNum)%DischCoeff3+ &
           (fact-MultizoneCompDetOpeningData(CompNum)%OpenFac3)/ &
           (MultizoneCompDetOpeningData(CompNum)%OpenFac4-MultizoneCompDetOpeningData(CompNum)%OpenFac3)* &
           (MultizoneCompDetOpeningData(CompNum)%DischCoeff4-MultizoneCompDetOpeningData(CompNum)%DischCoeff3)
        Else
          CALL ShowFatalError('Open Factor is above the maximum input range for opening factors in ' &
                              //'AirflowNetwork:MultiZone:Component:DetailedOpening = ' &
                                   //TRIM(MultizoneCompDetOpeningData(CompNum)%Name))
        end if
      end if

      ! calculate DpProfNew
      Do i=1,NrInt+2
        DpProfNew(i) = PDROP+DpProf(Loc+i)-DpL(1,iL)
      End Do

      ! Get opening data based on the opening factor
      if (fact .eq. 0) then
        ActLw=MultizoneSurfaceData(IL)%Width
        ActLh=MultizoneSurfaceData(IL)%Height
        Cfact = 0.0d0
      else
        ActLw=MultizoneSurfaceData(IL)%Width*Wfact
        ActLh=MultizoneSurfaceData(IL)%Height*HFact
        ActCD=CFact
      end if

      Cs=MultizoneCompDetOpeningData(CompNum)%FlowCoef
      Expn=MultizoneCompDetOpeningData(CompNum)%FlowExpo
      Type=MultizoneCompDetOpeningData(CompNum)%LVOType
      IF (Type.EQ.1) THEN
        Lextra=MultizoneCompDetOpeningData(CompNum)%LVOValue
        AxisHght=0.0d0
      ELSE IF (Type.EQ.2) THEN
        Lextra=0.0d0
        AxisHght=MultizoneCompDetOpeningData(CompNum)%LVOValue
        ActLw=MultizoneSurfaceData(IL)%Width
        ActLh=MultizoneSurfaceData(IL)%Height
      ENDIF

      ! Add window multiplier with window close
      If (MultizoneSurfaceData(IL)%Multiplier > 1.0d0) Cs=Cs*MultizoneSurfaceData(IL)%Multiplier
      ! Add window multiplier with window open
      If (fact .gt. 0.0d0) then
        If (MultizoneSurfaceData(IL)%Multiplier > 1.0d0) ActLw=ActLw*MultizoneSurfaceData(IL)%Multiplier
      End If

      ! Add recurring warnings
      If (fact .gt. 0.0d0) then
        If (ActLw .eq. 0.0d0) Then
          MultizoneCompDetOpeningData(CompNum)%WidthErrCount = MultizoneCompDetOpeningData(CompNum)%WidthErrCount + 1
          if (MultizoneCompDetOpeningData(CompNum)%WidthErrCount< 2) then
            CALL ShowWarningError('The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of '//&
              TRIM(MultizoneCompDetOpeningData(CompNum)%Name)//' is 0.')
            CALL ShowContinueError('The actual width is set to 1.0E-6 m.')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          else
            CALL ShowRecurringWarningErrorAtEnd('The actual width of the '// &
              'AirflowNetwork:MultiZone:Component:DetailedOpening of ' &
              //TRIM(MultizoneCompDetOpeningData(CompNum)%Name)//' is 0 error continues.', &
              MultizoneCompDetOpeningData(CompNum)%WidthErrIndex, ActLw,ActLw)
          end if
          ActLw = 1.0d-6
        End If
        If (ActLh .eq. 0.0d0) Then
          MultizoneCompDetOpeningData(CompNum)%HeightErrCount = MultizoneCompDetOpeningData(CompNum)%HeightErrCount + 1
          if (MultizoneCompDetOpeningData(CompNum)%HeightErrCount< 2) then
            CALL ShowWarningError('The actual height of the AirflowNetwork:MultiZone:Component:DetailedOpening of '//&
              TRIM(MultizoneCompDetOpeningData(CompNum)%Name)//' is 0.')
            CALL ShowContinueError('The actual height is set to 1.0E-6 m.')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          else
            CALL ShowRecurringWarningErrorAtEnd('The actual width of the '// &
              'AirflowNetwork:MultiZone:Component:DetailedOpening of ' &
              //TRIM(MultizoneCompDetOpeningData(CompNum)%Name)//' is 0 error continues.', &
              MultizoneCompDetOpeningData(CompNum)%HeightErrIndex, ActLh,ActLh)
          end if
          ActLh = 1.0d-6
        End If
      End If
      ! Initialization:
      NF = 1
      Interval=ActLh/NrInt
      fma12=0.0d0
      fma21=0.0d0
      dp1fma12=0.0d0
      dp1fma21=0.0d0

      ! Closed LO
      IF (Cfact.EQ.0) THEN
        DpZeroOffset=DifLim
        ! bottom crack
        IF (DpProfNew(1).GT.0) THEN
          IF (Abs(DpProfNew(1)).LE.DpZeroOffset) THEN
            dfmasum=Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
            fmasum=DpProfNew(1)*dfmasum
          ELSE
            fmasum=Cs*ActLw*(DpProfNew(1))**expn
            dfmasum=fmasum*expn/DpProfNew(1)
          ENDIF
          fma12=fma12+fmasum
          dp1fma12=dp1fma12+dfmasum
        ELSE
          IF (Abs(DpProfNew(1)).LE.DpZeroOffset) THEN
            dfmasum=-Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
            fmasum=DpProfNew(1)*dfmasum
          ELSE
            fmasum=Cs*ActLw*(-DpProfNew(1))**expn
            dfmasum=fmasum*expn/DpProfNew(1)
          ENDIF
          fma21=fma21+fmasum
          dp1fma21=dp1fma21+dfmasum
        ENDIF
        ! top crack
        IF (DpProfNew(NrInt+2).GT.0) THEN
          IF (Abs(DpProfNew(NrInt+2)).LE.DpZeroOffset) THEN
            dfmasum=Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
            fmasum=DpProfNew(NrInt+2)*dfmasum
          ELSE
            fmasum=Cs*ActLw*(DpProfNew(NrInt+2))**expn
            dfmasum=fmasum*expn/DpProfNew(NrInt+2)
          ENDIF
          fma12=fma12+fmasum
          dp1fma12=dp1fma12+dfmasum
        ELSE
          IF (Abs(DpProfNew(NrInt+2)).LE.DpZeroOffset) THEN
            dfmasum=-Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
            fmasum=DpProfNew(NrInt+2)*dfmasum
          ELSE
            fmasum=Cs*ActLw*(-DpProfNew(NrInt+2))**expn
            dfmasum=fmasum*expn/DpProfNew(NrInt+2)
          ENDIF
          fma21=fma21+fmasum
          dp1fma21=dp1fma21+dfmasum
        ENDIF
        ! side and extra cracks
        Prefact=Interval*(2+lextra/ActLh)*Cs
        DO i=2,NrInt+1
          IF (DpProfNew(i).GT.0) THEN
            IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
              dfmasum=Prefact*DpZeroOffset**expn/DpZeroOffset
              fmasum=DpProfNew(i)*dfmasum
            ELSE
              fmasum=Prefact*(DpProfNew(i))**expn
              dfmasum=fmasum*expn/DpProfNew(i)
            ENDIF
            fma12=fma12+fmasum
            dp1fma12=dp1fma12+dfmasum
          ELSE
            IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
              dfmasum=-Prefact*DpZeroOffset**expn/DpZeroOffset
              fmasum=DpProfNew(i)*dfmasum
            ELSE
              fmasum=Prefact*(-DpProfNew(i))**expn
              dfmasum=fmasum*expn/DpProfNew(i)
            ENDIF
            fma21=fma21+fmasum
            dp1fma21=dp1fma21+dfmasum
          ENDIF
        ENDDO
      ENDIF

      ! Open LO, type 1
      IF ((Cfact.NE.0).AND.(type.EQ.1))  THEN
        DpZeroOffset=DifLim*1d-3
        Prefact=ActLw*ActCd*Interval*SQRT(2.0d0)
        DO i=2,NrInt+1
          IF (DpProfNew(i).GT.0) THEN
            IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
              dfmasum=SQRT(RhoProfF(Loc+i)*DpZeroOffset)/DpZeroOffset
              fmasum=DpProfNew(i)*dfmasum
            ELSE
              fmasum=SQRT(RhoProfF(Loc+i)*DpProfNew(i))
              dfmasum=0.5d0*fmasum/DpProfNew(i)
            ENDIF
            fma12=fma12+fmasum
            dp1fma12=dp1fma12+dfmasum
          ELSE
            IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
              dfmasum=-SQRT(RhoProfT(Loc+i)*DpZeroOffset)/DpZeroOffset
              fmasum=DpProfNew(i)*dfmasum
            ELSE
              fmasum=SQRT(-RhoProfT(Loc+i)*DpProfNew(i))
              dfmasum=0.5d0*fmasum/DpProfNew(i)
            ENDIF
            fma21=fma21+fmasum
            dp1fma21=dp1fma21+dfmasum
          ENDIF
        ENDDO

        fma12=prefact*fma12
        fma21=prefact*fma21
        dp1fma12=prefact*dp1fma12
        dp1fma21=prefact*dp1fma21

      ENDIF

      ! Open LO, type 2
      IF ((Cfact.NE.0).AND.(type.EQ.2))  THEN
        ! Initialization
        DpZeroOffset=DifLim*1d-3
        ! New definition for opening factors for LVO type 2: opening angle = 90 degrees --> opening factor = 1.0
        ! should be PIOvr2 in below?
        alpha=fact*3.14159d0/2.d0
        h2=Axishght*(1.0d0-COS(alpha))
        h4=Axishght+(ActLh-Axishght)*COS(alpha)
        EvalHghts(1)=0.0d0
        EvalHghts(NrInt+2)=ActLh
        ! New definition for opening factors for LVO type 2: pening angle = 90 degrees --> opening factor = 1.0
        IF (fact.EQ.1.0d0) THEN
          h2=Axishght
          h4=Axishght
        ENDIF

        DO i=2,NrInt+1
          EvalHghts(i)=Interval*(i-1.5d0)
        ENDDO

        ! Calculation of massflow and its derivative
        DO i=2,NrInt+1
          IF (DpProfNew(i).GT.0) THEN
            rholink=RhoProfF(Loc+i)
          ELSE
            rholink=RhoProfT(Loc+i)
          ENDIF

          IF ((EvalHghts(i).LE.h2).OR.(EvalHghts(i).GE.h4)) THEN
            IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
              dfmasum=ActCd*ActLw*Interval*SQRT(2.0*rholink* &
              DpZeroOffset)/DpZeroOffset*DSIGN(1d0,DpProfNew(i))
              fmasum=DpProfNew(i)*dfmasum
            ELSE
              fmasum=ActCd*ActLw*Interval*SQRT(2.0*rholink*ABS(DpProfNew(i)))
              dfmasum=0.5d0*fmasum/DpProfNew(i)
            ENDIF
          ELSE
            ! triangular opening at the side of LO
            c1=ActCd*ActLw*Interval*SQRT(2.0*rholink)
            c2=2*ActCd*ABS((Axishght-Evalhghts(i)))*TAN(alpha)* &
              Interval*SQRT(2.0d0*rholink)
            IF ((c1.NE.0).AND.(c2.NE.0)) THEN
              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                dfmasum=SQRT(DpZeroOffset/(1/c1/c1+1/c2/c2))/ &
                        DpZeroOffset*DSIGN(1d0,DpProfNew(i))
                fmasum=DpProfNew(i)*dfmasum
              ELSE
                fmasum=SQRT(ABS(DpProfNew(i))/(1/c1/c1+1/c2/c2))
                dfmasum=0.5d0*fmasum/DpProfNew(i)
              ENDIF
            ELSE
              fmasum=0.0d0
              dfmasum=0.0d0
            ENDIF
          ENDIF

          IF (DpProfNew(i).GT.0) THEN
            fma12=fma12+fmasum
            dp1fma12=dp1fma12+dfmasum
          ELSE
            fma21=fma21+fmasum
            dp1fma21=dp1fma21+dfmasum
          ENDIF
        ENDDO

      ENDIF

      ! Calculate some velocity in the large opening
      area=ActLh*ActLw*actCD
      if (area.gt.(cs+RealMin)) then
        if (area.gt.RealMin) then
          FvVeloc=(fma21+fma12)/area
        else
          FvVeloc=0.0d0
        end if
      else
        ! here the average velocity over the full area, may blow half in half out.
        ! velocity= Fva/Nett area=Fma/Rho/(Cm/( (2**N)* sqrt(1.2) ) )
        if (cs.gt.0.0d0) then
          ! get the average Rho for this closed window
          DO i=2,NrInt+1
            rholink=0.0d0
            IF (DpProfNew(i).GT.0) THEN
              rholink=RhoProfF(Loc+i)
            ELSE
              rholink=RhoProfT(Loc+i)
            ENDIF
            rholink=rholink/nrInt
            rholink=1.2d0
          END DO
          FvVeloc=(Fma21+fma12)*(2.d0**ExpN)*SQRT(1.2d0)/(rhoLink*Cs)
        else
          FvVeloc=0.0d0
        end if
      end if

      ! Output mass flow rates and associated derivatives
      F(1) = fma12-fma21
      DF(1) = dp1fma12-dp1fma21
      F(2) = 0.0d0
      if (fma12 .NE. 0.0d0 .and. fma21 .NE. 0.0d0) then
        F(2) = fma21
      End if
      DF(2) = 0.0d0

      RETURN
    END SUBROUTINE AFEDOP

    SUBROUTINE PresProfile(il,Pprof,G,DpF,DpT,BetaF,BetaT,RhoStF, &
            RhoStT,From,To,ActLh,OwnHeightFactor)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  This subroutine is revised based on PresProfile subroutine from COMIS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates for a large opening profiles of stack pressure difference and
          ! densities in the zones linked by the a detailed opening cmponent.

          ! METHODOLOGY EMPLOYED:
          ! The profiles are obtained in the following
          ! way:    - the opening is divided into NrInt vertical intervals
          !         - the stack pressure difference and densities in From-
          !           and To-zone are calculated at the centre of each
          !           interval aswell as at the top and bottom of the LO
          !          - these values are stored in the (NrInt+2)-dimensional
          !             arrays DpProf, RhoProfF, RhoProfT.
          ! The calculation of stack pressure and density in the two zones
          ! is based on the arrays DpF/T, RhoStF/T, BetaF/T. These arrays
          ! are calculated in the COMIS routine Lclimb. They contain the
          ! values of stack pressure and density at the startheight of the
          ! opening and at startheights of all layers lying inside the
          ! opening, and the density gradients across the layers.
          ! The effective startheight zl(1/2) in the From/To zone and the
          ! effective length actLh of the LO take into account the
          ! startheightfactor, heightfactor and ownheightfactor. Thus for
          ! slanted windows the range of the profiles is the vertical
          ! projection of the actual opening.

          ! REFERENCES:
          ! Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
          ! Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN)  :: IL       ! Linkage number
          INTEGER, INTENT(IN)  :: Pprof    ! Opening number
          REAL(r64), INTENT(IN)     :: G        ! gravitation field strength [N/kg]
          REAL(r64), INTENT(IN) :: DpF(2) ! Stack pressures at start heights of Layers
                                                 ! in the FROM zone (starting at linkheight) [Pa]
          REAL(r64), INTENT(IN) :: DpT(2) ! Stack pressures at start heights of Layers
          REAL(r64), INTENT(IN)     :: BetaF(2) ! Density gradients in the FROM zone (starting at linkheight) [Kg/m3/m]
          REAL(r64), INTENT(IN)     :: BetaT(2) ! Density gradients in the TO zone (starting at linkheight) [Kg/m3/m]
          REAL(r64), INTENT(IN)     :: RhoStF(2) ! Density at the start heights of Layers in the FROM zone
                                            ! (starting at linkheight) [Kg/m3]
          REAL(r64), INTENT(IN)     :: RhoStT(2) ! Density at the start heights of Layers in the TO zone
                                            ! (starting at linkheight) [Kg/m3]
          REAL(r64), INTENT(IN)     :: ActLh    ! Actual height of opening [m]
          REAL(r64), INTENT(IN)     :: OwnHeightFactor ! Cosine of deviation angle of the opening plane from the vertical direction
          INTEGER, INTENT(IN)  :: From     ! Number of FROM zone
          INTEGER, INTENT(IN)  :: To       ! Number of To zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) zF(2),zT(2) ! Startheights of layers in FROM-, TO-zone
      REAL(r64) zStF(2),zStT(2) ! Startheights of layers within the LO, starting with the actual startheight of the LO.
                           ! The values in the arrays DpF, DpT, BetaF, BetaT, RhoStF, RhoStT are calculated at these heights.
      REAL(r64) hghtsFR, hghtsTR
      REAL(r64) hghtsF(NrInt+2),hghtsT(NrInt+2) ! Heights of evaluation points for pressure and density profiles
      REAL(r64) Interval ! Distance between two evaluation points
      REAL(r64) delzF,delzT ! Interval between actual evaluation point and startheight of actual layer in FROM-, TO-zone
      INTEGER AnzLayF,AnzLayT ! Number of layers in FROM-, TO-zone
      INTEGER lF,lT ! Actual index for DpF/T, BetaF/T, RhoStF/T, zStF/T
      INTEGER n,i,k


      ! FLOW:
      ! Initialization
      delzF=0.0d0
      delzT=0.0d0
      Interval=ActLh*OwnHeightFactor/NrInt

      DO n=1,NrInt
        hghtsF(n+1)=AirflowNetworkLinkageData(il)%NodeHeights(1)+Interval*(n-0.5d0)
        hghtsT(n+1)=AirflowNetworkLinkageData(il)%NodeHeights(2)+Interval*(n-0.5d0)
      END DO
      hghtsF(1)=AirflowNetworkLinkageData(il)%NodeHeights(1)
      hghtsT(1)=AirflowNetworkLinkageData(il)%NodeHeights(2)
      hghtsF(NrInt+2)=AirflowNetworkLinkageData(il)%NodeHeights(1)+ActLh*OwnHeightFactor
      hghtsT(NrInt+2)=AirflowNetworkLinkageData(il)%NodeHeights(2)+ActLh*OwnHeightFactor

      lF=1
      lT=1
      If (From .eq. 0) then
        AnzLayF=1
      Else
        AnzLayF=0
      End if
      If (To .eq. 0) then
        AnzLayT=1
      Else
        AnzLayT=0
      End if

      IF (AnzLayF.GT.0) THEN
        DO n=1,AnzLayF
          zF(n)=0.0d0
          If (hghtsF(1) .LT. 0.0d0) zF(n)=hghtsF(1)
        ENDDO
      ENDIF

      IF (AnzLayT.GT.0) THEN
        DO n=1,AnzLayT
          zT(n)=0.0d0
          If (hghtsT(1) .LT. 0.0d0) zT(n)=hghtsT(1)
        ENDDO
      ENDIF

      zStF(1)=AirflowNetworkLinkageData(il)%NodeHeights(1)
      i=2
      k=1

      DO WHILE (k.LE.AnzLayF)
        IF (zF(k).GT.zStF(1)) Exit
        k=k+1
      END DO

      DO WHILE (k.LE.AnzLayF)
        IF (zF(k).GT.hghtsF(NrInt)) EXIT
        zStF(i)=zF(k) !Objexx:BoundsViolation zStF(i) @ i>2 and zF(k) @ k>2
        i=i+1
        k=k+1
      END DO

      zStF(i)=AirflowNetworkLinkageData(il)%NodeHeights(1)+ActLh*OwnHeightFactor !Objexx:BoundsViolation zStF(i) @ i>2
      zStT(1)=AirflowNetworkLinkageData(il)%NodeHeights(2)
      i=2
      k=1

      DO WHILE (k.LE.AnzLayT)
        IF (zT(k).GT.zStT(1)) EXIT
        k=k+1
      END DO

      DO WHILE (k.LE.AnzLayT)
        IF (zT(k).GT.hghtsT(NrInt)) EXIT !Objexx:BoundsViolation zT(k) @ k>2
        zStT(i)=zT(k) !Objexx:BoundsViolation zStF(i) @ i>2 and zT(k) @ k>2
        i=i+1
        k=k+1
      END DO

      zStT(i)=AirflowNetworkLinkageData(il)%NodeHeights(2)+ActLh*OwnHeightFactor !Objexx:BoundsViolation zStT(i) @ i>2

      ! Calculation of DpProf, RhoProfF, RhoProfT
      DO i=1,NrInt+2
        hghtsFR=hghtsF(i)
        hghtsTR=hghtsT(i)

        DO
          IF (hghtsFR.GT.zStF(lF+1)) THEN
            IF (lF .gt. 2) EXIT
            lF=lF+1
          END IF
          IF (hghtsFR.LE.zStF(lF+1)) Exit
        END DO

        DO
          IF (hghtsTR.GT.zStT(lT+1)) THEN
            lT=lT+1
          END IF
          IF (hghtsTR.LE.zStT(lT+1)) Exit
        END DO

        delzF=hghtsF(i)-zStF(lF)
        delzT=hghtsT(i)-zStT(lT)

        RhoProfF(i+Pprof)=RhoStF(lF)+BetaF(lF)*delzF
        RhoProfT(i+Pprof)=RhoStT(lT)+BetaT(lT)*delzT

        DpProf(i+Pprof)=DpF(lF)-DpT(lT) &
            -G*(RhoStF(lF)*delzF+BetaF(lF)*delzF**2/2.0d0) &
            +G*(RhoStT(lT)*delzT+BetaT(lT)*delzT**2/2.0d0)
      END DO

      RETURN
    END SUBROUTINE PresProfile

    SUBROUTINE PStack

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  This subroutine is revised based on PresProfile subroutine from COMIS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the stack pressures for a link between two zones

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
          ! Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          REAL(r64), PARAMETER :: PI = 3.14159265358979323846d0
          REAL(r64), PARAMETER :: PSea=101325.0d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!      REAL(r64) RhoOut ! air density outside [kg/m3]
      REAL(r64) G ! gravity field strength [N/kg]
      REAL(r64) RhoL1,RhoL2 ! Air density [kg/m3]
      REAL(r64) Pbz ! Pbarom at entrance level [Pa]
      REAL(r64) RhoDrL(2,NumOfLinksMultiZone) ! dry air density on both sides of the link [kg/m3]
      REAL(r64) TempL1,TempL2 ! Temp in From and To zone at link level [C]
!      REAL(r64) Tout ! outside temperature [C]
      REAL(r64) Xhl1,Xhl2 ! Humidity in From and To zone at link level [kg/kg]
!      REAL(r64) Xhout ! outside humidity [kg/kg]
      REAL(r64) Hfl(NumOfLinksMultiZone) ! Own height factor for large (slanted) openings
      INTEGER Nl ! number of links

      REAL(r64) DpF(2),DpP,DpT(2)
      REAL(r64) H
      REAL(r64) RhoStF(2),RhoStT(2),RhoDrDummi
      REAL(r64) BetaStF(2),BetaStT(2)
      REAL(r64) T,X,HSt(2)
      REAL(r64) TzFrom,XhzFrom
      REAL(r64) TzTo, XhzTo
      REAL(r64) ActLh,ActLOwnh
      REAL(r64) Pref
      REAL(r64) PzFrom,PzTo
      REAL(r64) RhoLd(2),RhoStd
      INTEGER From,To,Fromz,Toz,Ltyp
      INTEGER I,ll,j,k,Pprof
      INTEGER ilayptr,OpenNum

      REAL(r64) RhoREF, CONV

      ! FLOW:
      RhoREF = PsyRhoAirFnPbTdbW(Psea,OutDryBulbTemp,OutHumRat)

      CONV=Latitude*2.d0*PI/360.d0
      G=9.780373d0*(1.d0+.0052891d0*(SIN(CONV))**2-.0000059d0*(SIN(2.d0*CONV))**2)

      Hfl = 1.0d0
      Pbz = OutBaroPress
      Nl = NumOfLinksMultiZone
      OpenNum=0
      RhoLd(1)=1.2d0
      RhoLd(2)=1.2d0
      RhoStd=1.2d0

      DO I=1,Nl
        ! Check surface tilt
        If (AirflowNetworkLinkageData(i)%DetOpenNum > 0 .and. &
            Surface(MultizoneSurfaceData(I)%SurfNum)%Tilt .LT. 90) then
          Hfl(i) = Surface(MultizoneSurfaceData(I)%SurfNum)%SinTilt
        End If
        ! Initialisation
        From=AirflowNetworkLinkageData(i)%NodeNums(1)
        To=AirflowNetworkLinkageData(i)%NodeNums(2)
        if (AirflowNetworkNodeData(From)%EPlusZoneNum > 0 .and.AirflowNetworkNodeData(To)%EPlusZoneNum > 0) then
          LL=0
        else if(AirflowNetworkNodeData(From)%EPlusZoneNum == 0 .and.AirflowNetworkNodeData(To)%EPlusZoneNum > 0) then
          LL=1
        else
          LL=3
        End if

        Ltyp=AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum
        IF (Ltyp.EQ.CompTypeNum_DOP) THEN
          ActLh=MultizoneSurfaceData(i)%Height
          ActLOwnh=ActLh*1.0d0
        ELSE
          ActLh=0.0d0
          ActLOwnh=0.0d0
        ENDIF

        TempL1=Tz(From)
        XhL1=Wz(From)
        TzFrom=Tz(From)
        XhzFrom=Wz(From)
        RhoL1=RhoZ(From)
        IF (LL.eq.0 .OR. LL.EQ.3) THEN
          PzFrom=Pz(From)
        ELSE
          PzFrom=0.0d0
          From=0
        ENDIF

        ilayptr = 0
        if (from .eq. 0) ilayptr = 1
        IF (ilayptr.EQ.0) THEN
          Fromz=0
        ELSE
          Fromz=From
        ENDIF

        TempL2=Tz(To)
        XhL2=Wz(To)
        TzTo=Tz(To)
        XhzTo=Wz(To)
        RhoL2=RhoZ(To)

        IF (LL.LT.3) THEN
          PzTo=Pz(To)
        ELSE
          PzTo=0.0d0
          To=0
        ENDIF
        ilayptr = 0
        if (To .eq. 0) ilayptr = 1
        IF (ilayptr.EQ.0) THEN
          Toz=0
        ELSE
          Toz=To
        ENDIF

        ! RhoDrL is Rho at link level without pollutant but with humidity
        RhoDrL(1,i) = PsyRhoAirFnPbTdbW(OutBaroPress+PzFrom,TempL1,XhL1)
        RhoDrL(2,i) = PsyRhoAirFnPbTdbW(OutBaroPress+PzTo,TempL2,XhL2)

        ! End initialisation

        ! calculate DpF the difference between Pz and P at Node 1 height
        ilayptr = 0
        If (fromz .eq. 0) ilayptr = 1
        J=ilayptr
        k=1
        CALL LClimb(G,Rhold(1),AirflowNetworkLinkageData(i)%NodeHeights(1),TempL1,XhL1, &
                   DpF(k),Toz,PzTo,Pbz,RhoDrL(1,I))
        RhoL1=Rhold(1)
        ! For large openings calculate the stack pressure difference profile and the
        ! density profile within the the top- and the bottom- height of the large opening
        IF (ActLOwnh.GT.0.0d0) THEN
          HSt(k)=AirflowNetworkLinkageData(i)%NodeHeights(1)
          RhoStF(k)=RhoL1
          k=k+1
          Hst(k)=0.0d0
          If (Hst(k-1) .LT. 0.0d0) Hst(k) = Hst(k-1)

          ! Search for the first startheight of a layer which is within the top- and the
          ! bottom- height of the large opening.
          Do
            ilayptr = 0
            If (Fromz .eq. 0) ilayptr = 9
            IF ((J.GT.ilayptr).OR.(Hst(k).GT.AirflowNetworkLinkageData(i)%NodeHeights(1))) Exit
            J=J+9
            Hst(k)=0.0d0
            If (Hst(k-1) .LT. 0.0d0) Hst(k) = Hst(k-1)
          End Do

          ! Calculate Rho and stack pressure for every StartHeight of a layer which is
          ! within the top- and the bottom-height of the  large opening.
          Do
            ilayptr = 0
            If (Fromz .eq. 0) ilayptr = 9
            IF ((J.GT.ilayptr).OR. &
               (Hst(k).GE.(AirflowNetworkLinkageData(i)%NodeHeights(1)+ActLOwnh))) Exit !Objexx:BoundsViolation HSt(k) @ k>2
            T=TzFrom
            X=XhzFrom
            CALL LClimb(G,RhoStd,HSt(k),T,X,DpF(k),Fromz,PzFrom,Pbz,RhoDrDummi) !Objexx:BoundsViolation HSt(k) and DpF(k) @ k>2
            RhoStF(k)=Rhostd !Objexx:BoundsViolation RhoStF(k) @ k>2
            J=J+9
            k=k+1 !Objexx k>2 now
            Hst(k)=0.0d0 !Objexx:BoundsViolation @ k>2
            If (Hst(k-1) .LT. 0.0d0) Hst(k) = Hst(k-1) !Objexx:BoundsViolation @ k>2
          End Do
          ! Stack pressure difference and rho for top-height of the large opening
          HSt(k)=AirflowNetworkLinkageData(i)%NodeHeights(1)+ActLOwnh !Objexx:BoundsViolation k>2 poss
          T=TzFrom
          X=XhzFrom
          CALL LClimb(G,RhoStd,HSt(k),T,X,DpF(k),Fromz,PzFrom,Pbz,RhoDrDummi) !Objexx:BoundsViolation k>2 poss
          RhoStF(k)=RhoStd !Objexx:BoundsViolation k >= 3 poss

          DO J=1,(k-1)
            BetaStF(J)=(RhoStF(J+1)-RhoStF(J))/(HSt(J+1)-HSt(J))
          END DO
        ENDIF

        ! repeat procedure for the "To" node, DpT
        ilayptr = 0
        if (Toz .eq. 0) ilayptr = 1
        J = ilayptr
        ! Calculate Rho at link height only if we have large openings or layered zones.
        k=1
        CALL LClimb(G,RhoLd(2),AirflowNetworkLinkageData(i)%NodeHeights(2),TempL2,XhL2, &
                   DpT(k),Toz,PzTo,Pbz,RhoDrL(2,I))
        RhoL2=RhoLd(2)

        ! For large openings calculate the stack pressure difference profile and the
        ! density profile within the the top- and the bottom- height of the large opening
        IF (ActLOwnh.GT.0.0d0) THEN
          HSt(k)=AirflowNetworkLinkageData(i)%NodeHeights(2)
          RhoStT(k)=RhoL2
          k=k+1
          Hst(k)=0.0d0
          If (Hst(k-1) .LT. 0.0d0) Hst(k) = Hst(k-1)
          Do
            ilayptr = 0
            if (Toz .eq. 0) ilayptr = 9
            IF ((J.GT.ilayptr).OR.(Hst(k).GT.AirflowNetworkLinkageData(i)%NodeHeights(2))) Exit
            J=J+9
            Hst(k)=0.0d0
            If (Hst(k-1) .LT. 0.0d0) Hst(k) = Hst(k-1)
          End Do
          ! Calculate Rho and stack pressure for every StartHeight of a layer which is
          ! within the top- and the bottom-height of the  large opening.
          Do
            ilayptr = 0
            if (Toz .eq. 0) ilayptr = 9
            IF ((J.GT.ilayptr).OR. (Hst(k).GE.(AirflowNetworkLinkageData(i)%NodeHeights(2)+ActLOwnh))) Exit !Objexx:BoundsViolation Hst(k) @ k>2
            T=TzTo
            X=XhzTo
            CALL LClimb(G,RhoStd,HSt(k),T,X,DpT(k),Toz,PzTo,Pbz,RhoDrDummi) !Objexx:BoundsViolation HSt(k) and DpT(k) @ k>2
            RhoStT(k)=RhoStd !Objexx:BoundsViolation RhoStT(k) @ k>2
            J=J+9
            k=k+1 !Objexx k>2 now
            Hst(k)=0.0d0 !Objexx:BoundsViolation @ k>2
            If (Hst(k-1) .LT. 0.0d0) Hst(k) = Hst(k-1) !Objexx:BoundsViolation @ k>2
          End Do
          ! Stack pressure difference and rho for top-height of the large opening
          HSt(k)=AirflowNetworkLinkageData(i)%NodeHeights(2)+ActLOwnh !Objexx:BoundsViolation k>2 poss
          T=TzTo
          X=XhzTo
          CALL LClimb(G,RhoStd,HSt(k),T,X,DpT(k),Toz,PzTo,Pbz,RhoDrDummi) !Objexx:BoundsViolation k>2 poss
          RhoStT(k)=RhoStd !Objexx:BoundsViolation k>2 poss

          DO J=1,(k-1)
            BetaStT(J)=(RhoStT(J+1)-RhoStT(J))/(HSt(J+1)-HSt(J))
          END DO
        ENDIF

        ! CALCULATE STACK PRESSURE FOR THE PATH ITSELF for different flow directions
        H=REAL(AirflowNetworkLinkageData(i)%NodeHeights(2),r64) -   &
                REAL(AirflowNetworkLinkageData(i)%NodeHeights(1),r64)
        IF (LL.eq.0 .OR. LL.EQ.3 .OR. LL.EQ.6) THEN
          H=H-AirflowNetworkNodeData(From)%NodeHeight
        ENDIF
        IF (LL.LT.3) THEN
          H=H+AirflowNetworkNodeData(To)%NodeHeight
        ENDIF

        ! IF AIR FLOWS from "From" to "To"
        Pref=Pbz+Pzfrom+DpF(1)
        DpP=psz(Pref,RhoLd(1),0.0d0,0.0d0,H,G)
        DpL(1,I)=(DpF(1)-DpT(1)+DpP)

        ! IF AIR FLOWS from "To" to "From"
        Pref=Pbz+Pzto+DpT(1)
        DpP=-psz(Pref,RhoLd(2),0.0d0,0.0d0,-H,G)
        DpL(2,I)=(DpF(1)-DpT(1)+DpP)

        IF (Ltyp.EQ.CompTypeNum_DOP) THEN
          Pprof=OpenNum*(NrInt+2)
          CALL PresProfile(i,Pprof,G,DpF,DpT,BetaStF,BetaStT,RhoStF,RhoStT,from,To,ActLh,Hfl(I))
          OpenNum = OpenNum+1
        ENDIF

      END DO

      RETURN

    END SUBROUTINE PStack

     REAL(r64) FUNCTION psz(Pz0,Rho0,beta,z0,z,g)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  This subroutine is revised based on psz function from COMIS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function determines the pressure due to buoyancy in a stratified density environment

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          REAL(r64), INTENT(IN)     :: Pz0   ! Pressure at altitude z0 [Pa]
          REAL(r64), INTENT(IN)     :: Rho0  ! density at altitude z0 [kg/m3]
          REAL(r64), INTENT(IN)     :: beta  ! density gradient [kg/m4]
          REAL(r64), INTENT(IN)     :: z0    ! reference altitude [m]
          REAL(r64), INTENT(IN)     :: z     ! altitude[m]
          REAL(r64), INTENT(IN)     :: g     ! gravity field strength [N/kg]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) dz,rho

      ! FLOW:
      dz=z-z0
      rho=(Rho0+beta*dz/2.0d0)
      Psz=-Pz0*(1.d0-EXP(-dz*rho*g/Pz0)) ! Differential pressure from z to z0 [Pa]

      RETURN
      END FUNCTION psz

      SUBROUTINE LClimb(G,Rho,Z,T,X,Dp,zone,Pz,Pbz,RhoDr)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  This subroutine is revised based on subroutine IClimb from COMIS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function the differential pressure from the reflevel in a zone To Z, the level of a link

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          REAL(r64), INTENT(IN)                 :: g     ! gravity field strength [N/kg]
          REAL(r64), INTENT(INOUT)  :: Rho   ! Density link level (initialized with rho zone) [kg/m3]
          REAL(r64), INTENT(IN)                 :: Z     ! Height of the link above the zone reference [m]
          REAL(r64), INTENT(INOUT)              :: T     ! temperature at link level [C]
          REAL(r64), INTENT(INOUT)              :: X     ! absolute humidity at link level [kg/kg]
          REAL(r64), INTENT(OUT)    :: Dp    ! Stackpressure to the linklevel [Pa]
          INTEGER, INTENT(IN)              :: zone  ! Zone number
          REAL(r64), INTENT(IN)     :: Pz    ! Zone Pressure (reflevel) [Pa]
          REAL(r64), INTENT(IN)                 :: Pbz   ! Barometric pressure at entrance level [Pa]
          REAL(r64), INTENT(OUT)                :: RhoDr ! Air density of dry air on the link level used
                                                    ! for the concentration routine [kg/m3]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) H         ! Start Height of the layer
      REAL(r64) BetaT     ! Temperature gradient of this layer
      REAL(r64) BetaXfct  ! Humidity gradient factor of this layer
      REAL(r64) BetaCfct  ! Concentration 1 gradient factor of this layer
      REAL(r64) X0
      REAL(r64) P,Htop,Hbot
      REAL(r64) Rho0,Rho1,Betarho
      INTEGER :: L=0
      INTEGER :: ilayptr=0

      ! FLOW:
      Dp=0.0d0
      Rho0=Rho
      x0=x
      IF (Z.GT.0.0d0) THEN
        ! initialize start values
        H=0.0d0
        BetaT=0.0d0
        BetaXfct=0.0d0
        BetaCfct=0.0d0
        BetaRho=0.0d0
        Hbot=0.0d0

        DO WHILE (H.LT.0.0d0)
          ! loop until H>0 ; The start of the layer is above 0
          BetaT=0.0d0
          BetaXfct=0.0d0
          BetaCfct=0.0d0
          L=L+9
          ilayptr = 0
          if (zone .eq. 0) ilayptr = 9
          IF (L.GE.ilayptr) THEN
            H=Z+1.0d0
          ELSE
            H=0.0d0
          ENDIF
        END DO

        ! The link is in this layer also if it is on the top of it.

        DO
          IF (H.GE.Z) THEN
            ! the link ends in this layer , we reached the final Dp and BetaRho
            Htop=Z
            P=Pz+Dp
            IF (Htop.NE.Hbot)THEN
              Rho0 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              T=T+(Htop-Hbot)*BetaT
              X=X+(Htop-Hbot)*BetaXfct*X0
              Rho1 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              BetaRho=(Rho1-Rho0)/(Htop-Hbot)
              Dp=Dp+psz(Pbz+p,Rho0,BetaRho,Hbot,Htop,G)
            ENDIF
            Rhodr = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)
            Rho = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)
            RETURN

          ELSE
            ! bottom of the layer is below Z  (Z above ref)
            Htop=H
            ! P is the pressure up to the start height of the layer we just reached
            P=Pz+dp
            IF (Htop.NE.Hbot)THEN
              Rho0 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              T=T+(Htop-Hbot)*BetaT
              X=X+(Htop-Hbot)*BetaXfct*X0
              Rho1 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              BetaRho=(Rho1-Rho0)/(Htop-Hbot)
              Dp=Dp+psz(Pbz+p,Rho0,BetaRho,Hbot,Htop,G)
            ENDIF

            Rhodr = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)
            Rho = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)

            ! place current values Hbot and Beta's
            Hbot=H
            BetaT=0.0d0
            BetaXfct=0.0d0
            BetaCfct=0.0d0
            L=L+9
            ilayptr = 0
            if (zone .eq. 0) ilayptr = 9
            IF (L.GE.ilayptr) THEN
              H=Z+1.0d0
            ELSE
              H=0.0d0
            ENDIF
          ENDIF
        END DO

      ELSE
        ! This is the ELSE for negative linkheights Z below the refplane
        H=0.0d0
        BetaT=0.0d0
        BetaXfct=0.0d0
        BetaCfct=0.0d0
        BetaRho=0.0d0
        Htop=0.0d0
        DO WHILE (H.GT.0.0d0)
          ! loop until H<0 ; The start of the layer is below the zone refplane
          L=L-9
          ilayptr = 0
          if (zone .eq. 0) ilayptr = 1
          IF (L.LT.ilayptr) THEN
            ! with H=Z (negative) this loop will exit, no data for interval Z-refplane
            H=Z
            BetaT=0.0d0
            BetaXfct=0.0d0
            BetaCfct=0.0d0
            BetaRho=0.0d0
          ELSE
            H=0.0d0
            BetaT=0.0d0
            BetaXfct=0.0d0
            BetaCfct=0.0d0
          ENDIF
        END DO

        ! The link is in this layer also if it is on the bottom of it.
        DO
          IF (H.LE.Z) THEN
            Hbot=Z
            P=Pz+dp
            IF (Htop.NE.Hbot)THEN
              Rho1 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              T=T+(Hbot-Htop)*BetaT
              X=X+(Hbot-Htop)*BetaXfct*X0
              Rho0 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              BetaRho=(Rho1-Rho0)/(Htop-Hbot)
              Dp=Dp-psz(Pbz+p,Rho0,BetaRho,Hbot,Htop,G)
            ENDIF
            Rhodr = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)
            Rho =  PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)
            RETURN
          ELSE
            ! bottom of the layer is below Z  (Z below ref)
            Hbot=H
            P=Pz+dp
            IF (Htop.NE.Hbot)THEN
              Rho1 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              ! T,X,C calculated for the lower height
              T=T+(Hbot-Htop)*BetaT
              X=X+(Hbot-Htop)*BetaXfct*X0
              Rho0 = PsyRhoAirFnPbTdbW(Pbz+p,t,x)
              BetaRho=(Rho1-Rho0)/(htop-hbot)
              Dp=Dp-psz(Pbz+p,Rho0,BetaRho,Hbot,Htop,G)
            ENDIF
            Rhodr = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)
            Rho = PsyRhoAirFnPbTdbW(Pbz+pz+dp,t,x)

            ! place current values Hbot and Beta's
            Htop=H
            L=L-9
            ilayptr = 0
            if (zone .eq. 0) ilayptr = 1
            IF (L.LT.ilayptr) THEN
              H=Z-1.0d0
              BetaT=0.0d0
              BetaXfct=0.0d0
              BetaCfct=0.0d0
            ELSE
              H=0.0d0
              BetaT=0.0d0
              BetaXfct=0.0d0
              BetaCfct=0.0d0
            ENDIF
          ENDIF
        ! ENDIF H<Z
        END DO
      ENDIF

      RETURN

      END SUBROUTINE LClimb

!*****************************************************************************************
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

END MODULE AirflowNetworkSolver
