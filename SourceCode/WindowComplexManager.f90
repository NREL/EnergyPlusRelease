MODULE WindowComplexManager

          ! Module containing the routines dealing with complex fenestration

          ! MODULE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   ???
          !       MODIFIED       November 2011, Simon Vidanovic
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !  Initialize data for solar and thermal calculations and also performs thermal calculations for BSDF window

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataVectorTypes
USE DataInterfaces
USE DataBSDFWindow
USE DataGlobals, ONLY: MaxNameLength, Pi, DegToRadians, NumOfTimeStepInHour, NumOfZones, rTinyValue, &
                        KelvinConv, SecInHour, TimeStepZone
USE DataSurfaces  !, ONLY: TotSurfaces,TotWindows,Surface,SurfaceWindow   !update this later
USE DataEnvironment, ONLY: SunIsUpValue, SkyTempKelvin, IsRain, SunIsUp, CloudFraction, OutBaroPress, OutHumRat
USE DataHeatBalance
USE DataShadowingCombinations
USE vectors
USE DataHeatBalFanSys

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:

REAL(r64), PRIVATE, PARAMETER :: sigma=5.6697d-8    ! Stefan-Boltzmann constant
REAL(r64), PRIVATE, PARAMETER :: PressureDefault = 101325.0d0

INTEGER, PARAMETER :: Calculate_Geometry    =1
INTEGER, PARAMETER :: Copy_Geometry      =2

INTEGER, PARAMETER  ::  TmpLen      =20  !Length increment of temporary arrays

INTEGER, PARAMETER  ::  Front_Incident      =1  !Ray identification types
INTEGER, PARAMETER  ::  Front_Transmitted    =2
INTEGER, PARAMETER  ::  Front_Reflected    =3
INTEGER, PARAMETER  ::  Back_Incident      =4
INTEGER, PARAMETER  ::  Back_Transmitted    =5
INTEGER, PARAMETER  ::  Back_Reflected      =6


          ! DERIVED TYPE DEFINITIONS:
TYPE WindowIndex
    INTEGER   ::  NumStates  =0  !No States for this window
    INTEGER  ::  SurfNo    !Surface number of window
    !REAL(r64)  ::  Azimuth    !Window surface azimuth
    !REAL(r64)  ::  Tilt    !Window surface tilt
END TYPE WindowIndex

TYPE WindowStateIndex
    INTEGER  ::  InitInc    !Flag indicating initialization needed on Incoming basis
    INTEGER  ::  IncBasisIndx  !Index of basis list entry for Incoming basis
    INTEGER  ::  CopyIncState  !Pointer to state from which geometry can be copied (Incident)
    INTEGER  ::  InitTrn    !Flag indicating initialization needed on Outgoing basis
    INTEGER  ::  TrnBasisIndx  !Index of basis list entry for Outgoing basis
    INTEGER  ::  CopyTrnState  !Pointer to state from which geometry can be copied (Outgoing)
    INTEGER  ::  Konst    !Index of state descript in Construct array
    !INTEGER  ::  ThermConst  !Index of state thermal description in Construct array
END TYPE WindowStateIndex

          ! MODULE VARIABLE DECLARATIONS:

INTEGER                  ::  NumComplexWind  =0  !Total number of complex windows
TYPE (BasisStruct), DIMENSION(:), ALLOCATABLE          ::  BasisList
TYPE (WindowIndex), DIMENSION(:), ALLOCATABLE        ::  WindowList
TYPE (WindowStateIndex), DIMENSION(: , :), ALLOCATABLE        ::  WindowStateList
!Debug
INTEGER,DIMENSION(24,60)    ::  DbgIBm
REAL(r64),DIMENSION(24,60)  ::  DbgTheta,DbgPhi
REAL(r64)                  ::  DdbgTheta,DdbgPhi
!EndDebug

          ! SUBROUTINE SPECIFICATIONS FOR MODULE WindowComplexManager:
PUBLIC CalcComplexWindowThermal
PUBLIC CalculateBasisLength
PUBLIC InitComplexWindows
PUBLIC UpdateComplexWindows
PUBLIC DetermineMaxBackSurfaces
PUBLIC CheckCFSStates
PUBLIC PierceSurfaceVector
PUBLIC DaylghtAltAndAzimuth
PRIVATE CalculateWindowBeamProperties
PRIVATE CalcStaticProperties
PRIVATE InitBSDFWindows
PRIVATE ConstructBasis
PRIVATE FillBasisElement
PRIVATE SetupComplexWindowStateGeometry
PRIVATE CalcWindowStaticProperties
PRIVATE SkyWeight
PRIVATE SkyGndWeight
PRIVATE WorldVectFromW6
PRIVATE W6CoordsFromWorldVect
PRIVATE FindInBasis
PRIVATE SearchAscTable
PRIVATE CrossProduct
PRIVATE ExpandComplexState

CONTAINS


SUBROUTINE InitBSDFWindows

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set up the overall optical geometry for a BSDF window

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      ! na

         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
   TYPE TempBasisIdx
       INTEGER    ::  Basis    !Basis no in basis table
       INTEGER    ::  State    !State in which basis first occurs
   END TYPE TempBasisIdx

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   LOGICAL    ::  BasisFound  = .FALSE.    !Flag for sorting Basis List
   LOGICAL    ::  Once  =.TRUE.  !Flag for insuring things happen once
   INTEGER    ::  IBasis  =0  !Index for identifying basis in BasisList
   INTEGER     ::  ISurf  =0  !Index for sorting thru Surface array
   INTEGER    ::  IConst  =0  !Index for accessing Construct array
   INTEGER    ::  IState  =0  !Index identifying the window state for a particular window
   INTEGER    ::  IWind  =0  !Index identifying a window in the WindowList
   INTEGER    ::  I  =0  !general purpose index
   INTEGER    ::  J  =0  !general purpose index
   INTEGER    ::  JSurf  =0  !back surface number
   INTEGER      ::  BaseSurf    !base surface number (used in finding back surface)
   INTEGER    ::  K  =0  !general purpose index
   INTEGER    ::  KBkSurf  =0  !back surface index
   INTEGER    ::  KBasis  =0  !secondary reference to a basis index
   INTEGER    ::  NThetas  =0  !Current number of theta values
   INTEGER    ::  NumBasis =0  !Number of unique bases (No. in BasisList)
   INTEGER    ::  NumElem  =0  !Number of elements in current basis
   INTEGER    ::  NBkSurf   =0  !Local variable for the number of back surfaces
   INTEGER    ::  NumStates    !Local variable for the number of states
   INTEGER    ::  ElemNo  =0  !Current basis element number
   INTEGER    ::  ThConst  =0  !Construct array index of thermal construction of state
   REAL(r64)    ::  Theta  =0.0d0 !Current theta value
   REAL(r64)    ::  Phi  =0.0d0 !Current phi value
   REAL(r64)    ::  DPhi  =0.0d0 !Increment for phi value (Window6 type input)
   REAL(r64)    ::  Lamda  =0.0d0 !Current 'Lamda' value (element weight)
   INTEGER    ::  MatrixNo  =0 !Index of Basis matrix
   REAL(r64)    ::  NextTheta  =0.0d0 !Next theta in the W6 basis after current
   REAL(r64)    ::  LastTheta  =0.0d0 !Previous theta in the W6 basis before current
   REAL(r64)    ::  LowerTheta =0.0d0 !Lower theta boundary of the element
   REAL(r64)    ::  UpperTheta =0.0d0 !Upper theta boundary of the element
   REAL(r64)    ::  Azimuth    !Azimuth of window surface (radians)
   REAL(r64)    ::  Tilt    !Tilt of window surface (radians)
   REAL(r64), DIMENSION(:), ALLOCATABLE     :: Thetas  !temp array holding theta values
   INTEGER, DIMENSION(:), ALLOCATABLE    :: NPhis  !temp array holding number of phis for a given theta
   TYPE (BasisStruct), DIMENSION(:), ALLOCATABLE  :: TempList    !Temporary Basis List
   REAL(r64),DIMENSION(3)  ::     V    !vector array
   REAL(r64)    ::     VLen    !Length of vector array
   TYPE (TempBasisIdx), DIMENSION(:), ALLOCATABLE  :: IHold    !Temporary array
   INTEGER          :: NHold  !No. values in the Temporary array


                  IF (TotComplexFenStates <= 0) RETURN  !Nothing to do if no complex fenestration states
    !
    !Construct Basis List
    !
                 ALLOCATE(TempList(TotComplexFenStates))

    !Note:  Construction of the basis list contains the assumption of identical incoming and outgoing bases in
    !            that the complex fenestration state definition contains only one basis description, hence
    !            assumes square property matrices.  If this assumption were relaxed through change of the
    !            definition or additional definition of a state type with non-square matrices, then the loop
    !            below should be modified to enter both of the bases into the basis list.

  BsLoop:        DO IConst = FirstBSDF, FirstBSDF+TotComplexFenStates-1
                       MatrixNo = Construct(IConst)%BSDFInput%BasisMatIndex
                       IF (NumBasis ==0) THEN
                           NumBasis = 1
                           CALL ConstructBasis (IConst, TempList(1))
                       ELSE
  BLsLp:                   DO IBasis=1, NumBasis
                                 IF (MatrixNo == TempList(IBasis)%BasisMatIndex) CYCLE BsLoop
                           END DO BLsLp
                           NumBasis=NumBasis + 1
                           CALL ConstructBasis (IConst, TempList(NumBasis) )
                       ENDIF
                 END DO BsLoop
                 ALLOCATE(BasisList(NumBasis))
                 BasisList=TempList(1:NumBasis)
                 DEALLOCATE(TempList)
                   !
                   !  Proceed to set up geometry for complex fenestration states
                   !
                 ALLOCATE(ComplexWind(TotSurfaces))  !Set up companion array to SurfaceWindow to hold window
                           !     geometry for each state.  This is an allocatable array of
                           !     geometries for the window states but only the complex
                           !     fenestration surfaces will have the arrays allocated
      !
      !  Search Thru Surfaces for Complex Fenestration State references
      !  This will define the first complex fenestration state for that window, others will follow if there are
      !     control specifications
      !
                  ALLOCATE(WindowList(TotSurfaces))  !Temporary allocation
                  ALLOCATE(WindowStateList(TotSurfaces, TotComplexFenStates))  !Temporary allocation
  SfLoop:   DO ISurf = 1,TotSurfaces
                       IConst=Surface(ISurf)%Construction
                       IF (IConst == 0) CYCLE ! This is true for overhangs (Shading:Zone:Detailed)
                       IF (.NOT.(Construct(IConst)%TypeIsWindow.AND.(Construct(IConst)%WindowTypeBSDF))) CYCLE  !Only BSDF windows
                       !Simon Check: Thermal construction removed
                       !ThConst = Construct(IConst)%BSDFInput%ThermalConstruction
                       SurfaceWindow(ISurf)%WindowModelType = WindowBSDFModel
                       NumComplexWind = NumComplexWind + 1
                       NumStates = 1
                       WindowList(NumComplexWind)%NumStates = 1         !Having found the construction reference in
                                 ! the Surface array defines the first state for this window
                       WindowList(NumComplexWind)%SurfNo = ISurf
                       !WindowList(NumComplexWind)%Azimuth=DegToRadians*Surface(ISurf)%Azimuth
         !WindowList(NumComplexWind)%Tilt=DegToRadians*Surface(ISurf)%Tilt
         WindowStateList(NumComplexWind, NumStates)%InitInc = Calculate_Geometry
         WindowStateList(NumComplexWind, NumStates)%InitTrn = Calculate_Geometry
         WindowStateList(NumComplexWind, NumStates)%CopyIncState = 0
         WindowStateList(NumComplexWind, NumStates)%CopyTrnState = 0
         WindowStateList(NumComplexWind, NumStates)%Konst =IConst
         !Simon Check: ThermalConstruction assigned to current construction
        !WindowStateList(NumComplexWind, NumStates)%ThermConst = ThConst
         DO I = 1 , NumBasis    !Find basis in Basis List
               IF(Construct(IConst)%BSDFInput%BasisMatIndex == BasisList(I)%BasisMatIndex) THEN
                     WindowStateList(NumComplexWind, NumStates)%IncBasisIndx = I    !Note: square property matrices
                     WindowStateList(NumComplexWind, NumStates)%TrnBasisIndx = I    !   assumption
               ENDIF
         END DO
         IF(WindowStateList(NumComplexWind, NumStates)%IncBasisIndx <= 0) THEN
               CALL ShowFatalError('Complex Window Init: Window Basis not in BasisList.')
         ENDIF
                 END DO SfLoop
      !
      !  Should now have a WindowList with NumComplexWind entries containing all the complex fenestrations
      !    with a first state defined for each.
      !
      !  *  *  *
      !
      !  Here a search should be made for control specifications, which will give additional states for
      !    controlled complex fenestrations.  These should be added to the WindowStateList, and
      !     WindowList( )%NumStates incremented for each window for which states are added.
      !      Added states should have WindowStateList ( , )%InitInc set to Calculate_Geometry
      !
      !  *  *  *

      ! At this point, we have a complete WindowList and WindowStateList, with NumComplexWind
      !   defined, and NumStates for each complex window defined
      !
      ! Now sort through the window list to see that geometry will only be done once for each
      !  window, basis combination
      !
      ! Note:  code below assumes identical incoming and outgoing bases; following code will
      !   need revision if this assumption relaxed
      !

                 DO   IWind = 1,NumComplexWind     !Search window list for repeated bases
                       IF (WindowList(IWind)%NumStates > 1) THEN
                             ALLOCATE (IHold(WindowList(IWind)%NumStates))
                             NHold=1
                             IHold(1)%State = 1
                             IHold(1)%Basis = WindowStateList(IWind, 1)%IncBasisIndx
                                  ! If the Mth new basis found is basis B in the basis list, and it
                                  ! first occurs in the WindowStateList  in state N, then IHold(M)%Basis=B
                                  ! and IHold(M)%State=N
                             DO K = 1, NumBasis
                                 IF(K > NHold )  EXIT
                                 KBasis = IHold(K)%Basis
                                 J = IHold(K)%State
                                 Once = .TRUE.
                                 DO I = J+1 , WindowList(IWind)%NumStates    !See if subsequent states have the same basis
                                         IF(( WindowStateList(NumComplexWind, I)%InitInc == Calculate_Geometry) .AND. &
     &                                       (WindowStateList(NumComplexWind, I)%IncBasisIndx == KBasis )) THEN
           !Note:  square property matrices (same inc & trn bases) assumption
            !If same incident and outgoing basis assumption removed, following code will need to
            !  be extended to treat the two bases separately
                                             WindowStateList(NumComplexWind, I )%InitInc = Copy_Geometry
                                             WindowStateList(NumComplexWind, I )%InitTrn = Copy_Geometry
                                             WindowStateList(NumComplexWind, I )%CopyIncState = J
                                             WindowStateList(NumComplexWind, I )%CopyTrnState = J
                                         ELSE IF (Once) THEN
                                             Once = .FALSE.  !First occurrence of a different basis
                                             NHold = NHold +1
                                             IHold(NHold)%State = I
                                             IHold(NHold)%Basis = WindowStateList(IWind, I )%IncBasisIndx
                                             WindowStateList(NumComplexWind, I )%InitTrn = Calculate_Geometry
                                             WindowStateList(NumComplexWind, I )%CopyIncState = 0
                                             WindowStateList(NumComplexWind, I )%CopyTrnState = 0
                                         ENDIF
                                 END DO
                             END DO
                             DEALLOCATE( IHold )
                       ENDIF
                 END DO

      !
      !  Now go through window list and window state list and calculate or copy the
      !   geometry information for each window, state
                 DO  IWind = 1 , NumComplexWind
                       ISurf = WindowList( IWind )%SurfNo
                       NumStates = WindowList( IWind )%NumStates
                       !ALLOCATE (SurfaceWindow( ISurf )%ComplexFen)    !activate the BSDF window description
                 !  for this surface
                       SurfaceWindow(ISurf)%ComplexFen%NumStates = NumStates
                       ALLOCATE (SurfaceWindow( ISurf )%ComplexFen%State(NumStates))  !Allocate space for the states
                       ComplexWind( ISurf )%NumStates = NumStates
                       ALLOCATE (ComplexWind (ISurf )%Geom( NumStates ))  !Allocate space for the geometries
                       !Azimuth = WindowList ( IWind )%Azimuth
                       !Tilt = WindowList ( IWind )%Tilt
                       ! Get the number of back surfaces for this window
                       BaseSurf = Surface(ISurf)%BaseSurf     !ShadowComb is organized by base surface
                       NBkSurf = ShadowComb(BaseSurf)%NumBackSurf
                       ComplexWind (ISurf )%NBkSurf = NBkSurf
                       ! Define the back surface directions
                       ALLOCATE( ComplexWind (ISurf )%sWinSurf(NBkSurf))
                       ALLOCATE( ComplexWind (ISurf )%sdotN(NBkSurf))
                       !Define the unit vectors pointing from the window center to the back surface centers
                       DO KBkSurf  = 1 , NBkSurf
                           BaseSurf = Surface(ISurf)%BaseSurf     !ShadowComb is organized by base surface
                           JSurf = ShadowComb(BaseSurf)%BackSurf(KBkSurf)    !these are all proper back surfaces
                           V = Surface(JSurf)%Centroid - Surface(ISurf)%Centroid
                           VLen = SQRT( DOT_PRODUCT( V , V ) )
                           !Define the unit vector from the window center to the back
                           ComplexWind (ISurf )%sWinSurf( KBkSurf ) = V / VLen
                          !surface center
                          !Define the back surface cosine(incident angle)
                           ComplexWind (ISurf )%sdotN( KBkSurf ) = DOT_PRODUCT ( V , Surface(JSurf)%OutNormVec )/VLen
                       ENDDO
                       DO  IState = 1, NumStates
                         !The following assumes identical incoming and outgoing bases.  The logic will need to be
                         !  redesigned if this assumption is relaxed
                             IConst = WindowStateList ( IWind , IState )%Konst
                             !ThConst = WindowStateList ( IWind , IState )%ThermConst
                             SurfaceWindow(ISurf)%ComplexFen%State(IState)%Konst = IConst
                             !SurfaceWindow(ISurf)%ComplexFen%State(IState)%ThermConst = ThConst
                             IF ( WindowStateList( IWind , IState )%InitInc == Calculate_Geometry ) THEN
                                  ComplexWind (ISurf)%Geom(IState)%Inc = BasisList(WindowStateList(IWind , IState)&
     &      %IncBasisIndx)    !Put in the basis structure from the BasisList
                                  ComplexWind (ISurf)%Geom(IState)%Trn = BasisList(WindowStateList(IWind , IState)&
     &      %TrnBasisIndx)

                                  CALL SetupComplexWindowStateGeometry (ISurf, IState,IConst, &
     &                             ComplexWind(ISurf),ComplexWind( ISurf )%Geom(IState),&
     &                             SurfaceWindow( ISurf )%ComplexFen%State(IState))
         !Note--setting up the state geometry will include constructing outgoing basis/surface
         !  maps and those incoming maps that will not depend on shading.
                             ELSE
                                  SurfaceWindow (ISurf )%ComplexFen%State(IState)=SurfaceWindow( ISurf )%ComplexFen &
     &                                %State(WindowStateList( IWind , IState )%CopyIncState)   !Note this overwrites Konst
                                  SurfaceWindow (ISurf )%ComplexFen%State(IState)%Konst = IConst    !  so it has to be put back
                                  !SurfaceWindow (ISurf )%ComplexFen%State(IState)%ThermConst = ThConst  !same for ThermConst
                                  ComplexWind(ISurf)%Geom(IState) = ComplexWind(ISurf) &
     &                                %Geom(WindowStateList( IWind , IState )%CopyIncState)
                             ENDIF

                       END DO    !State loop
                 END DO       !Complex Window loop
                 !
                 !  Allocate all beam-dependent complex fenestration quantities
                 DO  IWind = 1 , NumComplexWind
                       ISurf = WindowList( IWind )%SurfNo
                       NumStates = WindowList( IWind )%NumStates
                       DO  IState = 1, NumStates
                         CALL AllocateCFSStateHourlyData(ISurf, IState)
                       END DO    !State loop
                 END DO       !Complex Window loop





  RETURN

END SUBROUTINE InitBSDFWindows

SUBROUTINE AllocateCFSStateHourlyData(iSurf, iState)

  ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Allocate hourly data arrays for complex fenestration state

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN) :: iSurf ! Surface number
          INTEGER, INTENT(IN) :: iState ! Complex fenestration state number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          INTEGER :: NLayers ! Number of complex fenestration layers
          INTEGER :: NBkSurf ! Number of back surfaces
          INTEGER :: KBkSurf ! Back surfaces counter

          NLayers = SurfaceWindow(iSurf)%ComplexFen%State(iState)%NLayers
          NBkSurf = ComplexWind(iSurf)%NBkSurf

          ALLOCATE(ComplexWind(iSurf)%Geom(iState)%SolBmGndWt(ComplexWind(iSurf)%Geom(iState)%NGnd, 24, NumOfTimeStepInHour))
          ALLOCATE(ComplexWind(iSurf)%Geom(iState)%SolBmIndex(24, NumOfTimeStepInHour))
          ALLOCATE(ComplexWind(iSurf)%Geom(iState)%ThetaBm(24, NumOfTimeStepInHour))
          ALLOCATE(ComplexWind(iSurf)%Geom(iState)%PhiBm(24, NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%WinDirHemiTrans(24, NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%WinDirSpecTrans(24, NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%WinBmGndTrans(24, NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%WinBmFtAbs(NLayers, 24, NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%WinBmGndAbs(NLayers ,24, NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%WinToSurfBmTrans (NBkSurf ,24 ,NumOfTimeStepInHour))
          ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%BkSurf(NBkSurf))
          DO KBkSurf  = 1 , NBkSurf
            ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%BkSurf(KBkSurf)%WinDHBkRefl(24, NumOfTimeStepInHour))
            ALLOCATE(SurfaceWindow(iSurf)%ComplexFen%State(iState)%BkSurf(KBkSurf)%WinDirBkAbs(NLayers , 24, NumOfTimeStepInHour))
          END DO

  RETURN
END SUBROUTINE AllocateCFSStateHourlyData

SUBROUTINE ExpandComplexState(iSurf, iConst)

  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Simon Vidanovic
  !       DATE WRITTEN   May 2013
  !       MODIFIED       Simon Vidanovic (July 2013)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! When complex fenestration is controlled by EMS, program does not know in advance how many states are assigned to
  ! ceratin surface. This information can be obtain only at runtime. Purpose of this routine is to extend number of states
  ! used by complex fenestration in case that is necessary.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  ! na

  implicit none ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(in) :: iSurf  ! Surface number
  Integer, Intent(in) :: iConst ! Construction number

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer :: NumOfStates
  type(BSDFGeomDescr), dimension(:), allocatable :: tempGeom
  type(BSDFDaylghtGeomDescr), dimension(:), allocatable :: tempDaylightGeom
  type(BSDFStateDescr), dimension(:), allocatable :: tempState

  ! Read all previous states into temporary locations and then expands them by one
  NumOfStates = SurfaceWindow(iSurf)%ComplexFen%NumStates

  if (.not.allocated(tempGeom)) allocate(tempGeom(NumOfStates))
  if (.not.allocated(tempState)) allocate(tempState(NumOfStates))

  tempGeom = ComplexWind(iSurf)%Geom
  tempState = SurfaceWindow(iSurf)%ComplexFen%State

  if (allocated(ComplexWind(iSurf)%Geom)) deallocate(ComplexWind(iSurf)%Geom)
  if (allocated(SurfaceWindow(iSurf)%ComplexFen%State)) deallocate(SurfaceWindow(iSurf)%ComplexFen%State)

  allocate(ComplexWind(iSurf)%Geom(NumOfStates + 1))
  allocate(SurfaceWindow(iSurf)%ComplexFen%State(NumOfStates + 1))

  ComplexWind(iSurf)%Geom(1:NumOfStates) = tempGeom
  SurfaceWindow(iSurf)%ComplexFen%State(1:NumOfStates) = tempState

  if (allocated(tempGeom)) deallocate(tempGeom)
  if (allocated(tempState)) deallocate(tempState)

  ! Do daylighting geometry only in case it is initialized. If daylighting is not used then no need to expand state for that
  !if (ComplexWind(iSurf)%DaylightingInitialized) then
    if (.not.allocated(tempDaylightGeom)) allocate(tempDaylightGeom(NumOfStates))
    tempDaylightGeom = ComplexWind(iSurf)%DaylghtGeom
    if (allocated(ComplexWind(iSurf)%DaylghtGeom)) deallocate(ComplexWind(iSurf)%DaylghtGeom)
    allocate(ComplexWind(iSurf)%DaylghtGeom(NumOfStates + 1))
    if (ComplexWind(iSurf)%DaylightingInitialized) then
      ComplexWind(iSurf)%DaylghtGeom(1:NumOfStates) = tempDaylightGeom
    end if
    if (allocated(tempDaylightGeom)) deallocate(tempDaylightGeom)
    ComplexWind(iSurf)%DaylightingInitialized = .false.
  !end if

  ! Increase number of states and insert new state
  NumOfStates = NumOfStates + 1
  SurfaceWindow(iSurf)%ComplexFen%NumStates = NumOfStates
  ComplexWind(iSurf)%NumStates = NumOfStates

  SurfaceWindow(iSurf)%ComplexFen%State(NumOfStates)%Konst = iConst

  ! load basis and setup window state geometry
  call ConstructBasis(iConst, ComplexWind(iSurf)%Geom(NumOfStates)%Inc)
  call ConstructBasis(iConst, ComplexWind(iSurf)%Geom(NumOfStates)%Trn)

  call SetupComplexWindowStateGeometry (iSurf, NumOfStates, iConst, &
    ComplexWind(iSurf), ComplexWind( iSurf )%Geom(NumOfStates), &
    SurfaceWindow(iSurf)%ComplexFen%State(NumOfStates))

  ! allocation of memory for hourly data can be performed only after window state geometry has been setup
  call AllocateCFSStateHourlyData(iSurf, NumOfStates)

  ! calculate static properties for complex fenestration
  call CalcWindowStaticProperties(iSurf, NumOfStates, ComplexWind(iSurf), &
    ComplexWind(iSurf)%Geom(NumOfStates), SurfaceWindow(iSurf)%ComplexFen%State(NumOfStates))

  ! calculate hourly data from complex fenestration
  call CFSShadeAndBeamInitialization(iSurf, NumOfStates, ComplexWind(ISurf), &
    ComplexWind(ISurf)%Geom(NumOfStates), SurfaceWindow(iSurf)%ComplexFen%State(NumOfStates))

END SUBROUTINE ExpandComplexState

SUBROUTINE CheckCFSStates(iSurf)

  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Simon Vidanovic
  !       DATE WRITTEN   May 2013
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! Check if there are new states available for complex fenestration and performs proper initialization

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  ! na

  implicit none ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(in) :: iSurf ! Surface number

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer :: NumOfStates   ! number of states for current surface
  Logical :: StateFound    ! variable to indicate if state has been found
  Integer :: i             ! Local counter
  Integer :: CurrentCFSState

  StateFound = .false.
  CurrentCFSState = SurfaceWindow(iSurf)%ComplexFen%CurrentState

  ! Check if EMS changed construction number
  if (Surface(iSurf)%Construction /= SurfaceWindow(iSurf)%ComplexFen%State(CurrentCFSState)%Konst) then

    ! If construction number changed then take new state
    ! First search for existing states. Maybe state is already added in previous timestep
    NumOfStates = SurfaceWindow(iSurf)%ComplexFen%NumStates
    do i = 1, NumOfStates
      if (Surface(iSurf)%Construction == SurfaceWindow(iSurf)%ComplexFen%State(i)%Konst) then
        StateFound = .true.
        CurrentCFSState = i
        SurfaceWindow(iSurf)%ComplexFen%CurrentState = i
      end if
    end do
  else
    StateFound = .true.
  end if

  ! If new state is not found in the list of current states, then create new one, initialize and make it active
  if (.not. StateFound) then
    call ExpandComplexState(iSurf, Surface(iSurf)%Construction)
    CurrentCFSState = SurfaceWindow(iSurf)%ComplexFen%NumStates
    SurfaceWindow(iSurf)%ComplexFen%CurrentState = CurrentCFSState
  end if

END SUBROUTINE CheckCFSStates

SUBROUTINE InitComplexWindows

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Extract simple init for Complex Windows

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
  LOGICAL,SAVE    ::  Once  =.TRUE.  !Flag for insuring things happen once

 !One-time initialization
  IF (Once) THEN
    ONCE = .FALSE.
    CALL InitBSDFWindows
    CALL CalcStaticProperties
  ENDIF


  RETURN

END SUBROUTINE InitComplexWindows

SUBROUTINE UpdateComplexWindows

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   August 2011
          !       MODIFIED       B. Griffith, Nov. 2012 revised for detailed timestep integration mode
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Performs the shading-dependent initialization of the Complex Fenestration data;
          ! On first call, calls the one-time initializition

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: KickoffSizing, KickoffSimulation

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
  ! LOGICAL,SAVE    ::  Once  =.TRUE.  !Flag for insuring things happen once
  INTEGER    ::  NumStates    ! Number of states for a given complex fen
  INTEGER     ::  ISurf    !Index for sorting thru Surface array
  INTEGER    ::  IConst    !Index for accessing Construct array
  INTEGER    ::  IState    !Index identifying the window state for a particular window
  INTEGER    ::  IWind    !Index identifying a window in the WindowList

!
!
! !One-time initialization
!  IF (Once) THEN
!    ONCE = .FALSE.
!    CALL InitBSDFWindows
!    CALL CalcStaticProperties
!  ENDIF

  IF (NumComplexWind == 0) RETURN

  IF (KickoffSizing .or. KickoffSimulation) RETURN

   !Shading-dependent initialization; performed once for each shading period

   ! Initialize the geometric quantities

  DO IWind = 1 , NumComplexWind
    ISurf = WindowList( IWind )%SurfNo
    NumStates = ComplexWind( ISurf )%NumStates
    DO  IState = 1 , NumStates
      CALL CFSShadeAndBeamInitialization(ISurf, IState, ComplexWind(ISurf), &
        ComplexWind(ISurf)%Geom(IState), SurfaceWindow(ISurf)%ComplexFen%State(IState))
    END DO    !State loop
  END DO    !window loop

  RETURN

END SUBROUTINE UpdateComplexWindows

SUBROUTINE CFSShadeAndBeamInitialization(iSurf, iState, Window, Geom, State)

  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Simon Vidanovic
  !       DATE WRITTEN   May 2013
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! Calculates shading properties of complex fenestration
  ! Refactoring from Klems code

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  USE vectors
  USE DataGlobals, ONLY: HourOfDay, TimeStep, KickoffSizing, KickoffSimulation
  USE DataSystemVariables, ONLY: DetailedSolarTimestepIntegration

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     ::  ISurf    !Window surface number
  INTEGER, INTENT(IN)     ::  IState    !Window state number
  TYPE (BSDFWindowGeomDescr),INTENT(INOUT)    ::  Window  !Window Geometry
  TYPE (BSDFGeomDescr), INTENT(INOUT)  ::  Geom    !State Geometry
  TYPE (BSDFStateDescr), INTENT(INOUT)  ::  State    !State Description

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (vector)    ::  SunDir =vector(0.0d0, 0.0d0, 1.0d0)    !unit vector pointing toward sun (world CS)
  TYPE (vector)    ::  Posit  =vector(0.0d0, 0.0d0, 1.0d0)    !vector location of current ground point
  TYPE (vector)    ::  HitPt  =vector(0.0d0, 0.0d0, 1.0d0)       !vector location of ray intersection with a surface
  REAL(r64)        ::  DotProd =0.0d0                  !temporary variable for testing dot products
  INTEGER          ::  I    !general purpose index
  INTEGER          ::  IncRay    !Index of incident ray corresponding to beam direction
  REAL(r64)        ::  Theta  !Theta angle of incident ray correspongind to beam direction
  REAL(r64)        ::  Phi  !Phi angle of incident ray correspongind to beam direction
  INTEGER          ::  IHit  =0    !hit flag
  INTEGER          ::  J    !general purpose index
  INTEGER          ::  JSurf    !general purpose surface number
  INTEGER          ::  K    !general purpose index
  INTEGER          ::  Hour    !hour of day
  INTEGER          ::  TotHits    !hit counter
  INTEGER          ::  TS    !time step

  IF (KickoffSizing .or. KickoffSimulation) RETURN

  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    DO Hour =1 , 24
      DO TS = 1, NumOfTimeStepInHour
        SunDir = SUNCOSTS(1:3,Hour,TS)
        Theta = 0.0d0
        Phi = 0.0d0
        IF (SUNCOSTS(3 ,Hour, TS) > SunIsUpValue) THEN
          IncRay = FindInBasis(SunDir, Front_Incident, ISurf, IState, ComplexWind(ISurf)%Geom(IState)%Inc, Theta, Phi)
          ComplexWind(ISurf)%Geom(IState)%ThetaBm(Hour,TS) = Theta
          ComplexWind(ISurf)%Geom(IState)%PhiBm(Hour,TS) = Phi
        ELSE
          ComplexWind(ISurf)%Geom(IState)%ThetaBm(Hour,TS) = 0.0d0
          ComplexWind(ISurf)%Geom(IState)%PhiBm(Hour,TS) = 0.0d0
          IncRay = 0   !sundown can't have ray incident on window
        ENDIF
        IF (IncRay > 0 ) THEN
          !Sun may be incident on the window
          ComplexWind(ISurf)%Geom(IState)%SolBmIndex(Hour,TS) = IncRay
        ELSE
          !Window can't be sunlit, set front incidence ray index to zero
          ComplexWind(ISurf)%Geom(IState)%SolBmIndex(Hour,TS) = 0
        ENDIF
        DO I = 1, ComplexWind(ISurf)%Geom(IState)%NGnd   !Gnd pt loop
          IHit = 0
          TotHits = 0
          DO JSurf = 1, TotSurfaces
            ! the following test will cycle on anything except exterior surfaces and shading surfaces
            IF( Surface(JSurf)%HeatTransSurf.AND.Surface(JSurf)%ExtBoundCond /= ExternalEnvironment) CYCLE
            !  skip surfaces that face away from the ground point
            DotProd = SunDir .dot. Surface(JSurf)%NewellSurfaceNormalVector
            IF (DotProd >= 0.0d0) CYCLE
            !Looking for surfaces between GndPt and sun
            CALL PierceSurfaceVector(JSurf, ComplexWind(ISurf)%Geom(IState)%GndPt(I), SunDir, IHit, HitPt)
            IF (IHit == 0) CYCLE
            !  Are not going into the details of whether a hit surface is transparent
            !  Since this is ultimately simply weighting the transmittance, so great
            !  detail is not warranted
            TotHits = TotHits + 1
            EXIT
          END DO
          IF (TotHits > 0) THEN
            ComplexWind(ISurf)%Geom(IState)%SolBmGndWt(I ,Hour, TS) = 0.0d0
          ELSE
            ComplexWind(ISurf)%Geom(IState)%SolBmGndWt(I ,Hour, TS) = 1.0d0
          ENDIF
        END DO  ! Gnd pt loop

        ! update window beam properties
        CALL CalculateWindowBeamProperties(ISurf, IState, ComplexWind(ISurf), &
          ComplexWind(ISurf)%Geom(IState), SurfaceWindow(ISurf)%ComplexFen%State(IState), Hour, TS)
      END DO  ! Timestep loop
    END DO     ! Hour loop
    ELSE ! detailed timestep integration
      SunDir = SUNCOSTS(1:3, HourOfDay, TimeStep)
      Theta = 0.0d0
      Phi = 0.0d0
      IF (SUNCOSTS(3 , HourOfDay, TimeStep ) > SunIsUpValue) THEN
        IncRay = FindInBasis ( SunDir, Front_Incident, ISurf, IState, ComplexWind(ISurf)%Geom(IState)%Inc, Theta, Phi)
        ComplexWind(ISurf)%Geom(IState)%ThetaBm(HourOfDay,TimeStep) = Theta
        ComplexWind(ISurf)%Geom(IState)%PhiBm(HourOfDay,TimeStep) = Phi
      ELSE
        ComplexWind(ISurf)%Geom(IState)%ThetaBm(HourOfDay,TimeStep) = 0.0d0
        ComplexWind(ISurf)%Geom(IState)%PhiBm(HourOfDay,TimeStep) = 0.0d0
        IncRay = 0   !sundown can't have ray incident on window
      ENDIF

      IF (IncRay > 0) THEN
        !Sun may be incident on the window
        ComplexWind(ISurf)%Geom(IState)%SolBmIndex(HourOfDay,TimeStep) = IncRay
      ELSE
        !Window can't be sunlit, set front incidence ray index to zero
        ComplexWind(ISurf)%Geom(IState)%SolBmIndex(HourOfDay,TimeStep) = 0.0d0
      ENDIF
      DO   I = 1 ,ComplexWind(ISurf)%Geom(IState)%NGnd   !Gnd pt loop
        IHit = 0
        TotHits = 0
        DO JSurf = 1, TotSurfaces
          ! the following test will cycle on anything except exterior surfaces and shading surfaces
          IF(Surface(JSurf)%HeatTransSurf.AND.Surface(JSurf)%ExtBoundCond /= ExternalEnvironment) CYCLE
          !  skip surfaces that face away from the ground point
          DotProd = SunDir.dot.Surface(JSurf)%NewellSurfaceNormalVector
          IF (DotProd >= 0.0d0 ) CYCLE
          !Looking for surfaces between GndPt and sun
          CALL PierceSurfaceVector(JSurf, ComplexWind(ISurf)%Geom(IState)%GndPt(I), SunDir, IHit, HitPt)
          IF (IHit == 0) CYCLE
          !  Are not going into the details of whether a hit surface is transparent
          !  Since this is ultimately simply weighting the transmittance, so great
          !  detail is not warranted
          TotHits = TotHits + 1
          EXIT
        END DO
        IF (TotHits > 0) THEN
          ComplexWind(ISurf)%Geom(IState)%SolBmGndWt (I, HourOfDay, TimeStep) = 0.0d0
        ELSE
          ComplexWind(ISurf)%Geom(IState)%SolBmGndWt (I, HourOfDay, TimeStep) = 1.0d0
        ENDIF
      END DO  ! Gnd pt loop

      ! Update window beam properties
      CALL CalculateWindowBeamProperties(ISurf, IState, ComplexWind(ISurf),&
        ComplexWind(ISurf)%Geom(IState), SurfaceWindow( ISurf )%ComplexFen%State(IState), HourOfDay, TimeStep)
    ENDIF ! solar calculation mode, average over days or detailed

  RETURN

END SUBROUTINE CFSShadeAndBeamInitialization

SUBROUTINE CalculateWindowBeamProperties(ISurf, IState, Window, Geom, State, Hour, TS)

  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Joe Klems
  !       DATE WRITTEN   August 2011
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! Calculates those optical properties of all the Complex Fenestrations that
  !  depend on the beam direction (hence, on hour and time step)

  ! METHODOLOGY EMPLOYED:
  ! Locate the bidirectional property matrices in the BSDFInput structure
  ! and use them to calculate the desired average properties.

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! INTEGER, INTENT(IN)     ::  IWind    !Window number (in WindowList)
  INTEGER, INTENT(IN)     ::  ISurf    !Window surface number
  INTEGER, INTENT(IN)     ::  IState    !Window state number
  TYPE (BSDFWindowGeomDescr),INTENT(INOUT)    ::  Window  !Window Geometry
  TYPE (BSDFGeomDescr), INTENT(INOUT)  ::  Geom    !State Geometry
  TYPE (BSDFStateDescr), INTENT(INOUT)  ::  State    !State Description
  INTEGER, INTENT(IN)     ::  Hour    !Hour number
  INTEGER, INTENT(IN)     ::  TS    !Timestep number

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER        ::  IConst    !State construction number
  INTEGER        ::  I    !general purpose index--Back surface
  INTEGER        ::  J    !general purpose index--ray
  INTEGER        ::  JRay    !ray index number
  REAL(r64)      ::  Theta
  REAL(r64)      ::  Phi
  INTEGER        ::  JSurf    !gen purpose surface no
  INTEGER              ::  BaseSurf    !base surface no
  INTEGER        ::  K    !general purpose index
  INTEGER        ::  M    !general purpose index--ray
  INTEGER        ::  L    !general purpose index--layer
  INTEGER        ::  KBkSurf    !general purpose index--back surface
  REAL(r64)        ::  Sum1    !general purpose sum
  REAL(r64)        ::  Sum2    !general purpose sum
  REAL(r64)        ::  Sum3    !general purpose sum
  INTEGER        ::  IBm    !index of beam ray in incoming basis
  INTEGER        ::  BkIncRay    !index of sun dir in back incidence basis
  LOGICAL        ::  RegWindFnd  !flag for regular exterior back surf window
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  RegWinIndex  !bk surf nos of reg windows
  INTEGER        ::  NRegWin  =0  !no reg windows found as back surfaces
  INTEGER        ::  KRegWin  =0  !index of reg window as back surface
  TYPE (vector)        ::  SunDir    !current sun direction
  REAL(r64)        ::  Refl    !temporary reflectance
  REAL(r64),DIMENSION(:), ALLOCATABLE  ::  Absorb    !temporary layer absorptance

  IConst = SurfaceWindow(ISurf)%ComplexFen%State(IState)%Konst

  !  Begin calculation
  !  Calculate the Transmittance from a given beam direction to a given zone surface

                 IBm = Geom%SolBmIndex( Hour, TS )
                 IF(IBm <= 0.0d0 ) THEN    !Beam cannot be incident on window for this Hour, TS
                     State%WinToSurfBmTrans( 1 : Window%NBkSurf , Hour , TS ) = 0.0d0
                     State%WinDirHemiTrans( Hour, TS ) = 0.0d0
                     State%WinDirSpecTrans( Hour, TS ) = 0.0d0
                     State%WinBmFtAbs( 1 : State%NLayers , Hour , TS ) = 0.0d0
                 ELSE
                     DO  I = 1 , Window%NBkSurf   !Back surface loop
                         Sum1 = 0.0d0
                         DO  J = 1 , Geom%NSurfInt ( I )   !Ray loop
                             Sum1 = Sum1 + Geom%Trn%Lamda(Geom%SurfInt( I , J ))  * &
                                Construct(IConst)%BSDFInput%SolFrtTrans ( Geom%SurfInt( I , J ) , IBm )
                         END DO    !Ray loop
                         State%WinToSurfBmTrans( I , Hour , TS ) = Sum1
                     END DO    !Back surface loop
                     !
                     !Calculate the directional-hemispherical transmittance
                     Sum1 = 0.0d0
                     DO  J = 1 , Geom%Trn%NBasis
                         Sum1 = Sum1 + Geom%Trn%Lamda(J) * Construct(IConst)%BSDFInput%SolFrtTrans ( J , IBm )
                     END DO
                     State%WinDirHemiTrans( Hour, TS ) = Sum1
                     !Calculate the directional specular transmittance
                     !Note:  again using assumption that Inc and Trn basis have same structure
                     State%WinDirSpecTrans( Hour, TS ) = Geom%Trn%Lamda(IBm)*Construct(IConst)%BSDFInput%SolFrtTrans ( IBm , IBm )
                     !Calculate the layer front absorptance for beam radiation
                     FORALL ( L = 1 : State%NLayers )
                         State%WinBmFtAbs( L , Hour , TS ) = Construct(IConst)%BSDFInput%Layer(L)%FrtAbs( 1 , IBm)
                     END FORALL
                 ENDIF
                 !Calculate,  for a given beam direction, the transmittance into the zone
                 ! for ground-reflected radiation (transmitted radiation assumed uniformly diffuse)

                 Sum1 = 0.0d0
                 Sum2 = 0.0d0
                 DO   J  = 1 , Geom%NGnd   !Incident ray loop
                     JRay = Geom%GndIndex( J )
                     IF ( Geom%SolBmGndWt( J , Hour, TS) > 0.0d0 )  THEN
                         Sum2 = Sum2 + Geom%SolBmGndWt( J ,Hour, TS) * Geom%Inc%Lamda( JRay)
                         DO  M = 1 , Geom%Trn%NBasis     !Outgoing ray loop
                             Sum1 = Sum1 + Geom%SolBmGndWt( J ,Hour, TS) * &
                               Geom%Inc%Lamda( JRay) * Geom%Trn%Lamda(M) * &
                                   Construct(IConst)%BSDFInput%SolFrtTrans ( M , JRay )
                         END DO  !Outgoing ray loop
                     ENDIF
                 END DO  !Indcident ray loop
                 IF (Sum2 > 0.0d0 ) THEN
                     State%WinBmGndTrans( Hour , TS ) = Sum1/Sum2
                 ELSE
                     State%WinBmGndTrans( Hour , TS ) = 0.0d0  !No unshaded ground => no transmittance
                 ENDIF

                 !Calculate,  for a given beam direction, the layer front absorptance
                 ! for ground-reflected radiation

                 DO L = 1 , State%NLayers     !layer loop
                     Sum1 = 0.0d0
                     Sum2 = 0.0d0
                     DO  J  = 1 , Geom%NGnd   !Incident ray loop
                         JRay = Geom%GndIndex( J )
                         IF ( Geom%SolBmGndWt( J , Hour, TS) > 0.0d0 ) THEN
                             Sum2 = Sum2 + Geom%SolBmGndWt( J ,Hour, TS) * &
                               Geom%Inc%Lamda( JRay)
                             Sum1 = Sum1 + Geom%SolBmGndWt( J ,Hour, TS) * &
                               Geom%Inc%Lamda( JRay)  * Construct(IConst)%BSDFInput%Layer(L)%FrtAbs ( 1 , JRay )
                          END IF
                     END DO  !Incident ray loop
                     IF (Sum2 > 0.0d0 ) THEN
                         State%WinBmGndAbs( L , Hour , TS ) = Sum1/Sum2
                     ELSE
                         State%WinBmGndAbs( L , Hour , TS ) = 0.0d0  !No unshaded ground => no absorptance
                     ENDIF
                 END DO      !layer loop

                 !Check the back surfaces for exterior windows
                 RegWindFnd = .FALSE.
                 NRegWin = 0.0d0
                 ALLOCATE(RegWinIndex( Window%NBkSurf) )
                 DO KBkSurf = 1 , Window%NBkSurf
                     BaseSurf = Surface(ISurf)%BaseSurf     !ShadowComb is organized by base surface
                     JSurf = ShadowComb(BaseSurf)%BackSurf(KBkSurf)
                     IF (SurfaceWindow(JSurf)%WindowModelType == WindowBSDFModel ) CYCLE
                     IF (.NOT. (Surface(JSurf)%Class == SurfaceClass_Window .OR. Surface(JSurf)%Class == &
                           SurfaceClass_GlassDoor ) ) CYCLE
                     IF (.NOT. (Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%ExtBoundCond == ExternalEnvironment &
                           .AND. Surface(JSurf)%ExtSolar ) ) CYCLE
                     ! Back surface is an exterior window or door
                     RegWindFnd = .TRUE.
                     NRegWin = NRegWin + 1
                     RegWinIndex(NRegWin) = KBkSurf
                 END DO
                 IF (RegWindFnd) THEN
                     ALLOCATE(Absorb(State%NLayers) )
                     SunDir = SUNCOSTS(1:3,Hour,TS)
                     BkIncRay = FindInBasis ( SunDir, Back_Incident,ISurf,IState,&
                                         ComplexWind(ISurf)%Geom(IState)%Trn, Theta, Phi)
                     IF ( BkIncRay > 0 ) THEN
                         !Here calculate the back incidence properties for the solar ray
                         !this does not say whether or not the ray can pass through the
                         !back surface window and hit this one!
                         Sum1 = 0.0d0
                         DO  J = 1 , Geom%Trn%NBasis
                             Sum1 = Sum1 + Geom%Trn%Lamda(J) * &
                                       Construct(IConst)%BSDFInput%SolBkRefl( J, BkIncRay )
                         END DO
                         Refl = Sum1
                         DO L = 1 , State%NLayers
                             Absorb(L) =Construct(IConst)%BSDFInput%Layer(L)%BkAbs(1 , BkIncRay )
                         END DO
                     ELSE
                         !solar ray can't be incident on back, so set properties equal to zero
                         Refl = 0.0d0
                         DO L = 1 , State%NLayers
                             Absorb(L) =0.0d0
                         END DO
                     ENDIF
                     DO KRegWin = 1, NRegWin
                         KBkSurf = RegWinIndex( KRegWin )
                         State%BkSurf(KBkSurf)%WinDHBkRefl( Hour, TS )= Refl
                         DO L = 1 , State%NLayers
                             State%BkSurf(KBkSurf)%WinDirBkAbs (L , Hour, TS ) = Absorb(L)
                         END DO
                     ENDDO
                 ENDIF
                 IF (ALLOCATED(Absorb) ) DEALLOCATE (Absorb)
                 DEALLOCATE(RegWinIndex)

!
!

  RETURN

END SUBROUTINE CalculateWindowBeamProperties


SUBROUTINE CalcStaticProperties

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates those optical properties of all the Complex Fenestrations that
          ! do not depend on the beam direction (hence, on hour and time step)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

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
   INTEGER     ::  ISurf  =0  !Index for sorting thru Surface array
   INTEGER    ::  IConst  =0  !Index for accessing Construct array
   INTEGER    ::  IState  =0  !Index identifying the window state for a particular window
   INTEGER    ::  IWind  =0  !Index identifying a window in the WindowList
   INTEGER    ::  I  =0  !general purpose index
   INTEGER    ::  J  =0  !general purpose index
   INTEGER    ::  K  =0  !general purpose index
   INTEGER    ::  NumStates  =0  !local copy of no of states


!
!
  DO IWind = 1 , NumComplexWind
    ISurf = WindowList( IWind )%SurfNo
    NumStates = WindowList( IWind )%NumStates
    DO IState = 1, NumStates
      ! IConst = WindowStateList ( IWind , IState )%Konst
      SurfaceWindow(ISurf)%ComplexFen%State(IState)%Konst = WindowStateList (IWind ,IState)%Konst
      CALL CalcWindowStaticProperties(ISurf, IState, ComplexWind(ISurf), &
        ComplexWind(ISurf)%Geom(IState), SurfaceWindow(ISurf)%ComplexFen%State(IState))
    END DO
  END DO

  RETURN

END SUBROUTINE CalcStaticProperties

SUBROUTINE CalculateBasisLength ( Input,IConst,NBasis )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the basis length for a Window6 Non-Symmetric or Axisymmetric basis
          ! from the input basis matrix

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE (BSDFWindowInputStruct), INTENT(IN)       :: Input           ! BSDF data input struct for this construction
  INTEGER, INTENT(IN)                            :: IConst          !Construction number of input
  INTEGER, INTENT(OUT)                      :: NBasis     ! Calculated Basis length


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          INTEGER     ::  I     !gen purpose index

          IF (Input%BasisMatNcols == 1) THEN
              !Axisymmetric basis, No. rows is no. of thetas = basis length
              NBasis = Input%BasisMatNrows
              RETURN
          ENDIF
          NBasis = 1
          DO I = 2, Input%BasisMatNrows
          NBasis = NBasis + FLOOR(Construct(IConst)%BSDFInput%BasisMat(I,2)+0.001)
          END DO
  RETURN

END SUBROUTINE CalculateBasisLength

SUBROUTINE DetermineMaxBackSurfaces

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   September 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the basis length for a Window6 Non-Symmetric or Axisymmetric basis
          ! from the input basis matrix

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
          INTEGER     ::  I     !gen purpose index
          INTEGER     ::  ZoneNum   !Zone Number
          INTEGER     ::  SurfNum   !Surface Number
          INTEGER     ::  NumSurfInZone =0  !Number of zone surfaces
          LOGICAL     ::  ComplexFenInZone  = .FALSE.

      DO ZoneNum = 1, NumOfZones
          ComplexFenInZone = .FALSE.
          DO SurfNum = Zone(ZoneNum)%SurfaceFirst , Zone(ZoneNum)%SurfaceLast
              IF(SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) ComplexFenInZone = .TRUE.
          END DO
          IF(ComplexFenInZone) THEN
              NumSurfInZone = Zone(ZoneNum)%SurfaceLast - Zone(ZoneNum)%SurfaceFirst + 1
              IF(MaxBkSurf < NumSurfInZone) MaxBkSurf = NumSurfInZone
          END IF
      END DO

  RETURN

END SUBROUTINE DetermineMaxBackSurfaces

SUBROUTINE ConstructBasis (IConst, Basis)
         ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN  June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set up a basis from the matrix information pointed to in Construction by ICons

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN)    ::  IConst    !Index for accessing Construct array
   TYPE (BasisStruct), INTENT(OUT)  ::  Basis

         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

   INTEGER    ::  I  =0  !general purpose index
   INTEGER    ::  J  =0  !general purpose index
   INTEGER    ::  NThetas  =0  !Current number of theta values
   INTEGER    ::  NumElem  =0  !Number of elements in current basis
   INTEGER    ::  ElemNo  =0  !Current basis element number
   INTEGER      ::  MaxNPhis    !Max no of NPhis for any theta
   REAL(r64)    ::  Theta  =0.0d0 !Current theta value
   REAL(r64)    ::  Phi  =0.0d0 !Current phi value
   REAL(r64)    ::  DTheta  =0.0d0 !Increment for theta value (Window6 type input)
   REAL(r64)    ::  DPhi  =0.0d0 !Increment for phi value (Window6 type input)
   REAL(r64)    ::  HalfDTheta =0.0d0 !Half-width of all theta bins except first and last (W6 input)
   REAL(r64)    ::  Lamda  =0.0d0 !Current 'Lamda' value (element weight)
   REAL(r64)    ::  SolAng  =0.0d0 !Current element solid angle
   REAL(r64)    ::  NextTheta  =0.0d0 !Next theta in the W6 basis after current
   REAL(r64)    ::  LastTheta  =0.0d0 !Previous theta in the W6 basis before current
   REAL(r64)    ::  LowerTheta =0.0d0 !Lower theta boundary of the element
   REAL(r64)    ::  UpperTheta =0.0d0 !Upper theta boundary of the element
   REAL(r64)    ::  Azimuth    !Azimuth of window surface (radians)
   REAL(r64)    ::  Tilt    !Tilt of window surface (radians)
   REAL(r64), DIMENSION(:), ALLOCATABLE :: Thetas  !temp array holding theta values
   INTEGER, DIMENSION(:), ALLOCATABLE  :: NPhis  !temp array holding number of phis for a given theta
   !
   !
   !

            NThetas = Construct(IConst)%BSDFInput%BasisMatNrows  !Note here assuming row by row input
            Basis%NThetas = NThetas
            Basis%BasisMatIndex = Construct(IConst)%BSDFInput%BasisMatIndex
            Basis%NBasis = Construct(IConst)%BSDFInput%NBasis
            ALLOCATE(Basis%Grid(Basis%NBasis))
            ALLOCATE(Thetas(NThetas+1))    !Temp array
                  !By convention the Thetas array contains a final point at Pi/2 which is not a basis element
            ALLOCATE(NPhis(NThetas+1))    !Temp array
            ALLOCATE(Basis%Thetas(NThetas+1))
            ALLOCATE(Basis%NPhis(NThetas+1))

            ALLOCATE(Basis%Lamda(Construct(IConst)%BSDFInput%NBasis))
            ALLOCATE(Basis%SolAng(Construct(IConst)%BSDFInput%NBasis))
  BTW:    IF(Construct(IConst)%BSDFInput%BasisType == BasisType_WINDOW) THEN
    !
    !   WINDOW6 Basis
    !
    Basis%BasisType = BasisType_WINDOW
    BST:      IF(Construct(IConst)%BSDFInput%BasisSymmetryType == BasisSymmetry_None) THEN
            !
            ! No basis symmetry
            !
            Basis%BasisSymmetryType =   BasisSymmetry_None
            Thetas(1) = 0.0d0      !By convention, the first basis point is at the center (theta=0,phi=0)
            Thetas(NThetas + 1) = 0.5d0*Pi    !and there is an N+1st point (not a basis element) at Pi/2
            NPhis(1) = 1
            NumElem = 1
            DO I = 2,NThetas
              Thetas(I)=Construct(IConst)%BSDFInput%BasisMat(I,1)*DegToRadians
              NPhis(I)=FLOOR(Construct(IConst)%BSDFInput%BasisMat(I,2)+0.001)
              IF(NPhis(I)<=0) Call ShowFatalError('WindowComplexManager: incorrect input, no. phis must be positive.')
              NumElem=NumElem+NPhis(I)
            END DO
            MaxNPhis =MAXVAL(NPhis(1:NThetas))
            ALLOCATE(Basis%Phis( MaxNPhis + 1 , NThetas+1 ) )  !N+1st Phi point (not basis element) at 2Pi
          ALLOCATE( Basis%BasisIndex ( NThetas+1 , MaxNPhis ) )
          Basis%Phis =0.0d0   !Initialize so undefined elements will contain zero
          Basis%BasisIndex = 0  !Initialize so undefined elements will contain zero
            IF (NumElem /= Construct(IConst)%BSDFInput%NBasis) THEN  !Constructed Basis must match property matrices
           CALL ShowFatalError('WindowComplexManager: Constructed basis length does not match property matrices.')
            ENDIF
               Basis%Thetas = Thetas
               Basis%NPhis = NPhis
               ElemNo = 0
   ThLoop: DO I = 1,NThetas
              Theta = Thetas(I)
              IF ( I == 1 ) THEN  !First theta value must always be zero
              HalfDTheta=0.5d0*Thetas(I+1)
               LastTheta = 0.0d0
               NextTheta = Thetas(I+1)
               LowerTheta = 0.0d0
               UpperTheta = HalfDTheta
              ELSE IF(I > 1 .AND. I < NThetas) THEN
               LastTheta = Thetas(I-1)
               NextTheta = Thetas(I+1)
               LowerTheta=UpperTheta
               HalfDTheta = Theta - LowerTheta
               UpperTheta = Theta + HalfDTheta
             ELSE IF (I == NThetas) THEN
               LastTheta = Thetas(I-1)
               NextTheta = 0.5d0*Pi
               LowerTheta = UpperTheta  !It is assumed that Thetas(N) is the mean between the previous
                     !UpperTheta and pi/2.
               UpperTheta = 0.5d0*Pi
             ENDIF
             DPhi=2.0d0*Pi/NPhis(I)
             IF (I==1) THEN
               Lamda = Pi*(SIN(UpperTheta))**2
               SolAng = 2.0d0*Pi*(1.0d0 - COS(UpperTheta))
             ELSE
               Lamda=0.5d0*DPhi*((SIN(UpperTheta))**2-(SIN(LowerTheta))**2)  !For W6 basis, lamda is funct of Theta and
                    ! NPhis, not individual Phi
               SolAng = DPhi*(COS(LowerTheta) - COS(UpperTheta))
             END IF
             DTheta = UpperTheta-LowerTheta
             Basis%Phis ( NPhis(I) + 1 , I ) = 2.0d0*Pi  !Non-basis-element Phi point for table searching in Phi
             DO J = 1,NPhis(I)
               ElemNo = ElemNo+1
               Basis%BasisIndex(I,J) = ElemNo
               Phi = (J-1)*DPhi
               Basis%Phis( J , I ) = Phi  !Note: this ordering of I & J are necessary to allow Phis(Theta) to
                     !  be searched as a one-dimensional table
               Call FillBasisElement (Theta,Phi,ElemNo,Basis%Grid(ElemNo) &
                      &    ,LowerTheta,UpperTheta,DPhi,BasisType_WINDOW)  !This gets all the simple grid characteristics
                                         Basis%Lamda(ElemNo) = Lamda
                                         Basis%SolAng(ElemNo) = SolAng
                           END DO
             END DO ThLoop
          ELSE BST
              !
              !  Axisymmetric basis symmetry (Note this only useful specular systems, where it allows shorter data input)
              !
              Basis%BasisSymmetryType =   BasisSymmetry_Axisymmetric
            Thetas(1) = 0.0d0      !By convention, the first basis point is at the center (theta=0,phi=0)
            Thetas(NThetas + 1) = 0.5d0*Pi    !and there is an N+1st point (not a basis element) at Pi/2
            NPhis = 1      !As insurance, define one phi for each theta
            NumElem = 1
            DO I = 2,NThetas
              Thetas(I)=Construct(IConst)%BSDFInput%BasisMat(I,1)*DegToRadians
              NumElem=NumElem+1
            END DO
            ALLOCATE(Basis%Phis( NThetas , 1 ) )
          ALLOCATE( Basis%BasisIndex ( NThetas , 1 ) )
          Basis%Phis =0.0d0   !Initialize so undefined elements will contain zero
          Basis%BasisIndex = 0  !Initialize so undefined elements will contain zero
            IF (NumElem /= Construct(IConst)%BSDFInput%NBasis) THEN  !Constructed Basis must match property matrices
               CALL ShowFatalError('WindowComplexManager: Constructed basis length does not match property matrices.')
            ENDIF
               Basis%Thetas = Thetas
               Basis%NPhis = NPhis
               ElemNo = 0
               DPhi=2.0d0*Pi
   ThLoop2:   DO I = 1,NThetas
                   Theta = Thetas(I)
                   IF ( I == 1 ) THEN  !First theta value must always be zero
                       HalfDTheta=0.5d0*Thetas(I+1)
                       LastTheta = 0.0d0
                       NextTheta = Thetas(I+1)
                       LowerTheta = 0.0d0
                       UpperTheta = HalfDTheta
                   ELSE IF(I > 1 .AND. I < NThetas) THEN
                       LastTheta = Thetas(I-1)
                       NextTheta = Thetas(I+1)
                       LowerTheta=UpperTheta
                       HalfDTheta = Theta - LowerTheta
                       UpperTheta = Theta + HalfDTheta
                   ELSE IF (I == NThetas) THEN
                       LastTheta = Thetas(I-1)
                       NextTheta = 0.5d0*Pi
                       LowerTheta = UpperTheta  !It is assumed that Thetas(N) is the mean between the previous
                     !UpperTheta and pi/2.
                       UpperTheta = 0.5d0*Pi
                  ENDIF
                  IF (I==1) THEN
                       Lamda = Pi*(SIN(UpperTheta))**2
                       SolAng = 2.0d0*Pi*(1.0d0 - COS(UpperTheta))
                  ELSE
                       Lamda=0.5d0*DPhi*((SIN(UpperTheta))**2-(SIN(LowerTheta))**2)  !For W6 basis, lamda is funct of Theta and
                    ! NPhis, not individual Phi
                       SolAng = DPhi*(COS(LowerTheta) - COS(UpperTheta))
                  END IF
                       DTheta = UpperTheta-LowerTheta
                       ElemNo = ElemNo + 1
                       Basis%BasisIndex(I,1) = ElemNo
                       Phi = 0.0d0
                       Basis%Phis( 1 , I ) = Phi  !Note: this ordering of I & J are necessary to allow Phis(Theta) to
                     !  be searched as a one-dimensional table
                       Call FillBasisElement (Theta,Phi,ElemNo,Basis%Grid(ElemNo)&
                        ,LowerTheta,UpperTheta,DPhi,BasisType_WINDOW)  !This gets all the simple grid characteristics
                       Basis%Lamda(ElemNo) = Lamda
                       Basis%SolAng(ElemNo) = SolAng

          END DO ThLoop2
            END IF BST
          ELSE BTW
            Call ShowFatalError('WindowComplexManager: Non-Window6 basis type not yet implemented.')
          END IF BTW
    DEALLOCATE(Thetas,NPhis)

   RETURN

END SUBROUTINE ConstructBasis

SUBROUTINE FillBasisElement (Theta,Phi,Elem,BasisElem,LowerTheta,UpperTheta,DPhi,InputType)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! fill in values for all the components of a basis element

          ! METHODOLOGY EMPLOYED:
          ! <n/a>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT (IN) :: Theta       !Central polar angle of element
  REAL(r64), INTENT (IN) :: Phi         !Central azimuthal angle of element
  REAL(r64), INTENT (IN) :: LowerTheta  !Lower edge of element (polar angle)
  REAL(r64), INTENT (IN) :: UpperTheta  !Upper edge of element (polar angle)
  REAL(r64), INTENT (IN) :: DPhi        !Width of element (azimuthal angle)
  INTEGER, INTENT (IN)   :: Elem        !Index number of element in basis
  INTEGER, INTENT (IN)   :: InputType   !Basis type
  TYPE (BasisElemDescr), INTENT(INOUT)  :: BasisElem

         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (InputType == BasisType_WINDOW) THEN
    !
    !WINDOW6 Type BASIS
    !
    IF(Elem == 1) THEN
  !first element, theta=0, is special case
      BasisElem%Theta = Theta
      BasisElem%Phi = 0.0d0
      BasisElem%dPhi = 2.0d0*Pi
      BasisElem%UpprTheta =UpperTheta
      BasisElem%dTheta = BasisElem%UpprTheta-Theta
      BasisElem%LwrTheta = Theta
      BasisElem%LwrPhi = 0.0d0
      BasisElem%UpprPhi =  2.0d0*Pi
    ELSE
      BasisElem%Theta =Theta
      BasisElem%Phi = Phi
      BasisElem%dPhi = DPhi
      BasisElem%LwrPhi = Phi-DPhi/2.0d0
      BasisElem%UpprPhi = Phi+DPhi/2.0d0
      BasisElem%LwrTheta = LowerTheta
      BasisElem%UpprTheta = UpperTheta
      BasisElem%dTheta = BasisElem%UpprTheta-BasisElem%LwrTheta
    END IF
  ELSE
    !
    !Non-WINDOW6 Type Basis
    !
    !Currently not implemented
    CALL ShowFatalError('WindowComplexManager: Custom basis type not yet implemented.')
  END IF
  RETURN

END SUBROUTINE FillBasisElement

SUBROUTINE SetupComplexWindowStateGeometry (ISurf, IState,IConst,Window,Geom,State)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         J. Klems
          !       DATE WRITTEN   June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Define all the geometric quantites for a complex fenestration state

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
       !INTEGER, INTENT(IN)      ::  IWind            !Complex fenestration number (in window list)
       INTEGER, INTENT(IN)      ::  ISurf    !Surface number of the complex fenestration
       INTEGER, INTENT(IN)      ::  IState    !State number of the complex fenestration state
       INTEGER, INTENT(IN)      ::  IConst    !Pointer to construction for this state
       TYPE (BSDFWindowGeomDescr),INTENT(INOUT)    ::  Window  !Window Geometry
       TYPE (BSDFGeomDescr), INTENT(INOUT)  ::  Geom    !State Geometry
       TYPE (BSDFStateDescr), INTENT(INOUT)  ::  State    !State Description

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
       TYPE BackHitList
         INTEGER                ::  KBkSurf     !Back surface index of the hit surface
         INTEGER        ::  HitSurf    !Surface number of the hit surface
         TYPE (vector)        ::  HitPt    !coords of hit pt (world syst)
         REAL(r64)        ::  HitDsq    !Squared distance to the current hit pt
       END TYPE BackHitList

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
       REAL(r64)      ::  Azimuth    !Complex fenestration azimuth
       REAL(r64)      ::  Tilt    !Complex fenestration tilt
       INTEGER        ::  ElemNo    !Grid index variable
       INTEGER        ::  IHit    !Surface intersection flag
       INTEGER        ::  I, J    !Temp Indices
       INTEGER        ::  IRay    !Ray index variable
       INTEGER        ::  IZone    !Zone containing the complex window
       INTEGER        ::  JSurf    !Secondary Surface index
       INTEGER              ::  BaseSurf    !base surface index
       INTEGER              ::  KBkSurf     !Back surface index
       INTEGER        ::  MaxHits    !Max no of hits found
       INTEGER        ::  MaxInt    !Max no of intersections found
       INTEGER        ::  NSky    !No of sky rays
       INTEGER        ::  NGnd    !No of gnd rays
       INTEGER        ::  NReflSurf  !No of rays striking ext surfaces
       INTEGER        ::  NBkSurf  !No of back surfaces
       INTEGER        ::  TotHits    !Current number of surface intersections
       REAL(r64)        ::  Theta    !Basis theta angle
       REAL(r64)        ::   Phi    !Basis phi angle
       REAL(r64)        ::   HitDsq    !Squared distance to current hit pt
       REAL(r64)        ::   LeastHitDsq  !Squared distance to closest hit pt
       TYPE (vector)        ::   HitPt    !coords of hit pt (world syst)
       REAL(r64),DIMENSION(3)      ::   V    !vector array
       REAL(r64)        ::   VLen    !Length of vector array
       TYPE (vector)        ::   X    !position vector
       TYPE (vector)        ::   VecNorm  !outer normal vector
       INTEGER, DIMENSION(:), ALLOCATABLE  ::  TmpRfSfInd  !Temporary RefSurfIndex
       INTEGER, DIMENSION(:), ALLOCATABLE  ::  TmpRfRyNH  !Temporary RefRayNHits
       INTEGER, DIMENSION(:,:), ALLOCATABLE    ::  TmpHSurfNo  !Temporary HitSurfNo
       REAL(r64), DIMENSION(:,:), ALLOCATABLE    ::  TmpHSurfDSq  !Temporary HitSurfDSq
        INTEGER, DIMENSION(:), ALLOCATABLE    ::  TmpSkyInd  !Temporary sky index list
        INTEGER, DIMENSION(:), ALLOCATABLE    ::  TmpGndInd  !Temporary gnd index list
       TYPE (vector), DIMENSION(:), ALLOCATABLE    ::  TmpGndPt  !Temporary ground intersection list
       INTEGER, DIMENSION(:,:),ALLOCATABLE  ::  TmpSurfInt  !Temporary index of ray intersecing back surf
       REAL(r64), DIMENSION(:,:),ALLOCATABLE  ::  TmpSjdotN  !Temporary dot prod of ray angle w bk surf norm
       INTEGER, DIMENSION(:), ALLOCATABLE  ::  ITemp1D  !Temporary INT 1D array
       REAL(r64), DIMENSION(:,:), ALLOCATABLE    ::  Temp2D    !Temporary real 2D array
       TYPE (vector), DIMENSION(:,:), ALLOCATABLE  ::  TempV2D  !Temporary vector 2D array
       TYPE (vector), DIMENSION(:,:), ALLOCATABLE  ::  TmpHitPt    !Temporary HitPt
       REAL(r64)        ::  TransRSurf  !Norminal transmittance of shading surface
       REAL(r64)                         ::  WtSum    !Sum for normalizing various weights
       TYPE (BackHitList)      ::  BSHit    !Temp list of back surface hit quantities for a ray
       REAL(r64)                ::      DotProd     !Temporary variable for manipulating dot product .dot.

              !This routine primarily fills in the BSDFGeomDescr type for a given window and state
              !Note that on call the incoming and outgoing basis structure types have already been filled in
                  !
              !  Define the central ray directions (in world coordinate system)
              !

        SurfaceWindow(ISurf)%ComplexFen%State(IState)%NLayers = Construct(IConst)%BSDFInput%NumLayers
        Azimuth = DegToRadians * Surface(ISurf)%Azimuth
        Tilt = DegToRadians * Surface(ISurf)%Tilt

         !For incoming grid

         ALLOCATE (Geom%sInc(Geom%Inc%NBasis))
         Geom%sInc = vector(0.0d0, 0.0d0, 0.0d0)
         ALLOCATE (Geom%pInc(Geom%Inc%NBasis))
         ALLOCATE (Geom%CosInc(Geom%Inc%NBasis))
         ALLOCATE (Geom%DAInc(Geom%Inc%NBasis))
         Geom%pInc = BSDFDaylghtPosition(0.0d0, 0.0d0)
          DO ElemNo=1 , Geom%Inc%NBasis
             Theta = Geom%Inc%Grid(ElemNo)%Theta
             Phi = Geom%Inc%Grid(ElemNo)%Phi
                       !The following puts in the vectors depending on
                       ! window orientation
             Geom%sInc(ElemNo)  = WorldVectFromW6 (Theta, Phi, Front_Incident, Tilt, Azimuth)
             Geom%pInc(ElemNo)  = DaylghtAltAndAzimuth(Geom%sInc(ElemNo))

             Geom%CosInc(ElemNo) = COS(Geom%Inc%Grid(ElemNo)%Theta)
             !Geom%DAInc(ElemNo) = COS(Geom%pInc(ElemNo)%Altitude) * Geom%Inc%Grid(ElemNo)%dTheta * Geom%Inc%Grid(ElemNo)%dPhi
             ! Geom%DAInc(ElemNo) = Geom%Inc%Grid(ElemNo)%dTheta * Geom%Inc%Grid(ElemNo)%dPhi
             Geom%DAInc(ElemNo) = COS(Geom%Inc%Grid(ElemNo)%Theta) * Geom%Inc%Grid(ElemNo)%dTheta * Geom%Inc%Grid(ElemNo)%dPhi
          END DO
         !  For outgoing grid
         ALLOCATE (Geom%sTrn(Geom%Trn%NBasis))
         Geom%sTrn = vector(0.0d0, 0.0d0, 0.0d0)
         ALLOCATE (Geom%pTrn(Geom%Trn%NBasis))
         Geom%pTrn = BSDFDaylghtPosition(0.0d0, 0.0d0)
          DO ElemNo=1,Geom%Trn%NBasis
             Theta = Geom%Trn%Grid(ElemNo)%Theta
             Phi = Geom%Trn%Grid(ElemNo)%Phi
                       !The following puts in the vectors depending on
                       ! window orientation
             Geom%sTrn(ElemNo) = WorldVectFromW6 (Theta, Phi, Front_Transmitted, Tilt, Azimuth)
             Geom%pTrn(ElemNo)  = DaylghtAltAndAzimuth(Geom%sTrn(ElemNo))
          END DO
              !
              !  Incident Basis:
              !  Construct sky and ground ray index maps, and list of rays intersecting exterior surfaces
              !
                  !Sky, and ground ray index maps, and rays that are potentially beam radiation reflected from exterior surfaces
                  ALLOCATE(TmpRfSfInd (Geom%Inc%NBasis))
                  ALLOCATE(TmpRfRyNH (Geom%Inc%NBasis))
                  ALLOCATE(TmpHSurfNo (Geom%Inc%NBasis,TotSurfaces))
                  ALLOCATE(TmpHSurfDSq (Geom%Inc%NBasis,TotSurfaces))
                  ALLOCATE(TmpHitPt (Geom%Inc%NBasis,TotSurfaces))
                  ALLOCATE(TmpSkyInd (Geom%Inc%NBasis))
                  ALLOCATE(TmpGndInd(Geom%Inc%NBasis))
                  ALLOCATE(TmpGndPt(Geom%Inc%NBasis))
          NSky = 0
          NGnd = 0
          NReflSurf = 0
          TmpRfRyNH = 0
          Geom%NSkyUnobs = 0
          Geom%NGndUnobs = 0
          !  Note--this loop could be repeated for different positions in the window plane (as for detailed reflection
          !  calculations, varying the origin in the call to PierceSurface.  Essentially, have set NsubV =1.
          DO IRay = 1, Geom%Inc%NBasis
              IF (Geom%sInc(IRay)%z < 0.0d0) THEN
                  ! A ground ray
                  Geom%NGndUnobs = Geom%NGndUnobs + 1
              ELSE
                  ! A sky ray
                  Geom%NSkyUnobs = Geom%NSkyUnobs + 1
              ENDIF
          !
          ! Exterior reveal shadowing/reflection treatment should be inserted here
          !
             IHit = 0
             TotHits = 0
             DO JSurf = 1, TotSurfaces
               ! the following test will cycle on anything except exterior surfaces and shading surfaces
               IF( Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%ExtBoundCond /= ExternalEnvironment) CYCLE
               !  skip the base surface containing the window and any other subsurfaces of that surface
               IF( JSurf == Surface(ISurf)%BaseSurf .OR. Surface(JSurf)%BaseSurf == Surface(ISurf)%BaseSurf) CYCLE
               !  skip surfaces that face away from the window
               DotProd = Geom%sInc(IRay) .dot. Surface(JSurf)%NewellSurfaceNormalVector
               IF( DotProd >= 0.0d0 ) CYCLE
               CALL PierceSurfaceVector(JSurf, Surface(ISurf)%Centroid, Geom%sInc(IRay), IHit, HitPt)
               IF (IHit <= 0) CYCLE
               IHit = 0  !A hit, clear the hit flag for the next cycle
               IF(TotHits == 0 ) THEN
                   !  First hit for this ray
                   TotHits = 1
                   NReflSurf=NReflSurf + 1
                   TmpRfSfInd(NReflSurf) = IRay
                   TmpRfRyNH (NReflSurf) = 1
                   TmpHSurfNo (NReflSurf,1) = JSurf
                   TmpHitPt (NReflSurf,1) = HitPt
                   V = HitPt - Surface(ISurf)%Centroid    !vector array from window ctr to hit pt
                   LeastHitDsq = DOT_PRODUCT (V , V)  !dist^2 window ctr to hit pt
                   TmpHSurfDSq (NReflSurf , 1) = LeastHitDsq
                   IF(.NOT.Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%SchedShadowSurfIndex /= 0) THEN
                       TransRSurf = 1.0d0  !If a shadowing surface may have a scheduled transmittance,
                             !   treat it here as completely transparent
                   ELSE
                       TransRSurf= 0.0d0
                   ENDIF
               ELSE
                   V = HitPt - Surface(ISurf)%Centroid
                   HitDsq = DOT_PRODUCT (V , V)
                   IF (HitDsq >= LeastHitDsq) THEN
                       IF (TransRSurf  > 0.0d0) THEN  !forget the new hit if the closer hit is opaque
                           J = TotHits + 1
                           IF ( TotHits > 1) THEN
                               DO I = 2, TotHits
                                   IF ( HitDsq < TmpHSurfDSq(NReflSurf , I) ) THEN
                                       J = I
                                       EXIT
                                   ENDIF
                               ENDDO
                               IF(.NOT.Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%SchedShadowSurfIndex == 0)  THEN
                                   !  The new hit is opaque, so we can drop all the hits further away
                                   TmpHSurfNo (NReflSurf , J) = JSurf
                                   TmpHitPt (NReflSurf , J) = HitPt
                                   TmpHSurfDSq (NReflSurf , J) = HitDsq
                                   TotHits = J
                               ELSE
                                   !  The new hit is scheduled (presumed transparent), so keep the more distant hits
                                   !     Note that all the hists in the list will be transparent except the last,
                                   !       which may be either transparent or opaque
                                   IF (TotHits >= J ) THEN
                                       DO I = TotHits , J , -1
                                          TmpHSurfNo (NReflSurf , I+1) = TmpHSurfNo (NReflSurf , I)
                                          TmpHitPt (NReflSurf , I+1) = TmpHitPt (NReflSurf , I)
                                           TmpHSurfDSq (NReflSurf , I+1) = TmpHSurfDSq (NReflSurf , I)
                                       ENDDO
                                       TmpHSurfNo (NReflSurf , J) = JSurf
                                       TmpHitPt (NReflSurf , J) = HitPt
                                       TmpHSurfDSq (NReflSurf , J) = HitDsq
                                       TotHits = TotHits + 1
                                   ENDIF
                               ENDIF
                           ENDIF
                       ENDIF
                   ELSE
                       !  A new closest hit.  If it is opaque, drop the current hit list,
                       !    otherwise add it at the front
                       LeastHitDsq = HitDsq
                       IF(.NOT.Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%SchedShadowSurfIndex /= 0) THEN
                           TransRSurf = 1.0d0  ! New closest hit is transparent, keep the existing hit list
                           DO I = TotHits , 1 , -1
                               TmpHSurfNo (NReflSurf , I+1) = TmpHSurfNo (NReflSurf , I)
                                TmpHitPt (NReflSurf , I+1) = TmpHitPt (NReflSurf , I)
                               TmpHSurfDSq (NReflSurf , I+1) = TmpHSurfDSq (NReflSurf , I)
                               TotHits = TotHits + 1
                           ENDDO
                       ELSE
                           TransRSurf = 0.0d0  !New closest hit is opaque, drop the existing hit list
                           TotHits = 1
                       ENDIF
                       TmpHSurfNo (NReflSurf,1) = JSurf  ! In either case the new hit is put in position 1
                       TmpHitPt (NReflSurf,1) = HitPt
                       TmpHSurfDSq (NReflSurf , 1) = LeastHitDsq
                   ENDIF
               ENDIF
             END DO  !End of loop over surfaces
              IF (TotHits <= 0 ) THEN
                 !This ray reached the sky or ground unobstructed
                 IF (Geom%sInc(IRay)%z < 0.0d0) THEN
                     !A ground ray
                     NGnd = NGnd + 1
                     TmpGndInd(NGnd) = IRay
                     TmpGndPt( NGnd)%x =Surface(ISurf)%Centroid%x -   &
                        ( Geom%sInc(IRay)%x / Geom%sInc(IRay)%z) * Surface(ISurf)%Centroid%z
                          TmpGndPt( NGnd)%y =Surface(ISurf)%Centroid%y -   &
                             ( Geom%sInc(IRay)%y / Geom%sInc(IRay)%z) * Surface(ISurf)%Centroid%z
                     TmpGndPt( NGnd)%z = 0.0d0
                 ELSE
                    !A sky ray
                    NSky = NSky +1
                    TmpSkyInd(NSky) = IRay
                 ENDIF
             ELSE
                 !Save the number of hits for this ray
                 TmpRfRyNH (NReflSurf) = TotHits
             ENDIF
          END DO  !End of loop over basis rays
          !
          !Store results of indexing the incident basis for this window
          !
          Geom%NSky = NSky
          Geom%NGnd = NGnd
          Geom%NReflSurf = NReflSurf
          ALLOCATE (Geom%SkyIndex(NSky))
          Geom%SkyIndex = TmpSkyInd(1:NSky)
          DEALLOCATE (TmpSkyInd)
          ALLOCATE( Geom%GndIndex(NGnd) , Geom%GndPt(NGnd) )
          Geom%GndIndex = TmpGndInd(1:NGnd)
          Geom%GndPt = TmpGndPt(1:NGnd)
          DEALLOCATE(TmpGndInd,TmpGndPt)
          MaxHits = MAXVAL(TmpRfRyNH)
          ALLOCATE(Geom%RefSurfIndex(NReflSurf),Geom%RefRayNHits(NReflSurf))
          ALLOCATE(Geom%HitSurfNo(NReflSurf,MaxHits),Geom%HitSurfDSq(NReflSurf,MaxHits))
          ALLOCATE(Geom%HitPt(NReflSurf,MaxHits))
          Geom%RefSurfIndex = TmpRfSfInd(1:NReflSurf)
          Geom%RefRayNHits = TmpRfRyNH(1:NReflSurf)
          Geom%HitSurfNo = 0
          Geom%HitSurfDSq = 0.0d0
          Geom%HitPt = vector(0.0d0,0.0d0,0.0d0)
          DO I = 1 , NReflSurf
              TotHits = TmpRfRyNH(I)
              Geom%HitSurfNo(I,1:TotHits) = TmpHSurfNo(I,1:TotHits)
              Geom%HitSurfDSq(I,1:TotHits) = TmpHSurfDSq(I,1:TotHits)
              Geom%HitPt( I , 1:TotHits) = TmpHitPt ( I , 1:TotHits)
          ENDDO
          DEALLOCATE(TmpRfRyNH,TmpRfSfInd,TmpHSurfNo,TmpHSurfDSq,TmpHitPt)
          !
          !In above scheme sky and ground rays are those that intesect no exterior surfaces.
          !  The list of hit points is compiled for later (future?) calculation
          !  of reflections from these surfaces.  The hit list for each ray includes all
          !   surfaces with schedulable transmittance intersected by the ray,
          !   in order of increasing distance, up to the first opaque surface.
          !  Rays that intesect one or more schedulable transmittance but no opaque
          !  surfaces (therefore may reach the sky or ground) are left out of the sky/ground
          !  calcuation.  A correction for these rays could/should be made after the
          !  shading calculation.
          !
              !
              ! Now calculate weights for averaging the transmittance matrix
              !
          !
          !Sky Weights
          !
          ALLOCATE(Geom%SolSkyWt(NSky))
           DO I = 1, NSky
           J = Geom%SkyIndex(I)
           Geom%SolSkyWt(I) = SkyWeight(Geom%sInc(J))
           ENDDO
           WtSum = SUM(Geom%SolSkyWt(1:NSky))
           Geom%SolSkyWt(1:NSky) = Geom%SolSkyWt(1:NSky)/WtSum
           !
           !SkyGround Weights
           !
           ALLOCATE(Geom%SolSkyGndWt(NGnd))
           DO I = 1, NGnd
           Geom%SolSkyGndWt(I) = SkyGndWeight(Geom%GndPt(I))
           ENDDO
           WtSum = SUM(Geom%SolSkyGndWt(1:NGnd))
           Geom%SolSkyGndWt(1:NGnd) = Geom%SolSkyGndWt(1:NGnd)/WtSum
           !  Weights for beam reflected from ground are calculated after shading
           !  interval is determined
              !
              !Transmitted Basis:
              !  Construct back surface intersection maps
              !
              IZone = Surface(ISurf)%Zone
              NBkSurf = Window%NBkSurf
              ALLOCATE(Geom%NSurfInt(NBkSurf))
              Geom%NSurfInt = 0  !Initialize the number of intersections to zero
              ALLOCATE(TmpSurfInt(NBkSurf , Geom%Trn%NBasis))
              ALLOCATE(TmpSjdotN(NBkSurf , Geom%Trn%NBasis))
       !Find the intersections of the basis rays with the back surfaces
        DO IRay = 1 , Geom%Trn%NBasis  !ray loop
           IHit = 0
            TotHits = 0
        !
        !  Insert treatment of intersection & reflection from interior reveals here
        !
           DO KBkSurf  = 1 , NBkSurf    !back surf loop
               BaseSurf = Surface(ISurf)%BaseSurf     !ShadowComb is organized by base surface
              JSurf = ShadowComb(BaseSurf)%BackSurf(KBkSurf)    !these are all proper back surfaces
              CALL PierceSurfaceVector(JSurf, Surface(ISurf)%Centroid, Geom%sTrn(IRay), IHit, HitPt)
               IF (IHit <= 0) CYCLE
               IHit = 0  !A hit, clear the hit flag for the next cycle
               IF(TotHits == 0 ) THEN
                  !  First hit for this ray
                   TotHits=1
                   BSHit%KBkSurf = KBkSurf
                   BSHit%HitSurf = JSurf
                   BSHit%HitPt = HitPt
                   V = HitPt - Surface(ISurf)%Centroid
                   BSHit%HitDSq = DOT_PRODUCT ( V , V )
               ELSE IF (BSHit%HitSurf == Surface(JSurf)%BaseSurf) THEN
                   !  another hit, check whether this is a subsurface of a previously hit base surface
                   !  (which would be listed first in the Surface array)
                   !  if so, replace the previous hit with this one
                   TotHits=TotHits + 1
                   BSHit%KBkSurf = KBkSurf
                   BSHit%HitSurf = JSurf
                   BSHit%HitPt = HitPt
                   V = HitPt - Surface(ISurf)%Centroid
                   BSHit%HitDSq = DOT_PRODUCT ( V , V )
               ELSE
                   TotHits=TotHits + 1
                   ! is the new hit closer than the previous one (i.e., zone not strictly convex)?
                   ! if so, take the closer hit
                   V = HitPt - Surface(ISurf)%Centroid
                   HitDSq = DOT_PRODUCT ( V , V )
                   IF (HitDSq < BSHit%HitDSq) THEN
                       BSHit%KBkSurf = KBkSurf
                       BSHit%HitSurf = JSurf
                       BSHit%HitPt = HitPt
                       BSHit%HitDSq = HitDSq
                   ENDIF
               ENDIF
           ENDDO      !back surf loop
           IF (TotHits == 0) THEN  !this should not happen--means a ray has gotten lost
           !    CALL ShowWarningError ('BSDF--Zone surfaces do not completely enclose zone--transmitted ray lost')
           ELSE
               KBkSurf = BSHit%KBkSurf
               JSurf = BSHit%HitSurf
               Geom%NSurfInt( KBkSurf  ) = Geom%NSurfInt( KBkSurf ) +1
               TmpSurfInt(KBkSurf , Geom%NSurfInt( KBkSurf  )) = IRay
               VecNorm = Surface(JSurf)%OutNormVec
               TmpSjdotN(KBkSurf , Geom%NSurfInt( KBkSurf  )) = Geom%sTrn(IRay) .dot. VecNorm
           ENDIF
        ENDDO        !ray loop
              !  All rays traced, now put away the results in the temporary arrays
              MaxInt = MAXVAL( Geom%NSurfInt )
              ALLOCATE(Geom%SurfInt(Window%NBkSurf , MaxInt))
              ALLOCATE(Geom%SjdotN(Window%NBkSurf , MaxInt))
              Geom%SurfInt = 0
              DO I = 1 , Window%NBkSurf
                  Geom%SurfInt(I,1:Geom%NSurfInt(I)) = TmpSurfInt(I, 1:Geom%NSurfInt(I))
                  Geom%SjdotN(I,1:Geom%NSurfInt(I)) = TmpSjdotN(I, 1:Geom%NSurfInt(I))
              END DO

              DEALLOCATE(TmpSurfInt)
              DEALLOCATE(TmpSjdotN)

  RETURN

END SUBROUTINE SetupComplexWindowStateGeometry

SUBROUTINE CalcWindowStaticProperties(ISurf, IState, Window, Geom, State)

  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Joe Klems
  !       DATE WRITTEN   <date_written>
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! Calculates those optical properties of all the Complex Fenestrations that
  ! do not depend on the beam direction (hence, on hour and time step)

  ! METHODOLOGY EMPLOYED:
  ! <description>

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      ::  ISurf    !Surface number of the complex fenestration
  INTEGER, INTENT(IN)      ::  IState    !State number of the complex fenestration state
  TYPE (BSDFWindowGeomDescr),INTENT(INOUT)    ::  Window  !Window Geometry
  TYPE (BSDFGeomDescr), INTENT(INOUT)  ::  Geom    !State Geometry
  TYPE (BSDFStateDescr), INTENT(INOUT)  ::  State    !State Description


  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    ::  IConst    !Pointer to construction for this fenestration
  INTEGER    ::  I  =0  !general purpose index
  INTEGER    ::  J  =0  !general purpose index
  INTEGER    ::  JJ  =0  !general purpose index--ray
  INTEGER    ::  L  =0  !general purpose index--layer
  INTEGER    ::  M  =0  !general purpose index--ray
  INTEGER    ::  KBkSurf    !back surface index
  INTEGER    ::  JSurf    !surface number (used for back surface)
  INTEGER      ::  BaseSurf    !base surface number (used for finding back surface)
  REAL(r64)    ::  Sum1    !general purpose temporary sum
  REAL(r64)    ::  Sum2    !general purpose temporary sum
  REAL(r64)    ::  Sum3    !general purpose temporary sum
  REAL(r64)        ::  Hold        !temp variable

  IConst = SurfaceWindow(ISurf)%ComplexFen%State(IState)%Konst

  !Calculate the hemispherical-hemispherical transmittance

  Sum1 = 0.0d0
  Sum2 = 0.0d0
  DO  J = 1, Geom%Inc%NBasis     !Incident ray loop
    Sum2 = Sum2 + Geom%Inc%Lamda (J)
    DO  M = 1 , Geom%Trn%NBasis     !Outgoing ray loop
      Sum1 =Sum1 + Geom%Inc%Lamda(J) * Geom%Trn%Lamda(M) * Construct(IConst)%BSDFInput%SolFrtTrans (J, M)
    END DO        !Outgoing ray loop
  END DO        !Incident ray loop
  IF (Sum2 > 0 ) THEN
    State%WinDiffTrans = Sum1/Sum2
  ELSE
    State%WinDiffTrans = 0.0d0
    CALL ShowWarningError ('BSDF--Inc basis has zero projected solid angle')
  ENDIF

  !Calculate the hemispherical-hemispherical transmittance for visible spetrum

  Sum1 = 0.0d0
  Sum2 = 0.0d0
  DO  J = 1 , Geom%Inc%NBasis     !Incident ray loop
    Sum2 = Sum2 + Geom%Inc%Lamda(J)
    DO  M = 1, Geom%Trn%NBasis     !Outgoing ray loop
      Sum1 =Sum1 + Geom%Inc%Lamda(J) * Geom%Trn%Lamda(M) * Construct(IConst)%BSDFInput%VisFrtTrans (J, M)
    END DO        !Outgoing ray loop
  END DO        !Incident ray loop
  IF (Sum2 > 0.0d0 ) THEN
    State%WinDiffVisTrans = Sum1/Sum2
  ELSE
    State%WinDiffVisTrans = 0.0d0
    CALL ShowWarningError ('BSDF--Inc basis has zero projected solid angle')
  ENDIF

  !Set the nominal diffuse transmittance so the surface isn't mistaken as opaque
  Construct(IConst)%TransDiff = SurfaceWindow(ISurf)%ComplexFen%State(IState)%WinDiffTrans
  !Calculate Window Sky Transmittance (transmitted radiation assumed diffuse)
  !and Sky Absorptance (by layer)
  Sum1 = 0.0d0
  Sum2 = 0.0d0
  Sum3 = 0.0d0
  DO  JJ =  1 ,  Geom%NSky
    DO M = 1 , Geom%Trn%NBasis
      J = Geom%SkyIndex( JJ )
      Sum1 = Sum1 + Geom%SolSkyWt(JJ) * Construct(IConst)%BSDFInput%SolFrtTrans(J, M) * Geom%Inc%Lamda(J) * Geom%Trn%Lamda(M)
    END DO
  END DO
  DO  JJ =  1 ,  Geom%NSky
    J = Geom%SkyIndex( JJ )
    Sum2 = Sum2 + Geom%SolSkyWt ( JJ ) * Geom%Inc%Lamda( J )
  END DO

  IF (Sum2 /= 0.0d0) THEN
    State%WinSkyTrans = Sum1/Sum2
  ELSE
    State%WinSkyTrans = 0.0d0
  END IF

  ALLOCATE(State%WinSkyFtAbs(State%NLayers))
  !Also allocate the beam quantities for this state
  DO L = 1 , State%NLayers
    Sum3 = 0.0d0
    DO JJ = 1, Geom%NSky
      J = Geom%SkyIndex( JJ )
      Sum3 = Sum3 + Geom%SolSkyWt (JJ) * Geom%Inc%Lamda(J) * Construct(IConst)%BSDFInput%Layer(L)%FrtAbs(1 ,J)
    END DO

    IF (Sum2 /= 0.0d0) THEN
      State%WinSkyFtAbs(L) = Sum3/Sum2
    ELSE
      State%WinSkyFtAbs(L) = 0.0d0
    END IF

  END DO

  !Calculate Window Sky/Ground Transmittance
  !(applies to ground-reflected sky radiation, transmitted radiation assumed diffuse)
  !This is the same calculation as the sky transmittance, except that the set of incident
  !rays and the ray weights are different
  !Also calculate Window Sky/Ground Absorptance (by layer)
  Sum1 = 0.0d0
  Sum2 = 0.0d0
  Sum3 = 0.0d0

  DO JJ = 1, Geom%NGnd
    DO M = 1 , Geom%Trn%NBasis
      J = Geom%GndIndex(JJ)
      Sum1 = Sum1 + Geom%SolSkyGndWt(JJ) * Construct(IConst)%BSDFInput%SolFrtTrans(J, M) * Geom%Inc%Lamda(J) * Geom%Trn%Lamda(M)
    END DO
  END DO

  DO   JJ =  1 ,  Geom%NGnd
    J = Geom%GndIndex(JJ)
    Sum2 = Sum2 + Geom%SolSkyGndWt ( JJ ) * Geom%Inc%Lamda( J )
  END DO

  IF (Sum2 /= 0.0d0) THEN
    State%WinSkyGndTrans = Sum1/Sum2
  ELSE
    State%WinSkyGndTrans = 0.0d0
  END IF

  ALLOCATE(State%WinSkyGndAbs(State%NLayers))
  DO L = 1 , State%NLayers
    Sum3 = 0.0d0
    DO  JJ = 1, Geom%NGnd
      J = Geom%GndIndex( JJ )
      Sum3 = Sum3 + Geom%SolSkyGndWt(JJ) * Geom%Inc%Lamda(J) * Construct(IConst)%BSDFInput%Layer(L)%FrtAbs(1, J)
    END DO

    IF (Sum2 /= 0.0d0) THEN
      State%WinSkyGndAbs(L) = Sum3/Sum2
    ELSE
      State%WinSkyGndAbs(L) = 0.0d0
    END IF
  END DO

  !Calculate Window Back Hemispherical Reflectance and Layer Back Hemispherical Absorptance
  Sum1 = 0.0d0
  Sum2 = 0.0d0
  Sum3 = 0.0d0
  !Note this again assumes the equivalence Inc basis = transmission basis for back incidence and
  ! Trn basis = incident basis for back incidence
  DO   J = 1, Geom%Trn%NBasis
    DO M = 1, Geom%Inc%NBasis
      Sum1 = Sum1 +  Construct(IConst)%BSDFInput%SolBkRefl(J, M) * Geom%Trn%Lamda(J) * Geom%Inc%Lamda(M)
    END DO
  END DO
  DO  J = 1 , Geom%Trn%NBasis
      Sum2 = Sum2 +  Geom%Trn%Lamda( J )
  END DO

  IF (Sum2 /= 0.0d0) THEN
    State%WinBkHemRefl = Sum1/Sum2
  ELSE
    State%WinBkHemRefl = 0.0d0
  END IF

  Construct(IConst)%ReflectSolDiffBack = State%WinBkHemRefl

  ALLOCATE(State%WinBkHemAbs(State%NLayers))
  DO L = 1, State%NLayers
    DO J = 1, Geom%Trn%NBasis
      Sum3 = Sum3 +  Geom%Trn%Lamda(J) * Construct(IConst)%BSDFInput%Layer(L)%BkAbs(1, J)
    END DO

    IF (Sum2 /= 0.0d0) THEN
      State%WinBkHemAbs(L) = Sum3/Sum2
    ELSE
      State%WinBkHemAbs(L) = 0.0d0
    END IF

    !Put this into the construction for use in non-detailed optical calculations
    Construct(IConst)%AbsDiffBack(L) = State%WinBkHemAbs(L)
  END DO

  !Calculate Window Layer Front Hemispherical Absorptance
  Sum1 = 0.0d0
  Sum2 = 0.0d0
  DO  J = 1, Geom%Inc%NBasis
    Sum2 = Sum2 + Geom%Inc%Lamda(J)
  END DO
  ALLOCATE(State%WinFtHemAbs(State%NLayers))
  DO L = 1, State%NLayers
    Sum1 = 0.0d0
    DO   J = 1 , Geom%Inc%NBasis
      Sum1 = Sum1 +  Geom%Inc%Lamda( J ) * Construct(IConst)%BSDFInput%Layer(L)%FrtAbs(1, J)
    END DO

    IF (Sum2 /= 0.0d0) THEN
      State%WinFtHemAbs(L) = Sum1/Sum2
    ELSE
      State%WinFtHemAbs(L) = 0.0d0
    END IF

    !Put this into the construction for use in non-detailed optical calculations
    Construct(IConst)%AbsDiff(L) = State%WinFtHemAbs(L)
  END DO

  !Calculate Window Back Hemispherical Visible Reflectance
  Sum1 = 0.0d0
  Sum2 = 0.0d0
  !Note this again assumes the equivalence Inc basis = transmission basis for back incidence and
  ! Trn basis = incident basis for back incidence
  DO   J = 1, Geom%Trn%NBasis
    DO M = 1, Geom%Inc%NBasis
      Sum1 = Sum1 +  Construct(IConst)%BSDFInput%VisBkRefl(J, M) * Geom%Trn%Lamda(J) * Geom%Inc%Lamda(M)
    END DO
  END DO
  DO  J = 1 , Geom%Trn%NBasis
      Sum2 = Sum2 +  Geom%Trn%Lamda( J )
  END DO

  IF (Sum2 /= 0.0d0) THEN
    State%WinBkHemVisRefl = Sum1/Sum2
  ELSE
    State%WinBkHemVisRefl = 0.0d0
  END IF

  Construct(IConst)%ReflectVisDiffBack = State%WinBkHemVisRefl

  !     *     *     *     *
  !Note potential problem if one relaxes the assumption that Inc and Trn basis have same structure:
  !  The following calculations are made for the set of ray numbers defined in the Trn basis that
  !   were determined to connect the center of the window to a particular back surface.
  !   Here it is assumed that one can reverse these rays and get an equivalent set in the Trn
  !   basis for back-incidence quantities: back transmittance and back layer absorptance
  !   This assumption may fail if the Inc and Trn bases are allowed to have different structure.
  !   Note also that in this case one would need to rethink the relationship of the basis
  !   definitions to back-incidence quantities:  possibly this would
  !   also require that the basis for back incident quantities be
  !   different from the Trn basis, and similarly the basis for backward outgoing rays
  !   be different from the Inc basis.

  !     *     *     *     *
  !  Note that we are assuming that for back incidence the layer numberings are the same
  !  as for front incidence, i.e., from outside to inside when incidence is from inside
  !     *     *     *     *
  !For back surfaces that are complex fenestrations, calculate the directional-hemispherical back
  !  reflectance and the directional back absorptance by layer for this fenestration receiving
  !  radiation via the back surface
  !  Make this calculation only for cases where the back surface is a Complex Fenestration
  !
  !First allocate the back surface section of the state properties
  IF(.NOT.ALLOCATED(State%BkSurf)) ALLOCATE(State%BkSurf(Window%NBkSurf))
  DO KBkSurf = 1, Window%NBkSurf  !back surface loop
    BaseSurf = Surface(ISurf)%BaseSurf     !ShadowComb is organized by base surface
    JSurf = ShadowComb(BaseSurf)%BackSurf(KBkSurf)
    IF ( SurfaceWindow(JSurf)%WindowModelType /= WindowBSDFModel ) CYCLE

    !  Directional-hemispherical back reflectance
    Sum1 = 0.0d0
    Sum2 = 0.0d0
    DO J = 1, Geom%NSurfInt(KBkSurf)   !Inc Ray loop
      Sum2 = Sum2 + Geom%Trn%Lamda( Geom%SurfInt(KBkSurf , J ) )
      DO M = 1, Geom%Inc%NBasis   !Outgoing Ray loop
        Sum1 = Sum1 + Geom%Trn%Lamda(Geom%SurfInt(KBkSurf, J)) * Geom%Inc%Lamda(M) * &
                           Construct(IConst)%BSDFInput%SolBkRefl(M, Geom%SurfInt(KBkSurf, J))
      END DO    !Outgoing Ray loop
    END DO    !Inc Ray loop
    IF (Sum2 > 0.0d0 ) THEN
      Hold =Sum1/Sum2
      DO  I = 1, 24
        DO J = 1 ,NumOfTimeStepInHour
          State%BkSurf(KBkSurf)%WinDHBkRefl( I , J ) = Hold
        END DO
      END DO
    ELSE
      DO  I = 1 ,24
        DO J = 1 ,NumOfTimeStepInHour
          State%BkSurf(KBkSurf)%WinDHBkRefl(I, J) = 0.0d0
        END DO
      END DO
    ENDIF

    !  Directional layer  back absorption
    DO L = 1, State%NLayers  !layer loop
      Sum1 = 0.0d0
      Sum2 = 0.0d0
      DO  J = 1, Geom%NSurfInt(KBkSurf)  !Inc Ray loop
        Sum2 = Sum2 + Geom%Trn%Lamda(Geom%SurfInt(KBkSurf, J))
        Sum1 = Sum1 + Geom%Trn%Lamda(Geom%SurfInt(KBkSurf , J)) * &
                        Construct(IConst)%BSDFInput%Layer(L)%BkAbs (1, Geom%SurfInt(KBkSurf, J))
      END DO    !Inc Ray loop
      IF (Sum2 > 0.0d0 ) THEN
        Hold =Sum1/Sum2
        DO  I = 1, 24
          DO J = 1, NumOfTimeStepInHour
            State%BkSurf(KBkSurf)%WinDirBkAbs( L , I , J ) = Hold
          END DO
        END DO
      ELSE
        DO  I = 1, 24
          DO J = 1, NumOfTimeStepInHour
            State%BkSurf(KBkSurf)%WinDirBkAbs(L, I, J) = 0.0d0
          END DO
        END DO
      ENDIF

    END DO      !layer loop
  END DO  !back surface loop

  ! ********************************************************************************
  ! Allocation and calculation of integrated values for front of window surface
  ! ********************************************************************************

  ! Sum of front absorptances for each incident direction (integration of absorptances)
  IF(.not.ALLOCATED(State%IntegratedFtAbs)) ALLOCATE(State%IntegratedFtAbs(Geom%Inc%NBasis))
  DO  J = 1, Geom%Inc%NBasis
    Sum1 = 0.0d0
    DO L = 1, State%NLayers  !layer loop
      Sum1 = Sum1 + Construct(IConst)%BSDFInput%Layer(L)%FrtAbs(1, J)
    END DO
    State%IntegratedFtAbs(J) = Sum1
  END DO

  ! Integrating front transmittance
  IF(.not.ALLOCATED(State%IntegratedFtTrans)) ALLOCATE(State%IntegratedFtTrans(Geom%Inc%NBasis))
  DO  J = 1, Geom%Inc%NBasis     ! Incident ray loop
    Sum1 = 0.0d0
    DO  M = 1, Geom%Trn%NBasis     ! Outgoing ray loop
      Sum1 =Sum1 + Geom%Trn%Lamda(J) * Construct(IConst)%BSDFInput%SolFrtTrans(M, J)
    END DO        ! Outgoing ray loop
    State%IntegratedFtTrans(J) = Sum1
  END DO ! Incident ray loop

  IF(.not.ALLOCATED(State%IntegratedFtRefl)) ALLOCATE(State%IntegratedFtRefl(Geom%Inc%NBasis))
  ! Integrating front reflectance
  DO  J = 1 , Geom%Inc%NBasis     ! Incoming ray loop
    State%IntegratedFtRefl(J) = 1 - State%IntegratedFtTrans(J) - State%IntegratedFtAbs(J)
  END DO !Incoming ray loop

  ! ********************************************************************************
  ! Allocation and calculation of integrated values for back of window surface
  ! ********************************************************************************

  ! Sum of back absorptances for each incident direction (integration of absorptances)
  IF(.not.ALLOCATED(State%IntegratedBkAbs)) ALLOCATE(State%IntegratedBkAbs(Geom%Trn%NBasis))
  DO  J = 1, Geom%Trn%NBasis
    Sum1 = 0.0d0
    DO L = 1, State%NLayers  !layer loop
      Sum1 = Sum1 + Construct(IConst)%BSDFInput%Layer(L)%BkAbs(1, J)
    END DO
    State%IntegratedBkAbs(J) = Sum1
  END DO

  ! Integrating back reflectance
  if(.not.ALLOCATED(State%IntegratedBkRefl)) ALLOCATE(State%IntegratedBkRefl(Geom%Trn%NBasis))
  DO  J = 1, Geom%Trn%NBasis     ! Outgoing ray loop
    Sum1 = 0.0d0
    DO  M = 1, Geom%Inc%NBasis     ! Incident ray loop
      Sum1 = Sum1 + Geom%Inc%Lamda(J) * Construct(IConst)%BSDFInput%SolBkRefl(M, J)
    END DO        !Incident ray loop
    State%IntegratedBkRefl(J) = Sum1
  END DO !Outgoing ray loop

  if(.not.ALLOCATED(State%IntegratedBkTrans)) ALLOCATE(State%IntegratedBkTrans(Geom%Trn%NBasis))
  ! Integrating back transmittance
  DO  J = 1 , Geom%Trn%NBasis     ! Outgoing ray loop
    State%IntegratedBkTrans(J) = 1 - State%IntegratedBkRefl(J) - State%IntegratedBkAbs(J)
  END DO !Outgoing ray loop

  RETURN

END SUBROUTINE CalcWindowStaticProperties


FUNCTION SkyWeight (DirVec) RESULT(Wt)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Search a one-dimensional array for a given value, returning the index of the element equal to the value, if
          !   found, or zero

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE (vector), INTENT (IN)       ::  DirVec            ! Direction of the element to be weighted
  REAL(r64)         :: Wt  ! Weight

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS
          !na

          !Flow:

          Wt = 1.0d0

          ! To do:  figure out how to weight sky elements to reproduce the current E+ assumptions
          !  Possibly one will need to calculated average DH transmittance for isotropic sky and
          !  horizon separately and then later average according to sky conditions.  Then a completely
          !  different scheme for daylight.  For now: rays that reach sky equally weighted in calculating
          !  transmittance, rays passing through surfaces with scheduled transmittance are neglected.

      RETURN

END FUNCTION SkyWeight

FUNCTION SkyGndWeight (PosVec) RESULT(Wt)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Search a one-dimensional array for a given value, returning the index of the element equal to the value, if
          !   found, or zero

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE (vector), INTENT (IN)       ::  PosVec            ! x,y,z(=0) of ground intersection pt
  REAL(r64)         :: Wt  ! Weight

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS
          !na

          !Flow:

          Wt = 1.0d0

          !  At present, equally weights all ground rays for calculation of the complex window transmittance for
          !  sky radiation reflected from ground.  This does not take into account shading of the ground.
          !  The correct procedure would be to generate a set of rays to the sky and see which do not intersect
          !  surfaces, as is done in the reflection manager.  However, this would increase computational load.
          !  Given that equal weighting, by averaging the transmittance only over rays that come from the ground,
          !  already produces a more accurate ground transmittance than the existing method, it is at least questionable
          !  whether the more detailed procedure would produce enough improvement in accuracy to make up for
          !  the additional calculation time.  Therefore a more detailed treatment is deferred until there is some
          !  experience with the new method to determine whether further detail is warranted.


      RETURN

END FUNCTION SkyGndWeight

FUNCTION DaylghtAltAndAzimuth (UnitVect) RESULT(DayPos)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Transform unit vector (given in world coordinates) into altitude and azimuth.  Azimuth is measured from positive x-axe.
          ! Altitude range is from -pi/2 to pi/2. Vector laying in horizontal plane will have altitude equal to zero and vector
          ! pointing upward will have altitude equal to pi/2. Range for azimuth is calculated from -pi to +pi.

          ! METHODOLOGY EMPLOYED:
          ! <n/a>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
use DataBSDFWindow
use vectors
use DataGlobals

implicit none

type(vector), intent(in)         ::  UnitVect ! vector which needs to be converted
type(BSDFDaylghtPosition)        ::  DayPos  ! altitude and azimuth in world coordinates

if (UnitVect%x /= 0.0d0) then
  if (UnitVect%x >= 0.0d0) then
    DayPos%Azimuth = atan(UnitVect%y/UnitVect%x)
  else
    if (UnitVect%y >= 0.0d0) then
      DayPos%Azimuth = pi + atan(UnitVect%y/UnitVect%x)
    else
      DayPos%Azimuth = -pi + atan(UnitVect%y/UnitVect%x)
    end if
  end if
else
  if (UnitVect%y >= 0.0d0) then
    DayPos%Azimuth = PiOvr2
  else
    DayPos%Azimuth = -PiOvr2
  end if
end if

DayPos%Altitude = asin(UnitVect%z)

return

END FUNCTION DaylghtAltAndAzimuth

FUNCTION WorldVectFromW6 (Theta, Phi, RadType, Gamma, Alpha) RESULT(UnitVect)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Transform angular coordinates in the WINDOW6 coordinate system for
          ! a given surface into a unit vector in the world coordinate system,
          ! pointing to the radiation source (for incident radiation) or in
          ! the direction of propagation (for outgoing radiation)

          ! METHODOLOGY EMPLOYED:
          ! <n/a>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)      ::  Theta  !Polar angle in W6 Coords
  REAL(r64), INTENT(IN)      ::  Phi  !Azimuthal angle in W6 Coords
  INTEGER, INTENT(IN)      ::  RadType  !Type of radiation: Front_Incident, etc.
  REAL(r64), INTENT(IN)      ::  Gamma  !Surface tilt angle, radians, world coordinate system
  REAL(r64), INTENT(IN)      ::  Alpha  !Surface azimuth, radians, world coordinate system
  TYPE(Vector)        ::  UnitVect  !unit vector direction in world CS

  ! Error tolerance is used to make small numbers equal to zero.  Due to precision of pi constant used in E+, performing
  ! trigonometric operations on those constant will not cause absolutely accurate results
  REAL(r64), PARAMETER  :: ErrorTolerance = 1.d-10

         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  UnitVect = vector(0.0d0, 0.0d0, 0.0d0)
  SELECT CASE (RadType)
    CASE (Front_Incident)   !W6 vector will point in direction of propagation, must reverse to get world vector
                                             !  after the W6 vector has been rotated into the world CS
      UnitVect%x = SIN(Theta)*SIN(Phi)*COS(Gamma)*SIN(Alpha)-SIN(Theta)*COS(Phi)*COS(Alpha)&
        & +COS(Theta)*SIN(Gamma)*SIN(Alpha)
      UnitVect%y = SIN(Theta)*COS(Phi)*SIN(Alpha)+SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)&
        & +COS(Theta)*SIN(Gamma)*COS(Alpha)
      UnitVect%z = -(SIN(Theta)*SIN(Phi)*SIN(Gamma)-COS(Theta)*COS(Gamma) )
    CASE (Front_Transmitted)
      UnitVect%x = SIN(Theta)*COS(Phi)*COS(Alpha)-SIN(Theta)*SIN(Phi)*COS(Gamma)*SIN(Alpha)&
        & -COS(Theta)*SIN(Gamma)*SIN(Alpha)
      UnitVect%y = -(SIN(Theta)*COS(Phi)*SIN(Alpha)+SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)&
        & +COS(Theta)*SIN(Gamma)*COS(Alpha))
      UnitVect%z = SIN(Theta)*SIN(Phi)*SIN(Gamma)-COS(Theta)*COS(Gamma)
    CASE (Front_Reflected)
      UnitVect%x = SIN(Theta)*COS(Phi)*COS(Alpha)-SIN(Theta)*SIN(Phi)*COS(Gamma)*SIN(Alpha)&
        & +COS(Theta)*SIN(Gamma)*SIN(Alpha)
      UnitVect%y = COS(Theta)*SIN(Gamma)*COS(Alpha)-SIN(Theta)*COS(Phi)*SIN(Alpha)&
        & -SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)
      UnitVect%z = SIN(Theta)*SIN(Phi)*SIN(Gamma)+COS(Theta)*COS(Gamma)
    CASE (Back_Incident)
      UnitVect%x = SIN(Theta)*SIN(Phi)*COS(Gamma)*SIN(Alpha) - SIN(Theta)*COS(Phi)*COS(Alpha)&
        & -COS(Theta)*SIN(Gamma)*SIN(Alpha)
      UnitVect%y = SIN(Theta)*COS(Phi)*SIN(Alpha) + SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)&
        & -COS(Theta)*SIN(Gamma)*COS(Alpha)
      UnitVect%z = -COS(Theta)*COS(Gamma)-SIN(Theta)*SIN(Phi)*SIN(Gamma)
    CASE (Back_Transmitted)    !This is same as front reflected
      UnitVect%x = SIN(Theta)*COS(Phi)*COS(Alpha)-SIN(Theta)*SIN(Phi)*COS(Gamma)*SIN(Alpha)&
        & +COS(Theta)*SIN(Gamma)*SIN(Alpha)
      UnitVect%y = COS(Theta)*SIN(Gamma)*COS(Alpha)-SIN(Theta)*COS(Phi)*SIN(Alpha)&
        & -SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)
      UnitVect%z = SIN(Theta)*SIN(Phi)*SIN(Gamma)+COS(Theta)*COS(Gamma)
    CASE (Back_Reflected)    !This is same as front transmitted
      UnitVect%x = SIN(Theta)*COS(Phi)*COS(Alpha)-SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)&
        & -COS(Theta)*SIN(Gamma)*SIN(Alpha)
      UnitVect%y = -(SIN(Theta)*COS(Phi)*SIN(Alpha)+SIN(Theta)*SIN(Phi)*COS(Gamma)*COS(Alpha)&
        & +COS(Theta)*SIN(Gamma)*COS(Alpha))
      UnitVect%z = SIN(Theta)*SIN(Phi)*SIN(Gamma)-COS(Theta)*COS(Gamma)
    END SELECT

    ! Remove small numbers from evaluation (due to limited decimal points for pi)
    IF (abs(UnitVect%x) <= ErrorTolerance) UnitVect%x = 0.0d0
    IF (abs(UnitVect%y) <= ErrorTolerance) UnitVect%y = 0.0d0
    IF (abs(UnitVect%z) <= ErrorTolerance) UnitVect%z = 0.0d0

   RETURN

END FUNCTION WorldVectFromW6

FUNCTION FindInBasis ( RayToFind, RadType,ISurf,IState,Basis,Theta,Phi) RESULT ( RayIndex )


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !

          ! METHODOLOGY EMPLOYED:
          ! <n/a>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  TYPE(vector), INTENT(IN)      ::  RayToFind    !Ray vector direction in world CS
  INTEGER, INTENT(IN)      ::  RadType  !Type of radiation: Front_Incident, etc.
  ! INTEGER, INTENT(IN)      ::  IWind  !window index in window list
  INTEGER, INTENT(IN)      ::  ISurf  !Window Surface number
  INTEGER, INTENT(IN)      ::  IState  !Complex Fenestration state number
  TYPE (BasisStruct), INTENT(IN)    ::  Basis  !Complex Fenestration basis root
  REAL(r64), INTENT(OUT)        ::  Theta  !Theta value for ray
  REAL(r64), INTENT(OUT)        ::  Phi  !Phi value for ray
  INTEGER        ::  RayIndex  !Index of ray in basis, zero if ray not in hemisphere



         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          INTEGER        ::  ITheta  !Table index of Theta
          INTEGER        ::  IPhi  !Table index of Phi, given ITheta
          INTEGER        ::  IThDn  !Theta lower table index
          INTEGER        ::  IThUp  !Theta upper table index
          INTEGER        ::  IPhDn  !Phi lower table index
          INTEGER        ::  IPhUp  !Phi upper table index
          REAL(r64)        ::  Gamma  !Gamma (tilt) angle of window
          REAL(r64)        ::  Alpha  !Alpha (azimuth) angle of window
          REAL(r64)      :: DotProd


          Theta = 0.0d0
          Phi = 0.0d0

          ! Check if surface and vector are pointing in different directions
          DotProd = RayToFind .dot. Surface(ISurf)%NewellSurfaceNormalVector
          IF (DotProd <= 0.0d0) THEN
            RayIndex = 0
            RETURN
          END IF

          !get window tilt and azimuth
          Gamma = DegToRadians*Surface(ISurf)%Tilt
          Alpha = DegToRadians*Surface(ISurf)%Azimuth
          !get the corresponding local Theta, Phi for ray
          CALL W6CoordsFromWorldVect (RayToFind, RadType, Gamma, Alpha,Theta, Phi)

          IF (Theta >= 0.5d0*Pi) THEN     !Ray was in not in correct hemisphere
              RayIndex = 0
              RETURN
          ENDIF
          IF (Basis%BasisSymmetryType == BasisSymmetry_None ) THEN
              !Search the basis thetas
              IF(Theta <= 0.0d0 ) THEN
              !Special case, Theta = 0.; this is always the first basis element
              RayIndex = 1
              RETURN
              ENDIF
              !So here Theta > 0
              !Note the table searches always go to the limit point, which is not itself a basis element
              IThUp = SearchAscTable(Theta,Basis%NThetas + 1,Basis%Thetas)
              IThDn = IThUP -1
              !Determine which of the theta basis points is closer to the Theta value
              IF (Theta <= Basis%Grid( Basis%BasisIndex(IThDn,1) )%UpprTheta ) THEN
                  !Note this will take care of both the special cases IThUp=2 and IThUp=NThetas +1
                  ITheta = IThDn
              ELSE
                  ITheta = IThUp
              ENDIF
              !Now determine the Phi index
              IF (Basis%NPhis(ITheta) == 1 ) THEN
                  !Note that for W6 basis this can only happen for the first basis element
                  !If later bases are introduced this logic may have to be redesigned
                  RayIndex = Basis%BasisIndex(ITheta,1)
                  RETURN
              ENDIF
                  IPhUp = SearchAscTable( Phi, Basis%NPhis(ITheta)+1 , Basis%Phis (: , ITheta ) )
                  IPhDn = IPhUp - 1
                  IF (Phi <= Basis%Grid( Basis%BasisIndex(ITheta, IPhDn) )%UpprPhi  ) THEN
                      IPhi = IPhDn
                  ELSE
                      IF (IPhUp == Basis%NPhis(ITheta) + 1 ) THEN
                          ! Phi is above upper limit for highest Phi basis element, meaning it is closer to 2Pi,
                          ! i.e., the first element
                          IPhi =1
                      ELSE
                          IPhi = IPhUp
                      ENDIF
                  ENDIF
                  RayIndex = Basis%BasisIndex( ITheta , IPhi )
                  RETURN
          ELSE IF( Basis%BasisSymmetryType == BasisSymmetry_Axisymmetric ) THEN
               !Search the basis thetas
              IF(Theta <= 0.0d0 ) THEN
                  !Special case, Theta = 0.; this is always the first basis element
                  RayIndex = 1
                  RETURN
              ENDIF
              !So here Theta > 0
              !Note the table searches always go to the limit point, which is not itself a basis element
              IThUp = SearchAscTable(Theta,Basis%NThetas + 1,Basis%Thetas)
              IThDn = IThUP -1
              !Determine which of the theta basis points is closer to the Theta value
              IF (Theta <= Basis%Grid( Basis%BasisIndex(IThDn,1) )%UpprTheta ) THEN
                  !Note this will take care of both the special cases IThUp=2 and IThUp=NThetas +1
                  ITheta = IThDn
              ELSE
                  ITheta = IThUp
              ENDIF
              RayIndex = Basis%BasisIndex( ITheta , 1 )
              RETURN
          ENDIF
          !No other type is implemented
           RayIndex = 0



   RETURN

END FUNCTION FindInBasis


SUBROUTINE W6CoordsFromWorldVect (RayVect, RadType, Gamma, Alpha,Theta, Phi)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Invert the transformation from W6 to world coordinates to
          ! calculate the theta, phi corresponding to a given ray direction
          ! in the world coordinate system, for a window with a
          ! given rotation and tilt (Gamma and Alpha)
          !  (needed for locating the sun direction in the local coordinate system)

          ! METHODOLOGY EMPLOYED:
          ! <n/a>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  TYPE(vector), INTENT(IN)      ::  RayVect  !Ray vector direction in world CS
  INTEGER, INTENT(IN)      ::  RadType  !Type of radiation: Front_Incident, etc.
  REAL(r64), INTENT(IN)      ::  Gamma  !Surface tilt angle, world coordinate system
  REAL(r64), INTENT(IN)      ::  Alpha  !Surface azimuth, world coordinate system
  REAL(r64), INTENT(OUT)      ::  Theta  !Polar angle in W6 Coords
  REAL(r64), INTENT(OUT)      ::  Phi  !Azimuthal angle in W6 Coords


         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (vector)         ::  W6x  !W6 x coordinate unit vector
  TYPE (vector)         ::  W6y  !W6 y coordinate unit vector
  TYPE (vector)         ::  W6z  !W6 z coordinate unit vector
  REAL(r64)                ::  Cost  !Temp for cos theta
  REAL(r64)                ::  Sint  !Temp for sin theta
  REAL(r64)                ::  Psi  !Temp for phi before rotation adjustment
  REAL(r64)                     ::  RdotX   !Temp variable for manipulating .dot. produt
  REAL(r64)                     ::  RdotY   !Temp variable for manipulating .dot. produt
  REAL(r64)                     ::  RdotZ   !Temp variable for manipulating .dot. produt

  ! define the local W6 coordinate vectors
  W6x%x = COS(Alpha)
  W6x%y = -SIN(Alpha)
  W6x%z = 0.0d0
  W6y%x = -COS(Gamma)*SIN(Alpha)
  W6y%y =  -COS(Gamma)*COS(Alpha)
  W6y%z = SIN(Gamma)
  W6z%x = -SIN(Gamma)*SIN(Alpha)
  W6z%y = -SIN(Gamma)*COS(Alpha)
  W6z%z = -COS(Gamma)
  SELECT CASE (RadType)
    CASE (Front_Incident)
      RdotZ =W6z.dot.RayVect
      Cost = -RdotZ
      Sint = SQRT(1.0d0 - Cost**2)
      Theta = ACOS(Cost)
      RdotY = W6y.dot.RayVect
      RdotX = W6x.dot.RayVect
      Psi = ATAN2(-RdotY/Sint , -RdotX/Sint)
      IF (Psi < 0.0d0) THEN
          Phi = 2.0d0*Pi + Psi
      ELSE
          Phi = Psi
      ENDIF
    CASE (Front_Transmitted)
      Cost = W6z.dot.RayVect
      Sint = SQRT(1.0d0 - Cost**2)
      Theta = ACOS(Cost)
      RdotY = W6y.dot.RayVect
      RdotX = W6x.dot.RayVect
      Psi = ATAN2(RdotY/Sint , RdotX/Sint)
      IF (Psi < 0.0d0) THEN
          Phi = 2.0d0*Pi + Psi
      ELSE
          Phi = Psi
      ENDIF
    CASE (Front_Reflected)
      RdotZ =W6z.dot.RayVect
      Cost = -RdotZ
      Sint = SQRT(1.0d0 - Cost**2)
      Theta = ACOS(Cost)
      RdotY = W6y.dot.RayVect
      RdotX = W6x.dot.RayVect
      Psi = ATAN2(RdotY/Sint , RdotX/Sint)
      IF (Psi < 0.0d0) THEN
          Phi = 2.0d0*Pi + Psi
      ELSE
          Phi = Psi
      ENDIF
    CASE (Back_Incident)
      Cost = W6z.dot.RayVect
      Sint = SQRT(1.0d0 - Cost**2)
      Theta = ACOS(Cost)
      RdotY = W6y.dot.RayVect
      RdotX = W6x.dot.RayVect
      Psi = ATAN2(-RdotY/Sint , -RdotX/Sint)
      IF (Psi < 0.0d0) THEN
          Phi = 2*Pi + Psi
      ELSE
          Phi = Psi
      ENDIF
    CASE (Back_Transmitted)    !This is same as front reflected
      RdotZ = W6z.dot.RayVect
      Cost = -RdotZ
      Sint = SQRT(1.0d0 - Cost**2)
      Theta = ACOS(Cost)
      RdotY = W6y.dot.RayVect
      RdotX = W6x.dot.RayVect
      Psi = ATAN2(RdotY/Sint , RdotX/Sint)
      IF (Psi < 0.0d0) THEN
          Phi = 2.0d0*Pi + Psi
      ELSE
          Phi = Psi
      ENDIF
    CASE (Back_Reflected)    !This is same as front transmitted
      Cost = W6z.dot.RayVect
      Sint = SQRT(1.0d0 - Cost**2)
      Theta = ACOS(Cost)
      RdotY = W6y.dot.RayVect
      RdotX = W6x.dot.RayVect
      Psi = ATAN2(RdotY/Sint , RdotX/Sint)
      IF (Psi < 0.0d0) THEN
          Phi = 2.0d0*Pi + Psi
      ELSE
          Phi = Psi
      ENDIF
   END SELECT
   IF(ABS(Cost) < rTinyValue)Cost = 0.0d0
   IF(Cost < 0.0d0 ) Theta = Pi - Theta  !This signals ray out of hemisphere
   RETURN

END SUBROUTINE W6CoordsFromWorldVect


SUBROUTINE CalcComplexWindowThermal(SurfNum,ConstrNum,HextConvCoeff,SurfInsideTemp,SurfOutsideTemp,SurfOutsideEmiss,CalcCondition)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   October 2009
          !       MODIFIED       Simon Vidanovic
          !       RE-ENGINEERED  September 2011

          ! PURPOSE OF THIS SUBROUTINE:
          ! wrapper between E+ and TARCOG

          ! METHODOLOGY EMPLOYED:
          ! draft out an attempt for proof-of-concept, to reuse native TARCOG implementation
          ! based off of 1-26-2009 version of WinCOG/TARCOG solution from Carli, Inc.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  !USE WindowTARCOGManager, ONLY: tarcog
  USE DataBSDFWindow
  USE DataZoneEquipment, ONLY : ZoneEquipConfig
  USE DataLoopNode,      ONLY : Node
  USE Psychrometrics,    ONLY : PsyCpAirFnWTdb,PsyTdpFnWPb
  USE General,           ONLY : InterpSlatAng , InterpSw ! Function for slat angle interpolation
  USE InputProcessor,    ONLY : SameString
  USE DataHeatBalSurface, ONLY : HcExtSurf
  USE DataGlobals, ONLY: StefanBoltzmann
  USE TARCOGGassesParams, ONLY : maxgas
  USE TARCOGParams, ONLY : maxlay, maxlay1
  USE DataHeatBalance, ONLY : GasCoeffsAir, SupportPillar
  USE TARCOGMain, ONLY : TARCOG90

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: SurfNum         ! Surface number
  INTEGER, INTENT(IN)       :: CalcCondition   ! Calucation condition (summer, winter or no condition)
  INTEGER, INTENT(INOUT)    :: ConstrNum       ! Construction number
  REAL(r64), INTENT(IN)     :: HextConvCoeff   ! Outside air film conductance coefficient
  REAL(r64), INTENT(INOUT)  :: SurfInsideTemp  ! Inside window surface temperature
  REAL(r64), INTENT(INOUT)  :: SurfOutsideTemp ! Outside surface temperature (C)
                                                        ! (temperature of innermost face) [C]
  REAL(r64), INTENT(INOUT)  :: SurfOutsideEmiss
  !INTEGER, INTENT(IN)        :: CurrentThermalModelNumber

          ! SUBROUTINE PARAMETER DEFINITIONS:
  !INTEGER,  PARAMETER :: maxlay = 100 ! maximum number of layers (including laminates)
  !INTEGER,  PARAMETER :: maxgas = 10  ! maximum number of individual gasses
  !INTEGER, PARAMETER :: maxlay1  = maxlay+1     ! maximum number of 'gaps', including in and out (maxlay+1)
  !REAL(r64), PARAMETER :: StefanBoltzmannConst = 5.6697d-8   ! Stefan-Boltzmann constant in W/(m2*K4)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ! TARCOG Inputs:
  INTEGER   ::  nlayer = 0 !    Number of glazing layers
  INTEGER   ::  iwd    = 0 !    Wind direction:  0 - windward, 1 - leeward
  REAL(r64) ::  tout   = 0.0D0 !    Outdoor temperature [K]
  REAL(r64) ::  tind   = 0.0D0 !    Indoor temperature [K]
  REAL(r64) ::  trmin  = 0.0D0 !    Indoor mean radiant temperature [K]
  REAL(r64) ::  wso    = 0.0D0 !    Outdoor wind speed [m/s]
  REAL(r64) ::  wsi    = 0.0D0 !    Inside forced air speed [m/s]
  REAL(r64) ::  dir    = 0.0D0  !    Direct solar radiation [W/m^2]
  INTEGER   ::  isky   = 0  !    Flag for sky temperature (Tsky) and sky emittance (esky)
                            !                      0 - both tsky and esky are specified
                            !                      1 - tsky specified, esky = 1
                            !                      2 - Swinbank model for effective sky emittance
  REAL(r64) ::  tsky   = 0.0D0 !    Night sky temperature [K]
  REAL(r64) ::  esky   = 0.0D0 !    Effective night sky emittance
  REAL(r64) ::  fclr   = 0.0D0 !    Fraction of sky that is clear
  REAL(r64) ::  VacuumPressure ! maximal pressure for gas to be considered as vacuum [Pa]
  REAL(r64) ::  VacuumMaxGapThickness  ! maximal gap thickness for which vacuum calculation will work without issuing
                                       ! warning message
  REAL(r64) , DIMENSION(maxlay) ::  gap  = 0.0D0 ! Vector of gap widths [m] {maxlay}
  REAL(r64) , DIMENSION(maxlay) ::  thick    = 0.0D0 ! Vector of glass thicknesses [m] {maxlay}
  REAL(r64) , DIMENSION(maxlay) ::  scon     = 0.0D0 ! Vector of conductivities of each glazing layer  [W/m.K] {maxlay}
  REAL(r64) , DIMENSION(maxlay * 2 ) ::  tir = 0.0D0 ! Vector of IR transmittances of each layer {2*maxlay - 2 surfaces per layer}
  REAL(r64) , DIMENSION(maxlay * 2 ) ::  emis = 0.0D0 ! Vector of IR emittances of each surface {2*maxlay - 2 surfaces per layer}
  INTEGER, DIMENSION(maxlay) :: SupportPlr = 0      ! Shows whether or not gap have support pillar
                                                    ! 0 - does not have support pillar
                                                    ! 1 - have support pillar
  REAL(r64), DIMENSION(maxlay) :: PillarSpacing = 0.0d0        ! Pillar spacing for each gap (used in case there is support pillar)
  REAL(r64), DIMENSION(maxlay) :: PillarRadius = 0.0d0         ! Pillar radius for each gap (used in case there is support pillar)

  REAL(r64)  ::   totsol  = 0.0D0 !    Total solar transmittance of the IGU
  REAL(r64)  ::   tilt    = 0.0D0 !    Window tilt [degrees]
  REAL(r64) , DIMENSION(maxlay) ::   asol = 0.0D0  !    Vector of Absorbed solar energy fractions for each layer {maxlay}
  REAL(r64)  ::    height = 0.0D0  ! IGU cavity height [m]
  REAL(r64)  ::    heightt = 0.0D0 ! Total window height [m]
  REAL(r64)  ::    width   = 0.0D0 ! Window width [m]
  REAL(r64) , DIMENSION(maxlay+1) ::   presure  = 0.0D0 !   Vector of gas pressures in gaps [N/m^2] {maxlay+1}

  !Deflection
  !Tarcog requires deflection as input parameters.  Deflection is NOT used in EnergyPlus simulations
  INTEGER :: CalcDeflection     ! Deflection calculation flag:
                                !    0 - no deflection calculations
                                !    1 - perform deflection calculation (input is Pressure/Temp)
                                !    2 - perform deflection calculation (input is measured deflection)
  REAL(r64) :: Pa      ! Atmospheric (outside/inside) pressure (used onlu if CalcDeflection = 1)
  REAL(r64) :: Pini    ! Initial presssure at time of fabrication (used only if CalcDeflection = 1)
  REAL(r64) :: Tini   ! Initial temperature at time of fabrication (used only if CalcDeflection = 1)
  REAL(r64), DIMENSION(maxlay-1) :: GapDefMax    ! Vector of gap widths in deflected state.  It will be used as input
                                                ! if CalcDeflection = 2. In case CalcDeflection = 1 it will return recalculated
                                                ! gap widths. [m]
  REAL(r64), DIMENSION(maxlay) :: YoungsMod      ! Vector of Young's modulus. [m]
  REAL(r64), DIMENSION(maxlay) :: PoissonsRat    ! Vector of Poisson's Ratios. [m]
  REAL(r64), DIMENSION(maxlay) :: LayerDef       ! Vector of layers deflection. [m]

  INTEGER, DIMENSION(maxlay+1, maxgas)   ::  iprop = 1 !  Matrix of gas codes - see above {maxgap x maxgas}
  REAL(r64), DIMENSION(maxlay+1, maxgas) ::  frct  = 0.0D0  !  Matrix of mass percentages in gap mixtures  {maxgap x maxgas}
  REAL(r64), DIMENSION(maxgas, 3)        ::  gcon  = 0.0D0  !  Matrix of constants for gas conductivity calc
                                                            !     (A, B, C for max of 10 gasses) {maxgas x 3}
  REAL(r64), DIMENSION(maxgas, 3)        ::  gvis  = 0.0D0  !  Matrix of constants for gas dynamic viscosity calc
                                                            !     (A, B, C for max of 10 gasses) {maxgas x 3}
  REAL(r64), DIMENSION(maxgas, 3)        ::  gcp   = 0.0D0  !  Matrix of constants for gas specific heat calc at constant pressure
                                                            !     (A, B, C for max of 10 gasses) {maxgas x 3}
  REAL(r64), DIMENSION(maxgas)           ::  wght   = 0.0D0  !  Vector of Molecular weights for gasses {maxgas}
  REAL(r64), DIMENSION(maxgas)           ::  gama   = 0.0D0  !  Vector of spefic heat ration for low pressure calc {maxgas}
  LOGICAL                                 ::  feedData = .FALSE. !flag to notify if data needs to be feed into gas arrays
  INTEGER, DIMENSION(maxlay + 1)         ::  nmix   = 0      !  Vector of number of gasses in gas mixture of each gap {maxlay+1}
  REAL(r64)  ::   hin  = 0.0D0  !      Indoor combined film coefficient (if non-zero) [W/m^2.K]
  REAL(r64)  ::   hout = 0.0D0  !      Outdoor combined film coefficient (if non-zero) [W/m^2.K]
  INTEGER, DIMENSION(2) ::   ibc = 0  !  Vector of boundary condition flags (ibc(1) - outdoor, ibc(2) - indoor)
                                      !             0 - h to be calculated;
                                      !             1 - combined film coefficient (h) prescribed;
                                      !             2 - convective film coefficient (hc) prescribed.
                                      !           Also used in old algorithms for calculating h, accessible through
                                      !           negative values for flags:
                                      !             -1  - old SPC142 correlation
                                      !             -2  - Klems-Yazdanian correlation (applicable to outdoor only)
                                      !             -3  - Kimura correlation (applicable to outdoor only)
  REAL(r64), DIMENSION(maxlay) :: Atop = 0.0D0  !      Vector with areas of top openings - between SD layers and top of
                                                !               glazing cavity, for each layer [m^2] {maxlay} *
  REAL(r64), DIMENSION(maxlay) :: Abot = 0.0D0  !      Vector with areas of bottom openings - between SD layers
                                                !               and bottom of glazing cavity [m^2] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: Al   = 0.0D0  !      Vector with areas of left-hand side openings - between SD layers
                                                !               and left end of glazing cavity [m^2] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: Ar   = 0.0D0  !      Vector of areas of right-hand side openings - between SD layers
                                                !               and right end of glazing cavity [m^2] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: Ah   = 0.0D0  !      Vector of total areas of holes for each SD [m^2] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: SlatThick = 0.0D0 !  Thickness of the slat material [m] {maxlay} **
  REAL(r64), DIMENSION(maxlay) :: SlatWidth = 0.0D0 !  Slat width [m] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: SlatAngle = 0.0D0 !  Slat tilt angle [deg] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: SlatCond  = 0.0D0 !  Conductivity of the slat material [W/m.K] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: SlatSpacing = 0.0D0 ! Distance between slats [m] {maxlay}
  REAL(r64), DIMENSION(maxlay) :: SlatCurve = 0.0D0   ! Curvature radius of the slat [m] {maxlay}
  REAL(r64), DIMENSION(maxlay+1) :: vvent   = 0.0D0   !   Vector of velocities for forced ventilation, for each gap, and for
                                                      !               outdoor and indoor environment [m/s] {maxlay+1} ***
  REAL(r64), DIMENSION(maxlay+1) :: tvent   = 0.0D0   !  Vector of temperatures of ventilation gas for forced ventilation, for each
                                                      !  gap, and for outdoor and indoor environment [K] {maxlay+1}
  INTEGER, DIMENSION(maxlay) ::  LayerType = 0 !  Glazing layer type flag {maxlay}:
                                              !                 0 - Specular layer,
                                              !                 1 - Venetian blind (SD)
                                              !                 2 - Woven shade (SD) (not implemented)
                                              !                 3 - Diffuse shade (not implemented)
  INTEGER, DIMENSION(maxlay) ::   nslice   = 0  !   Vector of numbers of slices in a laminated glazing layers
                                                !   (0 - monolithic layer) {maxlay}
  REAL(r64), DIMENSION(maxlay) :: LaminateA = 0.0D0 !  Left-hand side array for creating slice equations {maxlay}
  REAL(r64), DIMENSION(maxlay) :: LaminateB = 0.0D0 !  Right-hand side array for creating slice equations {maxlay}
  REAL(r64), DIMENSION(maxlay) :: sumsol = 0.0D0    !  Array of absorbed solar energy fractions for each laminated
                                                  !               glazing layer [W/m^2] {maxlay}
  INTEGER ::   standard = 1 !   Calculation standard switch:
                            !                 1 - ISO 15099,
                            !                 2 - EN673 / ISO 10292 Declared,
                            !                 3 - EN673 / ISO 10292 Design.
  INTEGER ::    ThermalMod  = 0 ! Thermal model:
                                !                 0 - ISO15099
                                !                 1 - Scaled Cavity Width (SCW)
                                !                 2 - Convective Scalar Model (CSM)
  INTEGER ::   Debug_mode = 0 ! Switch for debug output files:
                              !                 0 - don’t create debug output files
                              !                 1 - append results to existing debug output file (where applicable)
                              !                 2 - store results in a new debug output file
  CHARACTER(len=256) ::   Debug_dir = ' '  ! Target directory for debug files (pointer to a character array)
  CHARACTER(len=256) ::   Debug_file = 'Test'  ! Template file name used to create debug output files
  INTEGER(4) ::    Window_ID = -1 !  ID of the window (long integer value, passed by W6)
  INTEGER(4) ::    IGU_ID    = -1 ! ID of the IGU (long integer value, passed by W6)
  REAL(r64) ::   SDScalar  = 0.0D0 !  SD convection factor (value between 0 and 1)
                                    !                 0.0 - No SD layer
                                    !                 1.0 - Closed SD
  !
  !               Notes:   * vvent, tvent, Atop, Abot, Al, Ar and Ah are considered for SD layers only.
  !                       ** SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve
  !                          are used for Venetian blind layers only.
  !                      *** For vvent & tvent: vvent(1) - exterior, vvent(nlayer+1) - interior.
  !                     **** Forced ventilation calculation is not active at this time.
  !
  ! TARCOG Output:

  REAL(r64) , DIMENSION(maxlay * 2 ) ::   theta  = 0.0D0 ! Vector of average temperatures of glazing surfaces [K] {2*maxlay}
  REAL(r64) , DIMENSION(maxlay * 2 + 1) ::   q = 0.0D0 !   Vector of various heat fluxes [W/m^2] {2*maxlay+1},
                                                        !    depending on element index:
                                                        !    1  = qout (heat flux from outer-most glazing surface to outdoor space)
                                                        !   2*i = qpane(i) (heat flux through i-th glazing layer)
                                                        ! 2*i-1 = qgap(i) (heat flux from i-th glazing cavity to indoor-faced
                                                        !          surface of the adjacent glazing layer)
                                                        ! 2*nlayer+1 = qin (heat flux from indoor space to inner-most glazing
                                                        !              surface)
  REAL(r64) , DIMENSION(maxlay1) :: qprim = 0.0D0 ! Vector of heat fluxes from the outdoor-faced surfaces of glazing layers
                                                        !    towards the adjacent glazing cavity [W/m2]
  REAL(r64) , DIMENSION(maxlay1) ::  qv = 0.0D0 !     Vector of heat fluxes to each gap by ventillation [W/m^2]
  REAL(r64) :: ufactor = 0.0D0 !    Center of glass U-value [W/m^2.K]
  REAL(r64) :: sc  = 0.0D0 !        Shading Coefficient
  REAL(r64) :: hflux   = 0.0D0 !     Net heat flux between room and window [W/m^2]
  REAL(r64) :: rhum  = 0.0D0 !       Relative humidity at inner-most surface for condensation
  REAL(r64) ::   rhout = 0.0D0 !       Relative humidity at outer-most surface for condensation
  REAL(r64) ::   hcin  = 0.0D0 !       Indoor convective surface heat transfer coefficient  [W/m^2.K]
  REAL(r64) ::   hcout = 0.0D0 !       Outdoor convective surface heat transfer coefficient [W/m^2.K]
  REAL(r64) ::   hrin  = 0.0D0 !       Indoor radiative surface heat transfer coefficient [W/m^2.K]
  REAL(r64) ::   hrout = 0.0D0 !       Outdoor radiative surface heat transfer coefficient [W/m^2.K]
  REAL(r64) , DIMENSION(maxlay1) ::   hcgap  = 0.0D0 !      Convective part of gap effective conductivity {maxlay}
  REAL(r64) , DIMENSION(maxlay1) ::   hrgap  = 0.0D0 ! Radiative part of gap effective conductivity (including in and out)
  REAL(r64) ::   shgc  = 0.0D0 !       Solar heat gain coefficient - per ISO 15099
  REAL(r64) ::   shgct = 0.0D0 !       Solar heat gain coefficient - per old procedure
  REAL(r64) ::   tamb  = 0.0D0 !       Outdoor environmental temperature [K]
  REAL(r64) ::     troom  = 0.0D0 !      Indoor environmental temperature [K]
  REAL(r64) , DIMENSION(maxlay) ::     hg    = 0.0D0  !      Gas conductance of the glazing cavity
                                                      !         [W/m^2.K] - EN673 and ISO 10292 procedure
  REAL(r64) , DIMENSION(maxlay) ::     hr   = 0.0D0   !      Radiation conductance of the glazing cavity
                                                      !         [W/m^2.K] - EN673 and ISO 10292 procedure
  REAL(r64) , DIMENSION(maxlay) ::     hs  = 0.0D0    !     Thermal conductance of the glazing cavity
                                                      !         [W/m^2.K] - EN673 and ISO 10292 procedure
  REAL(r64) ::     he  = 0.0D0 !         External heat transfer coefficient [W/m^2.K] - EN673 and ISO 10292 procedure
  REAL(r64) ::     hi = 0.0D0 !          Internal heat transfer coefficient [W/m^2.K] - EN673 and ISO 10292 procedure
  REAL(r64) , DIMENSION(maxlay + 1)  ::  Ra = 0.0D0 !         Vector of Rayleigh numbers, for each gap {maxlay}
  REAL(r64) , DIMENSION(maxlay + 1)  ::  Nu = 0.0D0 !         Vector of Nusselt numbers, for each gap {maxlay}
  INTEGER ::   nperr = 0 !               Error code
  REAL(r64) ::   ShadeEmisRatioOut = 0.0D0 !    Ratio of modified to glass emissivity at the outermost glazing surface
  REAL(r64) ::   ShadeEmisRatioIn   = 0.0D0 !   Ratio of modified to glass emissivity at the innermost glazing surface
  REAL(r64) ::   ShadeHcRatioOut   = 0.0D0 !    Ratio of modified to unshaded Hc at the outermost glazing surface
  REAL(r64) ::   ShadeHcRatioIn    = 0.0D0 !    Ratio of modified to unshaded Hc at the innermost glazing surface
  REAL(r64) ::   HcUnshadedOut     = 0.0D0 !    Hc value at outdoor surface of an unshaded subsystem [W/m^2.K]
  REAL(r64) ::   HcUnshadedIn     = 0.0D0 !     Hc value at indoor surface of an unshaded subsystem [W/m^2.K]
  REAL(r64) , DIMENSION(maxlay)  ::  Keff   = 0.0D0 !    Vector of keff values for gaps [W/m.K] {maxlay}
  REAL(r64) , DIMENSION(maxlay-1)  ::    ShadeGapKeffConv  = 0.0D0 ! Vector of convective keff values for areas above/below
                                                                   ! SD layers [W/m.K] {maxlay-1}


  INTEGER   :: ZoneNum                      ! Zone number corresponding to SurfNum

  REAL(r64) :: locTCSpecTemp                ! The temperature corresponding to the specified optical properties of the TC layer
  REAL(r64) :: locTCLayerTemp               ! TC layer temperature at each time step. C
  LOGICAL   :: locTCFlag =.False.           ! True if this surface is a TC window
  REAL(r64) :: deltaTemp(100) = 0.0d0
  INTEGER   :: i
  INTEGER   :: iMinDT(1) = 0
  INTEGER   :: IDConst(100) = 0
  REAL(r64) :: dT0 = 0.0d0
  REAL(r64) :: dT1 = 0.0d0

  INTEGER   :: IConst                       ! Construction number
  INTEGER   :: TotLay                       ! Total number of layers in a construction
                                            !   (sum of solid layers and gap layers)
  INTEGER   :: Lay                          ! Layer number
  INTEGER   :: LayPtr                       ! Material number for a layer
  INTEGER   :: ShadingLayPtr                ! Shading layer pointer for effective temperature calculations
  INTEGER   :: GlassLayPtr                  ! Glass layer pointer for effective temperature calculations
  REAL(r64) :: EpsGlassIR
  REAL(r64) :: RhoGlassIR
  REAL(r64) :: TauShadeIR
  REAL(r64) :: EpsShadeIR
  REAL(r64) :: RhoShadeIR
  INTEGER   :: IGlass                       ! glass layer number (1,2,3,...)
  INTEGER   :: IGap                         ! Gap layer number (1,2,...)
  INTEGER   :: ShadeLayPtr                  ! Material number corresponding to a shade layer
  INTEGER   :: TotGlassLay                  ! Total number of glass layers in a construction
  INTEGER   :: BlNum                        ! Blind number
  INTEGER   :: ZoneEquipConfigNum
  INTEGER   :: NodeNum
  REAL(r64) :: SumSysMCp                    ! Zone sum of air system MassFlowRate*Cp
  REAL(r64) :: SumSysMCpT                   ! Zone sum of air system MassFlowRate*Cp*T
  REAL(r64) :: MassFlowRate
  REAL(r64) :: NodeTemp
  REAL(r64) :: CpAir
  REAL(r64) :: RefAirTemp ! reference air temperatures
  INTEGER   :: tmpGasType
  INTEGER   :: k                            ! Layer counter
  INTEGER   :: SurfNumAdj                   ! An interzone surface's number in the adjacent zone
  INTEGER   :: ZoneNumAdj                   ! An interzone surface's adjacent zone number
  INTEGER   :: ShadeFlag                    ! Flag indicating whether shade or blind is on, and shade/blind position
  INTEGER   :: imix
  REAL(r64) :: EffShBlEmiss                 ! Effective interior shade or blind emissivity
  REAL(r64) :: EffGlEmiss                   ! Effective inside glass emissivity when interior shade or blind

  REAL(r64)         :: IncidentSolar         ! Solar incident on outside of window (W)
  REAL(r64)         :: ConvHeatFlowNatural   ! Convective heat flow from gap between glass and interior shade or blind (W)
  REAL(r64)         :: ShadeArea             ! shade/blind area (m2)
  REAL(r64)         :: sconsh                ! shade/blind conductance (W/m2-K)
  REAL(r64)         :: CondHeatGainShade     ! Conduction through shade/blind, outside to inside (W)

  REAL(r64)         :: ShGlReflFacIR         ! Factor for long-wave inter-reflection between shade/blind and adjacent glass
  REAL(r64)         :: RhoGlIR1,RhoGlIR2     ! Long-wave reflectance of glass surface facing shade/blind; 1=exterior shade/blind,
                                             !  2=interior shade/blind
  REAL(r64)         :: RhoShIR1,RhoShIR2     ! Long-wave reflectance of shade/blind surface facing glass; 1=interior shade/blind,
                                             !  2=exterior shade/blind
  REAL(r64)         :: EpsShIR1,EpsShIR2     ! Long-wave emissivity of shade/blind surface facing glass; 1=interior shade/blind,
                                             !  2=exterior shade/blind
  REAL(r64)         :: TauShIR               ! Long-wave transmittance of isolated shade/blind
  REAL(r64)         :: NetIRHeatGainShade    ! Net IR heat gain to zone from interior shade/blind (W)
  REAL(r64)         :: NetIRHeatGainGlass    ! Net IR heat gain to zone from shade/blind side of glass when interior
                                             !  shade/blind is present. Zero if shade/blind has zero IR transmittance (W)
  REAL(r64)         :: ConvHeatGainFrZoneSideOfShade ! Convective heat gain to zone from side of interior shade facing zone (W)
  REAL(r64)         :: ConvHeatGainFrZoneSideOfGlass ! Convective heat gain to zone from side of glass facing zone when
                                                   !  no interior shade/blind is present (W)
  REAL(r64)         :: CondHeatGainGlass     ! Conduction through inner glass layer, outside to inside (W)
  REAL(r64)         :: TotAirflowGap         ! Total volumetric airflow through window gap (m3/s)
  REAL(r64)         :: TAirflowGapOutlet     ! Temperature of air leaving airflow gap between glass panes (K)
  REAL(r64)         :: TAirflowGapOutletC    ! Temperature of air leaving airflow gap between glass panes (C)
  REAL(r64)         :: ConvHeatFlowForced    ! Convective heat flow from forced airflow gap (W)
  REAL(r64)         :: InletAirHumRat        ! Humidity ratio of air from window gap entering fan
  REAL(r64)         :: ZoneTemp              ! Zone air temperature (C)
  REAL(r64)         :: CpAirOutlet           ! Heat capacity of air from window gap (J/kg-K)
  REAL(r64)         :: CpAirZone             ! Heat capacity of zone air (J/kg-K)
  REAL(r64)         :: ConvHeatGainToZoneAir ! Convective heat gain to zone air from window gap airflow (W)
  INTEGER           :: ConstrNumSh           ! Construction number with shading device
  REAL(r64)         :: TransDiff             ! Diffuse shortwave transmittance
  INTEGER           :: CalcSHGC = 0          ! SHGC calculations are not necessary for E+ run
  INTEGER           :: NumOfIterations = 0   !

  integer :: GasType  !locally used coefficent to point at correct gas type
  integer :: ICoeff

  CHARACTER (len=2000) :: tarcogErrorMessage ! store error text from tarcog

  !Simon: locally used variables
  integer :: ngllayer, nglface, nglfacep, tempInt
  integer :: PillarPtr, DeflectionPtr, GasPointer, ThermalModelNum
  real(r64) :: Rmir, outir, Ebout                         ! IR radiance of window's interior surround (W/m2)
  REAL(r64) :: dominantGapWidth               ! store value for dominant gap width.  Used for airflow calculations

  ! fill local vars

  CalcDeflection = 0
  CalcSHGC = 0

  if (CalcCondition == noCondition) then
    ConstrNum = Surface(SurfNum)%Construction
    SurfNumAdj = Surface(SurfNum)%ExtBoundCond
    ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
  end if

  TotGlassLay = Construct(ConstrNum)%TotGlassLayers
  ngllayer = Construct(ConstrNum)%TotGlassLayers
  nglface  = 2*ngllayer
  nglfacep = nglface
  hrin = 0.0d0
  hcin = 0.0d0
  hrout = 0.0d0
  hcout = 0.0d0

  Pa = OutBaroPress

  ThermalModelNum = Construct(ConstrNum)%BSDFInput%ThermalModel
  standard        = WindowThermalModel(ThermalModelNum)%CalculationStandard
  ThermalMod      = WindowThermalModel(ThermalModelNum)%ThermalModel
  CalcDeflection  = WindowThermalModel(ThermalModelNum)%DeflectionModel
  SDScalar        = WindowThermalModel(ThermalModelNum)%SDScalar
  VacuumPressure  = WindowThermalModel(ThermalModelNum)%VacuumPressureLimit
  Tini            = WindowThermalModel(ThermalModelNum)%InitialTemperature - KelvinConv
  Pini            = WindowThermalModel(ThermalModelNum)%InitialPressure

  if (CalcCondition == noCondition) then
    ZoneNum = Surface(SurfNum)%Zone
  end if

  nlayer = Construct(ConstrNum)%TotSolidLayers
  isky = 3  ! IR radiation is provided from external source
  iwd  = 0 ! assume windward for now.  TODO compare surface normal with wind direction

  if (CalcCondition == noCondition) then

    ! determine reference air temperature for this surface
                                                              SELECT CASE (Surface(SurfNum)%TAirRef)
      CASE (ZoneMeanAirTemp)
          RefAirTemp = MAT(ZoneNum)
      CASE (AdjacentAirTemp)
          RefAirTemp = TempEffBulkAir(SurfNum)
      CASE (ZoneSupplyAirTemp)
              ! determine ZoneEquipConfigNum for this zone
  !            ControlledZoneAirFlag = .FALSE.
          ZoneEquipConfigNum = ZoneNum
  !            DO ZoneEquipConfigNum = 1, NumOfControlledZones
  !                IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum /= ZoneNum) CYCLE
  !                ControlledZoneAirFlag = .TRUE.
  !                EXIT
  !            END DO ! ZoneEquipConfigNum
              ! check whether this zone is a controlled zone or not
          IF (.NOT. Zone(ZoneNum)%IsControlled) THEN
             CALL ShowFatalError('Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone '//  &
                                 TRIM(Zone(ZoneNum)%Name))
             RETURN
          END IF
          ! determine supply air conditions
          SumSysMCp = 0.0d0
          SumSysMCpT = 0.0d0
          DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
              NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
              MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
              CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp,'CalcComplexWindowThermal')
              SumSysMCp = SumSysMCp + MassFlowRate * CpAir
              SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
          END DO
            ! a weighted average of the inlet temperatures.
            RefAirTemp = SumSysMCpT/SumSysMCp
        CASE DEFAULT
            ! currently set to mean air temp but should add error warning here
            RefAirTemp = MAT(ZoneNum)
    END SELECT

    tind = RefAirTemp + KelvinConv  ! Inside air temperature

    ! now get "outside" air temperature
    IF(SurfNumAdj > 0) THEN  ! Interzone window

    ZoneNumAdj = Surface(SurfNumAdj)%Zone

     ! determine reference air temperature for this surface
    SELECT CASE (Surface(SurfNumAdj)%TAirRef)
      CASE (ZoneMeanAirTemp)
          RefAirTemp = MAT(ZoneNumAdj)
      CASE (AdjacentAirTemp)
          RefAirTemp = TempEffBulkAir(SurfNumAdj)
      CASE (ZoneSupplyAirTemp)
          ! determine ZoneEquipConfigNum for this zone
          ZoneEquipConfigNum = ZoneNum
          ! check whether this zone is a controlled zone or not
          IF (.NOT. Zone(ZoneNum)%IsControlled) THEN
            CALL ShowFatalError('Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone '//  &
                               TRIM(Zone(ZoneNum)%Name))
            RETURN
          END IF
          ! determine supply air conditions
          SumSysMCp = 0.0d0
          SumSysMCpT = 0.0d0
          DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
              NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
              MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
              CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNumAdj), NodeTemp, 'CalcComplexWindowThermal')
              SumSysMCp = SumSysMCp + MassFlowRate * CpAir
              SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
          END DO
          ! a weighted average of the inlet temperatures.
          RefAirTemp = SumSysMCpT/SumSysMCp
      CASE DEFAULT
          ! currently set to mean air temp but should add error warning here
          RefAirTemp = MAT(ZoneNumAdj)
    END SELECT

      Tout = RefAirTemp + KelvinConv  ! outside air temperature

      tsky = MRT(ZoneNumAdj) + KelvinConv ! TODO this misses IR from sources such as high temp radiant and baseboards

    !  ! Add long-wave radiation from adjacent zone absorbed by glass layer closest to the adjacent zone.
    !
    !  AbsRadGlassFace(1) = AbsRadGlassFace(1) + QRadThermInAbs(SurfNumAdj)
    !
    !  ! The IR radiance of this window's "exterior" surround is the IR radiance
    !  ! from surfaces and high-temp radiant sources in the adjacent zone
    !
    outir = SurfaceWindow(SurfNumAdj)%IRfromParentZone + QHTRadSysSurf(SurfNumAdj) + QHWBaseboardSurf(SurfNumAdj)

    ELSE  ! Exterior window (ExtBoundCond = 0)

      IF(Surface(SurfNum)%ExtWind) THEN  ! Window is exposed to wind (and possibly rain)
        IF(IsRain) THEN  ! Raining: since wind exposed, outside window surface gets wet
          tout = Surface(SurfNum)%OutWetBulbTemp + KelvinConv
        ELSE             ! Dry
          tout = Surface(SurfNum)%OutDryBulbTemp + KelvinConv
        END IF
      ELSE                               ! Window not exposed to wind
        tout = Surface(SurfNum)%OutDryBulbTemp + KelvinConv
      END IF
        !tsky = SkyTemp + TKelvin
        tsky = SkyTempKelvin
        Ebout = sigma * tout**4
        outir = Surface(SurfNum)%ViewFactorSkyIR * (AirSkyRadSplit(SurfNum)*sigma*tsky**4 +   &
           (1.0d0-AirSkyRadSplit(SurfNum))*Ebout) + Surface(SurfNum)%ViewFactorGroundIR * Ebout

    END IF

    hin = HConvIn(SurfNum)  ! Room-side surface convective film conductance
    ibc(2) = 0  ! convective coefficient on indoor side will be recalculated (like in Winkelmann routines)

    !hcout=HExtConvCoeff  ! Exterior convection coefficient is passed in from outer routine
    hout=HExtConvCoeff  ! Exterior convection coefficient is passed in from outer routine
    ibc(1) = 2 ! prescribed convective film coeff on outdoor side
    tilt = Surface(SurfNum)%Tilt
    height = Surface(SurfNum)%Height
    heightt = height ! for now put same window and glazing pocket hights
    width = Surface(SurfNum)%Width

    !indoor mean radiant temperature.
    ! IR incident on window from zone surfaces and high-temp radiant sources
    rmir = SurfaceWindow(SurfNum)%IRfromParentZone + QHTRadSysSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + &
      QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)
    trmin = ( rmir / StefanBoltzmann)**0.25d0  ! TODO check model equation.

    ! outdoor wind speed
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      wso =  0.0d0  ! No wind exposure
      !ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      !  wso =  0.0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      wso = Surface(SurfNum)%WindSpeed
    ENDIF

    ! indoor wind speed
    wsi = 0.0d0 ! assumuption (TODO, what to use for inside air velocity?)


    fclr = 1.0D0 - CloudFraction
  end if

  !now fill layer data
  !IConst = ConstrNum
  !IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn &
  !    .OR.ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
  !  IConst = Surface(SurfNum)%ShadedConstruction
  !  IF(Surfacewindow(SurfNum)%StormWinFlag > 0) IConst = Surface(SurfNum)%StormWinShadedConstruction
  !END IF
  !TotLay = Construct(IConst)%TotLayers
  TotLay = Construct(ConstrNum)%TotLayers
  IGap = 0

  !****************************************************************************************************
  !
  ! Inside and outside gas coefficients
  !
  !****************************************************************************************************
  iprop(1, 1) = 1 ! air on outdoor side
  frct(1, 1)  = 1.0d0   ! pure air on outdoor side
  nmix(1)     = 1 ! pure air on outdoor side

  iprop(nlayer + 1, 1) = 1 ! air on indoor side
  frct(nlayer + 1, 1)  = 1.0d0 ! pure air on indoor side
  nmix(nlayer + 1)     = 1 ! pure air on indoor side

  !Simon: feed gas coefficients with air.  This is necessary for tarcog because it is used on indoor and outdoor sides
  GasType = GasCoeffsAir
  wght(iprop(1, 1)) = GasWght(GasType)
  gama(iprop(1, 1)) = GasSpecificHeatRatio(GasType)
  DO ICoeff = 1,3
    gcon(iprop(1, 1), ICoeff)   = GasCoeffsCon(GasType,ICoeff)
    gvis(iprop(1, 1), ICoeff)   = GasCoeffsVis(GasType,ICoeff)
    gcp(iprop(1, 1), ICoeff)   = GasCoeffsCp (GasType,ICoeff)
  END DO

  ! Fill window layer properties needed for window layer heat balance calculation
  IGlass = 0
  IGap = 0
  DO Lay = 1,TotLay
    LayPtr = Construct(ConstrNum)%LayerPoint(Lay)

    IF(( Material(LayPtr)%Group == WindowGlass) .OR. (Material(LayPtr)%Group == WindowSimpleGlazing) ) THEN
      IGlass = IGlass + 1
      LayerType(IGlass) = 0 ! this marks specular layer type
      thick(IGlass) =    Material(LayPtr)%Thickness
      scon(IGlass) =     Material(LayPtr)%Conductivity
      emis(2*IGlass-1) = Material(LayPtr)%AbsorpThermalFront
      emis(2*IGlass) =   Material(LayPtr)%AbsorpThermalBack
      tir(2*IGlass-1) =  Material(LayPtr)%TransThermal
      tir(2*IGlass) =    Material(LayPtr)%TransThermal
      YoungsMod(IGlass) = Material(LayPtr)%YoungModulus
      PoissonsRat(IGlass) = Material(LayPtr)%PoissonsRatio
    else if (Material(LayPtr)%Group == ComplexWindowShade) then
      if (CalcCondition == noCondition) then
        if (Lay == 1) SurfaceWindow(SurfNum)%ShadingFlag = ExtShadeOn
        if (Lay == TotLay) SurfaceWindow(SurfNum)%ShadingFlag = IntShadeOn
      end if
      IGlass = IGlass + 1
      TempInt = Material(LayPtr)%ComplexShadePtr
      LayerType(IGlass)  = ComplexShade(TempInt)%LayerType

      thick(IGlass)      = ComplexShade(TempInt)%Thickness
      scon(IGlass)      = ComplexShade(TempInt)%Conductivity
      emis(2*IGlass-1)  = ComplexShade(TempInt)%FrontEmissivity
      emis(2*IGlass)    = ComplexShade(TempInt)%BackEmissivity
      tir(2*IGlass-1)    = ComplexShade(TempInt)%IRTransmittance
      tir(2*IGlass)      = ComplexShade(TempInt)%IRTransmittance

      ! This needs to be converted into correct areas. That can be done only after loading complete window data
      Atop(IGlass)      = ComplexShade(TempInt)%TopOpeningMultiplier
      Abot(IGlass)      = ComplexShade(TempInt)%BottomOpeningMultiplier
      Al(IGlass)        = ComplexShade(TempInt)%LeftOpeningMultiplier
      Ar(IGlass)        = ComplexShade(TempInt)%RightOpeningMultiplier
      Ah(IGlass)        = ComplexShade(TempInt)%FrontOpeningMultiplier

      SlatThick(IGlass)    = ComplexShade(TempInt)%SlatThickness
      SlatWidth(IGlass)    = ComplexShade(TempInt)%SlatWidth
      SlatAngle(IGlass)    = ComplexShade(TempInt)%SlatAngle
      SlatCond(IGlass)    = ComplexShade(TempInt)%SlatConductivity
      SlatSpacing(IGlass)  = ComplexShade(TempInt)%SlatSpacing
      SlatCurve(IGlass)    = ComplexShade(TempInt)%SlatCurve
    else if(Material(LayPtr)%Group == ComplexWindowGap) THEN
      IGap = IGap + 1
      gap(IGap)  = Material(LayPtr)%Thickness
      presure(IGap) = Material(LayPtr)%Pressure

      DeflectionPtr = Material(LayPtr)%DeflectionStatePtr
      if(DeflectionPtr /= 0) then
        GapDefMax(IGap) = DeflectionState(DeflectionPtr)%DeflectedThickness
      else
        GapDefMax(IGap) = gap(IGap)
      end if

      PillarPtr = Material(LayPtr)%SupportPillarPtr

      if (PillarPtr.ne.0) then
        SupportPlr(IGap) = 1
        PillarSpacing(IGap) = SupportPillar(PillarPtr)%Spacing
        PillarRadius(IGap)  = SupportPillar(PillarPtr)%Radius
      end if

      GasPointer = Material(LayPtr)%GasPointer

      nmix(IGap+1) = Material(GasPointer)%NumberOfGasesInMixture
      DO IMix = 1,nmix(IGap+1)
        !iprop(IGap+1, IMix) = Material(LayPtr)%GasType(IMix)
        !iprop(IGap+1, IMix) = GetGasIndex(Material(LayPtr)%GasWGHT(IMix))
        frct(IGap+1,IMix) = Material(GasPointer)%GasFract(IMix)

        !Now has to build-up gas coefficients arrays. All used gasses should be stored into these arrays and
        !to be correctly referenced by gap arrays

        !First check if gas coefficients are already part of array.  Duplicates are not necessary
        CALL CheckGasCoefs(Material(GasPointer)%GasWGHT(IMix), iprop(IGap+1, IMix), wght, feedData)
        IF (feedData) THEN
          wght(iprop(IGap+1, IMix)) = Material(GasPointer)%GasWGHT(IMix)
          gama(iprop(IGap+1, IMix)) = Material(GasPointer)%GasSpecHeatRatio(IMix)
          DO i = 1, 3
            gcon(iprop(IGap+1, IMix), i) = Material(GasPointer)%GasCON(IMix, i)
            gvis(iprop(IGap+1, IMix), i) = Material(GasPointer)%GasVIS(IMix, i)
            gcp(iprop(IGap+1, IMix), i)  = Material(GasPointer)%GasCP(IMix, i)
          END DO
        END IF !IF feedData THEN
      END DO
    else
      CALL ShowContinueError('Illegal layer type in Construction:ComplexFenestrationState.')
      CALL ShowContinueError('Allowed object are:')
      CALL ShowContinueError('   - WindowMaterial:Glazing')
      CALL ShowContinueError('   - WindowMaterial:ComplexShade')
      CALL ShowContinueError('   - WindowMaterial:Gap')
      CALL ShowFatalError('halting because of error in layer definition for Construction:ComplexFenestrationState')
    end if

  END DO  ! End of loop over glass, gap and blind/shade layers in a window construction

  IF (CalcCondition == noCondition) THEN
    ! now calculate correct areas for multipliers
    DO Lay = 1, nlayer
      IF (LayerType(Lay) /= 0) THEN ! Layer is shading
        ! before changing multipliers, need to determine which one is dominant gap width
        IF (Lay == 1) THEN ! Exterior shading device
          dominantGapWidth = gap(Lay)
        ELSE IF (Lay == nlayer) THEN ! Interior shading device
          dominantGapWidth = gap(Lay - 1)
        ELSE ! In-between shading device
          dominantGapWidth = MIN(gap(Lay-1), gap(Lay))
        END IF
        Atop(Lay)      = Atop(Lay) * dominantGapWidth * width
        Abot(Lay)      = Abot(Lay) * dominantGapWidth * width
        Al(Lay)        = Al(Lay) * dominantGapWidth * height
        Ar(Lay)        = Ar(Lay) * dominantGapWidth * height
        Ah(Lay)        = Ah(Lay) * width * height
      END IF
    END DO
  END IF

  !ThermalMod = 0
  Debug_mode = 0
  CalcSHGC = 0

  Window_ID = ConstrNum

  !vector of absorbed solar energy fractions for each layer.
  asol = 0.0D0
  ! direct solar radiation
  if (CalcCondition == noCondition) then
    ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
    dir = QRadSWOutIncident(SurfNum) +  QS(Surface(SurfNum)%Zone) ! TODO, check , !
    !                  currently using Exterior beam plus diffuse solar incident on surface
    !                  plus zone short wave.  CHECK
    !if (dir.ne.0.0d0) then
      do IGlass = 1, nlayer
        !IF (dir > 0.0D0 ) THEN
        asol(IGLASS) = QRadSWwinAbs(SurfNum, IGLASS)
        !ELSE
        !  asol(IGLASS) = 0.0D0
        !ENDIF
      end do
    !end if

    ! Add contribution of IR from zone internal gains (lights, equipment and people). This is absorbed in zone-side layer and it
    ! is assumed that nothing is transmitted through
    asol(nlayer) = asol(nlayer) + QRadThermInAbs(SurfNum)

    presure = OutBaroPress

    ! Instead of doing temperature guess get solution from previous iteration.  That should be much better than guess
    do k = 1,2*nlayer
      theta(k) = SurfaceWindow(SurfNum)%ThetaFace(k)
    end do

  end if

  ! Standard conditions run (winter and summer)
  if (CalcCondition == winterCondition) then
    tind = 294.15d0
    tout = 255.15d0
    hcout = 26.0d0
    wso = 5.5d0
    dir = 0.0d0
  else if (CalcCondition == summerCondition) then
    tind = 297.15d0
    tout = 305.15d0
    hcout = 15.0d0
    wso = 2.75d0
    dir = 783.0d0
    CalcSHGC = 1
  end if

  ! Common condition data
  if (CalcCondition /= noCondition) then
    trmin = tind
    outir = 0.0d0
    tsky = tout
    wsi = 0.0d0
    fclr = 1.0d0
    ibc(1) = 0
    ibc(2) = 0
    presure = 101325.0d0
    iwd = 0 ! Windward wind direction
    isky = 0
    esky = 1.0d0
    height = 1.0d0
    heightt = 1.0d0
    width = 1.0d0
    tilt = 90.0d0
    ! Just to make initial quess different from absolute zero
    theta = 273.15d0
  end if

  !  call TARCOG
  CALL TARCOG90(nlayer, iwd, tout, tind, trmin, wso, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, &
                VacuumMaxGapThickness, CalcDeflection, Pa, Pini, Tini, gap, GapDefMax, thick, scon, YoungsMod, PoissonsRat, &
                tir, emis, totsol, tilt, asol, height, heightt, width, &
                presure, iprop, frct, gcon, gvis, gcp, wght, gama, nmix, &
                SupportPlr, PillarSpacing, PillarRadius, &
                theta, LayerDef, q, qv, ufactor, sc, hflux, hcin, hcout, hrin, hrout, hin, hout, hcgap, hrgap, shgc, nperr, &
                tarcogErrorMessage, shgct, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle,&
                SlatCond, SlatSpacing, SlatCurve, vvent,tvent, LayerType, nslice, LaminateA, LaminateB,&
                sumsol, hg, hr, hs, he, hi,Ra,Nu, standard, ThermalMod, Debug_mode, Debug_dir, Debug_file, Window_ID, IGU_ID,&
                ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn,&
                HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, CalcSHGC, NumOfIterations)

  ! process results from TARCOG
  IF ((nperr > 0).and.(nperr < 1000)) THEN ! process error signal from tarcog

    Call ShowSevereError('Window tarcog returned an error')
    tarcogErrorMessage = 'message = "'//tarcogErrorMessage//'"'
    Call ShowContinueErrorTimeStamp(tarcogErrorMessage)
    if (CalcCondition == noCondition) then
      CALL showContinueError('surface name = '//Surface(SurfNum)%Name)
    end if
    Call ShowContinueError('construction name = ' //Construct(ConstrNum)%Name)
    CALL ShowFatalError('halting because of error in tarcog')
  ELSE IF (CalcCondition == winterCondition) THEN
    NominalU(ConstrNum) = ufactor
  ELSE IF (CalcCondition == summerCondition) THEN
    !tempInt = SurfaceWindow(SurfNum)%ComplexFen%CurrentState
    !tempReal = SurfaceWindow(SurfNum)%ComplexFen%State(tempInt)%WinDiffTrans

    !Sum1 = 0.0d0
    !Sum2 = 0.0d0
    !do  j = 1 , ComplexWind(SurfNum)%Geom%Inc%NBasis     !Incident ray loop
    !  Sum2 = Sum2 + ComplexWind(SurfNum)%Geom%Inc%Lamda (j)
    !  do  m = 1 , ComplexWind(SurfNum)%Geom%Trn%NBasis     !Outgoing ray loop
    !    Sum1 =Sum1 + ComplexWind(SurfNum)%Geom%Inc%Lamda(j) * ComplexWind(SurfNum)%Geom%Trn%Lamda(m) * &
    !      & Construct(ConstrNum)%BSDFInput%SolFrtTrans ( j , m )
    !  end do      !Outgoing ray loop
    !end do      !Incident ray loop
    !if (Sum2 > 0 ) THEN
    !  tempReal = Sum1/Sum2
    !else
    !  tempReal = 0.
    !  call ShowWarningError ('BSDF--Inc basis has zero projected solid angle')
    !endif

    Construct(ConstrNum)%SummerSHGC = shgc

    !Construct(SurfNum)%VisTransNorm = SurfaceWindow(SurfNum)%ComplexFen%State(tempInt)%WinDiffVisTrans
  ELSE IF (CalcCondition == noCondition) THEN ! expect converged results...
         ! Window heat balance solution has converged.

    SurfaceWindow(SurfNum)%WindowCalcIterationsRep = NumOfIterations
    HConvIn(SurfNum) = hcin

         ! For interior shade, add convective gain from glass/shade gap air flow to zone convective gain;
         ! For all cases, get total window heat gain for reporting. See CalcWinFrameAndDividerTemps for
         ! contribution of frame and divider.

    SurfInsideTemp = theta(2*nlayer) - KelvinConv
    SurfOutsideTemp = theta(1) - KelvinConv
    SurfOutsideEmiss = emis(1)

    IncidentSolar = Surface(SurfNum)%Area * QRadSWOutIncident(SurfNum)
    IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
         ! Interior shade or blind
      ConvHeatFlowNatural = -qv(nlayer) * height * width

      SurfaceWindow(SurfNum)%ConvHeatFlowNatural = ConvHeatFlowNatural
      WinGapConvHtFlowRep(SurfNum) = ConvHeatFlowNatural
      WinGapConvHtFlowRepEnergy(SurfNum) = WinGapConvHtFlowRep(SurfNum) * TimeStepZone * SecInHour
         ! Window heat gain from glazing and shade/blind to zone. Consists of transmitted solar, convection
         !   from air exiting gap, convection from zone-side of shade/blind, net IR to zone from shade and net IR to
         !   zone from the glass adjacent to the shade/blind (zero if shade/blind IR transmittance is zero).
         ! Following assumes glazed area = window area (i.e., dividers ignored) in calculating
         !   IR to zone from glass when interior shade/blind is present.
      ShadeArea = Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea
      sconsh    = scon(ngllayer+1) / thick(ngllayer+1)
      nglfacep = nglface + 2
      CondHeatGainShade  = ShadeArea * sconsh * (theta(nglfacep-1) - theta(nglfacep))
      EpsShIR1 = emis(nglface+1)
      EpsShIR2 = emis(nglface+2)
      TauShIR  = tir(nglface+1)
      RhoShIR1 = MAX(0.d0,1.d0-TauShIR-EpsShIR1)
      RhoShIR2 = MAX(0.d0,1.d0-TauShIR-EpsShIR2)
      RhoGlIR2 = 1.d0-emis(2*ngllayer)
      ShGlReflFacIR = 1.d0-RhoGlIR2*RhoShIR1
      NetIRHeatGainShade = ShadeArea * &
        EpsShIR2*(sigma*theta(nglfacep)**4 - rmir) + &
        EpsShIR1*(sigma*theta(nglfacep-1)**4 - rmir)*RhoGlIR2*TauShIR/ShGlReflFacIR
      NetIRHeatGainGlass = ShadeArea * &
         (emis(2*ngllayer)*TauShIR/ShGlReflFacIR) * (sigma*theta(2*ngllayer)**4 - rmir)
      ConvHeatGainFrZoneSideOfShade = ShadeArea * hcin*(theta(nglfacep) - tind)
      WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatFlowNatural + ConvHeatGainFrZoneSideOfShade + &
                           NetIRHeatGainGlass + NetIRHeatGainShade
      ! store components for reporting
      WinGainConvGlazShadGapToZoneRep(SurfNum) = ConvHeatFlowNatural ! result is in [W]
      WinGainConvShadeToZoneRep(SurfNum)       = ConvHeatGainFrZoneSideOfShade
      WinGainIRGlazToZoneRep(SurfNum)          = NetIRHeatGainGlass
      WinGainIRShadeToZoneRep(SurfNum)         = NetIRHeatGainShade
    ELSE
         ! Interior shade or blind not present; innermost layer is glass
      CondHeatGainGlass = Surface(SurfNum)%Area * scon(nlayer)/thick(nlayer) * (theta(2*nlayer-1)-theta(2*nlayer))
      NetIRHeatGainGlass = Surface(SurfNum)%Area * emis(2*nlayer)*(sigma*theta(2*nlayer)**4 - rmir)
      ConvHeatGainFrZoneSideOfGlass = Surface(SurfNum)%Area * hcin*(theta(2*nlayer) - tind)
      WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass
      ! store components for reporting
      WinGainConvGlazToZoneRep(SurfNum) = ConvHeatGainFrZoneSideOfGlass
      WinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass
      ! need to report convective heat flow from the gap in case of exterior shade
      IF(ShadeFlag == ExtShadeOn) THEN
        ConvHeatFlowNatural = -qv(2) * height * width ! qv(1) is exterior environment

        WinGapConvHtFlowRep(SurfNum) = ConvHeatFlowNatural
        WinGapConvHtFlowRepEnergy(SurfNum) = WinGapConvHtFlowRep(SurfNum) * TimeStepZone * SecInHour
      END IF
    END IF

        ! Add convective heat gain from airflow window
        ! Note: effect of fan heat on gap outlet temperature is neglected since fan power (based
        ! on pressure drop through the gap) is extremely small

    !WinGapConvHtFlowRep(SurfNum) = 0.0d0
    !WinGapConvHtFlowRepEnergy(SurfNum) = 0.0d0
    TotAirflowGap = SurfaceWindow(SurfNum)%AirFlowThisTS * Surface(SurfNum)%Width
    TAirflowGapOutlet = KelvinConv ! TODO Need to calculate this
    TAirflowGapOutletC = TAirflowGapOutlet-KelvinConv
    SurfaceWindow(SurfNum)%TAirflowGapOutlet = TAirflowGapOutletC
    IF(SurfaceWindow(SurfNum)%AirFlowThisTS > 0.0d0) THEN
      ConvHeatFlowForced = sum(qv) ! TODO.  figure forced ventilation heat flow in Watts

      WinGapConvHtFlowRep(SurfNum) = ConvHeatFlowForced
      WinGapConvHtFlowRepEnergy(SurfNum) = WinGapConvHtFlowRep(SurfNum) * TimeStepZone * SecInHour
      ! Add heat from gap airflow to zone air if destination is inside air; save the heat gain to return
      ! air in case it needs to be sent to the zone (due to no return air determined in HVAC simulation)
      IF(SurfaceWindow(SurfNum)%AirFlowDestination == AirFlowWindow_Destination_IndoorAir  .or.   &
         SurfaceWindow(SurfNum)%AirFlowDestination == AirFlowWindow_Destination_ReturnAir) THEN
        IF (SurfaceWindow(SurfNum)%AirflowSource == AirFlowWindow_Source_IndoorAir) THEN
          InletAirHumRat = ZoneAirHumRat(ZoneNum)
        ELSE  ! AirflowSource = outside air
          InletAirHumRat = OutHumRat
        END IF
        ZoneTemp    = MAT(ZoneNum)  ! this should be Tin (account for different reference temps)
        CpAirOutlet = PsyCpAirFnWTdb(InletAirHumRat,TAirflowGapOutletC)
        CpAirZone   = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),ZoneTemp)
        ConvHeatGainToZoneAir = TotAirflowGap * (CpAirOutlet*(TAirflowGapOutletC) - CpAirZone*ZoneTemp)
        IF (SurfaceWindow(SurfNum)%AirFlowDestination == AirFlowWindow_Destination_IndoorAir) THEN
          SurfaceWindow(SurfNum)%ConvHeatGainToZoneAir = ConvHeatGainToZoneAir
          WinHeatGain(SurfNum) = WinHeatGain(SurfNum) + ConvHeatGainToZoneAir
        ELSE
          SurfaceWindow(SurfNum)%RetHeatGainToZoneAir = ConvHeatGainToZoneAir
        END IF
      END IF
      ! For AirflowDestination = ReturnAir in a controlled (i.e., conditioned) zone with return air, see CalcZoneLeavingConditions
      ! for calculation of modification of return-air temperature due to airflow from window gaps into return air.
    END IF

        ! Correct WinHeatGain for interior diffuse shortwave (solar and shortwave from lights) transmitted
        ! back out window
    ConstrNum = Surface(SurfNum)%Construction
    !ConstrNumSh = Surface(SurfNum)%ShadedConstruction
    !IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
    !  ConstrNum = Surface(SurfNum)%StormWinConstruction
    !  ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
    !END IF
    !IF(ShadeFlag <= 0) THEN
      TransDiff = Construct(ConstrNum)%TransDiff
    !ELSE IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn) THEN
    !  TransDiff = Construct(ConstrNum)%TransDiff
    !ELSE IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR.ShadeFlag==BGBlindOn) THEN
    !  TransDiff = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
    !                             Construct(ConstrNumSh)%BlTransDiff)
    !ELSE IF(ShadeFlag == SwitchableGlazing) THEN
    !  TransDiff = InterpSW(SurfaceWindow(SurfNum)%SwitchingFactor,Construct(ConstrNum)%TransDiff, &
    !                             Construct(ConstrNumSh)%TransDiff)
    !END IF
    WinHeatGain(SurfNum) = WinHeatGain(SurfNum) - QS(Surface(SurfNum)%Zone) * Surface(SurfNum)%Area * TransDiff
    WinLossSWZoneToOutWinRep(SurfNum) = QS(Surface(SurfNum)%Zone) * Surface(SurfNum)%Area * TransDiff

    IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn) THEN
      WinShadingAbsorbedSolar(SurfNum) = (SurfaceWindow(SurfNum)%ExtBeamAbsByShade + &
                                         SurfaceWindow(SurfNum)%ExtDiffAbsByShade) * &
                                         (Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea)
      WinShadingAbsorbedSolarEnergy(SurfNum) = WinShadingAbsorbedSolar(SurfNum) * TimeStepZone * SecInHour
    END IF
    IF(SunIsUp) THEN
      WinSysSolTransmittance(SurfNum) = WinTransSolar(SurfNum) / &
        (QRadSWOutIncident(SurfNum)*(Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea)+0.0001d0)
      WinSysSolAbsorptance(SurfNum)   = (QRadSWwinAbsTot(SurfNum)+WinShadingAbsorbedSolar(SurfNum)) / &
        (QRadSWOutIncident(SurfNum)*(Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea)+0.0001d0)
      WinSysSolReflectance(SurfNum)   = 1.0d0 - WinSysSolTransmittance(SurfNum) - WinSysSolAbsorptance(SurfNum)
    ELSE
      WinSysSolTransmittance(SurfNum) = 0.0d0
      WinSysSolAbsorptance(SurfNum)   = 0.0d0
      WinSysSolReflectance(SurfNum)   = 0.0d0
    END IF

       ! Save hcv for use in divider calc with interior or exterior shade (see CalcWinFrameAndDividerTemps)
   IF(ShadeFlag==IntShadeOn) SurfaceWindow(SurfNum)%ConvCoeffWithShade = 0.0d0

      IF(ShadeFlag == IntShadeOn) THEN
        SurfInsideTemp  = theta(2*ngllayer+2) - KelvinConv

        ! Get properties of inside shading layer
        ShadingLayPtr = Construct(ConstrNum)%LayerPoint(TotLay)
        ShadingLayPtr = Material(ShadingLayPtr)%ComplexShadePtr
        TauShadeIR = ComplexShade(ShadingLayPtr)%IRTransmittance
        EpsShadeIR = ComplexShade(ShadingLayPtr)%FrontEmissivity
        RhoShadeIR = MAX(0.d0,1.d0-TauShadeIR-EpsShadeIR)

        ! Get properties of glass next to inside shading layer
        GlassLayPtr = Construct(ConstrNum)%LayerPoint(TotLay - 2)
        EpsGlassIR = Material(GlassLayPtr)%AbsorpThermalBack
        RhoGlassIR = 1 - EpsGlassIR

        EffShBlEmiss = EpsShadeIR*(1.d0+RhoGlassIR*TauShadeIR/(1.d0-RhoGlassIR*RhoShadeIR))
        SurfaceWindow(SurfNum)%EffShBlindEmiss = EffShBlEmiss
        EffGlEmiss   = EpsGlassIR*TauShadeIR/(1.d0-RhoGlassIR*RhoShadeIR)
        SurfaceWindow(SurfNum)%EffGlassEmiss = EffGlEmiss
      !  EffShBlEmiss = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
      !                    SurfaceWindow(SurfNum)%EffShBlindEmiss)
      !  EffGlEmiss   = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
      !                    SurfaceWindow(SurfNum)%EffGlassEmiss)
        SurfaceWindow(SurfNum)%EffInsSurfTemp = (EffShBlEmiss * SurfInsideTemp + EffGlEmiss * (theta(2*ngllayer)-KelvinConv)) / &
                                                   (EffShBlEmiss + EffGlEmiss)
      !ELSE
      !  SurfInsideTemp = theta(2*ngllayer) - TKelvin
      !END IF
      !IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
        !SurfOutsideTemp = theta(2*ngllayer+1) - TKelvin  !this looks wrong.
      ELSE
        SurfOutsideTemp = theta(1) - KelvinConv
      END IF

      DO k = 1, nlayer
        SurfaceWindow(SurfNum)%ThetaFace(2*k-1) = theta(2*k-1)
        SurfaceWindow(SurfNum)%ThetaFace(2*k) = theta(2*k)

        ! temperatures for reporting
        FenLaySurfTempFront(SurfNum, k) = theta(2*k-1) - KelvinConv
        FenLaySurfTempBack(SurfNum, k) = theta(2*k) - KelvinConv
        !thetas(k) = theta(k)
      END DO

  ENDIF

  RETURN

END SUBROUTINE CalcComplexWindowThermal

! This function check if gas with molecular weight has already been feed into coefficients and
! feed arrays
SUBROUTINE CheckGasCoefs(currentWeight, indexNumber, wght, feedData)

USE DataPrecisionGlobals
USE TARCOGGassesParams, ONLY: maxgas

LOGICAL, INTENT(OUT) :: feedData
REAL(r64), DIMENSION(maxgas), INTENT(INOUT)           ::  wght

REAL(r64), INTENT(IN) :: currentWeight
INTEGER, INTENT(OUT) :: indexNumber

!Local variables
INTEGER :: counter = 1
INTEGER :: IMix, I
INTEGER :: numOfMixtures
LOGICAL :: coeffFound = .FALSE.

feedData = .FALSE.
coeffFound = .FALSE.
counter = 1
DO WHILE((counter.LE.maxgas).AND.(wght(counter).NE.0).AND.(.NOT.coeffFound))
  IF(ABS(currentWeight-wght(counter)).lt.1.0d-5) THEN
    coeffFound = .TRUE.
  ELSE
    counter = counter + 1
  ENDIF
END DO !DO WHILE((counter.LE.maxgas).AND.(wght(couner).NE.0).AND.(.NOT.coeffFound))

! In case coefficient is not found data needs to be stored in gas coefficients arrays
IF ((.NOT.coeffFound).AND.(counter.LT.maxgas)) THEN
  feedData = .TRUE.
ENDIF

indexNumber = counter

END SUBROUTINE CheckGasCoefs

INTEGER FUNCTION SearchAscTable(y,n,ytab)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Joe Klems
          !       DATE WRITTEN   Feb 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given an ascending monotonic table with n entries, find  an index i
          ! such that ytab(i-1) < y <= ytab(i)

          ! METHODOLOGY EMPLOYED:
          ! binary search

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT (IN)      :: y            ! Value to be found in the table
  INTEGER, INTENT (IN)        :: n    ! Number of values in the table
  REAL(r64), DIMENSION(:), INTENT (IN)  :: ytab      ! Table of values, monotonic, ascending order

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER     ::   Ih    ! Intex for upper end of interval
  INTEGER    ::   Il    ! Index for lower end of interval
  INTEGER    ::   Im    ! Index for midpoint of interval
  REAL(r64)    ::   Yh    ! Table value for upper end of interval
  REAL(r64)    ::  Yl    ! Table value for lower end of interval
  REAL(r64)    ::  Ym    ! Table value for midpoint of interval


  Yh = ytab(n)
  Yl = ytab(1)
  Ih = n
  Il = 1
  IF (y < Yl) THEN
    SearchAscTable = 1
    RETURN
  ELSE IF (y > Yh) THEN
    SearchAscTable = n
    RETURN
  ENDIF
  DO
    IF(Ih-Il <= 1) EXIT
    Im = (Ih + Il)/2
    Ym = ytab(Im)
    IF(y <= Ym) THEN
      Yh = Ym
      Ih = Im
    ELSE
      Yl = Ym
      Il = Im
    ENDIF

  END DO

  SearchAscTable = Ih

RETURN

END FUNCTION SearchAscTable

!=================================================================================================

SUBROUTINE CrossProduct(A, B, C)

  ! Cross product between vectors A and B

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: A(3),B(3),C(3)          ! Vector components: C = A X B

          ! FLOW:
  C(1) = A(2) * B(3) - A(3) * B(2)
  C(2) = A(3) * B(1) - A(1) * B(3)
  C(3) = A(1) * B(2) - A(2) * B(1)

  RETURN

END SUBROUTINE CrossProduct

SUBROUTINE PierceSurfaceVector(ISurf, Orig, Dir, IPIERC, HitPt)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       Sept 2003, FCW: modification of Daylighting routine DayltgPierceSurface
          !                             June 2011, JHK: inputs made vector types; copy of routine from
          !                                        SolarReflectionManager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns point CPhit that line through point R1 in direction of unit vector RN intersects
          ! the plan of surface ISurf. IPIERC = 1 if CPhit is inside the perimeter of ISurf. If not,
          ! IPIERC = 0. This routine works for convex and concave surfaces with 3 or more vertices.
          !
          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DPIERC.

          ! USE STATEMENTS:
  USE vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: ISurf      ! Surface index
  TYPE (vector), INTENT(IN)        :: Orig      ! Point from which ray originates
  TYPE (vector), INTENT(IN)        :: Dir      ! Unit vector along in direction of ray whose
                                             !  intersection with surface is to be determined
  INTEGER, INTENT(OUT)         :: IPIERC     ! =1 if line through point R1 in direction of unit vector
                                             !  RN intersects surface ISurf; =0 otherwise.
 TYPE (vector), INTENT(OUT)       :: HitPt   ! Point that ray along RN intersects plane of surface

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)        :: CPhit(3)   ! Point that ray along RN intersects plane of surface
  REAL(r64)        :: R1(3)      ! Point from which ray originates
  REAL(r64)        :: RN(3)      ! Unit vector along in direction of ray whose
  REAL(r64) :: V1(3)                    ! First vertex
  REAL(r64) :: V2(3)                    ! Second vertex
  REAL(r64) :: V3(3)                    ! Third vertex
  INTEGER   :: NV                       ! Number of vertices (3 or 4)
  REAL(r64) :: A1(3)                    ! Vector from vertex 1 to 2
  REAL(r64) :: A2(3)                    ! Vector from vertex 2 to 3
  REAL(r64) :: AXC(3)                   ! Cross product of A and C
  REAL(r64) :: SN(3)                    ! Vector normal to surface (SN = A1 X A2)
  REAL(r64) :: AA(3)                    ! AA(I) = A(N,I)
  REAL(r64) :: CC(3)                    ! CC(I) = C(N,I)
  REAL(r64) :: CCC(3)                   ! Vector from vertex 2 to CP
  REAL(r64) :: AAA(3)                   ! Vector from vertex 2 to vertex 1
  REAL(r64) :: BBB(3)                   ! Vector from vertex 2 to vertex 3
  INTEGER   :: N                        ! Vertex loop index
  INTEGER   :: I                        ! Vertext-to-vertex index
  REAL(r64) :: F1,F2                    ! Intermediate variables
  REAL(r64) :: SCALE                    ! Scale factor
  REAL(r64) :: DOTCB                    ! Dot product of vectors CCC and BBB
  REAL(r64) :: DOTCA                    ! Dot product of vectors CCC and AAA
  REAL(r64) :: DOTAXCSN                 ! Dot product of vectors AXC and SN

!  REAL(r64)      :: V(4,3)                   ! Vertices of surfaces
!  REAL(r64)      :: A(4,3)                   ! Vertex-to-vertex vectors; A(1,i) is from vertex 1 to 2, etc.
!  REAL(r64)      :: C(4,3)                   ! Vectors from vertices to intersection point
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:,:) :: V     ! Vertices of surfaces
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:,:) :: A     ! Vertex-to-vertex vectors; A(1,i) is from vertex 1 to 2, etc.
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:,:) :: C     ! Vectors from vertices to intersection point
  LOGICAL,SAVE :: FirstTime=.true.

          ! FLOW:
  IPIERC = 0
  R1 = Orig
  RN = Dir
  ! Vertex vectors
  IF (FirstTime) THEN
    ALLOCATE(V(MaxVerticesPerSurface,3))
    V=0.0d0
    ALLOCATE(A(MaxVerticesPerSurface,3))
    A=0.0d0
    ALLOCATE(C(MaxVerticesPerSurface,3))
    C=0.0d0
    FirstTime=.false.
  ENDIF

  NV = Surface(ISurf)%Sides
  DO N = 1,NV
    V(N,1) = Surface(ISurf)%Vertex(N)%X
    V(N,2) = Surface(ISurf)%Vertex(N)%Y
    V(N,3) = Surface(ISurf)%Vertex(N)%Z
  END DO

  ! Vertex-to-vertex vectors. A(1,2) is from vertex 1 to 2, etc.
  DO I = 1,3
    DO N = 1,NV-1
      A(N,I) = V(N+1,I) - V(N,I)
    END DO
    A(NV,I) = V(1,I) - V(NV,I)
    A1(I) = A(1,I)
    A2(I) = A(2,I)
    V1(I) = V(1,I)
    V2(I) = V(2,I)
    V3(I) = V(3,I)
  END DO

  ! Vector normal to surface
  CALL CrossProduct(A1, A2, SN)
  ! Scale factor, the solution of SN.(CPhit-V2) = 0 and
  ! CPhit = R1 + SCALE*RN, where CPhit is the point that RN,
  ! when extended, intersects the plane of the surface.
  F1 = DOT_PRODUCT(SN, V2 - R1)
  F2 = DOT_PRODUCT(SN, RN)
  ! Skip surfaces that are parallel to RN
  IF (ABS(F2) < 0.01d0) RETURN
  SCALE = F1 / F2
  ! Skip surfaces that RN points away from
  IF (SCALE <= 0.0d0) RETURN
  ! Point that RN intersects plane of surface
  CPhit = R1 + RN * SCALE
  HitPt = CPhit
  ! Vector from vertex 2 to CPhit
  CCC = CPhit - V2

  ! Two cases: rectangle and non-rectangle; do rectangle
  ! first since most common shape and faster calculation
  IF (Surface(ISurf)%Shape == Rectangle .OR. Surface(ISurf)%Shape == RectangularDoorWindow .or.         &
      Surface(ISurf)%Shape == RectangularOverhang .OR. Surface(ISurf)%Shape == RectangularLeftFin .or.  &
      Surface(ISurf)%Shape == RectangularRightFin) THEN
    !
    ! Surface is rectangular
    !
    ! Vectors from vertex 2 to vertex 1 and vertex 2 to vertex 3
    AAA = V1 - V2
    BBB = V3 - V2
    ! Intersection point, CCC, is inside rectangle if
    ! 0 < CCC.BBB < BBB.BBB AND 0 < CCC.AAA < AAA.AAA
    DOTCB = DOT_PRODUCT(CCC, BBB)
    IF (DOTCB < 0.0d0) RETURN
    IF (DOTCB > DOT_PRODUCT(BBB,BBB)) RETURN
    DOTCA = DOT_PRODUCT(CCC, AAA)
    IF (DOTCA < 0.0d0) RETURN
    IF (DOTCA > DOT_PRODUCT(AAA,AAA)) RETURN
    ! Surface is intersected
    IPIERC = 1
  ELSE
    !
    ! Surface is not rectangular
    !
    ! Vectors from surface vertices to CPhit
    DO N = 1,NV
      DO I=1,3
        C(N,I) = CPhit(I) - V(N,I)
      END DO
    END DO
    ! Cross products of vertex-to-vertex vectors and
    ! vertex-to-CPhit vectors
    DO N = 1,NV
      DO I=1,3
        AA(I) = A(N,I)
        CC(I) = C(N,I)
      END DO
      CALL CrossProduct(AA,CC,AXC)
      DOTAXCSN = DOT_PRODUCT(AXC,SN)
       ! If at least one of these dot products is negative
       ! intersection point is outside of surface
      IF (DOTAXCSN < 0.0d0) RETURN
    END DO
    ! Surface is intersected
    IPIERC = 1
  END IF

  RETURN

END SUBROUTINE PierceSurfaceVector


!     NOTICE
!
!     Copyright  1996-2013 The Board of Trustees of the University of Illinois
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

END MODULE WindowComplexManager
