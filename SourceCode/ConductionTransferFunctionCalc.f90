MODULE ConductionTransferFunctionCalc

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   January 1997
          !       MODIFIED       June-July 2000, RKS
          !       RE-ENGINEERED  June 1996, February 1997, August 1997, RKS
          !       RE-ENGINEERED  November 1999, LKL

          ! PURPOSE OF THIS MODULE:
          ! This module calculates the conduction transfer functions (CTFs) for
          ! all building constructions.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine uses the state space method of calculating CTFs.
          ! The state space method involves imposing a finite difference grid
          ! to a solution space (i.e., a building construction, inside to
          ! outside surface).  The finite difference grid is only used to
          ! derive a system of differential equations.  This first order
          ! system can then be solved using matrix algebra.  In this
          ! implementation of the state space method, a conversion from the
          ! internal units of EnergyPlus (SI) to English units is used, This
          ! is done due to observations made by Russ Taylor that the solution
          ! method was not as stable numerically when SI units were used.

          ! REFERENCES:
          ! While there are several important references on the state space
          ! method, the most definitive reference which includes details on
          ! implementing the state space method is:
          ! Seem, J.E.  1987.  Modeling of Heat Transfer in Buildings, Ph.D.
          ! Dissertation, Department of Mechanical Engineering, University of
          ! Wisconsin-Madison.

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHeatBalance ! This is the Heat balance super block data-only module
USE DataInterfaces


IMPLICIT NONE    ! Enforce explicit typing of all variables

PRIVATE
          ! MODULE PARAMETER DEFINITIONS:
!INTEGER, PRIVATE, PARAMETER :: MaxTotNodes = 75   ! Maximum total number of
          ! nodes per construction.  This limit is a compromise between faster,
          ! less accurate solutions and slower, possibly more accurate answers.

INTEGER, PARAMETER :: NumOfPerpendNodes = 7     ! Number of nodes in the direction
          ! perpendicular to the main direction of heat transfer.  This is only used
          ! when a two-dimensional solution has been requested for a construction
          ! with a heat source/sink.

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:)        :: AExp   ! Exponential of AMat
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:)        :: AInv   ! Inverse of AMat
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:)        :: AMat   ! "A" matrix from Seem's dissertation
                                                                        ! (constant coefficients of linear system)
REAL(r64), PRIVATE, DIMENSION(3)                       :: BMat   ! "B" matrix of state space method (non-zero elements)
REAL(r64), PRIVATE, DIMENSION(2)                       :: CMat   ! "C" matrix of state space method (non-zero elements)
REAL(r64), PRIVATE, DIMENSION(2)                       :: DMat   ! "D" matrix of state space method (non-zero elements)
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:)          :: e      ! Coefficients for the surface flux history term
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:)        :: Gamma1 ! Intermediate calculation array corresponding to a term
                                                                        ! in Seem's dissertation
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:)        :: Gamma2 ! Intermediate calculation array corresponding to a term
                                                                        ! in Seem's dissertation
INTEGER,   PRIVATE                                     :: NodeSource ! Node at which a source or sink is present
INTEGER,   PRIVATE                                     :: NodeUserTemp ! Node where user wishes to calculate a temperature
                                                                              ! (for constructions with sources/sinks only)
INTEGER,   PRIVATE                                     :: rcmax  ! Total number of nodes in the construct (<= MaxTotNodes)
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:,:)      :: s      ! Coefficients for the surface temperature history terms
REAL(r64), PRIVATE, DIMENSION(4,3)                     :: s0     ! Coefficients for the current surface temperature terms
REAL(r64), PRIVATE                                     :: TinyLimit
REAL(r64), PRIVATE, ALLOCATABLE, DIMENSION(:,:)        :: IdenMatrix  ! Identity Matrix

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ConductionTransferFunctionCalc

PUBLIC  InitConductionTransferFunctions
PRIVATE CalculateExponentialMatrix
PRIVATE CalculateInverseMatrix
PRIVATE CalculateGammas
PRIVATE CalculateCTFs
PRIVATE ReportCTFs

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE InitConductionTransferFunctions

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1990
          !       MODIFIED       July 1994, LKL, cosmetic and improve execution time
          !                      Dec 1995, Apr 1996, RKS, cosmetic and clean-up changes, changes to allow proper
          !                       handling of resistive layers
          !                      June 2000, RKS, addition of QTFs (both 1- and 2-D solutions for constructions
          !                       with embedded/internal heat sources/sinks)
          !                      July 2010-August 2011, RKS, R-value only layer enhancement
          !       RE-ENGINEERED  June 1996, February 1997, August-October 1997, RKS; Nov 1999, LKL

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine serves as the main drive for the
          ! calculation of Conduction Transfer Functions (CTFs)
          ! using the state space method.

          ! METHODOLOGY EMPLOYED:
          ! The basic steps of this routine (which may be a little difficult
          ! to decipher until another major revision is done) are:
          !   1. Determine if enough material info has been entered
          !   2. Determine whether construct is (a) all resistive,
          !      (b) the reverse of a previously calculated construct, or
          !      (c) neither (a) nor (b), i.e. a layer for which CTFs must
          !      be calculated.
          !   3. If the answer to 2 is (a), calculate the overall resistance
          !      and use this as the CTF (steady state conduction).
          !   4. If the answer to 2 is (b), transfer the CTFs for the reverse
          !      construction to the CTF arrays for this construct (reversing
          !      the inside and outside terms).
          !   5. If the answer to 2 is (c), calculate the CTFs using the state
          !      space method described below.
          ! The state space method of calculating CTFs involves
          ! applying a finite difference grid to a multilayered
          ! building element and performing linear algebra on the
          ! resulting system of equations (in matrix form).
          ! CTFs must be calculated for non-reversed layers which
          ! have an appreciable thermal mass.  A conversion from
          ! SI units to English units is made due to concerns
          ! about round off problems noted in earlier version of
          ! this subroutine.

          ! REFERENCES:
          ! Seem, J.E.  "Modeling of Heat Transfer in Buildings",
          !  Department of Mechanical Engineering, University of
          !  Wisconsin-Madison, 1987.
          ! Strand, R.K. "Testing Design Description for the CTF
          !  Calculation Code in BEST", BSO internal document,
          !  May/June 1996.
          ! Strand, R.K. "Heat Source Transfer Functions and Their
          !  Applicatoin to Low Temperature Radiant Heating System",
          !  Ph.D. Dissertation, Department of Mechanical and
          !  Industrial Engineering, University of Illinois at
          !  Urbana-Champaign, 1995.

          ! USE STATEMENTS:
  USE DataConversions
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: PhysPropLimit = 1.0d-6  ! Physical properties limit.
          ! This is more or less the traditional value from BLAST.

  REAL(r64), PARAMETER :: RValueLowLimit = 1.0d-3  ! Physical properties limit for R-value only layers
          ! This value was based on trial and error related to CR 7791 where a
          ! user had entered a "no insulation" layer with an R-value of 1.0E-05.
          ! Some trial and error established this as a potential value though
          ! there is no guarantee that this is a good value.

  INTEGER, PARAMETER :: MinNodes = 6 ! Minimum number of state space nodes
          ! per layer.  This value was chosen based on experience with IBLAST.

  REAL(r64), PARAMETER :: MaxAllowedCTFSumError = 0.01d0 ! Allow a 1 percent
          ! difference between the CTF series summations.  If the difference is
          ! greater than this, then the coefficients will not yield a valid steady
          ! state solution.

  REAL(r64), PARAMETER :: MaxAllowedTimeStep = 4.0d0   ! Sets the maximum allowed time step
          ! for CTF calculations to be 4 hours.  This is done in response to some
          ! rare situations where odd or faulty input will cause the routine to
          ! go off and get some huge time step (in excess of 20 hours).  This value
          ! is a compromise that does not really solve any input problems.  One run
          ! indicated that 2 meters of concrete will result in a time step of slightly
          ! more than 3 hours.  So, 4 hours was arbitrarily picked as a ceiling for
          ! time steps so that an error message can be produced to warn the user
          ! that something isn't right.  Note that the 4 hour limit does not guarantee
          ! that problems won't exist and it does not necessarily avoid any problems
          ! that interpolated temperature histories might cause.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, &
    DIMENSION(MaxLayersInConstruct) :: AdjacentResLayerNum ! Layers that are adjacent to each other which are resistive
                                                           ! only can and should be combined
  INTEGER                           :: AdjLayer   ! Loop counter for adjacent resistance-only layers
  REAL(r64)             :: amatx      ! Intermediate calculation variable
  REAL(r64)             :: amatxx     ! Intermediate calculation variable
  REAL(r64)             :: amaty      ! Intermediate calculation variable
  REAL(r64)             :: BiggestSum ! Largest CTF series summation (maximum of SumXi, SumYi, and SumZi)
  REAL(r64)             :: cap        ! Thermal capacitance of a node (intermediate calculation)
  REAL(r64)             :: capavg     ! Thermal capacitance of a node (average value for a node at an interface)
  REAL(r64)             :: cnd        ! Total thermal conductance (1/Rtot) of the bldg element
  INTEGER                           :: Constr     ! Loop counter
  INTEGER                           :: ConstrNum  ! Loop counter (construct number)
  REAL(r64), &
    DIMENSION(MaxLayersInConstruct) :: cp         ! Specific heat of a material layer
  LOGICAL                           :: CTFConvrg  ! Set after CTFs are calculated, based on whether there are too
                                                  ! many CTF terms
  INTEGER                           :: CurrentLayer ! Pointer to material number in Material derived type (current layer)
  REAL(r64), &
    DIMENSION(MaxLayersInConstruct) :: dl         ! Thickness of a material layer
  REAL(r64)             :: dtn        ! Intermediate calculation of the time step
  REAL(r64), &
    DIMENSION(MaxLayersInConstruct) :: dx         ! Distance between nodes in a particular material layer
  REAL(r64)             :: dxn        ! Intermediate calculation of nodal spacing
  REAL(r64)             :: dxtmp      ! Intermediate calculation variable ( = 1/dx/cap)
  REAL(r64)             :: dyn        ! Nodal spacing in the direction perpendicular to the main direction
                                                  ! of heat transfer (only valid for a 2-D solution)
  LOGICAL                           :: ErrorsFound=.false.  !Flag for input error condition
  INTEGER                           :: HistTerm   ! Loop counter
  INTEGER                           :: ipts1      ! Intermediate calculation for number of nodes per layer
  INTEGER                           :: ir         ! Loop control for constructing Identity Matrix
  INTEGER                           :: Layer      ! Loop counter
  INTEGER                           :: Layer1     ! Loop counter
  INTEGER                           :: LayersInConstruct ! Array containing the number of layers for each construct
                                                  ! Different from TotLayers because shades are not include in local var
  REAL(r64), &
    DIMENSION(MaxLayersInConstruct) :: lr         ! R value of a material layer
  INTEGER                           :: Node       ! Loop counter
  INTEGER                           :: Node2      ! Node number (modification of Node and NodeInRow)
  INTEGER                           :: NodeInLayer ! Loop counter
  INTEGER                           :: NodeInRow  ! Loop counter
  INTEGER, &
    DIMENSION(MaxLayersInConstruct) :: Nodes      ! Array containing the number of nodes per layer
  INTEGER                           :: NumResLayers ! Number of resistive layers in the construction
                                                  ! property allowable, traditional value from BLAST
  INTEGER                           :: NumAdjResLayers ! Number of resistive layers that are adjacent
  INTEGER                           :: OppositeLayer ! Used for comparing constructions (to see if one is the reverse of another)
  LOGICAL, &
    DIMENSION(MaxLayersInConstruct) :: ResLayer   ! Set true if the layer must be handled as a resistive
  LOGICAL                           :: RevConst   ! Set true if one construct is the reverse of another (CTFs already
                                                  ! available)
  REAL(r64), &
    DIMENSION(MaxLayersInConstruct) :: rho        ! Density of a material layer
  REAL(r64), &
    DIMENSION(MaxLayersInConstruct) :: rk         ! Thermal conductivity of a material layer
  REAL(r64)             :: rs         ! Total thermal resistance of the building element
  REAL(r64)             :: SumXi      ! Summation of all of the Xi terms (inside CTFs) for a construction
  REAL(r64)             :: SumYi      ! Summation of all of the Xi terms (cross CTFs) for a construction
  REAL(r64)             :: SumZi      ! Summation of all of the Xi terms (outside CTFs) for a construction
  LOGICAL :: DoCTFErrorReport
  REAL(r64) :: Alpha ! thermal diffusivity in m2/s, for local check of properties
  REAL(r64) :: DeltaTimestep      ! zone timestep in seconds, for local check of properties
  REAL(r64) :: ThicknessThreshold ! min thickness consistent with other thermal properties, for local check

          ! FLOW:
          ! Subroutine initializations
  TinyLimit=rTinyValue
  DoCTFErrorReport=.false.

  DO ConstrNum = 1, TotConstructs   ! Begin construction loop ...

    Construct(ConstrNum)%CTFCross       = 0.0D0
    Construct(ConstrNum)%CTFFlux        = 0.0D0
    Construct(ConstrNum)%CTFInside      = 0.0D0
    Construct(ConstrNum)%CTFOutside     = 0.0D0
    Construct(ConstrNum)%CTFSourceIn    = 0.0D0
    Construct(ConstrNum)%CTFSourceOut   = 0.0D0
    Construct(ConstrNum)%CTFTimeStep    = 0.0D0
    Construct(ConstrNum)%CTFTSourceOut  = 0.0D0
    Construct(ConstrNum)%CTFTSourceIn   = 0.0D0
    Construct(ConstrNum)%CTFTSourceQ    = 0.0D0
    Construct(ConstrNum)%CTFTUserOut    = 0.0D0
    Construct(ConstrNum)%CTFTUserIn     = 0.0D0
    Construct(ConstrNum)%CTFTUserSource = 0.0D0
    Construct(ConstrNum)%NumHistories   = 0
    Construct(ConstrNum)%NumCTFTerms    = 0
    Construct(ConstrNum)%UValue         = 0.0d0

    AdjacentResLayerNum = 0 ! Zero this out for each construct

    IF(Construct(ConstrNum)%TypeIsWindow) CYCLE

          ! Initialize construct parameters

    Construct(ConstrNum)%CTFTimeStep = TimeStepZone
    rs = 0.0D0
    LayersInConstruct = 0
    NumResLayers = 0
    ResLayer     = .FALSE.

    DO Layer = 1, Construct(ConstrNum)%TotLayers    ! Begin layer loop ...

          ! Loop through all of the layers in the current construct. The purpose
          ! of this loop is to define the thermal properties necessary to
          ! calculate the CTFs.

      CurrentLayer = Construct(ConstrNum)%LayerPoint(Layer)

        LayersInConstruct = LayersInConstruct + 1

          ! Obtain thermal properties from the Material derived type

        dl(Layer)  = Material(CurrentLayer)%Thickness
        rk(Layer)  = Material(CurrentLayer)%Conductivity
        rho(Layer) = Material(CurrentLayer)%Density
        cp(Layer)  = Material(CurrentLayer)%SpecHeat ! Must convert
                                        ! from kJ/kg-K to J/kg-k due to rk units

        IF (Construct(ConstrNum)%SourceSinkPresent .AND. .Not. Material(CurrentLayer)%WarnedForHighDiffusivity) THEN
          ! check for materials that are too conductive or thin
          IF ((rho(Layer) *  cp(Layer)) > 0.d0) THEN
            Alpha = rk(Layer) / (rho(Layer) *  cp(Layer))
            IF (Alpha > HighDiffusivityThreshold ) THEN
              DeltaTimestep      = TimeStepZone * SecInHour
              ThicknessThreshold = SQRT(Alpha * DeltaTimestep * 3.d0)
              IF (Material(CurrentLayer)%Thickness < ThicknessThreshold) THEN
                CALL ShowSevereError('InitConductionTransferFunctions: Found Material that is too thin and/or too ' &
                                       //'highly conductive, material name = '  // TRIM(Material(CurrentLayer)%Name))
                CALL ShowContinueError('High conductivity Material layers are not well supported for internal source ' &
                                        //'constructions, material conductivity = ' &
                                        //TRIM(RoundSigDigits(Material(CurrentLayer)%Conductivity, 3)) //' [W/m-K]')
                CALL ShowContinueError('Material thermal diffusivity = ' //TRIM(RoundSigDigits(Alpha, 3)) //' [m2/s]')
                CALL ShowContinueError('Material with this thermal diffusivity should have thickness > ' &
                                      //  TRIM(RoundSigDigits(ThicknessThreshold , 5)) // ' [m]')
                IF (Material(CurrentLayer)%Thickness < ThinMaterialLayerThreshold) THEN
                  CALL ShowContinueError('Material may be too thin to be modeled well, thickness = ' &
                                          // TRIM(RoundSigDigits(Material(currentLayer)%Thickness, 5 ))//' [m]')
                  CALL ShowContinueError('Material with this thermal diffusivity should have thickness > ' &
                                      //  TRIM(RoundSigDigits(ThinMaterialLayerThreshold , 5)) // ' [m]')
                ENDIF
                Material(CurrentLayer)%WarnedForHighDiffusivity = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF (rk(Layer) <= PhysPropLimit) THEN  ! Thermal conductivity too small,
                            ! thus this must be handled as a resistive layer

          ResLayer(Layer) = .TRUE.

        ELSE

          lr(Layer) = dl(Layer)/rk(Layer)
          IF ((dl(Layer)*SQRT(rho(Layer)*cp(Layer)/rk(Layer))) &
                                             < PhysPropLimit) THEN

          ! If the time constant is smaller than some reasonable
          ! limit, this should be treated as a resistive layer.

            ResLayer(Layer) = .TRUE.

          ELSE      ! Layer has significant thermal mass (non-resistive)

            ResLayer(Layer) = .FALSE.

          END IF
        END IF

          ! If not a resistive layer, nothing further is required
          ! for this layer.

        IF (ResLayer(Layer)) THEN  ! Resistive layer-check for R-value, etc.
          NumResLayers = NumResLayers + 1 ! Increment number of resistive layers
          lr(Layer) = Material(CurrentLayer)%Resistance    ! User defined thermal resistivity
          IF (lr(Layer) < RValueLowLimit) THEN  ! User didn't define enough
                    ! parameters to calculate CTFs for a building element
                    ! containing this layer.

            CALL ShowSevereError('InitConductionTransferFunctions: Material='//TRIM(Material(CurrentLayer)%Name)//  &
               'R Value below lowest allowed value')
            CALL ShowContinueError('Lowest allowed value=['//trim(RoundSigDigits(RValueLowLimit,3))//  &
                          '], Material R Value=['//trim(RoundSigDigits(lr(Layer),3))//'].')
            ErrorsFound=.true.

          ELSE      ! A valid user defined R-value is available.
                    ! If this is either the first or last layer in the construction,
                    ! then assign other properties based on air at 1 atm, 300K.
                    ! Reference for air properties:  Incropera and DeWitt,
                    ! Introduction to Heat Transfer, Appendix A, Table A.4,
                    ! John Wiley & Sons, New York, 1985.
                    ! If this is not the first or last layer in the construction,
                    ! then use the "exact" approach to model a massless layer
                    ! based on the node equations for the state space method.

            IF ( (Layer == 1) .OR. (Layer == Construct(ConstrNum)%TotLayers) .OR. &
                 (.NOT. Material(Construct(ConstrNum)%LayerPoint(Layer))%ROnly) ) THEN
              cp(Layer)  = 1.007d0
              rho(Layer) = 1.1614d0
              rk(Layer)  = 0.0263d0
              dl(Layer)  = rk(Layer)*lr(Layer)
            ELSE
              cp(Layer)  = 0.0d0
              rho(Layer) = 0.0d0
              rk(Layer)  = 1.0d0
              dl(Layer)  = lr(Layer)
            END IF

          END IF
        END IF      ! ... end of resistive layer determination IF-THEN block.
    END DO          ! ... end of layer loop.

    ! If errors have been found, just cycle

    IF (ErrorsFound) CYCLE

          ! Combine any adjacent resistive-only (no mass) layers together
          ! to avoid a divide by zero error in the CTF calculations below.
          ! Since the inner and outer layers cannot be resistive layers
          ! (inner and outer layer still converted to equivalent air layer)
          ! there can only be resistive layers adjacent to one another if
          ! there are more than three total layers and more than one
          ! resistive layer.
    IF ((LayersInConstruct > 3).AND.(NumResLayers > 1)) THEN
      NumAdjResLayers = 0
      DO Layer = 2, LayersInConstruct-2
        IF ( (ResLayer(Layer)) .AND. (ResLayer(Layer+1)) ) THEN
          NumAdjResLayers = NumAdjResLayers + 1
          ! There is method to the next assignment statement.  As the layers get shifted, the layer
          ! numbers will also shift.  Thus, we have to also shift which layer we are dealing with.
          AdjacentResLayerNum(NumAdjResLayers) = Layer + 1 - NumAdjResLayers
        END IF
      END DO
      DO AdjLayer = 1, NumAdjResLayers
        Layer = AdjacentResLayerNum(AdjLayer)
          ! Double check to make sure we are in the right place...
        IF ( (ResLayer(Layer)) .AND. (ResLayer(Layer+1)) ) THEN
            ! Shift layers forward after combining two adjacent layers.  Then
            ! restart the do loop.
          cp(Layer)  = 0.0d0
          rho(Layer) = 0.0d0
          rk(Layer)  = 1.0d0
          lr(Layer)  = lr(Layer) + lr(Layer+1)
          dl(Layer)  = lr(Layer)
          NumResLayers = NumResLayers - 1 ! Combining layers so decrease number of resistive layers
          DO Layer1 = Layer+1, LayersInConstruct - 1
            lr(Layer1)  = lr(Layer1+1)
            dl(Layer1)  = dl(Layer1+1)
            rk(Layer1)  = rk(Layer1+1)
            rho(Layer1) = rho(Layer1+1)
            cp(Layer1)  = cp(Layer1+1)
            ResLayer(Layer1) = ResLayer(Layer1+1)
          END DO
            ! Then zero out the layer that got shifted forward
          cp(LayersInConstruct)  = 0.0d0
          rho(LayersInConstruct) = 0.0d0
          rk(LayersInConstruct)  = 0.0d0
          lr(LayersInConstruct)  = 0.0d0
          dl(LayersInConstruct)  = 0.0d0
            ! Now reduce the number of layers in construct since merger is complete
          LayersInConstruct = LayersInConstruct - 1
            ! Also adjust layers with source/sinks if two layers are merged
          IF (Construct(ConstrNum)%SourceSinkPresent) THEN
            Construct(ConstrNum)%SourceAfterLayer = Construct(ConstrNum)%SourceAfterLayer - 1
            Construct(ConstrNum)%TempAfterLayer   = Construct(ConstrNum)%TempAfterLayer - 1
          END IF
        ELSE ! These are not adjacent layers and there is a logic flaw here (should not happen)
          CALL ShowFatalError('Combining resistance layers failed for '//TRIM(Construct(ConstrNum)%Name))
          CALL ShowContinueError('This should never happen.  Contact EnergyPlus Support for further assistance.')
        END IF
      END DO
    END IF

          ! Convert SI units to English.  In theory, conversion to English
          ! units is not necessary; however, Russ Taylor noted that some
          ! numerical problems when SI units were used and decided to continue
          ! calculating CTFs in English units.

    DO Layer = 1,LayersInConstruct  ! Begin units conversion loop ...

      lr(Layer)  = lr(Layer)*CFU
      dl(Layer)  = dl(Layer)/CFL
      rk(Layer)  = rk(Layer)/CFK
      rho(Layer) = rho(Layer)/CFD
      cp(Layer)  = cp(Layer)/(CFC*1000.0d0)

    END DO          ! ... end of layer loop for units conversion.

    IF (Construct(ConstrNum)%SolutionDimensions == 1) THEN
      dyn = 0.0d0
    ELSE
      dyn = (Construct(ConstrNum)%ThicknessPerpend/CFL)/REAL(NumOfPerpendNodes-1,r64)
    END IF

          ! Compute total construct conductivity and resistivity.

    DO Layer = 1,LayersInConstruct
      rs = rs + lr(Layer)   ! Resistances in series sum algebraically
    END DO

    cnd = 1.0D0/rs      ! Conductivity is the inverse of resistivity

    IF (LayersInConstruct > NumResLayers) THEN

          ! One or more are not simple resistive layers so CTFs will have to be
          ! calculated unless this is a reverse of a previously defined
          ! construction.

          ! Check for reversed construction of interzone surfaces by checking
          ! previous constructions for same number of layers as first indicator.

      RevConst = .FALSE.

      DO Constr = 1, ConstrNum-1       ! Constructions loop (check reversed) ...

          ! If a source or sink is present in this construction, do not allow any
          ! checks for reversed constructions, i.e., always force EnergyPlus to
          ! calculate CTF/QTFs.  So, don't even check for reversed constructions.
        IF (Construct(ConstrNum)%SourceSinkPresent) EXIT ! Constr DO loop

        IF (Construct(ConstrNum)%TotLayers == & ! Same number of layers--now
               Construct(Constr)%TotLayers) THEN  ! check for reversed construct.

          RevConst = .TRUE.

          DO Layer = 1, Construct(ConstrNum)%TotLayers  ! Begin layers loop ...

          ! RevConst is set to FALSE anytime a mismatch in materials is found.
          ! This will exit this DO immediately and go on to the next construct
          ! (if any remain).

            OppositeLayer = Construct(ConstrNum)%TotLayers-Layer+1

            IF (Construct(ConstrNum)%LayerPoint(Layer) /=  &
                Construct(Constr)%LayerPoint(OppositeLayer)) THEN

                RevConst = .FALSE.
                EXIT ! Layer DO loop

            END IF

          END DO    ! ... end of layers loop.

          IF (RevConst) THEN        ! Curent construction is a reverse of
                    ! construction Constr.  Thus, CTFs do not need to be re-
                    ! calculated.  Copy CTF info for construction Constr to
                    ! construction ConstrNum.

            Construct(ConstrNum)%CTFTimeStep  = Construct(Constr)%CTFTimeStep
            Construct(ConstrNum)%NumHistories = Construct(Constr)%NumHistories
            Construct(ConstrNum)%NumCTFTerms  = Construct(Constr)%NumCTFTerms

          ! Transfer the temperature and flux history terms to CTF arrays.
          ! Loop through the number of CTF history terms ...
            DO HistTerm = 0, Construct(ConstrNum)%NumCTFTerms

              Construct(ConstrNum)%CTFInside(HistTerm)  = Construct(Constr)%CTFOutside(HistTerm)
              Construct(ConstrNum)%CTFCross(HistTerm)   = Construct(Constr)%CTFCross(HistTerm)
              Construct(ConstrNum)%CTFOutside(HistTerm) = Construct(Constr)%CTFInside(HistTerm)
              IF (HistTerm /= 0) &
                 Construct(ConstrNum)%CTFFlux(HistTerm) = Construct(Constr)%CTFFlux(HistTerm)

            END DO  ! ... end of CTF history terms loop.

            EXIT ! Constr DO loop

          END IF    ! ... end of reversed construction found block

        END IF      ! ... end of reversed construct (same number of layers) block.

      END DO        ! ... end of construct loop (check reversed--Constr)

      IF (.NOT.RevConst) THEN       ! Calculate CTFs (non-reversed constr)

          ! Estimate number of nodes each layer of the construct will require
          ! and calculate the nodal spacing from that

        DO Layer = 1, LayersInConstruct ! Begin loop thru layers ...

          ! The calculation of dxn used here is based on a standard stability
          ! criteria for explicit finite difference solutions.  This criteria
          ! was chosen not because it is viewed to be correct, but rather for
          ! lack of any better criteria at this time.  The use of a Fourier
          ! number based criteria such as this is probably physically correct,
          ! though the coefficient (2.0) may not be.

          ! If this is a "resistive" layer, only need a single node
          IF ( (ResLayer(Layer)) .AND. (Layer>1) .AND. (Layer<LayersInConstruct) ) THEN
            Nodes(Layer) = 1
            dx(Layer)    = dl(Layer)
          ELSE
            dxn = sqrt(2.0d0*(rk(Layer)/rho(Layer)/cp(Layer)) &
                        *Construct(ConstrNum)%CTFTimeStep)

            ipts1 = int(dl(Layer)/dxn)  ! number of nodes=thickness/spacing

          ! Limit the upper and lower bounds of the number of
          ! nodes to MaxCTFTerms and MinNodes respectively.

            IF (ipts1 > MaxCTFTerms) THEN    ! Too many nodes
              Nodes(Layer) = MaxCTFTerms
            ELSE IF (ipts1 < MinNodes) THEN  ! Too few nodes
              Nodes(Layer) = MinNodes
            ELSE                              ! Calculated number of nodes ok
              Nodes(Layer) = ipts1
            END IF

            IF (Construct(ConstrNum)%SolutionDimensions > 1) THEN
              IF (ipts1 > MaxCTFTerms/2) ipts1 = MaxCTFTerms/2
            END IF

            dx(Layer) = dl(Layer)/REAL(Nodes(Layer),r64) ! calc node spacing

          END IF

        END DO      ! . .. end of layers in construction loop (calculating #nodes per layer)

          ! Determine the total number of nodes (rcmax)

        rcmax = 0
        DO Layer = 1, LayersInConstruct
          rcmax = rcmax + Nodes(Layer)
        END DO

          ! Nodes are placed throughout layers and at the interface between
          ! layers.  As a result, the end layers share a node with the adjacent
          ! layer-leaving one less node total for all layers.

        rcmax = rcmax-1
        IF (Construct(ConstrNum)%SolutionDimensions > 1) rcmax = rcmax * NumOfPerpendNodes

! This section no longer needed as rcmax/number of total nodes is allowed to float.
! If reinstated, this node reduction section would have to be modified to account for
! the possibility that a 2-D solution is potentially being performed.
          ! Check to see if the maximum number of nodes for the construct has
          ! been exceeded.  Reduce the nodes per layer if necessary, but only
          ! if the number of nodes in a particular layer is greater than the
          ! minimum node limit.

!        DO WHILE (rcmax > MaxTotNodes)     ! Begin total node reduction loop ...

!          rcmax = 0

!          DO Layer = 1, LayersInConstruct   ! Begin layer node reduction ...

!          ! If more nodes than the minimum limit for a layer, reduce the
!          ! number of nodes.

!            IF (Nodes(Layer) > MinNodes) THEN
!              Nodes(Layer) = Nodes(Layer)-1
!              dx(Layer) = dl(Layer)/dble(float(Nodes(Layer))) ! Recalc node spacing
!            END IF

!            rcmax = rcmax + Nodes(Layer) ! Recalculate total number of nodes

!          END DO        ! ... end of layer loop for node reduction.

!          rcmax = rcmax-1 ! See note above on counting rcmax

!        END DO      ! ... end of total node reduction loop.

          ! For constructions that have sources or sinks present, determine which
          ! node the source/sink is applied at and also where the temperature
          ! calculation has been requested.
        NodeSource   = 0
        NodeUserTemp = 0
        IF (Construct(ConstrNum)%SourceSinkPresent) THEN

          DO Layer = 1, Construct(ConstrNum)%SourceAfterLayer
            NodeSource = NodeSource + Nodes(Layer)
          END DO
          IF ( (NodeSource > 0) .AND. (Construct(ConstrNum)%SolutionDimensions > 1) ) &
            NodeSource = ((NodeSource-1)*NumOfPerpendNodes) + 1

          DO Layer = 1, Construct(ConstrNum)%TempAfterLayer
            NodeUserTemp = NodeUserTemp + Nodes(Layer)
          END DO
          IF ( (NodeUserTemp > 0) .AND. (Construct(ConstrNum)%SolutionDimensions > 1) ) &
            NodeUserTemp = ((NodeUserTemp-1)*NumOfPerpendNodes) + 1

        END IF

          ! "Adjust time step to ensure stability."  If the time step is too
          ! small, it will result in too many history terms which can lead to
          ! solution instability.  The method used here to determine whether or
          ! not the time step will produce a stable solution is based on a pure
          ! Fourier number calculation (Fo = 1) and has not proven to be
          ! completely effective.  If too many history terms are calculated,
          ! the time step is adjusted and the CTFs end up being recalculated
          ! (see later code in this routine).

        dtn = 0.0D0
        Construct(ConstrNum)%CTFTimeStep = 0.0D0
        DO Layer = 1, LayersInConstruct
          IF (Nodes(Layer) >= MaxCTFTerms) THEN
            IF (Construct(ConstrNum)%SolutionDimensions == 1) THEN
              dtn = rho(Layer)*cp(Layer)*dx(Layer)**2/rk(Layer)
            ELSE    ! 2-D solution requested-->this changes length parameter in Fourier number calculation
              dtn = rho(Layer)*cp(Layer)*((dx(Layer)**2)+(dyn**2))/rk(Layer)
            END IF
            IF (dtn > Construct(ConstrNum)%CTFTimeStep) &
                       Construct(ConstrNum)%CTFTimeStep = dtn
          END IF
        END DO

          ! If the user defined time step is significantly different than the
          ! calculated time step for this construct, then CTFTimeStep must be
          ! revised.

        IF (ABS((TimeStepZone-Construct(ConstrNum)%CTFTimeStep)/TimeStepZone) > 0.1d0) THEN

          IF (Construct(ConstrNum)%CTFTimeStep > TimeStepZone) THEN

          ! CTFTimeStep larger than TimeStepZone:  Make sure TimeStepZone
          ! divides evenly into CTFTimeStep
            Construct(ConstrNum)%NumHistories = &
              INT((Construct(ConstrNum)%CTFTimeStep/TimeStepZone)+0.5D0)
            Construct(ConstrNum)%CTFTimeStep = &
              TimeStepZone*REAL(Construct(ConstrNum)%NumHistories,r64)

          ELSE

          ! CTFTimeStep smaller than TimeStepZone:  Set to TimeStepZone
            Construct(ConstrNum)%CTFTimeStep = TimeStepZone
            Construct(ConstrNum)%NumHistories = 1

          END IF

        END IF

          ! Calculate the CTFs using the state space method
          ! outlined in Seem's dissertation.  The main matrices
          ! AMat, BMat, CMat, and DMat must be derived from
          ! applying a finite difference network to the layers of
          ! each bldg element.

          ! This section must continue looping until the CTFs
          ! calculated here will produce a stable solution (less
          ! history terms than MaxCTFTerms).

          ! This first subsection calculates the elements of AMat
          ! which characterizes the heat transfer inside the
          ! building element.

        CTFConvrg = .FALSE.         ! Initialize loop control logical

        ALLOCATE(AExp(rcmax,rcmax))
        AExp=0.0D0
        ALLOCATE(AMat(rcmax,rcmax))
        AMat=0.0D0
        ALLOCATE(AInv(rcmax,rcmax))
        AInv=0.0D0
        ALLOCATE(IdenMatrix(rcmax,rcmax))
        IdenMatrix=0.0D0
        DO ir=1,rcmax
          IdenMatrix(ir,ir)=1.0D0
        ENDDO
        ALLOCATE(e(rcmax))
        e=0.0D0
        ALLOCATE(Gamma1(rcmax,3))
        Gamma1=0.0D0
        ALLOCATE(Gamma2(rcmax,3))
        Gamma2=0.0D0
        ALLOCATE(s(rcmax,4,3))
        s=0.0D0

        DO WHILE (.NOT.CTFConvrg)   ! Begin CTF calculation loop ...

          BMat(3) = 0.0d0

          IF (Construct(ConstrNum)%SolutionDimensions == 1) THEN

          ! Set up intermediate calculations for the first layer.
            cap = rho(1)*cp(1)*dx(1)
            cap = 1.5d0*cap ! For the first node, account for the fact that the
                            ! half-node at the surface results in a "loss" of some
                            ! thermal mass.  Therefore, for simplicity, include it
                            ! at this node.  Same thing done at the last node...
            dxtmp = 1.0D0/dx(1)/cap

            AMat(1,1) = -2.0D0*rk(1)*dxtmp    ! Assign the matrix values for the
            AMat(1,2) = rk(1)*dxtmp           ! first node.
            BMat(1)   = rk(1)*dxtmp           ! Assign non-zero value of BMat.

            Layer = 1   ! Initialize the "layer" counter

            NodeInLayer = 2   ! Initialize the node (in a layer) counter (already
                              ! on the second node for the first layer

            DO Node = 2,rcmax-1 ! Begin nodes loop (includes all nodes except the
                                ! first/last which have special equations) ...

              IF ((NodeInLayer == Nodes(Layer)) .AND. &
                       (LayersInConstruct /= 1)) THEN    ! For a node at
                    ! the interface between two adjacent layers, the
                    ! capacitance of the node must be calculated from the 2
                    ! halves which may be made up of 2 different materials.

                cap = ( rho(Layer)*cp(Layer)*dx(Layer) &
                       +rho(Layer+1)*cp(Layer+1)*dx(Layer+1) ) * 0.5D0

                AMat(Node,Node-1) = rk(Layer)/dx(Layer)/cap           ! Assign matrix
                AMat(Node,Node)   = -1.0D0 * ( rk(Layer)/dx(Layer)+ & ! values for
                                    rk(Layer+1)/dx(Layer+1) ) / cap   ! the current
                AMat(Node,Node+1) = rk(Layer+1)/dx(Layer+1)/cap       ! node.

                NodeInLayer = 0       ! At an interface, reset nodes in layer counter
                Layer       = Layer+1 ! Also increment the layer counter

              ELSE    ! Standard node within any layer

                cap = rho(Layer)*cp(Layer)*dx(Layer)      ! Intermediate
                dxtmp = 1.0D0/dx(Layer)/cap               ! calculations.
                AMat(Node,Node-1) = rk(Layer)*dxtmp       ! Assign matrix
                AMat(Node,Node) = -2.0D0*rk(Layer)*dxtmp  ! values for the
                AMat(Node,Node+1) = rk(Layer)*dxtmp       ! current node.

              END IF

              NodeInLayer = NodeInLayer+1 ! Increment nodes in layer counter
              IF (Node == NodeSource) BMat(3) = 1.0d0/cap

            END DO    ! ... end of nodes loop.

            ! Intermediate calculations for the last node.
            cap = rho(LayersInConstruct)*cp(LayersInConstruct)* dx(LayersInConstruct)
            cap = 1.5d0*cap ! For the last node, account for the fact that the
                            ! half-node at the surface results in a "loss" of some
                            ! thermal mass.  Therefore, for simplicity, include it
                            ! at this node.  Same thing done at the first node...
            dxtmp = 1.0D0/dx(LayersInConstruct)/cap

            AMat(rcmax,rcmax) = -2.0D0*rk(LayersInConstruct)*dxtmp ! Assign matrix
            AMat(rcmax,rcmax-1) = rk(LayersInConstruct)*dxtmp      ! values for the
            BMat(2) = rk(LayersInConstruct)*dxtmp                  ! last node.

            CMat(1) = -rk(1)/dx(1)                ! Compute the necessary elements
            CMat(2) = rk(LayersInConstruct)/dx(LayersInConstruct)   ! of all other
            DMat(1) = rk(1)/dx(1)                         ! matrices for the state
            DMat(2) = -rk(LayersInConstruct)/dx(LayersInConstruct)  ! space method

          ELSE  ! 2-D solution requested (assign matrices appropriately)

            ! As with the 1-D solution, we are accounting for the thermal mass
            ! of the half-node at the surface by adding it to the first row
            ! of interior nodes at both sides of the construction.  This is not
            ! exact, but it does take all of the thermal mass into account.
            amatx = rk(1)/(1.5d0*rho(1)*cp(1)*dx(1)*dx(1))
            amaty = rk(1)/(1.5d0*rho(1)*cp(1)*dyn*dyn)

            ! FIRST ROW OF NODES: This first row within the first material layer
            ! is special in that it is exposed to a boundary condition.  Thus,
            ! the equations are slightly different.
            ! Note also that the first and last nodes in a row are slightly
            ! different from the rest since they are on an adiabatic plane in
            ! the direction perpendicular to the main direction of heat transfer.
            AMat(1,1)                   =-2.0d0*(amatx+amaty)
            AMat(1,2)                   = 2.0d0*amaty
            AMat(1,NumOfPerpendNodes+1) = amatx

            DO Node = 2, NumOfPerpendNodes-1
              AMat(Node,Node-1)                 = amaty
              AMat(Node,Node)                   =-2.0d0*(amatx+amaty)
              AMat(Node,Node+1)                 = amaty
              AMat(Node,Node+NumOfPerpendNodes) = amatx
            END DO

            AMat(NumOfPerpendNodes,NumOfPerpendNodes)                   =-2.0d0*(amatx+amaty)
            AMat(NumOfPerpendNodes,NumOfPerpendNodes-1)                 = 2.0d0*amaty
            AMat(NumOfPerpendNodes,NumOfPerpendNodes+NumOfPerpendNodes) = amatx

            BMat(1) = amatx

            Layer       = 1
            NodeInLayer = 2
            amatx = rk(1)/(rho(1)*cp(1)*dx(1)*dx(1))    ! Reset these to the normal capacitance
            amaty = rk(1)/(rho(1)*cp(1)*dyn*dyn)        ! Reset these to the normal capacitance
            DO Node = NumOfPerpendNodes+1, rcmax+1-2*NumOfPerpendNodes, NumOfPerpendNodes
              ! INTERNAL ROWS OF NODES: This is the majority of nodes which are all within
              ! a solid layer and not exposed to a boundary condition.
              IF ((LayersInConstruct == 1).OR.(NodeInLayer /= Nodes(Layer))) THEN
                ! Single material row: This row of nodes are all contained within a material
                ! and thus there is no special considerations necessary.
                IF (NodeInLayer == 1) THEN
                  ! These intermediate variables only need to be reassigned when a new layer is started.
                  ! When this is simply another row of the same material, these have already been assigned correctly.
                  amatx = rk(Layer)/(rho(Layer)*cp(Layer)*dx(Layer)*dx(Layer))
                  amaty = rk(Layer)/(rho(Layer)*cp(Layer)*dyn*dyn)
                END IF

                ! Note that the first and last layers in a row are slightly different
                ! from the rest since they are on an adiabatic plane in the direction
                ! perpendicular to the main direction of heat transfer.
                AMat(Node,Node)                   =-2.0d0*(amatx+amaty)
                AMat(Node,Node+1)                 = 2.0d0*amaty
                AMat(Node,Node-NumOfPerpendNodes) = amatx
                AMat(Node,Node+NumOfPerpendNodes) = amatx

                DO NodeInRow = 2, NumOfPerpendNodes-1
                  Node2 = Node + NodeInRow - 1
                  AMat(Node2,Node2-1)                 = amaty
                  AMat(Node2,Node2)                   =-2.0d0*(amatx+amaty)
                  AMat(Node2,Node2+1)                 = amaty
                  AMat(Node2,Node2-NumOfPerpendNodes) = amatx
                  AMat(Node2,Node2+NumOfPerpendNodes) = amatx
                END DO

                Node2 = Node - 1 + NumOfPerpendNodes
                AMat(Node2,Node2)                   =-2.0d0*(amatx+amaty)
                AMat(Node2,Node2-1)                 = 2.0d0*amaty
                AMat(Node2,Node2-NumOfPerpendNodes) = amatx
                AMat(Node2,Node2+NumOfPerpendNodes) = amatx

              ELSE  ! Row at a two-layer interface (half of node consists of one layer's materials
                    ! and the other half consist of the next layer's materials)
                capavg = 0.5d0*(rho(Layer)*cp(Layer)*dx(Layer)+rho(Layer+1)*cp(Layer+1)*dx(Layer+1))
                amatx  = rk(Layer)/(capavg*dx(Layer))
                amatxx = rk(Layer+1)/(capavg*dx(Layer+1))
                amaty  = (rk(Layer)*dx(Layer)+rk(Layer+1)*dx(Layer+1))/(capavg*dyn*dyn)

                AMat(Node,Node)                   =-amatx-amatxx-2.0d0*amaty
                AMat(Node,Node+1)                 = 2.0d0*amaty
                AMat(Node,Node-NumOfPerpendNodes) = amatx
                AMat(Node,Node+NumOfPerpendNodes) = amatxx

                DO NodeInRow = 2, NumOfPerpendNodes-1
                  Node2 = Node + NodeInRow - 1
                  AMat(Node2,Node2-1)                 = amaty
                  AMat(Node2,Node2)                   =-amatx-amatxx-2.0d0*amaty
                  AMat(Node2,Node2+1)                 = amaty
                  AMat(Node2,Node2-NumOfPerpendNodes) = amatx
                  AMat(Node2,Node2+NumOfPerpendNodes) = amatxx
                END DO

                Node2 = Node - 1 + NumOfPerpendNodes
                AMat(Node2,Node2)                   =-amatx-amatxx-2.0d0*amaty
                AMat(Node2,Node2-1)                 = 2.0d0*amaty
                AMat(Node2,Node2-NumOfPerpendNodes) = amatx
                AMat(Node2,Node2+NumOfPerpendNodes) = amatxx

                IF (Node == NodeSource) BMat(3) = 2.0d0*REAL(NumOfPerpendNodes-1,r64)/capavg
                NodeInLayer = 0
                Layer       = Layer + 1

              END IF
              NodeInLayer = NodeInLayer + 1

            END DO

            ! LAST ROW OF NODES: Like the first row of nodes, this row is exposed to a boundary
            ! condition and thus has slightly modified nodal equations.

            ! As with the 1-D solution, we are accounting for the thermal mass
            ! of the half-node at the surface by adding it to the first row
            ! of interior nodes at both sides of the construction.  This is not
            ! exact, but it does take all of the thermal mass into account.
            amatx = amatx/1.5d0
            amaty = amaty/1.5d0

            Node = rcmax + 1 - NumOfPerpendNodes
            AMat(Node,Node)                   =-2.0d0*(amatx+amaty)
            AMat(Node,Node+1)                 = 2.0d0*amaty
            AMat(Node,Node-NumOfPerpendNodes) = amatx

            DO Node = rcmax+2-NumOfPerpendNodes, rcmax-1
              AMat(Node,Node-1)                 = amaty
              AMat(Node,Node)                   =-2.0d0*(amatx+amaty)
              AMat(Node,Node+1)                 = amaty
              AMat(Node,Node-NumOfPerpendNodes) = amatx
            END DO

            AMat(rcmax,rcmax)                   =-2.0d0*(amatx+amaty)
            AMat(rcmax,rcmax-1)                 = 2.0d0*amaty
            AMat(rcmax,rcmax-NumOfPerpendNodes) = amatx

            BMat(2) = amatx

            CMat(1) =-rk(1)/dx(1)/REAL(NumOfPerpendNodes-1,r64)
            CMat(2) = rk(LayersInConstruct)/dx(LayersInConstruct)/REAL(NumOfPerpendNodes-1,r64)

            DMat(1) = rk(1)/dx(1)/REAL(NumOfPerpendNodes-1,r64)
            DMat(2) =-rk(LayersInConstruct)/dx(LayersInConstruct)/REAL(NumOfPerpendNodes-1,r64)

          END IF

          ! Calculation of the CTFs based on the state space
          ! method.  This process involves finding the exponential
          ! and inverse of AMat and using these results to
          ! determine the CTFs.  The Gammas are an intermediate
          ! calculations which are necessary before the CTFs can
          ! be computed in TransFuncCoeffs.
          CALL DisplayNumberAndString(ConstrNum,'Calculating CTFs for "'//TRIM(Construct(ConstrNum)%Name)//'", Construction #')

!          CALL DisplayNumberandString(ConstrNum,'Matrix exponential for Construction #')
          CALL CalculateExponentialMatrix(Construct(ConstrNum)%CTFTimeStep)           ! Compute exponential of AMat

!          CALL DisplayNumberandString(ConstrNum,'Invert Matrix for Construction #')
          CALL CalculateInverseMatrix   ! Compute inverse of AMat

!          CALL DisplayNumberandString(ConstrNum,'Gamma calculation for Construction #')
          CALL CalculateGammas(Construct(ConstrNum)%CTFTimeStep,Construct(ConstrNum)%SolutionDimensions)
                              ! Compute "gamma"s from AMat, AExp, and AInv

!          CALL DisplayNumberandString(ConstrNum,'Compute CTFs for Construction #')
          CALL CalculateCTFs(Construct(ConstrNum)%NumCTFTerms,Construct(ConstrNum)%SolutionDimensions)  ! Compute CTFs

          ! Now check to see if the number of transfer functions
          ! is greater than MaxCTFTerms.  If it is, then increase the
          ! time step and the number of history terms and
          ! recalculate.  Whether or not it will be necessary to
          ! recalculate the CTFs is controlled by this DO WHILE
          ! loop and the logical CTFConvrg.

          CTFConvrg = .TRUE.        ! Assume solution convergence

          ! If too many terms, then solution did not converge.  Increase the
          ! number of histories and the time step.  Reset CTFConvrg to continue
          ! the DO loop.
          IF (Construct(ConstrNum)%NumCTFTerms > (MaxCTFTerms-1)) THEN
            Construct(ConstrNum)%NumHistories = Construct(ConstrNum)%NumHistories+1
            Construct(ConstrNum)%CTFTimeStep  = Construct(ConstrNum)%CTFTimeStep &
                                                +TimeStepZone
            CTFConvrg = .FALSE.
          END IF

          ! If the number of terms is okay, then do a further check on the summation of
          ! the various series summations.  In theory, Sum(Xi) = Sum(Yi) = Sum(Zi).  If
          ! this is not the case, then the terms have not reached a valid solution, and
          ! we need to increase the number of histories and the time step as above.
          IF (CTFConvrg) THEN
            SumXi = s0(2,2)
            SumYi = s0(2,1)
            SumZi = s0(1,1)
            DO HistTerm = 1, Construct(ConstrNum)%NumCTFTerms
              SumXi = SumXi + s(HistTerm,2,2)
              SumYi = SumYi + s(HistTerm,2,1)
              SumZi = SumZi + s(HistTerm,1,1)
            END DO
            SumXi = ABS(SumXi)
            SumYi = ABS(SumYi)
            SumZi = ABS(SumZi)
            BiggestSum = MAX(SumXi,SumYi,SumZi)
            IF (BiggestSum > 0.0d0) THEN
              IF ( ((ABS(SumXi-SumYi)/BiggestSum) > MaxAllowedCTFSumError) .OR. &
                   ((ABS(SumZi-SumYi)/BiggestSum) > MaxAllowedCTFSumError) ) THEN
                Construct(ConstrNum)%NumHistories = Construct(ConstrNum)%NumHistories + 1
                Construct(ConstrNum)%CTFTimeStep  = Construct(ConstrNum)%CTFTimeStep + TimeStepZone
                CTFConvrg = .FALSE.
              END IF
            ELSE    ! Something terribly wrong--the surface has no CTFs, not even an R-value
              CALL ShowFatalError('Illegal construction definition, no CTFs calculated for '//TRIM(Construct(ConstrNum)%Name))
            END IF
          END IF

          ! Once the time step has reached a certain point, it is highly likely that
          ! there is either a problem with the input or the solution.  This should
          ! be extremely rare since other checks should flag most bad user input.
          ! Thus, if the time step reaches a certain point, error out and let the
          ! user know that something needs to be checked in the input file.
          IF (Construct(ConstrNum)%CTFTimeStep >= MaxAllowedTimeStep) THEN
            CALL ShowSevereError('CTF calculation convergence problem for Construction="'//TRIM(Construct(ConstrNum)%Name)//'".')
            CALL ShowContinueError('...with Materials (outside layer to inside)')
            CALL ShowContinueError('(outside)="'//trim(Material(Construct(ConstrNum)%LayerPoint(1))%Name)//'"')
            DO Layer=2,Construct(ConstrNum)%TotLayers
              IF (Layer /= Construct(ConstrNum)%TotLayers) THEN
                CALL ShowContinueError('(next)="'//trim(Material(Construct(ConstrNum)%LayerPoint(Layer))%Name)//'"')
              ELSE
                CALL ShowContinueError('(inside)="'//trim(Material(Construct(ConstrNum)%LayerPoint(Layer))%Name)//'"')
              ENDIF
            ENDDO
            CALL ShowContinueError('The Construction report will be produced. This will show more '//  &
                'details on Constructions and their materials.')
            CALL ShowContinueError('Attempts will be made to complete the CTF process but the report may be incomplete.')
            CALL ShowContinueError('Constructs reported after this construction may appear to have all 0 CTFs.')
            CALL ShowContinueError('The potential causes of this problem are related to the input for the construction')
            CALL ShowContinueError('listed in the severe error above.  The CTF calculate routine is unable to come up')
            CALL ShowContinueError('with a series of CTF terms that have a reasonable time step and this indicates an')
            CALL ShowContinueError('error.  Check the definition of this construction and the materials that make up')
            CALL ShowContinueError('the construction.  Very thin, highly conductive materials may cause problems.')
            CALL ShowContinueError('This may be avoided by ignoring the presence of those materials since they probably')
            CALL ShowContinueError('do not effect the heat transfer characteristics of the construction.  Highly')
            CALL ShowContinueError('conductive or highly resistive layers that are alternated with high mass layers')
            CALL ShowContinueError('may also result in problems.  After confirming that the input is correct and')
            CALL ShowContinueError('realistic, the user should contact the EnergyPlus support team.')
            DoCTFErrorReport=.true.
            ErrorsFound=.true.
            EXIT
!            CALL ShowFatalError('Program terminated for reasons listed (InitConductionTransferFunctions) ')
          END IF

        END DO      ! ... end of CTF calculation loop.

      END IF        ! ... end of IF block for non-reversed constructs.

    ELSE            ! Construct has only resistive layers (no thermal mass).
                    ! CTF calculation not necessary, overall resistance
                    ! (R-value) is all that is needed.

          ! Set time step for construct to user time step and the number of
          ! inter-time step interpolations to 1
      Construct(ConstrNum)%CTFTimeStep  = TimeStepZone
      Construct(ConstrNum)%NumHistories = 1
      Construct(ConstrNum)%NumCTFTerms = 1

      s0(1,1) =  cnd  ! CTFs for current time
      s0(1,2) = -cnd  ! step are set to the
      s0(2,1) =  cnd  ! overall conductance
      s0(2,2) = -cnd  ! of the construction.

      ALLOCATE(e(1))
      e=0.0D0
      ALLOCATE(s(1,2,2))
      s=0.0D0
      s(1,1,1) = 0.0D0  ! CTF temperature
      s(1,1,2) = 0.0D0  ! and flux
      s(1,2,1) = 0.0D0  ! history terms
      s(1,2,2) = 0.0D0  ! are all
      e(1) = 0.0D0      ! zero.

      IF (Construct(ConstrNum)%SourceSinkPresent) THEN
        CALL ShowSevereError('Sources/sinks not allowed in purely resistive constructions --> '//Construct(ConstrNum)%Name)
        ErrorsFound = .TRUE.
      END IF

      RevConst = .FALSE.    ! In the code that follows, handle a resistive
                            ! layer as a non-reversed construction.

    END IF          ! ... end of resistive construction IF block.

          ! Transfer the CTFs to the storage arrays for all non-reversed
          ! constructions.  This transfer was done earlier in the routine for
          ! reversed constructions.

    IF (.NOT.RevConst) THEN ! If this is either a new construction or a non-
                            ! reversed construction, the CTFs must be stored
                            ! in the proper arrays.  If this is a reversed
                            ! construction, nothing further needs to be done.

          ! Copy the CTFs into the storage arrays, converting them back to SI
          ! units in the process.  First the "zero" terms and then the history terms...
      Construct(ConstrNum)%CTFOutside(0)   =  s0(1,1)*CFU
      Construct(ConstrNum)%CTFCross(0)     =  s0(2,1)*CFU
      Construct(ConstrNum)%CTFInside(0)    = -s0(2,2)*CFU
      IF (Construct(ConstrNum)%SourceSinkPresent) THEN
          ! QTFs...
        Construct(ConstrNum)%CTFSourceOut(0) = s0(1,3)
        Construct(ConstrNum)%CTFSourceIn(0)  = s0(2,3)
          ! QTFs for temperature calculation at source/sink location
        Construct(ConstrNum)%CTFTSourceOut(0)   = s0(3,1)
        Construct(ConstrNum)%CTFTSourceIn(0)    = s0(3,2)
        Construct(ConstrNum)%CTFTSourceQ(0)     = s0(3,3)/CFU
        IF (Construct(ConstrNum)%TempAfterLayer /= 0) THEN
          ! QTFs for user specified interior temperature calculations...
          Construct(ConstrNum)%CTFTUserOut(0)     = s0(4,1)
          Construct(ConstrNum)%CTFTUserIn(0)      = s0(4,2)
          Construct(ConstrNum)%CTFTUserSource(0)  = s0(4,3)/CFU
        END IF
      END IF

      DO HistTerm = 1, Construct(ConstrNum)%NumCTFTerms
          ! "REGULAR" CTFs...
        Construct(ConstrNum)%CTFOutside(HistTerm)   =  s(HistTerm,1,1)*CFU
        Construct(ConstrNum)%CTFCross(HistTerm)     =  s(HistTerm,2,1)*CFU
        Construct(ConstrNum)%CTFInside(HistTerm)    = -s(HistTerm,2,2)*CFU
        IF (HistTerm /= 0) Construct(ConstrNum)%CTFFlux(HistTerm) = -e(HistTerm)
        IF (Construct(ConstrNum)%SourceSinkPresent) THEN
          ! QTFs...
          Construct(ConstrNum)%CTFSourceOut(HistTerm) = s(HistTerm,1,3)
          Construct(ConstrNum)%CTFSourceIn(HistTerm)  = s(HistTerm,2,3)
          ! QTFs for temperature calculation at source/sink location
          Construct(ConstrNum)%CTFTSourceOut(HistTerm)   = s(HistTerm,3,1)
          Construct(ConstrNum)%CTFTSourceIn(HistTerm)    = s(HistTerm,3,2)
          Construct(ConstrNum)%CTFTSourceQ(HistTerm)     = s(HistTerm,3,3)/CFU
          IF (Construct(ConstrNum)%TempAfterLayer /= 0) THEN
            ! QTFs for user specified interior temperature calculations...
            Construct(ConstrNum)%CTFTUserOut(HistTerm)     = s(HistTerm,4,1)
            Construct(ConstrNum)%CTFTUserIn(HistTerm)      = s(HistTerm,4,2)
            Construct(ConstrNum)%CTFTUserSource(HistTerm)  = s(HistTerm,4,3)/CFU
          END IF
        END IF
      END DO

    END IF          ! ... end of the reversed construction IF block.

    Construct(ConstrNum)%UValue = cnd*CFU

    IF (ALLOCATED(AExp)) DEALLOCATE(AExp)
    IF (ALLOCATED(AMat)) DEALLOCATE(AMat)
    IF (ALLOCATED(AInv)) DEALLOCATE(AInv)
    IF (ALLOCATED(IdenMatrix)) DEALLOCATE(IdenMatrix)
    IF (ALLOCATED(e)) DEALLOCATE(e)
    IF (ALLOCATED(Gamma1)) DEALLOCATE(Gamma1)
    IF (ALLOCATED(Gamma2)) DEALLOCATE(Gamma2)
    IF (ALLOCATED(s)) DEALLOCATE(s)

  END DO            ! ... end of construction loop.

  CALL ReportCTFs(DoCTFErrorReport)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Program terminated for reasons listed (InitConductionTransferFunctions)')
  ENDIF

  RETURN

END SUBROUTINE InitConductionTransferFunctions

SUBROUTINE CalculateExponentialMatrix(delt)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1990
          !       MODIFIED       Dec 1995, Apr 1996, RKS; June 2000 RKS
          !       RE-ENGINEERED  June 1996, RKS; Nov 1999, LKL;

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the exponential matrix exp(AMat*delt) for
          ! use in the state space method for the calculation of CTFs.

          ! METHODOLOGY EMPLOYED:
          ! Uses the method of Taylor expansion combined with scaling and
          ! squaring to most efficiently compute the exponential matrix.  The
          ! steps in the procedure are outlined in Seem's dissertation in
          ! Appendix A, page 128.  Exponential matrix multiplication modified
          ! to take advantage of the characteristic form of AMat.  AMat starts
          ! out as a tri-diagonal matrix.  Each time AMat is raised to a higher
          ! power two extra non-zero diagonals are added.  ExponMatrix now
          ! recognizes this.  This should speed up the calcs somewhat.  Also, a
          ! new cut-off criteria based on the significant figures of double-
          ! precision variables has been added.  The main loop for higher powers
          ! of AMat is now stopped whenever these powers of AMat will no longer
          ! add to the summation (AExp) instead ofstopping potentially at the
          ! artifical limit of AMat**100.

          ! REFERENCES:
          ! Seem, J.E.  "Modeling of Heat Transfer in Buildings",
          !  Department of Mechanical Engineering, University of
          !  Wisconsin-Madison, 1987.
          ! Strand, R.K. "Testing Design Description for the CTF
          !  Calculation Code in BEST", BSO internal document,
          !  May/June 1996.

          ! USE STATEMENTS:
          ! none

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) :: delt  ! Time step of the resulting CTFs

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER :: DPLimit = 1.0d-20
          ! This argument is nice, but not sure it's accurate -- LKL Nov 1999.
          ! Parameter set to the significant figures limit of double
          ! precision variables plus a safety factor.- The argument for setting this parameter to 1E-20 involves the
          ! number of significant figures for REAL(r64) variables which is 16 and the largest power to which
          ! AMat will be raised which is 100.  This would be a factor of 1E-18.  A factor of "safety" of another 100
          ! arrives at the value chosen.  It is argued that if one number is 1E-16 larger than a second number, then
          ! adding the second to the first will not effect the first.  However, on the conservative side, there could
          ! be up to 100 numbers which might, added together, still could effect the original number.  Each
          ! successive power of AMat will have terms smaller than the previous power.  Thus, when the ratio between
          ! the terms of the latest power of AMat and the total (AExp) is less than DPLim, all further powers of
          ! AMat will have absolutely no effect on the REAL(r64) value of AExp.  Thus, there is no need to
          ! continue the calculation.  In effect, AExp has "converged".  In REAL(r64)ity, 1E-16 would probably guarantee
          ! convergence since AMat terms drop off quickly, but the extra powers allows for differences between
          ! computer platforms.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                         :: AMatRowNorm      ! Row norm for AMat
  REAL(r64)                         :: AMatRowNormMax   ! Largest row norm for AMat
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AMat1    ! AMat factored by (delt/2^k)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AMato    ! AMat raised to the previous power (power of AMat1-1)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AMatN    ! Current value of AMat raised to power n (n = 1,2...)
  LOGICAL                                       :: Backup   ! Used when numerics get to small in Exponentiation
  REAL(r64)                                     :: CheckVal ! Used to avoid possible overflow from Double->REAL(r64)->Integer
  REAL(r64)                         :: fact     ! Intermediate calculation variable (delt/2^k)
  INTEGER                                       :: i        ! Loop counter
  INTEGER                                       :: ic       ! Loop counter
  INTEGER                                       :: ict      ! Loop counter
  INTEGER                                       :: idm      ! Loop counter
  INTEGER                                       :: ir       ! Loop counter
  INTEGER                                       :: isq      ! Loop counter
  INTEGER                                       :: j        ! Loop counter
  INTEGER                                       :: k        ! Power of 2 which is used to factor AMat
  INTEGER                                       :: l        ! Theoretical power to which the A matrix must be
                                                            ! raised to accurately calculate the exponential matrix
  LOGICAL                                       :: SigFigLimit      ! Significant figure limit logical, true
                                                                    ! when exponential calculation loop can be exited (i.e.
                                                                    ! the significant figure limit for REAL(r64)
                                                                    ! variables reached)

          ! FLOW:
  ALLOCATE(AMat1(rcmax,rcmax))
  ALLOCATE(AMato(rcmax,rcmax))
  ALLOCATE(AMatN(rcmax,rcmax))

  ! Subroutine initializations.  AMat is assigned to local variable AMat1 to
  ! avoid the corruption of the original AMat 2-d array.
  AMat1=AMat

  !  Other arrays are initialized to zero.
  AExp=0.0D0
  AMato=0.0D0
  AMatN=0.0D0


          ! Step 1, page 128 (Seem's thesis):  Compute the matrix row norm.
          ! See equation (A.3) which states that the matrix row norm is the
          ! maximum summation of the elements in a row of AMat multiplied by
          ! the time step.

  AMatRowNormMax = 0.0D0            ! Start of Step 1 ...

  DO i = 1,rcmax

    AMatRowNorm = 0.0D0
    DO j = 1,rcmax
      AMatRowNorm = AMatRowNorm+ABS(AMat1(i,j))
    END DO

    AMatRowNorm = AMatRowNorm*delt

    AMatRowNormMax=MAX(AMatRowNormMax,AMatRowNorm)

  END DO                            ! ... end of Step 1.

          ! Step 2, page 128:  Find smallest integer k such that
          ! AMatRowNormMax< = 2^k

  k = INT(LOG(AMatRowNormMax)/LOG(2.0D0))+1 !Objexx:Num Handle AMatRowNormMax=0

          ! Step 3, page 128:  Divide (AMat*delt) by 2^k.  This section of code
          ! takes advantage of the fact that AMat is tridiagonal.  Thus, it
          ! only factors the elements of the AMat that are known to be non-zero.

  fact  = delt/(2.0d0**k)   ! Start of Step 3 ...
  AMat1 = AMat1*fact        ! ... end of Step 3.

          ! Step 4, page 128:  Calculate l, the highest power to which AMat
          ! must be taken theoretically to accurately calculate its exponential.
          ! This is based on a paper by Cadzow and Martens ("Discrete-Time and
          ! Computer Control Systems",Prentice-Hall, pp. 389-390, 1970).  This
          ! number is now used as the maximum power to which AMat must be
          ! raised in order to calculate the exponential matrix.  A new cut-off
          ! criteria based on the number of significant figures in a double-
          ! precision variable is used as a more practical limit on the
          ! exponentiation algorithm.

  CheckVal = MIN(3.0d0*AMatRowNormMax+6.d0,100.0d0)
  l        = INT(CheckVal)

          ! Step 5, page 128:  Calculate the exponential.  First, add the
          ! linear term to the identity matrix.
   AExp = AMat1 + IdenMatrix        ! Start of Step 5 ...

          ! Now, add successive terms to the expansion as per the standard
          ! exponential formula.  AMato contains the last "power" of AMat
          ! which saves the program from having to remultiply the entire power
          ! of AMat each time.  Since this is still the linear power of AMat,
          ! AMat1 is still tridiagonal in nature.
  AMato = AMat1

  i = 1   ! Initialize the counter for the following DO loop

          ! The following DO WHILE loop continues to raise AMat to successive
          ! powers and add it to the exponential matrix (AExp).
  DO WHILE (i < l) ! Begin power raising loop ...

    i = i+1               ! Increment the loop counter
    SigFigLimit = .true.  ! Set the significant factor limit flag

    DO ir = 1, rcmax    ! Begin matrix multiplication loop ...
          ! The following matrix multiplication could be "optimized" since
          ! for one-dimensional heat transfer AMat is 3-diagonal, AMat squared
          ! is 5-diagonal, etc.  However, the code can be much simpler if we
          ! ignore this fact and just do a generic matrix multiplication.
          ! For 2-D heat transfer, the number of off-diagonal non-zero terms
          ! is slightly more complicated as well.
      DO ic = 1, rcmax
        AMatN(ir,ic) = 0.0d0
        DO ict = 1, rcmax
          ! Make sure the next term won't cause an underflow.  If it will end up being
          ! so small as to go below TinyLimit, then ignore it since it won't add anything
          ! to AMatN anyway.
          IF (ABS(AMat1(ict,ic)) > TinyLimit) THEN
            IF (ABS(AMato(ir,ict)) > ABS(REAL(i,r64)*TinyLimit/AMat1(ict,ic))) &
              AMatN(ir,ic) = AMatN(ir,ic)+AMato(ir,ict)*AMat1(ict,ic)/REAL(i,r64)
          END IF
        END DO
      END DO
    END DO          ! ... end of matrix multiplication loop.

          ! Update AMato and AExp matrices
    AMato = AMatN
    AExp  = AExp + AMato

          ! The next DO loop tests the significant figures limit criteria to
          ! see if any values in AExp are still changing appreciably.
    DO ir = 1, rcmax
      DO ic = 1, rcmax
          ! Test of limit criteria:
        IF (ABS(AExp(ir,ic)) > TinyLimit) THEN  ! Next line divides by AExp entry so it
                                                ! must be checked to avoid dividing by zero.
          ! If the ratio between any current element in the power
          ! of AMat and its corresponding element in AExp is
          ! greater than the number which might effect the overall
          ! exponential matrix based on stability criteria, then
          ! continue raising AMat to another power (SigFigLimit = false).

          IF (ABS(AMato(ir,ic)/AExp(ir,ic)) > DPLimit) THEN
            SigFigLimit = .FALSE.
            EXIT ! DO loop (anytime SigFigLimit is false, AMat must continue to be raised another power)
          END IF

        ELSE        ! There are still elements of AExp which are zero, so
                    ! the raising of AMat to higher powers should continue.

          SigFigLimit = .FALSE.
          EXIT ! DO loop (anytime SigFigLimit is false, AMat must continue to be raised another power)

        END IF
      END DO
      IF (.NOT.SigFigLimit) EXIT ! DO loop (anytime SigFigLimit is false, AMat must continue to be raised another power)
    END DO

          ! Compute next term, only if necessary.  If SigFigLimit is still true,
          ! then all of the new terms being added to AExp are too small to
          ! affect it.  Thus, there is no need to continue this do loop further.

    IF (SigFigLimit) i = 100  ! SigFigLimit is still true, set i to maximum possible
                              ! value of l (100).

  END DO            ! ... end of power raising loop and Step 5.

          ! Step 6, page 128:
          ! Square AExp "k times" to obtain the actual exponential matrix
          ! (remember that AExp was scaled earlier in this routine).

  DO isq = 1,k      ! Begin squaring DO loop and Step 6 ...

          ! Use AMato to store the old values of AExp
    AMato  = AExp
    Backup = .true.
    AExp   = 0.0D0

          ! Multiply the old value of AExp (AMato) by itself and store in AExp.
    DO ir = 1,rcmax
      DO ic = 1,rcmax
        DO idm = 1,rcmax
          IF (ABS(AMato(ir,idm)*AMato(idm,ic)) > TinyLimit) THEN
            AExp(ir,ic) = AExp(ir,ic)+AMato(ir,idm)*AMato(idm,ic)
            Backup=.false.
          END IF
        END DO
      END DO
    END DO
    ! Backup is true when every item of AExp didnt pass the TinyLimit test
    IF (Backup) THEN
      AExp=AMato
      EXIT
    END IF

  END DO            ! ... end of squaring loop and Step 6.

  DEALLOCATE(AMat1)
  DEALLOCATE(AMato)
  DEALLOCATE(AMatN)

  RETURN            ! AExp is now the exponential of AMat.

END SUBROUTINE CalculateExponentialMatrix

SUBROUTINE CalculateInverseMatrix

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   Dec 1995
          !       MODIFIED       June 2000 RKS (made routine generic to allow for 2-D solutions)
          !       RE-ENGINEERED  June 1996, February 1997 RKS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the inverse of AMat for use
          ! in the calculation of the CTFs.

          ! METHODOLOGY EMPLOYED:
          ! Uses row elimination to zero the off-diagonal terms of
          ! AMat while performing the same operations on another
          ! matrix which starts as the identity matrix.  Once AMat
          ! has been converted to an identity matrix(I), the other
          ! matrix which started as the I will then be the inverse
          ! of A.  This algorithm has been customized for a
          ! tri-diagonal matrix.

          ! REFERENCES:
          ! Any linear algebra test (this is a generic routine).

          ! USE STATEMENTS:
          ! none

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
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AMat1  ! Intermediate calculation matrix equivalent at first to AMat
  INTEGER                                       :: ic     ! Loop counter
  INTEGER                                       :: ir     ! Loop counter
  INTEGER                                       :: irr    ! Loop counter

          ! FLOW:

          ! Subroutine initializations ...
  ALLOCATE(AMat1(rcmax,rcmax))

  AMat1 = AMat          ! Set AMat1 = AMat to avoid AMat changes
  AInv  = IdenMatrix    ! Set AInv to Identity Matrix

          ! Use Gaussian elimination to zero all of the elements of AMat left
          ! of the diagonal.
          ! This DO loop will cycle through each of the rows in AMat except the
          ! last row which is handled later because it does not have to be used
          ! to eliminate any other rows.  The index ir is the current row
          ! number and also the column of the current diagonal element.

  DO ir = 1, rcmax-1    ! Begin forward elimination loop ...

          ! Factor all of the elements of the row being used to zero the next
          ! row in both AMat and AInv by the diagonal element of this row.
          ! We should only need to factor the elements to the right of the
          ! diagonal since those to the right of it should be zero.
    DO ic = ir+1, rcmax
      AMat1(ir,ic) = AMat1(ir,ic)/AMat1(ir,ir)
    END DO

          ! In the forward elimination process, all the elements in AInv to the
          ! right of the diagonal are zero so they do not need to be factored.
    DO ic = 1, ir
      AInv(ir,ic) = AInv(ir,ic)/AMat1(ir,ir)
    END DO

    AMat1(ir,ir) = 1.0d0      ! By definition, the diagonal of AMat is now 1.

          ! Use this factored row to eliminate the off-diagonal element of the
          ! rows below the current one (ir)...

    DO irr = ir+1, rcmax    ! Start of row reduction loop...

      DO ic = ir+1, rcmax
        AMat1(irr,ic) = AMat1(irr,ic) - AMat1(irr,ir)*AMat1(ir,ic)
      END DO

          ! Now, determine the effect on the next row of AInv.  Again, all of
          ! the elements in AInv to the right of the diagonal are zero, so they
          ! can be ignored.

      DO ic = 1,ir
        AInv(irr,ic) = AInv(irr,ic) - AMat1(irr,ir)*AInv(ir,ic)
      END DO

      AMat1(irr,ir) = 0.0d0 ! By definition, the element to the left of the
                            ! diagonal in the next row of AMat is now zero.

    END DO                  ! ...end of row reduction loop

  END DO            ! ... end of the forward elimination loop.

          ! Factor the last row of AInv by the current value of the last
          ! diagonal element of AMat. After this is done, all of the diagonal
          ! elements of AMat are unity and all of the elements in AMat left of
          ! the diagonal are zero.

  DO ic = 1,rcmax
    AInv(rcmax,ic) = AInv(rcmax,ic)/AMat1(rcmax,rcmax)
  END DO
  AMat1(rcmax,rcmax) = 1.0D0

          ! Now, use back substitution to eliminate the elements to the right
          ! of the diagonal in AMat.  The procedure is similar to the forward
          ! elimination process except that we only have to operate on AInv,
          ! though now all of the columns of AInv may be non-zero.

          ! This DO loop will cycle through the remaining rows which are not
          ! yet diagonalized in reverse order.  Note that the only effect on
          ! AMat is that the off-diagonal element is zeroed.  The diagonal
          ! (which has already been set to unity) is not effected by this row
          ! elimination process.
          ! In the following code ir is the column being zeroed and irr is the
          ! row being worked on

  DO ir = rcmax, 2, -1  ! Begin reverse elimination loop ...
    DO irr = 1, ir-1
      DO ic = 1,rcmax
        AInv(irr,ic) = AInv(irr,ic)-AMat1(irr,ir)*AInv(ir,ic)
      END DO
      AMat1(irr,ir) = 0.0d0
    END DO
  END DO                ! ... end of reverse elimination loop.

          ! At this point, AMat1 is equal to the identity matrix (I)
          ! and AInv is equal to the inverse of AMat.

  DEALLOCATE(AMat1)

  RETURN

END SUBROUTINE CalculateInverseMatrix

SUBROUTINE CalculateGammas(delt,SolutionDimensions)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1990
          !       MODIFIED       na
          !       RE-ENGINEERED  July 1996, RKS

          ! PURPOSE OF THIS SUBROUTINE:
          ! Compute gammas as defined in Seem's dissertation.
          ! Runs as a subroutine of the conduction transfer
          ! function solver (InitializeCTFs).

          ! METHODOLOGY EMPLOYED:
          ! Determine the Gamma1 and Gamma2 based on the results
          ! from the ExponMatrix and InvertMatrix subroutines.
          ! This routine is specialized to take advantage of the
          ! fact that most of BMat consists of zeroes.

          ! REFERENCES:
          ! The state space method of calculating CTFs is
          ! outlined in the doctoral dissertation of John Seem,
          ! "Modeling of Heat Transfer in Buildings", Department
          ! of Mechanical Engineering, University of Wisconsin-
          ! Madison, 1987.

          ! USE STATEMENTS:
          ! none

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: delt               ! Time increment in fraction of an hour
  INTEGER,          INTENT(IN) :: SolutionDimensions ! Integer relating whether a 1- or 2-D solution is required

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: ATemp    ! Intermediate variable equal to AExp - I
  INTEGER :: i          ! Loop counter
  INTEGER :: is1        ! Loop counter
  INTEGER :: j          ! Loop counter
  INTEGER :: SurfNode   ! Loop counter


          ! FLOW:

          ! Compute Gamma1 from equation (2.1.12) in Seem's dissertation which
          ! states that:  Gamma1  =  [AInv] * ([AExp]-[I]) * [BMat]
          ! noting that BMat contains only the non-zero values of the B Matrix.

  ALLOCATE(ATemp(rcmax,rcmax))
  ATemp  = AExp - IdenMatrix
  Gamma1 = 0.0d0

  DO i = 1,rcmax

    DO is1 = 1,rcmax

      IF (SolutionDimensions == 1) THEN
        Gamma1(i,1) = Gamma1(i,1)+AInv(i,is1)*ATemp(is1,1)*BMat(1)
        Gamma1(i,2) = Gamma1(i,2)+AInv(i,is1)*ATemp(is1,rcmax)*BMat(2)
      ELSE  ! SolutionDimensions = 2
        DO SurfNode = 1, NumOfPerpendNodes
          Gamma1(i,1) = Gamma1(i,1)+AInv(i,is1)*ATemp(is1,SurfNode)*BMat(1)
          Gamma1(i,2) = Gamma1(i,2)+AInv(i,is1)*ATemp(is1,rcmax+1-SurfNode)*BMat(2)
        END DO
      END IF

      IF (NodeSource > 0) THEN
        Gamma1(i,3) = Gamma1(i,3)+AInv(i,is1)*ATemp(is1,NodeSource)*BMat(3)
      END IF

    END DO

  END DO

  DEALLOCATE(ATemp)
          ! Compute Gamma2 from equation (2.1.13) in Seem's dissertation which
          ! states that:  Gamma2  =  [AInv] * ([Gamma1]/delt - [BMat])
          ! again noting that BMat contains only the non-zero values of B.
  Gamma2 = 0.0d0

  DO i = 1,rcmax

    DO j = 1,3

      DO is1 = 1,rcmax

        IF (SolutionDimensions == 1) THEN
          IF ( (j == 1) .AND. (is1 == 1) ) THEN
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt-BMat(1))
          ELSE IF ( (j == 2) .AND. (is1 == rcmax) ) THEN
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt-BMat(2))
          ELSE IF ( (j == 3) .AND. (is1 == NodeSource) ) THEN
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt-BMat(3))
          ELSE  ! the element of the actual BMat is zero
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt)
          END IF
        ELSE    ! SolutionDimensions = 2
          IF ( (j == 1) .AND. ((is1 >= 1).AND.(is1 <= NumOfPerpendNodes)) ) THEN
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt-BMat(1))
          ELSE IF ( (j == 2) .AND. ((is1 <= rcmax).AND.(is1 >= rcmax+1-NumOfPerpendNodes)) ) THEN
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt-BMat(2))
          ELSE IF ( (j == 3) .AND. (is1 == NodeSource) ) THEN
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt-BMat(3))
          ELSE  ! the element of the actual BMat is zero
            Gamma2(i,j) = Gamma2(i,j)+AInv(i,is1)*(Gamma1(is1,j)/delt)
          END IF
        END IF

      END DO

    END DO

  END DO

  RETURN  ! The calculation of Gamma1 and Gamma2 is now complete.

END SUBROUTINE CalculateGammas

SUBROUTINE CalculateCTFs(nrf,SolutionDimensions)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1990
          !       MODIFIED       Apr 96, RKS, cosmetic, algorithm neutral changes
          !       RE-ENGINEERED  July 1996, RKS; Nov 1999, LKL (allocatable arrays)

          ! PURPOSE OF THIS SUBROUTINE:
          ! Subprogram to calculate the Sj and ej coefficients of Seem's
          ! dissertation.  Follows Seem's technique to compute the coefficients
          ! in order with minimum storage requirements.

          ! METHODOLOGY EMPLOYED:
          ! Combine the results of the ExponMatrix, InvertMatrix, and
          ! CalculateGammas routines together to arrive at the temperature
          ! coefficients (s, s0) and the heat flux history coefficients (e) of
          ! the CTFs.  The outline of this subroutine is based on step 5 of
          ! Seem's suggested implementation of the state space method found on
          ! pages 26+27 of his dissertation.

          ! REFERENCES:
          ! The state space method of calculating CTFs is outlined in the
          ! doctoral dissertation of John Seem, "Modeling of Heat Transfer in
          ! Buildings", Department of Mechanical Engineering, University of
          ! Wisconsin-Madison, 1987.  In particular, the equations used for
          ! these calculations are equations (2.1.24) through (2.1.26) in Seem's
          ! dissertation.

          ! USE STATEMENTS:
          ! none

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(OUT) :: nrf                ! Number of response factor terms
  INTEGER, INTENT(IN)  :: SolutionDimensions ! Integer relating whether a 1- or 2-D solution is required

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ConvrgLim = 1.0d-13   ! Convergence limit (ratio) for cutting off the calculation of further
                                                       ! CTFs.  This value was found to give suitable accuracy in IBLAST.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE DUMMY VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64)                                :: avg       ! Intermediate calculation variable (average)
  LOGICAL                                              :: CTFConvrg ! Set after CTFs are calculated, based on whether there are
                                                                    ! too many CTFs terms
  INTEGER                                              :: i         ! Loop counter
  INTEGER                                              :: ic        ! Loop counter
  INTEGER                                              :: inum      ! Loop counter
  INTEGER                                              :: ir        ! Loop counter
  INTEGER                                              :: is        ! Loop counter
  INTEGER                                              :: is2       ! Loop counter
  INTEGER                                              :: j         ! Loop counter
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)        :: PhiR0     ! Product of Phi( = AExp) and R0 matrices from the state
                                                                    ! space method
  REAL(r64)                                :: rat       ! Intermediate calculation variable (ratio of flux history
                                                                    ! terms)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)        :: Rnew      ! Current R matrix
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)        :: Rold      ! R matrix from the last iteration
  INTEGER                                              :: SurfNode  ! Loop counter (for nodes at a surface)
  REAL(r64)                                :: SurfNodeFac ! Multiplying factor applied to various surface nodes
  REAL(r64)                                :: trace     ! Trace of the product of Phi( = AExp) and R0

          ! FLOW:

          ! Subroutine initializations
  ALLOCATE(PhiR0(rcmax,rcmax))
  ALLOCATE(Rnew(rcmax,rcmax))
  ALLOCATE(Rold(rcmax,rcmax))
  PhiR0 = 0.0D0
  Rold  = 0.0D0

  s0   = 0.0D0
  s    = 0.0D0
  e    = 0.0D0
  Rnew = IdenMatrix ! Rnew initialized to the identity matrix

          ! Calculate Gamma1-Gamma2.  Gamma1 is not used by itself in the
          ! equations, only Gamma1-Gamma2.  Thus, reset Gamma1 to:
          ! Gamma1-Gamma2
  DO i = 1,rcmax
    DO j = 1,3
      Gamma1(i,j) = Gamma1(i,j)-Gamma2(i,j)
    END DO
  END DO

          ! Compute s0.  See Seem's thesis equation (2.1.24) which states that:
          ! s0  =  (CMat*R0*Gamma2) + (DMat)
          ! Note that for a two-dimensional solution, there is more than one
          ! node at the surface and the effect of each of these must be added
          ! together.
  IF (SolutionDimensions == 1) THEN
    s0(1,1) = CMat(1)*Gamma2(1,1) + DMat(1)
    s0(1,2) = CMat(1)*Gamma2(1,2)
    s0(1,3) = CMat(1)*Gamma2(1,3)
    s0(2,1) = CMat(2)*Gamma2(rcmax,1)
    s0(2,2) = CMat(2)*Gamma2(rcmax,2) + DMat(2)
    s0(2,3) = CMat(2)*Gamma2(rcmax,3)
  ELSE  ! SolutionDimensions = 2
    DO SurfNode = 1, NumOfPerpendNodes
      IF ( (SurfNode == 1) .OR. (SurfNode == NumOfPerpendNodes) ) THEN
        SurfNodeFac = 0.5d0
      ELSE
        SurfNodeFac = 1.0d0
      END IF
      s0(1,1) = s0(1,1) + SurfNodeFac*CMat(1)*Gamma2(SurfNode,1)
      s0(1,2) = s0(1,2) + SurfNodeFac*CMat(1)*Gamma2(SurfNode,2)
      s0(1,3) = s0(1,3) + SurfNodeFac*CMat(1)*Gamma2(SurfNode,3)
      s0(2,1) = s0(2,1) + SurfNodeFac*CMat(2)*Gamma2(rcmax+1-SurfNode,1)
      s0(2,2) = s0(2,2) + SurfNodeFac*CMat(2)*Gamma2(rcmax+1-SurfNode,2)
      s0(2,3) = s0(2,3) + SurfNodeFac*CMat(2)*Gamma2(rcmax+1-SurfNode,3)
    END DO
    s0(1,1) = s0(1,1) + REAL(NumOfPerpendNodes-1,r64)*DMat(1)
    s0(2,2) = s0(2,2) + REAL(NumOfPerpendNodes-1,r64)*DMat(2)
  END IF

  IF (NodeSource > 0) THEN
    s0(3,1) = Gamma2(NodeSource,1)
    s0(3,2) = Gamma2(NodeSource,2)
    s0(3,3) = Gamma2(NodeSource,3)
  END IF
  IF (NodeUserTemp > 0) THEN
    s0(4,1) = Gamma2(NodeUserTemp,1)
    s0(4,2) = Gamma2(NodeUserTemp,2)
    s0(4,3) = Gamma2(NodeUserTemp,3)
  END IF

          ! Check for and enforce symmetry in the cross term (Y)
  IF (ABS(s0(1,2)) /= ABS(s0(2,1))) THEN
    avg = (ABS(s0(1,2))+ABS(s0(2,1))) * 0.5D0
    s0(1,2) = avg*s0(1,2)/ABS(s0(1,2))
    s0(2,1) = avg*s0(2,1)/ABS(s0(2,1))
  END IF

          ! Compute S's and e's from 1 to n-1.  See equations (2.1.25) and
          ! (2.1.26) and Appendix C.
  inum = 1            ! Set history term counter
  CTFConvrg = .FALSE. ! Set the convergence logical to false

          ! The following DO WHILE loop calculates each successive set of time
          ! history terms until there are rcmax number of history terms or the
          ! latest flux history term is negligibly small compared to the first
          ! flux history term.
  DO WHILE ((.NOT.CTFConvrg) .AND. (inum < rcmax))   ! Begin CTF calculation loop ...

          ! Compute e(inum) based on Appendix C (Seem's dissertation). First,
          ! compute the new PhiR0 and its trace.

    trace = 0.0D0

    DO ir = 1,rcmax

      DO ic = 1,rcmax
        PhiR0(ir,ic) = 0.0D0
        DO is = 1,rcmax
          ! Make sure the next term won't cause an underflow.  If it will end up being
          ! so small as to go below TinyLimit, then ignore it since it won't add anything
          ! to PhiR0 anyway.
          IF (ABS(Rnew(is,ic)) > TinyLimit) THEN
            IF (ABS(AExp(ir,is)) > ABS(TinyLimit/Rnew(is,ic))) &
              PhiR0(ir,ic) = PhiR0(ir,ic)+AExp(ir,is)*Rnew(is,ic)
          END IF
        END DO
      END DO

      trace = trace + PhiR0(ir,ir)

    END DO

          ! Now calculate ej from the trace.  According to Appendix C:
          ! e(j) = -Trace[AExp*R(j-1)]/j

    e(inum) = -trace/REAL(inum,r64)

          ! Update Rold and compute Rnew.  Note:  PhiR0 = AExp*R(j-1) here.
          ! According to Appendix C:  R(j) = AExp*R(j-1) + e(j-1)

    DO ir = 1,rcmax
      DO ic = 1,rcmax
        Rold(ir,ic) = Rnew(ir,ic)
        Rnew(ir,ic) = PhiR0(ir,ic)
      END DO
      Rnew(ir,ir) = Rnew(ir,ir)+e(inum)
    END DO

          ! Compute S(inum) based on eq.(2.1.25) which states:
          ! S(j)  =  CMat*[R(j-1)*(Gamma1-Gamma2)+R(j)*Gamma2]
          !          + e(j)*DMat
    IF (SolutionDimensions == 1) THEN
      DO j = 1, 3
        DO is2 = 1,rcmax
          s(inum,1,j) = s(inum,1,j) + CMat(1)*( Rold(1,is2)*Gamma1(is2,j)  &
                                               +Rnew(1,is2)*Gamma2(is2,j) )
          s(inum,2,j) = s(inum,2,j) + CMat(2)*( Rold(rcmax,is2)*Gamma1(is2,j)  &
                                               +Rnew(rcmax,is2)*Gamma2(is2,j) )
          IF (NodeSource > 0) THEN
            s(inum,3,j) = s(inum,3,j) + ( Rold(NodeSource,is2)*Gamma1(is2,j)  &
                                         +Rnew(NodeSource,is2)*Gamma2(is2,j) )
          END IF
          IF (NodeUserTemp > 0) THEN
            s(inum,4,j) = s(inum,4,j) + ( Rold(NodeUserTemp,is2)*Gamma1(is2,j)  &
                                         +Rnew(NodeUserTemp,is2)*Gamma2(is2,j) )
          END IF

        END DO
        IF (j /= 3) s(inum,j,j) = s(inum,j,j) + e(inum)*DMat(j)
      END DO
    ELSE ! SolutionDimensions = 2
      DO j = 1, 3
        DO is2 = 1,rcmax
          DO SurfNode = 1, NumOfPerpendNodes
            IF ( (SurfNode == 1) .OR. (SurfNode == NumOfPerpendNodes) ) THEN
              SurfNodeFac = 0.5d0
            ELSE
              SurfNodeFac = 1.0d0
            END IF
            s(inum,1,j) = s(inum,1,j) + SurfNodeFac*CMat(1)*( Rold(SurfNode,is2)*Gamma1(is2,j)  &
                                                             +Rnew(SurfNode,is2)*Gamma2(is2,j) )
            s(inum,2,j) = s(inum,2,j) + SurfNodeFac*CMat(2)*( Rold(rcmax+1-SurfNode,is2)*Gamma1(is2,j)  &
                                                             +Rnew(rcmax+1-SurfNode,is2)*Gamma2(is2,j) )
          END DO
          IF (NodeSource > 0) THEN
            s(inum,3,j) = s(inum,3,j) + ( Rold(NodeSource,is2)*Gamma1(is2,j)  &
                                         +Rnew(NodeSource,is2)*Gamma2(is2,j) )
          END IF
          IF (NodeUserTemp > 0) THEN
            s(inum,4,j) = s(inum,4,j) + ( Rold(NodeUserTemp,is2)*Gamma1(is2,j)  &
                                         +Rnew(NodeUserTemp,is2)*Gamma2(is2,j) )
          END IF
        END DO
      END DO
      s(inum,1,1) = s(inum,1,1) + e(inum)*DMat(1)*REAL(NumOfPerpendNodes-1,r64)
      s(inum,2,2) = s(inum,2,2) + e(inum)*DMat(2)*REAL(NumOfPerpendNodes-1,r64)
    END IF

          ! Check for and enforce symmetry in the cross term (Y)
    IF (ABS(s(inum,1,2)) /= ABS(s(inum,2,1))) THEN
      avg = (ABS(s(inum,1,2))+ABS(s(inum,2,1))) * 0.5D0
      s(inum,1,2) = avg*s(inum,1,2)/ABS(s(inum,1,2))
      s(inum,2,1) = avg*s(inum,2,1)/ABS(s(inum,2,1))
    END IF

          ! Check for convergence of the CTFs.
    IF (e(1) == 0.0D0) THEN

      nrf = 1                   ! e(1) is zero, so there are no history terms.
      CTFConvrg = .TRUE.        ! CTF calculations have converged--set logical.

    ELSE
          ! e(1) is non-zero -- Calculate and compare the ratio of the flux
          ! terms to the convergence limit.
      rat = ABS(e(inum)/e(1))

      IF (rat < ConvrgLim) THEN

          ! If the ratio is less than the convergence limit, then any other
          ! terms would have a neglible impact on the CTF-based energy balances.
        nrf = inum
        CTFConvrg = .TRUE.    ! CTF calculations have converged--set logical.

      END IF
    END IF  ! ... end of convergence check block.

    inum = inum+1

  END DO    ! ... end of CTF calculation loop.
            ! Continue to the next coefficient if the solution has not converged
                    !
  IF (.NOT.CTFConvrg) THEN  ! Compute last e and S, if still unconverged.

          ! Compute e(inum) based on Appendix C (Seem's dissertation) or see
          ! equation above.  First compute the new PhiR0 and its trace.

    trace = 0.0D0

    DO ir = 1,rcmax
      DO is = 1,rcmax
        trace = trace+AExp(ir,is)*Rnew(is,ir)
      END DO
    END DO

    e(rcmax) = -trace/REAL(rcmax,r64)     ! Now calculate ej from the trace.

          ! Compute S(inum) based on eq.(2.1.25) which states:
          !   S(last) = CMat*R(last-1)*(Gamma1-Gamma2)+e(last)*DMat

    IF (SolutionDimensions == 1) THEN
      DO j = 1, 3
        DO is2 = 1,rcmax
          s(rcmax,1,j) = s(rcmax,1,j) + CMat(1)*Rnew(1,is2)*Gamma1(is2,j)
          s(rcmax,2,j) = s(rcmax,2,j) + CMat(2)*Rnew(rcmax,is2)*Gamma1(is2,j)
          IF (NodeSource > 0) THEN
            s(rcmax,3,j) = s(rcmax,3,j) + Rnew(NodeSource,is2)*Gamma1(is2,j)
          END IF
          IF (NodeUserTemp > 0) THEN
            s(rcmax,4,j) = s(rcmax,4,j) + Rnew(NodeUserTemp,is2)*Gamma1(is2,j)
          END IF
        END DO
      END DO
      s(rcmax,1,1) = s(rcmax,1,1)+e(rcmax)*DMat(1)
      s(rcmax,2,2) = s(rcmax,2,2)+e(rcmax)*DMat(2)
      nrf = rcmax
    ELSE ! SolutionDimensions = 2
      DO j = 1, 3
        DO is2 = 1,rcmax
          DO SurfNode = 1, NumOfPerpendNodes
            IF ( (SurfNode == 1) .OR. (SurfNode == NumOfPerpendNodes) ) THEN
              SurfNodeFac = 0.5d0
            ELSE
              SurfNodeFac = 1.0d0
            END IF
            s(rcmax,1,j) = s(rcmax,1,j) + SurfNodeFac*CMat(1)*Rnew(SurfNode,is2)*Gamma1(is2,j)
            s(rcmax,2,j) = s(rcmax,2,j) + SurfNodeFac*CMat(2)*Rnew(rcmax+1-SurfNode,is2)*Gamma1(is2,j)
          END DO
          IF (NodeSource > 0) THEN
            s(rcmax,3,j) = s(rcmax,3,j) + Rnew(NodeSource,is2)*Gamma1(is2,j)
          END IF
          IF (NodeUserTemp > 0) THEN
            s(rcmax,4,j) = s(rcmax,4,j) + Rnew(NodeUserTemp,is2)*Gamma1(is2,j)
          END IF
        END DO
      END DO
      s(rcmax,1,1) = s(rcmax,1,1) + e(rcmax)*DMat(1)*REAL(NumOfPerpendNodes-1,r64)
      s(rcmax,2,2) = s(rcmax,2,2) + e(rcmax)*DMat(2)*REAL(NumOfPerpendNodes-1,r64)
    END IF

          ! Check for and enforce symmetry in the cross term (Y)

    IF (ABS(s(rcmax,1,2)) /= ABS(s(rcmax,2,1))) THEN
      avg = (ABS(s(rcmax,1,2))+ABS(s(rcmax,2,1)))*0.5D0
      s(rcmax,1,2) = avg*s(rcmax,1,2)/ABS(s(rcmax,1,2))
      s(rcmax,2,1) = avg*s(rcmax,2,1)/ABS(s(rcmax,2,1))
    END IF

  END IF    ! ... end of IF block for calculation of last e and S.

  DEALLOCATE(PhiR0)
  DEALLOCATE(Rnew)
  DEALLOCATE(Rold)

  RETURN    ! The array e and the matrices s and s0 now contain the conduction
            ! transfer functions for this construction.

END SUBROUTINE CalculateCTFs

SUBROUTINE ReportCTFs(DoReportBecauseError)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gives a detailed report to the user about
          ! the conduction transfer functions and other thermal data
          ! of each construction.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: ScanForReports

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: DoReportBecauseError

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  CHARACTER(len=12),DIMENSION(6)  :: Roughness = (/'VeryRough   ','Rough       ','MediumRough ', &
!                                                   'MediumSmooth','Smooth      ','VerySmooth  '/)
  LOGICAL :: DoReport

  INTEGER ThisNum
  INTEGER Layer
  INTEGER I

  CALL ScanForReports('Constructions',DoReport,'Constructions')

  IF (DoReport .or. DoReportBecauseError) THEN
!
!                                      Write Descriptions
    Write(OutputFileInits,'(A)') '! <Construction CTF>,Construction Name,Index,#Layers,#CTFs,Time Step {hours},'// &
                             'ThermalConductance {w/m2-K},'// &
                             'OuterThermalAbsorptance,InnerThermalAbsorptance,OuterSolarAbsorptance,'// &
                             'InnerSolarAbsorptance,Roughness'
    Write(OutputFileInits,'(A)') '! <Material CTF Summary>,Material Name,Thickness {m},Conductivity {w/m-K},Density {kg/m3},'//  &
                             'Specific Heat {J/kg-K},ThermalResistance {m2-K/w}'
    Write(OutputFileInits,'(A)') '! <Material:Air>,Material Name,ThermalResistance {m2-K/w}'
    Write(OutputFileInits,'(A)') '! <CTF>,Time,Outside,Cross,Inside,Flux (except final one)'


    DO ThisNum=1,TotConstructs

      IF (Construct(ThisNum)%TypeIsWindow) CYCLE

      Write(OutputFileInits,700) TRIM(Construct(ThisNum)%Name),ThisNum, Construct(ThisNum)%TotLayers,  &
                                 Construct(ThisNum)%NumCTFTerms, &
                                 Construct(ThisNum)%CTFTimeStep,                                                          &
                                 Construct(ThisNum)%UValue,Construct(ThisNum)%OutsideAbsorpThermal,                         &
                                 Construct(ThisNum)%InsideAbsorpThermal,Construct(ThisNum)%OutsideAbsorpSolar,              &
                                 Construct(ThisNum)%InsideAbsorpSolar,  &
                                 TRIM(DisplayMaterialRoughness(Construct(ThisNum)%OutsideRoughness))
!
 700  FORMAT(' Construction CTF,',A,3(',',I4),',',F8.3,',',G15.4,4(',',F8.3),',',A)
 701  FORMAT(' Material CTF Summary,',A,',',F8.4,',',F14.3,',',F11.3,',',F13.3,',',G12.4)
 702  FORMAT(' Material:Air,',A,',',G12.4)
 703  FORMAT(' CTF,',I4,4(',',G20.8))
 704  FORMAT(' CTF,',I4,3(',',G20.8))
 705  FORMAT(' QTF,',I4,2(',',G20.8))
 706  FORMAT(' Source/Sink Loc Internal Temp QTF,',I4,3(',',G20.8))
 707  FORMAT(' User Loc Internal Temp QTF,',I4,3(',',G20.8))

      DO I=1,Construct(ThisNum)%TotLayers
        Layer=Construct(ThisNum)%LayerPoint(I)
        SELECT CASE (Material(Layer)%Group)
          CASE (Air)
            Write(OutputFileInits,702) TRIM(Material(Layer)%Name),Material(Layer)%Resistance
          CASE DEFAULT
            Write(OutputFileInits,701) TRIM(Material(Layer)%Name),Material(Layer)%Thickness,Material(Layer)%Conductivity,  &
                                       Material(Layer)%Density,Material(Layer)%SpecHeat,Material(Layer)%Resistance
        END SELECT
      ENDDO

      DO I=Construct(ThisNum)%NumCTFTerms,0,-1
        IF (I /= 0) THEN
          Write(OutputFileInits,703) I,Construct(ThisNum)%CTFOutside(I),Construct(ThisNum)%CTFCross(I),   &
                                     Construct(ThisNum)%CTFInside(I),Construct(ThisNum)%CTFFlux(I)
        ELSE
          Write(OutputFileInits,704) I,Construct(ThisNum)%CTFOutside(I),Construct(ThisNum)%CTFCross(I),   &
                                     Construct(ThisNum)%CTFInside(I)
        ENDIF
      ENDDO

      IF (Construct(ThisNum)%SourceSinkPresent) THEN
          ! QTFs...
        DO I=Construct(ThisNum)%NumCTFTerms,0,-1
          WRITE(OutputFileInits,705) I,Construct(ThisNum)%CTFSourceOut(I),Construct(ThisNum)%CTFSourceIn(I)
        END DO
          ! QTFs for source/sink location temperature calculation...
        DO I=Construct(ThisNum)%NumCTFTerms,0,-1
            WRITE(OutputFileInits,706) I,Construct(ThisNum)%CTFTSourceOut(I), &
                                         Construct(ThisNum)%CTFTSourceIn(I),  &
                                         Construct(ThisNum)%CTFTSourceQ(I)
        END DO
        IF (Construct(ThisNum)%TempAfterLayer /= 0) THEN
          ! QTFs for user specified interior temperature calculation...
          DO I=Construct(ThisNum)%NumCTFTerms,0,-1
            WRITE(OutputFileInits,707) I,Construct(ThisNum)%CTFTUserOut(I),  &
                                         Construct(ThisNum)%CTFTUserIn(I),   &
                                         Construct(ThisNum)%CTFTUserSource(I)
          END DO
        END IF
      END IF

    ENDDO

  ENDIF

  RETURN

END SUBROUTINE ReportCTFs

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

END MODULE ConductionTransferFunctionCalc
