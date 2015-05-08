MODULE PlantPipingSystemsManager

          ! Module containing the routines dealing with the PipingSystems

          ! MODULE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Simulate all cases of plant "piping systems"
          !      PipingSystem:Underground
          !      PipingSystem:Generalized

          ! METHODOLOGY EMPLOYED:
          ! A 3D mesh is established, with full 3D conduction being employed
          ! For ground simulation, moisture content and soil freezing is included
          ! The mesh can include any number of pipe circuits placed within the domain
          ! The mesh can interact with basement walls also

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataPrecisionGlobals, ONLY: r64
    USE DataInterfaces,       ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError
    USE DataGlobals,          ONLY: MaxNameLength,outputfiledebug
    USE DataPlantPipingSystems

    IMPLICIT NONE ! Enforce explicit typing of all variables

    PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: ObjName_ug_GeneralDomain = 'PipingSystem:Underground:Domain'
    CHARACTER(len=*), PARAMETER :: objName_Circuit          = 'PipingSystem:Underground:PipeCircuit'
    CHARACTER(len=*), PARAMETER :: objName_Segment          = 'PipingSystem:Underground:PipeSegment'
    CHARACTER(len=*), PARAMETER :: objName_HorizTrench      = 'GroundHeatExchanger:HorizontalTrench'

          ! MODULE INTERFACE DEFINITIONS:
    INTERFACE IssueSevereInputFieldError
      MODULE PROCEDURE IssueSevereAlphaInputFieldError
      MODULE PROCEDURE IssueSevereRealInputFieldError
    END INTERFACE

    INTERFACE IsInRange
      MODULE PROCEDURE Integer_IsInRange
      MODULE PROCEDURE Real_IsInRange
    END INTERFACE

          ! DERIVED TYPE DEFINITIONS:
    ! na

          ! MODULE VARIABLE DECLARATIONS:
    INTEGER, ALLOCATABLE, DIMENSION(:) :: NeighborFieldCells
    INTEGER, ALLOCATABLE, DIMENSION(:) :: NeighborBoundaryCells


          ! SUBROUTINE SPECIFICATIONS FOR MODULE:
    ! ************************************* !
    ! Driver/Manager Routines               !
    ! ************************************* !
    !   Public Entry Point                  !
    PUBLIC  SimPipingSystemCircuit          !
    !   Other Management                    !
    PRIVATE GetPipingSystemsInput           !
    PRIVATE ReadGeneralDomainInputs         !
    PRIVATE ReadPipeCircuitInputs           !
    PRIVATE ReadPipeSegmentInputs           !
    PRIVATE InitPipingSystems               !
    PRIVATE UpdatePipingSystems             !
    !   Management/Input workers
    PRIVATE IssueSevereAlphaInputFieldError !
    PRIVATE IssueSevereRealInputFieldError  !
    PRIVATE GetSurfaceCountForOSCM          !
    PRIVATE GetSurfaceIndecesForOSCM        !
    ! ************************************* !
    ! ************************************* !

    ! ******************************************** !
    ! Utility Routines                             !
    ! ******************************************** !
    !   Useful numeric routines                    !
    PRIVATE Integer_IsInRange                      !
    PRIVATE Real_IsInRange                         !
    PRIVATE Real_ConstrainTo                       !
    !   Extensions for data classes                !
    PRIVATE CellType_IsFieldCell                   !
    PRIVATE MeshPartitionArray_Contains            !
    PRIVATE RadialCellInfo_XY_CrossSectArea        !
    PRIVATE DomainRectangle_Contains               !
    PRIVATE MeshPartition_SelectionSort            !
    PRIVATE MeshPartition_CompareByDimension       !
    PRIVATE BaseThermalPropertySet_Diffusivity     !
    PRIVATE RectangleF_Contains                    !
    PRIVATE RadialSizing_Thickness                 !
    PRIVATE PipeSegmentInfo_InitPipeCells          !
    PRIVATE PipeCircuitInfo_InitInOutCells         !
    !   Convergence checks                         !
    PRIVATE IsConverged_CurrentToPrevIteration     !
    PRIVATE IsConverged_PipeCurrentToPrevIteration !
    !   Array shifting                             !
    PRIVATE ShiftTemperaturesForNewTimeStep        !
    PRIVATE ShiftTemperaturesForNewIteration       !
    PRIVATE ShiftPipeTemperaturesForNewIteration   !
    !   Error checking                             !
    PRIVATE CheckForOutOfRangeTemps                !
    !   Other utilities                            !
    !   Cartesian cell property routines           !
    PRIVATE Width                                  !
    PRIVATE Height                                 !
    PRIVATE Depth                                  !
    PRIVATE XNormalArea                            !
    PRIVATE YNormalArea                            !
    PRIVATE ZNormalArea                            !
    PRIVATE Volume                                 !
    PRIVATE XYRectangle                            !
    PRIVATE XZRectangle                            !
    PRIVATE YZRectangle                            !
    PRIVATE NormalArea                             !
    PRIVATE NeighborInformationArray_Value         !
    !   Class "constructors"                       !
    PRIVATE CartesianPipeCellInformation_ctor      !
    PRIVATE RadialCellInformation_ctor             !
    PRIVATE FluidCellInformation_ctor              !
    ! ******************************************** !
    ! ******************************************** !

    ! ***************************************** !
    ! Simulation Algorithms                     !
    ! ***************************************** !
    !   Mesh Development routines               !
    PRIVATE DevelopMesh                         !
    PRIVATE CreatePartitionCenterList           !
    PRIVATE CreatePartitionRegionList           !
    PRIVATE CreateRegionListCount               !
    PRIVATE CreateRegionList                    !
    PRIVATE CreateBoundaryListCount             !
    PRIVATE CreateBoundaryList                  !
    PRIVATE CreateCellArray                     !
    PRIVATE SetupCellNeighbors                  !
    PRIVATE AddNeighborInformation              !
    PRIVATE SetupPipeCircuitInOutCells          !
    PRIVATE GetCellWidthsCount                  !
    PRIVATE GetCellWidths                       !
    !   Simulation algorithms                   !
    PRIVATE PerformIterationLoop                !
    PRIVATE PerformTemperatureFieldUpdate       !
    PRIVATE EvaluateFieldCellTemperature        !
    PRIVATE EvaluateGroundSurfaceTemperature    !
    PRIVATE EvaluateAdiabaticSurfaceTemperature !
    PRIVATE EvaluateBasementCellTemperature     !
    PRIVATE GetBasementWallHeatFlux             !
    PRIVATE GetBasementFloorHeatFlux            !
    PRIVATE UpdateBasementSurfaceTemperatures   !
    PRIVATE GetAverageTempByType                !
    PRIVATE EvaluateFarfieldBoundaryTemperature !
    PRIVATE EvaluateFarfieldCharacteristics     !
    PRIVATE GetFarfieldTemp                     !
    PRIVATE PreparePipeCircuitSimulation        !
    PRIVATE PerformPipeCircuitSimulation        !
    PRIVATE PerformPipeCellSimulation           !
    PRIVATE SimulateRadialToCartesianInterface  !
    PRIVATE SimulateOuterMostRadialSoilSlice    !
    PRIVATE SimulateAllInteriorRadialSoilSlices !
    PRIVATE SimulateInnerMostRadialSoilSlice    !
    PRIVATE SimulateRadialInsulationCell        !
    PRIVATE SimulateRadialPipeCell              !
    PRIVATE SimulateFluidCell                   !
    PRIVATE DoOneTimeInitializations            !
    PRIVATE DoStartOfTimeSTepInitializations    !
    PRIVATE DoEndOfIterationOperations          !
    PRIVATE EvaluateSoilRhoCp                   !
    PRIVATE SetAdditionalNeighborData           !
    PRIVATE EvaluateNeighborCharacteristics     !
    PRIVATE EvaluateCellNeighborDirections      !
    ! ***************************************** !
    ! ***************************************** !

    CONTAINS

!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimPipingSystemCircuit(EquipName, EqNum, FirstHVACIteration, InitLoopEquip)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,                  ONLY: FindItemInList
    USE General,                         ONLY: TrimSigDigits

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN)    :: EquipName        ! name of the Pipe Heat Transfer.
    INTEGER,          INTENT(INOUT) :: EqNum            ! index in local derived types for external calling
    LOGICAL,          INTENT(IN)    :: FirstHVACIteration       ! component number
    LOGICAL,          INTENT(IN)    :: InitLoopEquip

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName = 'SimPipingSystems'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL,SAVE  :: GetInputFlag = .TRUE.  ! First time, input is "gotten"
    INTEGER :: CircuitNum
    INTEGER :: DomainNum
    INTEGER :: NumOfPipeCircuits

    !Read input if necessary
    IF (GetInputFlag) THEN
        CALL GetPipingSystemsInput()
        GetInputFlag = .FALSE.
    ENDIF

    !Look for circuit index
    NumOfPipeCircuits = SIZE(PipingSystemCircuits)
    IF (EqNum == 0) THEN
        CircuitNum = FindItemInList(EquipName,PipingSystemCircuits%Name,NumOfPipeCircuits)
        IF (CircuitNum == 0) THEN
            ! Catch any bad names before crashing
            CALL ShowFatalError(RoutineName//': Piping circuit requested not found='//TRIM(EquipName))
        ENDIF
        EqNum=CircuitNum
    ELSE
        CircuitNum=EqNum
        IF (CircuitNum > NumOfPipeCircuits .or. CircuitNum < 1) THEN
            CALL ShowFatalError(RoutineName//':  Invalid component index passed='//  &
                              TRIM(TrimSigDigits(DomainNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumOfPipeCircuits))//  &
                              ', Entered Unit name='//TRIM(EquipName))
        ENDIF
        IF (PipingSystemCircuits(CircuitNum)%CheckEquipName) THEN
            IF (EquipName /= PipingSystemCircuits(CircuitNum)%Name) THEN
                CALL ShowFatalError(RoutineName//': Invalid component name passed='//  &
                                TRIM(TrimSigDigits(CircuitNum))// &
                                ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                                TRIM(PipingSystemCircuits(CircuitNum)%Name))
            ENDIF
            PipingSystemCircuits(CircuitNum)%CheckEquipName=.false.
        ENDIF
    ENDIF

    !If we are just initializing data structures, then return
    IF(InitLoopEquip) RETURN

    !Retrieve the parent domain index for this pipe circuit
    DomainNum = PipingSystemCircuits(CircuitNum)%ParentDomainIndex

    !Do any initialization here
    CALL InitPipingSystems(DomainNum, CircuitNum)

    !Update the temperature field
    CALL PerformIterationLoop(DomainNum, CircuitNum)

    !Update outlet nodes, etc.
    CALL UpdatePipingSystems(DomainNum, CircuitNum)

  RETURN

END SUBROUTINE SimPipingSystemCircuit
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE GetPipingSystemsInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY : GetNumObjectsFound, FindItemInList, SameString
    USE DataGlobals,    ONLY : MaxNameLength
    USE DataInterfaces, ONLY : SetupOutputVariable
    USE General, ONLY : TrimSigDigits

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='GetPipingSystemsInput'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL                      :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
    INTEGER                      :: NumGeneralizedDomains
    INTEGER                      :: PipeCtr
    INTEGER                      :: CircuitCtr
    INTEGER                      :: CircuitIndex
    INTEGER                      :: ThisSegmentIndex
    INTEGER                      :: NumPipeCircuits
    INTEGER                      :: NumPipeSegmentsInInput
    INTEGER                      :: NumCircuitsInThisDomain
    INTEGER                      :: NumHorizontalTrenches
    INTEGER                      :: NumSegmentsInHorizontalTrenches
    INTEGER                      :: DomainNum
    INTEGER                      :: TotalNumDomains
    INTEGER                      :: TotalNumCircuits
    INTEGER                      :: TotalNumSegments
    INTEGER                      :: ThisCircuitPipeSegmentCounter
    CHARACTER(len=MaxNameLength) :: ThisSegmentName
    INTEGER                      :: InputPipeSegmentCounter

    !Read number of objects and allocate main data structures - first domains
    NumGeneralizedDomains=GetNumObjectsFound(ObjName_ug_GeneralDomain)
    NumHorizontalTrenches=GetNumObjectsFound(objName_HorizTrench)
    TotalNumDomains = NumGeneralizedDomains + NumHorizontalTrenches
    ALLOCATE(PipingSystemDomains(TotalNumDomains))

    ! then circuits
    NumPipeCircuits=GetNumObjectsFound(ObjName_Circuit)
    TotalNumCircuits = NumPipeCircuits + NumHorizontalTrenches
    ALLOCATE(PipingSystemCircuits(TotalNumCircuits))

    ! then segments
    NumPipeSegmentsInInput = GetNumObjectsFound(objName_Segment)
    NumSegmentsInHorizontalTrenches = GetNumSegmentsForHorizontalTrenches(NumHorizontalTrenches)
    TotalNumSegments = NumPipeSegmentsInInput + NumSegmentsInHorizontalTrenches
    ALLOCATE(PipingSystemSegments(TotalNumSegments))

    !Read in raw inputs, don't try to interpret dependencies yet
    CALL ReadGeneralDomainInputs(1, NumGeneralizedDomains, ErrorsFound)
    CALL ReadPipeCircuitInputs(NumPipeCircuits, ErrorsFound)
    CALL ReadPipeSegmentInputs(NumPipeSegmentsInInput, ErrorsFound)
    CALL ReadHorizontalTrenchInputs(NumGeneralizedDomains+1, NumPipeCircuits+1, NumPipeSegmentsInInput+1, &
                                                                               NumHorizontalTrenches, ErrorsFound)

    !Report errors that are purely input problems
    IF (ErrorsFound) CALL ShowFatalError(RoutineName//': Preceding input errors cause program termination.')

    !Setup output variables
    CALL SetupAllOutputVariables(TotalNumSegments, TotalNumCircuits)

    !Validate CIRCUIT-SEGMENT cross references
    DO CircuitCtr = LBOUND(PipingSystemCircuits, 1), UBOUND(PipingSystemCircuits, 1)

        !validate circuit-segment name-to-index references
        DO ThisCircuitPipeSegmentCounter = LBOUND(PipingSystemCircuits(CircuitCtr)%PipeSegmentNames, 1), &
                                           UBOUND(PipingSystemCircuits(CircuitCtr)%PipeSegmentNames, 1)

            ThisSegmentName = PipingSystemCircuits(CircuitCtr)%PipeSegmentNames(ThisCircuitPipeSegmentCounter)
            ThisSegmentIndex = FindItemInList(ThisSegmentName,PipingSystemSegments%Name,TotalNumSegments)
            IF (ThisSegmentIndex > 0) THEN
                PipingSystemCircuits(CircuitCtr)%PipeSegmentIndeces(ThisCircuitPipeSegmentCounter) = ThisSegmentIndex
                PipingSystemSegments(ThisSegmentIndex)%ParentCircuitIndex = CircuitCtr
            ELSE
                CALL ShowSevereError(RoutineName//': Could not match a pipe segment for: '// &
                                     TRIM(ObjName_Circuit)//'='//TRIM(PipingSystemCircuits(CircuitCtr)%Name))
                CALL ShowContinueError(RoutineName//': Looking for: '//TRIM(objName_Segment)//'='//TRIM(ThisSegmentName))
                ErrorsFound = .TRUE.
            END IF

        END DO !Segment loop

    END DO !Circuit loop

    !Validate DOMAIN-CIRCUIT cross references
    DO DomainNum = 1, TotalNumDomains

        !Convenience
        NumCircuitsInThisDomain = SIZE(PipingSystemDomains(DomainNum)%CircuitNames)

        !validate pipe domain-circuit name-to-index references
        DO CircuitCtr = 1, NumCircuitsInThisDomain
            CircuitIndex = FindItemInList(PipingSystemDomains(DomainNum)%CircuitNames(CircuitCtr), &
                                          PipingSystemCircuits%Name,SIZE(PipingSystemCircuits))
            PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitCtr) = CircuitIndex
            PipingSystemCircuits(CircuitIndex)%ParentDomainIndex = DomainNum
        END DO

        !correct segment locations for: INTERNAL DATA STRUCTURE Y VALUE MEASURED FROM BOTTOM OF DOMAIN,
        !                               INPUT WAS MEASURED FROM GROUND SURFACE
        DO CircuitCtr = 1, NumCircuitsInThisDomain
            CircuitIndex = PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitCtr)
            DO PipeCtr = LBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1), &
                         UBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1)
                ThisSegmentIndex = PipingSystemCircuits(CircuitCtr)%PipeSegmentIndeces(PipeCtr)
                PipingSystemSegments(ThisSegmentIndex)%PipeLocation%Y = &
                       PipingSystemDomains(DomainNum)%Extents%Ymax - PipingSystemSegments(ThisSegmentIndex)%PipeLocation%Y
            END DO !segment loop
        END DO !circuit loop

        !correct segment locations for: BASEMENT X SHIFT
        IF (PipingSystemDomains(DomainNum)%HasBasement .AND. PipingSystemDomains(DomainNum)%BasementZone%ShiftPipesByWidth) THEN
            DO CircuitCtr = 1, NumCircuitsInThisDomain
                CircuitIndex = PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitCtr)
                DO PipeCtr = LBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1), &
                             UBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1)
                    ThisSegmentIndex = PipingSystemCircuits(CircuitCtr)%PipeSegmentIndeces(PipeCtr)
                    PipingSystemSegments(ThisSegmentIndex)%PipeLocation%X = &
                           PipingSystemDomains(DomainNum)%BasementZone%Width + PipingSystemSegments(ThisSegmentIndex)%PipeLocation%X
                END DO !segment loop
            END DO !circuit loop
        END IF

        !now we will have good values of pipe segment locations, we can validate them
        DO CircuitCtr = 1, NumCircuitsInThisDomain

            !retrieve the index
            CircuitIndex = PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitCtr)

            !check to make sure it isn't outside the domain
            DO PipeCtr = LBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1), &
                         UBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1)
                ThisSegmentIndex = PipingSystemCircuits(CircuitCtr)%PipeSegmentIndeces(PipeCtr)
                IF ((PipingSystemSegments(ThisSegmentIndex)%PipeLocation%X > PipingSystemDomains(DomainNum)%Extents%Xmax) .OR. &
                    (PipingSystemSegments(ThisSegmentIndex)%PipeLocation%X < 0.0d0) .OR. &
                    (PipingSystemSegments(ThisSegmentIndex)%PipeLocation%Y > PipingSystemDomains(DomainNum)%Extents%Ymax) .OR. &
                    (PipingSystemSegments(ThisSegmentIndex)%PipeLocation%Y < 0.0d0)) THEN
                    CALL ShowSevereError('PipingSystems::'//RoutineName//':A pipe was found to be outside of the domain extents'//&
                                                                 ' after performing any corrections for basement or burial depth.')
                    CALL ShowContinueError('Pipe segment name:'//TRIM(PipingSystemSegments(ThisSegmentIndex)%Name))
                    CALL ShowContinueError('Corrected pipe location: (x,y)=('// &
                                       TrimSigDigits(PipingSystemSegments(ThisSegmentIndex)%PipeLocation%X,2)//','// &
                                       TrimSigDigits(PipingSystemSegments(ThisSegmentIndex)%PipeLocation%Y,2)//')')
                END IF
            END DO !segment loop

        END DO !circuit loop

    END DO !domain loop

    !If we encountered any other errors that we couldn't handle separately than stop now
    IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//':'//ObjName_ug_GeneralDomain//': Errors found in input.')
    ENDIF

  RETURN

END SUBROUTINE GetPipingSystemsInput
!*********************************************************************************************!

!*********************************************************************************************!
INTEGER FUNCTION GetNumSegmentsForHorizontalTrenches(NumHorizontalTrenches) RESULT(Total)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetObjectItem
    USE DataIPShortCuts

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: NumHorizontalTrenches

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: HorizontalCtr
    INTEGER :: NumPipesInThisHorizontal
    INTEGER :: NumAlphas, NumNumbers
    INTEGER :: IOStatus

    Total = 0

    DO HorizontalCtr = 1, NumHorizontalTrenches

        !Set up all the inputs for this domain object
        CALL GetObjectItem(objName_HorizTrench,HorizontalCtr,cAlphaArgs,NumAlphas, &
                             rNumericArgs,NumNumbers,IOStatus,  &
                             AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        NumPipesInThisHorizontal = rNumericArgs(3)

        Total = Total + NumPipesInThisHorizontal

    END DO

  RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ReadGeneralDomainInputs(IndexStart, NumGeneralizedDomains, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetObjectItem, FindItemInList, SameString, VerifyName, MakeUPPERCase
    USE DataIPShortCuts
    USE DataSurfaces,     ONLY: Surface, OSCM, TotOSCM, TotSurfaces, OtherSideCondModeledExt !not sure if we need all these...
    USE General, ONLY: TrimSigDigits
    USE DataGlobals, ONLY: SecsInDay

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)     :: IndexStart
    INTEGER, INTENT(IN)     :: NumGeneralizedDomains
    LOGICAL, INTENT(IN OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='ReadGeneralDomainInputs'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: DomainNum    ! Item to be "gotten"
    INTEGER :: NumAlphas  ! Number of Alphas for each GetObjectItem call
    INTEGER :: NumNumbers ! Number of Numbers for each GetObjectItem call
    INTEGER :: IOStatus   ! Used in GetObjectItem
    INTEGER :: NumCircuitsInThisDomain
    INTEGER :: CircuitCtr
    LOGICAL :: BasementInputError
    INTEGER :: NumSurfacesWithThisOSCM
    INTEGER :: NumAlphasBeforePipeCircOne
    INTEGER :: CurIndex
    LOGICAL :: IsBlank
    LOGICAL :: IsNotOK


    DO DomainNum = IndexStart, NumGeneralizedDomains

        !Set up all the inputs for this domain object
        CALL GetObjectItem(ObjName_ug_GeneralDomain,DomainNum,cAlphaArgs,NumAlphas, &
                             rNumericArgs,NumNumbers,IOStatus,  &
                             AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        !Get the name, validate
        PipingSystemDomains(DomainNum)%Name = cAlphaArgs(1)
        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(cAlphaArgs(1),PipingSystemDomains%Name,DomainNum-1,IsNotOK,IsBlank,TRIM(ObjName_ug_GeneralDomain)//' Name')
        IF (IsNotOK) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Duplicate name encountered'
        ELSEIF (IsBlank) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Blank name encountered'
        ENDIF

        !Mesh extents, validated by IP
        PipingSystemDomains(DomainNum)%Extents%Xmax = rNumericArgs(1)
        PipingSystemDomains(DomainNum)%Extents%Ymax = rNumericArgs(2)
        PipingSystemDomains(DomainNum)%Extents%Zmax = rNumericArgs(3)

        !X direction mesh inputs, validated by IP
        PipingSystemDomains(DomainNum)%Mesh%X%RegionMeshCount  = rNumericArgs(4)
        SELECT CASE (MakeUPPERCase(cAlphaArgs(2)))
        CASE ('UNIFORM')
            PipingSystemDomains(DomainNum)%Mesh%X%MeshDistribution = MeshDistribution_Uniform
        CASE ('SYMMETRICGEOMETRIC')
            PipingSystemDomains(DomainNum)%Mesh%X%MeshDistribution = MeshDistribution_SymmetricGeometric
            IF (MOD(PipingSystemDomains(DomainNum)%Mesh%X%RegionMeshCount, 2) .NE. 0) THEN
                CALL ShowWarningError('PipingSystems:'//RoutineName//': Invalid mesh type-count combination.')
                CALL ShowContinueError('Instance:'//ObjName_ug_GeneralDomain//'='//TRIM(PipingSystemDomains(DomainNum)%Name))
                CALL ShowContinueError('An ODD-valued X mesh count was found in the input for symmetric geometric configuration.')
                CALL ShowContinueError('This is invalid, mesh count incremented UP by one to next EVEN value.')
                PipingSystemDomains(DomainNum)%Mesh%X%RegionMeshCount = PipingSystemDomains(DomainNum)%Mesh%X%RegionMeshCount + 1
                PipingSystemDomains(DomainNum)%Mesh%X%GeometricSeriesCoefficient = rNumericArgs(5)
            ELSE
                PipingSystemDomains(DomainNum)%Mesh%X%GeometricSeriesCoefficient = 1.0d0
            END IF
        CASE DEFAULT
            CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(2), &
                                            cAlphaArgs(2), 'Use a choice from the available mesh type keys.', ErrorsFound)
        END SELECT

        !Y direction mesh inputs, validated by IP
        PipingSystemDomains(DomainNum)%Mesh%Y%RegionMeshCount  = rNumericArgs(6)
        SELECT CASE (TRIM(ADJUSTL(cAlphaArgs(3))))
        CASE ('UNIFORM')
            PipingSystemDomains(DomainNum)%Mesh%Y%MeshDistribution = MeshDistribution_Uniform
        CASE ('SYMMETRICGEOMETRIC')
            PipingSystemDomains(DomainNum)%Mesh%Y%MeshDistribution = MeshDistribution_SymmetricGeometric
            IF (MOD(PipingSystemDomains(DomainNum)%Mesh%Y%RegionMeshCount, 2) .NE. 0) THEN
                CALL ShowWarningError('PipingSystems:'//RoutineName//': Invalid mesh type-count combination.')
                CALL ShowContinueError('Instance:'//ObjName_ug_GeneralDomain//'='//TRIM(PipingSystemDomains(DomainNum)%Name))
                CALL ShowContinueError('An ODD-valued Y mesh count was found in the input for symmetric geometric configuration.')
                CALL ShowContinueError('This is invalid, mesh count incremented UP by one to next EVEN value.')
                PipingSystemDomains(DomainNum)%Mesh%Y%RegionMeshCount = PipingSystemDomains(DomainNum)%Mesh%Y%RegionMeshCount + 1
                PipingSystemDomains(DomainNum)%Mesh%Y%GeometricSeriesCoefficient = rNumericArgs(7)
            ELSE
                PipingSystemDomains(DomainNum)%Mesh%Y%GeometricSeriesCoefficient = 1.0d0
            END IF
        CASE DEFAULT
            CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(3), &
                                            cAlphaArgs(3), 'Use a choice from the available mesh type keys.', ErrorsFound)
        END SELECT

        !Z direction mesh inputs, validated by IP
        PipingSystemDomains(DomainNum)%Mesh%Z%RegionMeshCount  = rNumericArgs(8)
        SELECT CASE (TRIM(ADJUSTL(cAlphaArgs(4))))
        CASE ('UNIFORM')
            PipingSystemDomains(DomainNum)%Mesh%Z%MeshDistribution = MeshDistribution_Uniform
        CASE ('SYMMETRICGEOMETRIC')
            PipingSystemDomains(DomainNum)%Mesh%Z%MeshDistribution = MeshDistribution_SymmetricGeometric
            IF (MOD(PipingSystemDomains(DomainNum)%Mesh%Z%RegionMeshCount, 2) .NE. 0) THEN
                CALL ShowWarningError('PipingSystems:'//RoutineName//': Invalid mesh type-count combination.')
                CALL ShowContinueError('Instance:'//ObjName_ug_GeneralDomain//'='//TRIM(PipingSystemDomains(DomainNum)%Name))
                CALL ShowContinueError('An ODD-valued Z mesh count was found in the input for symmetric geometric configuration.')
                CALL ShowContinueError('This is invalid, mesh count incremented UP by one to next EVEN value.')
                PipingSystemDomains(DomainNum)%Mesh%Z%RegionMeshCount = PipingSystemDomains(DomainNum)%Mesh%Z%RegionMeshCount + 1
                PipingSystemDomains(DomainNum)%Mesh%Z%GeometricSeriesCoefficient = rNumericArgs(9)
            ELSE
                PipingSystemDomains(DomainNum)%Mesh%Z%GeometricSeriesCoefficient = 1.0d0
            END IF
        CASE DEFAULT
            CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(4), &
                                            cAlphaArgs(4), 'Use a choice from the available mesh type keys.', ErrorsFound)
        END SELECT

        !Soil properties, validated min/max by IP
        PipingSystemDomains(DomainNum)%GroundProperties%Conductivity = rNumericArgs(10)
        PipingSystemDomains(DomainNum)%GroundProperties%Density = rNumericArgs(11)
        PipingSystemDomains(DomainNum)%GroundProperties%SpecificHeat = rNumericArgs(12)

        !Moisture properties, validated min/max by IP, and converted to a fraction for computation here
        PipingSystemDomains(DomainNum)%Moisture%Theta_Liq = rNumericArgs(13) / 100.0d0
        PipingSystemDomains(DomainNum)%Moisture%Theta_Sat = rNumericArgs(14) / 100.0d0

        !Farfield model parameters, validated min/max by IP
        PipingSystemDomains(DomainNum)%Farfield%AverageGroundTemperature = rNumericArgs(15)
        PipingSystemDomains(DomainNum)%Farfield%AverageGroundTemperatureAmplitude = rNumericArgs(16)
        PipingSystemDomains(DomainNum)%Farfield%PhaseShiftOfMinGroundTempDays = rNumericArgs(17)

        !Unit conversion
        PipingSystemDomains(DomainNum)%Farfield%PhaseShiftOfMinGroundTemp = &
                    PipingSystemDomains(DomainNum)%Farfield%PhaseShiftOfMinGroundTempDays * SecsInDay

        !check if there is a basement
        IF (SameString(cAlphaArgs(5), 'YES')) THEN
            PipingSystemDomains(DomainNum)%HasBasement = .TRUE.
        ELSEIF (SameString(cAlphaArgs(5), 'NO')) THEN
            PipingSystemDomains(DomainNum)%HasBasement = .FALSE.
        ELSE
            CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(5), &
                                            cAlphaArgs(5), 'Must enter either yes or no.', ErrorsFound)
        END IF

        !more work to do if there is a basement
        IF (PipingSystemDomains(DomainNum)%HasBasement) THEN

            ! check if there are blank inputs related to the basement,
            ! IP can't catch this because they are inherently optional if there ISN'T a basement
            IF (       lNumericFieldBlanks(18) &
                  .OR. lNumericFieldBlanks(19) &
                  .OR. lAlphaFieldBlanks(6)    &
                  .OR. lAlphaFieldBlanks(7)    &
                  .OR. lAlphaFieldBlanks(8)    ) THEN
                CALL ShowSevereError('Erroneous basement inputs for '//TRIM(ObjName_ug_GeneralDomain)//'='//TRIM(cAlphaArgs(1)))
                CALL ShowContinueError('Object specified to have a basement, while at least one basement input was left blank.')
                ErrorsFound = .TRUE.
            END IF

            ! get dimensions for meshing
            CurIndex = 18
            PipingSystemDomains(DomainNum)%BasementZone%Width = rNumericArgs(CurIndex)
            IF (PipingSystemDomains(DomainNum)%BasementZone%Width <= 0.0d0) THEN
                CALL IssueSevereInputFieldError(RoutineName,ObjName_ug_GeneralDomain,cAlphaArgs(1),cNumericFieldNames(CurIndex), &
                                           rNumericArgs(CurIndex), 'Basement width must be a positive nonzero value.', ErrorsFound)
            END IF

            CurIndex = 19
            PipingSystemDomains(DomainNum)%BasementZone%Depth = rNumericArgs(CurIndex)
            IF (PipingSystemDomains(DomainNum)%BasementZone%Depth <= 0.0d0) THEN
                CALL IssueSevereInputFieldError(RoutineName,ObjName_ug_GeneralDomain,cAlphaArgs(1),cNumericFieldNames(CurIndex), &
                                           rNumericArgs(CurIndex), 'Basement depth must be a positive nonzero value.', ErrorsFound)
            END IF

            ! check for dimension shift
            CurIndex = 6
            IF (SameString(cAlphaArgs(CurIndex), 'YES')) THEN
                PipingSystemDomains(DomainNum)%BasementZone%ShiftPipesByWidth = .TRUE.
            ELSEIF (SameString(cAlphaArgs(CurIndex), 'NO')) THEN
                PipingSystemDomains(DomainNum)%BasementZone%ShiftPipesByWidth = .FALSE.
            ELSE
                CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                                                cAlphaArgs(CurIndex), 'Must enter either yes or no.', ErrorsFound)
            END IF

            ! get boundary condition model names and indeces --error check
            CurIndex = 7
            PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMName = cAlphaArgs(CurIndex)
            PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMIndex = &
                FindItemInList(PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMName,OSCM%Name,TotOSCM)
            IF (PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMIndex <= 0) THEN
                CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                            cAlphaArgs(CurIndex), 'Could not match with an Other Side Conditions Model input object.', ErrorsFound)
            ELSE
                NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM(PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMIndex)
                IF (NumSurfacesWithThisOSCM <= 0) THEN
                    CALL IssueSevereInputFieldError(RoutineName,ObjName_ug_GeneralDomain,cAlphaArgs(1),cAlphaFieldNames(CurIndex),&
                               cAlphaArgs(CurIndex), 'Entry matched an Other Side Conditions Model, but no surfaces were found'// &
                                                     ' to be using this Other Side Conditions Model.', ErrorsFound)
                ELSE
                    ALLOCATE(PipingSystemDomains(DomainNum)%BasementZone%WallSurfacePointers(NumSurfacesWithThisOSCM))
                    PipingSystemDomains(DomainNum)%BasementZone%WallSurfacePointers = &
                        GetSurfaceIndecesForOSCM(PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMIndex, &
                                                 NumSurfacesWithThisOSCM)
                END IF
            END IF

            CurIndex = 8
            PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMName = cAlphaArgs(CurIndex)
            PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMIndex = &
                FindItemInList(PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMName,OSCM%Name,TotOSCM)
            IF (PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMIndex <= 0) THEN
                CALL IssueSevereInputFieldError(RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                            cAlphaArgs(CurIndex), 'Could not match with an Other Side Conditions Model input object.', ErrorsFound)
            ELSE
                NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM(PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMIndex)
                IF (NumSurfacesWithThisOSCM <= 0) THEN
                    CALL IssueSevereInputFieldError(RoutineName,ObjName_ug_GeneralDomain,cAlphaArgs(1),cAlphaFieldNames(CurIndex),&
                               cAlphaArgs(CurIndex), 'Entry matched an Other Side Conditions Model, but no surfaces were found'// &
                                                     ' to be using this Other Side Conditions Model.', ErrorsFound)
                ELSE
                    ALLOCATE(PipingSystemDomains(DomainNum)%BasementZone%FloorSurfacePointers(NumSurfacesWithThisOSCM))
                    PipingSystemDomains(DomainNum)%BasementZone%FloorSurfacePointers = &
                        GetSurfaceIndecesForOSCM(PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMIndex, &
                                                 NumSurfacesWithThisOSCM)
                END IF
            END IF


        END IF

        !get some convergence tolerances, minimum/maximum are enforced by the IP, along with default values if user left them blank
        PipingSystemDomains(DomainNum)%SimControls%Convergence_CurrentToPrevIteration = rNumericArgs(20)
        PipingSystemDomains(DomainNum)%SimControls%MaxIterationsPerTS = rNumericArgs(21)

        !additional evapotranspiration parameter, min/max validated by IP
        PipingSystemDomains(DomainNum)%Moisture%GroundCoverCoefficient = rNumericArgs(22)

        !Allocate the circuit placeholder arrays
        NumCircuitsInThisDomain = INT(rNumericArgs(23))
        ALLOCATE(PipingSystemDomains(DomainNum)%CircuitNames(NumCircuitsInThisDomain))
        ALLOCATE(PipingSystemDomains(DomainNum)%CircuitIndeces(NumCircuitsInThisDomain))

        !Check for blank or missing or mismatched number...
        NumAlphasBeforePipeCircOne = 8
        DO CircuitCtr = 1, NumCircuitsInThisDomain
            PipingSystemDomains(DomainNum)%CircuitNames(CircuitCtr) = cAlphaArgs(CircuitCtr + NumAlphasBeforePipeCircOne)
        END DO

    ENDDO

  RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ReadPipeCircuitInputs(NumPipeCircuits, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetObjectItem, VerifyName
    USE DataIPShortCuts
    USE DataLoopNode
    USE NodeInputManager,      ONLY : GetOnlySingleNode
    USE BranchNodeConnections, ONLY : TestCompSet
    USE DataPlant, ONLY: TypeOf_PipingSystemPipeCircuit
    USE DataInterfaces, ONLY : SetupOutputVariable

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)     :: NumPipeCircuits
    LOGICAL, INTENT(IN OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='ReadPipeCircuitInputs'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: NumPipeSegments
    INTEGER :: NumAlphas
    INTEGER :: NumNumbers
    INTEGER :: IOStatus
    INTEGER :: PipeCircuitCounter
    INTEGER :: ThisCircuitPipeSegmentCounter
    LOGICAL :: IsNotOK
    LOGICAL :: IsBlank
    INTEGER :: CurIndex
    INTEGER :: NumAlphasBeforeSegmentOne

    DO PipeCircuitCounter = 1, NumPipeCircuits

        !Read all the inputs for this pipe circuit
        CALL GetObjectItem(ObjName_Circuit,PipeCircuitCounter,cAlphaArgs,NumAlphas, &
                             rNumericArgs,NumNumbers,IOStatus,  &
                             AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        !Get the name, validate
        PipingSystemCircuits(PipeCircuitCounter)%Name = cAlphaArgs(1)
        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(cAlphaArgs(1), PipingSystemCircuits%Name, PipeCircuitCounter-1, &
                        IsNotOK, IsBlank, TRIM(ObjName_Circuit)//' Name')
        IF (IsNotOK) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Duplicate name encountered'
        ELSEIF (IsBlank) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Blank name encountered'
        ENDIF

        !Read pipe thermal properties, validated by IP
        PipingSystemCircuits(PipeCircuitCounter)%PipeProperties%Conductivity = rNumericArgs(1)
        PipingSystemCircuits(PipeCircuitCounter)%PipeProperties%Density = rNumericArgs(2)
        PipingSystemCircuits(PipeCircuitCounter)%PipeProperties%SpecificHeat = rNumericArgs(3)

        !Read pipe sizing, validated individually by IP, validated comparison here
        PipingSystemCircuits(PipeCircuitCounter)%PipeSize%InnerDia = rNumericArgs(4)
        PipingSystemCircuits(PipeCircuitCounter)%PipeSize%OuterDia = rNumericArgs(5)
        IF (   PipingSystemCircuits(PipeCircuitCounter)%PipeSize%InnerDia >= &
               PipingSystemCircuits(PipeCircuitCounter)%PipeSize%OuterDia) THEN
            CurIndex = 5
            CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                                          cAlphaArgs(CurIndex), 'Outer diameter must be greater than inner diameter.', ErrorsFound)
        END IF

        !Read design flow rate, validated positive by IP
        PipingSystemCircuits(PipeCircuitCounter)%DesignVolumeFlowRate = rNumericArgs(6)

        !Read inlet and outlet node names and validate them
        PipingSystemCircuits(PipeCircuitCounter)%InletNodeName = cAlphaArgs(2)
        PipingSystemCircuits(PipeCircuitCounter)%InletNodeNum  = GetOnlySingleNode(cAlphaArgs(2),ErrorsFound, &
                                                                   TRIM(ObjName_Circuit),cAlphaArgs(1), &
                                                                   NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
        IF (PipingSystemCircuits(PipeCircuitCounter)%InletNodeNum == 0) THEN
            CurIndex = 2
            CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                                            cAlphaArgs(CurIndex), 'Bad node name.', ErrorsFound)
        END IF
        PipingSystemCircuits(PipeCircuitCounter)%OutletNodeName = cAlphaArgs(3)
        PipingSystemCircuits(PipeCircuitCounter)%OutletNodeNum  = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound, &
                                                                    TRIM(ObjName_Circuit),cAlphaArgs(1), &
                                                                    NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
        IF (PipingSystemCircuits(PipeCircuitCounter)%OutletNodeNum == 0) THEN
            CurIndex = 3
            CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                                            cAlphaArgs(CurIndex), 'Bad node name.', ErrorsFound)
        END IF
        CALL TestCompSet(TRIM(ObjName_Circuit),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Piping System Circuit Nodes')

        !Convergence tolerance values, validated by IP
        PipingSystemCircuits(PipeCircuitCounter)%Convergence_CurrentToPrevIteration = rNumericArgs(7)
        PipingSystemCircuits(PipeCircuitCounter)%MaxIterationsPerTS = rNumericArgs(8)

        !Radial mesh inputs, validated by IP
        ! -- mesh thickness should be considered slightly dangerous until mesh dev engine can trap erroneous values
        PipingSystemCircuits(PipeCircuitCounter)%NumRadialCells = rNumericArgs(9)
        PipingSystemCircuits(PipeCircuitCounter)%RadialMeshThickness = rNumericArgs(10)

        !Read number of pipe segments for this circuit, allocate arrays
        NumPipeSegments = rNumericArgs(11)
        ALLOCATE(PipingSystemCircuits(PipeCircuitCounter)%PipeSegmentIndeces(NumPipeSegments))
        ALLOCATE(PipingSystemCircuits(PipeCircuitCounter)%PipeSegmentNames(NumPipeSegments))

        !Check for blank or missing or mismatched number...
        NumAlphasBeforeSegmentOne = 3
        DO ThisCircuitPipeSegmentCounter = 1, NumPipeSegments
            CurIndex = ThisCircuitPipeSegmentCounter + NumAlphasBeforeSegmentOne
            IF (lAlphaFieldBlanks(CurIndex)) THEN
                CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                          cAlphaArgs(CurIndex), 'Expected a pipe segment name, check pipe segment count input field.', ErrorsFound)
            END IF
            PipingSystemCircuits(PipeCircuitCounter)%PipeSegmentNames(ThisCircuitPipeSegmentCounter) = cAlphaArgs(CurIndex)
        END DO

    END DO !All pipe circuits in input

  RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ReadPipeSegmentInputs(NumPipeSegmentsInInput, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetObjectItem, VerifyName
    USE DataInterfaces, ONLY: SetupOutputVariable
    USE DataIPShortCuts

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)     :: NumPipeSegmentsInInput
    LOGICAL, INTENT(IN OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='ReadPipeSegmentInputs'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: SegmentCtr
    INTEGER :: NumAlphas  ! Number of Alphas for each GetObjectItem call
    INTEGER :: NumNumbers ! Number of Numbers for each GetObjectItem call
    INTEGER :: IOStatus   ! Used in GetObjectItem
    INTEGER :: CurIndex
    LOGICAL :: IsNotOK
    LOGICAL :: IsBlank

    !Read in all pipe segments
    DO SegmentCtr = 1, NumPipeSegmentsInInput

        !Read all inputs for this pipe segment
        CALL GetObjectItem(ObjName_Segment,SegmentCtr,cAlphaArgs,NumAlphas, &
                 rNumericArgs,NumNumbers,IOStatus,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        !Get the name, validate
        PipingSystemSegments(SegmentCtr)%Name = cAlphaArgs(1)
        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(cAlphaArgs(1),PipingSystemSegments%Name,SegmentCtr-1,IsNotOK,IsBlank,TRIM(ObjName_Segment)//' Name')
        IF (IsNotOK) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Duplicate name encountered'
        ELSEIF (IsBlank) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Blank name encountered'
        ENDIF

        !Read in the pipe location, validated as positive by IP
        ! -- note that these values will be altered by the main GetInput routine in two ways:
        !   1) shift for basement wall if selected
        !   2) invert y direction to be measured from domain bottom surface for calculations
        PipingSystemSegments(SegmentCtr)%PipeLocation = PointF(rNumericArgs(1), rNumericArgs(2))

        !Read in the flow direction
        SELECT CASE (TRIM(ADJUSTL(cAlphaArgs(2))))
        CASE ('INCREASINGZ')
            PipingSystemSegments(SegmentCtr)%FlowDirection = SegmentFlow_IncreasingZ
        CASE ('DECREASINGZ')
            PipingSystemSegments(SegmentCtr)%FlowDirection = SegmentFlow_DecreasingZ
        CASE DEFAULT
            CurIndex = 2
            CALL IssueSevereInputFieldError(RoutineName, ObjName_Segment, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
                                     cAlphaArgs(CurIndex), 'Invalid flow direction, use one of the available keys.', ErrorsFound)
        END SELECT

    END DO

  RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ReadHorizontalTrenchInputs(StartingDomainNumForHorizontal, StartingCircuitNumForHorizontal, &
                              StartingSegmentNumForHorizontal, NumHorizontalTrenchesInInput, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetObjectItem, VerifyName
    USE DataInterfaces, ONLY: SetupOutputVariable
    USE DataIPShortCuts
    USE DataGlobals, ONLY: SecsInDay
    USE DataLoopNode
    USE NodeInputManager,      ONLY : GetOnlySingleNode
    USE BranchNodeConnections, ONLY : TestCompSet
    USE DataEnvironment, ONLY: PubGroundTempSurfFlag, PubGroundTempSurface

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)     :: StartingDomainNumForHorizontal
    INTEGER, INTENT(IN)     :: StartingCircuitNumForHorizontal
    INTEGER, INTENT(IN)     :: StartingSegmentNumForHorizontal
    INTEGER, INTENT(IN)     :: NumHorizontalTrenchesInInput
    LOGICAL, INTENT(IN OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName = 'ReadHorizontalTrenchInputs'
    INTEGER, PARAMETER          :: MonthsInYear = 12
    REAL(r64), PARAMETER        :: LargeNumber = 10000.0d0
    REAL(r64), PARAMETER        :: AvgDaysInMonth = 365.0d0/12.0d0

          ! DERIVED TYPE DEFINITIONS:
    TYPE HorizontalTrenchData
        CHARACTER(len=MaxNameLength) :: ObjName                   = ''
        CHARACTER(len=MaxNameLength) :: InletNodeName             = ''
        CHARACTER(len=MaxNameLength) :: OutletNodeName            = ''
        REAL(r64)                    :: AxialLength               = 0.0d0
        REAL(r64)                    :: PipeID                    = 0.0d0
        REAL(r64)                    :: PipeOD                    = 0.0d0
        INTEGER                      :: NumPipes                  = 0
        REAL(r64)                    :: BurialDepth               = 0.0d0
        REAL(r64)                    :: DesignFlowRate            = 0.0d0
        REAL(r64)                    :: SoilConductivity          = 0.0d0
        REAL(r64)                    :: SoilDensity               = 0.0d0
        REAL(r64)                    :: SoilSpecificHeat          = 0.0d0
        REAL(r64)                    :: PipeConductivity          = 0.0d0
        REAL(r64)                    :: PipeDensity               = 0.0d0
        REAL(r64)                    :: PipeSpecificHeat          = 0.0d0
        REAL(r64)                    :: InterPipeSpacing          = 0.0d0
        REAL(r64)                    :: MoistureContent           = 0.0d0
        REAL(r64)                    :: SaturationMoistureContent = 0.0d0
        REAL(r64)                    :: KusudaAvgSurfTemp         = 0.0d0
        REAL(r64)                    :: KusudaAvgAmplitude        = 0.0d0
        REAL(r64)                    :: KusudaPhaseShift          = 0.0d0
        REAL(r64)                    :: EvapotranspirationCoeff   = 0.0d0
        LOGICAL                      :: UseGroundTempDataForKusuda= .FALSE.
        REAL(r64)                    :: MinSurfTemp               = 0.0d0
        INTEGER                      :: MonthOfMinSurfTemp        = 0
    END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: HorizontalGHXCtr
    INTEGER :: NumAlphas  ! Number of Alphas for each GetObjectItem call
    INTEGER :: NumNumbers ! Number of Numbers for each GetObjectItem call
    INTEGER :: IOStatus   ! Used in GetObjectItem
    INTEGER :: CurIndex
    LOGICAL :: IsNotOK
    LOGICAL :: IsBlank
    TYPE(HorizontalTrenchData), DIMENSION(NumHorizontalTrenchesInInput) :: HGHX
    INTEGER :: DomainCtr
    INTEGER :: CircuitCtr
    INTEGER :: SegmentCtr
    INTEGER :: NumPipeSegments
    INTEGER :: ThisCircuitPipeSegmentCounter
    INTEGER :: MonthIndex

    ! initialize these counters properly so they can be incremented within the DO loop
    DomainCtr = StartingDomainNumForHorizontal - 1
    CircuitCtr= StartingCircuitNumForHorizontal - 1
    SegmentCtr= StartingSegmentNumForHorizontal - 1

    ! For each horizontal, we need to process the inputs into a local array of derived type,
    !  then resolve each one, creating definitions for a pipe domain, pipe circuit, and series of pipe segments
    ! This way, the outer get input routines can handle it as though they were generalized routines

    !Read in all pipe segments
    DO HorizontalGHXCtr = 1, NumHorizontalTrenchesInInput

        !Increment the domain and circuit counters here
        DomainCtr = DomainCtr + 1
        CircuitCtr = CircuitCtr + 1

        !Read all inputs for this pipe segment
        CALL GetObjectItem(objName_HorizTrench,HorizontalGHXCtr,cAlphaArgs,NumAlphas, &
                 rNumericArgs,NumNumbers,IOStatus,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        !Get the name, validate
        HGHX(HorizontalGHXCtr)%ObjName = cAlphaArgs(1)
        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(cAlphaArgs(1),HGHX%ObjName,HorizontalGHXCtr-1,IsNotOK,IsBlank,TRIM(objName_HorizTrench)//' Name')
        IF (IsNotOK) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Duplicate name encountered'
        ELSEIF (IsBlank) THEN
            ErrorsFound = .TRUE.
            cAlphaArgs(1) = 'Blank name encountered'
        ENDIF

        !Read in the rest of the inputs into the local type for clarity during transition
        HGHX(HorizontalGHXCtr)%InletNodeName             = cAlphaArgs(2)
        HGHX(HorizontalGHXCtr)%OutletNodeName            = cAlphaArgs(3)
        HGHX(HorizontalGHXCtr)%DesignFlowRate            = rNumericArgs(1)
        HGHX(HorizontalGHXCtr)%AxialLength               = rNumericArgs(2)
        HGHX(HorizontalGHXCtr)%NumPipes                  = rNumericArgs(3)
        HGHX(HorizontalGHXCtr)%InterPipeSpacing          = rNumericArgs(4)
        HGHX(HorizontalGHXCtr)%PipeID                    = rNumericArgs(5)
        HGHX(HorizontalGHXCtr)%PipeOD                    = rNumericArgs(6)
        HGHX(HorizontalGHXCtr)%BurialDepth               = rNumericArgs(7)
        HGHX(HorizontalGHXCtr)%SoilConductivity          = rNumericArgs(8)
        HGHX(HorizontalGHXCtr)%SoilDensity               = rNumericArgs(9)
        HGHX(HorizontalGHXCtr)%SoilSpecificHeat          = rNumericArgs(10)
        HGHX(HorizontalGHXCtr)%PipeConductivity          = rNumericArgs(11)
        HGHX(HorizontalGHXCtr)%PipeDensity               = rNumericArgs(12)
        HGHX(HorizontalGHXCtr)%PipeSpecificHeat          = rNumericArgs(13)
        HGHX(HorizontalGHXCtr)%MoistureContent           = rNumericArgs(14)
        HGHX(HorizontalGHXCtr)%SaturationMoistureContent = rNumericArgs(15)
        HGHX(HorizontalGHXCtr)%KusudaAvgSurfTemp         = rNumericArgs(16)
        HGHX(HorizontalGHXCtr)%KusudaAvgAmplitude        = rNumericArgs(17)
        HGHX(HorizontalGHXCtr)%KusudaPhaseShift          = rNumericArgs(18)
        HGHX(HorizontalGHXCtr)%EvapotranspirationCoeff   = rNumericArgs(19)
        HGHX(HorizontalGHXCtr)%UseGroundTempDataForKusuda= lNumericFieldBlanks(16) &
                                                           .OR.lNumericFieldBlanks(17) &
                                                           .OR.lNumericFieldBlanks(18)

        !******* We'll first set up the domain ********
        !the extents will be: Zmax = axial length; Ymax = burial depth*2; Xmax = (NumPipes+1)*HorizontalPipeSpacing
        PipingSystemDomains(DomainCtr)%IsActuallyPartOfAHorizontalTrench = .TRUE.
        WRITE(PipingSystemDomains(DomainCtr)%Name, '("HorizontalTrenchDomain",I4)') HorizontalGHXCtr
        PipingSystemDomains(DomainCtr)%Extents%Xmax = (REAL(HGHX(HorizontalGHXCtr)%NumPipes,r64) + 1.0d0) * &
                                                                            HGHX(HorizontalGHXCtr)%InterPipeSpacing
        PipingSystemDomains(DomainCtr)%Extents%Ymax = 2.0d0 * HGHX(HorizontalGHXCtr)%BurialDepth
        PipingSystemDomains(DomainCtr)%Extents%Zmax = HGHX(HorizontalGHXCtr)%AxialLength

        !set up the mesh with some default parameters
        PipingSystemDomains(DomainCtr)%Mesh%X%RegionMeshCount  = 4
        PipingSystemDomains(DomainCtr)%Mesh%X%MeshDistribution = MeshDistribution_Uniform
        PipingSystemDomains(DomainCtr)%Mesh%Y%RegionMeshCount  = 4
        PipingSystemDomains(DomainCtr)%Mesh%Y%MeshDistribution = MeshDistribution_Uniform
        PipingSystemDomains(DomainCtr)%Mesh%Z%RegionMeshCount  = 4
        PipingSystemDomains(DomainCtr)%Mesh%Z%MeshDistribution = MeshDistribution_Uniform

        !Soil properties
        PipingSystemDomains(DomainCtr)%GroundProperties%Conductivity = HGHX(HorizontalGHXCtr)%SoilConductivity
        PipingSystemDomains(DomainCtr)%GroundProperties%Density      = HGHX(HorizontalGHXCtr)%SoilDensity
        PipingSystemDomains(DomainCtr)%GroundProperties%SpecificHeat = HGHX(HorizontalGHXCtr)%SoilSpecificHeat

        !Moisture properties
        PipingSystemDomains(DomainCtr)%Moisture%Theta_Liq = HGHX(HorizontalGHXCtr)%MoistureContent / 100.0d0
        PipingSystemDomains(DomainCtr)%Moisture%Theta_Sat = HGHX(HorizontalGHXCtr)%SaturationMoistureContent / 100.0d0

        !Farfield model parameters
        IF (.NOT. HGHX(HorizontalGHXCtr)%UseGroundTempDataForKusuda) THEN
            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature = HGHX(HorizontalGHXCtr)%KusudaAvgSurfTemp
            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperatureAmplitude = HGHX(HorizontalGHXCtr)%KusudaAvgAmplitude
            PipingSystemDomains(DomainCtr)%Farfield%PhaseShiftOfMinGroundTempDays = HGHX(HorizontalGHXCtr)%KusudaPhaseShift
        ELSE
            !If ground temp data was not brought in manually in GETINPUT,
            ! then we must get it from the surface ground temperatures

            IF (.NOT. PubGroundTempSurfFlag) THEN
                CALL ShowSevereError("Input problem for "//objName_HorizTrench//"="//HGHX(HorizontalGHXCtr)%ObjName)
                CALL ShowContinueError("No Site:GroundTemperature:Shallow object found in the input file")
                CALL ShowContinueError("This is required for the horizontal ground heat exchanger if farfield parameters are")
                CALL ShowContinueError(" not directly entered into the input object.")
                ErrorsFound = .TRUE.
            END IF

            !Calculate Average Ground Temperature for all 12 months of the year:
            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature = 0.0d0
            DO MonthIndex = 1, MonthsInYear
                PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature = &
                                 PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature + PubGroundTempSurface(MonthIndex)
            END DO
            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature = &
                                 PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature / MonthsInYear

            !Calculate Average Amplitude from Average:
            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperatureAmplitude = 0.0d0
            DO MonthIndex = 1, MonthsInYear
                PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperatureAmplitude = &
                            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperatureAmplitude + &
                            ABS(PubGroundTempSurface(MonthIndex) - PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperature)
            END DO
            PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperatureAmplitude = &
                                             PipingSystemDomains(DomainCtr)%Farfield%AverageGroundTemperatureAmplitude /MonthsInYear

            !Also need to get the month of minimum surface temperature to set phase shift for Kusuda and Achenbach:
            HGHX(HorizontalGHXCtr)%MonthOfMinSurfTemp = 0
            HGHX(HorizontalGHXCtr)%MinSurfTemp = LargeNumber !Set high month 1 temp will be lower and actually get updated
            DO MonthIndex = 1, MonthsInYear
                IF (PubGroundTempSurface(MonthIndex) <= HGHX(HorizontalGHXCtr)%MinSurfTemp) THEN
                    HGHX(HorizontalGHXCtr)%MonthOfMinSurfTemp = MonthIndex
                    HGHX(HorizontalGHXCtr)%MinSurfTemp = PubGroundTempSurface(MonthIndex)
                END IF
            END DO
            PipingSystemDomains(DomainCtr)%Farfield%PhaseShiftOfMinGroundTempDays = &
                            HGHX(HorizontalGHXCtr)%MonthOfMinSurfTemp * AvgDaysInMonth
        END IF

        !Unit conversion
        PipingSystemDomains(DomainCtr)%Farfield%PhaseShiftOfMinGroundTemp = &
                    PipingSystemDomains(DomainCtr)%Farfield%PhaseShiftOfMinGroundTempDays * SecsInDay

        !Other parameters
        PipingSystemDomains(DomainCtr)%SimControls%Convergence_CurrentToPrevIteration = 0.001d0
        PipingSystemDomains(DomainCtr)%SimControls%MaxIterationsPerTS = 250

        !additional evapotranspiration parameter, min/max validated by IP
        PipingSystemDomains(DomainCtr)%Moisture%GroundCoverCoefficient = HGHX(HorizontalGHXCtr)%EvapotranspirationCoeff

        !Allocate the circuit placeholder arrays
        ALLOCATE(PipingSystemDomains(DomainCtr)%CircuitNames(1))
        ALLOCATE(PipingSystemDomains(DomainCtr)%CircuitIndeces(1))
        PipingSystemDomains(DomainCtr)%CircuitNames(1) = HGHX(HorizontalGHXCtr)%ObjName

        !******* We'll next set up the circuit ********
        PipingSystemCircuits(CircuitCtr)%IsActuallyPartOfAHorizontalTrench = .TRUE.
        PipingSystemCircuits(CircuitCtr)%Name = HGHX(HorizontalGHXCtr)%ObjName

        !Read pipe thermal properties
        PipingSystemCircuits(CircuitCtr)%PipeProperties%Conductivity = HGHX(HorizontalGHXCtr)%PipeConductivity
        PipingSystemCircuits(CircuitCtr)%PipeProperties%Density      = HGHX(HorizontalGHXCtr)%PipeDensity
        PipingSystemCircuits(CircuitCtr)%PipeProperties%SpecificHeat = HGHX(HorizontalGHXCtr)%PipeSpecificHeat

        !Pipe sizing
        PipingSystemCircuits(CircuitCtr)%PipeSize%InnerDia = HGHX(HorizontalGHXCtr)%PipeID
        PipingSystemCircuits(CircuitCtr)%PipeSize%OuterDia = HGHX(HorizontalGHXCtr)%PipeOD
        IF (   PipingSystemCircuits(CircuitCtr)%PipeSize%InnerDia >= &
               PipingSystemCircuits(CircuitCtr)%PipeSize%OuterDia) THEN
            !CurIndex = 5
            !CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
            !                            cAlphaArgs(CurIndex), 'Outer diameter must be greater than inner diameter.', ErrorsFound)
        END IF

        !Read design flow rate, validated positive by IP
        PipingSystemCircuits(CircuitCtr)%DesignVolumeFlowRate = HGHX(HorizontalGHXCtr)%DesignFlowRate

        !Read inlet and outlet node names and validate them
        PipingSystemCircuits(CircuitCtr)%InletNodeName = HGHX(HorizontalGHXCtr)%InletNodeName
        PipingSystemCircuits(CircuitCtr)%InletNodeNum  = GetOnlySingleNode( &
                                PipingSystemCircuits(CircuitCtr)%InletNodeName,ErrorsFound, &
                                TRIM(objName_HorizTrench),HGHX(HorizontalGHXCtr)%ObjName, &
                                NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
        IF (PipingSystemCircuits(CircuitCtr)%InletNodeNum == 0) THEN
            CurIndex = 2
            !CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
            !                                cAlphaArgs(CurIndex), 'Bad node name.', ErrorsFound)
        END IF
        PipingSystemCircuits(CircuitCtr)%OutletNodeName = HGHX(HorizontalGHXCtr)%OutletNodeName
        PipingSystemCircuits(CircuitCtr)%OutletNodeNum  = GetOnlySingleNode( &
                                PipingSystemCircuits(CircuitCtr)%OutletNodeName, ErrorsFound, &
                                TRIM(objName_HorizTrench),HGHX(HorizontalGHXCtr)%ObjName, &
                                NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
        IF (PipingSystemCircuits(CircuitCtr)%OutletNodeNum == 0) THEN
            CurIndex = 3
            !CALL IssueSevereInputFieldError(RoutineName, ObjName_Circuit, cAlphaArgs(1), cAlphaFieldNames(CurIndex), &
            !                                cAlphaArgs(CurIndex), 'Bad node name.', ErrorsFound)
        END IF
        CALL TestCompSet(TRIM(objName_HorizTrench), &
                        HGHX(HorizontalGHXCtr)%ObjName, &
                        PipingSystemCircuits(CircuitCtr)%InletNodeName, &
                        PipingSystemCircuits(CircuitCtr)%OutletNodeName, &
                        'Piping System Circuit Nodes')

        !Convergence tolerance values, validated by IP
        PipingSystemCircuits(CircuitCtr)%Convergence_CurrentToPrevIteration = 0.001d0
        PipingSystemCircuits(CircuitCtr)%MaxIterationsPerTS = 100

        !Radial mesh inputs, validated by IP
        ! -- mesh thickness should be considered slightly dangerous until mesh dev engine can trap erroneous values
        PipingSystemCircuits(CircuitCtr)%NumRadialCells = 4
        PipingSystemCircuits(CircuitCtr)%RadialMeshThickness = PipingSystemCircuits(CircuitCtr)%PipeSize%InnerDia / 2.0d0

        !Read number of pipe segments for this circuit, allocate arrays
        NumPipeSegments = HGHX(HorizontalGHXCtr)%NumPipes
        ALLOCATE(PipingSystemCircuits(CircuitCtr)%PipeSegmentIndeces(NumPipeSegments))
        ALLOCATE(PipingSystemCircuits(CircuitCtr)%PipeSegmentNames(NumPipeSegments))

        !Hard-code the segments
        DO ThisCircuitPipeSegmentCounter = 1, NumPipeSegments
            WRITE(PipingSystemCircuits(CircuitCtr)%PipeSegmentNames(ThisCircuitPipeSegmentCounter), &
                    '("HorizontalTrenchCircuit",I4,"Segment",I4)') HorizontalGHXCtr, ThisCircuitPipeSegmentCounter
        END DO

        !******* Then we'll do the segments *******!
        DO ThisCircuitPipeSegmentCounter = 1, NumPipeSegments
            SegmentCtr = SegmentCtr + 1
            WRITE(PipingSystemSegments(SegmentCtr)%Name, '("HorizontalTrenchCircuit",I4,"Segment",I4)') &
                                                                             HorizontalGHXCtr, ThisCircuitPipeSegmentCounter

            PipingSystemSegments(SegmentCtr)%IsActuallyPartOfAHorizontalTrench = .TRUE.
            PipingSystemSegments(SegmentCtr)%PipeLocation = &
                                       PointF( ThisCircuitPipeSegmentCounter*HGHX(HorizontalGHXCtr)%InterPipeSpacing, &
                                               HGHX(HorizontalGHXCtr)%BurialDepth )

            IF (MOD(ThisCircuitPipeSegmentCounter, 2) /= 0) THEN
                PipingSystemSegments(SegmentCtr)%FlowDirection = SegmentFlow_IncreasingZ
            ELSE
                PipingSystemSegments(SegmentCtr)%FlowDirection = SegmentFlow_DecreasingZ
            END IF

        END DO

    END DO

  RETURN

END SUBROUTINE
!*********************************************************************************************!

SUBROUTINE SetupAllOutputVariables(TotalNumSegments, TotalNumCircuits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataInterfaces, ONLY: SetupOutputVariable

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: TotalNumSegments
    INTEGER, INTENT(IN) :: TotalNumCircuits

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: PipeCircuitCounter
    INTEGER :: SegmentCtr

    DO SegmentCtr = 1, TotalNumSegments

        IF (.NOT. PipingSystemSegments(SegmentCtr)%IsActuallyPartOfAHorizontalTrench) THEN

            CALL SetupOutputVariable('Pipe Segment Inlet Temperature [C]',      &
                                     PipingSystemSegments(SegmentCtr)%InletTemperature,'Plant','Average',   &
                                     PipingSystemSegments(SegmentCtr)%Name)
            CALL SetupOutputVariable('Pipe Segment Outlet Temperature [C]',      &
                                     PipingSystemSegments(SegmentCtr)%OutletTemperature,'Plant','Average',   &
                                     PipingSystemSegments(SegmentCtr)%Name)

            CALL SetupOutputVariable('Pipe Segment Fluid Heat Transfer Rate [W]',      &
                                     PipingSystemSegments(SegmentCtr)%FluidHeatLoss,'Plant','Average',   &
                                     PipingSystemSegments(SegmentCtr)%Name)

        END IF

    END DO

    DO PipeCircuitCounter = 1, TotalNumCircuits

        IF (.NOT. PipingSystemCircuits(PipeCircuitCounter)%IsActuallyPartOfAHorizontalTrench) THEN

            CALL SetupOutputVariable('Pipe Circuit Mass Flow Rate [kg/s]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%CurCircuitFlowRate,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)

            CALL SetupOutputVariable('Pipe Circuit Inlet Temperature [C]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%InletTemperature,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)
            CALL SetupOutputVariable('Pipe Circuit Outlet Temperature [C]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%OutletTemperature,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)

            CALL SetupOutputVariable('Pipe Circuit Fluid Heat Transfer Rate [W]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%FluidHeatLoss,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)

        ELSE ! it is a horizontal trench

            CALL SetupOutputVariable('Ground Heat Exchanger Mass Flow Rate [kg/s]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%CurCircuitFlowRate,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)

            CALL SetupOutputVariable('Ground Heat Exchanger Inlet Temperature [C]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%InletTemperature,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)
            CALL SetupOutputVariable('Ground Heat Exchanger Outlet Temperature [C]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%OutletTemperature,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)

            CALL SetupOutputVariable('Ground Heat Exchanger Fluid Heat Transfer Rate [W]',      &
                                     PipingSystemCircuits(PipeCircuitCounter)%FluidHeatLoss,'Plant','Average',   &
                                     PipingSystemCircuits(PipeCircuitCounter)%Name)

        END IF

    END DO

  RETURN

END SUBROUTINE


!*********************************************************************************************!
SUBROUTINE InitPipingSystems(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataHVACGlobals,   ONLY : TimeStepSys, SysTimeElapsed
    USE DataPlant, ONLY: ScanPlantLoopsForObject, TypeOf_PipingSystemPipeCircuit, PlantLoop, &
                        TypeOf_GrndHtExchgHorizTrench
    USE DataGlobals,       ONLY : BeginSimFlag, BeginEnvrnFlag, DayOfSim, HourOfDay, &
                                  TimeStep, TimeStepZone, SecInHour, InitConvTemp
    USE DataLoopNode, ONLY: Node
    USE PlantUtilities, ONLY: SetComponentFlowRate
    USE FluidProperties, ONLY: GetDensityGlycol

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName = 'InitPipingSystems'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL :: errFlag
    INTEGER :: InletNodeNum
    INTEGER :: OutletNodeNum
    INTEGER :: CircCtr
    INTEGER :: SegCtr
    INTEGER :: SegmentIndex
    REAL(r64) :: rho
    INTEGER :: TypeToLookFor

    !Do any one-time initializations
    IF (PipingSystemCircuits(CircuitNum)%NeedToFindOnPlantLoop) THEN

        errFlag = .FALSE.

        IF (PipingSystemCircuits(CircuitNum)%IsActuallyPartOfAHorizontalTrench) THEN
            TypeToLookFor = TypeOf_GrndHtExchgHorizTrench
        ELSE
            TypeToLookFor = TypeOf_PipingSystemPipeCircuit
        END IF

        CALL ScanPlantLoopsForObject(PipingSystemCircuits(CircuitNum)%Name, &
                                     TypeToLookFor, &
                                     PipingSystemCircuits(CircuitNum)%LoopNum, &
                                     PipingSystemCircuits(CircuitNum)%LoopSideNum, &
                                     PipingSystemCircuits(CircuitNum)%BranchNum, &
                                     PipingSystemCircuits(CircuitNum)%CompNum,  &
                                     errFlag=errFlag)

        IF (errFlag) THEN
            CALL ShowFatalError('PipingSystems:'//RoutineName//': Program terminated due to previous condition(s).')
        END IF

        !Once we find ourselves on the plant loop, we can do other things
        rho = GetDensityGlycol(PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidIndex,&
                                RoutineName)
        PipingSystemCircuits(CircuitNum)%DesignMassFlowRate = &
                                PipingSystemCircuits(CircuitNum)%DesignVolumeFlowRate * rho

        PipingSystemCircuits(CircuitNum)%NeedToFindOnPlantLoop = .FALSE.

    END IF

    IF (PipingSystemDomains(DomainNum)%DomainNeedsToBeMeshed) THEN

        CALL DevelopMesh(DomainNum)

        ! would be OK to do some post-mesh error handling here I think
        DO CircCtr = 1, SIZE(PipingSystemDomains(DomainNum)%CircuitIndeces)
            DO SegCtr = 1, SIZE(PipingSystemCircuits(PipingSystemDomains(DomainNum)%CircuitIndeces(CircCtr))%PipeSegmentIndeces)
             SegmentIndex = PipingSystemCircuits(PipingSystemDomains(DomainNum)%CircuitIndeces(CircCtr))%PipeSegmentIndeces(SegCtr)
                IF (.NOT. PipingSystemSegments(SegmentIndex)%PipeCellCoordinatesSet) THEN
                    CALL ShowSevereError('PipingSystems:'//RoutineName//':Pipe segment index not set.')
                    CALL ShowContinueError('...Possibly because pipe segment was placed outside of the domain.')
                    CALL ShowContinueError('...Verify piping system domain inputs, circuits, and segments.')
                    CALL ShowFatalError('Preceding error causes program termination')
                END IF
            END DO
        END DO

        PipingSystemDomains(DomainNum)%DomainNeedsToBeMeshed = .FALSE.

    END IF

    !The time init should be done here before we DoOneTimeInits because the DoOneTimeInits
    ! includes a ground temperature initialization, which is based on the Cur%CurSimTimeSeconds variable
    ! which would be carried over from the previous environment
    PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize = TimeStepSys*SecInHour
    PipingSystemDomains(DomainNum)%Cur%CurSimTimeSeconds =   (dayofSim - 1) * 24           &
                                                           + (hourofday - 1)               &
                                                           + (timestep - 1) * timestepZone &
                                                           + SysTimeElapsed

    !There are also some inits that are "close to one time" inits...(one-time in standalone, each envrn in E+)
    IF(     (BeginSimFlag   .AND. PipingSystemDomains(DomainNum)%BeginSimInit) &
       .OR. (BeginEnvrnFlag .AND. PipingSystemDomains(DomainNum)%BeginSimEnvrn)) THEN

        ! this seemed to clean up a lot of reverse DD stuff because fluid thermal properties were
        ! being based on the inlet temperature, which wasn't updated until later
        InletNodeNum = PipingSystemCircuits(CircuitNum)%InletNodeNum
        PipingSystemCircuits(CircuitNum)%CurCircuitInletTemp = Node(InletNodeNum)%Temp
        PipingSystemCircuits(CircuitNum)%InletTemperature = PipingSystemCircuits(CircuitNum)%CurCircuitInletTemp

        CALL DoOneTimeInitializations(DomainNum, CircuitNum)

        PipingSystemDomains(DomainNum)%BeginSimInit = .FALSE.
        PipingSystemDomains(DomainNum)%BeginSimEnvrn = .FALSE.

    END IF
    IF (.NOT. BeginSimFlag) PipingSystemDomains(DomainNum)%BeginSimInit = .TRUE.
    IF (.NOT. BeginEnvrnFlag) PipingSystemDomains(DomainNum)%BeginSimEnvrn = .TRUE.

    !Shift history arrays only if necessary
    IF(ABS(PipingSystemDomains(DomainNum)%Cur%CurSimTimeSeconds-PipingSystemDomains(DomainNum)%Cur%PrevSimTimeSeconds)>1.0d-6)THEN
        PipingSystemDomains(DomainNum)%Cur%PrevSimTimeSeconds = PipingSystemDomains(DomainNum)%Cur%CurSimTimeSeconds
        CALL ShiftTemperaturesForNewTimeStep(DomainNum)
        PipingSystemDomains(DomainNum)%DomainNeedsSimulation = .TRUE.
    END IF

    !Get the mass flow and inlet temperature to use for this time step
    InletNodeNum = PipingSystemCircuits(CircuitNum)%InletNodeNum
    OutletNodeNum = PipingSystemCircuits(CircuitNum)%OutletNodeNum
    PipingSystemCircuits(CircuitNum)%CurCircuitInletTemp = Node(InletNodeNum)%Temp

    !request design, set component flow will decide what to give us based on restrictions and flow lock status
    PipingSystemCircuits(CircuitNum)%CurCircuitFlowRate = PipingSystemCircuits(CircuitNum)%DesignMassFlowRate
    CALL SetComponentFlowRate( PipingSystemCircuits(CircuitNum)%CurCircuitFlowRate, &
                               InletNodeNum, &
                               OutletNodeNum, &
                               PipingSystemCircuits(CircuitNum)%LoopNum, &
                               PipingSystemCircuits(CircuitNum)%LoopSideNum, &
                               PipingSystemCircuits(CircuitNum)%BranchNum, &
                               PipingSystemCircuits(CircuitNum)%CompNum)

  RETURN

END SUBROUTINE InitPipingSystems
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE UpdatePipingSystems(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode, ONLY: Node

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        INTEGER :: OutletNodeNum

        OutletNodeNum = PipingSystemCircuits(CircuitNum)%OutletNodeNum
        Node(OutletNodeNum)%Temp = PipingSystemDomains(DomainNum)%Cells(    &
            PipingSystemCircuits(CircuitNum)%CircuitOutletCell%X, &
            PipingSystemCircuits(CircuitNum)%CircuitOutletCell%Y, &
            PipingSystemCircuits(CircuitNum)%CircuitOutletCell%Z) &
                %PipeCellData%Fluid%MyBase%Temperature
  RETURN

END SUBROUTINE UpdatePipingSystems
!*********************************************************************************************!

!=====================  Utility/Other routines for module.

!*********************************************************************************************!
SUBROUTINE IssueSevereAlphaInputFieldError(RoutineName, ObjectName, InstanceName, FieldName, FieldEntry, Condition, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowSevereError

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*), INTENT(IN) :: RoutineName
    CHARACTER(LEN=*), INTENT(IN) :: ObjectName
    CHARACTER(LEN=*), INTENT(IN) :: InstanceName
    CHARACTER(LEN=*), INTENT(IN) :: FieldName
    CHARACTER(LEN=*), INTENT(IN) :: FieldEntry
    CHARACTER(LEN=*), INTENT(IN) :: Condition
    LOGICAL, INTENT(IN OUT)      :: ErrorsFound

    CALL ShowSevereError(TRIM(RoutineName)//':'//TRIM(ObjectName)//'="'//TRIM(InstanceName)//  &
          '", invalid '//TRIM(FieldName)//'="'//TRIM(FieldEntry)//'", Condition: '//TRIM(Condition))

    ErrorsFound = .TRUE.

  RETURN

END SUBROUTINE IssueSevereAlphaInputFieldError
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE IssueSevereRealInputFieldError(RoutineName, ObjectName, InstanceName, FieldName, FieldEntry, Condition, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowSevereError
    USE General, ONLY: TrimSigDigits

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(LEN=*), INTENT(IN)     :: RoutineName
    CHARACTER(LEN=*), INTENT(IN)     :: ObjectName
    CHARACTER(LEN=*), INTENT(IN)     :: InstanceName
    CHARACTER(LEN=*), INTENT(IN)     :: FieldName
    REAL(r64),        INTENT(IN)     :: FieldEntry
    CHARACTER(LEN=*), INTENT(IN)     :: Condition
    LOGICAL,          INTENT(IN OUT) :: ErrorsFound

    CALL ShowSevereError(TRIM(RoutineName)//':'//TRIM(ObjectName)//'="'//TRIM(InstanceName)//  &
          '", invalid '//TRIM(FieldName)//'="'//TrimSigDigits(FieldEntry, 3)//'", Condition: '//TRIM(Condition))

    ErrorsFound = .TRUE.

  RETURN

END SUBROUTINE IssueSevereRealInputFieldError
!*********************************************************************************************!

!*********************************************************************************************!
INTEGER FUNCTION GetSurfaceCountForOSCM(OSCMIndex) RESULT (RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataSurfaces, ONLY: Surface

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: OSCMIndex

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: SurfCtr

    RetVal = 0
    DO SurfCtr = 1, SIZE(Surface)
        IF (Surface(SurfCtr)%OSCMPtr == OSCMIndex) RetVal = RetVal + 1
    END DO

  RETURN

END FUNCTION GetSurfaceCountForOSCM
!*********************************************************************************************!

!*********************************************************************************************!
FUNCTION GetSurfaceIndecesForOSCM(OSCMIndex, SurfCount) RESULT (RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataSurfaces, ONLY: Surface

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: OSCMIndex
    INTEGER, INTENT(IN) :: SurfCount

          ! FUNCTION RETURN VALUE DEFINITION:
    INTEGER, DIMENSION(1:SurfCount) :: RetVal

            ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: SurfCtr
    INTEGER :: FoundSurfIndexCtr

    FoundSurfIndexCtr = 0
    DO SurfCtr = 1, SIZE(Surface)
        IF (Surface(SurfCtr)%OSCMPtr == OSCMIndex) THEN
            FoundSurfIndexCtr = FoundSurfIndexCtr + 1
            RetVal(FoundSurfIndexCtr) = SurfCtr
        END IF
    END DO

  RETURN

END FUNCTION GetSurfaceIndecesForOSCM
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION Integer_IsInRange(i, lower, upper) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: i
    INTEGER, INTENT(IN) :: lower
    INTEGER, INTENT(IN) :: upper

    IF ((i >= lower) .AND. (i <= upper)) THEN
        RetVal = .TRUE.
    ELSE
        RetVal = .FALSE.
    END IF

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION Real_IsInRange(r, lower, upper) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: r, lower, upper

    IF ((r >= lower) .AND. (r <= upper)) THEN
        RetVal = .TRUE.
    ELSE
        RetVal = .FALSE.
    END IF

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION Real_ConstrainTo(r, MinVal, MaxVal) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: r, MinVal, MaxVal

    RetVal = MIN(r, MaxVal)
    RetVal = MAX(r, MinVal)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION CellType_IsFieldCell(CellType) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: CellType !From Enum: CellType

    SELECT CASE (CellType)
    CASE (CellType_GeneralField, CellType_BasementCorner, &
          CellType_BasementWall, CellType_BasementFloor)
        RetVal = .TRUE.
    CASE DEFAULT
        RetVal = .FALSE.
    END SELECT

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION MeshPartitionArray_Contains(meshes, value) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: meshes
    REAL(r64), INTENT(IN) :: value

      ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: meshnum

    RetVal = .FALSE.

    DO meshnum = LBOUND(meshes, 1), UBOUND(meshes, 1)
        IF (meshes(meshnum)%rDimension == value) THEN
            RetVal = .TRUE.
            EXIT
        END IF
    END DO

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION RadialCellInfo_XY_CrossSectArea(r) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(RadialCellInformation), INTENT(IN) :: r

    RetVal = 3.14159d0 * ((r%OuterRadius**2) - (r%InnerRadius**2))

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION DomainRectangle_Contains(Rect, p) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(DomainRectangle), INTENT(IN) :: Rect
    TYPE(Point), INTENT(IN) :: p

    IF (IsInRange(p%X, Rect%XMin, Rect%XMax) .AND. IsInRange(p%Y, Rect%YMin, Rect%YMax)) THEN
        RetVal = .TRUE.
    ELSE
        RetVal = .FALSE.
    END IF

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE MeshPartition_SelectionSort(X)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! SUBROUTINE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:), INTENT(IN OUT) :: X

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(MeshPartition) :: TEMP
    INTEGER :: I
    INTEGER :: ISWAP(1)
    INTEGER :: ISWAP1

    DO I = LBOUND(X, 1), UBOUND(X, 1)-1
        ISWAP=MINLOC(X(I:)%rDimension)
        ISWAP1=ISWAP(1)+I-1
        IF(ISWAP1.NE.I) THEN
            TEMP=X(I)
            X(I)=X(ISWAP1)
            X(ISWAP1)=TEMP
        ENDIF
    END DO

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
INTEGER FUNCTION MeshPartition_CompareByDimension(x, y) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(MeshPartition), INTENT(IN) :: x
    TYPE(MeshPartition), INTENT(IN) :: y

    IF (x%rDimension < y%rDimension) THEN
      RetVal = -1
    ELSEIF (x%rDimension > y%rDimension) THEN
      RetVal = 1
    ELSE
      RetVal = 0
    ENDIF

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION BaseThermalPropertySet_Diffusivity(p) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(BaseThermalPropertySet), INTENT(IN) :: p

    RetVal = p%Conductivity / (p%Density * p%SpecificHeat)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION RectangleF_Contains(rect, p) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(RectangleF), INTENT(IN) :: rect
    TYPE(PointF), INTENT(IN) :: p

    RetVal = (      (Rect%X_min <= p%X)                &
              .AND. (p%X < (Rect%X_min + rect%Width))  &
              .AND. (rect%Y_min <= p%Y)                &
              .AND. (p%Y < (rect%Y_min + rect%Height)) &
             )

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
!Extension methods for Sim classes
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION RadialSizing_Thickness(r) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(RadialSizing), INTENT(IN) :: r

    RetVal = (r%OuterDia - r%InnerDia) / 2.0d0

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PipeSegmentInfo_InitPipeCells(s, x, y)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(PipeSegmentInfo), INTENT(IN OUT) :: s
    INTEGER, INTENT(IN) :: x
    INTEGER, INTENT(IN) :: y

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(Point) :: TempPt

    TempPt%X = x
    TempPt%Y = y

    s%PipeCellCoordinates = TempPt
    s%PipeCellCoordinatesSet = .TRUE.

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PipeCircuitInfo_InitInOutCells(c, in, out)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(PipeCircuitInfo), INTENT(IN OUT) :: c
    TYPE(CartesianCell), INTENT(IN) :: in
    TYPE(CartesianCell), INTENT(IN) :: out

    c%CircuitInletCell = Point3DInteger(in%X_index, in%Y_index, in%Z_index)
    c%CircuitOutletCell = Point3DInteger(out%X_index, out%Y_index, out%Z_index)

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
! Convergence checking
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION IsConverged_CurrentToPrevIteration(DomainNum) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

      ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: LocalMax
    REAL(r64) :: ThisCellMax
    INTEGER :: X
    INTEGER :: Y
    INTEGER :: Z

    LocalMax = 0.0d0
    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells,3), UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells,2), UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells,1), UBOUND(PipingSystemDomains(DomainNum)%Cells,1)
                ThisCellMax = ABS(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature - &
                                          PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature_PrevIteration)
                LocalMax = MAX(LocalMax, ThisCellMax)
            END DO
        END DO
    END DO

    RetVal = (LocalMax < PipingSystemDomains(DomainNum)%SimControls%Convergence_CurrentToPrevIteration)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION IsConverged_PipeCurrentToPrevIteration(CircuitNum, CellToCheck, MaxDivAmount) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: CircuitNum
    TYPE(CartesianCell), INTENT(IN) :: CellToCheck
    REAL(r64), INTENT(IN OUT) :: MaxDivAmount

      ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: RadialCtr
    REAL(r64) :: ThisCellMax
    TYPE(RadialCellInformation) :: radCell

    MaxDivAmount = 0.0d0
    DO RadialCtr = LBOUND(CellToCheck%PipeCellData%Soil,1), UBOUND(CellToCheck%PipeCellData%Soil,1)
        radCell = CellToCheck%PipeCellData%Soil(RadialCtr)
        ThisCellMax = ABS(radCell%MyBase%Temperature - radCell%MyBase%Temperature_PrevIteration)
        IF (ThisCellMax > MaxDivAmount) THEN
            MaxDivAmount = ThisCellMax
        END IF
    END DO
    !'also do the pipe cell
    ThisCellMax = ABS(  CellToCheck%PipeCellData%Pipe%MyBase%Temperature &
                      - CellToCheck%PipeCellData%Pipe%MyBase%Temperature_PrevIteration)
    IF (ThisCellMax > MaxDivAmount) THEN
        MaxDivAmount = ThisCellMax
    END IF
    !'also do the water cell
    ThisCellMax = ABS(  CellToCheck%PipeCellData%Fluid%MyBase%Temperature &
                      - CellToCheck%PipeCellData%Fluid%MyBase%Temperature_PrevIteration)
    IF (ThisCellMax > MaxDivAmount) THEN
        MaxDivAmount = ThisCellMax
    END IF
    !'also do insulation if it exists
    IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
        ThisCellMax = ABS(  CellToCheck%PipeCellData%Insulation%MyBase%Temperature &
                          - CellToCheck%PipeCellData%Insulation%MyBase%Temperature_PrevIteration)
        IF (ThisCellMax > MaxDivAmount) THEN
            MaxDivAmount = ThisCellMax
        END IF
    END IF

    RetVal = (MaxDivAmount < PipingSystemCircuits(CircuitNum)%Convergence_CurrentToPrevIteration)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ShiftTemperaturesForNewTimeStep(DomainNum)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: X
    INTEGER :: Y
    INTEGER :: Z
    INTEGER :: RadCtr

    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells,3), UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells,2), UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells,1), UBOUND(PipingSystemDomains(DomainNum)%Cells,1)

                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature_PrevTimeStep = &
                                                      PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature

                IF (PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType == CellType_Pipe) THEN

                    DO RadCtr = LBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil, 1), &
                                UBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil, 1)

                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(RadCtr)%MyBase%Temperature_PrevTimeStep = &
                                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(RadCtr)%MyBase%Temperature

                    END DO

                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature_PrevTimeStep = &
                                           PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature

                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature_PrevTimeStep = &
                                           PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature

                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature_PrevTimeStep = &
                                           PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature

                END IF

            END DO
        END DO
    END DO

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ShiftTemperaturesForNewIteration(DomainNum)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: X
    INTEGER :: Y
    INTEGER :: Z
    INTEGER :: RadCtr

    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells,3), UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells,2), UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells,1), UBOUND(PipingSystemDomains(DomainNum)%Cells,1)

                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature_PrevIteration = &
                                            PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature

                IF (PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType == CellType_Pipe) THEN

                    DO RadCtr = LBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil, 1), &
                                UBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil, 1)

                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(RadCtr)%MyBase%Temperature_PrevIteration= &
                                  PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(RadCtr)%MyBase%Temperature

                    END DO

                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature_PrevIteration = &
                                   PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature

                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature_PrevIteration = &
                                   PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature

                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature_PrevIteration = &
                                   PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature

                END IF

            END DO
        END DO
    END DO

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE ShiftPipeTemperaturesForNewIteration(ThisPipeCell)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisPipeCell

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: RadCtr

    IF (ThisPipeCell%CellType == CellType_Pipe) THEN !It better be!

        DO RadCtr = LBOUND(ThisPipeCell%PipeCellData%Soil, 1), UBOUND(ThisPipeCell%PipeCellData%Soil, 1)
            ThisPipeCell%PipeCellData%Soil(RadCtr)%MyBase%Temperature_PrevIteration = &
                                              ThisPipeCell%PipeCellData%Soil(RadCtr)%MyBase%Temperature
        END DO

        ThisPipeCell%PipeCellData%Fluid%MyBase%Temperature_PrevIteration = &
                                              ThisPipeCell%PipeCellData%Fluid%MyBase%Temperature

        ThisPipeCell%PipeCellData%Pipe%MyBase%Temperature_PrevIteration = &
                                              ThisPipeCell%PipeCellData%Pipe%MyBase%Temperature

        ThisPipeCell%PipeCellData%Insulation%MyBase%Temperature_PrevIteration = &
                                              ThisPipeCell%PipeCellData%Insulation%MyBase%Temperature

    END IF

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
LOGICAL FUNCTION CheckForOutOfRangeTemps(DomainNum) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    REAL(r64) :: MaxLimit
    REAL(r64) :: MinLimit

    MaxLimit = PipingSystemDomains(DomainNum)%SimControls%MaximumTemperatureLimit
    MinLimit = PipingSystemDomains(DomainNum)%SimControls%MinimumTemperatureLimit

    RetVal = (ANY(PipingSystemDomains(DomainNum)%Cells%MyBase%Temperature .GT. MaxLimit) .OR. &
              ANY(PipingSystemDomains(DomainNum)%Cells%MyBase%Temperature .LT. MinLimit))

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION Width(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = c%X_max - c%X_min

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION Height(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = c%Y_max - c%Y_min

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION Depth(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = c%Z_max - c%Z_min

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION XNormalArea(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = Depth(c) * Height(c)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION YNormalArea(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = Depth(c) * Width(c)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION ZNormalArea(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = Width(c) * Height(c)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION Volume(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = Width(c) * Depth(c) * Height(c)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
TYPE(RectangleF) FUNCTION XYRectangle(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = RectangleF(c%X_min, c%Y_min, Width(c), Height(c))

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
TYPE(RectangleF) FUNCTION XZRectangle(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = RectangleF(c%X_min, c%Z_min, Width(c), Depth(c))

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
TYPE(RectangleF) FUNCTION YZRectangle(c) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c

    RetVal = RectangleF(c%Y_min, c%Z_min, Height(c), Depth(c))

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION NormalArea(c, Direction) RESULT (RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN) :: c
    INTEGER, INTENT(IN) :: Direction !From Enum: Direction

    SELECT CASE (Direction)
    CASE (Direction_PositiveY, Direction_NegativeY)
        RetVal = YNormalArea(c)
    CASE (Direction_PositiveX, Direction_NegativeX)
        RetVal = XNormalArea(c)
    CASE (Direction_PositiveZ, Direction_NegativeZ)
        RetVal = ZNormalArea(c)
    END SELECT
    !Objexx:Return Check/enforce that one of these CASEs holds to assure return value is set

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
TYPE(NeighborInformation) FUNCTION NeighborInformationArray_Value(dict, direction) RESULT(RetVal)

      ! FUNCTION INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS FUNCTION:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(DirectionNeighbor_Dictionary), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: dict
    INTEGER, INTENT(IN) :: Direction !From Enum: Direction

      ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Index

    DO Index = LBOUND(dict,1), UBOUND(dict,1)
        IF (dict(Index)%Direction == direction) THEN
            RetVal = dict(Index)%Value
            EXIT
        END IF
    END DO
    !Objexx:Return Check/enforce that return value is set

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
! Constructors for generic classes
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE CartesianPipeCellInformation_ctor(c, GridCellWidth, PipeSizes, NumRadialNodes, &
                                             CellDepth, InsulationThickness, RadialGridExtent, SimHasInsulation)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(CartesianPipeCellInformation), INTENT(IN OUT) :: c
    REAL(r64), INTENT(IN) :: GridCellWidth
    TYPE(RadialSizing), INTENT(IN) :: PipeSizes
    INTEGER, INTENT(IN) :: NumRadialNodes
    REAL(r64), INTENT(IN) :: CellDepth
    REAL(r64), INTENT(IN) :: InsulationThickness
    REAL(r64), INTENT(IN) :: RadialGridExtent
    LOGICAL, INTENT(IN) :: SimHasInsulation

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: InsulationInnerRadius
    REAL(r64) :: InsulationOuterRadius
    REAL(r64) :: InsulationCentroid
    REAL(r64) :: PipeOuterRadius
    REAL(r64) :: PipeInnerRadius
    REAL(r64) :: MinimumSoilRadius
    REAL(r64) :: ThisSliceInnerRadius
    REAL(r64) :: Rval
    INTEGER :: RadialCellCtr

     !'calculate pipe radius
    PipeOuterRadius = PipeSizes%OuterDia / 2
    PipeInnerRadius = PipeSizes%InnerDia / 2

    !'--we will work from inside out, calculating dimensions and instantiating variables--
    !'first instantiate the water cell
    CALL FluidCellInformation_ctor(c%Fluid, PipeInnerRadius, CellDepth)

    !'then the pipe cell
    CALL RadialCellInformation_ctor(c%Pipe, (PipeOuterRadius + PipeInnerRadius) / 2.0d0, PipeInnerRadius, PipeOuterRadius)

    !'then the insulation if we have it
    IF (InsulationThickness > 0) THEN
        InsulationInnerRadius = PipeOuterRadius
        InsulationOuterRadius = InsulationInnerRadius + InsulationThickness
        InsulationCentroid = (InsulationInnerRadius + InsulationOuterRadius) / 2.0d0
        CALL RadialCellInformation_ctor(c%Insulation, InsulationCentroid, InsulationInnerRadius, InsulationOuterRadius)
    END IF

    !'determine where to start applying the radial soil cells based on whether we have insulation or not
    IF (.NOT. SimHasInsulation) THEN
        MinimumSoilRadius = PipeOuterRadius
    ELSE
        MinimumSoilRadius = c%Insulation%OuterRadius
    END IF

    !'the radial cells are distributed evenly throughout this region
    c%RadialSliceWidth = RadialGridExtent / NumRadialNodes

    !allocate the array of radial soil nodes
    ALLOCATE(c%Soil(0:NumRadialNodes - 1))

    !first set Rval to the minimum soil radius plus half a slice thickness for the innermost radial node
    Rval = MinimumSoilRadius + (c%RadialSliceWidth / 2.0d0)
    ThisSliceInnerRadius = MinimumSoilRadius
    CALL RadialCellInformation_ctor(c%Soil(0), Rval, ThisSliceInnerRadius, ThisSliceInnerRadius + c%RadialSliceWidth)

    !'then loop through the rest and assign them, each radius is simply one more slice thickness
    DO RadialCellCtr = 1, UBOUND(c%Soil,1)
        Rval = Rval + c%RadialSliceWidth
        ThisSliceInnerRadius = ThisSliceInnerRadius + c%RadialSliceWidth
        CALL RadialCellInformation_ctor(c%Soil(RadialCellCtr), Rval, ThisSliceInnerRadius, ThisSliceInnerRadius+c%RadialSliceWidth)
    END DO

    !'also assign the interface cell surrounding the radial system
    c%InterfaceVolume = (1.0d0 - (3.1415926535d0 / 4.0d0)) * (GridCellWidth ** 2) * CellDepth

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE RadialCellInformation_ctor(c, m_RadialCentroid, m_MinRadius, m_MaxRadius)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(RadialCellInformation), INTENT(IN OUT) :: c
    REAL(r64), INTENT(IN) :: m_RadialCentroid
    REAL(r64), INTENT(IN) :: m_MinRadius
    REAL(r64), INTENT(IN) :: m_MaxRadius

    c%RadialCentroid = m_RadialCentroid
    c%InnerRadius = m_MinRadius
    c%OuterRadius = m_MaxRadius

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE FluidCellInformation_ctor(c, m_PipeInnerRadius, m_CellDepth)

      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Edwin Lee
      !       DATE WRITTEN   Summer 2011
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! <description>

      ! METHODOLOGY EMPLOYED:
      ! <description>

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

      ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(FluidCellInformation), INTENT(IN OUT) :: c
    REAL(r64), INTENT(IN) :: m_PipeInnerRadius
    REAL(r64), INTENT(IN) :: m_CellDepth

    c%PipeInnerRadius = m_PipeInnerRadius
    c%Volume = 3.1415926535d0 * (m_PipeInnerRadius ** 2) * m_CellDepth

    RETURN

END SUBROUTINE
!*********************************************************************************************!

! ==================================================
! =========== Mesh Development routines ============
! ==================================================

!*********************************************************************************************!
SUBROUTINE DevelopMesh(DomainNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:) :: XPartitionRegions
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:) :: YPartitionRegions
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:) :: ZPartitionRegions
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:) :: XRegions
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:) :: YRegions
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:) :: ZRegions
    REAL(r64),        ALLOCATABLE, DIMENSION(:) :: XBoundaryPoints
    REAL(r64),        ALLOCATABLE, DIMENSION(:) :: YBoundaryPoints
    REAL(r64),        ALLOCATABLE, DIMENSION(:) :: ZBoundaryPoints
    INTEGER :: RegionListCount
    INTEGER :: BoundaryListCount
    LOGICAL :: XPartitionsExist
    LOGICAL :: YPartitionsExist
    LOGICAL :: ZPartitionsExist

    !'****** LAYOUT PARTITIONS ******'
    CALL CreatePartitionCenterList(DomainNum)

    IF (ALLOCATED(PipingSystemDomains(DomainNum)%Partitions%X)) THEN
        ALLOCATE(XPartitionRegions(0:UBOUND(PipingSystemDomains(DomainNum)%Partitions%X,1)))
        XPartitionsExist = .TRUE.
    ELSE
        ALLOCATE(XPartitionRegions(0:0))
        ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X(0:0))
        XPartitionsExist = .FALSE.
    END IF
    XPartitionRegions = CreatePartitionRegionList(DomainNum, &
                                                  PipingSystemDomains(DomainNum)%Partitions%X, &
                                                  XPartitionsExist, &
                                                  PipingSystemDomains(DomainNum)%Extents%XMax, &
                                                  UBOUND(PipingSystemDomains(DomainNum)%Partitions%X, 1))

    IF (ALLOCATED(PipingSystemDomains(DomainNum)%Partitions%Y)) THEN
        ALLOCATE(YPartitionRegions(0:UBOUND(PipingSystemDomains(DomainNum)%Partitions%Y,1)))
        YPartitionsExist = .TRUE.
    ELSE
        ALLOCATE(YPartitionRegions(0:0))
        ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y(0:0))
        YPartitionsExist = .FALSE.
    END IF
    YPartitionRegions = CreatePartitionRegionList(DomainNum, &
                                                  PipingSystemDomains(DomainNum)%Partitions%Y, &
                                                  YPartitionsExist, &
                                                  PipingSystemDomains(DomainNum)%Extents%YMax, &
                                                  UBOUND(PipingSystemDomains(DomainNum)%Partitions%Y, 1))

    ZPartitionsExist = .FALSE.

    !'***** LAYOUT MESH REGIONS *****'
    RegionListCount = CreateRegionListCount(XPartitionRegions, &
                                            PipingSystemDomains(DomainNum)%Extents%XMax, &
                                            XPartitionsExist)
    ALLOCATE(XRegions(0:RegionListCount-1))

    XRegions = CreateRegionList(DomainNum, &
                                XPartitionRegions, &
                                PipingSystemDomains(DomainNum)%Extents%XMax, &
                                RegionType_XDirection, &
                                RegionListCount-1, &
                                XPartitionsExist, &
                                BasementWallXIndex=PipingSystemDomains(DomainNum)%BasementZone%BasementWallXIndex)

    RegionListCount = CreateRegionListCount(YPartitionRegions, &
                                            PipingSystemDomains(DomainNum)%Extents%YMax, &
                                            YPartitionsExist)
    ALLOCATE(YRegions(0:RegionListCount-1))
    YRegions = CreateRegionList(DomainNum, &
                                YPartitionRegions, &
                                PipingSystemDomains(DomainNum)%Extents%YMax, &
                                RegionType_YDirection, &
                                RegionListCount-1, &
                                YPartitionsExist, &
                                BasementFloorYIndex=PipingSystemDomains(DomainNum)%BasementZone%BasementFloorYIndex)

     RegionListCount = CreateRegionListCount(ZPartitionRegions, &
                                            PipingSystemDomains(DomainNum)%Extents%ZMax, &
                                            ZPartitionsExist)
    ALLOCATE(ZRegions(0:RegionListCount-1))
    ZRegions = CreateRegionList(DomainNum, &
                                ZPartitionRegions, &
                                PipingSystemDomains(DomainNum)%Extents%ZMax, &
                                RegionType_ZDirection, &
                                RegionListCount-1, &
                                ZPartitionsExist)


    !'** MAKE REGIONS > BOUNDARIES **'
    BoundaryListCount = CreateBoundaryListCount(XRegions, RegionType_XDirection)
    ALLOCATE(XBoundaryPoints(0:BoundaryListCount-1))
    XBoundaryPoints = CreateBoundaryList(XRegions, &
                                         PipingSystemDomains(DomainNum)%Extents%XMax, &
                                         RegionType_XDirection, &
                                         0, &
                                         BoundaryListCount-1)

    BoundaryListCount = CreateBoundaryListCount(YRegions, RegionTYpe_YDirection)
    ALLOCATE(YBoundaryPoints(0:BoundaryListCount-1))
    YBoundaryPoints = CreateBoundaryList(YRegions, &
                                         PipingSystemDomains(DomainNum)%Extents%YMax, &
                                         RegionType_YDirection, &
                                         0, &
                                         BoundaryListCount-1)

    BoundaryListCount = CreateBoundaryListCount(ZRegions, RegionTYpe_ZDirection)
    ALLOCATE(ZBoundaryPoints(0:BoundaryListCount-1))
    ZBoundaryPoints = CreateBoundaryList(ZRegions, &
                                         PipingSystemDomains(DomainNum)%Extents%ZMax, &
                                         RegionType_ZDirection, &
                                         0, &
                                         BoundaryListCount-1)

    !'****** DEVELOP CELL ARRAY *****'
    CALL CreateCellArray(DomainNum, &
                         XBoundaryPoints, &
                         YBoundaryPoints, &
                         ZBoundaryPoints, &
                         PipingSystemDomains(DomainNum)%BasementZone%BasementWallXIndex, &
                         PipingSystemDomains(DomainNum)%BasementZone%BasementFloorYIndex)

    !'***** SETUP CELL NEIGHBORS ****'
    CALL SetupCellNeighbors(DomainNum)

    !'** SET UP PIPE CIRCUIT CELLS **'
    CALL SetupPipeCircuitInOutCells(DomainNum)

    IF(ALLOCATED(XPartitionRegions)) DEALLOCATE(XPartitionRegions)
    IF(ALLOCATED(YPartitionRegions)) DEALLOCATE(YPartitionRegions)
    IF(ALLOCATED(ZPartitionRegions)) DEALLOCATE(ZPartitionRegions)
    IF(ALLOCATED(XRegions)) DEALLOCATE(XRegions)
    IF(ALLOCATED(YRegions)) DEALLOCATE(YRegions)
    IF(ALLOCATED(ZRegions)) DEALLOCATE(ZRegions)
    IF(ALLOCATED(XBoundaryPoints)) DEALLOCATE(XBoundaryPoints)
    IF(ALLOCATED(YBoundaryPoints)) DEALLOCATE(YBoundaryPoints)
    IF(ALLOCATED(ZBoundaryPoints)) DEALLOCATE(ZBoundaryPoints)

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE CreatePartitionCenterList(DomainNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64) :: BasementCellFraction = 0.001d0 !the fraction of domain extent to use for the basement cells
                                                !actual dimension shouldn't matter for calculation purposes

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: BasementDistFromBottom
    INTEGER :: CircuitCtr
    INTEGER :: CircuitIndex
    INTEGER :: PipeCtr
    INTEGER :: PreviousUbound
    TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:) :: PreviousEntries
    REAL(r64) :: PipeCellWidth
    REAL(r64) :: SurfCellWidth !Basement surface...
    TYPE(PipeSegmentInfo) :: ThisSegment

    !'NOTE: pipe location y values have already been corrected to be measured from the bottom surface
    !'in input they are measured by depth, but internally they are referred to by distance from y = 0, or the bottom boundary
    DO CircuitCtr = LBOUND(PipingSystemDomains(DomainNum)%CircuitIndeces, 1), &
                    UBOUND(PipingSystemDomains(DomainNum)%CircuitIndeces, 1)
        CircuitIndex = PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitCtr)

        !set up a convenience variable here
        !'account for the pipe and insulation if necessary
        IF (.NOT. PipingSystemCircuits(CircuitIndex)%HasInsulation) THEN
            PipeCellWidth = PipingSystemCircuits(CircuitIndex)%PipeSize%OuterDia
        ELSE
            PipeCellWidth = PipingSystemCircuits(CircuitIndex)%InsulationSize%OuterDia
        END IF
        !'then add the radial mesh thickness on both sides of the pipe/insulation construct
        PipeCellWidth = PipeCellWidth + 2 * PipingSystemCircuits(CircuitIndex)%RadialMeshThickness

        DO PipeCtr = LBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1), &
                     UBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1)

            ThisSegment = PipingSystemSegments(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces(PipeCtr))
            IF (.NOT. ALLOCATED(PipingSystemDomains(DomainNum)%Partitions%X)) THEN
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X(0:0))
                PipingSystemDomains(DomainNum)%Partitions%X(0) = &
                            MeshPartition(ThisSegment%PipeLocation%X, PartitionType_Pipe, PipeCellWidth)
            ELSEIF(.NOT. MeshPartitionArray_Contains(PipingSystemDomains(DomainNum)%Partitions%X, ThisSegment%PipeLocation%X)) THEN
                PreviousUbound = UBOUND(PipingSystemDomains(DomainNum)%Partitions%X, 1)
                IF (ALLOCATED(PreviousEntries)) DEALLOCATE(PreviousEntries)
                ALLOCATE(PreviousEntries(0:PreviousUbound))
                PreviousEntries = PipingSystemDomains(DomainNum)%Partitions%X
                DEALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X)
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X(0:PreviousUbound+1))
                PipingSystemDomains(DomainNum)%Partitions%X(0:PreviousUbound) = PreviousEntries
                PipingSystemDomains(DomainNum)%Partitions%X(PreviousUbound + 1) = &
                            MeshPartition(ThisSegment%PipeLocation%X, PartitionType_Pipe, PipeCellWidth)
            END IF
            IF (.NOT. ALLOCATED(PipingSystemDomains(DomainNum)%Partitions%Y)) THEN
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y(0:0))
                PipingSystemDomains(DomainNum)%Partitions%Y(0) = &
                            MeshPartition(ThisSegment%PipeLocation%Y, PartitionType_Pipe, PipeCellWidth)
            ELSEIF(.NOT. MeshPartitionArray_Contains(PipingSystemDomains(DomainNum)%Partitions%Y, ThisSegment%PipeLocation%Y)) THEN
                PreviousUbound = UBOUND(PipingSystemDomains(DomainNum)%Partitions%Y, 1)
                IF (ALLOCATED(PreviousEntries)) DEALLOCATE(PreviousEntries)
                ALLOCATE(PreviousEntries(0:PreviousUbound))
                PreviousEntries = PipingSystemDomains(DomainNum)%Partitions%Y
                DEALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y)
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y(0:PreviousUbound+1))
                PipingSystemDomains(DomainNum)%Partitions%Y(0:PreviousUbound) = PreviousEntries
                PipingSystemDomains(DomainNum)%Partitions%Y(PreviousUbound + 1) = &
                            MeshPartition(ThisSegment%PipeLocation%Y, PartitionType_Pipe, PipeCellWidth)
            END IF

        END DO

    END DO

    IF (PipingSystemDomains(DomainNum)%HasBasement) THEN
        !'NOTE: the basement depth is still a depth from the ground surface, need to correct for this here
        IF (PipingSystemDomains(DomainNum)%BasementZone%Width > 0) THEN
            SurfCellWidth = PipingSystemDomains(DomainNum)%Extents%Xmax * BasementCellFraction
            IF (.NOT. ALLOCATED(PipingSystemDomains(DomainNum)%Partitions%X)) THEN
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X(0:0))
                PipingSystemDomains(DomainNum)%Partitions%X(0) = &
                        MeshPartition(PipingSystemDomains(DomainNum)%BasementZone%Width, PartitionType_BasementWall, SurfCellWidth)
            ELSEIF (.NOT. MeshPartitionArray_Contains(PipingSystemDomains(DomainNum)%Partitions%X, &
                                                      PipingSystemDomains(DomainNum)%BasementZone%Width)) THEN
                PreviousUbound = UBOUND(PipingSystemDomains(DomainNum)%Partitions%X, 1)
                IF (ALLOCATED(PreviousEntries)) DEALLOCATE(PreviousEntries)
                ALLOCATE(PreviousEntries(0:PreviousUbound))
                PreviousEntries = PipingSystemDomains(DomainNum)%Partitions%X
                DEALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X)
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%X(0:PreviousUbound+1))
                PipingSystemDomains(DomainNum)%Partitions%X(0:PreviousUbound) = PreviousEntries
                PipingSystemDomains(DomainNum)%Partitions%X(PreviousUbound + 1) = &
                        MeshPartition(PipingSystemDomains(DomainNum)%BasementZone%Width, PartitionType_BasementWall, SurfCellWidth)
            END IF
        END IF
        IF (PipingSystemDomains(DomainNum)%BasementZone%Depth > 0) THEN
            SurfCellWidth = PipingSystemDomains(DomainNum)%Extents%Ymax * BasementCellFraction
            BasementDistFromBottom = PipingSystemDomains(DomainNum)%Extents%Ymax-PipingSystemDomains(DomainNum)%BasementZone%Depth
            IF (.NOT. ALLOCATED(PipingSystemDomains(DomainNum)%Partitions%Y)) THEN
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y(0:0))
                PipingSystemDomains(DomainNum)%Partitions%Y(0) = &
                        MeshPartition(BasementDistFromBottom, PartitionType_BasementFloor, SurfCellWidth)
            ELSEIF (.NOT. MeshPartitionArray_Contains(PipingSystemDomains(DomainNum)%Partitions%Y, BasementDistFromBottom)) THEN
                PreviousUbound = UBOUND(PipingSystemDomains(DomainNum)%Partitions%Y, 1)
                IF (ALLOCATED(PreviousEntries)) DEALLOCATE(PreviousEntries)
                ALLOCATE(PreviousEntries(0:PreviousUbound))
                PreviousEntries = PipingSystemDomains(DomainNum)%Partitions%Y
                DEALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y)
                ALLOCATE(PipingSystemDomains(DomainNum)%Partitions%Y(0:PreviousUbound+1))
                PipingSystemDomains(DomainNum)%Partitions%Y(0:PreviousUbound) = PreviousEntries
                PipingSystemDomains(DomainNum)%Partitions%Y(PreviousUbound + 1) = &
                        MeshPartition(BasementDistFromBottom, PartitionType_BasementFloor, SurfCellWidth)
            END IF
        END IF
    END IF

    CALL MeshPartition_SelectionSort(PipingSystemDomains(DomainNum)%Partitions%X)
    CALL MeshPartition_SelectionSort(PipingSystemDomains(DomainNum)%Partitions%Y)

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
FUNCTION CreatePartitionRegionList(DomainNum, ThesePartitionCenters, PartitionsExist, DirExtentMax, PartitionsUBound)   &
             RESULT(ThesePartitionRegions)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: ThesePartitionCenters
    INTEGER, INTENT(IN) :: PartitionsUbound
    REAL(r64), INTENT(IN) :: DirExtentMax
    LOGICAL, INTENT(IN) :: PartitionsExist

          ! FUNCTION PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName = 'CreatePartitionRegionList'

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Index
    REAL(r64) :: ThisCellWidthBy2
    INTEGER :: ThisPartitionType !From Enum: RegionType
    REAL(r64) :: CellLeft
    REAL(r64) :: CellRight
    INTEGER :: SubIndex

          ! FUNCTION RETURN VALUE
    TYPE(GridRegion), DIMENSION(0:PartitionsUBound) :: ThesePartitionRegions

    IF (.NOT. PartitionsExist) THEN
        RETURN
    END IF

    !'loop across all partitions
    DO Index = LBOUND(ThesePartitionCenters, 1), UBOUND(ThesePartitionCenters, 1)

        ThisCellWidthBy2 = ThesePartitionCenters(Index)%TotalWidth / 2.0d0
        ThisPartitionType = ThesePartitionCenters(Index)%PartitionType

        !'use this half width to validate the region and add it to the collection
        CellLeft = ThesePartitionCenters(Index)%rDimension - ThisCellWidthBy2
        CellRight = ThesePartitionCenters(Index)%rDimension + ThisCellWidthBy2

        !check to make sure this location is valid
        IF (CellLeft < 0.0d0 .OR. CellRight > DirExtentMax) THEN
            CALL ShowSevereError('PlantPipingSystems::'//RoutineName//': Invalid partition location in domain.')
            CALL ShowContinueError('Occurs during mesh development for domain='//TRIM(PipingSystemDomains(DomainNum)%Name))
            CALL ShowContinueError('A pipe or basement is located outside of the domain extents.')
            CALL ShowFatalError('Preceding error causes program termination.')
        END IF

        !Scan all grid regions to make sure this range doesn't fall within an already entered range
        DO SubIndex = 0, Index-1
            IF (     IsInRange(CellLeft, ThesePartitionRegions(SubIndex)%Min, ThesePartitionRegions(SubIndex)%Max) &
                .OR. IsInRange(CellRight, ThesePartitionRegions(SubIndex)%Min, ThesePartitionRegions(SubIndex)%Max)) THEN

                CALL ShowSevereError('PlantPipingSystems::'//RoutineName//': Invalid partition location in domain.')
                CALL ShowContinueError('Occurs during mesh development for domain='//TRIM(PipingSystemDomains(DomainNum)%Name))
                CALL ShowContinueError('A mesh conflict was encountered where partitions were overlapping.')
                CALL ShowContinueError('Ensure that all pipes exactly line up or are separated to allow meshing in between them')
                CALL ShowContinueError('Also verify the pipe and basement dimensions to avoid conflicts there.')
                CALL ShowFatalError('Preceding error causes program termination')

            END IF
        END DO

        ThesePartitionRegions(Index)%Min = CellLeft
        ThesePartitionRegions(Index)%Max = CellRight

        !Need to map partition type into region type parameters, since they are different enumerations
        SELECT CASE (ThisPartitionType)
        CASE (PartitionType_BasementWall)
            ThesePartitionRegions(Index)%RegionType = RegionType_BasementWall
        CASE (PartitionType_BasementFloor)
            ThesePartitionRegions(Index)%RegionType = RegionType_BasementFloor
        CASE (PartitionType_Pipe)
            ThesePartitionRegions(Index)%RegionType = RegionType_Pipe
        CASE DEFAULT
            !diagnostic error
        END SELECT

    END DO

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
INTEGER FUNCTION CreateRegionListCount(ThesePartitionRegions, DirExtentMax, PartitionsExist) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: ThesePartitionRegions
    REAL(r64), INTENT(IN) :: DirExtentMax
    LOGICAL, INTENT(IN) :: PartitionsExist

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Index

    RetVal = 0
    IF (PartitionsExist) THEN
        DO Index = LBOUND(ThesePartitionRegions,1), UBOUND(ThesePartitionRegions,1)
            !'add a mesh region to the "left" of the partition
            RetVal = RetVal + 1
            !'then add the pipe node itself
            RetVal = RetVal + 1
            !some cleanup based on where we are
            IF ((Index==0 .AND. SIZE(ThesePartitionRegions)==1) .OR. &
                (Index == UBOUND(ThesePartitionRegions,1) .AND. ThesePartitionRegions(Index)%Max < DirExtentMax)) THEN
                !'if there is only one partition, add a mesh region to the "right" before we leave
                !'or if we are on the last partition, and we have room on the "right" side then add a mesh region
                RetVal = RetVal + 1
            END IF
        END DO
    ELSE !Input partitions were not allocate
        !'if we don't have a region, we still need to make a single mesh region
        RetVal = RetVal + 1
    END IF

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
FUNCTION CreateRegionList(DomainNum, ThesePartitionRegions, DirExtentMax, &
                          DirDirection, RetValUbound, PartitionsExist, &
                          BasementWallXIndex, BasementFloorYIndex)   RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: ThesePartitionRegions
    REAL(r64), INTENT(IN) :: DirExtentMax
    INTEGER, INTENT(IN) :: DirDirection
    INTEGER, INTENT(IN) :: RetValUBound
    LOGICAL, INTENT(IN) :: PartitionsExist
    INTEGER, INTENT(IN OUT), OPTIONAL :: BasementWallXIndex
    INTEGER, INTENT(IN OUT), OPTIONAL :: BasementFloorYIndex

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    TYPE(GridRegion), DIMENSION(0:RetValUbound) :: RetVal
    TYPE(TempGridRegionData), DIMENSION(0:RetValUbound) :: TempRegions

    TYPE(GridRegion) :: ThisRegion
    TYPE(TempGridRegionData) :: PreviousRegion
    REAL(r64) :: LeftRegionExtent
    INTEGER :: PreviousUbound
    INTEGER :: Index
    INTEGER :: SubIndex
    INTEGER :: CellCountUpToNow
    INTEGER :: NumCellWidths

    PreviousUbound = -1
    IF (PartitionsExist) THEN
        DO Index = LBOUND(ThesePartitionRegions,1), UBOUND(ThesePartitionRegions,1)

            ThisRegion = ThesePartitionRegions(Index)

            IF (Index == 0) THEN
                LeftRegionExtent = 0.0d0
            ELSE
                LeftRegionExtent = ThesePartitionRegions(Index - 1)%Max
            END IF

            !'add a mesh region to the "left" of the partition
            PreviousUbound = PreviousUbound + 1
            TempRegions(PreviousUbound) = TempGridRegionData(LeftRegionExtent, ThisRegion%Min, DirDirection)

            !'alert calling routines to the location of the basement cells within the domain
            CellCountUpToNow = 0

            DO SubIndex = LBOUND(TempRegions,1), PreviousUbound
                PreviousRegion = TempRegions(SubIndex)

                SELECT CASE (PreviousRegion%RegionType)
                CASE (RegionType_Pipe, RegionType_BasementFloor, RegionType_BasementWall)
                    CellCountUpToNow = CellCountUpToNow + 1
                CASE DEFAULT
                    CellCountUpToNow = CellCountUpToNow + GetCellWidthsCount(DomainNum, DirDirection)
                END SELECT

            END DO

            IF (ThisRegion%RegionType == RegionType_BasementWall) THEN
                IF (PRESENT(BasementWallXIndex)) BasementWallXIndex = CellCountUpToNow
            ELSEIF (ThisRegion%RegionType == RegionType_BasementFloor) THEN
                IF (PRESENT(BasementFloorYIndex)) BasementFloorYIndex = CellCountUpToNow
            END IF

            !'then add the pipe node itself
            PreviousUbound = PreviousUbound + 1
            TempRegions(PreviousUbound) = TempGridRegionData(ThisRegion%Min, ThisRegion%Max, ThisRegion%RegionType)

            !some cleanup based on where we are
            IF ((Index==0 .AND. SIZE(ThesePartitionRegions)==1) .OR. &
                (Index == UBOUND(ThesePartitionRegions,1) .AND. ThisRegion%Max < DirExtentMax)) THEN
                !'if there is only one partition, add a mesh region to the "right" before we leave
                !'or if we are on the last partition, and we have room on the "right" side then add a mesh region
                PreviousUbound = PreviousUbound + 1
                TempRegions(PreviousUbound) = TempGridRegionData(ThisRegion%Max, DirExtentMax, DirDirection)
            END IF

        END DO
    ELSE !Input partitions were not allocate
        !'if we don't have a region, we still need to make a single mesh region
        TempRegions(0) = TempGridRegionData(0.0d0, DirExtentMax, DirDirection)
    END IF

    !'finally repackage the grid regions into the final class form with cell counts included
    DO Index = LBOUND(TempRegions,1), UBOUND(TempRegions,1)
        RetVal(Index)%Min = TempRegions(Index)%Min
        RetVal(Index)%Max = TempRegions(Index)%Max
        RetVal(Index)%RegionType = TempRegions(Index)%RegionType
        NumCellWidths = GetCellWidthsCount(DomainNum, DirDirection)
        IF (ALLOCATED(RetVal(Index)%CellWidths)) DEALLOCATE(RetVal(Index)%CellWidths)
        ALLOCATE (RetVal(Index)%CellWidths(0:NumCellWidths-1))
!        write(outputfiledebug,*) '3332,index,retval',index, retval(index)%regiontype, &
!          numcellwidths
        CALL GetCellWidths(DomainNum,RetVal(Index))
!        RetVal(Index)%CellWidths = GetCellWidths(DomainNum, RetVal(Index))
    END DO

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
INTEGER FUNCTION CreateBoundaryListCount(RegionList, dirDirection) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: RegionList
    INTEGER, INTENT(IN) :: dirDirection

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Index
    INTEGER :: CellWidthCtr

    RetVal = 0

    DO Index = LBOUND(RegionList,1), UBOUND(RegionList,1)
        SELECT CASE (RegionList(Index)%RegionType)
        CASE (RegionType_Pipe, RegionType_BasementFloor, RegionType_BasementWall)
            RetVal = RetVal + 1
        CASE DEFAULT
            IF (RegionList(Index)%RegionType == dirDirection) THEN
                DO CellWidthCtr = LBOUND(RegionList(Index)%CellWidths,1), UBOUND(RegionList(Index)%CellWidths,1)
                    RetVal = RetVal + 1
                END DO
            END IF
        END SELECT
    END DO
    RetVal = RetVal + 1

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
FUNCTION CreateBoundaryList(RegionList, DirExtentMax, DirDirection, RetValLbound, RetValUbound) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(GridRegion), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: RegionList
    REAL(r64), INTENT(IN) :: DirExtentMax
    INTEGER, INTENT(IN) :: DirDirection
    INTEGER, INTENT(IN) :: RetValLbound
    INTEGER, INTENT(IN) :: RetValUbound

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: StartingPointCounter
    INTEGER :: Index
    INTEGER :: Counter
    INTEGER :: CellWidthCtr

          ! FUNCTION RETURN VALUE
    REAL(r64) :: RetVal(RetValLbound:RetValUbound)

    Counter = -1
    DO Index = LBOUND(RegionList,1), UBOUND(RegionList,1)
        SELECT CASE (RegionList(Index)%RegionType)
        CASE (RegionType_Pipe, RegionType_BasementFloor, RegionType_BasementWall)
            Counter = Counter + 1
            RetVal(Counter) = RegionList(Index)%Min
        CASE DEFAULT
            IF (RegionList(Index)%RegionType == dirDirection) THEN
                StartingPointCounter = RegionList(Index)%Min
                DO CellWidthCtr = LBOUND(RegionList(Index)%CellWidths,1), UBOUND(RegionList(Index)%CellWidths,1)
                    Counter = Counter + 1
                    RetVal(Counter) = StartingPointCounter
                    StartingPointCounter = StartingPointCounter + RegionList(Index)%CellWidths(CellWidthCtr)
                END DO
            END IF
        END SELECT
    END DO
    RetVal(UBOUND(RetVal,1)) = dirExtentMax

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE CreateCellArray(DomainNum, XBoundaryPoints, YBoundaryPoints, &
                           ZBoundaryPoints, MaxBasementXNodeIndex, MinBasementYNodeIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    REAL(r64), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: XBoundaryPoints
    REAL(r64), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: YBoundaryPoints
    REAL(r64), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: ZBoundaryPoints
    INTEGER, INTENT(IN) :: MaxBasementXNodeIndex
    INTEGER, INTENT(IN) :: MinBasementYNodeIndex

          ! DERIVED TYPE DEFINITIONS:
    TYPE tCellExtents
        TYPE(MeshExtents) :: MyBase
        REAL(r64)         :: Xmin
        REAL(r64)         :: Ymin
        REAL(r64)         :: Zmin
    END TYPE tCellExtents

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: YIndexMax
    TYPE(DomainRectangle) :: BasementRectangle
    TYPE(tCellExtents) :: CellExtents
    TYPE(Point3DReal) :: Centroid
    TYPE(Point3DInteger) :: CellIndeces
    TYPE(RectangleF) :: XYRectangle
    INTEGER :: CellType !From Enum: CellType
    INTEGER :: ZWallCellType !From Enum: CellType
    INTEGER :: UnderBasementBoundary !From Enum: CellType
    INTEGER :: PipeCounter

    INTEGER   :: X
    INTEGER   :: CellXIndex
    REAL(r64) :: CellXMinValue
    REAL(r64) :: CellXMaxValue
    REAL(r64) :: CellXCenter
    REAL(r64) :: CellWidth

    INTEGER   :: Y
    INTEGER   :: CellYIndex
    REAL(r64) :: CellYMinValue
    REAL(r64) :: CellYMaxValue
    REAL(r64) :: CellYCenter
    REAL(r64) :: CellHeight

    INTEGER   :: Z
    INTEGER   :: CellZIndex
    REAL(r64) :: CellZMinValue
    REAL(r64) :: CellZMaxValue
    REAL(r64) :: CellZCenter
    REAL(r64) :: CellDepth

    INTEGER :: PipeIndex
    INTEGER :: NumRadialCells
    REAL(r64) :: InsulationThickness
    INTEGER :: CircuitCtr
    INTEGER :: CircuitIndex
    INTEGER :: FoundOnCircuitIndex

    TYPE(RadialSizing) :: PipeSizing
    TYPE(PipeSegmentInfo) :: ThisSegment
    REAL(r64) :: RadialMeshThickness
    LOGICAL :: HasInsulation

    !'subtract 2 in each dimension:
    !'     one for zero based array
    !'     one because the boundary points contain one entry more than the number of cells WITHIN the domain
    ALLOCATE(PipingSystemDomains(DomainNum)%Cells( 0:SIZE(XBoundaryPoints) - 2, &
                                                   0:SIZE(YBoundaryPoints) - 2, &
                                                   0:SIZE(ZBoundaryPoints) - 2)  )

    YIndexMax = UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)
    BasementRectangle = DomainRectangle(0, MaxBasementXNodeIndex, MinBasementYNodeIndex, YIndexMax)

    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells,3), UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells,2), UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells,1), UBOUND(PipingSystemDomains(DomainNum)%Cells,1)

                !'set up x-direction variables
                CellXIndex = X  !'zero based index
                CellXMinValue = XBoundaryPoints(X)  !'left wall x-value
                CellXMaxValue = XBoundaryPoints(X + 1)  !'right wall x-value
                CellXCenter = (CellXMinValue + CellXMaxValue) / 2
                CellWidth = CellXMaxValue - CellXMinValue

                !'set up y-direction variables
                CellYIndex = Y !'zero based index
                CellYMinValue = YBoundaryPoints(Y)  !'bottom wall y-value
                CellYMaxValue = YBoundaryPoints(Y + 1)  !'top wall y-value
                CellYCenter = (CellYMinValue + CellYMaxValue) / 2
                CellHeight = CellYMaxValue - CellYMinValue

                !'set up z-direction variables
                CellZIndex = Z  !'zero based index
                CellZMinValue = ZBoundaryPoints(Z)  !'lower z value
                CellZMaxValue = ZBoundaryPoints(Z + 1)  !'higher z value
                CellZCenter = (CellZMinValue + CellZMaxValue) / 2
                CellDepth = CellZMaxValue - CellZMinValue

                !'set up an extent class for this cell
                CellExtents = tCellExtents(MeshExtents(CellXMaxValue, CellYMaxValue, CellZMaxValue), &
                                                                CellXMinValue, CellYMinValue, CellZMinValue)

                !'set up centroid, index, and overall size
                Centroid = Point3DReal(CellXCenter, CellYCenter, CellZCenter)
                CellIndeces = Point3DInteger(CellXIndex, CellYIndex, CellZIndex)
                XYRectangle = RectangleF(CellXMinValue, CellYMinValue, CellWidth, CellHeight)

                !'determine cell type
                CellType = CellType_Unknown

                !'if this is a pipe node, some flags are needed
                PipeIndex = -1
                NumRadialCells = -1
                CircuitIndex = -1

                !Since we removed the z wall cell type to always be adiabatic, this is only temporary
                ZWallCellType = CellType_AdiabaticWall
                UnderBasementBoundary = CellType_AdiabaticWall

                !'apply boundary conditions
                IF (CellXIndex == MaxBasementXNodeIndex .AND. CellYIndex == MinBasementYNodeIndex) THEN
                    CellType = CellType_BasementCorner
                ELSE IF (CellXIndex == MaxBasementXNodeIndex .AND. CellYIndex > MinBasementYNodeIndex) THEN
                    CellType = CellType_BasementWall
                ELSE IF (CellXIndex < MaxBasementXNodeIndex .AND. CellYIndex == MinBasementYNodeIndex) THEN
                    CellType = CellType_BasementFloor
                ELSE IF (CellXIndex < MaxBasementXNodeIndex .AND. CellYIndex > MinBasementYNodeIndex) THEN
                    CellType = CellType_BasementCutAway
                ELSE IF (CellYIndex == UBOUND(PipingSystemDomains(DomainNum)%Cells,2)) THEN
                    CellType = CellType_GroundSurface
                ELSE IF (CellXIndex == 0) THEN
                    IF (PipingSystemDomains(DomainNum)%HasBasement .AND. Y>0) THEN
                        CellType = UnderBasementBoundary !'this must come after the basement cutaway ELSEIF branch
                    ELSE
                        CellType = CellType_FarfieldBoundary
                    END IF
                ELSE IF (CellXIndex == UBOUND(PipingSystemDomains(DomainNum)%Cells,1) .OR. CellYIndex == 0) THEN
                    CellType = CellType_FarfieldBoundary
                ELSE IF (CellZIndex == 0 .OR. CellZIndex == UBOUND(PipingSystemDomains(DomainNum)%Cells,3)) THEN
                    CellType = ZWallCellType
                END IF

                !'check to see if this is a pipe node...
CircuitLoop:    DO CircuitCtr = LBOUND(PipingSystemDomains(DomainNum)%CircuitIndeces, 1), &
                                UBOUND(PipingSystemDomains(DomainNum)%CircuitIndeces, 1)

                    FoundOnCircuitIndex = PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitCtr)
SegmentLoop:        DO PipeCounter = LBOUND(PipingSystemCircuits(FoundOnCircuitIndex)%PipeSegmentIndeces,1), &
                                     UBOUND(PipingSystemCircuits(FoundOnCircuitIndex)%PipeSegmentIndeces,1)

                        ThisSegment=PipingSystemSegments(PipingSystemCircuits(FoundOnCircuitIndex)%PipeSegmentIndeces(PipeCounter))
                        IF (RectangleF_Contains(XYRectangle, ThisSegment%PipeLocation)) THEN
                            !'inform the cell that it is a pipe node
                            CellType = CellType_Pipe
                            !'inform the cell of which pipe it contains
                            PipeIndex = PipeCounter
                            !'inform the cell of which pipe circuit contains it
                            CircuitIndex = FoundOnCircuitIndex
                            !'inform the pipe of what cell it is inside
                            CALL PipeSegmentInfo_InitPipeCells( &
                                 PipingSystemSegments(PipingSystemCircuits(FoundOnCircuitIndex)%PipeSegmentIndeces(PipeCounter)), &
                                 CellXIndex, CellYIndex)
                            !'set the number of cells to be generated in this near-pipe region
                            NumRadialCells = PipingSystemCircuits(FoundOnCircuitIndex)%NumRadialCells
                            !'exit the pipe counter loop
                            EXIT CircuitLoop
                        END IF

                    END DO SegmentLoop

                END DO CircuitLoop

                !'if it still isn't anything, then it is just an interior node
                IF (CellType == CellType_Unknown) THEN
                    CellType = CellType_GeneralField
                END IF

                ! if we were found on a pipe circuit, get some things for convenience
                IF (CircuitIndex .NE. -1) THEN
                    IF (PipingSystemCircuits(CircuitIndex)%HasInsulation) THEN
                        InsulationThickness = RadialSizing_Thickness(PipingSystemCircuits(CircuitIndex)%InsulationSize)
                    END IF
                    PipeSizing = PipingSystemCircuits(CircuitIndex)%PipeSize
                    RadialMeshThickness = PipingSystemCircuits(CircuitIndex)%RadialMeshThickness
                    HasInsulation = PipingSystemCircuits(CircuitIndex)%HasInsulation
                END IF

                !'instantiate the cell class
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%X_min = CellExtents%Xmin
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%X_max = CellExtents%MyBase%Xmax
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Y_min = CellExtents%Ymin
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Y_max = CellExtents%MyBase%Ymax
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Z_min = CellExtents%Zmin
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Z_max = CellExtents%MyBase%Zmax
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%X_index = CellIndeces%X
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Y_index = CellIndeces%Y
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Z_index = CellIndeces%Z
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Centroid = Centroid
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType = CellType

                IF (PipeIndex .NE. -1) THEN
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeIndex = PipeIndex
                    CALL CartesianPipeCellInformation_ctor(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData, &
                      PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%X_max - PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%X_min, &
                      PipeSizing, &
                      NumRadialCells, &
                      Depth(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)), &
                      InsulationThickness, &
                      RadialMeshThickness, &
                      HasInsulation)
                END IF

            END DO !'z
        END DO !'y
    END DO !'x

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SetupCellNeighbors(DomainNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: X, Y, Z
    REAL(r64) :: ThisCellCentroidX
    REAL(r64) :: ThisCellCentroidY
    REAL(r64) :: ThisCellCentroidZ
    REAL(r64) :: CellRightCentroidX
    REAL(r64) :: CellRightLeftWallX
    REAL(r64) :: CellLeftCentroidX
    REAL(r64) :: CellLeftRightWallX
    REAL(r64) :: LeftCellCentroidX
    REAL(r64) :: LeftCellRightWallX
    REAL(r64) :: RightCellCentroidX
    REAL(r64) :: RightCellLeftWallX
    REAL(r64) :: UpperCellCentroidY
    REAL(r64) :: UpperCellLowerWallY
    REAL(r64) :: LowerCellCentroidY
    REAL(r64) :: LowerCellUpperWallY
    REAL(r64) :: UpperZCellCentroidZ
    REAL(r64) :: UpperZCellLowerWallZ
    REAL(r64) :: LowerZCellCentroidZ
    REAL(r64) :: LowerZCellUpperWallZ

    DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells, 1), UBOUND(PipingSystemDomains(DomainNum)%Cells, 1)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells, 2), UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)
            DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells, 3), UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)

                !'for convenience
                ThisCellCentroidX = PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Centroid%X
                ThisCellCentroidY = PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Centroid%Y
                ThisCellCentroidZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%Centroid%Z

                !'setup east/west cell neighbors
                IF (X == 0) THEN
                    CellRightCentroidX = PipingSystemDomains(DomainNum)%Cells(X + 1, Y, Z)%Centroid%X
                    CellRightLeftWallX = PipingSystemDomains(DomainNum)%Cells(X + 1, Y, Z)%X_min
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveX, CellRightCentroidX - ThisCellCentroidX, &
                                                                                         CellRightLeftWallX - ThisCellCentroidX, &
                                                                                         CellRightCentroidX - CellRightLeftWallX)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeX, 0.0d0, 0.0d0, 0.0d0)
                ELSE IF (X == UBOUND(PipingSystemDomains(DomainNum)%Cells,1)) THEN
                    CellLeftCentroidX = PipingSystemDomains(DomainNum)%Cells(X - 1, Y, Z)%Centroid%X
                    CellLeftRightWallX = PipingSystemDomains(DomainNum)%Cells(X - 1, Y, Z)%X_max
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeX, ThisCellCentroidX - CellLeftCentroidX, &
                                                                                         ThisCellCentroidX - CellLeftRightWallX, &
                                                                                         CellLeftRightWallX - CellLeftCentroidX)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveX, 0.0d0, 0.0d0, 0.0d0)
                ELSE
                    LeftCellCentroidX = PipingSystemDomains(DomainNum)%Cells(X - 1, Y, Z)%Centroid%X
                    LeftCellRightWallX = PipingSystemDomains(DomainNum)%Cells(X - 1, Y, Z)%X_max
                    RightCellCentroidX = PipingSystemDomains(DomainNum)%Cells(X + 1, Y, Z)%Centroid%X
                    RightCellLeftWallX = PipingSystemDomains(DomainNum)%Cells(X + 1, Y, Z)%X_min
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeX, ThisCellCentroidX - LeftCellCentroidX, &
                                                                                         ThisCellCentroidX - LeftCellRightWallX, &
                                                                                         LeftCellRightWallX - LeftCellCentroidX)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveX, RightCellCentroidX - ThisCellCentroidX, &
                                                                                         RightCellLeftWallX - ThisCellCentroidX, &
                                                                                         RightCellCentroidX - RightCellLeftWallX)
                END IF

                !'setup north/south cell neighbors
                IF (Y == 0) THEN
                    UpperCellCentroidY = PipingSystemDomains(DomainNum)%Cells(X, Y + 1, Z)%Centroid%Y
                    UpperCellLowerWallY = PipingSystemDomains(DomainNum)%Cells(X, Y + 1, Z)%Y_min
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveY, UpperCellCentroidY - ThisCellCentroidY, &
                                                                                         UpperCellLowerWallY - ThisCellCentroidY, &
                                                                                         UpperCellCentroidY - UpperCellLowerWallY)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeY, 0.0d0, 0.0d0, 0.0d0)
                ELSE IF (Y == UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)) THEN
                    LowerCellCentroidY = PipingSystemDomains(DomainNum)%Cells(X, Y - 1, Z)%Centroid%Y
                    LowerCellUpperWallY = PipingSystemDomains(DomainNum)%Cells(X, Y - 1, Z)%Y_max
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeY, ThisCellCentroidY - LowerCellCentroidY, &
                                                                                         ThisCellCentroidY - LowerCellUpperWallY, &
                                                                                         LowerCellUpperWallY - LowerCellCentroidY)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveY, 0.0d0, 0.0d0, 0.0d0)
                ELSE
                    UpperCellCentroidY = PipingSystemDomains(DomainNum)%Cells(X, Y + 1, Z)%Centroid%Y
                    LowerCellCentroidY = PipingSystemDomains(DomainNum)%Cells(X, Y - 1, Z)%Centroid%Y
                    UpperCellLowerWallY = PipingSystemDomains(DomainNum)%Cells(X, Y + 1, Z)%Y_min
                    LowerCellUpperWallY = PipingSystemDomains(DomainNum)%Cells(X, Y - 1, Z)%Y_max
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeY, ThisCellCentroidY - LowerCellCentroidY, &
                                                                                         ThisCellCentroidY - LowerCellUpperWallY, &
                                                                                         LowerCellUpperWallY - LowerCellCentroidY)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveY, UpperCellCentroidY - ThisCellCentroidY, &
                                                                                         UpperCellLowerWallY - ThisCellCentroidY, &
                                                                                         UpperCellCentroidY - UpperCellLowerWallY)
                END IF

                !'setup forward/backward cell neighbors
                IF (Z==0) THEN
                    UpperZCellCentroidZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z + 1)%Centroid%Z
                    UpperZCellLowerWallZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z + 1)%Z_min
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveZ, UpperZCellCentroidZ - ThisCellCentroidZ, &
                                                                                         UpperZCellLowerWallZ - ThisCellCentroidZ, &
                                                                                         UpperZCellCentroidZ - UpperZCellLowerWallZ)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeZ, 0.0d0, 0.0d0, 0.0d0)
                ELSE IF (Z == UBOUND(PipingSystemDomains(DomainNum)%Cells,3)) THEN
                    LowerZCellCentroidZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z - 1)%Centroid%Z
                    LowerZCellUpperWallZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z - 1)%Z_max
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeZ, ThisCellCentroidZ - LowerZCellCentroidZ, &
                                                                                         ThisCellCentroidZ - LowerZCellUpperWallZ, &
                                                                                         LowerZCellUpperWallZ - LowerZCellCentroidZ)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveZ, 0.0d0, 0.0d0, 0.0d0)
                ELSE
                    LowerZCellCentroidZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z - 1)%Centroid%Z
                    UpperZCellCentroidZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z + 1)%Centroid%Z
                    UpperZCellLowerWallZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z + 1)%Z_min
                    LowerZCellUpperWallZ = PipingSystemDomains(DomainNum)%Cells(X, Y, Z - 1)%Z_max
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_NegativeZ, ThisCellCentroidZ - LowerZCellCentroidZ, &
                                                                                         ThisCellCentroidZ - LowerZCellUpperWallZ, &
                                                                                         LowerZCellUpperWallZ - LowerZCellCentroidZ)
                    CALL AddNeighborInformation(DomainNum, X,Y,Z, Direction_PositiveZ, UpperZCellCentroidZ - ThisCellCentroidZ, &
                                                                                         UpperZCellLowerWallZ - ThisCellCentroidZ, &
                                                                                         UpperZCellCentroidZ - UpperZCellLowerWallZ)
                END IF

            END DO
        END DO
    END DO

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE AddNeighborInformation(DomainNum, X, Y, Z, Direction, ThisCentroidToNeighborCentroid, &
                                                                    ThisCentroidToNeighborWall, ThisWallToNeighborCentroid)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: X
    INTEGER, INTENT(IN) :: Y
    INTEGER, INTENT(IN) :: Z
    INTEGER, INTENT(IN) :: Direction !From Enum: Direction
    REAL(r64), INTENT(IN) :: ThisCentroidToNeighborCentroid
    REAL(r64), INTENT(IN) :: ThisCentroidToNeighborWall
    REAL(r64), INTENT(IN) :: ThisWallToNeighborCentroid

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE(DirectionNeighbor_Dictionary), ALLOCATABLE, DIMENSION(:) :: PrevValues
    INTEGER :: PrevUbound

    IF (.NOT. ALLOCATED(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation)) THEN
        ALLOCATE(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(0:0))
        PrevUBound = -1
    ELSE
        PrevUbound = UBOUND(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation, 1)
        ALLOCATE(PrevValues(0:PrevUbound))
        PrevValues = PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation
        DEALLOCATE(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation)
        ALLOCATE(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(0:PrevUbound+1))
        PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(0:PrevUbound) = PrevValues
        DEALLOCATE(PrevValues)
    END IF

    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(PrevUbound+1)%Direction = Direction

    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(PrevUbound+1)%Value%ThisCentroidToNeighborCentroid = &
                                                                                                ThisCentroidToNeighborCentroid

    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(PrevUbound+1)%Value%ThisCentroidToNeighborWall = &
                                                                                                ThisCentroidToNeighborWall

    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%NeighborInformation(PrevUbound+1)%Value%ThisWallToNeighborCentroid = &
                                                                                                ThisWallToNeighborCentroid

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SetupPipeCircuitInOutCells(DomainNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: CircuitNum
    INTEGER :: CircuitIndex
    LOGICAL ::  CircuitInletCellSet
    TYPE(CartesianCell) :: CircuitInletCell
    TYPE(CartesianCell) :: CircuitOutletCell
    TYPE(CartesianCell) :: SegmentInletCell
    TYPE(CartesianCell) :: SegmentOutletCell
    TYPE(PipeSegmentInfo) :: Segment
    INTEGER :: SegmentCtr

    INTEGER :: SegmentInletCellX, SegmentInletCellY, SegmentInletCellZ ,SegmentOutletCellX, SegmentOutletCellY, SegmentOutletCellZ
    INTEGER :: CircuitInletCellX, CircuitInletCellY, CircuitInletCellZ ,CircuitOutletCellX, CircuitOutletCellY, CircuitOutletCellZ

    DO CircuitNum = LBOUND(PipingSystemDomains(DomainNum)%CircuitIndeces, 1), &
                    UBOUND(PipingSystemDomains(DomainNum)%CircuitIndeces, 1)

        CircuitIndex = PipingSystemDomains(DomainNum)%CircuitIndeces(CircuitNum)
        CircuitInletCellSet = .FALSE.

        DO SegmentCtr = LBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1), &
                        UBOUND(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces, 1)

            Segment = PipingSystemSegments(PipingSystemCircuits(CircuitIndex)%PipeSegmentIndeces(SegmentCtr))
            SELECT CASE (Segment%FlowDirection)
                CASE (SegmentFlow_IncreasingZ)

                  SegmentInletCellX = segment%PipeCellCoordinates%X
                  SegmentInletCellY = segment%PipeCellCoordinates%Y
                  SegmentInletCellZ = 0

                  SegmentOutletCellX = segment%PipeCellCoordinates%X
                  SegmentOutletCellY = segment%PipeCellCoordinates%Y
                  SegmentOutletCellZ = UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)

                CASE (SegmentFlow_DecreasingZ)

                  SegmentInletCellX = segment%PipeCellCoordinates%X
                  SegmentInletCellY = segment%PipeCellCoordinates%Y
                  SegmentInletCellZ = UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)

                  SegmentOutletCellX = segment%PipeCellCoordinates%X
                  SegmentOutletCellY = segment%PipeCellCoordinates%Y
                  SegmentOutletCellZ = 0

            END SELECT
            IF (.NOT. CircuitInletCellSet) THEN
                CircuitInletCellX = SegmentInletCellX
                CircuitInletCellY = SegmentInletCellY
                CircuitInletCellZ = SegmentInletCellZ
                CircuitInletCellSet = .TRUE.
            END IF
            CircuitOutletCellX = SegmentOutletCellX
            CircuitOutletCellY = SegmentOutletCellY
            CircuitOutletCellZ = SegmentOutletCellZ

        END DO

        CALL PipeCircuitInfo_InitInOutCells(PipingSystemCircuits(CircuitIndex), &
                                             PipingSystemDomains(DomainNum)%Cells(CircuitInletCellX,  &
                                                                                  CircuitInletCellY,  &
                                                                                  CircuitInletCellZ), &
                                             PipingSystemDomains(DomainNum)%Cells(CircuitOutletCellX, &
                                                                                  CircuitOutletCellY, &
                                                                                  CircuitOutletCellZ) &
                                            )

    END DO

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
INTEGER FUNCTION GetCellWidthsCount(DomainNum, dir) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: dir !From Enum: RegionType

    SELECT CASE (dir)
    CASE (RegionType_XDirection)
        RetVal = PipingSystemDomains(DomainNum)%Mesh%X%RegionMeshCount
    CASE (RegionType_YDirection)
        RetVal = PipingSystemDomains(DomainNum)%Mesh%Y%RegionMeshCount
    CASE (RegionType_ZDirection)
        RetVal = PipingSystemDomains(DomainNum)%Mesh%Z%RegionMeshCount
    END SELECT
    !Objexx:Return Check/enforce that one of these CASEs holds to assure return value is set

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
!FUNCTION GetCellWidths(DomainNum, g) RESULT(RetVal)
SUBROUTINE GetCellWidths(DomainNum, g)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(GridRegion) :: g

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    TYPE(DistributionStructure) :: ThisMesh
    REAL(r64) :: GridWidth
    INTEGER :: NumCellsOnEachSide
    REAL(r64) :: SummationTerm
    INTEGER :: I
    REAL(r64) :: CellWidth
    INTEGER :: SubIndex
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: RetVal
    INTEGER :: RetMaxIndex
!write(outputfiledebug,*) ' regiontype=',g%RegionType
    !'determine which mesh "direction" we are going to be using

    ThisMesh%MeshDistribution =0 !From Enum: MeshDistribution
    ThisMesh%RegionMeshCount=0
    ThisMesh%GeometricSeriesCoefficient  = 0.0d0

    SELECT CASE (g%RegionType)
    CASE (RegionType_XDirection)
        ThisMesh = PipingSystemDomains(DomainNum)%Mesh%X
    CASE (RegionType_YDirection)
        ThisMesh = PipingSystemDomains(DomainNum)%Mesh%Y
    CASE (RegionType_ZDirection)
        ThisMesh = PipingSystemDomains(DomainNum)%Mesh%Z
    CASE DEFAULT
        !Error
    END SELECT

!write(outputfiledebug,*) 'thismesh%regionmeshcount=',ThisMesh%RegionMeshCount
    IF (ThisMesh%RegionMeshCount > 0) THEN
      ALLOCATE(RetVal(0:ThisMesh%RegionMeshCount-1))
      RetMaxIndex=ThisMesh%RegionMeshCount-1
    ELSE
      ALLOCATE(RetVal(0:0))
      RetMaxIndex=0
    ENDIF

    GridWidth = g%Max - g%Min

    IF (ThisMesh%MeshDistribution == MeshDistribution_Uniform) THEN

        !we have it quite simple

        CellWidth = GridWidth / ThisMesh%RegionMeshCount

        DO I = 0, ThisMesh%RegionMeshCount - 1
            RetVal(I) = CellWidth
        END DO

    ELSEIF (ThisMesh%MeshDistribution == MeshDistribution_SymmetricGeometric) THEN

        !'then apply this "direction"'s conditions to generate a cell width array
        !'first get the total number of cells on this half of the region
        NumCellsOnEachSide = ThisMesh%RegionMeshCount / 2  !Already validated to be an even #

        !'calculate geometric series
        SummationTerm = 0.0d0
        DO I = 1, NumCellsOnEachSide
            SummationTerm = SummationTerm + ThisMesh%GeometricSeriesCoefficient ** (I - 1)
        END DO

        !'set up a list of cell widths for this region
        CellWidth = (GridWidth / 2) / SummationTerm
        RetVal(0) = CellWidth
        DO I = 1, NumCellsOnEachSide - 1
            CellWidth = CellWidth * ThisMesh%GeometricSeriesCoefficient
            RetVal(I) = CellWidth
        END DO
        SubIndex = NumCellsOnEachSide
        DO I = NumCellsOnEachSide-1, 0, -1
            SubIndex = SubIndex + 1
            RetVal(SubIndex) = RetVal(I)
        END DO

    END IF

    g%CellWidths(0:RetMaxIndex)=RetVal(0:RetMaxIndex)
    DEALLOCATE(RetVal)
!END FUNCTION
END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PerformIterationLoop(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: IterationIndex
    LOGICAL :: FinishedIterationLoop

    ! Always do start of time step inits
    CALL DoStartOfTimeStepInitializations(DomainNum, CircuitNum)

    ! Prepare the pipe circuit for calculations, but we'll actually do calcs at the iteration level
    CALL PreparePipeCircuitSimulation(DomainNum, CircuitNum)

    ! Begin iterating for this time step
    DO IterationIndex = 1, PipingSystemDomains(DomainNum)%SimControls%MaxIterationsPerTS

        CALL ShiftTemperaturesForNewIteration(DomainNum)

        CALL PerformPipeCircuitSimulation(DomainNum, CircuitNum)

        IF (PipingSystemDomains(DomainNum)%DomainNeedsSimulation) CALL PerformTemperatureFieldUpdate(DomainNum)

        FinishedIterationLoop = .FALSE.
        CALL DoEndOfIterationOperations(DomainNum, FinishedIterationLoop)
        IF (FinishedIterationLoop) EXIT

    END DO

    ! Update the basement surface temperatures, if any
    IF (PipingSystemDomains(DomainNum)%HasBasement) THEN
        CALL UpdateBasementSurfaceTemperatures(DomainNum)
    END IF

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PerformTemperatureFieldUpdate(DomainNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: X, Y, Z

    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells, 3), UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells, 2), UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells, 1), UBOUND(PipingSystemDomains(DomainNum)%Cells, 1)

                SELECT CASE (PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%CellType)
                CASE (CellType_Pipe)
                    !'pipes are simulated separately
                CASE (CellType_GeneralField)
                    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%MyBase%Temperature = &
                                EvaluateFieldCellTemperature(DomainNum, PipingSystemDomains(DomainNum)%Cells(X,Y,Z))
                CASE (CellType_GroundSurface)
                    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%MyBase%Temperature = &
                                EvaluateGroundSurfaceTemperature(DomainNum, PipingSystemDomains(DomainNum)%Cells(X,Y,Z))
                CASE (CellType_FarfieldBoundary)
                    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%MyBase%Temperature = &
                                EvaluateFarfieldBoundaryTemperature(DomainNum, PipingSystemDomains(DomainNum)%Cells(X,Y,Z))
                CASE (CellType_BasementWall, CellType_BasementCorner, CellType_BasementFloor)
                    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%MyBase%Temperature = &
                                EvaluateBasementCellTemperature(DomainNum, PipingSystemDomains(DomainNum)%Cells(X,Y,Z))
                CASE (CellType_AdiabaticWall)
                    PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%MyBase%Temperature = &
                                EvaluateAdiabaticSurfaceTemperature(DomainNum, PipingSystemDomains(DomainNum)%Cells(X,Y,Z))
                END SELECT

            END DO
        END DO
    END DO

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION EvaluateFieldCellTemperature(DomainNum, ThisCell) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: ThisCell

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Beta
    REAL(r64) :: NeighborTemp
    REAL(r64) :: Resistance
    INTEGER :: DirectionCounter
    INTEGER :: CurDirection !From Enum: Direction

    !Set up once-per-cell items
    Numerator = 0.0d0
    Denominator = 0.0d0
    Beta = ThisCell%MyBase%Beta

    !add effect from cell history
    Numerator = Numerator + ThisCell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    !determine the neighbor types based on cell location
    CALL EvaluateCellNeighborDirections(DomainNum, ThisCell)

    !loop across each direction in the simulation
    DO DirectionCounter = LBOUND(NeighborFieldCells,1), UBOUND(NeighborFieldCells,1)

        CurDirection = NeighborFieldCells(DirectionCounter)

        !'evaluate the transient expression terms
        CALL EvaluateNeighborCharacteristics(DomainNum, ThisCell, CurDirection, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + Beta / Resistance

    END DO

    !'now that we have passed all directions, update the temperature
    RetVal = Numerator / Denominator

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION EvaluateGroundSurfaceTemperature(DomainNum, cell) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataEnvironment, ONLY: Latitude, Longitude, Elevation, TimeZoneMeridian, WindSpeed
    USE DataGlobals, ONLY: SecsInDay, SecInHour

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell

          ! FUNCTION PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: AirDensity = 1.22521d0 !'[kg/m3]
    REAL(r64), PARAMETER :: AirSpecificHeat = 1003d0 !'[J/kg-K]
    !evapotranspiration parameters
    REAL(r64), PARAMETER :: MeanSolarConstant = 0.08196d0  ! 1367 [W/m2], entered in [MJ/m2-minute]
    REAL(r64), PARAMETER :: A_s  = 0.25d0 !?
    REAL(r64), PARAMETER :: B_s  = 0.5d0  !?
    REAL(r64), PARAMETER :: Absor_Corrected = 0.77d0
    REAL(r64), PARAMETER :: Convert_Wm2_To_MJhrmin = 3600.0d0 / 1000000.0d0
    REAL(r64), PARAMETER :: Convert_MJhrmin_To_Wm2 = 1.0d0 / Convert_Wm2_To_MJhrmin
    REAL(r64), PARAMETER :: Rho_water = 998.0d0  ![kg/m3]

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    !declare some variables
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: NeighborTemp
    REAL(r64) :: ThisNormalArea
    REAL(r64) :: IncidentHeatGain
    INTEGER :: DirectionCounter
    INTEGER :: CurDirection
    REAL(r64) :: AdiabaticMultiplier
    REAL(r64) :: Beta
    REAL(r64) :: Latitude_Degrees   !Latitude, degrees N
    REAL(r64) :: StMeridian_Degrees !Standard meridian, degrees W -- note it is degrees E in DataEnvironment
    REAL(r64) :: Longitude_Degrees  !Longitude, degrees W -- note it is degrees E in DataEnvironment
    !evapotranspiration calculated values
    REAL(r64) :: Latitude_Radians
    REAL(r64) :: DayOfYear
    REAL(r64) :: HourOfDay
    REAL(r64) :: CurSecondsIntoToday
    REAL(r64) :: dr
    REAL(r64) :: Declination
    REAL(r64) :: b_sc
    REAL(r64) :: Sc
    REAL(r64) :: Hour_Angle
    REAL(r64) :: X_sunset
    REAL(r64) :: Sunset_Angle
    REAL(r64) :: Altitude_Angle
    REAL(r64) :: Solar_Angle_1
    REAL(r64) :: Solar_Angle_2
    REAL(r64) :: QRAD_A
    REAL(r64) :: QRAD_SO
    REAL(r64) :: Ratio_SO
    REAL(r64) :: IncidentSolar_MJhrmin
    REAL(r64) :: AbsorbedIncidentSolar_MJhrmin
    REAL(r64) :: VaporPressureSaturated_kPa
    REAL(r64) :: VaporPressureActual_kPa
    REAL(r64) :: QRAD_NL
    REAL(r64) :: NetIncidentRadiation_MJhr ![MJ/hr]
    REAL(r64) :: NetIncidentRadiation_Wm2 ![W/m2]
    REAL(r64) :: CN
    REAL(r64) :: G_hr
    REAL(r64) :: Cd
    REAL(r64) :: Slope_S
    REAL(r64) :: Pressure
    REAL(r64) :: PsychrometricConstant
    REAL(r64) :: EvapotransFluidLoss_mmhr
    REAL(r64) :: EvapotransFluidLoss_mhr
    REAL(r64) :: LatentHeatVaporization
    REAL(r64) :: EvapotransHeatLoss_MJhrmin  ![MJ/m2-hr]
    REAL(r64) :: EvapotransHeatLoss_Wm2       ![W/m2]
    REAL(r64) :: CurAirTempK
    REAL(r64) :: MyLatitude
    REAL(r64) :: MyLongitude
    REAL(r64) :: MyTimeZoneMeridian
    REAL(r64) :: MyElevation
    REAL(r64) :: GroundCoverCoefficient

    !retrieve information from E+ globals
    Latitude_Degrees   = Latitude
    StMeridian_Degrees = -TimeZoneMeridian  !Standard meridian, degrees W
    Longitude_Degrees  = -Longitude         !Longitude, degrees W

    !retrieve any information from input data structure
    GroundCoverCoefficient = PipingSystemDomains(DomainNum)%Moisture%GroundCoverCoefficient

    !initialize values
    Numerator = 0.0d0
    Denominator = 0.0d0
    Resistance = 0.0d0
    Beta = cell%MyBase%Beta
    ThisNormalArea = NormalArea(cell, Direction_PositiveY)

    !'add effect from previous time step
    Numerator = Numerator + cell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    !now that we aren't infinitesimal, we need to determine the neighbor types based on cell location
    CALL EvaluateCellNeighborDirections(DomainNum, cell)

    !loop over all regular neighbor cells, check if we have adiabatic on opposite surface
    DO DirectionCounter = LBOUND(NeighborFieldCells,1), UBOUND(NeighborFieldCells,1)
        CurDirection = NeighborFieldCells(DirectionCounter)

        !We have adiabatic z-faces, check if we are adjacent to one in the opposite direction
        IF ( (CurDirection==Direction_NegativeZ) .AND. (cell%Z_index==UBOUND(PipingSystemDomains(DomainNum)%Cells,3)) ) THEN
            AdiabaticMultiplier = 2.0d0
        ELSE IF ( (CurDirection==Direction_PositiveZ) .AND. (cell%Z_index==0) ) THEN
            AdiabaticMultiplier = 2.0d0
        ELSE
            AdiabaticMultiplier = 1.0d0
        END IF

        !Use the multiplier (either 1 or 2) to calculate the neighbor cell effects
        CALL EvaluateNeighborCharacteristics(DomainNum, cell, CurDirection, NeighborTemp, Resistance)
        Numerator = AdiabaticMultiplier * Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = AdiabaticMultiplier * Denominator + (Beta / Resistance)

    END DO

    !do all non-adiabatic boundary types here
    DO DirectionCounter = LBOUND(NeighborBoundaryCells,1), UBOUND(NeighborBoundaryCells,1)
        CurDirection = NeighborBoundaryCells(DirectionCounter)

        !x-direction will always be a farfield boundary
        !z-direction will be handled above -- adiabatic
        !-y we don't handle here because -y will always be a neighbor cell, so handled above
        !+y will always be the outdoor air
        SELECT CASE (CurDirection)
            CASE (Direction_PositiveX, Direction_NegativeX)
                ! always farfield
                CALL EvaluateFarfieldCharacteristics(DomainNum, cell, CurDirection, NeighborTemp, Resistance)
                Numerator = Numerator + (Beta / Resistance) * NeighborTemp
                Denominator = Denominator + (Beta / Resistance)
            CASE (Direction_PositiveZ, Direction_NegativeZ)
                ! debug error, can't get here
            CASE (Direction_PositiveY)
                ! convection at the surface
                IF (WindSpeed .GT. 0.1d0) THEN
                    Resistance = 208.0d0 / (AirDensity * AirSpecificHeat * WindSpeed * ThisNormalArea)
                    Numerator = Numerator + (Beta / Resistance) * PipingSystemDomains(DomainNum)%Cur%CurAirTemp
                    Denominator = Denominator + (Beta / Resistance)
                ELSE
                    !Future development should include additional natural convection effects here
                END IF
            CASE (Direction_NegativeY)
                !debug error, can't get here!
        END SELECT


    END DO

    ! Initialize, this variable is used for both evapotranspiration and non-ET cases, [W]
    IncidentHeatGain = 0.0d0

    ! Latitude, converted to radians...positive for northern hemisphere, [radians]
    Latitude_Radians = Pi / 180.0d0 * Latitude_Degrees

    ! The day of year at this point in the simulation
    DayOfYear = INT( PipingSystemDomains(DomainNum)%Cur%CurSimTimeSeconds / SecsInDay )

    ! The number of seconds into the current day
    CurSecondsIntoToday = INT( MOD( PipingSystemDomains(DomainNum)%Cur%CurSimTimeSeconds, SecsInDay ) )

    ! The number of hours into today
    HourOfDay = INT( CurSecondsIntoToday / SecInHour )

    ! For convenience convert to Kelvin once
    CurAirTempK = PipingSystemDomains(DomainNum)%Cur%CurAirTemp + 273.15d0

    ! Calculate some angles
    dr   = 1.0d0 + 0.033d0 * COS(2.0d0 * Pi * DayOfYear / 365.0d0)
    Declination = 0.409d0 * SIN(2.0d0 * Pi / 365.0d0 * DayOfYear - 1.39d0)
    b_SC = 2.0d0 * Pi * (DayOfYear - 81.0d0)/364.0d0
    Sc   = 0.1645d0 * SIN(2.0d0 * b_SC) - 0.1255d0 * COS(b_SC) - 0.025d0 * SIN(b_SC)
    Hour_Angle = Pi / 12.0d0 * ( ( (HourOfDay - 0.5d0) + 0.06667d0 * (StMeridian_Degrees - Longitude_Degrees) + Sc) - 12.0d0)

    ! Calculate sunset something, and constrain to a minimum of 0.000001
    X_sunset = 1.0d0 - TAN(Latitude_Radians)**2 * TAN(Declination)**2
    X_sunset = MAX(X_sunset, 0.000001d0)

    ! Find sunset angle
    Sunset_angle = Pi / 2.0d0 - ATAN(-TAN(Latitude_Radians) * TAN(Declination) / X_sunset**0.5d0 )

    ! Find the current sun angle
    Altitude_Angle = ASIN( SIN(Latitude_Radians) * SIN(Declination) + COS(Latitude_Radians) * COS(Declination) * COS(Hour_Angle) )

    ! Find solar angles
    Solar_angle_1 = Hour_Angle - Pi / 24.0d0
    Solar_angle_2 = Hour_Angle + Pi / 24.0d0

    ! Constrain solar angles
    IF(Solar_angle_1 .LT. -Sunset_angle ) Solar_angle_1 = -Sunset_angle
    IF(Solar_angle_2 .LT. -Sunset_angle ) Solar_angle_2 = -Sunset_angle
    IF(Solar_angle_1 .GT.  Sunset_angle ) Solar_angle_1 =  Sunset_angle
    IF(Solar_angle_2 .GT.  Sunset_angle ) Solar_angle_2 =  Sunset_angle
    IF(Solar_angle_1 .GT.  Solar_angle_2) Solar_angle_1 =  Solar_angle_2

    ! Convert input solar radiation [w/m2] into units for ET model, [MJ/hr-min]
    IncidentSolar_MJhrmin = PipingSystemDomains(DomainNum)%Cur%CurIncidentSolar * Convert_Wm2_To_MJhrmin

    ! Calculate another Q term...
    QRAD_a = 12.0d0 * 60.0d0 / Pi * MeanSolarConstant * dr * &
        (   (Solar_angle_2 - Solar_angle_1) * SIN(Latitude_Radians) * SIN(Declination)         &
          + COS(Latitude_Radians) * COS(Declination) * (SIN(Solar_angle_2)-SIN(Solar_angle_1)) &
        )

    ! Calculate another Q term...
    QRAD_SO = ( A_s + B_s + 0.00002d0 * Elevation ) * QRAD_a

    ! Correct the Qrad term ... better way??
    IF (IncidentSolar_MJhrmin .LT. 0.01d0) THEN
        Ratio_SO = 0.0d0
    ELSE
        IF(QRAD_SO /= 0.d0)THEN
          Ratio_SO = IncidentSolar_MJhrmin / QRAD_SO
        ELSE
    ! I used logic below to choose value, divide by 0 = infinity, so value = 1, not sure if correct...
          Ratio_SO = 1.d0
        END IF

    END IF

    ! Constrain Ratio_SO
    Ratio_SO = MIN(Ratio_SO, 1.0d0)
    Ratio_SO = MAX(Ratio_SO, 0.3d0)

    ! Calculate another Q term, [MJ/hr-min]
    AbsorbedIncidentSolar_MJhrmin = ABSOR_CORRECTED * IncidentSolar_MJhrmin

    ! Calculate saturated vapor pressure, [kPa]
    VaporPressureSaturated_kPa = 0.6108d0 * EXP(17.27d0 * PipingSystemDomains(DomainNum)%Cur%CurAirTemp / &
                                                            (PipingSystemDomains(DomainNum)%Cur%CurAirTemp + 237.3d0))

    ! Calculate actual vapor pressure, [kPa]
    VaporPressureActual_kPa = VaporPressureSaturated_kPa * PipingSystemDomains(DomainNum)%Cur%CurRelativeHumidity / 100.0d0

    ! Calculate another Q term, [MJ/m2-hr]
    QRAD_NL = 2.042D-10 * CurAirTempK**4 * (0.34d0 - 0.14d0 * SQRT(VaporPressureActual_kPa)) * (1.35d0 * Ratio_SO - 0.35d0)

    ! Calculate another Q term, [MJ/hr]
    NetIncidentRadiation_MJhr = AbsorbedIncidentSolar_MJhrmin - QRAD_NL

    ! ?
    Cn = 37.0d0

    ! Check whether there was sun
    IF (NetIncidentRadiation_MJhr .LT. 0.0d0)THEN
        G_hr = 0.5d0 * NetIncidentRadiation_MJhr
        Cd = 0.96d0
    ELSE
        G_hr = 0.1d0 * NetIncidentRadiation_MJhr
        Cd = 0.24d0
    END IF

    ! Just For Check
    ! Lu Xing Sep 22 2009

    Slope_S = 2503.0d0 * EXP( 17.27d0 * PipingSystemDomains(DomainNum)%Cur%CurAirTemp /     &
                                (PipingSystemDomains(DomainNum)%Cur%CurAirTemp + 237.3d0)   &
                            )  / (PipingSystemDomains(DomainNum)%Cur%CurAirTemp+237.3d0)**2
    Pressure = 98.0d0
    PsychrometricConstant = 0.665d-3 * Pressure

    ! Evapotranspiration constant, [mm/hr]
    EvapotransFluidLoss_mmhr = (GroundCoverCoefficient * Slope_s * (NetIncidentRadiation_MJhr - G_hr) &
                                + PsychrometricConstant * (Cn / CurAirTempK) * PipingSystemDomains(DomainNum)%Cur%Curwindspeed &
                                  * (VaporPressureSaturated_kPa - VaporPressureActual_kPa) &
                               ) &
             / (Slope_s + PsychrometricConstant * (1 + Cd * PipingSystemDomains(DomainNum)%Cur%CurWindSpeed))

    ! Convert units, [m/hr]
    EvapotransFluidLoss_mhr = EvapotransFluidLoss_mmhr / 1000.0d0

    ! Calculate latent heat, [MJ/kg]
    ! Full formulation is cubic: L(T) = -0.0000614342 * T**3 + 0.00158927 * T**2 - 2.36418 * T + 2500.79[5]
    ! In: Cubic fit to Table 2.1,p.16, Textbook: R.R.Rogers & M.K. Yau, A Short Course in Cloud Physics, 3e,(1989), Pergamon press
    ! But a linear relation should suffice;
    ! note-for now using the previous time step temperature as an approximation to help ensure stability
    LatentHeatVaporization = 2.501d0 - 2.361d-3 * cell%MyBase%Temperature_PrevTimeStep

    ! Calculate evapotranspiration heat loss, [MJ/m2-hr]
    EvapotransHeatLoss_MJhrmin = RHO_water * EvapotransFluidLoss_mhr * LatentHeatVaporization

    ! Convert net incident solar units, [W/m2]
    NetIncidentRadiation_Wm2 = NetIncidentRadiation_MJhr * Convert_MJhrmin_To_Wm2

    ! Convert evapotranspiration units, [W/m2]
    EvapotransHeatLoss_Wm2 = EvapotransHeatLoss_MJhrmin * Convert_MJhrmin_To_Wm2

    ! Calculate overall net heat ?gain? into the cell, [W]
    IncidentHeatGain = (NetIncidentRadiation_Wm2 - EvapotransHeatLoss_Wm2) * ThisNormalArea

    ! Add any solar/evapotranspiration heat gain here
    Numerator = Numerator + Beta * IncidentHeatGain

    ! Calculate the return temperature and leave
    RetVal = Numerator / Denominator

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION EvaluateAdiabaticSurfaceTemperature(DomainNum, cell) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: NeighborTemp
    REAL(r64) :: Beta
    INTEGER :: DirectionCounter
    INTEGER :: CurDirection
    REAL(r64) :: AdiabaticMultiplier

    Numerator = 0.0d0
    Denominator = 0.0d0
    Resistance = 0.0d0
    Beta = cell%MyBase%Beta

    !'add effect from previous time step
    Numerator = Numerator + cell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    !now that we aren't infinitesimal, we need to determine the neighbor types based on cell location
    CALL EvaluateCellNeighborDirections(DomainNum, cell)

    DO DirectionCounter = LBOUND(NeighborFieldCells,1), UBOUND(NeighborFieldCells,1)
        CurDirection = NeighborFieldCells(DirectionCounter)
        AdiabaticMultiplier = 1.0d0

        ! There are only a few cases for adiabatic cells to be handled here
        ! These cases must be validated during mesh development as they aren't here
        ! For example, the +x case below will only be hit if the celltype is actually
        !   assigned to be Adiabatic...which only happens if the mesh dev engine
        !   recognizes that there is in fact a basement, and the boundary type is
        !   specified as adiabatic.
        SELECT CASE (CurDirection)
            CASE (Direction_PositiveZ) ! Case: front face looking in +z direction
                IF (cell%Z_index == 0) AdiabaticMultiplier = 2.0d0
            CASE (Direction_NegativeZ) ! Case: back face looking in -z direction
                IF (cell%Z_index == UBOUND(PipingSystemDomains(DomainNum)%Cells,3)) AdiabaticMultiplier = 2.0d0
            CASE (Direction_PositiveX) ! Case: Under basement floor, far left cell
                IF (cell%X_index == 0) AdiabaticMultiplier = 2.0d0
            CASE (Direction_NegativeY) ! Case: basement wall ground surface boundary
                !Not sure if this is ever hit (it should be a basement wall celltype)
                IF (cell%Y_index == UBOUND(PipingSystemDomains(DomainNum)%Cells,2)) AdiabaticMultiplier = 2.0d0
        END SELECT

        !Use the multiplier (either 1 or 2) to calculate the neighbor cell effects
        CALL EvaluateNeighborCharacteristics(DomainNum, cell, CurDirection, NeighborTemp, Resistance)
        Numerator = AdiabaticMultiplier * Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = AdiabaticMultiplier * Denominator + (Beta / Resistance)

    END DO

    RetVal = Numerator / Denominator

  RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION EvaluateBasementCellTemperature(DomainNum, cell) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Beta
    REAL(r64) :: Resistance
    REAL(r64) :: NeighborTemp
    REAL(r64) :: HeatFlux

    !Initialize
    Numerator = 0.0d0
    Denominator = 0.0d0
    Resistance = 0.0d0
    SELECT CASE (cell%CellType)
    CASE (CellType_BasementWall, CellType_BasementFloor)
        !This is actually only a half-cell since the basement wall slices right through the middle in one direction
        Beta = cell%MyBase%Beta / 2.0d0
    CASE (CellType_BasementCorner)
        !This is actually only a three-quarter-cell since the basement wall slices right through the middle in both directions
        Beta = cell%MyBase%Beta * 3.0d0/4.0d0
    END SELECT

    !add effect from previous time step
    Numerator = Numerator + cell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    SELECT CASE (cell%CellType)
    CASE (CellType_BasementWall)

        !we will only have heat flux from the basement wall and heat conduction to the +x cell

        !get the average basement wall heat flux and add it to the tally
        HeatFlux = GetBasementWallHeatFlux(DomainNum)
        Numerator = Numerator + Beta * HeatFlux * Height(cell)

        ! then get the +x conduction to continue the heat balance
        CALL EvaluateNeighborCharacteristics(DomainNum, cell, Direction_PositiveX, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)

    CASE (CellType_BasementFloor)

        !we will only have heat flux from the basement floor and heat conduction to the lower cell

        !get the average basement floor heat flux and add it to the tally
        HeatFlux = GetBasementFloorHeatFlux(DomainNum)
        Numerator = Numerator + Beta * HeatFlux * Width(cell)

        ! then get the -y conduction to continue the heat balance
        CALL EvaluateNeighborCharacteristics(DomainNum, cell, Direction_NegativeY, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)

    CASE (CellType_BasementCorner)

        !we will only have heat conduction to the +x and -y cells
        CALL EvaluateNeighborCharacteristics(DomainNum, cell, Direction_PositiveX, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)

        CALL EvaluateNeighborCharacteristics(DomainNum, cell, Direction_NegativeY, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)

    END SELECT

    RetVal = Numerator / Denominator

  RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION GetBasementWallHeatFlux(DomainNum) RESULT (RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataHeatBalSurface, ONLY: QdotConvOutRepPerArea

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RunningSummation
    INTEGER :: SurfaceCounter
    INTEGER :: SurfacePointer
    INTEGER :: NumSurfaces

    RunningSummation = 0.0d0
    NumSurfaces = SIZE(PipingSystemDomains(DomainNum)%BasementZone%WallSurfacePointers)

    DO SurfaceCounter = 1, NumSurfaces
        SurfacePointer = PipingSystemDomains(DomainNum)%BasementZone%WallSurfacePointers(SurfaceCounter)
        RunningSummation = RunningSummation + QdotConvOutRepPerArea(SurfacePointer)
    END DO

    RetVal = -RunningSummation / NumSurfaces ! heat flux is negative here

  RETURN

END FUNCTION GetBasementWallHeatFlux
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION GetBasementFloorHeatFlux(DomainNum) RESULT (RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataHeatBalSurface, ONLY: QdotConvOutRepPerArea

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RunningSummation
    INTEGER :: SurfaceCounter
    INTEGER :: SurfacePointer
    INTEGER :: NumSurfaces

    RunningSummation = 0.0d0
    NumSurfaces = SIZE(PipingSystemDomains(DomainNum)%BasementZone%FloorSurfacePointers)

    DO SurfaceCounter = 1, NumSurfaces
        SurfacePointer = PipingSystemDomains(DomainNum)%BasementZone%FloorSurfacePointers(SurfaceCounter)
        RunningSummation = RunningSummation + QdotConvOutRepPerArea(SurfacePointer)
    END DO

    RetVal = -RunningSummation / NumSurfaces   ! heat flux is negative here

  RETURN

END FUNCTION GetBasementFloorHeatFlux
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE UpdateBasementSurfaceTemperatures(DomainNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataSurfaces,  ONLY: OSCM

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: BigNumber = 10000.0d0

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AvgTemp
    INTEGER :: OSCMIndex

    !First the wall
    AvgTemp = GetAverageTempByType(DomainNum, CellType_BasementWall)
    OSCMIndex = PipingSystemDomains(DomainNum)%BasementZone%WallBoundaryOSCMIndex
    OSCM(OSCMIndex)%TConv   = AvgTemp
    OSCM(OSCMIndex)%HConv   = BigNumber
    OSCM(OSCMIndex)%TRad    = AvgTemp
    OSCM(OSCMIndex)%HRad    = 0.0d0

    !Then the floor
    AvgTemp = GetAverageTempByType(DomainNum, CellType_BasementFloor)
    OSCMIndex = PipingSystemDomains(DomainNum)%BasementZone%FloorBoundaryOSCMIndex
    OSCM(OSCMIndex)%TConv   = AvgTemp
    OSCM(OSCMIndex)%HConv   = BigNumber
    OSCM(OSCMIndex)%TRad    = AvgTemp
    OSCM(OSCMIndex)%HRad    = 0.0d0

  RETURN

END SUBROUTINE UpdateBasementSurfaceTemperatures
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION GetAverageTempByType(DomainNum, CellType) RESULT (RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CellType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RunningSummation
    INTEGER :: RunningCounter
    INTEGER :: X, Y, Z

    RunningSummation = 0.0d0
    RunningCounter = 0

    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells,3), UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells,2), UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells,1), UBOUND(PipingSystemDomains(DomainNum)%Cells,1)
                IF (PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType == CellType) THEN
                    RunningCounter = RunningCounter + 1
                    RunningSummation = RunningSummation + PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature
                END IF
            END DO
        END DO
    END DO

    IF (RunningCounter > 0) THEN
        RetVal = RunningSummation / REAL(RunningCounter,r64)
    ELSE
        !ERROR!!!
        RetVal = 0.0d0 !Objexx:Return Line added to assure return value is set: Proper error handling needed here!
    END IF

  RETURN

END FUNCTION GetAverageTempByType
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION EvaluateFarfieldBoundaryTemperature(DomainNum, cell) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Beta
    REAL(r64) :: Resistance
    INTEGER :: DirectionCounter
    INTEGER :: CurDirection
    REAL(r64) :: NeighborTemp

    Numerator = 0.0d0
    Denominator = 0.0d0
    Resistance = 0.0d0
    Beta = cell%MyBase%Beta

    !add effect from previous time step
    Numerator = Numerator + cell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    !now that we aren't infinitesimal, we need to determine the neighbor types based on cell location
    CALL EvaluateCellNeighborDirections(DomainNum, cell)

    !This may be incomplete, as there may need to be adiabatic conditions to be handled here as well

    !Do all neighbor cells
    DO DirectionCounter = LBOUND(NeighborFieldCells,1), UBOUND(NeighborFieldCells,1)
        CurDirection = NeighborFieldCells(DirectionCounter)
        CALL EvaluateNeighborCharacteristics(DomainNum, cell, CurDirection, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)
    END DO

    !Then all farfield boundaries
    DO DirectionCounter = LBOUND(NeighborBoundaryCells,1), UBOUND(NeighborBoundaryCells,1)
        CurDirection = NeighborBoundaryCells(DirectionCounter)
        CALL EvaluateFarfieldCharacteristics(DomainNum, cell, CurDirection, NeighborTemp, Resistance)
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)
    END DO

    RetVal = Numerator / Denominator

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE EvaluateFarfieldCharacteristics(DomainNum, cell, direction, neighbortemp, resistance)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell
    INTEGER, INTENT(IN) :: direction
    REAL(r64), INTENT(OUT) :: neighbortemp
    REAL(r64), INTENT(OUT) :: resistance

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: distance

    SELECT CASE (direction)
    CASE(Direction_NegativeX, Direction_PositiveX)
        distance = (width(cell) / 2.0d0)
    CASE(Direction_NegativeY, Direction_PositiveY)
        distance = (height(cell) / 2.0d0)
    CASE(Direction_NegativeZ, Direction_PositiveZ)
        distance = (depth(cell) / 2.0d0)
    END SELECT

    resistance = (distance / 2.0d0) / (cell%mybase%properties%conductivity * NormalArea(cell, direction))
    neighbortemp = GetFarfieldTemp(DomainNum, cell)

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
REAL(r64) FUNCTION GetFarfieldTemp(DomainNum, cell) RESULT(RetVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals, ONLY: SecsInDay

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: z
    REAL(r64) :: Term1
    REAL(r64) :: Term2
    REAL(r64) :: Diffusivity
    REAL(r64) :: SecondsInYear
    REAL(r64) :: KATemp
    REAL(r64) :: KAAmp
    REAL(r64) :: KAPhase
    REAL(r64) :: CurTime

    KATemp  = PipingSystemDomains(DomainNum)%Farfield%AverageGroundTemperature
    KAAmp   = PipingSystemDomains(DomainNum)%Farfield%AverageGroundTemperatureAmplitude
    KAPhase = PipingSystemDomains(DomainNum)%Farfield%PhaseShiftOfMinGroundTemp
    CurTime = PipingSystemDomains(DomainNum)%Cur%CurSimTimeSeconds

    SecondsInYear = SecsInDay * 365.0d0
    z = PipingSystemDomains(DomainNum)%Extents%Ymax - cell%Centroid%Y
    Diffusivity = BaseThermalPropertySet_Diffusivity(PipingSystemDomains(DomainNum)%GroundProperties)

    Term1 = -z * SQRT(PI / (SecondsInYear * Diffusivity))
    Term2 = (2 * PI / SecondsInYear) * (CurTime - KAPhase - (z / 2) * SQRT(SecondsInYear / (PI * Diffusivity)))
    RetVal = KATemp - KAAmp * EXP(Term1) * COS(Term2)

    RETURN

END FUNCTION
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PreparePipeCircuitSimulation(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), PARAMETER :: StagnantFluidConvCoeff = 200.0d0

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Density
    REAL(r64) :: Viscosity
    REAL(r64) :: Conductivity
    REAL(r64) :: Prandtl
    REAL(r64) :: Area_c
    REAL(r64) :: Velocity
    REAL(r64) :: ConvCoefficient
    REAL(r64) :: Reynolds
    REAL(r64) :: ExponentTerm
    REAL(r64) :: Nusselt
    REAL(r64) :: SpecificHeat
    INTEGER :: CellX
    INTEGER :: CellY
    INTEGER :: CellZ

    !Setup circuit flow conditions -- convection coefficient
    CellX = PipingSystemCircuits(CircuitNum)%CircuitInletCell%X
    CellY = PipingSystemCircuits(CircuitNum)%CircuitInletCell%Y
    CellZ = PipingSystemCircuits(CircuitNum)%CircuitInletCell%Z

    !Look up current fluid properties
    Density = PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%MyBase%Density
    Viscosity = PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%Viscosity
    Conductivity = PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%MyBase%Conductivity
    Prandtl = PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%Prandtl
    SpecificHeat = PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%MyBase%SpecificHeat

    !Flow calculations
    Area_c = (Pi/4.0d0) * PipingSystemCircuits(CircuitNum)%PipeSize%InnerDia**2
    Velocity = PipingSystemCircuits(CircuitNum)%CurCircuitFlowRate / (Density * Area_c)

    !Determine convection coefficient based on flow conditions
    IF (Velocity > 0) THEN
        Reynolds = Density * PipingSystemCircuits(CircuitNum)%PipeSize%InnerDia * Velocity / Viscosity
        IF ( PipingSystemDomains(DomainNum)%Cells(CellX, CellY, CellZ)%PipeCellData%Fluid%MyBase%Temperature > &
               PipingSystemDomains(DomainNum)%Cells(CellX, CellY, CellZ)%PipeCellData%Pipe%MyBase%Temperature ) THEN
            ExponentTerm = 0.3d0
        ELSE
            ExponentTerm = 0.4d0
        END IF
        Nusselt = 0.023d0 * Reynolds**(4.0d0/5.0d0) * Prandtl**ExponentTerm
        ConvCoefficient = Nusselt * Conductivity / PipingSystemCircuits(DomainNum)%PipeSize%InnerDia
    ELSE
        ConvCoefficient = StagnantFluidConvCoeff
    END IF

    !Assign the convection coefficient
    PipingSystemCircuits(CircuitNum)%CurCircuitConvectionCoefficient = ConvCoefficient

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PerformPipeCircuitSimulation(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataInterfaces, ONLY: ShowFatalError

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: CircuitCrossTemp
    REAL(r64) :: FlowRate
    REAL(r64) :: EnteringTemp
    INTEGER :: SegmentCtr
    INTEGER :: SegmentCellCtr
    INTEGER :: StartingSegment
    INTEGER :: EndingSegment
    INTEGER :: StartingZ
    INTEGER :: EndingZ
    INTEGER :: Increment
    INTEGER :: PipeX
    INTEGER :: PipeY
    INTEGER :: Zindex
    INTEGER :: SegmentIndex

    !retrieve initial conditions from the data structure
    !these have been set either by the init routine or by the heat pump routine
    FlowRate = PipingSystemCircuits(CircuitNum)%CurCircuitFlowRate
    EnteringTemp = PipingSystemCircuits(CircuitNum)%CurCircuitInletTemp

    !initialize
    SegmentCellCtr = 0
    StartingSegment = LBOUND(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces,1)
    EndingSegment = UBOUND(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces,1)

    !'loop across all segments (pipes) of the circuit
    DO SegmentCtr = StartingSegment, EndingSegment

        SegmentIndex = PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces(SegmentCtr)

        !'set simulation flow direction
        SELECT CASE (PipingSystemSegments(SegmentIndex)%FlowDirection)
        CASE (SegmentFlow_IncreasingZ)
            StartingZ = 0
            EndingZ = UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)
            Increment = 1
        CASE (SegmentFlow_DecreasingZ)
            StartingZ = UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)
            EndingZ = 0
            Increment = -1
        CASE DEFAULT
            CALL ShowFatalError('Debug error: invalid flow direction on piping system segment')
        END SELECT

        !'find the cell we are working on in order to retrieve cell and neighbor information
        PipeX = PipingSystemSegments(SegmentIndex)%PipeCellCoordinates%X
        PipeY = PipingSystemSegments(SegmentIndex)%PipeCellCoordinates%Y

        !'loop across all z-direction indeces
        DO Zindex = StartingZ, EndingZ, Increment

            !'overall cell segment counter
            SegmentCellCtr = SegmentCellCtr + 1

            IF (SegmentCellCtr == 1) THEN
                !'we have the very first cell, need to pass in circuiting entering temperature
                CALL PerformPipeCellSimulation(DomainNum, &
                                               CircuitNum, &
                                               PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex), &
                                               FlowRate, &
                                               EnteringTemp)
            ELSE
                !'we don't have the first cell so just normal simulation
                IF (Zindex == EndingZ) THEN
                    !simulate current cell using upstream as entering conditions
                    CALL PerformPipeCellSimulation(DomainNum, &
                          CircuitNum, &
                          PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex), &
                          FlowRate, &
                          PipingSystemDomains(DomainNum)%Cells(PipeX,PipeY,Zindex-Increment)%PipeCellData%Fluid%MyBase%Temperature)
                    !store this outlet condition to be passed to the next segment
                    CircuitCrossTemp = &
                            PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex)%PipeCellData%Fluid%MyBase%Temperature
                ELSE IF (Zindex == StartingZ) THEN
                    !we are starting another segment, use the previous cross temperature
                    CALL PerformPipeCellSimulation(DomainNum, &
                                                   CircuitNum, &
                                                   PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex), &
                                                   FlowRate, &
                                                   CircuitCrossTemp)
                ELSE
                    !we are in an interior node, so just get the upstream cell and use the main simulation
                    CALL PerformPipeCellSimulation(DomainNum, &
                          CircuitNum, &
                          PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex), &
                          FlowRate, &
                          PipingSystemDomains(DomainNum)%Cells(PipeX,PipeY,Zindex-Increment)%PipeCellData%Fluid%MyBase%Temperature)
                END IF
            END IF

            !Bookkeeping: segment fluid temperature updates
            IF (Zindex == StartingZ) THEN
                IF (SegmentCtr == StartingSegment) THEN
                    PipingSystemSegments(SegmentIndex)%InletTemperature = EnteringTemp
                ELSE
                    PipingSystemSegments(SegmentIndex)%InletTemperature = CircuitCrossTemp
                END IF
            ELSEIF (Zindex == EndingZ) THEN
                PipingSystemSegments(SegmentIndex)%OutletTemperature = &
                       PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex)%PipeCellData%Fluid%MyBase%Temperature
                PipingSystemSegments(SegmentIndex)%FluidHeatLoss = &
                       FlowRate * PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%MyBase%SpecificHeat * &
                       (PipingSystemSegments(SegmentIndex)%InletTemperature - PipingSystemSegments(SegmentIndex)%OutletTemperature)
            END IF

            !Bookkeeping: circuit fluid temperature updates
            IF ( (SegmentCtr == StartingSegment) .AND. (Zindex == StartingZ) ) THEN
                PipingSystemCircuits(CircuitNum)%InletTemperature = EnteringTemp
            ELSEIF ( (SegmentCtr == EndingSegment) .AND. (Zindex == EndingZ) ) THEN
                PipingSystemCircuits(CircuitNum)%OutletTemperature = &
                            PipingSystemDomains(DomainNum)%Cells(PipeX, PipeY, Zindex)%PipeCellData%Fluid%MyBase%Temperature
                PipingSystemCircuits(CircuitNum)%FluidHeatLoss = &
                       FlowRate * PipingSystemCircuits(CircuitNum)%CurFluidPropertySet%MyBase%SpecificHeat * &
                       (PipingSystemCircuits(CircuitNum)%InletTemperature - PipingSystemCircuits(CircuitNum)%OutletTemperature)
            END IF

        END DO

    END DO

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE PerformPipeCellSimulation(DomainNum, CircuitNum, ThisCell, FlowRate, EnteringTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell
    REAL(r64), INTENT(IN) :: FlowRate
    REAL(r64), INTENT(IN) :: EnteringTemp

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Iter
    REAL(r64) :: MaxDeviationAmount

    DO Iter = 1, PipingSystemCircuits(CircuitNum)%MaxIterationsPerTS

        !'shift all the pipe related temperatures for the next internal pipe iteration
        CALL ShiftPipeTemperaturesForNewIteration(ThisCell)

        !'simulate the funny interface soil cell between the radial and cartesian systems
        CALL SimulateRadialToCartesianInterface(DomainNum, ThisCell)

        !'simulate the outermost radial slice
        CALL SimulateOuterMostRadialSoilSlice(DomainNum, CircuitNum, ThisCell)

        !'we only need to simulate these if they actually exist!
        IF (SIZE(ThisCell%PipeCellData%Soil) > 1) THEN

            !'simulate all interior radial slices
            CALL SimulateAllInteriorRadialSoilSlices(ThisCell)

            !'simulate the innermost radial soil slice
            CALL SimulateInnerMostRadialSoilSlice(DomainNum, CircuitNum, ThisCell)

        END IF

        IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
            CALL SimulateRadialInsulationCell(ThisCell)
        END IF

        !'simulate the pipe cell
        CALL SimulateRadialPipeCell(DomainNum, CircuitNum, ThisCell, &
                                    PipingSystemCircuits(CircuitNum)%CurCircuitConvectionCoefficient)

        !'simulate the water cell
        CALL SimulateFluidCell(ThisCell, FlowRate, PipingSystemCircuits(CircuitNum)%CurCircuitConvectionCoefficient, EnteringTemp)

        !'check convergence
        IF (IsConverged_PipeCurrentToPrevIteration(DomainNum, ThisCell, MaxDeviationAmount)) EXIT

    END DO

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateRadialToCartesianInterface(DomainNum, ThisCell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell

          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, DIMENSION(4), PARAMETER :: Directions = (/Direction_NegativeX, Direction_NegativeY, &
                                                       Direction_PositiveX, Direction_PositiveY/)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: Beta
    INTEGER :: DirCtr
    INTEGER :: Dir
    REAL(r64) :: NeighborTemp
    REAL(r64) :: OutermostRadialCellOuterRadius
    REAL(r64) :: OutermostRadialCellRadialCentroid
    REAL(r64) :: OutermostRadialCellTemperature

    Numerator = 0.0d0
    Denominator = 0.0d0

    !'retrieve beta
    Beta = ThisCell%MyBase%Beta

    !'add effects from this cell history
    Numerator = Numerator + ThisCell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    !'add effects from outermost radial cell
    OutermostRadialCellOuterRadius = ThisCell%PipeCellData%Soil(UBOUND(ThisCell%PipeCellData%Soil,1))%OuterRadius
    OutermostRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(UBOUND(ThisCell%PipeCellData%Soil,1))%RadialCentroid
    OutermostRadialCellTemperature = ThisCell%PipeCellData%Soil(UBOUND(ThisCell%PipeCellData%Soil,1))%MyBase%Temperature
    Resistance = LOG(OutermostRadialCellOuterRadius / OutermostRadialCellRadialCentroid) / &
                 (2.0d0 * Pi * Depth(ThisCell) * ThisCell%MyBase%Properties%Conductivity)
    Numerator = Numerator + (Beta / Resistance) * OutermostRadialCellTemperature
    Denominator = Denominator + (Beta / Resistance)

    !'add effects from neighboring Cartesian cells
    DO DirCtr = LBOUND(Directions,1), UBOUND(Directions,1)
        Dir = Directions(DirCtr)

        !'get info about cartesian neighbors
        CALL EvaluateNeighborCharacteristics(DomainNum, ThisCell, Dir, NeighborTemp, Resistance)

        !'add to the numerator and denominator expressions
        Numerator = Numerator + (Beta / Resistance) * NeighborTemp
        Denominator = Denominator + (Beta / Resistance)

    END DO

    !'calculate the new temperature
    ThisCell%MyBase%Temperature = Numerator / Denominator

    RETURN

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateOuterMostRadialSoilSlice(DomainNum, CircuitNum, ThisCell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: Beta
    INTEGER :: MaxRadialIndex
    REAL(r64) :: ThisRadialCellOuterRadius
    REAL(r64) :: ThisRadialCellRadialCentroid
    REAL(r64) :: ThisRadialCellConductivity
    REAL(r64) :: ThisRadialCellInnerRadius
    REAL(r64) :: ThisRadialCellTemperature_PrevTimeStep
    REAL(r64) :: ThisRadialCellTemperature
    REAL(r64) :: NextOuterRadialCellOuterRadius
    REAL(r64) :: NextOuterRadialCellRadialCentroid
    REAL(r64) :: NextOuterRadialCellConductivity
    REAL(r64) :: NextOuterRadialCellInnerRadius
    REAL(r64) :: NextOuterRadialCellTemperature

    Numerator = 0.0d0
    Denominator = 0.0d0
    Resistance = 0.0d0

    !'convenience variables
    MaxRadialIndex = UBOUND(ThisCell%PipeCellData%Soil,1)
    ThisRadialCellOuterRadius = ThisCell%PipeCellData%Soil(MaxRadialIndex)%OuterRadius
    ThisRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(MaxRadialIndex)%RadialCentroid
    ThisRadialCellConductivity = ThisCell%PipeCellData%Soil(MaxRadialIndex)%MyBase%Properties%Conductivity
    ThisRadialCellInnerRadius = ThisCell%PipeCellData%Soil(MaxRadialIndex)%InnerRadius
    ThisRadialCellTemperature_PrevTimeStep = ThisCell%PipeCellData%Soil(MaxRadialIndex)%MyBase%Temperature_PrevTimeStep
    ThisRadialCellTemperature = ThisCell%PipeCellData%Soil(MaxRadialIndex)%MyBase%Temperature
    IF (SIZE(ThisCell%PipeCellData%Soil)==1) THEN
        IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
            NextOuterRadialCellOuterRadius = ThisCell%PipeCellData%Insulation%OuterRadius
            NextOuterRadialCellRadialCentroid = ThisCell%PipeCellData%Insulation%RadialCentroid
            NextOuterRadialCellConductivity = ThisCell%PipeCellData%Insulation%MyBase%Properties%Conductivity
            NextOuterRadialCellInnerRadius = ThisCell%PipeCellData%Insulation%InnerRadius
            NextOuterRadialCellTemperature = ThisCell%PipeCellData%Insulation%MyBase%Temperature
        ELSE
            NextOuterRadialCellOuterRadius = ThisCell%PipeCellData%Pipe%OuterRadius
            NextOuterRadialCellRadialCentroid = ThisCell%PipeCellData%Pipe%RadialCentroid
            NextOuterRadialCellConductivity = ThisCell%PipeCellData%Pipe%MyBase%Properties%Conductivity
            NextOuterRadialCellInnerRadius = ThisCell%PipeCellData%Pipe%InnerRadius
            NextOuterRadialCellTemperature = ThisCell%PipeCellData%Pipe%MyBase%Temperature
        END IF
    ELSE
        NextOuterRadialCellOuterRadius = ThisCell%PipeCellData%Soil(MaxRadialIndex - 1)%OuterRadius
        NextOuterRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(MaxRadialIndex - 1)%RadialCentroid
        NextOuterRadialCellConductivity = ThisCell%PipeCellData%Soil(MaxRadialIndex - 1)%MyBase%Properties%Conductivity
        NextOuterRadialCellInnerRadius = ThisCell%PipeCellData%Soil(MaxRadialIndex - 1)%InnerRadius
        NextOuterRadialCellTemperature = ThisCell%PipeCellData%Soil(MaxRadialIndex - 1)%MyBase%Temperature
    END IF

    !'any broadly defined variables
    Beta = ThisCell%PipeCellData%Soil(MaxRadialIndex)%MyBase%Beta

    !'add effects from this cell history
    Numerator = Numerator + ThisRadialCellTemperature_PrevTimeStep
    Denominator = Denominator + 1

    !'add effects from interface cell
    Resistance = LOG(ThisRadialCellOuterRadius / ThisRadialCellRadialCentroid) / &
                    (2 * Pi * Depth(ThisCell) * ThisRadialCellConductivity)
    Numerator = Numerator + (Beta / Resistance) * ThisCell%MyBase%Temperature
    Denominator = Denominator + (Beta / Resistance)

    !'add effects from inner radial cell
    Resistance = (LOG(ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius) / &
                    (2 * Pi * Depth(ThisCell) * ThisRadialCellConductivity)) &
                + (LOG(NextOuterRadialCellOuterRadius/NextOuterRadialCellRadialCentroid) / &
                    (2 * Pi * Depth(ThisCell) * NextOuterRadialCellConductivity))
    Numerator = Numerator + (Beta / Resistance) * NextOuterRadialCellTemperature
    Denominator = Denominator + (Beta / Resistance)

    !'calculate the new temperature
    ThisCell%PipeCellData%Soil(MaxRadialIndex)%MyBase%Temperature = Numerator / Denominator

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateAllInteriorRadialSoilSlices(ThisCell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: Beta
    INTEGER :: rCtr

    REAL(r64) :: ThisRadialCellOuterRadius
    REAL(r64) :: ThisRadialCellRadialCentroid
    REAL(r64) :: ThisRadialCellConductivity
    REAL(r64) :: ThisRadialCellInnerRadius
    REAL(r64) :: ThisRadialCellTemperature_PrevTimeStep
    REAL(r64) :: ThisRadialCellTemperature

    REAL(r64) :: InnerRadialCellOuterRadius
    REAL(r64) :: InnerRadialCellRadialCentroid
    REAL(r64) :: InnerRadialCellConductivity
    REAL(r64) :: InnerRadialCellInnerRadius
    REAL(r64) :: InnerRadialCellTemperature

    REAL(r64) :: OuterRadialCellOuterRadius
    REAL(r64) :: OuterRadialCellRadialCentroid
    REAL(r64) :: OuterRadialCellConductivity
    REAL(r64) :: OuterRadialCellInnerRadius
    REAL(r64) :: OuterRadialCellTemperature

    Numerator = 0.0d0
    Denominator = 0.0d0

    DO rCtr = UBOUND(ThisCell%PipeCellData%Soil,1)-1, 1, -1

        Numerator = 0.0d0
        Denominator = 0.0d0
        Resistance = 0.0d0

        !'convenience variables
        ThisRadialCellOuterRadius = ThisCell%PipeCellData%Soil(rCtr)%OuterRadius
        ThisRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(rCtr)%RadialCentroid
        ThisRadialCellConductivity = ThisCell%PipeCellData%Soil(rCtr)%MyBase%Properties%Conductivity
        ThisRadialCellInnerRadius = ThisCell%PipeCellData%Soil(rCtr)%InnerRadius
        ThisRadialCellTemperature_PrevTimeStep = ThisCell%PipeCellData%Soil(rCtr)%MyBase%Temperature_PrevTimeStep
        ThisRadialCellTemperature = ThisCell%PipeCellData%Soil(rCtr)%MyBase%Temperature

        InnerRadialCellOuterRadius = ThisCell%PipeCellData%Soil(rCtr-1)%OuterRadius
        InnerRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(rCtr-1)%RadialCentroid
        InnerRadialCellConductivity = ThisCell%PipeCellData%Soil(rCtr-1)%MyBase%Properties%Conductivity
        InnerRadialCellInnerRadius = ThisCell%PipeCellData%Soil(rCtr-1)%InnerRadius
        InnerRadialCellTemperature = ThisCell%PipeCellData%Soil(rCtr-1)%MyBase%Temperature

        OuterRadialCellOuterRadius = ThisCell%PipeCellData%Soil(rCtr+1)%OuterRadius
        OuterRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(rCtr+1)%RadialCentroid
        OuterRadialCellConductivity = ThisCell%PipeCellData%Soil(rCtr+1)%MyBase%Properties%Conductivity
        OuterRadialCellInnerRadius = ThisCell%PipeCellData%Soil(rCtr+1)%InnerRadius
        OuterRadialCellTemperature = ThisCell%PipeCellData%Soil(rCtr+1)%MyBase%Temperature


        !'any broadly defined variables
        Beta = ThisCell%PipeCellData%Soil(rCtr)%MyBase%Beta

        !'add effects from this cell history
        Numerator = Numerator + ThisRadialCellTemperature_PrevTimeStep
        Denominator = Denominator + 1

        !'add effects from outer cell
        Resistance = (LOG(OuterRadialCellRadialCentroid / OuterRadialCellInnerRadius) / &
                        (2 * Pi * Depth(ThisCell) * OuterRadialCellConductivity)) &
                + (LOG(ThisRadialCellOuterRadius/ThisRadialCellRadialCentroid) / &
                        (2 * Pi * Depth(ThisCell) * ThisRadialCellConductivity))
        Numerator = Numerator + (Beta / Resistance) * OuterRadialCellTemperature
        Denominator = Denominator + (Beta / Resistance)

        !'add effects from inner cell
        Resistance = (LOG(ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius) / &
                        (2 * Pi * Depth(ThisCell) * ThisRadialCellConductivity)) &
                + (LOG(InnerRadialCellOuterRadius/InnerRadialCellRadialCentroid) / &
                        (2 * Pi * Depth(ThisCell) * InnerRadialCellConductivity))
        Numerator = Numerator + (Beta / Resistance) * InnerRadialCellTemperature
        Denominator = Denominator + (Beta / Resistance)

        !'calculate the new temperature
        ThisCell%PipeCellData%Soil(rCtr)%MyBase%Temperature = Numerator / Denominator

    END DO

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateInnerMostRadialSoilSlice(DomainNum, CircuitNum, ThisCell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: Beta

    REAL(r64) :: ThisRadialCellOuterRadius
    REAL(r64) :: ThisRadialCellRadialCentroid
    REAL(r64) :: ThisRadialCellConductivity
    REAL(r64) :: ThisRadialCellInnerRadius
    REAL(r64) :: ThisRadialCellTemperature_PrevTimeStep
    REAL(r64) :: ThisRadialCellTemperature

    REAL(r64) :: InnerNeighborRadialCellOuterRadius
    REAL(r64) :: InnerNeighborRadialCellRadialCentroid
    REAL(r64) :: InnerNeighborRadialCellConductivity
    REAL(r64) :: InnerNeighborRadialCellInnerRadius
    REAL(r64) :: InnerNeighborRadialCellTemperature

    REAL(r64) :: OuterNeighborRadialCellOuterRadius
    REAL(r64) :: OuterNeighborRadialCellRadialCentroid
    REAL(r64) :: OuterNeighborRadialCellConductivity
    REAL(r64) :: OuterNeighborRadialCellInnerRadius
    REAL(r64) :: OuterNeighborRadialCellTemperature

    Numerator = 0.0d0
    Denominator = 0.0d0

    !'convenience variables
    IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
        InnerNeighborRadialCellOuterRadius = ThisCell%PipeCellData%Insulation%OuterRadius
        InnerNeighborRadialCellRadialCentroid = ThisCell%PipeCellData%Insulation%RadialCentroid
        InnerNeighborRadialCellConductivity = ThisCell%PipeCellData%Insulation%MyBase%Properties%Conductivity
        InnerNeighborRadialCellInnerRadius = ThisCell%PipeCellData%Insulation%InnerRadius
        InnerNeighborRadialCellTemperature = ThisCell%PipeCellData%Insulation%MyBase%Temperature
    ELSE
        InnerNeighborRadialCellOuterRadius = ThisCell%PipeCellData%Pipe%OuterRadius
        InnerNeighborRadialCellRadialCentroid = ThisCell%PipeCellData%Pipe%RadialCentroid
        InnerNeighborRadialCellConductivity = ThisCell%PipeCellData%Pipe%MyBase%Properties%Conductivity
        InnerNeighborRadialCellInnerRadius = ThisCell%PipeCellData%Pipe%InnerRadius
        InnerNeighborRadialCellTemperature = ThisCell%PipeCellData%Pipe%MyBase%Temperature
    END IF

    ThisRadialCellOuterRadius = ThisCell%PipeCellData%Soil(0)%OuterRadius
    ThisRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(0)%RadialCentroid
    ThisRadialCellConductivity = ThisCell%PipeCellData%Soil(0)%MyBase%Properties%Conductivity
    ThisRadialCellInnerRadius = ThisCell%PipeCellData%Soil(0)%InnerRadius
    ThisRadialCellTemperature_PrevTimeStep = ThisCell%PipeCellData%Soil(0)%MyBase%Temperature_PrevTimeStep
    ThisRadialCellTemperature = ThisCell%PipeCellData%Soil(0)%MyBase%Temperature

    OuterNeighborRadialCellOuterRadius = ThisCell%PipeCellData%Soil(1)%OuterRadius
    OuterNeighborRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(1)%RadialCentroid
    OuterNeighborRadialCellConductivity = ThisCell%PipeCellData%Soil(1)%MyBase%Properties%Conductivity
    OuterNeighborRadialCellInnerRadius = ThisCell%PipeCellData%Soil(1)%InnerRadius
    OuterNeighborRadialCellTemperature = ThisCell%PipeCellData%Soil(1)%MyBase%Temperature

    !'any broadly defined variables
    Beta = ThisCell%PipeCellData%Soil(0)%MyBase%Beta

    !'add effects from this cell history
    Numerator = Numerator + ThisRadialCellTemperature_PrevTimeStep
    Denominator = Denominator + 1

    !'add effects from outer radial cell
    Resistance = (LOG(OuterNeighborRadialCellRadialCentroid / OuterNeighborRadialCellInnerRadius) / &
                    (2 * PI * Depth(ThisCell) * OuterNeighborRadialCellConductivity)) &
                +(LOG(ThisRadialCellOuterRadius / ThisRadialCellRadialCentroid) / &
                    (2 * PI * Depth(ThisCell) * ThisRadialCellConductivity))
    Numerator = Numerator + (Beta / Resistance) * OuterNeighborRadialCellTemperature
    Denominator = Denominator + (Beta / Resistance)

    !'add effects from pipe cell
    Resistance = (LOG(ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius) / &
                    (2 * PI * Depth(ThisCell) * ThisRadialCellConductivity)) &
                + (LOG(InnerNeighborRadialCellOuterRadius / InnerNeighborRadialCellRadialCentroid) / &
                    (2 * PI * Depth(ThisCell) * InnerNeighborRadialCellConductivity))
    Numerator = Numerator + (Beta / Resistance) * InnerNeighborRadialCellTemperature
    Denominator = Denominator + (Beta / Resistance)

    !'calculate the new temperature
    ThisCell%PipeCellData%Soil(0)%MyBase%Temperature = Numerator / Denominator

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateRadialInsulationCell(ThisCell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: Beta
    TYPE(RadialCellInformation) :: PipeCell
    TYPE(RadialCellInformation) :: ThisInsulationCell
    TYPE(RadialCellInformation) :: NextInnerRadialCell

    Numerator = 0.0d0
    Denominator = 0.0d0

    !'convenience variables
    PipeCell = ThisCell%PipeCellData%Pipe
    ThisInsulationCell = ThisCell%PipeCellData%Insulation
    NextInnerRadialCell = ThisCell%PipeCellData%Soil(0)

    !'any broadly defined variables
    Beta = ThisInsulationCell%MyBase%Beta

    !'add effects from this cell history
    Numerator = Numerator + ThisInsulationCell%MyBase%Temperature_PrevTimeStep
    Denominator = Denominator + 1

    !'add effects from outer radial cell
    Resistance = (LOG(NextInnerRadialCell%RadialCentroid / NextInnerRadialCell%InnerRadius) / &
                    (2 * PI * Depth(ThisCell) * NextInnerRadialCell%MyBase%Properties%Conductivity)) &
                + (LOG(ThisInsulationCell%OuterRadius / ThisInsulationCell%RadialCentroid) / &
                    (2 * PI * Depth(ThisCell) * ThisInsulationCell%MyBase%Properties%Conductivity))
    Numerator = Numerator + (Beta / Resistance) * NextInnerRadialCell%MyBase%Temperature
    Denominator = Denominator + (Beta / Resistance)

    !'add effects from pipe cell
    Resistance = (LOG(ThisInsulationCell%RadialCentroid / ThisInsulationCell%InnerRadius) / &
                    (2 * PI * Depth(ThisCell) * ThisInsulationCell%MyBase%Properties%Conductivity)) &
                + (LOG(PipeCell%OuterRadius / PipeCell%RadialCentroid) / &
                    (2 * PI * Depth(ThisCell) * PipeCell%MyBase%Properties%Conductivity))
    Numerator = Numerator + (Beta / Resistance) * PipeCell%MyBase%Temperature
    Denominator = Denominator + (Beta / Resistance)

    !'calculate the new temperature
    ThisCell%PipeCellData%Insulation%MyBase%Temperature = Numerator / Denominator

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateRadialPipeCell(DomainNum, CircuitNum, ThisCell, ConvectionCoefficient)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell
    REAL(r64), INTENT(IN) :: ConvectionCoefficient

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: Resistance
    REAL(r64) :: Beta
    REAL(r64) :: PipeConductionResistance
    REAL(r64) :: ConvectiveResistance

    REAL(r64) :: ThisPipeCellOuterRadius
    REAL(r64) :: ThisPipeCellRadialCentroid
    REAL(r64) :: ThisPipeCellConductivity
    REAL(r64) :: ThisPipeCellInnerRadius
    REAL(r64) :: ThisPipeCellTemperature_PrevTimeStep
    REAL(r64) :: ThisPipeCellTemperature

    REAL(r64) :: FluidCellOuterRadius
    REAL(r64) :: FluidCellRadialCentroid
    REAL(r64) :: FluidCellConductivity
    REAL(r64) :: FluidCellInnerRadius
    REAL(r64) :: FluidCellTemperature

    REAL(r64) :: OuterNeighborRadialCellOuterRadius
    REAL(r64) :: OuterNeighborRadialCellRadialCentroid
    REAL(r64) :: OuterNeighborRadialCellConductivity
    REAL(r64) :: OuterNeighborRadialCellInnerRadius
    REAL(r64) :: OuterNeighborRadialCellTemperature

    Numerator = 0.0d0
    Denominator = 0.0d0
    Resistance = 0.0d0

    !'convenience variables
    IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
        OuterNeighborRadialCellOuterRadius = ThisCell%PipeCellData%Insulation%OuterRadius
        OuterNeighborRadialCellRadialCentroid = ThisCell%PipeCellData%Insulation%RadialCentroid
        OuterNeighborRadialCellConductivity = ThisCell%PipeCellData%Insulation%MyBase%Properties%Conductivity
        OuterNeighborRadialCellInnerRadius = ThisCell%PipeCellData%Insulation%InnerRadius
        OuterNeighborRadialCellTemperature = ThisCell%PipeCellData%Insulation%MyBase%Temperature
    ELSE
        OuterNeighborRadialCellOuterRadius = ThisCell%PipeCellData%Soil(0)%OuterRadius
        OuterNeighborRadialCellRadialCentroid = ThisCell%PipeCellData%Soil(0)%RadialCentroid
        OuterNeighborRadialCellConductivity = ThisCell%PipeCellData%Soil(0)%MyBase%Properties%Conductivity
        OuterNeighborRadialCellInnerRadius = ThisCell%PipeCellData%Soil(0)%InnerRadius
        OuterNeighborRadialCellTemperature = ThisCell%PipeCellData%Soil(0)%MyBase%Temperature
    END IF

    ThisPipeCellOuterRadius = ThisCell%PipeCellData%Pipe%OuterRadius
    ThisPipeCellRadialCentroid = ThisCell%PipeCellData%Pipe%RadialCentroid
    ThisPipeCellConductivity = ThisCell%PipeCellData%Pipe%MyBase%Properties%Conductivity
    ThisPipeCellInnerRadius = ThisCell%PipeCellData%Pipe%InnerRadius
    ThisPipeCellTemperature_PrevTimeStep = ThisCell%PipeCellData%Pipe%MyBase%Temperature_PrevTimeStep
    ThisPipeCellTemperature = ThisCell%PipeCellData%Pipe%MyBase%Temperature

    FluidCellTemperature = ThisCell%PipeCellData%Fluid%MyBase%Temperature

    !'any broadly defined variables
    Beta = ThisCell%PipeCellData%Pipe%MyBase%Beta

    !'add effects from this cell history
    Numerator = Numerator + ThisPipeCellTemperature_PrevTimeStep
    Denominator = Denominator + 1

    !'add effects from outer radial cell
    Resistance = (LOG(OuterNeighborRadialCellRadialCentroid / OuterNeighborRadialCellInnerRadius) / &
                 (2 * PI * Depth(ThisCell) * OuterNeighborRadialCellConductivity)) &
               + (LOG(ThisPipeCellOuterRadius / ThisPipeCellRadialCentroid) / &
                 (2 * PI * Depth(ThisCell) * ThisPipeCellConductivity))
    Numerator = Numerator + (Beta / Resistance) * OuterNeighborRadialCellTemperature
    Denominator = Denominator + (Beta / Resistance)

    !'add effects from water cell
    PipeConductionResistance = LOG(ThisPipeCellRadialCentroid / ThisPipeCellInnerRadius) / &
                               (2 * PI * Depth(ThisCell) * ThisPipeCellConductivity)
    ConvectiveResistance = 1.0d0 / (ConvectionCoefficient * 2 * PI * ThisPipeCellInnerRadius * Depth(ThisCell))
    Resistance = PipeConductionResistance + ConvectiveResistance
    Numerator = Numerator + (Beta / Resistance) * FluidCellTemperature
    Denominator = Denominator + (Beta / Resistance)

    !'calculate new temperature
    ThisCell%PipeCellData%Pipe%MyBase%Temperature = Numerator / Denominator

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SimulateFluidCell(ThisCell, FlowRate, ConvectionCoefficient, EnteringFluidTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE(CartesianCell), INTENT(IN OUT) :: ThisCell
    REAL(r64), INTENT(IN) :: FlowRate
    REAL(r64), INTENT(IN) :: ConvectionCoefficient
    REAL(r64), INTENT(IN) :: EnteringFluidTemp

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Beta
    REAL(r64) :: Numerator
    REAL(r64) :: Denominator
    REAL(r64) :: TotalPipeResistance
    REAL(r64) :: PipeConductionResistance
    REAL(r64) :: ConvectiveResistance
    REAL(r64) :: UpstreamResistance
    REAL(r64) :: EnteringFluidConductance

    REAL(r64) :: FluidCellTemperature_PrevTimeStep
    REAL(r64) :: FluidCellSpecificHeat
    REAL(r64) :: PipeCellOuterRadius
    REAL(r64) :: PipeCellRadialCentroid
    REAL(r64) :: PipeCellConductivity
    REAL(r64) :: PipeCellInnerRadius
    REAL(r64) :: PipeCellTemperature

    Numerator = 0.0d0
    Denominator = 0.0d0

    !'convenience variables
    FluidCellTemperature_PrevTimeStep = ThisCell%PipeCellData%Fluid%MyBase%Temperature_PrevTimeStep
    FluidCellSpecificHeat = ThisCell%PipeCellData%Fluid%Properties%MyBase%SpecificHeat

    PipeCellOuterRadius = ThisCell%PipeCellData%Pipe%OuterRadius
    PipeCellRadialCentroid = ThisCell%PipeCellData%Pipe%RadialCentroid
    PipeCellConductivity = ThisCell%PipeCellData%Pipe%MyBase%Properties%Conductivity
    PipeCellInnerRadius = ThisCell%PipeCellData%Pipe%InnerRadius
    PipeCellTemperature = ThisCell%PipeCellData%Pipe%MyBase%Temperature

    Beta = ThisCell%PipeCellData%Fluid%MyBase%Beta

    !'add effects from this cell history
    Numerator = Numerator + FluidCellTemperature_PrevTimeStep
    Denominator = Denominator + 1

    !'add effects from outer pipe cell
    PipeConductionResistance = LOG(PipeCellRadialCentroid / PipeCellInnerRadius) / &
                    (2 * PI * Depth(ThisCell) * PipeCellConductivity)
    ConvectiveResistance = 1.0d0 / (ConvectionCoefficient * 2 * PI * PipeCellInnerRadius * Depth(ThisCell))
    TotalPipeResistance = PipeConductionResistance + ConvectiveResistance
    Numerator = Numerator + (Beta / TotalPipeResistance) * PipeCellTemperature
    Denominator = Denominator + (Beta / TotalPipeResistance)

    !'add effects from upstream flow
    EnteringFluidConductance = 0.0d0
    IF (FlowRate > 0.0d0) THEN
        UpstreamResistance = 1 / (FlowRate * FluidCellSpecificHeat)
        !EnteringFluidConductance = ( (1/UpstreamResistance) - (0.5*TotalPipeResistance) )
        Numerator = Numerator + (Beta / UpstreamResistance) * EnteringFluidTemp
        Denominator = Denominator + (Beta / UpstreamResistance)
    END IF

    !'calculate new temperature
    ThisCell%PipeCellData%Fluid%MyBase%Temperature = Numerator / Denominator

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE DoOneTimeInitializations(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: X, Y, Z, rCtr
    INTEGER :: NX, NY, NZ
    INTEGER :: SegIndex
    INTEGER :: StartingZ
    INTEGER :: EndingZ
    INTEGER :: Increment
    INTEGER :: ZIndex
    INTEGER :: PipeX, PipeY
    REAL(r64) :: NeighborTemp
    REAL(r64) :: Resistance
    INTEGER :: DirectionCtr
    INTEGER :: CurDirection
    REAL(r64) :: Dummy = 0.0d0
    INTEGER :: TotalSegments
    INTEGER :: SegCtr2
    REAL(r64) :: ThisCellTemp

    !'initialize cell properties
    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells, 3), UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells, 2), UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells, 1), UBOUND(PipingSystemDomains(DomainNum)%Cells, 1)

                SELECT CASE (PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType)
                CASE (CellType_Pipe)
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties = &
                                                PipingSystemDomains(DomainNum)%GroundProperties
                    DO rctr = 0, UBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil, 1)
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Properties = &
                                                PipingSystemDomains(DomainNum)%GroundProperties
                    END DO
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Properties = &
                                                PipingSystemCircuits(CircuitNum)%PipeProperties
                    IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Properties = &
                                                PipingSystemCircuits(CircuitNum)%InsulationProperties
                    END IF
                CASE (CellType_GeneralField, CellType_GroundSurface, CellType_AdiabaticWall, CellType_FarfieldBoundary, &
                      CellType_BasementWall, CellType_BasementFloor, CellType_BasementCorner)
                    !basement cells are partially ground, give them some props
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties = &
                                                PipingSystemDomains(DomainNum)%GroundProperties
                END SELECT

            END DO
        END DO
    END DO

    !'calculate one-time resistance terms for cartesian cells
    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells, 3), UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells, 2), UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells, 1), UBOUND(PipingSystemDomains(DomainNum)%Cells, 1)
                CALL EvaluateCellNeighborDirections(DomainNum, PipingSystemDomains(DomainNum)%Cells(X, Y, Z))
                DO DirectionCtr = 0, UBOUND(NeighborFieldCells,1)
                    CurDirection = NeighborFieldCells(DirectionCtr)
                    CALL EvaluateNeighborCharacteristics(DomainNum, PipingSystemDomains(DomainNum)%Cells(X, Y, Z), &
                                                         CurDirection, NeighborTemp, Resistance, NX, NY, NZ)
                    CALL SetAdditionalNeighborData(DomainNum, X, Y, Z, CurDirection, Resistance, &
                                                   PipingSystemDomains(DomainNum)%Cells(NX, NY, NZ))
                END DO
            END DO
        END DO
    END DO

    !'create circuit array for convenience

    IF (.NOT. ALLOCATED(PipingSystemCircuits(CircuitNum)%ListOfCircuitPoints)) THEN

        SegCtr2 = -1

        TotalSegments = SIZE(PipingSystemDomains(DomainNum)%Cells, 3) * SIZE(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces)
        ALLOCATE(PipingSystemCircuits(CircuitNum)%ListOfCircuitPoints(0:TotalSegments-1))

        DO SegIndex = LBOUND(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces,1), &
                      UBOUND(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces,1)

            !'set simulation flow direction
            SELECT CASE (PipingSystemSegments(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces(SegIndex))%FlowDirection)
            CASE (SegmentFlow_IncreasingZ)
                StartingZ = 0
                EndingZ = UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
                Increment = 1
            CASE (SegmentFlow_DecreasingZ)
                StartingZ = UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
                EndingZ = 0
                Increment = -1
            END SELECT

            PipeX = PipingSystemSegments(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces(SegIndex))%PipeCellCoordinates%X
            PipeY = PipingSystemSegments(PipingSystemCircuits(CircuitNum)%PipeSegmentIndeces(SegIndex))%PipeCellCoordinates%Y

            !'loop across all z-direction indeces
            DO Zindex = StartingZ, EndingZ, Increment
                SegCtr2 = SegCtr2 + 1
                PipingSystemCircuits(CircuitNum)%ListOfCircuitPoints(SegCtr2) = Point3DInteger(PipeX, PipeY, Zindex)
            END DO

        END DO

    END IF

    !'initialize freezing calculation variables
    CALL EvaluateSoilRhoCp(DomainNum, InitOnly=.TRUE.)

    !'we can also initialize the domain based on the farfield temperature here
    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells, 3), UBOUND(PipingSystemDomains(DomainNum)%Cells, 3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells, 2), UBOUND(PipingSystemDomains(DomainNum)%Cells, 2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells, 1), UBOUND(PipingSystemDomains(DomainNum)%Cells, 1)

                !On OneTimeInit, the cur sim time should be zero, so this will be OK
                ThisCellTemp = GetFarfieldTemp(DomainNum, PipingSystemDomains(DomainNum)%Cells(X, Y, Z))
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature = ThisCellTemp
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature_PrevIteration = ThisCellTemp
                PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature_PrevTimeStep = ThisCellTemp

                IF (PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType == CellType_Pipe) THEN

                    DO rctr = 0, UBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil, 1)
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Temperature = ThisCellTemp
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Temperature_PrevIteration = &
                                                                                                                        ThisCellTemp
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Temperature_PrevTimeStep = &
                                                                                                                        ThisCellTemp
                    END DO
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature = ThisCellTemp
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature_PrevIteration = ThisCellTemp
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Temperature_PrevTimeStep = ThisCellTemp
                    IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature = ThisCellTemp
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature_PrevIteration = &
                                                                                                                        ThisCellTemp
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Insulation%MyBase%Temperature_PrevTimeStep = &
                                                                                                                        ThisCellTemp
                    END IF
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature = ThisCellTemp
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature_PrevIteration = ThisCellTemp
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Temperature_PrevTimeStep = ThisCellTemp

                END IF

            END DO
        END DO
    END DO

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE DoStartOfTimeStepInitializations(DomainNum, CircuitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataEnvironment, ONLY: OutDryBulbTemp, OutRelHum, WindSpeed, BeamSolarRad
    USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol, GetConductivityGlycol, GetViscosityGlycol
    USE DataPlant, ONLY: PlantLoop !only for fluid name/index

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: CircuitNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: X, Y, Z
    REAL(r64) :: Temperature
    REAL(r64) :: Beta
    REAL(r64) :: CellTemp
    REAL(r64) :: CellRhoCp
    INTEGER :: radialctr
    INTEGER :: rCtr
    REAL(r64) :: FluidCp
    REAL(r64) :: FluidDensity
    REAL(r64) :: FluidConductivity
    REAL(r64) :: FluidViscosity
    REAL(r64) :: FluidPrandtl

    !Update environmental conditions
    PipingSystemDomains(DomainNum)%Cur%CurAirTemp = OutDryBulbTemp
    PipingSystemDomains(DomainNum)%Cur%CurWindSpeed = WindSpeed
    PipingSystemDomains(DomainNum)%Cur%CurRelativeHumidity = OutRelHum
    PipingSystemDomains(DomainNum)%Cur%CurIncidentSolar = BeamSolarRad

    !retreive fluid properties based on the circuit inlet temperature -- which varies during the simulation
    ! but need to verify the value of inlet temperature during warmup, etc.
    FluidCp      = GetSpecificHeatGlycol(PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidName, &
                                         PipingSystemCircuits(CircuitNum)%InletTemperature, &
                                         PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidIndex, &
                                         'PipingSystemCircuit::DoStartOfTimeStepInitializations')
    FluidDensity = GetDensityGlycol(PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidName, &
                                        PipingSystemCircuits(CircuitNum)%InletTemperature, &
                                        PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidIndex, &
                                        'PipingSystemCircuit::DoStartOfTimeStepInitializations')
    FluidConductivity = GetConductivityGlycol(PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidName, &
                                        PipingSystemCircuits(CircuitNum)%InletTemperature, &
                                        PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidIndex, &
                                        'PipingSystemCircuit::DoStartOfTimeStepInitializations')
    FluidViscosity = GetViscosityGlycol(PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidName, &
                                        PipingSystemCircuits(CircuitNum)%InletTemperature, &
                                        PlantLoop(PipingSystemCircuits(CircuitNum)%LoopNum)%FluidIndex, &
                                        'PipingSystemCircuit::DoStartOfTimeStepInitializations')

    !Doesn't anyone care about poor Ludwig Prandtl?
    FluidPrandtl = 3.0d0

    !then assign these fluid properties to the current fluid property set for easy lookup as needed
    PipingSystemCircuits(CircuitNum)%CurFluidPropertySet = ExtendedFluidProperties(  &
                                        BaseThermalPropertySet( &
                                            FluidConductivity, &
                                            FluidDensity,    &
                                            FluidCp), &
                                        FluidViscosity,  &
                                        FluidPrandtl)

    !'now update cell properties
    DO Z = LBOUND(PipingSystemDomains(DomainNum)%Cells,3), UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
        DO Y = LBOUND(PipingSystemDomains(DomainNum)%Cells,2), UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
            DO X = LBOUND(PipingSystemDomains(DomainNum)%Cells,1), UBOUND(PipingSystemDomains(DomainNum)%Cells,1)

                SELECT CASE(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%CellType)
                CASE(CellType_GeneralField, CellType_AdiabaticWall, CellType_FarfieldBoundary, CellType_GroundSurface, &
                     CellType_BasementCorner, CellType_BasementFloor, CellType_BasementWall)

                    !UPDATE CELL PROPERTY SETS
                    !'main ground cells, update with soil properties
                    CellTemp = PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature
                    CALL EvaluateSoilRhoCp(DomainNum, CellTemp, CellRhoCp)
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%SpecificHeat = &
                            CellRhoCp / PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%Density

                    !UPDATE BETA VALUE
                    !'these are basic cartesian calculation cells
                    Beta = PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize / &
                        (  PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%Density &
                         * Volume(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)) &
                         * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%SpecificHeat)
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Beta = Beta

                CASE(CellType_Pipe)

                    !UPDATE CELL PROPERTY SETS
                    !'first update the outer cell itself
                    CellTemp = PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Temperature
                    CALL EvaluateSoilRhoCp(DomainNum, CellTemp, CellRhoCp)
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%SpecificHeat = &
                            CellRhoCp / PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%Density
                    !'then update all the soil radial cells
                    DO radialctr = LBOUND(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%PipeCellData%Soil,1), &
                                   UBOUND(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%PipeCellData%Soil,1)
                        CellTemp = PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(radialctr)%MyBase%Temperature
                        CALL EvaluateSoilRhoCp(DomainNum, CellTemp, CellRhoCp)
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(radialctr)%MyBase%Properties%SpecificHeat= &
                           CellRhoCp / &
                            PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(radialctr)%MyBase%Properties%Density
                    END DO

                    !UPDATE BETA VALUES
                    !'set the interface cell
                    Beta = PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize / &
                            (  PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%Density &
                             * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%InterfaceVolume &
                             * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Properties%SpecificHeat)
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%MyBase%Beta = Beta

                    !'set the radial soil cells
                    DO rctr = 0, UBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil,1)
                        Beta = PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize / &
                            (  PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Properties%Density * &
                          RadialCellInfo_XY_CrossSectArea(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)) &
                             * Depth(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)) &
                             * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Properties%SpecificHeat)
                        PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Soil(rctr)%MyBase%Beta = Beta
                    END DO

                    !'then insulation if it exists
                    IF (PipingSystemCircuits(CircuitNum)%HasInsulation) THEN
                        Beta = PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize / &
                          (  PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%PipeCellData%Insulation%MyBase%Properties%Density * &
                           RadialCellInfo_XY_CrossSectArea(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%PipeCellData%Insulation) * &
                           Depth(PipingSystemDomains(DomainNum)%Cells(X,Y,Z)) * &
                           PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%PipeCellData%Insulation%MyBase%Properties%SpecificHeat)
                        PipingSystemDomains(DomainNum)%Cells(X,Y,Z)%PipeCellData%Insulation%MyBase%Beta = Beta
                    END IF

                    !'set the pipe cell
                    Beta = PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize / &
                            (  PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Properties%Density &
                             * RadialCellInfo_XY_CrossSectArea(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe) &
                             * Depth(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)) &
                             * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Properties%SpecificHeat)
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Pipe%MyBase%Beta = Beta

                    ! now the fluid cell also
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%Properties = &
                            PipingSystemCircuits(CircuitNum)%CurFluidPropertySet
                    PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%MyBase%Beta = &
                        PipingSystemDomains(DomainNum)%Cur%CurSimTimeStepSize / &
                            (  PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%Properties%MyBase%Density &
                             * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%Volume &
                             * PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%PipeCellData%Fluid%Properties%MyBase%SpecificHeat)


                END SELECT
            END DO
        END DO
    END DO

    !'conductivity calculations
    !'Dim K_quartz As Double = 7.7! 'W / mk
    !'Dim RHO_b As Double = 1290 '!Kg / m3
    !'Dim qua As Double = 0.32
    !'Dim porosity As Double = Theta_sat
    !'Dim K_water As Double = 0.594 'w / mk
    !''"Performance Evaluation of Soil Thermal Conductivity Models"
    !'Dim K_dry As Double = (0.135 * RHO_b + 64.7) / (2700 - 0.947 * RHO_b)
    !''from(" An improved model for predicting soil thermal conductivity from water content at room temperature, Fig 4")
    !'Dim K_other As Double = 2.0
    !'Dim K_s As Double = K_quartz ^ qua * K_other ^ (1 - qua)
    !'Dim K_sat As Double = K_s ^ (1 - porosity) * K_water ^ porosity
    !'Dim Sr As Double = Theta_liq / Theta_sat
    !'Dim Ke As Double = Math.Log10(Sr) + 1.0
    !'If Ke < 0.0 Then
    !'  Ke = 0.01
    !'End If
    !'Dim K_soil As Double = (K_sat - K_dry) * Ke + K_dry
    !'Dim K1 As Double = K_soil



END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE DoEndOfIterationOperations(DomainNum, Finished)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    LOGICAL, INTENT(IN OUT) :: Finished

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName = 'DoEndOfIterationOperations'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL :: OutOfRange

    !'check if we have converged for this iteration if we are doing implicit transient
    Finished = IsConverged_CurrentToPrevIteration(DomainNum)

    !'check for out of range temperatures here so they aren't plotted
    !'this routine should be *much* more restrictive than the exceptions, so we should be safe with this location
    OutOfRange = CheckForOutOfRangeTemps(DomainNum)
    IF (OutOfRange) THEN
        CALL ShowSevereError('PipingSystems:'//RoutineName//': Out of range temperatures detected in piping system simulation.')
        CALL ShowContinueError('This could be due to the size of the pipe circuit in relation to the loads being imposed.')
        CALL ShowContinueError('Try increasing the size of the pipe circuit and investigate sizing effects.')
        CALL ShowFatalError('Preceding error(s) cause program termination')
    END IF

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE EvaluateSoilRhoCp(DomainNum, CellTemp, rhoCp, InitOnly)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    REAL(r64), INTENT(IN),OPTIONAL :: CellTemp
    REAL(r64), INTENT(OUT),OPTIONAL :: rhoCp
    LOGICAL, INTENT(IN), OPTIONAL :: InitOnly

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    !'static variables only calculated once per simulation run
    REAL(r64), SAVE ::  Theta_ice
    REAL(r64), SAVE ::  Theta_liq
    REAL(r64), SAVE ::  Theta_sat
    REAL(r64), SAVE ::  rho_ice
    REAL(r64), SAVE ::  rho_liq
    REAL(r64), SAVE ::  rhoCp_soil_liq_1
    REAL(r64), SAVE ::  CP_liq
    REAL(r64), SAVE ::  CP_ice
    REAL(r64), SAVE ::  Lat_fus
    REAL(r64), SAVE ::  Cp_transient
    REAL(r64), SAVE ::  rhoCP_soil_liq
    REAL(r64), SAVE ::  rhoCP_soil_transient
    REAL(r64), SAVE ::  rhoCP_soil_ice
    !other variables
    REAL(r64) :: frzAllIce
    REAL(r64) :: frzIceTrans
    REAL(r64) :: frzLiqTrans
    REAL(r64) :: frzAllLiq
    REAL(r64) :: rhoCP_soil

    !These vary by domain now, so we must be careful to retrieve them every time
    Theta_liq = PipingSystemDomains(DomainNum)%Moisture%Theta_Liq
    Theta_sat = PipingSystemDomains(DomainNum)%Moisture%Theta_Sat

    !Assumption
    Theta_ice = Theta_liq

    IF (PRESENT(InitOnly)) THEN
        !'Cp (freezing) calculations
        rho_ice = 917.d0 !'Kg / m3
        rho_liq = 1000.d0 !'kg / m3
        rhoCp_soil_liq_1 = 1225000.0d0 / (1 - Theta_sat) !'J/m3K
        !'from(" An improved model for predicting soil thermal conductivity from water content at room temperature, Fig 4")
        CP_liq = 4180.0d0 !'J / KgK
        CP_ice = 2066.0d0 !'J / KgK
        Lat_fus = 334000.d0 !'J / Kg
        Cp_transient = Lat_fus / 0.4d0 + (0.5d0 * CP_ice - (CP_liq + CP_ice) / 2.d0 * 0.1d0) / 0.4d0
        !'from(" Numerical and experimental investigation of melting and freezing processes in phase change material storage")
        rhoCP_soil_liq = rhoCp_soil_liq_1 * (1.d0 - Theta_sat) + rho_liq * CP_liq * Theta_liq
        rhoCP_soil_transient = rhoCp_soil_liq_1 * (1.d0 - Theta_sat) + ((rho_liq + rho_ice)/2.0d0) * Cp_transient * Theta_ice
        rhoCP_soil_ice = rhoCp_soil_liq_1 * (1.d0 - Theta_sat) + rho_ice * CP_ice * Theta_ice  !'!J / m3K
        RETURN
    END IF

    !'set some temperatures here for generalization -- these could be set in the input file
    frzAllIce = -0.5d0
    frzIceTrans = -0.4d0
    frzLiqTrans = -0.1d0
    frzAllLiq = 0.0d0

    !'calculate this cell's new Cp value based on the cell temperature
    IF (CellTemp >= frzAllLiq) THEN
        rhoCP_soil = rhoCp_soil_liq_1
    ELSE IF (CellTemp <= frzAllIce) THEN
        rhoCP_soil = rhoCP_soil_ice
    ELSE IF ((CellTemp < frzAllLiq) .AND. (CellTemp > frzLiqTrans)) THEN
        rhoCP_soil = rhoCp_soil_liq_1 + (rhoCP_soil_transient - rhoCP_soil_liq) / (frzAllLiq-frzLiqTrans)*(frzAllLiq-CellTemp)
    ELSE IF ((CellTemp <= frzLiqTrans) .AND. (CellTemp >= frzIceTrans)) THEN
        rhoCP_soil = rhoCP_soil_transient
    ELSE IF ((CellTemp < frzIceTrans) .AND. (CellTemp > frzAllIce)) THEN
        rhoCP_soil = rhoCP_soil_transient + (rhoCP_soil_transient - rhoCP_soil_ice) / (frzIceTrans-frzAllIce)*(CellTemp-frzAllIce)
    End If
    rhoCp = rhoCP_soil

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE SetAdditionalNeighborData(DomainNum, X, Y, Z, Direction, Resistance, NeighborCell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    INTEGER, INTENT(IN) :: X, Y, Z
    INTEGER, INTENT(IN) :: Direction
    REAL(r64), INTENT(IN) :: Resistance
    TYPE(CartesianCell), INTENT(IN) :: NeighborCell

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: NeighborIndex

    DO NeighborIndex = 0, UBOUND(PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%NeighborInformation,1)
        IF (PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%NeighborInformation(NeighborIndex)%Direction == Direction) THEN
           PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%NeighborInformation(NeighborIndex)%Value%ConductionResistance = Resistance
           PipingSystemDomains(DomainNum)%Cells(X, Y, Z)%NeighborInformation(NeighborIndex)%Value%NeighborCellIndeces = &
                    Point3DInteger(NeighborCell%X_index, NeighborCell%Y_index, NeighborCell%Z_index)
        END IF
    END DO

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE EvaluateNeighborCharacteristics(DomainNum, ThisCell, CurDirection, &
                                           NeighborTemp, Resistance, NeighborX, NeighborY, NeighborZ)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: ThisCell
    INTEGER, INTENT(IN) :: CurDirection
    REAL(r64), INTENT(OUT) :: NeighborTemp
    REAL(r64), INTENT(OUT) :: Resistance
    INTEGER, INTENT(OUT), OPTIONAL :: NeighborX
    INTEGER, INTENT(OUT), OPTIONAL :: NeighborY
    INTEGER, INTENT(OUT), OPTIONAL :: NeighborZ

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: ThisCellLength
    REAL(r64) :: NeighborCellLength
    REAL(r64) :: ThisCellConductivity
    REAL(r64) :: NeighborConductivity
    REAL(r64) :: ThisNormalArea
    TYPE(NeighborInformation) :: TempNeighborInfo

    INTEGER :: NX, NY, NZ
    INTEGER :: X, Y, Z

    X = ThisCell%X_index
    Y = ThisCell%Y_index
    Z = ThisCell%Z_index

    !'get neighbor data
    SELECT CASE (CurDirection)
    CASE (Direction_PositiveY)
        NX = X
        NY = Y + 1
        NZ = Z
    CASE (Direction_NegativeY)
        NX = X
        NY = Y - 1
        NZ = Z
    CASE (Direction_PositiveX)
        NX = X + 1
        NY = Y
        NZ = Z
    CASE (Direction_NegativeX)
        NX = X - 1
        NY = Y
        NZ = Z
    CASE (Direction_PositiveZ)
        NX = X
        NY = Y
        NZ = Z + 1
    CASE (Direction_NegativeZ)
        NX = X
        NY = Y
        NZ = Z - 1
    END SELECT

    !'split effects between the two cells so we can carefully calculate resistance values
    ThisCellLength = 0.0d0
    NeighborCellLength = 0.0d0
    ThisCellConductivity = 10000.0D0
    IF (ThisCell%MyBase%Properties%Conductivity > 0.0d0) ThisCellConductivity = ThisCell%MyBase%Properties%Conductivity
    NeighborConductivity = 10000.0D0
    IF (PipingSystemDomains(DomainNum)%Cells(NX, NY, NZ)%MyBase%Properties%Conductivity > 0.0d0) NeighborConductivity = &
                                                PipingSystemDomains(DomainNum)%Cells(NX, NY, NZ)%MyBase%Properties%Conductivity

    !'calculate normal surface area
    ThisNormalArea = NormalArea(ThisCell, CurDirection)

    !'set distance based on cell types
    TempNeighborInfo = NeighborInformationArray_Value(ThisCell%NeighborInformation, CurDirection)
    IF (ThisCell%CellType == CellType_Pipe) THEN
        !'we need to be a bit careful with pipes, as they are full centroid to centroid in the z direction,
        !' but only centroid to wall in the x and y directions
        IF (CurDirection == Direction_NegativeZ .OR. CurDirection == Direction_PositiveZ) THEN
            ThisCellLength = TempNeighborInfo%ThisCentroidToNeighborWall
            NeighborCellLength = TempNeighborInfo%ThisWallToNeighborCentroid
        ELSE
            ThisCellLength = 0.0d0
            NeighborCellLength = TempNeighborInfo%ThisWallToNeighborCentroid
        END IF
    ELSE IF (PipingSystemDomains(DomainNum)%Cells(NX, NY, NZ)%CellType == CellType_Pipe) THEN
        ThisCellLength = TempNeighborInfo%ThisCentroidToNeighborWall
        NeighborCellLength = 0.0d0
    ELSE
        ThisCellLength = TempNeighborInfo%ThisCentroidToNeighborWall
        NeighborCellLength = TempNeighborInfo%ThisWallToNeighborCentroid
    END IF

    !'calculate resistance based on different conductivities between the two cells
    Resistance = (ThisCellLength / (ThisNormalArea * ThisCellConductivity)) + &
     (NeighborCellLength / (ThisNormalArea * NeighborConductivity))

    !'return proper temperature for the given simulation type
    NeighborTemp = PipingSystemDomains(DomainNum)%Cells(NX, NY, NZ)%MyBase%Temperature

    IF (PRESENT(NeighborX)) NeighborX = NX
    IF (PRESENT(NeighborX)) NeighborY = NY
    IF (PRESENT(NeighborX)) NeighborZ = NZ

END SUBROUTINE
!*********************************************************************************************!

!*********************************************************************************************!
SUBROUTINE EvaluateCellNeighborDirections(DomainNum, cell)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: DomainNum
    TYPE(CartesianCell), INTENT(IN) :: cell

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: Xmax, Ymax, Zmax
    INTEGER :: Xindex, Yindex, Zindex
    INTEGER :: NumFieldCells, NumBoundaryCells
    INTEGER :: FieldCellCtr, BoundaryCellCtr
    INTEGER, PARAMETER :: TotalNumDimensions = 6

    Xmax = UBOUND(PipingSystemDomains(DomainNum)%Cells,1)
    Ymax = UBOUND(PipingSystemDomains(DomainNum)%Cells,2)
    Zmax = UBOUND(PipingSystemDomains(DomainNum)%Cells,3)
    Xindex = cell%X_index
    Yindex = cell%Y_index
    Zindex = cell%Z_index
    !Initialize the counters

    NumFieldCells = 0
    NumBoundaryCells = 0

    !First get the count for each array
    IF(Xindex < Xmax)  NumFieldCells = NumFieldCells + 1
    IF(Xindex > 0)     NumFieldCells = NumFieldCells + 1
    IF(Yindex < Ymax)  NumFieldCells = NumFieldCells + 1
    IF(Yindex > 0)     NumFieldCells = NumFieldCells + 1
    IF(Zindex < Zmax)  NumFieldCells = NumFieldCells + 1
    IF(Zindex > 0)     NumFieldCells = NumFieldCells + 1
    NumBoundaryCells = TotalNumDimensions - NumFieldCells

    !Allocate the arrays
    IF (ALLOCATED(NeighborFieldCells)) DEALLOCATE(NeighborFieldCells)
    ALLOCATE(NeighborFieldCells(0:NumFieldCells-1))
    IF (ALLOCATED(NeighborBoundaryCells)) DEALLOCATE(NeighborBoundaryCells)
    ALLOCATE(NeighborBoundaryCells(0:NumBoundaryCells-1))

    !Then add to each array appropriately
    FieldCellCtr = -1
    BoundaryCellCtr = -1
    IF(Xindex < Xmax) THEN
        FieldCellCtr = FieldCellCtr + 1
        NeighborFieldCells(FieldCellCtr) = Direction_PositiveX
    ELSE
        BoundaryCellCtr = BoundaryCellCtr + 1
        NeighborBoundaryCells(BoundaryCellCtr) = Direction_PositiveX
    END IF

    IF(Xindex > 0)    THEN
        FieldCellCtr = FieldCellCtr + 1
        NeighborFieldCells(FieldCellCtr) = Direction_NegativeX
    ELSE
        BoundaryCellCtr = BoundaryCellCtr + 1
        NeighborBoundaryCells(BoundaryCellCtr) = Direction_NegativeX
    END IF

    IF(Yindex < Ymax) THEN
        FieldCellCtr = FieldCellCtr + 1
        NeighborFieldCells(FieldCellCtr) = Direction_PositiveY
    ELSE
        BoundaryCellCtr = BoundaryCellCtr + 1
        NeighborBoundaryCells(BoundaryCellCtr) = Direction_PositiveY
    END IF

    IF(Yindex > 0)    THEN
        FieldCellCtr = FieldCellCtr + 1
        NeighborFieldCells(FieldCellCtr) = Direction_NegativeY
    ELSE
        BoundaryCellCtr = BoundaryCellCtr + 1
        NeighborBoundaryCells(BoundaryCellCtr) = Direction_NegativeY
    END IF

    IF(Zindex < Zmax) THEN
        FieldCellCtr = FieldCellCtr + 1
        NeighborFieldCells(FieldCellCtr) = Direction_PositiveZ
    ELSE
        BoundaryCellCtr = BoundaryCellCtr + 1
        NeighborBoundaryCells(BoundaryCellCtr) = Direction_PositiveZ
    END IF

    IF(Zindex > 0)    THEN
        FieldCellCtr = FieldCellCtr + 1
        NeighborFieldCells(FieldCellCtr) = Direction_NegativeZ
    ELSE
        BoundaryCellCtr = BoundaryCellCtr + 1
        NeighborBoundaryCells(BoundaryCellCtr) = Direction_NegativeZ
    END IF

END SUBROUTINE


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
END MODULE


