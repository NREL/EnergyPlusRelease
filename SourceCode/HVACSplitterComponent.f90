Module SplitterComponent
  ! Module containing the Splitter simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   March 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage Air Path Splitter Components

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:


  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataLoopNode
USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables

          ! MODULE PARAMETERS:
PRIVATE

  ! MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC :: SplitterConditions  ! public because USEd by SimAirServingZones and the Direct Air Unit
  CHARACTER(len=MaxNameLength) :: SplitterName=' '  ! Name of the Splitter
  REAL(r64)    :: InletTemp=0.0d0
  REAL(r64)    :: InletHumRat=0.0d0
  REAL(r64)    :: InletEnthalpy=0.0d0
  REAL(r64)    :: InletPressure=0.0d0
  INTEGER      :: InletNode=0
  REAL(r64)    :: InletMassFlowRate=0.0d0  !MassFlow through the Splitter being Simulated [kg/Sec]
  REAL(r64)    :: InletMassFlowRateMaxAvail=0.0d0  !Max Avail MassFlow through the Splitter being Simulated [kg/Sec]
  REAL(r64)    :: InletMassFlowRateMinAvail=0.0d0  !Min Avail MassFlow through the Splitter being Simulated [kg/Sec]
  INTEGER      :: NumOutletNodes=0
  INTEGER, DIMENSION(:), ALLOCATABLE :: OutletNode
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletMassFlowRate
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletMassFlowRateMaxAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletMassFlowRateMinAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletTemp
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletHumRat
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletEnthalpy
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: OutletPressure
END TYPE SplitterConditions

  ! MODULE VARIABLE DECLARATIONS:
  LOGICAL :: GetSplitterInputFlag = .TRUE.
  ! Public because Used by SimAirServingZones and the Direct Air Unit
  INTEGER, PUBLIC :: NumSplitters=0     ! The Number of Splitters found in the Input
  TYPE (SplitterConditions), PUBLIC, ALLOCATABLE, DIMENSION(:) :: SplitterCond
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimAirLoopSplitter

          ! Get Input routines for module
Public  GetSplitterInput

          ! Initialization routines for module
PRIVATE InitAirLoopSplitter

          ! Algorithms for the module
Private CalcAirLoopSplitter

          ! Update routine to check convergence and update nodes
Private UpdateSplitter

          ! Reporting routines for module
Private ReportSplitter

Public GetSplitterNodeNumbers

Public GetSplitterOutletNumber

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimAirLoopSplitter(CompName,FirstHVACIteration, FirstCall, SplitterInletChanged,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Splitter component simulation.
          ! It is called from the SimAirLoopComponent
          ! at the system time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
      USE InputProcessor, ONLY: FindItemInList
      USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
  LOGICAL, INTENT(IN)    :: FirstCall
  LOGICAL                :: SplitterInletChanged
  INTEGER, INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SplitterNum     ! The Splitter that you are currently loading input for

          ! FLOW:

  ! Obtains and Allocates Splitter related parameters from input file
  IF (GetSplitterInputFlag) THEN  !First time subroutine has been entered
    CALL GetSplitterInput
  End If

  ! Find the correct SplitterNumber
  IF (CompIndex == 0) THEN
    SplitterNum=FindItemInList(CompName,SplitterCond%SplitterName,NumSplitters)
    IF (SplitterNum == 0) THEN
      CALL ShowFatalError('SimAirLoopSplitter: Splitter not found='//TRIM(CompName))
    ENDIF
    CompIndex=SplitterNum
  ELSE
    SplitterNum=CompIndex
    IF (SplitterNum > NumSplitters .or. SplitterNum < 1) THEN
      CALL ShowFatalError('SimAirLoopSplitter: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(SplitterNum))// &
                          ', Number of Splitters='//TRIM(TrimSigDigits(NumSplitters))//  &
                          ', Splitter name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(SplitterNum)) THEN
      IF (CompName /= SplitterCond(SplitterNum)%SplitterName) THEN
        CALL ShowFatalError('SimAirLoopSplitter: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(SplitterNum))// &
                            ', Splitter name='//TRIM(CompName)//', stored Splitter Name for that index='//  &
                            TRIM(SplitterCond(SplitterNum)%SplitterName))
      ENDIF
      CheckEquipName(SplitterNum)=.false.
    ENDIF
  ENDIF

  CALL InitAirLoopSplitter(SplitterNum, FirstHVACIteration, FirstCall)  ! Initialize all Splitter related parameters

  CALL CalcAirLoopSplitter(SplitterNum, FirstCall)

  ! Update the current Splitter to the outlet nodes
  Call UpdateSplitter(SplitterNum, SplitterInletChanged, FirstCall)

  ! Report the current Splitter
  Call ReportSplitter(SplitterNum)

  RETURN

END SUBROUTINE SimAirLoopSplitter

!*******************************


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetSplitterInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines and
          ! Get routines.  The Splitter only gets node connection data and not mass
          ! flow rates.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER (len=*), PARAMETER   :: RoutineName='GetSplitterInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: SplitterNum      ! The Splitter that you are currently loading input into
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: NodeNum
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound=.false.
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    INTEGER :: NumParams
    INTEGER :: OutNodeNum1
    INTEGER :: OutNodeNum2
    CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.


          ! RESET THE GETINPUT FLAG
    GetSplitterInputFlag = .FALSE.

          ! Flow
    CurrentModuleObject = 'AirLoopHVAC:ZoneSplitter'
    NumSplitters = GetNumObjectsFound(CurrentModuleObject)

    IF (NumSplitters.GT.0) ALLOCATE(SplitterCond(NumSplitters))
    ALLOCATE(CheckEquipName(NumSplitters))
    CheckEquipName=.true.

    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
    ALLOCATE(AlphArray(NumAlphas))
    AlphArray=' '
    ALLOCATE(cAlphaFields(NumAlphas))
    cAlphaFields=' '
    ALLOCATE(lAlphaBlanks(NumAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(cNumericFields(NumNums))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(NumNums))
    lNumericBlanks=.TRUE.
    ALLOCATE(NumArray(NumNums))
    NumArray=0.0d0

    DO SplitterNum = 1,  NumSplitters
      CALL GetObjectItem(CurrentModuleObject,SplitterNum,AlphArray,NumAlphas, &
                         NumArray,NumNums,IOSTAT, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(AlphArray(1),SplitterCond%SplitterName,SplitterNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      SplitterCond(SplitterNum)%SplitterName   = AlphArray(1)
      SplitterCond(SplitterNum)%InletNode      = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      SplitterCond(SplitterNum)%NumOutletNodes = NumAlphas - 2

      ALLOCATE(SplitterCond(SplitterNum)%OutletNode(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletMassFlowRate(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletMassFlowRateMaxAvail(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletMassFlowRateMinAvail(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletTemp(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletHumRat(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletEnthalpy(SplitterCond(SplitterNum)%NumOutletNodes))
      ALLOCATE(SplitterCond(SplitterNum)%OutletPressure(SplitterCond(SplitterNum)%NumOutletNodes))

      SplitterCond(SplitterNum)%InletMassFlowRate = 0.0d0
      SplitterCond(SplitterNum)%InletMassFlowRateMaxAvail = 0.0d0
      SplitterCond(SplitterNum)%InletMassFlowRateMinAvail = 0.0d0

      DO NodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes

        SplitterCond(SplitterNum)%OutletNode(NodeNum) = &
               GetOnlySingleNode(AlphArray(2+NodeNum),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
        IF (lAlphaBlanks(2+NodeNum)) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(2+NodeNum))//' is Blank, '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
          ErrorsFound=.true.
        ENDIF
      END DO

    END DO   ! end Number of Splitter Loop

    ! Check for duplicate names specified in Zone Splitter
    DO SplitterNum=1,NumSplitters
      NodeNum=SplitterCond(SplitterNum)%InletNode
      DO OutNodeNum1=1,SplitterCond(SplitterNum)%NumOutletNodes
        IF (NodeNum /= SplitterCond(SplitterNum)%OutletNode(OutNodeNum1)) CYCLE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(SplitterCond(SplitterNum)%SplitterName)//  &
           ' specifies an outlet node name the same as the inlet node.')
        CALL ShowContinueError('..'//TRIM(cAlphaFields(2))//'='//TRIM(NodeID(NodeNum)))
        CALL ShowContinueError('..Outlet Node #'//TRIM(TrimSigDigits(OutNodeNum1))//  &
           ' is duplicate.')
        ErrorsFound=.true.
      ENDDO
      DO OutNodeNum1=1,SplitterCond(SplitterNum)%NumOutletNodes
        DO OutNodeNum2=OutNodeNum1+1,SplitterCond(SplitterNum)%NumOutletNodes
          IF (SplitterCond(SplitterNum)%OutletNode(OutNodeNum1) /= SplitterCond(SplitterNum)%OutletNode(OutNodeNum2)) CYCLE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(SplitterCond(SplitterNum)%SplitterName)//  &
             ' specifies duplicate outlet nodes in its outlet node list.')
          CALL ShowContinueError('..Outlet Node #'//TRIM(TrimSigDigits(OutNodeNum1))//  &
             ' Name='//TRIM(NodeID(OutNodeNum1)))
          CALL ShowContinueError('..Outlet Node #'//TRIM(TrimSigDigits(OutNodeNum2))//  &
             ' is duplicate.')
          ErrorsFound=.true.
        ENDDO
      ENDDO
    ENDDO

    DEALLOCATE(AlphArray)
    DEALLOCATE(NumArray)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(lNumericBlanks)


    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in getting input.')
    ENDIF

  RETURN

END SUBROUTINE GetSplitterInput

! End of Get Input subroutines for the HB Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitAirLoopSplitter(SplitterNum, FirstHVACIteration, FirstCall)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initialisations of the Splitter Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY : OutBaroPress, OutHumRat
  USE Psychrometrics,  ONly : PsyHFnTdbW
  USE DataContaminantBalance, ONLY: Contaminant, OutdoorCO2, OutdoorGC

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: SplitterNum
  LOGICAL, INTENT(IN)            :: FirstHVACIteration
  LOGICAL, INTENT(IN)            :: FirstCall

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: InletNode
  INTEGER        :: OutletNode
  INTEGER        :: NodeNum
  REAL(r64)      :: AirEnthalpy ! [J/kg]
  LOGICAL,SAVE   :: MyEnvrnFlag=.true.

          ! FLOW:

! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN

          ! Calculate the air density and enthalpy for standard conditions...
      AirEnthalpy = PsyHFnTdbW(20.0d0,OutHumRat)

          ! Initialize the inlet node to s standard set of conditions so that the
          !  flows match around the loop & do not cause convergence problems.
      InletNode = SplitterCond(SplitterNum)%InletNode
      Node(InletNode)%Temp      = 20.0d0
      Node(InletNode)%HumRat    = OutHumRat
      Node(InletNode)%Enthalpy  = AirEnthalpy
      Node(InletNode)%Press     = OutBaroPress
      IF (Contaminant%CO2Simulation) Then
        Node(InletNode)%CO2 = OutdoorCO2
      End If
      IF (Contaminant%GenericContamSimulation) Then
        Node(InletNode)%GenContam = OutdoorGC
      End If

      MyEnvrnFlag = .FALSE.

  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  ! Set the inlet node for the Splitter
  InletNode = SplitterCond(SplitterNum)%InletNode


  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! Load the node data in this section for the component simulation

  ! This section is very important to understand.  The system off condition is important
  ! transfer around the loop even if the splitter does not have enough information to
  ! calculate the correct flow rates since the dampers are downstream and there is no pressure
  ! simulation.  What happens in this section is the flow from upstream is not zero is
  ! arbitrarily split by the number of inlet nodes.  This is by no way meant to determine the
  ! correct split flow!  Just to give each outlet a non-zero flow so that the Air Distribution
  ! Unit(ADU) downstream knows that the system is operating or has flow.  This is only done the first
  ! iteration through and the splitter first pass.  After the first iteration the ADU sets the
  ! correct flow and that is used and passed back upstream.
  IF (FirstHVACIteration .AND. FirstCall) THEN
    IF(Node(InletNode)%MassFlowRate > 0.0d0) Then
     DO NodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
       OutletNode = SplitterCond(SplitterNum)%OutletNode(NodeNum)
       Node(OutletNode)%MassFlowRate = Node(InletNode)%MassFlowRate/  &
                                       SplitterCond(SplitterNum)%NumOutletNodes
     END DO
    END IF
    IF(Node(InletNode)%MassFlowRateMaxAvail > 0.0d0) Then
     DO NodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
       OutletNode = SplitterCond(SplitterNum)%OutletNode(NodeNum)
       Node(OutletNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRateMaxAvail/  &
                                               SplitterCond(SplitterNum)%NumOutletNodes
     END DO
    END IF

  END IF !For FirstHVACIteration and FirstCall


  IF (FirstCall) THEN
  !There is one exception to the rule stated above and that is if the system shuts OFF
  ! for some operational or algorithm dependency.  This IF block should catch that condition
  ! and then pass the NO flow condition downstream to the waiting ADU's.  Most of the time
  ! this IF is jumped over.
    IF(Node(InletNode)%MassFlowRateMaxAvail == 0.0d0) Then

       DO NodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes

         OutletNode = SplitterCond(SplitterNum)%OutletNode(NodeNum)
         Node(OutletNode)%MassFlowRate = 0.0d0
         Node(OutletNode)%MassFlowRateMaxAvail = 0.0d0
         Node(OutletNode)%MassFlowRateMinAvail = 0.0d0

       END DO
    End IF !For Node inlet Max Avail = 0.0

    !Pass the State Properties through every time.  This is what mainly happens each time
    ! through the splitter,
    InletNode = SplitterCond(SplitterNum)%InletNode
    SplitterCond(SplitterNum)%InletTemp         = Node(InletNode)%Temp
    SplitterCond(SplitterNum)%InletHumRat       = Node(InletNode)%HumRat
    SplitterCond(SplitterNum)%InletEnthalpy     = Node(InletNode)%Enthalpy
    SplitterCond(SplitterNum)%InletPressure     = Node(InletNode)%Press

  Else  !On the second call from the ZoneEquipManager this is where the flows are passed back to
        ! the splitter inlet.
    DO NodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes

       OutletNode = SplitterCond(SplitterNum)%OutletNode(NodeNum)
       SplitterCond(SplitterNum)%OutletMassFlowRate(NodeNum) = Node(OutletNode)%MassFlowRate
       SplitterCond(SplitterNum)%OutletMassFlowRateMaxAvail(NodeNum) = Node(OutletNode)%MassFlowRateMaxAvail
       SplitterCond(SplitterNum)%OutletMassFlowRateMinAvail(NodeNum) = Node(OutletNode)%MassFlowRateMinAvail

    End Do

  END IF !For FirstCall

 RETURN

END SUBROUTINE InitAirLoopSplitter

! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE CalcAirLoopSplitter(SplitterNum, FirstCall)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SplitterNum
  LOGICAL, INTENT(IN) :: FirstCall

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER :: OutletNodeNum

 !The first time through the State properties are split and passed through
 IF(FirstCall) Then
   ! Moisture balance to get outlet air humidity ratio
   DO OutletNodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
     SplitterCond(SplitterNum)%OutletHumRat(OutletNodeNum) = SplitterCond(SplitterNum)%InletHumRat
   END DO

   ! "Momentum balance" to get outlet air pressure
   DO OutletNodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
     SplitterCond(SplitterNum)%OutletPressure(OutletNodeNum) = SplitterCond(SplitterNum)%InletPressure
   END DO

   ! Energy balance to get outlet air enthalpy
   DO OutletNodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
     SplitterCond(SplitterNum)%OutletEnthalpy(OutletNodeNum) = SplitterCond(SplitterNum)%InletEnthalpy
   END DO

   ! Set outlet temperatures equal to inlet temperature
   DO OutletNodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
     SplitterCond(SplitterNum)%OutletTemp(OutletNodeNum) = SplitterCond(SplitterNum)%InletTemp
   END DO

 ELSE
   !This is the second time through and this is where the mass flows from each outlet are
   ! summed and then assigned upstream to the inlet node.
   ! Overall Mass Continuity Equation to get inlet mass flow rates
   !Zero the inlet Totals before the Inlets are summed
   SplitterCond(SplitterNum)%InletMassFlowRate         = 0.0d0
   SplitterCond(SplitterNum)%InletMassFlowRateMaxAvail = 0.0d0
   SplitterCond(SplitterNum)%InletMassFlowRateMinAvail = 0.0d0

   DO OutletNodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
     SplitterCond(SplitterNum)%InletMassFlowRate = SplitterCond(SplitterNum)%InletMassFlowRate + &
                                                   SplitterCond(SplitterNum)%OutletMassFlowRate(OutletNodeNum)

     SplitterCond(SplitterNum)%InletMassFlowRateMaxAvail = SplitterCond(SplitterNum)%InletMassFlowRateMaxAvail +  &
                                                           SplitterCond(SplitterNum)%OutletMassFlowRateMaxAvail(OutletNodeNum)
     SplitterCond(SplitterNum)%InletMassFlowRateMinAvail = SplitterCond(SplitterNum)%InletMassFlowRateMinAvail +  &
                                                           SplitterCond(SplitterNum)%OutletMassFlowRateMinAvail(OutletNodeNum)

   END DO

   ! What happens if Splitter inlet mass flow rate is greater than max available
 END IF

 RETURN
END SUBROUTINE CalcAirLoopSplitter

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the Splitter Module
! *****************************************************************************

SUBROUTINE UpdateSplitter(SplitterNum, SplitterInletChanged, FirstCall)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, Intent(IN)   :: SplitterNum
  LOGICAL, Intent(InOut)::SplitterInletChanged
  LOGICAL, INTENT(IN)   :: FirstCall

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: FlowRateToler    = 0.01d0     ! Tolerance for mass flow rate convergence (in kg/s)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer :: InletNode
  Integer :: OutletNode
  Integer :: NodeNum


 !Set the inlet node for this splitter to be used throughout subroutine for either case
 InletNode = SplitterCond(SplitterNum)%InletNode

 !On the FirstCall the State properties are passed through and the mass flows are not dealt with
 ! except for NO flow conditions
 IF(FirstCall) Then
   ! Set the outlet nodes for properties that just pass through & not used
   DO NodeNum = 1, SplitterCond(SplitterNum)%NumOutletNodes
     OutletNode = SplitterCond(SplitterNum)%OutletNode(NodeNum)
     Node(OutletNode)%Temp          = SplitterCond(SplitterNum)%OutletTemp(NodeNum)
     Node(OutletNode)%HumRat        = SplitterCond(SplitterNum)%OutletHumRat(NodeNum)
     Node(OutletNode)%Enthalpy      = SplitterCond(SplitterNum)%OutletEnthalpy(NodeNum)
     Node(OutletNode)%Quality       = Node(InletNode)%Quality
     Node(OutletNode)%Press         = SplitterCond(SplitterNum)%OutletPressure(NodeNum)
     IF (Contaminant%CO2Simulation) Then
       Node(OutletNode)%CO2 = Node(InletNode)%CO2
     End If
     IF (Contaminant%GenericContamSimulation) Then
       Node(OutletNode)%GenContam = Node(InletNode)%GenContam
     End If
   END DO

 ELSE
   ! The second time through just updates the mass flow conditions back upstream
   !  to the inlet.  Before it sets the inlet it checks to see that the flow rate has not
   !  changed or not.  The tolerance has been relaxed some now that the splitter has been
   !  re-written

   ! Set the outlet air nodes of the Splitter if the splitter results have changed
   !  beyond the tolerance.
   If(ABS(Node(InletNode)%MassFlowRate - SplitterCond(SplitterNum)%InletMassFlowRate).GT.FlowRateToler) Then
      SplitterInletChanged = .TRUE.
   END IF
   Node(InletNode)%MassFlowRate = SplitterCond(SplitterNum)%InletMassFlowRate
   Node(InletNode)%MassFlowRateMaxAvail = SplitterCond(SplitterNum)%InletMassFlowRateMaxAvail
   Node(InletNode)%MassFlowRateMinAvail = SplitterCond(SplitterNum)%InletMassFlowRateMinAvail

 END IF !The FirstCall END IF

 RETURN
END Subroutine UpdateSplitter


!        End of Update subroutines for the Splitter Module
! *****************************************************************************


! Beginning of Reporting subroutines for the Splitter Module
! *****************************************************************************

SUBROUTINE ReportSplitter(SplitterNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: SplitterNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

 ! Write(*,*)=SplitterCond(SplitterNum)%SplitterPower    Still needs to report the Splitter power from this component


  RETURN
END Subroutine ReportSplitter

FUNCTION GetSplitterOutletNumber(SplitterName,SplitterNum,ErrorsFound) RESULT(SplitterOutletNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given AirLoopHVAC:ZoneSplitter and returns the number of outlet nodes.  If
          ! incorrect AirLoopHVAC:ZoneSplitter name is given, errorsfound is returned as true
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SplitterName     ! must match Splitter names for the Splitter type
  INTEGER, INTENT(IN) :: SplitterNum     ! Index of Splitters
  LOGICAL, INTENT(INOUT)       :: ErrorsFound     ! set to true if problem

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichSplitter, SplitterOutletNumber

  ! Obtains and Allocates AirLoopHVAC:ZoneSplitter related parameters from input file
  IF (GetSplitterInputFlag) THEN  !First time subroutine has been entered
    CALL GetSplitterInput
    GetSplitterInputFlag=.false.
  End If

  If (SplitterNum .EQ. 0) Then
    WhichSplitter = FindItemInList(SplitterName,SplitterCond%SplitterName,NumSplitters)
  Else
    WhichSplitter = SplitterNum
  End If

  IF (WhichSplitter /= 0) THEN
    SplitterOutletNumber = SplitterCond(WhichSplitter)%NumOutletNodes
  ENDIF

  IF (WhichSplitter == 0) THEN
    CALL ShowSevereError('GetSplitterOuletNumber: Could not find Splitter = "'//TRIM(SplitterName)//'"')
    ErrorsFound=.true.
    SplitterOutletNumber = 0
  ENDIF

  RETURN

END FUNCTION GetSplitterOutletNumber

FUNCTION GetSplitterNodeNumbers(SplitterName,SplitterNum,ErrorsFound) RESULT(SplitterNodeNumbers)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given AirLoopHVAC:ZoneSplitter and returns the node numbers.  If
          ! incorrect AirLoopHVAC:ZoneSplitter name is given, errorsfound is returned as true
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SplitterName     ! must match Splitter names for the Splitter type
  INTEGER, INTENT(IN) :: SplitterNum     ! Index of Splitters
  LOGICAL, INTENT(INOUT)       :: ErrorsFound     ! set to true if problem

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichSplitter
  INTEGER :: I
  INTEGER, DIMENSION(:), ALLOCATABLE :: SplitterNodeNumbers

  ! Obtains and Allocates AirLoopHVAC:ZoneSplitter related parameters from input file
  IF (GetSplitterInputFlag) THEN  !First time subroutine has been entered
    CALL GetSplitterInput
    GetSplitterInputFlag=.false.
  End If

  If (SplitterNum .EQ. 0) Then
    WhichSplitter = FindItemInList(SplitterName,SplitterCond%SplitterName,NumSplitters)
  Else
    WhichSplitter = SplitterNum
  End If

  IF (WhichSplitter /= 0) THEN
    ALLOCATE(SplitterNodeNumbers(SplitterCond(WhichSplitter)%NumOutletNodes+2))
    SplitterNodeNumbers(1) = SplitterCond(WhichSplitter)%InletNode
    SplitterNodeNumbers(2) = SplitterCond(WhichSplitter)%NumOutletNodes
    Do I=1,SplitterNodeNumbers(2)
      SplitterNodeNumbers(i+2) = SplitterCond(WhichSplitter)%OutletNode(I)
    End Do
  ENDIF

  IF (WhichSplitter == 0) THEN
    CALL ShowSevereError('GetSplitterNodeNumbers: Could not find Splitter = "'//TRIM(SplitterName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetSplitterNodeNumbers

!        End of Reporting subroutines for the Splitter Module
! *****************************************************************************

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

End Module SplitterComponent

