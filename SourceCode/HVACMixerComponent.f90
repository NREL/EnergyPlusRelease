Module MixerComponent
  ! Module containing the Mixer simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   March 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage Air Path Mixer Components

  ! METHODOLOGY EMPLOYED:
  ! This Mixer is very simple.  It just takes the inlets and sums them
  ! and sets that to the outlet conditions.  For the State Properties
  ! it just takes the flow weighted averages of them.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,    ONLY: BeginEnvrnFlag,BeginDayFlag
USE DataInterfaces, ONLY: ShowSevereError,ShowFatalError,ShowContinueError
USE DataLoopNode
USE DataHVACGlobals
Use DataEnvironment, ONLY: OutBaroPress

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
REAL(r64), PARAMETER :: MassFlowTol = 0.001d0

  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC :: MixerConditions
  CHARACTER(len=MaxNameLength) :: MixerName=' '  ! Name of the Mixer
  REAL(r64)    :: OutletTemp=0.0d0
  REAL(r64)    :: OutletHumRat=0.0d0
  REAL(r64)    :: OutletEnthalpy=0.0d0
  REAL(r64)    :: OutletPressure=0.0d0
  INTEGER      :: OutletNode=0
  REAL(r64)    :: OutletMassFlowRate=0.0d0 !MassFlow through the Mixer being Simulated [kg/Sec]
  REAL(r64)    :: OutletMassFlowRateMaxAvail=0.0d0 ! [kg/Sec]
  REAL(r64)    :: OutletMassFlowRateMinAvail=0.0d0 ! [kg/Sec]
  LOGICAL      :: InitFlag=.false.
  INTEGER      :: NumInletNodes=0
  INTEGER, DIMENSION(:), ALLOCATABLE ::InletNode
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletMassFlowRate
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletMassFlowRateMaxAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletMassFlowRateMinAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletTemp
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletHumRat
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletEnthalpy
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletPressure
END TYPE MixerConditions

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER, PUBLIC, SAVE     :: NumMixers=0             ! The Number of Mixers found in the Input
  INTEGER           :: LoopInletNode=0
  INTEGER           :: LoopOutletNode=0
  TYPE (MixerConditions), PUBLIC, ALLOCATABLE, DIMENSION(:) :: MixerCond
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName



  ! SUBROUTINE SPECIFICATIONS FOR MODULE Mixers
Public  SimAirMixer
Public  GetMixerInput
PRIVATE InitAirMixer
Private CalcAirMixer
Private UpdateAirMixer
Private ReportMixer

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimAirMixer(CompName,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Mixer component simulation.
          ! It is called from the SimAirLoopComponent
          ! at the system time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: MixerNum     ! The Mixer that you are currently loading input into
  Logical,Save      :: GetInputFlag = .True. ! Flag set to make sure you get input once

          ! FLOW:

  ! Obtains and Allocates Mixer related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetMixerInput
    GetInputFlag=.false.
  End If


  ! Find the correct MixerNumber
  IF (CompIndex == 0) THEN
    MixerNum=FindItemInList(CompName,MixerCond%MixerName,NumMixers)
    IF (MixerNum == 0) THEN
      CALL ShowFatalError('SimAirLoopMixer: Mixer not found='//TRIM(CompName))
    ENDIF
    CompIndex=MixerNum
  ELSE
    MixerNum=CompIndex
    IF (MixerNum > NumMixers .or. MixerNum < 1) THEN
      CALL ShowFatalError('SimAirLoopMixer: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(MixerNum))// &
                          ', Number of Mixers='//TRIM(TrimSigDigits(NumMixers))//  &
                          ', Mixer name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(MixerNum)) THEN
      IF (CompName /= MixerCond(MixerNum)%MixerName) THEN
        CALL ShowFatalError('SimAirLoopMixer: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(MixerNum))// &
                            ', Mixer name='//TRIM(CompName)//', stored Mixer Name for that index='//  &
                            TRIM(MixerCond(MixerNum)%MixerName))
      ENDIF
      CheckEquipName(MixerNum)=.false.
    ENDIF
  ENDIF

  ! With the correct MixerNum Initialize
  CALL InitAirMixer(MixerNum)  ! Initialize all Mixer related parameters

  CALL CalcAirMixer(MixerNum)

  ! Update the current Mixer to the outlet nodes
  Call UpdateAirMixer(MixerNum)

  ! Report the current Mixer
  Call ReportMixer(MixerNum)

  RETURN

END SUBROUTINE SimAirMixer


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetMixerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,VerifyName,GetObjectDefMaxArgs
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER (len=*), PARAMETER   :: RoutineName='GetMixerInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: MixerNum      ! The Mixer that you are currently loading input into
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: NodeNum
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound=.false.
    LOGICAL       :: IsNotOK               ! Flag to verify name
    LOGICAL       :: IsBlank               ! Flag for blank name
    INTEGER       :: NumParams
    INTEGER       :: InNodeNum1
    INTEGER       :: InNodeNum2
    CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.


          ! Flow
    CurrentModuleObject = 'AirLoopHVAC:ZoneMixer'
    NumMixers = GetNumObjectsFound(CurrentModuleObject)

    IF (NumMixers.GT.0) ALLOCATE(MixerCond(NumMixers))
    ALLOCATE(CheckEquipName(NumMixers))
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

    DO MixerNum = 1,  NumMixers
      CALL GetObjectItem(CurrentModuleObject,MixerNum,AlphArray,NumAlphas, &
                         NumArray,NumNums,IOSTAT, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(AlphArray(1),MixerCond%MixerName,MixerNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      MixerCond(MixerNum)%MixerName = AlphArray(1)

      MixerCond(MixerNum)%OutletNode  = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      MixerCond(MixerNum)%NumInletNodes = NumAlphas -2

      MixerCond%InitFlag = .TRUE.

      ALLOCATE(MixerCond(MixerNum)%InletNode(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletMassFlowRate(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletMassFlowRateMaxAvail(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletMassFlowRateMinAvail(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletTemp(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletHumRat(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletEnthalpy(MixerCond(MixerNum)%NumInletNodes))
      ALLOCATE(MixerCond(MixerNum)%InletPressure(MixerCond(MixerNum)%NumInletNodes))

      MixerCond(MixerNum)%InletNode = 0
      MixerCond(MixerNum)%InletMassFlowRate = 0.0d0
      MixerCond(MixerNum)%InletMassFlowRateMaxAvail = 0.0d0
      MixerCond(MixerNum)%InletMassFlowRateMinAvail = 0.0d0
      MixerCond(MixerNum)%InletTemp = 0.0d0
      MixerCond(MixerNum)%InletHumRat = 0.0d0
      MixerCond(MixerNum)%InletEnthalpy = 0.0d0
      MixerCond(MixerNum)%InletPressure = 0.0d0
      MixerCond(MixerNum)%OutletMassFlowRate = 0.0d0
      MixerCond(MixerNum)%OutletMassFlowRateMaxAvail = 0.0d0
      MixerCond(MixerNum)%OutletMassFlowRateMinAvail = 0.0d0
      MixerCond(MixerNum)%OutletTemp = 0.0d0
      MixerCond(MixerNum)%OutletHumRat = 0.0d0
      MixerCond(MixerNum)%OutletEnthalpy = 0.0d0
      MixerCond(MixerNum)%OutletPressure = 0.0d0

      DO NodeNum = 1, MixerCond(MixerNum)%NumInletNodes

        MixerCond(MixerNum)%InletNode(NodeNum) = &
               GetOnlySingleNode(AlphArray(2+NodeNum),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        IF (lAlphaBlanks(2+NodeNum)) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(2+NodeNum))//' is Blank, '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
          ErrorsFound=.true.
        ENDIF

      END DO

    END DO   ! end Number of Mixer Loop

    ! Check for duplicate names specified in Zone Mixer
    DO MixerNum=1,NumMixers
      NodeNum=MixerCond(MixerNum)%OutletNode
      DO InNodeNum1=1,MixerCond(MixerNum)%NumInletNodes
        IF (NodeNum /= MixerCond(MixerNum)%InletNode(InNodeNum1)) CYCLE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(MixerCond(MixerNum)%MixerName)//  &
           ' specifies an inlet node name the same as the outlet node.')
        CALL ShowContinueError('..'//TRIM(cAlphaFields(2))//' = '//TRIM(NodeID(NodeNum)))
        CALL ShowContinueError('..Inlet Node #'//TRIM(TrimSigDigits(InNodeNum1))//  &
           ' is duplicate.')
        ErrorsFound=.true.
      ENDDO
      DO InNodeNum1=1,MixerCond(MixerNum)%NumInletNodes
        DO InNodeNum2=InNodeNum1+1,MixerCond(MixerNum)%NumInletNodes
          IF (MixerCond(MixerNum)%InletNode(InNodeNum1) /= MixerCond(MixerNum)%InletNode(InNodeNum2)) CYCLE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(MixerCond(MixerNum)%MixerName)//  &
             ' specifies duplicate inlet nodes in its inlet node list.')
          CALL ShowContinueError('..Inlet Node #'//TRIM(TrimSigDigits(InNodeNum1))//  &
             ' Name='//TRIM(NodeID(InNodeNum1)))
          CALL ShowContinueError('..Inlet Node #'//TRIM(TrimSigDigits(InNodeNum2))//  &
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

END SUBROUTINE GetMixerInput

! End of Get Input subroutines for the HB Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitAirMixer(MixerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations of the Mixer Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: MixerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: InletNode
  Integer             :: NodeNum
  ! FLOW:


  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.

  !Transfer the node data to MixerCond data structure
  DO NodeNum = 1, MixerCond(MixerNum)%NumInletNodes

    InletNode = MixerCond(MixerNum)%InletNode(NodeNum)
    !Set all of the inlet mass flow variables from the nodes
    MixerCond(MixerNum)%InletMassFlowRate(NodeNum) = Node(InletNode)%MassFlowRate
    MixerCond(MixerNum)%InletMassFlowRateMaxAvail(NodeNum) = Node(InletNode)%MassFlowRateMaxAvail
    MixerCond(MixerNum)%InletMassFlowRateMinAvail(NodeNum) = Node(InletNode)%MassFlowRateMinAvail
    !Set all of the inlet state variables from the inlet nodes
    MixerCond(MixerNum)%InletTemp(NodeNum)         = Node(InletNode)%Temp
    MixerCond(MixerNum)%InletHumRat(NodeNum)       = Node(InletNode)%HumRat
    MixerCond(MixerNum)%InletEnthalpy(NodeNum)     = Node(InletNode)%Enthalpy
    MixerCond(MixerNum)%InletPressure(NodeNum)     = Node(InletNode)%Press

  END DO


  RETURN

END SUBROUTINE InitAirMixer




 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE CalcAirMixer(MixerNum)


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
  USE Psychrometrics, ONlY: PsyTdbFnHW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      Integer :: MixerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      Integer :: InletNodeNum


   !Reset the totals to zero before they are summed.
   MixerCond(MixerNum)%OutletMassFlowRate = 0.0d0
   MixerCond(MixerNum)%OutletMassFlowRateMaxAvail = 0.0d0
   MixerCond(MixerNum)%OutletMassFlowRateMinAvail = 0.0d0
   MixerCond(MixerNum)%OutletTemp = 0.0d0
   MixerCond(MixerNum)%OutletHumRat = 0.0d0
   MixerCond(MixerNum)%OutletPressure = 0.0d0
   MixerCond(MixerNum)%OutletEnthalpy = 0.0d0



   DO InletNodeNum = 1, MixerCond(MixerNum)%NumInletNodes
     MixerCond(MixerNum)%OutletMassFlowRate = MixerCond(MixerNum)%OutletMassFlowRate + &
                                                 MixerCond(MixerNum)%InletMassFlowRate(InletNodeNum)
     MixerCond(MixerNum)%OutletMassFlowRateMaxAvail = MixerCond(MixerNum)%OutletMassFlowRateMaxAvail + &
                                                 MixerCond(MixerNum)%InletMassFlowRateMaxAvail(InletNodeNum)
     MixerCond(MixerNum)%OutletMassFlowRateMinAvail = MixerCond(MixerNum)%OutletMassFlowRateMinAvail + &
                                                 MixerCond(MixerNum)%InletMassFlowRateMinAvail(InletNodeNum)
   END DO

   If(MixerCond(MixerNum)%OutletMassFlowRate .gt. 0.0d0) Then

      ! Mass balance on moisture to get outlet air humidity ratio

      DO InletNodeNum = 1, MixerCond(MixerNum)%NumInletNodes
        MixerCond(MixerNum)%OutletHumRat = MixerCond(MixerNum)%OutletHumRat + &
                                     MixerCond(MixerNum)%InletMassFlowRate(InletNodeNum) * &
                                     MixerCond(MixerNum)%InletHumRat(InletNodeNum) / &
                                     MixerCond(MixerNum)%OutletMassFlowRate
      END DO

     ! "Momentum balance" to get outlet air pressure

     DO InletNodeNum = 1, MixerCond(MixerNum)%NumInletNodes
       MixerCond(MixerNum)%OutletPressure = MixerCond(MixerNum)%OutletPressure + &
                                                MixerCond(MixerNum)%InletPressure(InletNodeNum) * &
                                                MixerCond(MixerNum)%InletMassFlowRate(InletNodeNum) / &
                                                MixerCond(MixerNum)%OutletMassFlowRate
     END DO

     ! Energy balance to get outlet air enthalpy

     DO InletNodeNum = 1, MixerCond(MixerNum)%NumInletNodes
       MixerCond(MixerNum)%OutletEnthalpy = MixerCond(MixerNum)%OutletEnthalpy + &
                                     MixerCond(MixerNum)%InletEnthalpy(InletNodeNum) * &
                                     MixerCond(MixerNum)%InletMassFlowRate(InletNodeNum) / &
                                     MixerCond(MixerNum)%OutletMassFlowRate
     END DO

     ! Use Enthalpy and humidity ratio to get outlet temperature from psych chart

     MixerCond(MixerNum)%OutletTemp = PsyTdbFnHW(MixerCond(MixerNum)%OutletEnthalpy, &
                                              MixerCond(MixerNum)%OutletHumRat)

   Else
      ! Mass Flow in air loop is zero and loop is not operating.
      ! Arbitrarily set the output to the first inlet leg
      MixerCond(MixerNum)%OutletHumRat     = MixerCond(MixerNum)%InletHumRat(1)
      MixerCond(MixerNum)%OutletPressure   = MixerCond(MixerNum)%InletPressure(1)
      MixerCond(MixerNum)%OutletEnthalpy   = MixerCond(MixerNum)%InletEnthalpy(1)
      MixerCond(MixerNum)%OutletTemp       = MixerCond(MixerNum)%InletTemp(1)
   End If

   ! make sure MassFlowRateMaxAvail is >= MassFlowRate
   MixerCond(MixerNum)%OutletMassFlowRateMaxAvail = MAX(MixerCond(MixerNum)%OutletMassFlowRateMaxAvail, &
                                                        MixerCond(MixerNum)%OutletMassFlowRate)

 RETURN
END SUBROUTINE CalcAirMixer


! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the Mixer Module
! *****************************************************************************

SUBROUTINE UpdateAirMixer(MixerNum)


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
  Integer, Intent(IN) :: MixerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutletNode
  Integer             :: InletNode
  Integer             :: InletNodeNum


   OutletNode = MixerCond(MixerNum)%OutletNode
   InletNode = MixerCond(MixerNum)%InletNode(1) ! For now use first inlet node

   ! Set the outlet air nodes of the Mixer
   Node(OutletNode)%MassFlowRate  = MixerCond(MixerNum)%OutletMassFlowRate
   Node(OutletNode)%MassFlowRateMaxAvail  = MixerCond(MixerNum)%OutletMassFlowRateMaxAvail
   Node(OutletNode)%MassFlowRateMinAvail  = MixerCond(MixerNum)%OutletMassFlowRateMinAvail
   Node(OutletNode)%Temp          = MixerCond(MixerNum)%OutletTemp
   Node(OutletNode)%HumRat        = MixerCond(MixerNum)%OutletHumRat
   Node(OutletNode)%Enthalpy      = MixerCond(MixerNum)%OutletEnthalpy
   Node(OutletNode)%Press         = MixerCond(MixerNum)%OutletPressure
   ! Set the outlet nodes for properties that just pass through & not used
   Node(OutletNode)%Quality         = Node(InletNode)%Quality

  IF (Contaminant%CO2Simulation) Then
    If(MixerCond(MixerNum)%OutletMassFlowRate .gt. 0.d0) Then
      ! CO2 balance to get outlet air CO2
      Node(OutletNode)%CO2 = 0.0d0
      DO InletNodeNum = 1, MixerCond(MixerNum)%NumInletNodes
        Node(OutletNode)%CO2 = Node(OutletNode)%CO2 + Node(MixerCond(MixerNum)%InletNode(InletNodeNum))%CO2 * &
              MixerCond(MixerNum)%InletMassFlowRate(InletNodeNum) / MixerCond(MixerNum)%OutletMassFlowRate
      END DO
    Else
      Node(OutletNode)%CO2 = Node(InletNode)%CO2
    End If
  End If

  IF (Contaminant%GenericContamSimulation) Then
    If(MixerCond(MixerNum)%OutletMassFlowRate .gt. 0.d0) Then
      ! Generic contaminant balance to get outlet air CO2
      Node(OutletNode)%GenContam = 0.0d0
      DO InletNodeNum = 1, MixerCond(MixerNum)%NumInletNodes
        Node(OutletNode)%GenContam = Node(OutletNode)%GenContam + Node(MixerCond(MixerNum)%InletNode(InletNodeNum))%GenContam * &
              MixerCond(MixerNum)%InletMassFlowRate(InletNodeNum) / MixerCond(MixerNum)%OutletMassFlowRate
      END DO
    Else
      Node(OutletNode)%GenContam = Node(InletNode)%GenContam
    End If
  End If

  RETURN
END Subroutine UpdateAirMixer


!        End of Update subroutines for the Mixer Module
! *****************************************************************************


! Beginning of Reporting subroutines for the Mixer Module
! *****************************************************************************

SUBROUTINE ReportMixer(MixerNum)


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
  Integer, Intent(IN) :: MixerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

 ! Write(*,*)=MixerCond(MixerNum)%MixerPower    Still needs to report the Mixer power from this component


  RETURN
END Subroutine ReportMixer

!        End of Reporting subroutines for the Mixer Module
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

End Module MixerComponent

