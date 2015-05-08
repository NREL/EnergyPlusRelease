MODULE HVACDuct

          ! Module containing the routines dealing with the Duct component
          ! in forced air air conditioning systems

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and routines required to model duct
          ! components in the EnergyPlus HVAC simulation

          ! METHODOLOGY EMPLOYED:
          ! At this point ducts are passive elements in the loop that just pass inlet node
          ! conditions to the outlet node. The function of a duct component is to allow the
          ! definition of a bypass branch: a branch must contain at least 1 component.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength, BeginEnvrnFlag
  USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, SetupOutputVariable, ShowContinueError
  USE DataHVACGlobals
  USE DataLoopNode

          ! <use statements for access to subroutines in other modules>

  IMPLICIT NONE ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE DuctData
    CHARACTER(len=MaxNameLength)           :: Name = ' '        ! duct unique name
    INTEGER                                :: InletNodeNum = 0  ! inlet node number
    INTEGER                                :: OutletNodeNum = 0 ! outlet node number
  END TYPE DuctData

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER                                    :: NumDucts = 0
  TYPE (DuctData), ALLOCATABLE, DIMENSION(:) :: Duct
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE HVACDuct:

          ! <name Public routines, optionally name Private routines within this module>

  PUBLIC  SimDuct
  PRIVATE GetDuctInput
  PRIVATE CalcDuct
  PRIVATE UpdateDuct
  PRIVATE ReportDuct

CONTAINS

SUBROUTINE SimDuct(CompName,FirstHVACIteration,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of a duct component

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY:        TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)    :: CompName            ! name of the duct component
  LOGICAL,          INTENT (IN)    :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep !unused1208
  INTEGER,          INTENT (INOUT) :: CompIndex           ! index of duct component

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE      :: GetInputFlag = .true.  ! First time, input is "gotten"
  INTEGER      :: DuctNum                     ! index of duct being simulated

  IF (GetInputFlag) THEN
    CALL GetDuctInput
    GetInputFlag=.false.
  ENDIF

  ! Get the duct component index
  IF (CompIndex == 0) THEN
    DuctNum = FindItemInList(CompName,Duct%Name,NumDucts)
    IF (DuctNum == 0) THEN
      CALL ShowFatalError('SimDuct: Component not found='//TRIM(CompName))
    ENDIF
    CompIndex=DuctNum
  ELSE
    DuctNum=CompIndex
    IF (DuctNum > NumDucts .or. DuctNum < 1) THEN
      CALL ShowFatalError('SimDuct:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DuctNum))// &
                          ', Number of Components='//TRIM(TrimSigDigits(NumDucts))//  &
                          ', Entered Component name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(DuctNum)) THEN
      IF (CompName /= Duct(DuctNum)%Name) THEN
        CALL ShowFatalError('SimDuct: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(DuctNum))// &
                            ', Component name='//TRIM(CompName)//', stored Component Name for that index='//  &
                            TRIM(Duct(DuctNum)%Name))
      ENDIF
      CheckEquipName(DuctNum)=.false.
    ENDIF
  ENDIF

  CALL InitDuct(DuctNum)

  CALL CalcDuct(DuctNum)

  CALL UpdateDuct(DuctNum)

  CALL ReportDuct(DuctNum)

  RETURN

END SUBROUTINE SimDuct

SUBROUTINE GetDuctInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for ducts and stores it in duct data structures.

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataIPShortCuts

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
  INTEGER                        :: DuctNum    ! duct index
  CHARACTER(len=*), PARAMETER    :: RoutineName='GetDuctInput:'
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: IsNotOK              ! Flag to verify name
  LOGICAL                        :: IsBlank              ! Flag for blank name

  cCurrentModuleObject='Duct'
  NumDucts = GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(Duct(NumDucts))
  ALLOCATE(CheckEquipName(NumDucts))
  CheckEquipName=.true.

  DO DuctNum=1,NumDucts
    CALL GetObjectItem(cCurrentModuleObject,DuctNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),Duct%Name,DuctNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    Duct(DuctNum)%Name = cAlphaArgs(1)
    Duct(DuctNum)%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    Duct(DuctNum)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Air Nodes')
  ENDDO

  ! No output variables

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//' Errors found in input')
  ENDIF

  RETURN

END SUBROUTINE GetDuctInput

SUBROUTINE InitDuct(DuctNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Duct Components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DuctNum ! number of the current duct being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag

  ! do one time initializations
  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumDucts))
    MyEnvrnFlag = .TRUE.

    MyOneTimeFlag = .false.

  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(DuctNum)) THEN

  END IF


  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(DuctNum)=.true.
  ENDIF

  ! do these initializations every HVAC time step

  RETURN

END SUBROUTINE InitDuct

SUBROUTINE CalcDuct(DuctNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! na

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DuctNum ! number of the current duct being simulated !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  RETURN

END SUBROUTINE CalcDuct

SUBROUTINE UpdateDuct(DuctNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Moves duct output to the outlet nodes

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT (IN) :: DuctNum ! number of the current duct being simulated

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode ! inlet node number
  INTEGER             :: OutNode ! outlet node number

  InNode =Duct(DuctNum)%InletNodeNum
  OutNode = Duct(DuctNum)%OutletNodeNum
  ! Set the outlet air node conditions of the duct
  Node(OutNode)%MassFlowRate        = Node(InNode)%MassFlowRate
  Node(OutNode)%Temp                = Node(InNode)%Temp
  Node(OutNode)%HumRat              = Node(InNode)%HumRat
  Node(OutNode)%Enthalpy            = Node(InNode)%Enthalpy
  Node(OutNode)%Quality             = Node(InNode)%Quality
  Node(OutNode)%Press               = Node(InNode)%Press
  Node(OutNode)%MassFlowRateMin     = Node(InNode)%MassFlowRateMin
  Node(OutNode)%MassFlowRateMax     = Node(InNode)%MassFlowRateMax
  Node(OutNode)%MassFlowRateMinAvail= Node(InNode)%MassFlowRateMinAvail
  Node(OutNode)%MassFlowRateMaxAvail= Node(InNode)%MassFlowRateMaxAvail

  IF (Contaminant%CO2Simulation) Then
    Node(OutNode)%CO2 = Node(InNode)%CO2
  End If

  IF (Contaminant%GenericContamSimulation) Then
    Node(OutNode)%GenContam = Node(InNode)%GenContam
  End If

  RETURN

END SUBROUTINE UpdateDuct

SUBROUTINE ReportDuct(DuctNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   17May2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill remaining report variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DuctNum ! number of the current duct being simulated !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  RETURN

END SUBROUTINE ReportDuct

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

END MODULE HVACDuct

