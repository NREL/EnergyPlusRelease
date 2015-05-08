MODULE Pipes

  ! Module containing the routines dealing with the <module_name>

  ! MODULE INFORMATION:
  !       AUTHOR         <author>
  !       DATE WRITTEN   <date_written>
  !       MODIFIED       Rahul Chillar , Jan 2005
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Added steam pipe to the module: RC

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, SetupOutputVariable, ShowContinueError
USE DataHVACGlobals
USE DataLoopNode
USE DataPlant,    ONLY : TypeOf_Pipe,TypeOf_PipeSteam

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  TYPE LocalPipeData
    CHARACTER(len=MaxNameLength)    :: Name=' '        ! main plant (cooling) loop ID
    INTEGER                         :: TypeOf=0        ! type of pipe
    INTEGER                         :: InletNodeNum=0  ! Node number on the inlet side of the plant
    INTEGER                         :: OutletNodeNum=0 ! Node number on the inlet side of the plant
    INTEGER                         :: LoopNum=0       ! Index of plant loop where this pipe resides
    INTEGER                         :: LoopSide=0      ! Index of plant loop side where this pipe resides
    INTEGER                         :: BranchIndex=0   ! Index of plant Branch index where this pipe resides
    INTEGER                         :: CompIndex=0     ! Index of plant Comp index where this pipe resides
    LOGICAL                         :: OneTimeInit = .TRUE.
    LOGICAL                         :: CheckEquipName = .TRUE.
    LOGICAL                         :: EnvrnFlag   = .TRUE.
  END TYPE LocalPipeData


  ! MODULE VARIABLE DECLARATIONS:

 INTEGER  :: NumLocalPipes=0
 LOGICAL  :: GetPipeInputFlag = .TRUE.
 TYPE (LocalPipeData), ALLOCATABLE, DIMENSION(:)  :: LocalPipe         !dimension to number of pipes

          ! SUBROUTINE SPECIFICATIONS FOR MODULE Pipe
PUBLIC    SimPipes
PRIVATE   GetPipeInput
PUBLIC    InitializePipes


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Plant Loop Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimPipes(CompType,PipeName,CompIndex, MaxVolFlowRate,InitLoopEquip,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, SameString
  USE General,        ONLY: TrimSigDigits
  USE DataPlant,      ONLY: PlantLoop, ScanPlantLoopsForObject
  USE PlantUtilities, ONLY: SafeCopyPlantNode, InitComponentNodes
  USE DataGlobals,    ONLY: BeginEnvrnFlag

  IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: CompType
  CHARACTER(len=*)      :: PipeName
  INTEGER,INTENT(INOUT) :: CompIndex
  LOGICAL, INTENT(IN)   :: FirstHVACIteration
  REAL(r64), INTENT(IN) :: MaxVolFlowRate
  LOGICAL, INTENT(IN)   :: InitLoopEquip

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PipeNum
!  INTEGER :: LoopNum
!  INTEGER :: LoopSideNum
!  INTEGER :: BranchNum
!  INTEGER :: CompNum
  INTEGER :: FoundOnLoop
  LOGICAL :: errFlag

          !FLOW

          !GET INPUT
  IF (GetPipeInputFlag) THEN
    CALL GetPipeInput
    GetPipeInputFlag = .FALSE.
  END IF

  IF (CompIndex == 0) THEN
    PipeNum=FindItemInList(PipeName,LocalPipe%Name,NumLocalPipes)
    IF (PipeNum == 0) THEN
      CALL ShowFatalError('SimPipes: Pipe requested not found ='//TRIM(PipeName)) ! Catch any bad names before crashing
    ENDIF
    CompIndex=PipeNum
  ELSE
    PipeNum=CompIndex
    IF (PipeNum > NumLocalPipes .or. PipeNum < 1) THEN
      CALL ShowFatalError('SimPipes: Invalid CompIndex passed='//TRIM(TrimSigDigits(PipeNum))// &
                          ', Number of Pipes='//TRIM(TrimSigDigits(NumLocalPipes))//', Pipe name='//TRIM(PipeName))
    ENDIF
    IF (LocalPipe(PipeNum)%CheckEquipName) THEN
      IF (PipeName /= LocalPipe(PipeNum)%Name) THEN
        CALL ShowFatalError('SimPipes: Invalid CompIndex passed='//TRIM(TrimSigDigits(PipeNum))// &
                           ', Pipe name='//TRIM(PipeName)//', stored Pipe Name for that index='//  &
                            TRIM(LocalPipe(PipeNum)%Name))
      ENDIF
      LocalPipe(PipeNum)%CheckEquipName=.false.
    ENDIF
  ENDIF

  IF(LocalPipe(PipeNum)%OneTimeInit) THEN
    FoundOnLoop = 0
    errFlag=.false.
    CALL ScanPlantLoopsForObject(LocalPipe(PipeNum)%Name, &
                                 CompType, &
                                 LocalPipe(PipeNum)%LoopNum, &
                                 LocalPipe(PipeNum)%LoopSide, &
                                 LocalPipe(PipeNum)%BranchIndex, &
                                 LocalPipe(PipeNum)%CompIndex, &
                                 CountMatchPlantLoops = FoundOnLoop,  &
                                 errFlag=errFlag)
    IF (FoundOnLoop == 0) THEN
      CALL ShowFatalError('SimPipes: Pipe="'//trim(PipeName)//'" not found on a Plant Loop.')
    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('SimPipes: Program terminated due to previous condition(s).')
    ENDIF
    LocalPipe(PipeNum)%OneTimeInit=.FALSE.
  END IF

  IF (BeginEnvrnFlag .AND. LocalPipe(PipeNum)%EnvrnFlag) THEN
    CALL InitComponentNodes( 0.d0, PlantLoop(LocalPipe(PipeNum)%LoopNum)%MaxMassFlowRate, &
                                 LocalPipe(PipeNum)%InletNodeNum, &
                                 LocalPipe(PipeNum)%OutletNodeNum, &
                                 LocalPipe(PipeNum)%LoopNum, &
                                 LocalPipe(PipeNum)%LoopSide, &
                                 LocalPipe(PipeNum)%BranchIndex, &
                                 LocalPipe(PipeNum)%CompIndex)
    LocalPipe(PipeNum)%EnvrnFlag = .FALSE.
  ENDIF

  IF (.NOT. BeginEnvrnFlag) LocalPipe(PipeNum)%EnvrnFlag = .TRUE.

  CALL SafeCopyPlantNode(LocalPipe(PipeNum)%InletNodeNum, &
                         LocalPipe(PipeNum)%OutletNodeNum, LocalPipe(PipeNum)%LoopNum )
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%FluidType            = Node(LocalPipe(PipeNum)%InletNodeNum)%FluidType
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Temp                 = Node(LocalPipe(PipeNum)%InletNodeNum)%Temp
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMin              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMin
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMax              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMax
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRate         = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRate
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMin      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMin
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMax      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMax
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMinAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMinAvail
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMaxAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMaxAvail
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Quality              = Node(LocalPipe(PipeNum)%InletNodeNum)%Quality
!  !Only pass pressure if we aren't doing a pressure simulation
!  IF (PlantLoop(LocalPipe(PipeNum)%LoopNum)%PressureSimType > 1) THEN
!    !Don't do anything
!  ELSE
!    Node(LocalPipe(PipeNum)%OutletNodeNum)%Press              = Node(LocalPipe(PipeNum)%InletNodeNum)%Press
!  END IF
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Enthalpy             = Node(LocalPipe(PipeNum)%InletNodeNum)%Enthalpy
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%HumRat               = Node(LocalPipe(PipeNum)%InletNodeNum)%HumRat



RETURN
END SUBROUTINE SimPipes

! End Plant Loop Module Driver Subroutines
!******************************************************************************


! Beginning of Plant Loop Module Get Input subroutines
!******************************************************************************

SUBROUTINE GetPipeInput
          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    April 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  !USE DataPlant, ONLY: LoopData

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
  INTEGER                     :: PipeNum
  INTEGER                     :: NumWaterPipes
  INTEGER                     :: NumSteamPipes
  INTEGER                     :: PipeSteamNum
  INTEGER                     :: PipeWaterNum
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name

          !GET NUMBER OF ALL EQUIPMENT TYPES
  NumWaterPipes = GetNumObjectsFound('Pipe:Adiabatic')
  NumSteamPipes = GetNumObjectsFound('Pipe:Adiabatic:Steam')
  NumLocalPipes = NumWaterPipes + NumSteamPipes
  ALLOCATE(LocalPipe(NumLocalPipes))

  cCurrentModuleObject = 'Pipe:Adiabatic'
  DO PipeWaterNum = 1 , NumWaterPipes
     PipeNum=PipeWaterNum
    CALL GetObjectItem(cCurrentModuleObject,PipeWaterNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),LocalPipe%Name,PipeNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    LocalPipe(PipeNum)%Name            = cAlphaArgs(1)
    LocalPipe(PipeNum)%TypeOf          = TypeOf_Pipe

    LocalPipe(PipeNum)%InletNodeNum    = &
                GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    LocalPipe(PipeNum)%OutletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Pipe Nodes')
  END DO

  PipeNum=NumWaterPipes
  cCurrentModuleObject = 'Pipe:Adiabatic:Steam'

  DO PipeSteamNum = 1 , NumSteamPipes
    PipeNum=PipeNum+1
    CALL GetObjectItem(cCurrentModuleObject,PipeSteamNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),LocalPipe%Name,PipeNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    LocalPipe(PipeNum)%Name            = cAlphaArgs(1)
    LocalPipe(PipeNum)%TypeOf          = TypeOf_PipeSteam
    LocalPipe(PipeNum)%InletNodeNum    = &
                GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                NodeType_Steam,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    LocalPipe(PipeNum)%OutletNodeNum   = &
                 GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Steam,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Pipe Nodes')
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetPipeInput: Errors getting input for pipes')
  ENDIF

RETURN
END SUBROUTINE GetPipeInput

! End of Get Input subroutines for the Plant Loop Module
!******************************************************************************


! Beginning Initialization Section of the Plant Loop Module
!******************************************************************************

SUBROUTINE InitializePipes(PipeType,PipeName,PipeNum,MaxVolFlowRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provide an external call to initialize Pipes/index numbers.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: PipeType  ! Type of Pipe
  CHARACTER(len=*), INTENT(IN) :: PipeName  ! Name of Pipe
  INTEGER, INTENT(INOUT)       :: PipeNum   ! Index into pipe structure for name
  REAL(r64), INTENT(IN)        :: MaxVolFlowRate  ! unused at present time

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetPipeInputFlag) THEN
    CALL GetPipeInput
    GetPipeInputFlag = .FALSE.
  END IF

  IF (PipeNum == 0) THEN
    PipeNum=FindItemInList(PipeName,LocalPipe%Name,NumLocalPipes)
    IF (PipeNum == 0) THEN
      CALL ShowFatalError('SimPipes: Pipe requested not found ='//TRIM(PipeName)) ! Catch any bad names before crashing
    ENDIF
  ELSE
    IF (PipeNum > NumLocalPipes .or. PipeNum < 1) THEN
      CALL ShowFatalError('InitializePipe: Invalid PipeNum passed='//TRIM(TrimSigDigits(PipeNum))// &
                          ', Number of Pipes='//TRIM(TrimSigDigits(NumLocalPipes))//', Pipe name='//TRIM(PipeName))
    ENDIF
    IF (LocalPipe(PipeNum)%CheckEquipName) THEN
      IF (PipeName /= LocalPipe(PipeNum)%Name) THEN
        CALL ShowFatalError('InitializePipe: Invalid PipeNum passed='//TRIM(TrimSigDigits(PipeNum))// &
                            ', Pipe name='//TRIM(PipeName)//', stored Pipe Name for that index='//TRIM(LocalPipe(PipeNum)%Name))
      ENDIF
      LocalPipe(PipeNum)%CheckEquipName=.false.
    ENDIF
  ENDIF

!  Node(LocalPipe(PipeNum)%OutletNodeNum)%FluidType            = Node(LocalPipe(PipeNum)%InletNodeNum)%FluidType
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Temp                 = Node(LocalPipe(PipeNum)%InletNodeNum)%Temp
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMin              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMin
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%TempMax              = Node(LocalPipe(PipeNum)%InletNodeNum)%TempMax
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRate         = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRate
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMin      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMin
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMax      = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMax
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMinAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMinAvail
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%MassFlowRateMaxAvail = Node(LocalPipe(PipeNum)%InletNodeNum)%MassFlowRateMaxAvail
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Quality              = Node(LocalPipe(PipeNum)%InletNodeNum)%Quality
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Press                = Node(LocalPipe(PipeNum)%InletNodeNum)%Press
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%Enthalpy             = Node(LocalPipe(PipeNum)%InletNodeNum)%Enthalpy
!  Node(LocalPipe(PipeNum)%OutletNodeNum)%HumRat               = Node(LocalPipe(PipeNum)%InletNodeNum)%HumRat

  RETURN

END SUBROUTINE InitializePipes


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

END MODULE Pipes
