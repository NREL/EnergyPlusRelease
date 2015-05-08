MODULE ReturnAirPathManager
  ! Module containing the routines dealing with the AirLoopHVAC:ReturnPath (formerly Return Air Path)

  ! MODULE INFORMATION:
  !       AUTHOR         Russ Taylor
  !       DATE WRITTEN   January 1998
  !       MODIFIED       Lawrie, September 1999 -- consolidate ReturnAirPath data structure
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To manage the return air path.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
  USE DataPrecisionGlobals
  USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength
  USE DataInterfaces,  ONLY: ShowSevereError, ShowFatalError, ShowContinueError
  USE DataZoneEquipment, ONLY: ReturnAirPath, NumReturnAirPaths, ZoneMixer_Type, ZoneReturnPlenum_Type

  ! Use statements for access to subroutines in other modules


  IMPLICIT NONE

  PRIVATE

  !MODULE PARAMETER DEFINITIONS
  ! na

  !DERIVED TYPE DEFINITIONS
  ! na

  !MODULE VARIABLE DECLARATIONS:
  ! na

  !SUBROUTINE SPECIFICATIONS FOR MODULE ReturnAirPathManager

  PUBLIC SimReturnAirPath

  CONTAINS

  SUBROUTINE SimReturnAirPath

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Russ Taylor
            !       DATE WRITTEN:    Nov 1997

            ! PURPOSE OF THIS SUBROUTINE: This subroutine

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:

    IMPLICIT NONE


    INTEGER :: ReturnAirPathNum
    Logical,Save :: GetInputFlag = .True.  ! Flag set to make sure you get input once

    ! Obtains and Allocates Mixer related parameters from input file
    IF (GetInputFlag) THEN  !First time subroutine has been entered
      CALL GetReturnAirPathInput
      GetInputFlag = .FALSE.
    END IF


    DO ReturnAirPathNum=1,NumReturnAirPaths

      CALL CalcReturnAirPath(ReturnAirPathNum)

    END DO

  END SUBROUTINE SimReturnAirPath

  SUBROUTINE GetReturnAirPathInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Russ Taylor
            !       DATE WRITTEN:    Nov 1997

            ! PURPOSE OF THIS SUBROUTINE: This subroutine

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:

    USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,VerifyName, SameString
    USE DataIPShortCuts
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE DataLoopNode

    IMPLICIT NONE

    INTEGER :: PathNum
    INTEGER :: CompNum
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    INTEGER :: Counter
    LOGICAL :: ErrorsFound=.false.
    LOGICAL       :: IsNotOK               ! Flag to verify name
    LOGICAL       :: IsBlank               ! Flag for blank name

    IF (ALLOCATED(ReturnAirPath)) THEN
      RETURN
    ENDIF
    cCurrentModuleObject = 'AirLoopHVAC:ReturnPath'
    NumReturnAirPaths = GetNumObjectsFound(cCurrentModuleObject)

    IF (NumReturnAirPaths > 0) THEN

      ALLOCATE(ReturnAirPath(NumReturnAirPaths))

      DO PathNum = 1,  NumReturnAirPaths

        CALL GetObjectItem(cCurrentModuleObject,PathNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),ReturnAirPath%Name,PathNum-1,IsNotOK,IsBlank,&
                               TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        ReturnAirPath(PathNum)%Name   = cAlphaArgs(1)
        ReturnAirPath(PathNum)%NumOfComponents = NINT((NumAlphas - 2.0d0) / 2.0d0)

        ReturnAirPath(PathNum)%OutletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

        ALLOCATE(ReturnAirPath(PathNum)%ComponentType(ReturnAirPath(PathNum)%NumOfComponents))
        ReturnAirPath(PathNum)%ComponentType=' '
        ALLOCATE(ReturnAirPath(PathNum)%ComponentType_Num(ReturnAirPath(PathNum)%NumOfComponents))
        ReturnAirPath(PathNum)%ComponentType_Num=0
        ALLOCATE(ReturnAirPath(PathNum)%ComponentName(ReturnAirPath(PathNum)%NumOfComponents))
        ReturnAirPath(PathNum)%ComponentName=' '
        ALLOCATE(ReturnAirPath(PathNum)%ComponentIndex(ReturnAirPath(PathNum)%NumOfComponents))
        ReturnAirPath(PathNum)%ComponentIndex=0
        Counter=3

        DO CompNum = 1, ReturnAirPath(PathNum)%NumOfComponents

          IF ( ( SameString(cAlphaArgs(Counter) , 'AirLoopHVAC:ZoneMixer')) .OR. &
               ( SameString(cAlphaArgs(Counter) , 'AirLoopHVAC:ReturnPlenum')) ) THEN

            ReturnAirPath(PathNum)%ComponentType(CompNum) = cAlphaArgs(Counter)
            ReturnAirPath(PathNum)%ComponentName(CompNum) = cAlphaArgs(Counter+1)
            CALL ValidateComponent(ReturnAirPath(PathNum)%ComponentType(CompNum),  &
                                   ReturnAirPath(PathNum)%ComponentName(CompNum),  &
                                   IsNotOK,'AirLoopHVAC:ReturnPath')
            IF (IsNotOK) THEN
              CALL ShowContinueError('In AirLoopHVAC:ReturnPath ='//TRIM(ReturnAirPath(PathNum)%Name))
              ErrorsFound=.true.
            ENDIF
            IF ( SameString(cAlphaArgs(Counter) , 'AirLoopHVAC:ZoneMixer')) &
                              ReturnAirPath(PathNum)%ComponentType_Num(CompNum)=ZoneMixer_Type
            IF (SameString(cAlphaArgs(Counter) , 'AirLoopHVAC:ReturnPlenum')) &
                              ReturnAirPath(PathNum)%ComponentType_Num(CompNum)=ZoneReturnPlenum_Type
          ELSE
            CALL ShowSevereError('Unhandled component type in AirLoopHVAC:ReturnPath of '//TRIM(cAlphaArgs(Counter)))
            CALL ShowContinueError('Occurs in AirLoopHVAC:ReturnPath = '//TRIM(ReturnAirPath(PathNum)%Name))
            CALL ShowContinueError('Must be "AirLoopHVAC:ZoneMixer" or "AirLoopHVAC:ReturnPlenum"')
            ErrorsFound = .TRUE.
          ENDIF

          Counter=Counter+2

        END DO

      END DO

    ENDIF

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found getting AirLoopHVAC:ReturnPath.  Preceding condition(s) causes termination.')
    ENDIF

    RETURN
  END SUBROUTINE GetReturnAirPathInput

  SUBROUTINE InitReturnAirPath(ReturnAirPathNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Russ Taylor
            !       DATE WRITTEN:    Nov 1997

            ! PURPOSE OF THIS SUBROUTINE: This subroutine

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:

    IMPLICIT NONE

    INTEGER :: ReturnAirPathNum !unused1208

    RETURN
  END SUBROUTINE InitReturnAirPath

  SUBROUTINE CalcReturnAirPath(ReturnAirPathNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Russ Taylor
            !       DATE WRITTEN:    Nov 1997

            ! PURPOSE OF THIS SUBROUTINE: This subroutine

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:

    USE MixerComponent, ONLY: SimAirMixer
    USE ZonePlenum, ONLY: SimAirZonePlenum
    USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkFanActivated,AirflowNetworkControlMultizone
    USE DataHVACGlobals, ONLY: TurnFansOn

    IMPLICIT NONE

    INTEGER :: ReturnAirPathNum
    INTEGER :: ComponentNum

      DO ComponentNum = 1, ReturnAirPath(ReturnAirPathNum)%NumOfComponents

        SELECT CASE (ReturnAirPath(ReturnAirPathNum)%ComponentType_Num(ComponentNum))

        CASE(ZoneMixer_Type) ! 'AirLoopHVAC:ZoneMixer'

          if (.NOT. (AirflowNetworkFanActivated .AND. SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone)) then
            CALL SimAirMixer(ReturnAirPath(ReturnAirPathNum)%ComponentName(ComponentNum),  &
                             CompIndex=ReturnAirPath(ReturnAirPathNum)%ComponentIndex(ComponentNum))
          endif

        CASE(ZoneReturnPlenum_Type) ! 'AirLoopHVAC:ReturnPlenum'

          CALL SimAirZonePlenum(ReturnAirPath(ReturnAirPathNum)%ComponentName(ComponentNum),ZoneReturnPlenum_Type,  &
                                ReturnAirPath(ReturnAirPathNum)%ComponentIndex(ComponentNum))

        CASE DEFAULT
          CALL ShowSevereError('Invalid AirLoopHVAC:ReturnPath Component='//  &
                              TRIM(ReturnAirPath(ReturnAirPathNum)%ComponentType(ComponentNum)))
          CALL ShowContinueError('Occurs in AirLoopHVAC:ReturnPath ='//TRIM(ReturnAirPath(ReturnAirPathNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')

        END SELECT

      END DO

    RETURN
  END SUBROUTINE CalcReturnAirPath

  SUBROUTINE ReportReturnAirPath(ReturnAirPathNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Russ Taylor
            !       DATE WRITTEN:    Nov 1997

            ! PURPOSE OF THIS SUBROUTINE: This subroutine

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:

    IMPLICIT NONE

    INTEGER :: ReturnAirPathNum !unused1208

    RETURN
  END SUBROUTINE ReportReturnAirPath

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

END MODULE ReturnAirPathManager
