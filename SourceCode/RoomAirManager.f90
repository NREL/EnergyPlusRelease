MODULE RoomAirModelManager

          ! MODULE INFORMATION
          !       AUTHOR         Weixiu Kong
          !       DATE WRITTEN   March 2003
          !       MODIFIED       July 2003, CC
          !                      Aug, 2005, BG
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Contains subroutines for managing the room air models

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals !,                ONLY : MaxNameLength
    USE InputProcessor,             ONLY : SameString
    USE DataRoomAirModel
    USE DataInterfaces
    USE General, ONLY: RoundSigDigits

    IMPLICIT NONE         ! Enforce explicit typing of all variables

          ! MODULE PARAMETER DEFINITIONS
          ! na

            ! DERIVED TYPE DEFINITIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:

    LOGICAL :: GetUCSDDVDataFlag = .TRUE.  ! UCSD

          ! SUBROUTINE SPECIFICATIONS FOR MODULE
    PUBLIC ManageAirModel
    PRIVATE GetAirModelDatas
    PRIVATE GetAirNodeData
    PRIVATE GetMundtData
    PRIVATE GetDisplacementVentData   ! UCSD
    PRIVATE GetCrossVentData          !UCSD-CV
    PRIVATE GetUserDefinedPatternData !BTG Aug 2005
    PRIVATE GetUFADZoneData           ! UCSD UFAD zone data

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE ManageAirModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Weixiu Kong
          !       DATE WRITTEN   April 2003
          !       MODIFIED       July 2003, CC
          !                      Jan 2004, CC
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     manage room air models.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE MundtSimMgr,                ONLY : ManageMundtModel
    USE DisplacementVentMgr,        ONLY : ManageUCSDDVModel
    USE CrossVentMgr,               ONLY : ManageUCSDCVModel
    USE RoomAirModelUserTempPattern,  ONLY : ManageUserDefinedPatterns
    USE UFADManager,             ONLY : ManageUCSDUFModels

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     INTEGER                         :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL, SAVE  :: GetAirModelData=.true.  ! Used to "get" all air model data


          ! FLOW:
    IF (GetAirModelData) THEN
      Call GetAirModelDatas
      GetAirModelData=.false.
    ENDIF

    IF (UCSDModelUsed) THEN
      CALL SharedDVCVUFDataInit(ZoneNum)
    ENDIF

    SELECT CASE (AirModel(ZoneNum)%AirModelType)

      CASE (RoomAirModel_UserDefined)

        Call ManageUserDefinedPatterns(ZoneNum)

      CASE (RoomAirModel_Mixing) ! Mixing air model
            ! do nothing

      CASE (RoomAirModel_Mundt) ! Mundt air model
        ! simulate room airflow using Mundt model
        CALL ManageMundtModel(ZoneNum)

      CASE (RoomAirModel_UCSDDV) !UCDV Displacement Ventilation model
        ! simulate room airflow using UCSDDV model
        CALL ManageUCSDDVModel(ZoneNum)

      CASE (RoomAirModel_UCSDCV) !UCSD Cross Ventilation model
        ! simulate room airflow using UCSDDV model
        CALL ManageUCSDCVModel(ZoneNum)

      CASE(RoomAirModel_UCSDUFI) ! UCSD UFAD interior zone model
        ! simulate room airflow using the UCSDUFI model
        CALL ManageUCSDUFModels(ZoneNum,RoomAirModel_UCSDUFI)

      CASE(RoomAirModel_UCSDUFE) ! UCSD UFAD exterior zone model
        ! simulate room airflow using the UCSDUFE model
        CALL ManageUCSDUFModels(ZoneNum,RoomAirModel_UCSDUFE)

      CASE DEFAULT ! mixing air model
          ! do nothing
    END SELECT

    RETURN

END SUBROUTINE ManageAirModel

!*****************************************************************************************

SUBROUTINE GetAirModelDatas

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine "gets" all the data for the "RoomAir" models by calling individual
          ! routines.

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
  LOGICAL        :: ErrorsFound

  ErrorsFound=.false.
  ! get air node input data for all zones
  CALL GetAirNodeData(ErrorsFound)

  ! get mundt model controls for all zones
  CALL GetMundtData(ErrorsFound)

  ! get UCSDDV model controls for all zones
  CALL GetDisplacementVentData(ErrorsFound)

  ! get UCSDCV model controls for all zones
  CALL GetCrossVentData(ErrorsFound)

  ! get BTG's user-defined patterns for all zones
  CALL GetUserDefinedPatternData(ErrorsFound)

  ! get UCSD UFAD interior zone model controls for all zones
  ! get UCSD UFAD exterior zone model controls for all zones
  CALL GetUFADZoneData(ErrorsFound)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetAirModelData: Errors found getting air model input.  Program terminates.')
  ENDIF

  RETURN

END SUBROUTINE GetAirModelDatas

SUBROUTINE GetUserDefinedPatternData(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine "gets" all the data for the "User-Defined RoomAir"

          ! METHODOLOGY EMPLOYED:
          ! usual energyplus input routines
          ! for the actual patterns, a single structure array holds
          ! different patterns in nested derived types.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataGlobals,    ONLY: NumOfZones
  USE DataInterfaces, ONLY: ShowMessage
  USE InputProcessor, ONLY : FindItemInList, GetNumObjectsFound, GetObjectItem, GetObjectDefMaxArgs
  USE DataIPShortCuts
  USE DataSurfaces,   ONLY: Surface, TotSurfaces, SurfaceClass_IntMass
  USE DataHeatBalance, ONLY: Zone
  USE ScheduleManager,  ONLY : GetScheduleIndex
  USE RoomAirModelUserTempPattern, ONLY: FigureNDheightInZone
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataErrorTracking, ONLY: TotalWarningErrors, TotalRoomAirPatternTooLow, TotalRoomAirPatternTooHigh
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! True if errors found during this get input routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetUserDefinedPatternData: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                     :: NumAlphas    !number of alphas
  INTEGER                                     :: NumNumbers   ! Number of numbers encountered
  INTEGER                                     :: Status       ! Notes if there was an error in processing the input

  INTEGER         :: thisSurfinZone !working variable for indexing surfaces within a ZoneInfo structure
  INTEGER         :: thisHBsurfID !working variable for indexing surfaces in main Surface structure
  INTEGER         :: thisPattern

!unused1208  INTEGER         :: void  ! unused integer needed for parameter in subroutine call
!unused1208  INTEGER         :: MaxAlphaCount !max number of alphas in a type of extensible object
!unused1208  INTEGER         :: MaxNumCount !Max number of Numbers in a type of extensible object

  INTEGER         :: i ! do loop indexer
  INTEGER         :: numPairs ! number of zeta/deltaTai pairs
  INTEGER         :: ObjNum ! loop indexer of input objects if the same type
  INTEGER         :: ZoneNum ! zone number in heat balance domain
  INTEGER         :: found !test for FindItemInList

    !access input file and setup
  numTempDistContrldZones = GetNumObjectsFound(cUserDefinedControlObject)

  NumConstantGradient     = GetNumObjectsFound(cTempPatternConstGradientObject)
  NumTwoGradientInterp    = GetNumObjectsFound(cTempPatternTwoGradientObject)
  NumNonDimensionalHeight = GetNumObjectsFound(cTempPatternNDHeightObject)
  NumSurfaceMapping       = GetNumObjectsFound(cTempPatternSurfMapObject)

  NumAirTempPatterns = NumConstantGradient + NumTwoGradientInterp +  NumNonDimensionalHeight + NumSurfaceMapping

  cCurrentModuleObject = cUserDefinedControlObject
  IF (numTempDistContrldZones == 0) THEN
     IF (NumAirTempPatterns /= 0) THEN ! user may have missed control object
        Call ShowWarningError('Missing '//TRIM(cCurrentModuleObject)//' object needed to use roomair temperature patterns')
       ! ErrorsFound = .true.
     ENDIF
   RETURN
  ENDIF

  ! now allocate AirPatternZoneInfo to length of all zones for easy indexing
  IF (.not. Allocated(AirPatternZoneInfo)) THEN
       ALLOCATE(AirPatternZoneInfo(NumOfZones))
  ENDIF


  DO ObjNum = 1, numTempDistContrldZones

    CALL GetObjectItem(cCurrentModuleObject,ObjNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, &
                             NumericFieldNames=cNumericFieldNames)
     !first get zone ID
    ZoneNum = 0
    ZoneNum = FindItemInList(cAlphaArgs(2), zone%name, NumOfZones)
    IF (ZoneNum == 0 ) THEN   !throw error
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid data.')
      CALL ShowContinueError('Invalid-not found '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".' )
       ErrorsFound = .TRUE.
       RETURN ! halt to avoid hard crash
    ENDIF
    AirPatternZoneInfo(ZoneNum)%IsUsed         = .TRUE.
    AirPatternZoneInfo(ZoneNum)%Name           = cAlphaArgs(1)  !Name of this Control Object
    AirPatternZoneInfo(ZoneNum)%ZoneName       = cAlphaArgs(2) ! Zone Name

    AirPatternZoneInfo(ZoneNum)%AvailSched     = cAlphaArgs(3)
    IF (lAlphaFieldBlanks(3)) THEN
      AirPatternZoneInfo(ZoneNum)%AvailSchedID   = ScheduleAlwaysOn
    ELSE
      AirPatternZoneInfo(ZoneNum)%AvailSchedID   = GetScheduleIndex(cAlphaArgs(3) )
      IF (AirPatternZoneInfo(ZoneNum)%AvailSchedID == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid data.')
        CALL ShowContinueError('Invalid-not found '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
      END IF
    ENDIF

    AirPatternZoneInfo(ZoneNum)%PatternCntrlSched = cAlphaArgs(4) ! Schedule Name for Leading Pattern Control for this Zone
    AirPatternZoneInfo(ZoneNum)%PatternSchedID    = GetScheduleIndex(cAlphaArgs(4) )
    IF (AirPatternZoneInfo(ZoneNum)%PatternSchedID == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid data.')
      CALL ShowContinueError('Invalid-not found '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".' )
      ErrorsFound = .TRUE.
    END IF

    AirPatternZoneInfo(ZoneNum)%ZoneID            = ZoneNum

    !   figure number of surfaces for this zone
    AirPatternZoneInfo(ZoneNum)%totNumSurfs       = Zone(ZoneNum)%SurfaceLast &
                                                         - Zone(ZoneNum)%SurfaceFirst + 1
    !   allocate nested derived type for surface info
    ALLOCATE(AirPatternZoneInfo(ZoneNum)%Surf(AirPatternZoneInfo(ZoneNum)%totNumSurfs ) )

    !   Fill in what we know for nested structure for surfaces
    DO thisSurfinZone = 1, AirPatternZoneInfo(ZoneNum)%totNumSurfs
        thisHBsurfID = Zone(ZoneNum)%SurfaceFirst + thisSurfinZone - 1
        IF (Surface(thisHBsurfID)%Class == SurfaceClass_IntMass) THEN
          AirPatternZoneInfo(ZoneNum)%Surf(thisSurfinZone)%SurfID = thisHBsurfID
          AirPatternZoneInfo(ZoneNum)%Surf(thisSurfinZone)%Zeta   = 0.5d0
          CYCLE
        ENDIF

        AirPatternZoneInfo(ZoneNum)%Surf(thisSurfinZone)%SurfID = thisHBsurfID

        AirPatternZoneInfo(ZoneNum)%Surf(thisSurfinZone)%Zeta   = FigureNDheightInZone(thisHBsurfID)

    ENDDO !loop through surfaces in this zone

  ENDDO ! loop through number of 'RoomAir:TemperaturePattern:UserDefined' objects

  ! Check against AirModel.  Make sure there is a match here.
  DO ZoneNum=1,NumOfZones
    IF (AirModel(ZoneNum)%AirModelType /= RoomAirModel_UserDefined) CYCLE
    IF (AirPatternZoneInfo(ZoneNum)%IsUsed) CYCLE  ! There is a Room Air Temperatures object for this zone
    CALL ShowSevereError(RoutineName//'AirModel for Zone=['//TRIM(Zone(ZoneNum)%Name)//'] is indicated as "User Defined".')
    CALL ShowContinueError('...but missing a '//TRIM(cCurrentModuleObject)//' object for control.')
    ErrorsFound=.true.
  ENDDO

    ! now get user defined temperature patterns
  IF (.not. ALLOCATED( RoomAirPattern)) THEN
       ALLOCATE( RoomAirPattern(NumAirTempPatterns))
  ENDIF

  ! Four different objects to get
  cCurrentModuleObject = cTempPatternConstGradientObject
  DO  ObjNum= 1, NumConstantGradient
    thisPattern = ObjNum
    CALL GetObjectItem(cCurrentModuleObject,ObjNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)

    RoomAirPattern(thisPattern)%Name               = cAlphaArgs(1)
    RoomAirPattern(thisPattern)%PatrnID            = rNumericArgs(1)
    RoomAirPattern(thisPattern)%PatternMode        = ConstGradTempPattern
    RoomAirPattern(thisPattern)%DeltaTstat         = rNumericArgs(2)
    RoomAirPattern(thisPattern)%DeltaTleaving      = rNumericArgs(3)
    RoomAirPattern(thisPattern)%DeltaTexhaust      = rNumericArgs(4)
    RoomAirPattern(thisPattern)%GradPatrn%Gradient = rNumericArgs(5)

  ENDDO

  cCurrentModuleObject = cTempPatternTwoGradientObject
  DO ObjNum = 1,  NumTwoGradientInterp
    thisPattern = NumConstantGradient + ObjNum
    CALL GetObjectItem(cCurrentModuleObject,ObjNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
    RoomAirPattern(thisPattern)%PatternMode = TwoGradInterpPattern
    RoomAirPattern(thisPattern)%Name        = cAlphaArgs(1)
    RoomAirPattern(thisPattern)%PatrnID     = rNumericArgs(1)
    RoomAirPattern(thisPattern)%TwoGradPatrn%TstatHeight    = rNumericArgs(2)
    RoomAirPattern(thisPattern)%TwoGradPatrn%TleavingHeight = rNumericArgs(3)
    RoomAirPattern(thisPattern)%TwoGradPatrn%TexhaustHeight = rNumericArgs(4)
    RoomAirPattern(thisPattern)%TwoGradPatrn%LowGradient    = rNumericArgs(5)
    RoomAirPattern(thisPattern)%TwoGradPatrn%HiGradient     = rNumericArgs(6)



    If (SameString(cAlphaArgs(2), 'OutdoorDryBulbTemperature')) THEN
          RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode = OutdoorDrybulbMode
    ELSEIF (SameString(cAlphaArgs(2), 'ZoneDryBulbTemperature')) THEN
          RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode = ZoneAirTempMode
    ELSEIF (SameString(cAlphaArgs(2), 'ZoneAndOutdoorTemperatureDifference')) THEN
          RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode = DeltaOutdoorZone
    ELSEIF (SameString(cAlphaArgs(2), 'SensibleCoolingLoad')) THEN
          RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode = SensibleCoolingMode
    ELSEIF (SameString(cAlphaArgs(2), 'SensibleHeatingLoad')) THEN
          RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode = SensibleHeatingMode
    ELSE
       CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
       ErrorsFound = .TRUE.
    ENDIF


    RoomAirPattern(thisPattern)%TwoGradPatrn%UpperBoundTempScale     = rNumericArgs(7)
    RoomAirPattern(thisPattern)%TwoGradPatrn%LowerBoundTempScale     = rNumericArgs(8)

    RoomAirPattern(thisPattern)%TwoGradPatrn%UpperBoundHeatRateScale = rNumericArgs(9)
    RoomAirPattern(thisPattern)%TwoGradPatrn%LowerBoundHeatRateScale = rNumericArgs(10)

    ! now test the input some
    IF (RoomAirPattern(thisPattern)%TwoGradPatrn%HiGradient    &
        == RoomAirPattern(thisPattern)%TwoGradPatrn%LowGradient ) THEN
       CALL ShowWarningError('Upper and lower gradients equal, use '//TRIM(cTempPatternConstGradientObject)//' instead ')
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
    ENDIF
    IF (  (RoomAirPattern(thisPattern)%TwoGradPatrn%UpperBoundTempScale                          &
        == RoomAirPattern(thisPattern)%TwoGradPatrn%LowerBoundTempScale )                     &
        .and. (                                                                               &
             (RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode == OutdoorDrybulbMode)    &
        .or. (RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode ==  ZoneAirTempMode) &
        .or. (RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode == DeltaOutdoorZone) &
               )     ) THEN
      ! throw error, will cause divide by zero when used for scaling
      CALL ShowSevereError('Error in temperature scale in '//TRIM(cCurrentModuleObject)//': '//trim(cAlphaArgs(1)) )
      errorsfound = .true.
    ENDIF
    IF (  (RoomAirPattern(thisPattern)%TwoGradPatrn%HiGradient                                   &
        == RoomAirPattern(thisPattern)%TwoGradPatrn%LowGradient )                                &
        .and. (                                                                                  &
             (RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode == SensibleCoolingMode) &
        .or. (RoomAirPattern(thisPattern)%TwoGradPatrn%InterpolationMode == SensibleHeatingMode) &
              ) )THEN
      ! throw error, will cause divide by zero when used for scaling
      CALL ShowSevereError('Error in load scale in '//TRIM(cCurrentModuleObject)//': '//trim(cAlphaArgs(1)) )
      errorsfound = .true.
    ENDIF

  ENDDO

  cCurrentModuleObject =   cTempPatternNDHeightObject
  DO ObjNum = 1, NumNonDimensionalHeight
     thisPattern = NumConstantGradient+NumTwoGradientInterp + ObjNum
     RoomAirPattern(thisPattern)%PatternMode =NonDimenHeightPattern

     CALL GetObjectItem(cCurrentModuleObject,ObjNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
     RoomAirPattern(thisPattern)%Name       = cAlphaArgs(1)
     RoomAirPattern(thisPattern)%PatrnID    = rNumericArgs(1)
     RoomAirPattern(thisPattern)%DeltaTstat = rNumericArgs(2)
     RoomAirPattern(thisPattern)%DeltaTleaving = rNumericArgs(3)
     RoomAirPattern(thisPattern)%DeltaTexhaust = rNumericArgs(4)

     NumPairs = floor( (REAL(NumNumbers,r64) - 4.0d0)/2.0d0 )

      ! TODO error checking

     ALLOCATE( RoomAirPattern(thisPattern)%VertPatrn%ZetaPatrn(NumPairs)     )
     ALLOCATE( RoomAirPattern(thisPattern)%VertPatrn%DeltaTaiPatrn(NumPairs) )

     ! init these since they can't be in derived type
     RoomAirPattern(thisPattern)%VertPatrn%ZetaPatrn     = 0.0d0
     RoomAirPattern(thisPattern)%VertPatrn%DeltaTaiPatrn = 0.0d0

     DO i=0, NumPairs-1

        RoomAirPattern(thisPattern)%VertPatrn%ZetaPatrn(i+1)     = rNumericArgs(2*i + 5)
        RoomAirPattern(thisPattern)%VertPatrn%DeltaTaiPatrn(i+1) = rNumericArgs(2*i + 6)

     ENDDO

     !TODO  check order (TODO sort ? )
     DO i=2, NumPairs
        If (RoomAirPattern(thisPattern)%VertPatrn%ZetaPatrn(i)   &
            < RoomAirPattern(thisPattern)%VertPatrn%ZetaPatrn(i-1) ) Then
          Call ShowSevereError('Zeta values not in increasing order in '//TRIM(cCurrentModuleObject)//': '// &
                                              trim( cAlphaArgs(1)) )
          ErrorsFound = .TRUE.
        ENDIF
     ENDDO
  ENDDO

  cCurrentModuleObject = cTempPatternSurfMapObject
  DO ObjNum = 1,NumSurfaceMapping
     thisPattern = NumConstantGradient+NumTwoGradientInterp+NumNonDimensionalHeight+ObjNum
     RoomAirPattern(thisPattern)%PatternMode = SurfMapTempPattern

     CALL GetObjectItem(cCurrentModuleObject,ObjNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
     RoomAirPattern(thisPattern)%Name       = cAlphaArgs(1)
     RoomAirPattern(thisPattern)%PatrnID    = rNumericArgs(1)
     RoomAirPattern(thisPattern)%DeltaTstat = rNumericArgs(2)
     RoomAirPattern(thisPattern)%DeltaTleaving = rNumericArgs(3)
     RoomAirPattern(thisPattern)%DeltaTexhaust = rNumericArgs(4)

     NumPairs = NumNumbers - 4

     IF (NumPairs /= (NumAlphas - 1) ) THEN
       CALL ShowSevereError('Error in number of entries in '//TRIM(cCurrentModuleObject)//' object: '//trim(cAlphaArgs(1)) )
       errorsfound = .true.
     ENDIF
     ALLOCATE(RoomAirPattern(thisPattern)%MapPatrn%SurfName(NumPairs))
     ALLOCATE(RoomAirPattern(thisPattern)%MapPatrn%DeltaTai(NumPairs))
     ALLOCATE(RoomAirPattern(thisPattern)%MapPatrn%SurfID(NumPairs))

     ! init just allocated
     RoomAirPattern(thisPattern)%MapPatrn%SurfName = ' '
     RoomAirPattern(thisPattern)%MapPatrn%DeltaTai = 0.0d0
     RoomAirPattern(thisPattern)%MapPatrn%SurfID   = 0

     DO I=1, numPairs
        RoomAirPattern(thisPattern)%MapPatrn%SurfName(I)  = cAlphaArgs(I+1)
        RoomAirPattern(thisPattern)%MapPatrn%DeltaTai(I)  = rNumericArgs(I+4)
        found = FindItemInList(cAlphaArgs(I+1), Surface%Name, TotSurfaces)
        IF (found /= 0) THEN
          RoomAirPattern(thisPattern)%MapPatrn%SurfID(I) = found
        ELSE
          CALL ShowSevereError('Surface name not found in '//TRIM(cCurrentModuleObject)//' object: '//trim(cAlphaArgs(1)) )
          errorsfound = .true.
        ENDIF

     ENDDO
     RoomAirPattern(thisPattern)%MapPatrn%numSurfs = numPairs


  ENDDO

  IF (TotalRoomAirPatternTooLow > 0) THEN
    call ShowWarningError('GetUserDefinedPatternData: RoomAirModelUserTempPattern: '//  &
       trim(RoundSigDigits(TotalRoomAirPatternTooLow))//  &
       ' problem(s) in non-dimensional height calculations, '//  &
       'too low surface height(s) in relation to floor height of zone(s).')
    call ShowContinueError('...Use OutputDiagnostics,DisplayExtraWarnings; to see details.')
    TotalWarningErrors=TotalWarningErrors+TotalRoomAirPatternTooLow
  ENDIF
  IF (TotalRoomAirPatternTooHigh > 0) THEN
    call ShowWarningError('GetUserDefinedPatternData: RoomAirModelUserTempPattern: '//  &
       trim(RoundSigDigits(TotalRoomAirPatternTooHigh))//  &
       ' problem(s) in non-dimensional height calculations, '//  &
       'too high surface height(s) in relation to ceiling height of zone(s).')
    call ShowContinueError('...Use OutputDiagnostics,DisplayExtraWarnings; to see details.')
    TotalWarningErrors=TotalWarningErrors+TotalRoomAirPatternTooHigh
  ENDIF

  ! now do one time setups from and checks on user data

  ! Find and set return and exhaust node ids

  DO I = 1, NumOfZones
    IF  (AirPatternZoneInfo(I)%IsUsed) THEN
      ! first get return and exhaust air node index
      found = FindItemInList(AirPatternZoneInfo(I)%ZoneName, ZoneEquipConfig%ZoneName, NumOfZones)
      IF (found /= 0) then

            AirPatternZoneInfo(I)%ReturnAirNodeID = ZoneEquipConfig(found)%ReturnAirNode
            AirPatternZoneInfo(I)%ZoneNodeID      = ZoneEquipConfig(found)%ZoneNode
            If (Allocated(ZoneEquipConfig(found)%ExhaustNode)) then
               Allocate(AirPatternZoneInfo(I)%ExhaustAirNodeID(ZoneEquipConfig(found)%NumExhaustNodes))
               AirPatternZoneInfo(I)%ExhaustAirNodeID = ZoneEquipConfig(found)%ExhaustNode
            ENDIF !exhaust nodes present
      ENDIF !found ZoneEquipConf

      ! second get zone height values
      AirPatternZoneInfo(I)%ZoneHeight = Zone(I)%CeilingHeight

    ENDIF !air pattern is used
  ENDDO

  RETURN


END SUBROUTINE GetUserDefinedPatternData


SUBROUTINE GetAirNodeData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2001
          !       RE-ENGINEERED  April 2003, Weixiu Kong
          !       MODIFIED       July 2003, CC
          !                      Jan 2004, CC

          ! PURPOSE OF THIS SUBROUTINE:
          !     Get AirNode data for all zones at once

          ! METHODOLOGY EMPLOYED:
          !     Use input processer to get input from idf file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,             ONLY : GetNumObjectsFound, GetObjectItem,GetObjectItemNum,FindIteminList,VerifyName
    USE DataIPShortCuts
    USE DataHeatBalance,            ONLY : Zone
    USE DataSurfaces,               ONLY : Surface

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    LOGICAL, INTENT(INOUT) :: ErrorsFound  ! True if errors found during this get input routine

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                                     :: NumAlphas    ! States which alpha value to read from a
                                                                ! "Number" line
    INTEGER                                     :: NumNumbers   ! Number of numbers encountered
    INTEGER                                     :: Status       ! Notes if there was an error in processing the input
    INTEGER                                     :: AirNodeNum   ! Index number for air nodes
    INTEGER                                     :: ZoneNum      ! Index number for zones
    INTEGER                                     :: NumSurfsInvolved ! Number of surfaces involved with air nodes
    INTEGER                                     :: SurfCount    ! Number of surfaces involved with air nodes
                                                                ! (used for checking error)
    INTEGER                                     :: SurfNum      ! Index number for surfaces
    INTEGER                                     :: SurfFirst    ! Index number for first surface of zones
    INTEGER                                     :: NumOfSurfs   ! Index number for last surface of zones
    INTEGER                                     :: ListSurfNum  ! Index number of surfaces listed in the air node object
    LOGICAL                                     :: SurfNeeded
    LOGICAL :: IsNotOK
    LOGICAL :: IsBlank

          ! FLOW:

    IF (.not. MundtModelUsed) RETURN

    ! Initialize default values for air nodes
    ALLOCATE (TotNumOfZoneAirNodes(NumOfZones))
    TotNumOfAirNodes        = 0
    TotNumOfZoneAirNodes    = 0

    cCurrentModuleObject = 'RoomAir:Node'
    TotNumOfAirNodes = GetNumObjectsFound(cCurrentModuleObject)

    IF (TotNumOfAirNodes.LE.0) THEN
      ! no air node object is found, terminate the program
      CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' objects found in input.')
      CALL ShowContinueError('The OneNodeDisplacementVentilation model requires '//TRIM(cCurrentModuleObject)//' objects')
      ErrorsFound=.true.
      RETURN
    ELSE
      ! air node objects are found so allocate airnode variable
      ALLOCATE (AirNode(TotNumOfAirNodes))
    END IF

    DO AirNodeNum = 1, TotNumOfAirNodes

        ! get air node objects
        CALL GetObjectItem(cCurrentModuleObject,AirNodeNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),AirNode%Name,AirNodeNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        AirNode(AirNodeNum)%Name                = cAlphaArgs(1)

        AirNode(AirNodeNum)%ZoneName            = cAlphaArgs(3)         ! Zone name
        AirNode(AirNodeNum)%ZonePtr=FindItemInList(AirNode(AirNodeNum)%ZoneName,Zone%Name,NumOfZones)
        IF (AirNode(AirNodeNum)%ZonePtr == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          ErrorsFound=.true.
        ELSE
          ZoneNum=AirNode(AirNodeNum)%ZonePtr
          NumOfSurfs = Zone(ZoneNum)%SurfaceLast - Zone(ZoneNum)%SurfaceFirst + 1
          ALLOCATE (AirNode(AirNodeNum)%SurfMask(NumOfSurfs))
        ENDIF

        SELECT CASE (cAlphaArgs(2))
            CASE ('INLET')
                AirNode(AirNodeNum)%ClassType   = InletAirNode
            CASE ('FLOOR')
                AirNode(AirNodeNum)%ClassType   = FloorAirNode
            CASE ('CONTROL')
                AirNode(AirNodeNum)%ClassType   = ControlAirNode
            CASE ('CEILING')
                AirNode(AirNodeNum)%ClassType   = CeilingAirNode
            CASE ('MUNDTROOM')
                AirNode(AirNodeNum)%ClassType   = MundtRoomAirNode
            CASE ('RETURN')
                AirNode(AirNodeNum)%ClassType   = ReturnAirNode
!            CASE ('PLUME1')
!                AirNode(AirNodeNum)%ClassType   = PlumeAirNode1
!            CASE ('PLUME2')
!                AirNode(AirNodeNum)%ClassType   = PlumeAirNode2
!            CASE ('PLUME3')
!                AirNode(AirNodeNum)%ClassType   = PlumeAirNode3
!            CASE ('PLUME4')
!                AirNode(AirNodeNum)%ClassType   = PlumeAirNode4
!            CASE ('REESROOM1')
!                AirNode(AirNodeNum)%ClassType   = RoomAirNode1
!            CASE ('REESROOM2')
!                AirNode(AirNodeNum)%ClassType   = RoomAirNode2
!            CASE ('REESROOM3')
!                AirNode(AirNodeNum)%ClassType   = RoomAirNode3
!            CASE ('REESROOM4')
!                AirNode(AirNodeNum)%ClassType   = RoomAirNode4
            CASE DEFAULT
                CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
                CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
                ErrorsFound=.true.
        END SELECT

        AirNode(AirNodeNum)%Height              = rNumericArgs(1)        ! Air node height
        NumSurfsInvolved                        = NumAlphas - 3     ! Number of surfaces involved with air nodes

        ! Initialize
        AirNode(AirNodeNum)%SurfMask            = .FALSE.

        IF (NumSurfsInvolved.LE.0) THEN

            ! report severe error since the following air nodes require surfaces associated with them
            SELECT CASE (cAlphaArgs(2))
                CASE ('FLOOR','CEILING','MUNDTROOM','PLUME4','REESROOM1','REESROOM2','REESROOM3','REESROOM4')
                    ! terminate the program due to a severe error in the specified input
                    CALL ShowSevereError('GetAirNodeData: '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                        '" invalid air node specification.')
                    CALL ShowContinueError('Mundt Room Air Model: No surface names specified.  '//  &
                           'Air node="' //TRIM(AirNode(AirNodeNum)%Name) // &
                           ' requires name of surfaces associated with it.')
                    ErrorsFound=.true.
                CASE DEFAULT
            END SELECT

        ELSE

            ! initialize
            SurfNeeded = .TRUE.

            ! report warning error since the following air nodes do not require surfaces associated with them
            ! and assign .FALSE. to 'SurfNeeded'
            SELECT CASE (cAlphaArgs(2))
                CASE ('INLET','CONTROL','RETURN','PLUME1','PLUME2','PLUME3')
                    CALL ShowWarningError('GetAirNodeData: '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                       '" invalid linkage' )
                    CALL ShowContinueError('Mundt Room Air Model: No surface names needed.  '//  &
                       'Air node="' //TRIM(AirNode(AirNodeNum)%Name) // &
                      ' does not relate to any surfaces.')
                    SurfNeeded = .FALSE.
                CASE DEFAULT
            END SELECT

            IF (SurfNeeded) THEN

              ! this air node is in this zone; hence, first get name of all surfaces in this zone
              ZoneNum=AirNode(AirNodeNum)%ZonePtr
              SurfFirst=Zone(ZoneNum)%SurfaceFirst
              NumOfSurfs = Zone(ZoneNum)%SurfaceLast - Zone(ZoneNum)%SurfaceFirst + 1

              ! terminate the program due to a severe error in the specified input
              IF ((NumSurfsInvolved).GT.NumOfSurfs) THEN
                CALL ShowFatalError('GetAirNodeData: Mundt Room Air Model: Number of surfaces connected to ' //  &
                   TRIM(AirNode(AirNodeNum)%Name) // &
                   ' is greater than number of surfaces in ' // TRIM(Zone(ZoneNum)%Name))
                RETURN
              END IF

              ! relate surfaces to this air node and check to see whether surface names are specified correctly or not
              SurfCount = 0
              SurfFirst=SurfFirst-1
              DO ListSurfNum = 4, NumAlphas
                DO SurfNum = 1, NumOfSurfs
                  IF (cAlphaArgs(ListSurfNum) == Surface(SurfFirst+SurfNum)%Name) THEN
                    AirNode(AirNodeNum)%SurfMask(SurfNum)=.TRUE.
                    SurfCount = SurfCount + 1
                  END IF
                END DO
              END DO

              ! report warning error since surface names are specified correctly
              IF ((NumSurfsInvolved).NE.SurfCount) THEN
                CALL ShowWarningError('GetAirNodeData: Mundt Room Air Model: Some surface names specified for ' //  &
                   TRIM(AirNode(AirNodeNum)%Name) // &
                   ' are not in ' // TRIM(Zone(ZoneNum)%Name))
              END IF

            END IF

        END IF

    END DO

    ! get number of air nodes in each zone
    DO ZoneNum = 1, NumOfZones

        ! this zone uses other air model so skip the rest
        IF (AirModel(ZoneNum)%AirModelType.NE.RoomAirModel_Mundt) CYCLE

        ! this zone uses a nodal air model so get number of air nodes in each zone
        DO AirNodeNum = 1, TotNumOfAirNodes
            IF (SameString(AirNode(AirNodeNum)%ZoneName,Zone(ZoneNum)%Name)) THEN
                TotNumOfZoneAirNodes(ZoneNum) = TotNumOfZoneAirNodes(ZoneNum) + 1
            END IF
        END DO

    END DO

    RETURN

END SUBROUTINE GetAirNodeData

!*****************************************************************************************

SUBROUTINE GetMundtData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  April 2003, Weixiu Kong
          !                      July 2003, CC

          ! PURPOSE OF THIS SUBROUTINE:
          !     Get Mundt model controls for all zones at once

          ! METHODOLOGY EMPLOYED:
          !     Use input processer to get input from idf file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,             ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList
    USE DataIPShortCuts
    USE DataHeatBalance,            ONLY : Zone

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    LOGICAL, INTENT(INOUT) :: ErrorsFound  ! True if errors found during this get input routine

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                                     :: NumAlphas    !
    INTEGER                                     :: NumNumbers   ! Number of numbers encountered
    INTEGER                                     :: Status       ! Notes if there was an error in processing the input
    INTEGER                                     :: ControlNum   ! Index number
    INTEGER                                     :: NumOfMundtContrl ! Number of Mundt Model Controls
    INTEGER                                     :: ZoneNum      ! Index number for zones

          ! FLOW:

    IF (.not. MundtModelUsed) RETURN

    ! Initialize default values for Mundt model controls
    ALLOCATE (ConvectiveFloorSplit(NumOfZones))
    ALLOCATE (InfiltratFloorSplit(NumOfZones))
    ConvectiveFloorSplit        = 0.0d0
    InfiltratFloorSplit         = 0.0d0

    cCurrentModuleObject = 'RoomAirSettings:OneNodeDisplacementVentilation'
    NumOfMundtContrl = GetNumObjectsFound(cCurrentModuleObject)
    IF (NumOfMundtContrl.GT.NumOfZones) THEN
        CALL ShowSevereError('Too many '//TRIM(cCurrentModuleObject)//' objects in input file')
        Call ShowContinueError('There cannot be more '//TRIM(cCurrentModuleObject)//' objects than number of zones.')
        ErrorsFound=.true.
    END IF

    IF (NumOfMundtContrl.EQ.0) THEN
        CALL ShowWarningError('No '//TRIM(cCurrentModuleObject)//' objects found, program assumes no convection '// &
                              ' or infiltration gains near floors')
        RETURN
    END IF

    ! this zone uses Mundt model so get Mundt Model Control
    ! loop through all 'RoomAirSettings:OneNodeDisplacementVentilation' objects
    Mundt_Control_Loop: DO ControlNum=1, NumOfMundtContrl
      CALL GetObjectItem(cCurrentModuleObject,ControlNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                             AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
      ZoneNum=FindItemInList(cAlphaArgs(1),Zone%Name,NumOfZones)
      IF (ZoneNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Not a valid zone name.')
        ErrorsFound=.true.
        CYCLE
      ENDIF
      IF (AirModel(ZoneNum)%AirModelType /= RoomAirModel_Mundt) THEN
        CALL ShowSevereError('Zone specified="'//TRIM(cAlphaArgs(1))//'", Air Model type is not OneNodeDisplacementVentilation.')
        CALL ShowContinueError('Air Model Type for zone='//TRIM(ChAirModel(AirModel(ZoneNum)%AirModelType)))
        ErrorsFound=.true.
        CYCLE
      ENDIF
      ConvectiveFloorSplit(ZoneNum) = rNumericArgs(1)
      InfiltratFloorSplit(ZoneNum)  = rNumericArgs(2)
    END DO Mundt_Control_Loop


    RETURN

END SUBROUTINE GetMundtData

SUBROUTINE GetDisplacementVentData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   January 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !  Get UCSD Displacement ventilation model controls for all zones at once

          ! METHODOLOGY EMPLOYED:
          ! Use input processor to get input from idf file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,             ONLY : GetNumObjectsFound, GetObjectItem,FindItemInList
    USE DataIPShortCuts
    USE DataHeatBalance,            ONLY : Zone
    USE ScheduleManager
    USE DataSurfaces,               ONLY : TotSurfaces

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    LOGICAL, INTENT(INOUT) :: ErrorsFound  ! True if errors found during this get input routine

          ! SUBROUTINE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER                 :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                     :: IOStat
  INTEGER                                     :: NumAlpha
  INTEGER                                     :: NumNumber
  INTEGER                                     :: Loop

  IF (.not. UCSDModelUsed) RETURN
  cCurrentModuleObject = 'RoomAirSettings:ThreeNodeDisplacementVentilation'
  TotUCSDDV=GetNumObjectsFound(cCurrentModuleObject)

  IF (TotUCSDDV <= 0) RETURN

  ALLOCATE(ZoneUCSDDV(TotUCSDDV))

  DO Loop=1,TotUCSDDV

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                             AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
    ! First is Zone Name
    ZoneUCSDDV(Loop)%ZoneName = cAlphaArgs(1)
    ZoneUCSDDV(Loop)%ZonePtr = FindIteminList(cAlphaArgs(1),Zone%Name,NumOfZones)
    IF (ZoneUCSDDV(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Zone Name not found.')
      ErrorsFound=.true.
    ELSE
      IsZoneDV(ZoneUCSDDV(Loop)%ZonePtr)=.true.
    ENDIF
    ! Second Alpha is Schedule Name
    ZoneUCSDDV(Loop)%SchedGainsName = cAlphaArgs(2)
    ZoneUCSDDV(Loop)%SchedGainsPtr = GetScheduleIndex(cAlphaArgs(2))
    IF (ZoneUCSDDV(Loop)%SchedGainsPtr == 0) THEN
      IF (lAlphaFieldBlanks(2) ) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError(' Schedule name must be input.')
        ErrorsFound=.true.
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Schedule name was not found.')
        ErrorsFound=.true.
      ENDIF
    ENDIF

    ZoneUCSDDV(Loop)%NumPlumesPerOcc = rNumericArgs(1)
    ZoneUCSDDV(Loop)%ThermostatHeight = rNumericArgs(2)
    ZoneUCSDDV(Loop)%ComfortHeight = rNumericArgs(3)
    ZoneUCSDDV(Loop)%TempTrigger = rNumericArgs(4)

  END DO

  RETURN

END SUBROUTINE GetDisplacementVentData

SUBROUTINE GetCrossVentData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   October 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !  Get UCSD Cross ventilation model controls for all zones at once

          ! METHODOLOGY EMPLOYED:
          ! Use input processor to get input from idf file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,             ONLY : GetNumObjectsFound, GetObjectItem,FindItemInList,FindItem, SameString
  USE DataIPShortCuts
  USE DataHeatBalance,            ONLY : Zone
  USE ScheduleManager

  USE DataSurfaces,               ONLY : TotSurfaces,Surface
  USE DataAirflowNetwork
  USE DataHeatBalance,            ONLY : TotPeople, People
  USE General,                    ONLY : RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! True if errors found during this get input routine

          ! SUBROUTINE PARAMETER DEFINITIONS

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                     :: IOStat
  INTEGER                                     :: NumAlpha
  INTEGER                                     :: NumNumber
  INTEGER                                     :: Loop
  INTEGER                                     :: Loop2
  INTEGER                                     :: ThisZone
  INTEGER                                     :: CompNum = 0
  INTEGER                                     :: TypeNum = 0
  INTEGER                                     :: NodeNum1 = 0
  INTEGER                                     :: NodeNum2 = 0

  IF (.not. UCSDModelUsed) RETURN
  cCurrentModuleObject = 'RoomAirSettings:CrossVentilation'
  TotUCSDCV=GetNumObjectsFound(cCurrentModuleObject)

  IF (TotUCSDCV <= 0) RETURN

  ALLOCATE(ZoneUCSDCV(TotUCSDCV))

  DO Loop=1,TotUCSDCV

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                             AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, &
                             NumericFieldNames=cNumericFieldNames)
    ! First is Zone Name
    ZoneUCSDCV(Loop)%ZoneName = cAlphaArgs(1)
    ZoneUCSDCV(Loop)%ZonePtr = FindIteminList(cAlphaArgs(1),Zone%Name,NumOfZones)
    IF (ZoneUCSDCV(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Zone name was not found.')
      ErrorsFound=.true.
    ELSE
      IsZoneCV(ZoneUCSDCV(Loop)%ZonePtr)=.true.
    ENDIF
    ! Second Alpha is Schedule Name
    ZoneUCSDCV(Loop)%SchedGainsName = cAlphaArgs(2)
    ZoneUCSDCV(Loop)%SchedGainsPtr = GetScheduleIndex(cAlphaArgs(2))
    IF (ZoneUCSDCV(Loop)%SchedGainsPtr == 0) THEN
      IF (lAlphaFieldBlanks(2) ) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Schedule name field is blank.' )
        ErrorsFound=.true.
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Schedule name was not found.' )
        ErrorsFound=.true.
      ENDIF
    ENDIF

     ! Third Alpha is a string: JET or RECIRCULATION
    IF (SameString(cAlphaArgs(3) , 'Jet')) THEN
      ZoneUCSDCV(Loop)%VforComfort = VComfort_Jet
    ELSEIF (SameString(cAlphaArgs(3) , 'Recirculation')) THEN
      ZoneUCSDCV(Loop)%VforComfort = VComfort_Recirculation
    ELSE
      ZoneUCSDCV(Loop)%VforComfort = VComfort_Invalid
    ENDIF

    DO Loop2=1,TotPeople
      IF (People(Loop2)%ZonePtr /= ZoneUCSDCV(Loop)%ZonePtr) CYCLE
      IF (People(Loop2)%Fanger) THEN
        IF (ZoneUCSDCV(Loop)%VforComfort == VComfort_Invalid) THEN
          IF (lAlphaFieldBlanks(3) ) THEN
            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
            CALL ShowContinueError('Airflow region used for thermal comfort '//  &
                                 'evaluation is required for Zone='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Field is blank, please choose Jet or Recirculation.')
            ErrorsFound=.true.
          ELSE
            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
            CALL ShowContinueError('Airflow region used for thermal comfort '//  &
                                 'evaluation is required for Zone='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Please choose Jet or Recirculation.')
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    ThisZone=ZoneUCSDCV(Loop)%ZonePtr
    IF (ThisZone == 0) CYCLE

    ! Following depend on valid zone

    Loop2=FindItemInList(zone(ZoneUCSDCV(Loop)%ZonePtr)%name,MultiZoneZoneData%ZoneName,AirflowNetworkNumOfZones)
    IF (Loop2 == 0) THEN
      CALL ShowSevereError('Problem with '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('AirflowNetwork airflow model must be active in this zone')
      ErrorsFound=.true.
    END IF

    ! If a crack is used it must have an air flow coefficient = 0.5
    DO Loop2=1, NumOfLinksMultizone
      NodeNum1 = MultizoneSurfaceData(Loop2)%NodeNums(1)
      NodeNum2 = MultizoneSurfaceData(Loop2)%NodeNums(2)
      IF (Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Zone == ThisZone .or.   &
         (AirflowNetworkNodeData(NodeNum2)%EPlusZoneNum==ThisZone .and. AirflowNetworkNodeData(NodeNum1)%EPlusZoneNum > 0) .or. &
         (AirflowNetworkNodeData(NodeNum2)%EPlusZoneNum>0 .and. AirflowNetworkNodeData(NodeNum1)%EPlusZoneNum==ThisZone)) THEN
        CompNum = AirflowNetworkLinkageData(Loop2)%CompNum
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) THEN
          IF (MultizoneSurfaceCrackData(TypeNum)%FlowExpo /= 0.50d0) THEN
            AirModel(ThisZone)%AirModelType=RoomAirModel_Mixing
            CALL ShowWarningError('Problem with '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
            CALL ShowWarningError('Roomair model will not be applied for Zone='//TRIM(cAlphaArgs(1))//'.')
            CALL ShowContinueError('AirflowNetwrok:Multizone:Surface crack object must have an air flow coefficient = 0.5, '//  &
                'value was='//TRIM(RoundSigDigits(MultizoneSurfaceCrackData(TypeNum)%FlowExpo,2)))
          ENDIF
        ENDIF
      END IF
    ENDDO
  END DO

  RETURN

END SUBROUTINE GetCrossVentData

SUBROUTINE GetUFADZoneData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2005
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !  Get UCSD UFAD interior zone model controls for all zones at once

          ! METHODOLOGY EMPLOYED:
          ! Use input processor to get input from idf file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,             ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList, SameString
  USE DataIPShortCuts
  USE DataHeatBalance,            ONLY : Zone
  USE ScheduleManager
  USE DataSurfaces,               ONLY : TotSurfaces

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! True if errors found during this get input routine

          ! SUBROUTINE PARAMETER DEFINITIONS

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                   :: IOStat
  INTEGER                                    :: NumAlpha
  INTEGER                                    :: NumNumber
  INTEGER                                    :: Loop


  IF (.not. UCSDModelUsed) THEN
    TotUCSDUI = 0
    TotUCSDUE = 0
    RETURN
  END IF
  cCurrentModuleObject = 'RoomAirSettings:UnderFloorAirDistributionInterior'
  TotUCSDUI=GetNumObjectsFound(cCurrentModuleObject)
  cCurrentModuleObject = 'RoomAirSettings:UnderFloorAirDistributionExterior'
  TotUCSDUE=GetNumObjectsFound(cCurrentModuleObject)

  IF (TotUCSDUI <= 0 .AND. TotUCSDUE <= 0) RETURN

  ALLOCATE(ZoneUCSDUI(TotUCSDUI))
  ALLOCATE(ZoneUCSDUE(TotUCSDUE))
  ALLOCATE(ZoneUFPtr(NumOfZones))
  ZoneUFPtr = 0

  cCurrentModuleObject = 'RoomAirSettings:UnderFloorAirDistributionInterior'
  DO Loop=1,TotUCSDUI
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                             AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, &
                             NumericFieldNames=cNumericFieldNames)
    ! First is Zone Name
    ZoneUCSDUI(Loop)%ZoneName = cAlphaArgs(1)
    ZoneUCSDUI(Loop)%ZonePtr = FindIteminList(cAlphaArgs(1),Zone%Name,NumOfZones)
    ZoneUFPtr(ZoneUCSDUI(Loop)%ZonePtr) = Loop
    IF (ZoneUCSDUI(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Zone name was not found.')
      ErrorsFound=.true.
    ELSE
      IsZoneUI(ZoneUCSDUI(Loop)%ZonePtr)=.true.
    ENDIF
    ! 2nd alpha is diffuser type
    IF (SameString(cAlphaArgs(2),'Swirl')) THEN
      ZoneUCSDUI(Loop)%DiffuserType = Swirl
    ELSE IF (SameString(cAlphaArgs(2),'VariableArea')) THEN
      ZoneUCSDUI(Loop)%DiffuserType = VarArea
    ELSE IF (SameString(cAlphaArgs(2),'HorizontalSwirl')) THEN
      ZoneUCSDUI(Loop)%DiffuserType  = DisplVent
    ELSE IF (SameString(cAlphaArgs(2),'Custom')) THEN
      ZoneUCSDUI(Loop)%DiffuserType  = Custom
      ELSE IF (SameString(cAlphaArgs(2),'LinearBarGrille')) THEN
      ZoneUCSDUI(Loop)%DiffuserType  = LinBarGrille
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      ErrorsFound = .TRUE.
    END IF
    ! 1st number is Number of Diffusers per Zone
    ZoneUCSDUI(Loop)%DiffusersPerZone = rNumericArgs(1)
    ! 2nd number is Power per Plume
    ZoneUCSDUI(Loop)%PowerPerPlume = rNumericArgs(2)
    ! 3rd number is Design Effective Area of Diffuser
    ZoneUCSDUI(Loop)%DiffArea = rNumericArgs(3)
    ! 4th number is Diffuser Slot Angle from Vertical
    ZoneUCSDUI(Loop)%DiffAngle = rNumericArgs(4)
    ! 5th number is Thermostat Height
    ZoneUCSDUI(Loop)%ThermostatHeight = rNumericArgs(5)
    ! 6th number is Comfort Height
    ZoneUCSDUI(Loop)%ComfortHeight = rNumericArgs(6)
    ! 7th number is Temperature Difference Threshold for Reporting
    ZoneUCSDUI(Loop)%TempTrigger = rNumericArgs(7)
    ! 8th number user-specified transition height
    ZoneUCSDUI(Loop)%TransHeight = rNumericArgs(8)
    ! 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUI(Loop)%A_Kc = rNumericArgs(9)
    ! 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUI(Loop)%B_Kc = rNumericArgs(10)
    ! 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUI(Loop)%C_Kc = rNumericArgs(11)
    ! 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUI(Loop)%D_Kc = rNumericArgs(12)
    ! 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUI(Loop)%E_Kc = rNumericArgs(13)
  END DO

  cCurrentModuleObject = 'RoomAirSettings:UnderFloorAirDistributionExterior'
  DO Loop=1,TotUCSDUE
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                             AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, &
                             NumericFieldNames=cNumericFieldNames)
    ! First is Zone Name
    ZoneUCSDUE(Loop)%ZoneName = cAlphaArgs(1)
    ZoneUCSDUE(Loop)%ZonePtr = FindIteminList(cAlphaArgs(1),Zone%Name,NumOfZones)
    ZoneUFPtr(ZoneUCSDUE(Loop)%ZonePtr) = Loop
    IF (ZoneUCSDUE(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Zone name was not found.')
      ErrorsFound=.true.
    ELSE
      IsZoneUI(ZoneUCSDUE(Loop)%ZonePtr)=.true.
    ENDIF
    ! 2nd alpha is diffuser type
    IF (SameString(cAlphaArgs(2),'Swirl')) THEN
      ZoneUCSDUE(Loop)%DiffuserType = Swirl
    ELSE IF (SameString(cAlphaArgs(2),'VariableArea')) THEN
      ZoneUCSDUE(Loop)%DiffuserType = VarArea
    ELSE IF (SameString(cAlphaArgs(2),'HorizontalSwirl')) THEN
      ZoneUCSDUE(Loop)%DiffuserType  = DisplVent
    ELSE IF (SameString(cAlphaArgs(2),'Custom')) THEN
      ZoneUCSDUE(Loop)%DiffuserType  = Custom
      ELSE IF (SameString(cAlphaArgs(2),'LinearBarGrille')) THEN
      ZoneUCSDUE(Loop)%DiffuserType  = LinBarGrille
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      ErrorsFound = .TRUE.
    END IF
    ! 1st number is Number of Diffusers per Zone
    ZoneUCSDUE(Loop)%DiffusersPerZone = rNumericArgs(1)
    ! 2nd number is Power per Plume
    ZoneUCSDUE(Loop)%PowerPerPlume = rNumericArgs(2)
    ! 3rd number is Design Effective Area of Diffuser
    ZoneUCSDUE(Loop)%DiffArea = rNumericArgs(3)
    ! 4th number is Diffuser Slot Angle from Vertical
    ZoneUCSDUE(Loop)%DiffAngle = rNumericArgs(4)
    ! 5th number is Thermostat Height
    ZoneUCSDUE(Loop)%ThermostatHeight = rNumericArgs(5)
    ! 6th number is Comfort Height
    ZoneUCSDUE(Loop)%ComfortHeight = rNumericArgs(6)
    ! 7th number is Temperature Difference Threshold for Reporting
    ZoneUCSDUE(Loop)%TempTrigger = rNumericArgs(7)
    ! 8th number user-specified transition height
    ZoneUCSDUE(Loop)%TransHeight = rNumericArgs(8)
    ! 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUE(Loop)%A_Kc = rNumericArgs(9)
    ! 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUE(Loop)%B_Kc = rNumericArgs(10)
    ! 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUE(Loop)%C_Kc = rNumericArgs(11)
    ! 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUE(Loop)%D_Kc = rNumericArgs(12)
    ! 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    ZoneUCSDUE(Loop)%E_Kc = rNumericArgs(13)
  END DO

  RETURN

END SUBROUTINE GetUFADZoneData

SUBROUTINE SharedDVCVUFDataInit(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2005
          !       MODIFIED       Aug, 2013, Sam Brunswick -- for RoomAirCrossCrossVent modifications
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine allocates and initializes(?) the data that is shared between the
          ! UCSD models (DV and CV)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataUCSDSharedData
  USE DataEnvironment
  USE DataHeatBalFanSys
  USE DataSurfaces
  USE DataGlobals,  ONLY : NumOfZones, MaxNameLength
  USE DataInterfaces,  ONLY : ShowWarningError, ShowFatalerror, ShowSevereError, ShowContinueError
  USE DataHeatBalance, ONLY : Zone
  USE DataAirflowNetwork
  USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW
  USE DataZoneEquipment,          ONLY: ZoneEquipConfig

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                         :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER  :: BaseDischargeCoef = 0.62d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER         :: SurfNum                ! DO loop counter for surfaces
  INTEGER         :: ZNum                   ! DO loop counter for zones
  INTEGER         :: contFloorBegin    = 0 ! counter
  INTEGER         :: contFloorLast     = 0 ! counter
  INTEGER         :: contFloor         = 0 ! counter
  INTEGER         :: contCeilingBegin     = 0 ! counter
  INTEGER         :: contCeilingLast      = 0 ! counter
  INTEGER         :: contCeiling          = 0 ! counter
  INTEGER         :: contWallBegin        = 0 ! counter
  INTEGER         :: contWallLast         = 0 ! counter
  INTEGER         :: contWall             = 0 ! counter
  INTEGER         :: contWindowBegin      = 0 ! counter
  INTEGER         :: contWindowLast       = 0 ! counter
  INTEGER         :: contWindow           = 0 ! counter
  INTEGER         :: contInternalBegin    = 0 ! counter
  INTEGER         :: contInternalLast     = 0 ! counter
  INTEGER         :: contInternal         = 0 ! counter
  INTEGER         :: contDoorBegin        = 0 ! counter
  INTEGER         :: contDoorLast         = 0 ! counter
  INTEGER         :: contDoor             = 0 ! counter
  INTEGER         :: Loop                 = 0 ! counter
  INTEGER         :: Loop2                = 0 ! counter
  INTEGER         :: i                    = 0 ! counter
  INTEGER         :: N                    = 0 ! counter
  REAL(r64)       :: Z1ZoneAux            = 0.0d0 ! Auxiliary variables
  REAL(r64)       :: Z2ZoneAux            = 0.0d0 ! Auxiliary variables
  REAL(r64)       :: Z1Zone               = 0.0d0 ! Auxiliary variables
  REAL(r64)       :: Z2Zone               = 0.0d0 ! Auxiliary variables
  REAL(r64)       :: CeilingHeightDiffMax = 0.1d0 ! Maximum difference between wall height and ceiling height
  LOGICAL         :: SetZoneAux
  INTEGER, ALLOCATABLE, DIMENSION (:)   :: AuxSurf
  INTEGER         :: MaxSurf
  INTEGER, ALLOCATABLE, DIMENSION (:,:) :: AuxAirflowNetworkSurf
  REAL(r64)       :: WidthFactMax
  REAL(r64)       :: HeightFactMax
  REAL(r64)       :: WidthFact
  REAL(r64)       :: HeightFact
  INTEGER         :: Loop3                = 0 ! counter
  INTEGER         :: ZoneEquipConfigNum       ! counter
  REAL(r64)       :: AinCV
  INTEGER         :: AirflowNetworkSurfPtr
  INTEGER         :: NSides
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag

  INTEGER         :: CompNum = 0 ! AirflowNetwork Component number
  INTEGER         :: TypeNum = 0 ! Airflownetwork Type Number within a component
  INTEGER         :: NodeNum1 = 0 ! The first node number in an AirflowNetwork linkage data
  INTEGER         :: NodeNum2 = 0 ! The Second node number in an AirflowNetwork linkage data

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumOfZones))

    ALLOCATE (APos_Wall(TotSurfaces))
    ALLOCATE (APos_Floor(TotSurfaces))
    ALLOCATE (APos_Ceiling(TotSurfaces))
    ALLOCATE (PosZ_Wall(NumOfZones*2))
    ALLOCATE (PosZ_Floor(NumOfZones*2))
    ALLOCATE (PosZ_Ceiling(NumOfZones*2))
    ALLOCATE (APos_Window(TotSurfaces))
    ALLOCATE (APos_Door(TotSurfaces))
    ALLOCATE (APos_Internal(TotSurfaces))
    ALLOCATE (PosZ_Window(NumOfZones*2))
    ALLOCATE (PosZ_Door(NumOfZones*2))
    ALLOCATE (PosZ_Internal(NumOfZones*2))
    ALLOCATE (Hceiling(TotSurfaces))
    ALLOCATE (HWall(TotSurfaces))
    ALLOCATE (HFloor(TotSurfaces))
    ALLOCATE (HInternal(TotSurfaces))
    ALLOCATE (HWindow(TotSurfaces))
    ALLOCATE (HDoor(TotSurfaces))

    ALLOCATE (AuxSurf(NumOfZones))

    ALLOCATE (ZoneCeilingHeight(NumOfZones*2))
    ZoneCeilingHeight=0.0d0


    ! Arrays initializations
    APos_Wall     = 0
    APos_Floor    = 0
    APos_Ceiling  = 0
    PosZ_Wall     = 0
    PosZ_Floor    = 0
    PosZ_Ceiling  = 0
    APos_Window   = 0
    APos_Door     = 0
    APos_Internal = 0
    PosZ_Window   = 0
    PosZ_Door     = 0
    PosZ_Internal = 0
    Hceiling      = 0.0d0
    HWall         = 0.0d0
    HFloor        = 0.0d0
    HInternal     = 0.0d0
    HWindow       = 0.0d0
    HDoor         = 0.0d0

    ! Put the surface and zone information in Apos and PosZ arrays
    DO ZNum = 1,NumOfZones
      ! advance ONE position in the arrays PosZ because this is a new zone
      contWallBegin       = contWall + 1
      contFloorBegin   = contFloor + 1
      contCeilingBegin    = contCeiling + 1
      contWindowBegin     = contWindow + 1
      contInternalBegin   = contInternal + 1
      contDoorBegin       = contDoor + 1
      SetZoneAux          = .true.

      ! cycle in this zone for all the surfaces
      DO SurfNum = Zone(ZNum)%SurfaceFirst,Zone(ZNum)%SurfaceLast
        IF (Surface(SurfNum)%Class /= SurfaceClass_IntMass) THEN
          ! Recalculate lowest and highest height for the zone
          Z1Zone = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
          Z2Zone = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        ENDIF

        IF (SetZoneAux) THEN
          ! lowest height for the zone (for the first surface of the zone)
          Z1ZoneAux = Z1Zone
          ! highest height for the zone (for the first surface of the zone)
          Z2ZoneAux = Z2Zone
          SetZoneAux=.false.
        ENDIF

        IF(Z1Zone < Z1ZoneAux) THEN
          Z1ZoneAux = Z1Zone
        ENDIF
        IF(Z2Zone > Z2ZoneAux) THEN
          Z2ZoneAux = Z2Zone
        ENDIF
        Z1Zone = Z1ZoneAux
        Z2Zone = Z2ZoneAux

        ! Put the reference to this surface in the appropriate array
        IF (Surface(SurfNum)%Class .eq. SurfaceClass_Floor) THEN
          contFloor = contFloor + 1
          APos_Floor(contFloor) = SurfNum
        ELSEIF (Surface(SurfNum)%Class .eq. SurfaceClass_Wall) THEN
          contWall = contWall + 1
          APos_Wall(contWall) = SurfNum
        ELSEIF(Surface(SurfNum)%Class .eq. SurfaceClass_Window)THEN
         contWindow = contWindow + 1
          APos_Window(contWindow) = SurfNum
        ELSEIF(Surface(SurfNum)%Class .eq. SurfaceClass_IntMass)THEN
          contInternal = contInternal + 1
          APos_Internal(contInternal) = SurfNum
        ELSEIF(Surface(SurfNum)%Class .eq. SurfaceClass_Door)THEN
          contDoor = contDoor + 1
          APos_Door(contDoor) = SurfNum
        ELSE
          contCeiling = contCeiling + 1
          APos_Ceiling(contCeiling) = SurfNum
        END IF
      END DO ! Surfaces

      contWallLast        = contWall
      contFloorLast       = contFloor
      contCeilingLast     = contCeiling
      contWindowLast      = contWindow
      contDoorLast        = contDoor
      contInternalLast    = contInternal
     ! PosZ_Wall (... + 1) has the Begin Wall reference in Apos_Wall for the ZNum
      ! PosZ_Wall (... + 2) has the End Wall reference in Apos_Wall for the ZNum
      PosZ_Wall((ZNum-1)*2 + 1)     = contWallBegin
      PosZ_Wall((ZNum-1)*2 + 2)     = contWallLast
      PosZ_Floor((ZNum-1)*2 + 1)    = contFloorBegin
      PosZ_Floor((ZNum-1)*2 + 2)    = contFloorLast
      PosZ_Ceiling((ZNum-1)*2 + 1)  = contCeilingBegin
      PosZ_Ceiling((ZNum-1)*2 + 2)  = contCeilingLast
      PosZ_Window((ZNum-1)*2 + 1)   = contWindowBegin
      PosZ_Window((ZNum-1)*2 + 2)   = contWindowLast
      PosZ_Door((ZNum-1)*2 + 1)     = contDoorBegin
      PosZ_Door((ZNum-1)*2 + 2)     = contDoorLast
      PosZ_Internal((ZNum-1)*2 + 1) = contInternalBegin
      PosZ_Internal((ZNum-1)*2 + 2) = contInternalLast
      ! Save the highest and lowest height for this zone
      ZoneCeilingHeight((ZNum-1)*2 + 1) = Z1Zone
      ZoneCeilingHeight((ZNum-1)*2 + 2) = Z2Zone

      IF (ABS((Z2Zone-Z1Zone) - Zone(ZNum)%CeilingHeight) > CeilingHeightDiffMax) THEN
        CALL ShowWarningError ('RoomAirManager: Inconsistent ceiling heights in Zone: '//TRIM(Zone(ZNum)%Name))
        CALL ShowContinueError('Lowest height=['//trim(RoundSigDigits(Z1Zone,3))//'].')
        CALL ShowContinueError('Highest height=['//trim(RoundSigDigits(Z2Zone,3))//'].')
        CALL ShowContinueError('Ceiling height=['//trim(RoundSigDigits(Zone(ZNum)%CeilingHeight,3))//'].')
      ENDIF
    END DO ! Zones

    AuxSurf=0
    CVNumAirflowNetworkSurfaces=0

    ! calculate maximum number of airflow network surfaces in each zone
    DO Loop=1, NumOfLinksMultizone
      AuxSurf(Surface(MultizoneSurfaceData(Loop)%SurfNum)%Zone)=AuxSurf(Surface(MultizoneSurfaceData(Loop)%SurfNum)%Zone)+1
      CVNumAirflowNetworkSurfaces=CVNumAirflowNetworkSurfaces+1
      ! Check if this is an interzone airflow network surface
      IF (Surface(MultizoneSurfaceData(Loop)%SurfNum)%ExtBoundCond > 0 .and. (MultizoneSurfaceData(Loop)%SurfNum /=   &
             Surface(MultizoneSurfaceData(Loop)%SurfNum)%ExtBoundCond) ) THEN
        AuxSurf(Surface(Surface(MultizoneSurfaceData(Loop)%SurfNum)%ExtBoundCond)%Zone)=  &
                  AuxSurf(Surface(Surface(MultizoneSurfaceData(Loop)%SurfNum)%ExtBoundCond)%Zone)+1
        CVNumAirflowNetworkSurfaces=CVNumAirflowNetworkSurfaces+1
      END IF
    END DO
    ! calculate maximum number of airflow network surfaces in a single zone
    MaxSurf=AuxSurf(1)
    DO Loop=2, NumOfZones
      IF (AuxSurf(Loop)> MaxSurf) MaxSurf=AuxSurf(Loop)
    END DO


    IF (.NOT. allocated(AirflowNetworkSurfaceUCSDCV)) THEN
     ALLOCATE (AirflowNetworkSurfaceUCSDCV(NumofZones, 0:MaxSurf))
    ENDIF
    IF (.NOT. allocated(CVJetRecFlows)) THEN
     ALLOCATE (CVJetRecFlows(NumofZones, 0:MaxSurf))
    ENDIF
    ALLOCATE (AuxAirflowNetworkSurf(NumofZones, 0:MaxSurf))
    ! Width and Height for airflow network surfaces
    IF (.NOT. allocated(SurfParametersCVDV)) THEN
     ALLOCATE (SurfParametersCVDV(NumOfLinksMultizone))
    ENDIF



    AirflowNetworkSurfaceUCSDCV=0
    ! Organize surfaces in vector AirflowNetworkSurfaceUCSDCV(Zone, surface indexes)
    DO Loop=1, NumOfZones
      ! the 0 component of the array has the number of relevant AirflowNetwork surfaces for the zone
      AirflowNetworkSurfaceUCSDCV(Loop,0)=AuxSurf(Loop)
      IF (AuxSurf(Loop)/=0) THEN
        SurfNum=1
        DO Loop2=1, NumOfLinksMultizone
          IF (Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Zone ==Loop) THEN
            ! SurfNum has the reference surface number relative to AirflowNetworkSurfaceData
            AirflowNetworkSurfaceUCSDCV(Loop,SurfNum)= Loop2
            ! calculate the surface width and height
            CompNum = AirflowNetworkLinkageData(Loop2)%CompNum
            TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
           IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_DOP) then
              WidthFactMax=0.0d0
              HeightFactMax=0.0d0
              DO Loop3=1,MultizoneCompDetOpeningData(TypeNum)%NumFac
                If (Loop3 .eq. 1) then
                  WidthFact=MultizoneCompDetOpeningData(TypeNum)%WidthFac1
                  HeightFact=MultizoneCompDetOpeningData(TypeNum)%HeightFac1
                end if
                If (Loop3 .eq. 2) then
                  WidthFact=MultizoneCompDetOpeningData(TypeNum)%WidthFac2
                  HeightFact=MultizoneCompDetOpeningData(TypeNum)%HeightFac2
                end if
                If (Loop3 .eq. 3) then
                  WidthFact=MultizoneCompDetOpeningData(TypeNum)%WidthFac3
                  HeightFact=MultizoneCompDetOpeningData(TypeNum)%HeightFac3
                end if
                If (Loop3 .eq. 4) then
                  WidthFact=MultizoneCompDetOpeningData(TypeNum)%WidthFac4
                  HeightFact=MultizoneCompDetOpeningData(TypeNum)%HeightFac4
                end if
                IF (WidthFact > WidthFactMax) THEN
                  WidthFactMax = WidthFact
                END IF
                IF (HeightFact>HeightFactMax )THEN
                  HeightFactMax = HeightFact
                END IF
              END DO
              SurfParametersCVDV(Loop2)%Width=WidthFactMax*Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Width
              SurfParametersCVDV(Loop2)%Height=HeightFactMax*Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Height
            ELSE IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) then ! surface type = CRACK
              SurfParametersCVDV(Loop2)%Width=Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Width/2
              AinCV= MultizoneSurfaceCrackData(TypeNum)%FlowCoef / &
                       (BaseDischargeCoef*SQRT(2.0d0/PsyRhoAirFnPbTdbW(OutBaroPress,MAT(Loop),ZoneAirHumRat(Loop)) ))
              SurfParametersCVDV(Loop2)%Height=AinCV/SurfParametersCVDV(Loop2)%Width
            END IF
            ! calculate the surface Zmin and Zmax
            IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_DOP) THEN
              AirflowNetworkSurfPtr=MultizoneSurfaceData(Loop2)%SurfNum
              NSides=Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Sides
              SurfParametersCVDV(Loop2)%zmin =   &
                  MINVAL(Surface(AirflowNetworkSurfPtr)%Vertex(1:NSides)%Z)-ZoneCeilingHeight((loop-1)*2 + 1)
              SurfParametersCVDV(Loop2)%zmax =   &
                  MAXVAL(Surface(AirflowNetworkSurfPtr)%Vertex(1:NSides)%Z)-ZoneCeilingHeight((loop-1)*2 + 1)
            ELSE IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) then ! surface type = CRACK
              AirflowNetworkSurfPtr=MultizoneSurfaceData(Loop2)%SurfNum
              NSides=Surface(MultizoneSurfaceData(Loop2)%SurfNum)%Sides
              SurfParametersCVDV(Loop2)%zmin =   &
                  MINVAL(Surface(AirflowNetworkSurfPtr)%Vertex(1:NSides)%Z)-ZoneCeilingHeight((loop-1)*2 + 1)
              SurfParametersCVDV(Loop2)%zmax =   &
                  MAXVAL(Surface(AirflowNetworkSurfPtr)%Vertex(1:NSides)%Z)-ZoneCeilingHeight((loop-1)*2 + 1)
            END IF

            SurfNum=SurfNum+1
            ! Check if airflow network Surface is an interzone surface:
          ELSE
            NodeNum1 = MultizoneSurfaceData(Loop2)%NodeNums(1)
            NodeNum2 = MultizoneSurfaceData(Loop2)%NodeNums(2)
            IF ((AirflowNetworkNodeData(NodeNum2)%EPlusZoneNum==Loop .and. AirflowNetworkNodeData(NodeNum1)%EPlusZoneNum > 0)  &
                   .or.  &
                (AirflowNetworkNodeData(NodeNum2)%EPlusZoneNum>0 .and. AirflowNetworkNodeData(NodeNum1)%EPlusZoneNum==Loop) ) THEN
              AirflowNetworkSurfaceUCSDCV(Loop,SurfNum)= Loop2
              SurfNum=SurfNum+1
            END IF
          END IF
        END DO
      END IF
    END DO

    DEALLOCATE(AuxSurf)

    IF (Any(IsZoneDV) .OR. Any(IsZoneUI)) THEN
      ALLOCATE (MaxTempGrad(NumOfZones))
      ALLOCATE (AvgTempGrad(NumOfZones))
      ALLOCATE (TCMF(NumOfZones))
      ALLOCATE (FracMinFlow(NumOfZones))
      ALLOCATE (ZoneAirSystemON(NumOfZones))
    ! Allocate histories of displacement ventilation temperatures PH 3/5/04
      ALLOCATE(MATFloor(NumOfZones))
      ALLOCATE(XMATFloor(NumOfZones))
      ALLOCATE(XM2TFloor(NumOfZones))
      ALLOCATE(XM3TFloor(NumOfZones))
      ALLOCATE(XM4TFloor(NumOfZones))
      ALLOCATE(DSXMATFloor(NumOfZones))
      ALLOCATE(DSXM2TFloor(NumOfZones))
      ALLOCATE(DSXM3TFloor(NumOfZones))
      ALLOCATE(DSXM4TFloor(NumOfZones))
      ALLOCATE(MATOC(NumOfZones))
      ALLOCATE(XMATOC(NumOfZones))
      ALLOCATE(XM2TOC(NumOfZones))
      ALLOCATE(XM3TOC(NumOfZones))
      ALLOCATE(XM4TOC(NumOfZones))
      ALLOCATE(DSXMATOC(NumOfZones))
      ALLOCATE(DSXM2TOC(NumOfZones))
      ALLOCATE(DSXM3TOC(NumOfZones))
      ALLOCATE(DSXM4TOC(NumOfZones))
      ALLOCATE(MATMX(NumOfZones))
      ALLOCATE(XMATMX(NumOfZones))
      ALLOCATE(XM2TMX(NumOfZones))
      ALLOCATE(XM3TMX(NumOfZones))
      ALLOCATE(XM4TMX(NumOfZones))
      ALLOCATE(DSXMATMX(NumOfZones))
      ALLOCATE(DSXM2TMX(NumOfZones))
      ALLOCATE(DSXM3TMX(NumOfZones))
      ALLOCATE(DSXM4TMX(NumOfZones))
      ALLOCATE(ZTM1Floor(NumOfZones))
      ALLOCATE(ZTM2Floor(NumOfZones))
      ALLOCATE(ZTM3Floor(NumOfZones))
      ALLOCATE(ZTM1OC(NumOfZones))
      ALLOCATE(ZTM2OC(NumOfZones))
      ALLOCATE(ZTM3OC(NumOfZones))
      ALLOCATE(ZTM1MX(NumOfZones))
      ALLOCATE(ZTM2MX(NumOfZones))
      ALLOCATE(ZTM3MX(NumOfZones))
      ALLOCATE(AIRRATFloor(NumOfZones))
      ALLOCATE(AIRRATOC(NumOfZones))
      ALLOCATE(AIRRATMX(NumOfZones))
      ALLOCATE (ZTOC(NumOfZones))
      ALLOCATE (ZTMX(NumOfZones))
      ALLOCATE (ZTFLOOR(NumOfZones))
      ALLOCATE (HeightTransition(NumOfZones))
      ALLOCATE (Phi(NumOfZones))
      ALLOCATE (Zone1Floor(NumOfZones))
      ALLOCATE (ZoneMXFloor(NumOfZones))
      ALLOCATE (ZoneM2Floor(NumOfZones))
      ALLOCATE (Zone1OC(NumOfZones))
      ALLOCATE (ZoneMXOC(NumOfZones))
      ALLOCATE (ZoneM2OC(NumOfZones))
      ALLOCATE (Zone1MX(NumOfZones))
      ALLOCATE (ZoneMXMX(NumOfZones))
      ALLOCATE (ZoneM2MX(NumOfZones))

      MaxTempGrad = 0.0d0
      AvgTempGrad = 0.0d0
      TCMF=23.0d0
      FracMinFlow   = 0.0d0
!      ZoneDVMixedFlagRep    = 0.0
      ZoneAirSystemON = .FALSE.
!      ZoneDVMixedFlag=0
      MATFloor = 23.0d0
      XMATFloor = 23.0d0
      XM2TFloor = 23.0d0
      XM3TFloor = 23.0d0
      XM4TFloor = 23.0d0
      DSXMATFloor = 23.0d0
      DSXM2TFloor = 23.0d0
      DSXM3TFloor = 23.0d0
      DSXM4TFloor = 23.0d0
      MATOC = 23.0d0
      XMATOC = 23.0d0
      XM2TOC = 23.0d0
      XM3TOC = 23.0d0
      XM4TOC = 23.0d0
      DSXMATOC = 23.0d0
      DSXM2TOC = 23.0d0
      DSXM3TOC = 23.0d0
      DSXM4TOC = 23.0d0
      MATMX = 23.0d0
      XMATMX = 23.0d0
      XM2TMX = 23.0d0
      XM3TMX = 23.0d0
      XM4TMX = 23.0d0
      DSXMATMX = 23.0d0
      DSXM2TMX = 23.0d0
      DSXM3TMX = 23.0d0
      DSXM4TMX = 23.0d0
      ZTM1Floor = 23.0d0
      ZTM2Floor = 23.0d0
      ZTM3Floor = 23.0d0
      ZTM1OC = 23.0d0
      ZTM2OC = 23.0d0
      ZTM3OC = 23.0d0
      ZTM1MX = 23.0d0
      ZTM2MX = 23.0d0
      ZTM3MX = 23.0d0
      Zone1Floor = 23.0d0
      ZoneMXFloor = 23.0d0
      ZoneM2Floor = 23.0d0
      Zone1OC = 23.0d0
      ZoneMXOC = 23.0d0
      ZoneM2OC = 23.0d0
      Zone1MX = 23.0d0
      ZoneMXMX = 23.0d0
      ZoneM2MX = 23.0d0
      AIRRATFloor=0.0d0
      AIRRATOC=0.0d0
      AIRRATMX=0.0d0
      ZTOC = 23.0d0
      ZTMX = 23.0d0
      ZTFLOOR = 23.0d0
      HeightTransition = 0.0d0
      Phi = 0.0d0
      Hceiling      = 0.0d0
      HWall         = 0.0d0
      HFloor        = 0.0d0
      HInternal     = 0.0d0
      HWindow       = 0.0d0
      HDoor         = 0.0d0

    ENDIF

    IF (Any(IsZoneDV)) THEN

      ALLOCATE (DVHcIn(TotSurfaces))
      ALLOCATE (ZoneDVMixedFlagRep(NumOfZones))
      ALLOCATE (ZoneDVMixedFlag(NumOfZones))
      DVHcIn = 0.0d0
      ZoneDVMixedFlagRep = 0.0d0
      ZoneDVMixedFlag = 0
      ! Output variables and DV zone flag
      DO Loop=1,NumOfZones
        If (AirModel(loop)%AirModelType /= RoomAirModel_UCSDDV) cycle  !don't set these up if they don't make sense
        !CurrentModuleObject='RoomAirSettings:ThreeNodeDisplacementVentilation'
        CALL SetupOutputVariable('Room Air Zone Mixed Subzone Temperature [C]',ZTMX(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Occupied Subzone Temperature [C]',ZTOC(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Floor Subzone Temperature [C]',ZTFLOOR(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Transition Height [m]',HeightTransition(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Recommended Minimum Flow Fraction []', &
                                  FracMinFlow(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Is Mixed Status []',ZoneDVMixedFlagRep(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Average Temperature Gradient [K/m]', &
                                  AvgTempGrad(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Maximum Temperature Gradient [K/m]', &
                                  MaxTempGrad(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Thermal Comfort Effective Air Temperature [C]', &
                                  TCMF(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Thermostat Temperature [C]',TempTstatAir(Loop),'HVAC','State',Zone(Loop)%Name)
      ENDDO

    END IF

    IF (Any(IsZoneUI)) THEN
      ALLOCATE (ZoneUFMixedFlag(NumOfZones))
      ALLOCATE (ZoneUFMixedFlagRep(NumOfZones))
      ALLOCATE (UFHcIn(TotSurfaces))
      ALLOCATE (ZoneUFGamma(NumOfZones))
      ALLOCATE (ZoneUFPowInPlumes(NumOfZones))
      ALLOCATE (ZoneUFPowInPlumesfromWindows(NumOfZones))
      ZoneUFMixedFlag = 0
      ZoneUFMixedFlagRep = 0.0d0
      UFHcIn = 0.0d0
      ZoneUFGamma = 0.0d0
      ZoneUFPowInPlumes = 0.0d0
      ZoneUFPowInPlumesfromWindows = 0.0d0
      ! Output variables and UF zone flag
      DO Loop=1,NumOfZones
        If (AirModel(loop)%AirModelType /= RoomAirModel_UCSDUFI) cycle  !don't set these up if they don't make sense
        !CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionInterior'
        CALL SetupOutputVariable('Room Air Zone Mixed Subzone Temperature [C]',ZTMX(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Occupied Subzone Temperature [C]',ZTOC(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Transition Height [m]',HeightTransition(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Is Mixed Status []',ZoneUFMixedFlagRep(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Average Temperature Gradient [K/m]', &
                                  AvgTempGrad(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Effective Comfort Air Temperature [C]',TCMF(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Thermostat Temperature [C]',TempTstatAir(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Transition Height Gamma Value []',ZoneUFGamma(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Plume Heat Transfer Rate [W]', &
                                  ZoneUFPowInPlumes(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Temperature Stratification Fraction []',Phi(Loop),'HVAC','State',Zone(Loop)%Name)

        ! set zone equip pointer in the UCSDUI data structure
        DO ZoneEquipConfigNum = 1, NumOfZones
          IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum == Loop) THEN
            ZoneUCSDUI(ZoneUFPtr(Loop))%ZoneEquipPtr = ZoneEquipConfigNum
            EXIT
          END IF
        END DO ! ZoneEquipConfigNum
      ENDDO
      DO Loop=1,NumOfZones
        If (AirModel(loop)%AirModelType /= RoomAirModel_UCSDUFE) cycle  !don't set these up if they don't make sense
        !CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionExterior'
        CALL SetupOutputVariable('Room Air Zone Mixed Subzone Temperature [C]',ZTMX(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Occupied Subzone Temperature [C]',ZTOC(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Transition Height [m]',HeightTransition(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Is Mixed Status []',ZoneUFMixedFlagRep(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Average Temperature Gradient [K/m]', &
                                  AvgTempGrad(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Effective Comfort Air Temperature [C]',TCMF(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Thermostat Temperature [C]',TempTstatAir(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Transition Height Gamma Value []',ZoneUFGamma(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Plume Heat Transfer Rate [W]', &
                                  ZoneUFPowInPlumes(Loop),'HVAC','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Window Plume Heat Transfer Rate [W]',ZoneUFPowInPlumesfromWindows(Loop),'HVAC',&
                                  'State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Temperature Stratification Fraction []',Phi(Loop),'HVAC','State',Zone(Loop)%Name)
        ! set zone equip pointer in the UCSDUE data structure
        DO ZoneEquipConfigNum = 1, NumOfZones
          IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum == Loop) THEN
            ZoneUCSDUE(ZoneUFPtr(Loop))%ZoneEquipPtr = ZoneEquipConfigNum
            EXIT
          END IF
        END DO ! ZoneEquipConfigNum
      ENDDO
    END IF

    IF (Any(IsZoneCV)) THEN
      ALLOCATE (CVHcIn(TotSurfaces))
      ALLOCATE (ZTJET(NumOfZones))
      ! Most ZTJet takes defaults
      ALLOCATE (ZTREC(NumOfZones))
      ALLOCATE (RoomOutflowTemp(NumOfZones))
      ! Most ZTrec takes defaults
      ALLOCATE (JetRecAreaRatio(NumOfZones))
      ALLOCATE (Urec(NumOfZones))
      ALLOCATE (Ujet(NumOfZones))
      ALLOCATE (Qrec(NumOfZones))
      ALLOCATE (Qtot(NumOfZones))
      ALLOCATE (RecInflowRatio(NumOfZones))
      ALLOCATE (Uhc(NumOfZones))
      ALLOCATE (Ain(NumOfZones))
      ALLOCATE (Tin(NumOfZones))
      ALLOCATE (Droom(NumOfZones))
      ALLOCATE (Dstar(NumOfZones))
      ALLOCATE (ZoneCVisMixing(NumOfZones))
      ALLOCATE (Rfr(NumOfZones))
      ALLOCATE (ZoneCVhasREC(NumofZones))

      ZTJET           = 23.0d0
      RoomOutflowTemp = 23.0d0
      ZTREC           = 23.0d0
      CVHcIn          = 0.0d0
      JetRecAreaRatio = 0.2d0
      Urec=0.2d0
      Ujet=0.2d0
      Qrec=0.2d0
      Uhc = 0.2d0
      Ain=1.0d0
      Tin = 23.0d0
      Droom=6.0d0
      ZoneCVisMixing=0.0d0
      Rfr=10.0d0
      ZoneCVhasREC=1.0d0
      Hceiling      = 0.0d0
      HWall         = 0.0d0
      HFloor        = 0.0d0
      HInternal     = 0.0d0
      HWindow       = 0.0d0
      HDoor         = 0.0d0

      DO Loop=1,NumOfZones
        IF (AirModel(loop)%AirModelType /= RoomAirModel_UCSDCV) cycle  !don't set these up if they don't make sense
        ZoneEquipConfigNum = ZoneNum
        ! check whether this zone is a controlled zone or not
        IF (ZoneEquipConfig(ZoneEquipConfigNum)%IsControlled) THEN
          IsZoneCV(Loop) = .FALSE.
          AirModel(Loop)%SimAirModel= .FALSE.
          CALL ShowSevereError('Unmixed Flow: Cross Ventilation cannot be applied for Zone='//TRIM(zone(loop)%Name))
          CALL ShowContinueError('An HVAC system is present in the zone. Fully mixed airflow model will be used for Zone='//  &
                                 TRIM(zone(loop)%Name))
          cycle
        ENDIF
        !CurrentModuleObject='RoomAirSettings:CrossVentilation'
        CALL SetupOutputVariable('Room Air Zone Jet Region Temperature [C]',ZTjet(Loop),'Zone','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Recirculation Region Temperature [C]', &
                                  ZTrec(Loop),'Zone','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Jet Region Average Air Velocity [m/s]',Ujet(Loop),'Zone','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Recirculation Region Average Air Velocity [m/s]', &
                                  Urec(Loop),'Zone','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Recirculation and Inflow Rate Ratio []',RecInflowRatio(Loop),'Zone','Average',  &
            Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Inflow Opening Area [m2]',Ain(Loop),'Zone','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Room Length [m]',Dstar(Loop),'Zone','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Is Mixing Status []',ZoneCVisMixing(Loop),'Zone','State',Zone(Loop)%Name)
        CALL SetupOutputVariable('Room Air Zone Is Recirculating Status []',ZoneCVhasREC(Loop),  &
                                        'Zone','State',Zone(Loop)%Name)
        DO i=1,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
          N = AirflowNetworkLinkageData(i)%CompNum
          IF (AirflowNetworkCompData(N)%CompTypeNum==CompTypeNum_DOP) THEN
            SurfNum = MultizoneSurfaceData(i)%SurfNum
            CALL SetupOutputVariable('Room Air Window Jet Region Average Air Velocity [m/s]',  &
             CVJetRecFlows(Loop,i)%Ujet,'Zone','Average', &
            MultizoneSurfaceData(i)%SurfName)
          END IF
        END DO
      ENDDO
    ENDIF

    MyEnvrnFlag = .TRUE.

    MyOneTimeFlag = .false.

  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(ZoneNum)) THEN

    IF (IsZoneDV(ZoneNum) .OR. IsZoneUI(ZoneNum)) THEN

      MaxTempGrad(ZoneNum) = 0.0d0
      AvgTempGrad(ZoneNum) = 0.0d0
      TCMF(ZoneNum)=23.0d0
      FracMinFlow(ZoneNum)   = 0.0d0
      ZoneAirSystemON(ZoneNum) = .FALSE.
      MATFloor(ZoneNum) = 23.0d0
      XMATFloor(ZoneNum) = 23.0d0
      XM2TFloor(ZoneNum) = 23.0d0
      XM3TFloor(ZoneNum) = 23.0d0
      XM4TFloor(ZoneNum) = 23.0d0
      DSXMATFloor(ZoneNum) = 23.0d0
      DSXM2TFloor(ZoneNum) = 23.0d0
      DSXM3TFloor(ZoneNum) = 23.0d0
      DSXM4TFloor(ZoneNum) = 23.0d0
      MATOC(ZoneNum) = 23.0d0
      XMATOC(ZoneNum) = 23.0d0
      XM2TOC(ZoneNum) = 23.0d0
      XM3TOC(ZoneNum) = 23.0d0
      XM4TOC(ZoneNum) = 23.0d0
      DSXMATOC(ZoneNum) = 23.0d0
      DSXM2TOC(ZoneNum) = 23.0d0
      DSXM3TOC(ZoneNum) = 23.0d0
      DSXM4TOC(ZoneNum) = 23.0d0
      MATMX(ZoneNum) = 23.0d0
      XMATMX(ZoneNum) = 23.0d0
      XM2TMX(ZoneNum) = 23.0d0
      XM3TMX(ZoneNum) = 23.0d0
      XM4TMX(ZoneNum) = 23.0d0
      DSXMATMX(ZoneNum) = 23.0d0
      DSXM2TMX(ZoneNum) = 23.0d0
      DSXM3TMX(ZoneNum) = 23.0d0
      DSXM4TMX(ZoneNum) = 23.0d0
      ZTM1Floor(ZoneNum) = 23.0d0
      ZTM2Floor(ZoneNum) = 23.0d0
      ZTM3Floor(ZoneNum) = 23.0d0
      Zone1Floor(ZoneNum) = 23.0d0
      ZoneMXFloor(ZoneNum) = 23.0d0
      ZoneM2Floor(ZoneNum) = 23.0d0
      ZTM1OC(ZoneNum) = 23.0d0
      ZTM2OC(ZoneNum) = 23.0d0
      ZTM3OC(ZoneNum) = 23.0d0
      Zone1OC(ZoneNum) = 23.0d0
      ZoneMXOC(ZoneNum) = 23.0d0
      ZoneM2OC(ZoneNum) = 23.0d0
      ZTM1MX(ZoneNum) = 23.0d0
      ZTM2MX(ZoneNum) = 23.0d0
      ZTM3MX(ZoneNum) = 23.0d0
      Zone1MX(ZoneNum) = 23.0d0
      ZoneMXMX(ZoneNum) = 23.0d0
      ZoneM2MX(ZoneNum) = 23.0d0
      AIRRATFloor(ZoneNum)=0.0d0
      AIRRATOC(ZoneNum)=0.0d0
      AIRRATMX(ZoneNum)=0.0d0
      ZTOC(ZoneNum) = 23.0d0
      ZTMX(ZoneNum) = 23.0d0
      ZTFLOOR(ZoneNum) = 23.0d0
      HeightTransition(ZoneNum) = 0.0d0
      Phi(ZoneNum) = 0.0d0
      Hceiling      = 0.0d0
      HWall         = 0.0d0
      HFloor        = 0.0d0
      HInternal     = 0.0d0
      HWindow       = 0.0d0
      HDoor         = 0.0d0

    END IF

    IF (IsZoneDV(ZoneNum)) THEN

      DVHcIn = 0.0d0
      ZoneDVMixedFlagRep(ZoneNum)    = 0.0d0
      ZoneDVMixedFlag(ZoneNum)=0

    END IF

    IF (IsZoneUI(ZoneNum)) THEN

      UFHcIn = 0.0d0
      ZoneUFMixedFlag(ZoneNum) = 0
      ZoneUFMixedFlagRep(ZoneNum) = 0.0d0
      ZoneUFGamma(ZoneNum) = 0.0d0
      ZoneUFPowInPlumes(ZoneNum) = 0.0d0
      ZoneUFPowInPlumesfromWindows(ZoneNum) = 0.0d0

    END IF


    IF (ISZoneCV(ZoneNum)) THEN
      ZTjet(ZoneNum) = 23.0d0
      RoomOutflowTemp(ZoneNum) = 23.0d0
      ZTrec(ZoneNum) = 23.0d0
      CVHcIn        = 0.0d0
      JetRecAreaRatio(ZoneNum) = 0.2d0
      Urec(ZoneNum)=0.2d0
      Ujet(ZoneNum)=0.2d0
      Uhc(ZoneNum) = 0.2d0
      Ain(ZoneNum)=1.0d0
      Tin(ZoneNum) = 23.0d0
      Droom(ZoneNum)=6.0d0
      Dstar(ZoneNum)=6.0d0
      ZoneCVisMixing(ZoneNum)=0.0d0
      Rfr(ZoneNum)=10.0d0
      ZoneCVhasREC(ZoneNum)=1.0d0
      Hceiling      = 0.0d0
      HWall         = 0.0d0
      HFloor        = 0.0d0
      HInternal     = 0.0d0
      HWindow       = 0.0d0
      HDoor         = 0.0d0

    END IF

    MyEnvrnFlag(ZoneNum) = .FALSE.
  END IF ! end one time inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ZoneNum) = .true.
  ENDIF

  RETURN

END SUBROUTINE SharedDVCVUFDataInit

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
END MODULE RoomAirModelManager
