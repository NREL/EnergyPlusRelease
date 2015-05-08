MODULE RoomAirModelUserTempPattern

          ! MODULE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2005 (started in January 2004)
          !       RE-ENGINEERED

          ! PURPOSE OF THIS MODULE:
          ! This module is the main module for running the
          ! user-defined temperature pattern model.
          ! This "air model" doesn't predict anything about the room air
          ! but provides a method for users to model the
          ! impact of non-uniform air temps.  the distribution of air temperatures
          ! is defined by the user and referred to as a "pattern"

          ! METHODOLOGY EMPLOYED:
          ! This module contains all subroutines required by the
          ! user defined temperature pattern roomair modeling.
          ! See DataRoomAir.f90 for variable declarations
          !

          ! REFERENCES:
          ! none


          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals,       ONLY : MaxNameLength, DisplayExtraWarnings
    USE DataInterfaces,    ONLY : ShowWarningError, ShowFatalError
    USE DataRoomAirModel

    IMPLICIT NONE         ! Enforce explicit typing of all variables

          ! MODULE PARAMETER DEFINITIONS:

          ! MODULE DERIVED TYPE DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! see DataRoomAir

          ! SUBROUTINE SPECIFICATIONS FOR MODULE TempDistSimMgr

         ! main subsroutine
    PUBLIC ManageUserDefinedPatterns

    ! get input routines are in RoomAirManager.f90

    ! Routines for transferring data between Heat Balance and Air model domains
    PRIVATE GetSurfHBDataForTempDistModel ! also does some initializations
    PRIVATE SetSurfHBDataForTempDistModel

        ! Routines for actual calculations in TempDist model
    PRIVATE InitTempDistModel
    PRIVATE CalcTempDistModel
    PRIVATE FigureSurfMapPattern
    PRIVATE FigureHeightPattern
    PRIVATE FigureTwoGradInterpPattern
    PRIVATE FigureConstGradPattern
    PUBLIC  FigureNDheightInZone ! called by roomairmanager where get input routines


    CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE ManageUserDefinedPatterns(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   January 2004/Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  manage the user-defined air temp. distribution model

          ! METHODOLOGY EMPLOYED:
          ! calls subroutines

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: ZoneNum      ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

    ! transfer data from surface domain to air domain for the specified zone
    CALL InitTempDistModel(ZoneNum)


    CALL GetSurfHBDataForTempDistModel(ZoneNum)

    ! perform TempDist model calculations
    CALL CalcTempDistModel(ZoneNum)

    ! transfer data from air domain back to surface domain for the specified zone
    CALL SetSurfHBDataForTempDistModel(ZoneNum)

    RETURN

END SUBROUTINE ManageUserDefinedPatterns

!****************************************************
SUBROUTINE InitTempDistModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals , ONLY: NumOfZones, BeginEnvrnFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                 :: ZoneNum  ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! flag for init once at start of environment
  LOGICAL, SAVE                            :: MyOneTimeFlag = .true. ! one time setup flag
  INTEGER                                  :: SurfNum ! do loop counter

  If (MyOneTimeFlag) then
    ALLOCATE(MyEnvrnFlag(NumOfZones))
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  ENDIF

  If (BeginEnvrnFlag .and. MyEnvrnFlag(ZoneNum) ) Then
    AirPatternZoneInfo(ZoneNum)%TairMean = 23.0d0
    AirPatternZoneInfo(ZoneNum)%Tstat    = 23.0d0
    AirPatternZoneInfo(ZoneNum)%Tleaving = 23.0d0
    AirPatternZoneInfo(ZoneNum)%Texhaust = 23.0d0
    AirPatternZoneInfo(ZoneNum)%Gradient = 0.0D0
    Do SurfNum = 1, AirPatternZoneInfo(ZoneNum)%totNumSurfs
      AirPatternZoneInfo(ZoneNum)%Surf(SurfNum)%TadjacentAir = 23.0d0
    ENDDO
    MyEnvrnFlag(ZoneNum) =  .FALSE.
  endif

  If (.not. BeginEnvrnFlag)  MyEnvrnFlag(ZoneNum) = .TRUE.

  ! init report variable
  AirPatternZoneInfo(ZoneNum)%Gradient = 0.0D0

  RETURN

END SUBROUTINE InitTempDistModel


SUBROUTINE GetSurfHBDataForTempDistModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  map data from Heat Balance domain to Room Air Modeling Domain
          !  for the current zone, (only need mean air temp)
          !  also acts as an init routine

          ! METHODOLOGY EMPLOYED:
          ! use ZT from DataHeatBalFanSys

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    USE DataHeatBalFanSys,       ONLY : MAT, ZT, ZTAV
    USE DataHeatBalance   ,      ONLY : zone
    USE InputProcessor    ,      ONLY : FindItem
    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: ZoneNum  ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused    INTEGER    :: thisZoneInfo

    !intialize in preperation for calculations
   AirPatternZoneInfo(ZoneNum)%Tstat             = MAT(zoneNum)
   AirPatternZoneInfo(ZoneNum)%Tleaving          = MAT(zoneNum)
   AirPatternZoneInfo(ZoneNum)%Texhaust          = MAT(zoneNum)
   AirPatternZoneInfo(ZoneNum)%Surf%TadjacentAir = MAT(zoneNum)

    ! the only input this method needs is the zone MAT or ZT or ZTAV  ?  (original was ZT)
    AirPatternZoneInfo(ZoneNum)%TairMean          = MAT(zoneNum) ! this is lagged from previous corrector result


    RETURN

END SUBROUTINE GetSurfHBDataForTempDistModel


!*****************************************************************************************

SUBROUTINE CalcTempDistModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! figure out which pattern is scheduled and call
          ! appropriate subroutine

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataSurfaces,        ONLY : Surface, AdjacentAirTemp, ZoneMeanAirTemp
    USE ScheduleManager ,    ONLY : GetCurrentScheduleValue
    USE DataHeatBalance ,    ONLY : Zone
    USE InputProcessor  ,    ONLY : FindItem
    Use OutputReportTabular, ONLY : IntToStr
    USE FluidProperties,     ONLY : FindArrayIndex ! routine should be moved to a general module rather than FluidProperties
    USE General,             ONLY : FindNumberinList

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: ZoneNum          ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused    INTEGER    :: thisZoneInfo
    REAL(r64)  :: AvailTest
    INTEGER    :: CurntPatternKey
    INTEGER    :: CurPatrnID
!unused    INTEGER    :: thisZoneInfoSurf
!unused    INTEGER    :: lowSideID
!unused    INTEGER    :: highSideID
!unused    REAL(r64)  :: thisZeta
!unused    REAL(r64)  :: lowSideZeta
!unused    REAL(r64)  :: hiSideZeta
!unused    REAL(r64)  :: fractBtwn
!unused    REAL(r64)  :: tmpDeltaTai


  !first determine availability
  AvailTest = GetCurrentScheduleValue(AirPatternZoneInfo(ZoneNum)%AvailSchedID)

  If ((AvailTest /= 1.0d0) .OR. ( .NOT. AirPatternZoneInfo(ZoneNum)%IsUsed) ) Then
  ! model not to be used. Use complete mixing method

    AirPatternZoneInfo(ZoneNum)%Tstat     = AirPatternZoneInfo(ZoneNum)%TairMean
    AirPatternZoneInfo(ZoneNum)%Tleaving  = AirPatternZoneInfo(ZoneNum)%TairMean
    AirPatternZoneInfo(ZoneNum)%Texhaust  = AirPatternZoneInfo(ZoneNum)%TairMean
    AirPatternZoneInfo(ZoneNum)%Surf%TadjacentAir = AirPatternZoneInfo(ZoneNum)%TairMean

    RETURN

  ELSE  ! choose pattern and call subroutine

    CurntPatternKey = GetCurrentScheduleValue(AirPatternZoneInfo(ZoneNum)%PatternSchedID)

    CurPatrnID  = FindNumberinList(CurntPatternKey,RoomAirPattern%PatrnID,NumAirTempPatterns)

    If (CurPatrnID == 0) Then
      ! throw error here ? way to test schedules before getting to this point?
      CALL showFatalError('User defined room air pattern index not found: '//trim(IntToStr(CurntPatternKey)) )
      RETURN
    ENDIF

    Select Case (RoomAirPattern(CurPatrnID)%PatternMode)

    CASE(ConstGradTempPattern)

      CALL FigureConstGradPattern(CurPatrnID, ZoneNum)

    CASE(TwoGradInterpPattern)

      CALL FigureTwoGradInterpPattern(CurPatrnID, ZoneNum)

    CASE(NonDimenHeightPattern)

      CALL FigureHeightPattern(CurPatrnID, ZoneNum)

    CASE(SurfMapTempPattern)

      CALL FigureSurfMapPattern(CurPatrnID, ZoneNum)

    CASE DEFAULT
      !should not come here

    END SELECT


  ENDIF ! availability control construct

  RETURN

END SUBROUTINE CalcTempDistModel

SUBROUTINE FigureSurfMapPattern(PattrnID, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! main calculation routine for surface pattern

          ! METHODOLOGY EMPLOYED:
          ! simple polling and applying prescribed
          ! delta Tai's to current mean air temp
          ! on a surface by surface basis

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: FindNumberinList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: PattrnID
  INTEGER, INTENT(IN)  :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)     :: Tmean
  INTEGER  :: found
  INTEGER  :: i

  Tmean = AirPatternZoneInfo(ZoneNum)%TairMean

  DO i=1, AirPatternZoneInfo(ZoneNum)%totNumSurfs
      ! cycle through zone surfaces and look for match
      found = FindNumberInList(AirPatternZoneInfo(ZoneNum)%Surf(i)%SurfID,   &
                     RoomAirPattern(PattrnID)%MapPatrn%SurfID, RoomAirPattern(PattrnID)%MapPatrn%numSurfs)
      IF (found /= 0) THEN ! if surf is in map then assign, else give it MAT
       AirPatternZoneInfo(ZoneNum)%Surf(i)%TadjacentAir = RoomAirPattern(PattrnID)%MapPatrn%DeltaTai(found) + Tmean
      ELSE
       AirPatternZoneInfo(ZoneNum)%Surf(i)%TadjacentAir = Tmean
      ENDIF
  ENDDO

  AirPatternZoneInfo(ZoneNum)%Tstat    =  RoomAirPattern(PattrnID)%DeltaTstat    + Tmean
  AirPatternZoneInfo(ZoneNum)%Tleaving =  RoomAirPattern(PattrnID)%DeltaTleaving + Tmean
  AirPatternZoneInfo(ZoneNum)%Texhaust =  RoomAirPattern(PattrnID)%DeltaTexhaust + Tmean

  RETURN

END SUBROUTINE FigureSurfMapPattern


SUBROUTINE FigureHeightPattern(PattrnID, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate the pattern for non-dimensional vertical profile

          ! METHODOLOGY EMPLOYED:
          ! treat profile as lookup table and interpolate

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE FluidProperties,  ONLY : FindArrayIndex !
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: PattrnID
  INTEGER, INTENT(IN)  :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: Tmean !
  Integer :: lowSideID
  INTEGER :: highSideID
  REAL(r64)    :: thisZeta
  INTEGER :: I
  REAL(r64)    :: lowSideZeta
  REAL(r64)    :: hiSideZeta
  REAL(r64)    :: fractBtwn
  REAL(r64)    :: tmpDeltaTai

  tmpDeltaTai = 0.0d0
  Tmean = AirPatternZoneInfo(ZoneNum)%TairMean

  Do i=1, AirPatternZoneInfo(ZoneNum)%totNumSurfs

        thisZeta = AirPatternZoneInfo(ZoneNum)%Surf(i)%Zeta
        lowSideID = FindArrayIndex(thisZeta,RoomAirPattern(PattrnID)%VertPatrn%ZetaPatrn)
        highSideID = lowSideID + 1
        If (lowSideID == 0) lowSideID = 1  !protect against array bounds

        lowSideZeta = RoomAirPattern(PattrnID)%VertPatrn%ZetaPatrn(lowSideID)
        IF (highSideID <= SIZE(RoomAirPattern(PattrnID)%VertPatrn%ZetaPatrn) ) THEN
          hiSideZeta  = RoomAirPattern(PattrnID)%VertPatrn%ZetaPatrn(highSideID)
        ELSE !trap array bounds
          hiSideZeta  = lowSideZeta
        ENDIF
        IF ((hiSideZeta - lowSideZeta) /= 0.0d0) THEN !
          fractBtwn   = (thisZeta - lowSideZeta)/(hiSideZeta - lowSideZeta)
          tmpDeltaTai =  RoomAirPattern(PattrnID)%VertPatrn%DeltaTaiPatrn(lowSideID) &
                       + fractBtwn * ( RoomAirPattern(PattrnID)%VertPatrn%DeltaTaiPatrn(highsideID)  &
                                        - RoomAirPattern(PattrnID)%VertPatrn%DeltaTaiPatrn(lowSideID )  )

        ELSE ! would divide by zero, using low side value

          tmpDeltaTai = RoomAirPattern(PattrnID)%VertPatrn%DeltaTaiPatrn(lowSideID)

        ENDIF

        AirPatternZoneInfo(ZoneNum)%Surf(i)%TadjacentAir = tmpDeltaTai + Tmean

  ENDDO !surfaces in this zone

  AirPatternZoneInfo(ZoneNum)%Tstat    =  RoomAirPattern(PattrnID)%DeltaTstat    + Tmean
  AirPatternZoneInfo(ZoneNum)%Tleaving =  RoomAirPattern(PattrnID)%DeltaTleaving + Tmean
  AirPatternZoneInfo(ZoneNum)%Texhaust =  RoomAirPattern(PattrnID)%DeltaTexhaust + Tmean

  RETURN

END SUBROUTINE FigureHeightPattern


SUBROUTINE FigureTwoGradInterpPattern(PattrnID, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Aug 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate two gradient interpolation pattern

          ! METHODOLOGY EMPLOYED:
          ! Case statement controls how interpolations are done
          ! based on user selected mode.
          ! calculations vary by mode

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataHeatBalance, ONLY: Zone, SNLoadCoolRate, SNLoadHeatRate
  USE DataGlobals,     ONLY: NumOfZones
  USE DataInterfaces,  ONLY: SetupOutputVariable

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: PattrnID
  INTEGER, INTENT(IN)  :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Tmean ! MAT deg C
  REAL(r64)  :: Grad ! vertical temperature gradient C/m
  REAL(r64)  :: DeltaT ! temperature difference
  REAL(r64)  :: CoolLoad ! sensible cooling load
  REAL(r64)  :: HeatLoad ! sensible heating load
  REAL(r64)  :: ZetaTmean ! non-dimensional height for mean air temp
  INTEGER :: I ! do loop index
  REAL(r64)  :: thisZeta !non-dimensional height
  REAL(r64)  :: DeltaHeight ! height difference in m
  REAL(r64)  :: tempDeltaTai ! temporary temperature difference
  LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: SetupOutputFlag !flag to set up output variable one-time if 2-grad model used
  Logical, Save :: MyOneTimeFlag = .TRUE.

  IF (MyOneTimeFlag) THEN
    ALLOCATE(SetupOutputFlag(NumOfZones))
    SetupOutputFlag = .TRUE. ! init
    MyOneTimeFlag = .FALSE.
  ENDIF

  IF (SetupOutputFlag(ZoneNum)) THEN
    CALL SetupOutputVariable('Room Air Zone Vertical Temperature Gradient [K/m]', &
                              AirPatternZoneInfo(ZoneNum)%Gradient,'HVAC','State',&
                              AirPatternZoneInfo(ZoneNum)%ZoneName)

    SetupOutputFlag(ZoneNum) = .FALSE.
  ENDIF


  Tmean = AirPatternZoneInfo(ZoneNum)%TairMean

  !determine gradient depending on mode
  SELECT CASE (RoomAirPattern(PattrnID)%TwoGradPatrn%InterpolationMode)

  CASE(OutdoorDrybulbMode)
    IF (Zone(ZoneNum)%OutDryBulbTemp >= RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale) THEN
      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient

    ELSEIF (Zone(ZoneNum)%OutDryBulbTemp <= RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) THEN

      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
    ELSE ! interpolate

     IF ((RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale -   &
            RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) /= 0.0d0) then
      ! bad user input. should be trapped during get input in RoomAirManager.f90
      Grad  = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
     ELSE

      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient &
              + ( (Zone(ZoneNum)%OutDryBulbTemp - RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) &
                 / (RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale - &
                         RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) ) &
                 * (RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient - RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient)

     ENDIF
    ENDIF

  CASE(ZoneAirTempMode)

    IF ( Tmean >= RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale) THEN
      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient

    ELSEIF ( Tmean <= RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) THEN

      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
    ELSE ! interpolate
      IF ((RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale -  &
             RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) == 0.0d0) then
        ! bad user input, trapped during get input
        Grad  = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
      ELSE

        Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient &
              + ( (Tmean - RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) &
                 / (RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale - &
                            RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) ) &
                 * (RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient - RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient)

      ENDIF
    ENDIF

  CASE(DeltaOutdoorZone)
    DeltaT = Zone(ZoneNum)%OutDryBulbTemp -  Tmean
    IF ( DeltaT >= RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale) THEN
      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient

    ELSEIF ( DeltaT <= RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) THEN

      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
    ELSE ! interpolate
      IF ((RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale -   &
               RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) == 0.0d0) then
        ! bad user input, trapped during get input
        Grad  = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
      ELSE

        Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient &
              + ( (DeltaT - RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) &
                 / (RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundTempScale - &
                          RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundTempScale) ) &
                 * (RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient - RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient)
      ENDIF
    ENDIF

  CASE(SensibleCoolingMode)

    CoolLoad = SNLoadCoolRate(ZoneNum)
    IF ( CoolLoad >= RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundHeatRateScale) THEN
      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient

    ELSEIF ( CoolLoad <= RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) THEN

      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
    ELSE ! interpolate
      IF ((RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundHeatRateScale -   &
             RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) == 0.0d0) then
        Grad  = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
      ELSE

        Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient &
              + ( (CoolLoad - RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) &
                 / (RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundHeatRateScale - &
                      RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) ) &
                 * (RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient - RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient)
      ENDIF
    ENDIF

  CASE(SensibleHeatingMode)

    HeatLoad = SNLoadHeatRate(ZoneNum)
    IF ( HeatLoad >= RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundHeatRateScale) THEN
      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient

    ELSEIF ( HeatLoad <= RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) THEN

      Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
    ELSE ! interpolate
      IF ((RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundHeatRateScale -   &
               RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) == 0.0d0) then
        Grad  = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient
      ELSE

        Grad = RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient &
              + ( (HeatLoad - RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) &
                 / (RoomAirPattern(PattrnID)%TwoGradPatrn%UpperBoundHeatRateScale - &
                        RoomAirPattern(PattrnID)%TwoGradPatrn%LowerBoundHeatRateScale) ) &
                 * (RoomAirPattern(PattrnID)%TwoGradPatrn%HiGradient - RoomAirPattern(PattrnID)%TwoGradPatrn%LowGradient)
      ENDIF
    ENDIF

  END SELECT

  ZetaTmean = 0.5d0 ! by definition,

  DO i=1, AirPatternZoneInfo(ZoneNum)%totNumSurfs
    thisZeta = AirPatternZoneInfo(ZoneNum)%surf(i)%zeta

    DeltaHeight = -1.0d0*(ZetaTmean - thisZeta)* AirPatternZoneInfo(ZoneNum)%ZoneHeight

    tempDeltaTai = DeltaHeight * Grad

    AirPatternZoneInfo(ZoneNum)%surf(i)%TadjacentAir = tempDeltaTai + Tmean

  ENDDO

  AirPatternZoneInfo(ZoneNum)%Tstat    =  -1.0d0*(0.5d0*AirPatternZoneInfo(ZoneNum)%ZoneHeight- &
                                                RoomAirPattern(PattrnID)%TwoGradPatrn%TstatHeight) &
                                            * Grad + Tmean
  AirPatternZoneInfo(ZoneNum)%Tleaving =  -1.0d0*(0.5d0*AirPatternZoneInfo(ZoneNum)%ZoneHeight-   &
                                                RoomAirPattern(PattrnID)%TwoGradPatrn%TleavingHeight) &
                                            * Grad + Tmean
  AirPatternZoneInfo(ZoneNum)%Texhaust  =  -1.0d0*(0.5d0*AirPatternZoneInfo(ZoneNum)%ZoneHeight-  &
                                                 RoomAirPattern(PattrnID)%TwoGradPatrn%TexhaustHeight) &
                                            * Grad + Tmean

  AirPatternZoneInfo(ZoneNum)%Gradient  = Grad

  RETURN

END SUBROUTINE FigureTwoGradInterpPattern



SUBROUTINE FigureConstGradPattern(PattrnID, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2005
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
  INTEGER, INTENT(IN)  :: PattrnID
  INTEGER, INTENT(IN)  :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  REAL(r64)     :: Tmean ! MAT
  INTEGER  :: i ! loop counter
  REAL(r64)     :: grad ! vertical temperature gradient
  REAL(r64)     :: ZetaTmean ! non-dimens. height for MAT, 0.5
  REAL(r64)     :: thisZeta ! temporary non-dimens height
  REAL(r64)     :: DeltaHeight ! temporary height difference
  REAL(r64)     :: tempDeltaTai ! temporary Delta Tai

  Tmean = AirPatternZoneInfo(ZoneNum)%TairMean
  Grad  = RoomAirPattern(PattrnID)%GradPatrn%Gradient

  ZetaTmean = 0.5d0 ! by definition,

  DO i=1, AirPatternZoneInfo(ZoneNum)%totNumSurfs
    thisZeta = AirPatternZoneInfo(ZoneNum)%surf(i)%zeta
    DeltaHeight = -1.0d0*(ZetaTmean - thisZeta)* AirPatternZoneInfo(ZoneNum)%ZoneHeight
    tempDeltaTai = DeltaHeight * Grad
    AirPatternZoneInfo(ZoneNum)%surf(i)%TadjacentAir = tempDeltaTai + Tmean
  ENDDO

  AirPatternZoneInfo(ZoneNum)%Tstat    =  RoomAirPattern(PattrnID)%DeltaTstat    + Tmean
  AirPatternZoneInfo(ZoneNum)%Tleaving =  RoomAirPattern(PattrnID)%DeltaTleaving + Tmean
  AirPatternZoneInfo(ZoneNum)%Texhaust =  RoomAirPattern(PattrnID)%DeltaTexhaust + Tmean

  RETURN

END SUBROUTINE FigureConstGradPattern


!*****************************************************************************************
  REAL(r64) FUNCTION FigureNDheightInZone(thisHBsurf)
      ! FUNCTION INFORMATION:
          !       AUTHOR         B.Griffith
          !       DATE WRITTEN   aug 2005, Jan2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! return a non-dimensional height zeta

          ! METHODOLOGY EMPLOYED:
          ! figure average floor height (follows code in surfacegeometry.f90
          ! use ceiling height from Zone structure
          ! non dimensionalize surface's centroid's Z value
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    USE DataSurfaces   ,    ONLY : Surface, SurfaceClass_Floor, SurfaceClass_Wall
    USE DataHeatBalance,    ONLY : Zone
    Use DataInterfaces,     ONLY: ShowWarningError, ShowContinueError
    Use General, Only: RoundSigDigits
    USE DataErrorTracking,  ONLY : TotalRoomAirPatternTooLow, TotalRoomAirPatternTooHigh

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: thisHBsurf  ! index in main Surface array

          ! FUNCTION PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: TolValue=.0001d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na


          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER  :: thisZone  !
    REAL(r64)     :: ZoneZorig
    REAL(r64)     :: ZoneCeilHeight
    REAL(r64)     :: Zcm
    REAL(r64)     :: SurfMinZ
    REAL(r64)     :: SurfMaxZ
    REAL(r64)     :: Zeta
    REAL(r64)     :: FloorCount
    REAL(r64)     :: ZFlrAvg
    REAL(r64)     :: ZMax
    REAL(r64)     :: ZMin
    INTEGER  :: Count
    INTEGER  :: SurfNum
    REAL(r64)     :: Z1
    REAL(r64)     :: Z2

    ! Get the centroid height for the surface
    Zcm = Surface(thisHBsurf)%centroid%z
    thisZone = surface(thisHBsurf)%zone

   !this next Do block is copied from SurfaceGeometry.f90 with modification for just floor Z
   ! used find floor z.
    FloorCount=0.0d0
    ZFlrAvg=0.0d0
    ZMax=0.0d0
    ZMin=0.0d0
    Count=0
    DO SurfNum=Zone(thisZone)%SurfaceFirst,Zone(thisZone)%SurfaceLast
      IF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
        ! Use Average Z for surface, more important for roofs than floors...
        FloorCount=FloorCount+1.0d0
        Z1=MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2=MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        ZFlrAvg=ZFlrAvg+(Z1+Z2)/2.0d0
      ENDIF
      IF (Surface(SurfNum)%Class == SurfaceClass_Wall) THEN
        ! Use Wall calculation in case no floor in zone
        Count=Count+1
        IF (Count == 1) THEN
          ZMax=Surface(SurfNum)%Vertex(1)%Z
          ZMin=ZMax
        ENDIF
        ZMax=MAX(ZMax,MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z))
        ZMin=MIN(ZMin,MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z))
      ENDIF
    ENDDO
    IF (FloorCount > 0.0d0) THEN
      ZFlrAvg=ZFlrAvg/FloorCount
    ELSE
      ZFlrAvg=ZMin
    endif
    ZoneZorig = ZFlrAvg  ! Z floor  [M]
    ZoneCeilHeight =  zone(thisZone)%CeilingHeight

    !first check if some basic things are reasonable

    SurfMinZ   = MINVAL(surface(thisHBsurf)%vertex%Z)
    SurfMaxZ   = MAXVAL(surface(thisHBsurf)%vertex%Z)

    IF (SurfMinZ < (ZoneZorig-TolValue)) then
      IF (DisplayExtraWarnings) THEN
        call ShowWarningError('RoomAirModelUserTempPattern: Problem in non-dimensional height calculation')
        call ShowContinueError('too low surface: '//trim(surface(thisHBsurf)%name)//' in zone: '//trim(zone(thisZone)%name  ) )
        call ShowContinueError('**** Average floor height of zone is: '//trim(RoundSigDigits(ZoneZorig, 3) ) )
        call ShowContinueError('**** Surface minimum height is: '//trim(RoundSigDigits(SurfMinZ , 3) ) )
      ELSE
        TotalRoomAirPatternTooLow=TotalRoomAirPatternTooLow+1
      ENDIF
    ENDIF

    IF (SurfMaxZ > (ZoneZorig + ZoneCeilHeight + TolValue)  ) then
      IF (DisplayExtraWarnings) THEN
        call ShowWarningError('RoomAirModelUserTempPattern: Problem in non-dimensional height calculation')
        call ShowContinueError(' too high surface: '//trim(surface(thisHBsurf)%name )//' in zone: '//trim(zone(thisZone)%name) )
        call ShowContinueError('**** Average Ceiling height of zone is: '//trim(RoundSigDigits((ZoneZorig + ZoneCeilHeight), 3) ) )
        call ShowContinueError('**** Surface Maximum height is: '//trim(RoundSigDigits(SurfMaxZ , 3) ) )
      ELSE
        TotalRoomAirPatternTooHigh=TotalRoomAirPatternTooHigh+1
      ENDIF
    ENDIF

    !non dimensionalize.
     Zeta = (Zcm - ZoneZorig)  / ZoneCeilHeight
    ! bound so that floors and ceiling are just in from endpoints.

    If (Zeta > 0.99d0 ) Zeta  = 0.99d0

    If (Zeta < 0.01d0)  Zeta  = 0.01d0

    FigureNDheightInZone = zeta

  END FUNCTION  FigureNDheightInZone
!***************************************************
  SUBROUTINE SetSurfHBDataForTempDistModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2005,Feb. 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  map data from air domain back to surface domain for each zone
          !  collects code couples to remote data structures

          ! METHODOLOGY EMPLOYED:
          ! sets values in Heat balance variables
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    USE DataEnvironment,            ONLY : OutBaroPress
    USE DataLoopNode,               ONLY : Node
    USE DataSurfaces,               ONLY : Surface, AdjacentAirTemp, ZoneMeanAirTemp, SurfaceWindow,   &
                                           AirFlowWindow_Destination_ReturnAir
    USE DataHeatBalance,            ONLY : Zone, TempEffBulkAir, ZoneIntGain, RefrigCaseCredit
    USE DataZoneEquipment,          ONLY : ZoneEquipConfig
    USE DataHeatBalFanSys,          ONLY : MAT, ZT, TempZoneThermostatSetpoint, TempTstatAir, SysDepZoneLoads, &
                                           ZoneLatentGain
    USE InputProcessor,             ONLY : FindItem
    USE Psychrometrics,             ONLY:PsyHFnTdbW, PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW, PsyHgAirFnWTdb
    USE InternalHeatGains,          ONLY : SumAllReturnAirConvectionGains, SumAllReturnAirLatentGains
    USE DataHVACGlobals,            ONLY : RetTempMax, RetTempMin
    USE DataGlobals,                ONLY : ZoneSizingCalc

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: ZoneNum  ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                         :: SurfFirst    ! index number of the first surface in the zone
  INTEGER                         :: SurfLast
  REAL(r64)      :: qretair          ! Heat to return air from lights
  REAL(r64)      :: cpair            ! Air heat capacity [J/kg-K]
  REAL(r64)      :: TempRetAir       ! Return air temperature [C]
  REAL(r64)      :: TempZoneAir      ! Zone air temperature [C]
  INTEGER        :: ReturnNode       ! Node number of controlled zone's return air
  INTEGER        :: ZoneNode         ! Node number of controlled zone
  INTEGER        :: SurfNum          ! Surface number
  REAL(r64)      :: MassFlowRA       ! Return air mass flow [kg/s]
  REAL(r64)      :: FlowThisTS       ! Window gap air mass flow [kg/s]
  REAL(r64)      :: WinGapFlowtoRA   ! Mass flow to return air from all airflow windows in zone [kg/s]
  REAL(r64)      :: WinGapFlowTtoRA  ! Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]
  REAL(r64)      :: WinGapTtoRA      ! Temp of outlet flow mixture to return air from all airflow windows in zone [C]
  REAL(r64)      :: H2OHtOfVap       ! Heat of vaporization of water (W/kg)
  REAL(r64)      :: RhoAir           ! Density of air (Kg/m3)
  REAL(r64)      :: ZoneMult
  REAL(r64)      :: SumRetAirLatentGainRate

          ! FLOW:

  SurfFirst  = Zone(ZoneNum)%SurfaceFirst
  SurfLast   = Zone(ZoneNum)%SurfaceLast

     ! set air system leaving node conditions
   ! this is not so easy.  THis task is normally done in CalcZoneLeavingConditions
   !  but efforts to do this update there were not succesful.
   !  Need to revisit how to best implement this. Ended up taking code from CalcZoneLeavingConditions
   !  ZoneNum is already equal to ActualZoneNum , changed block of source

  IF (AirPatternZoneInfo(ZoneNum)%ZoneNodeID /= 0) THEN
    ! the zone system node should get the conditions leaving the zone (but before return air heat gains are added).
    Node(AirPatternZoneInfo(ZoneNum)%ZoneNodeID)%Temp = AirPatternZoneInfo(ZoneNum)%Tleaving
  ENDIF

  IF( AirPatternZoneInfo(ZoneNum)%ReturnAirNodeID /= 0) THEN
    !BEGIN BLOCK of code from CalcZoneLeavingConditions*********************************
    ReturnNode = AirPatternZoneInfo(ZoneNum)%ReturnAirNodeID
    ZoneNode   = AirPatternZoneInfo(ZoneNum)%ZoneNodeID
    ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
     !RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
     ! Add sensible heat gain from refrigerated cases with under case returns
    CALL SumAllReturnAirConvectionGains(ZoneNum, QRetAir)


    CpAir   = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat, Node(ZoneNode)%Temp)

     ! Need to add the energy to the return air from lights and from airflow windows. Where the heat
     ! is added depends on if there is system flow or not.  If there is system flow the heat is added
     ! to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
     ! Correct step through the SysDepZoneLoads variable.

     MassFlowRA  = Node(ReturnNode)%MassFlowRate / ZoneMult
     TempZoneAir = AirPatternZoneInfo(ZoneNum)%Tleaving ! key difference from
     TempRetAir  = TempZoneAir
     WinGapFlowtoRA = 0.0d0
     WinGapTtoRA = 0.0d0
     WinGapFlowTtoRA = 0.0d0

     DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
       IF(SurfaceWindow(SurfNum)%AirFlowThisTS > 0.0d0 .AND.   &
          SurfaceWindow(SurfNum)%AirflowDestination == AirFlowWindow_Destination_ReturnAir) THEN
         FlowThisTS = PsyRhoAirFnPbTdbW(OutBaroPress,SurfaceWindow(SurfNum)%TAirFlowGapOutlet, Node(ZoneNode)%HumRat) * &
           SurfaceWindow(SurfNum)%AirFlowThisTS * Surface(SurfNum)%Width
         WinGapFlowtoRA = WinGapFlowtoRA + FlowThisTS
         WinGapFlowTtoRA = WinGapFlowTtoRA + FlowThisTS * SurfaceWindow(SurfNum)%TAirFlowGapOutlet
       END IF
     END DO
     IF(WinGapFlowtoRA > 0.0d0) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowtoRA

     IF (.NOT. Zone(ZoneNum)%NoHeatToReturnAir) THEN
       IF(MassFlowRA > 0.0d0) Then
         IF(WinGapFlowtoRA > 0.0d0) THEN
           ! Add heat-to-return from window gap airflow
           IF(MassFlowRA >= WinGapFlowtoRA) THEN
             TempRetAir = (WinGapFlowTtoRA + (MassFlowRA-WinGapFlowtoRA)*TempZoneAir)/MassFlowRA
           ELSE
             ! All of return air comes from flow through airflow windows
             TempRetAir = WinGapTtoRA
             ! Put heat from window airflow that exceeds return air flow into zone air
             SysDepZoneLoads(ZoneNum) = SysDepZoneLoads(ZoneNum) + &
               (WinGapFlowToRA - MassFlowRA) * CpAir*(WinGapTtoRA - TempZoneAir)
          END IF
         END IF
         ! Add heat-to-return from lights
         TempRetAir = TempRetAir + QRetAir/(MassFlowRA * CpAir)
         IF (TempRetAir > RetTempMax) THEN
           Node(ReturnNode)%Temp = RetTempMax
           IF (.not. ZoneSizingCalc) THEN
             SysDepZoneLoads(ZoneNum) = SysDepZoneLoads(ZoneNum) + CpAir*MassFlowRA*(TempRetAir-RetTempMax)
           END IF
         ELSE IF (TempRetAir < RetTempMin) THEN
           Node(ReturnNode)%Temp = RetTempMin
           IF (.not. ZoneSizingCalc) THEN
             SysDepZoneLoads(ZoneNum) = SysDepZoneLoads(ZoneNum) + CpAir*MassFlowRA*(TempRetAir-RetTempMin)
           END IF
         ELSE
           Node(ReturnNode)%Temp = TempRetAir
         END IF
       ELSE  ! No return air flow
         ! Assign all heat-to-return from window gap airflow to zone air
         IF(WinGapFlowToRA > 0.0d0) &
           SysDepZoneLoads(ZoneNum) = SysDepZoneLoads(ZoneNum) + &
             WinGapFlowToRA * CpAir * (WinGapTtoRA - TempZoneAir)
         ! Assign all heat-to-return from lights to zone air
         IF(QRetAir > 0.0d0) &
           SysDepZoneLoads(ZoneNum) = SysDepZoneLoads(ZoneNum) + QRetAir
         Node(ReturnNode)%Temp = Node(ZoneNode)%Temp
       END IF
     ELSE
       Node(ReturnNode)%Temp = Node(ZoneNode)%Temp
     END IF

     ! Update the rest of the Return Air Node conditions, if the return air system exists!
     Node(ReturnNode)%Press = Node(ZoneNode)%Press

     H2OHtOfVap = PsyHgAirFnWTdb(Node(ZoneNode)%HumRat,Node(ReturnNode)%Temp)
     RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ReturnNode)%Temp,Node(ZoneNode)%HumRat)

     ! Include impact of under case returns for refrigerated display cases when updateing return node
     ! humidity ratio
     IF (.NOT. Zone(ZoneNum)%NoHeatToReturnAir) THEN
       IF (MassFlowRA > 0) THEN
         CALL SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)
         Node(ReturnNode)%HumRat = Node(ZoneNode)%HumRat + (SumRetAirLatentGainRate / &
                                 (H2OHtOfVap * MassFlowRA))
       ELSE
       ! If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
         Node(ReturnNode)%HumRat = Node(ZoneNode)%HumRat
         RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone = RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone + &
                                                               RefrigCaseCredit(ZoneNum)%LatCaseCreditToHVAC
                  ! shouldn't the HVAC term be zeroed out then?
         CALL SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)
         ZoneLatentGain(ZoneNum) = ZoneLatentGain(ZoneNum) + SumRetAirLatentGainRate
       END IF
     ELSE
       Node(ReturnNode)%HumRat = Node(ZoneNode)%HumRat
       RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone = RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone + &
                                                               RefrigCaseCredit(ZoneNum)%LatCaseCreditToHVAC
         ! shouldn't the HVAC term be zeroed out then?
       CALL SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)
       ZoneLatentGain(ZoneNum) = ZoneLatentGain(ZoneNum) + SumRetAirLatentGainRate
     END IF

     Node(ReturnNode)%Enthalpy = PsyHFnTdbW(Node(ReturnNode)%Temp,Node(ReturnNode)%HumRat)

     !END BLOCK of code from CalcZoneLeavingConditions*********************************

    ENDIF

     ! set exhaust node leaving temp if present
    If (Allocated(AirPatternZoneInfo(ZoneNum)%ExhaustAirNodeID)) THEN
     ! Do I= 1, size(AirPatternZoneInfo(ZoneNum)%ExhaustAirNodeID) (array assignment instead of do loop)
        Node(AirPatternZoneInfo(ZoneNum)%ExhaustAirNodeID)%Temp = AirPatternZoneInfo(ZoneNum)%Texhaust
     ! ENDDO
    ENDIF

     ! set thermostat reading for air system .
    TempTstatAir(ZoneNum) =  AirPatternZoneInfo(ZoneNum)%Tstat

    ! set results for all surface (note array assignments instead of looping)
    TempEffBulkAir(SurfFirst:SurfLast) = AirPatternZoneInfo(ZoneNum)%surf%TadjacentAir

    ! set flag for reference air temperature mode
    Surface(SurfFirst:SurfLast)%TAirRef = AdjacentAirTemp

  RETURN

END SUBROUTINE SetSurfHBDataForTempDistModel


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
END MODULE RoomAirModelUserTempPattern
