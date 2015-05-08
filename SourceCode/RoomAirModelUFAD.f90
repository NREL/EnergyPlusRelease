MODULE UFADManager

          ! Module containing the routines dealing with the UnderFloor Air
          ! Distribution zone model

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Encapsulate the routines that do the simulation of the UCSD UFAD non-uniform
          ! zone models

          ! METHODOLOGY EMPLOYED:
          ! 2-node zone model with the node heights varying as a function of internal loads
          ! and supply air flow (and other factors)

          ! REFERENCES:
          ! See the EnergyPlus Engineering Reference and the PhD thesis of Anna Liu, UC San Diego

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals
  USE DataLoopNode
  USE DataEnvironment
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataSurfaces
  USE DataRoomAirModel
  USE DataInterfaces
  USE ConvectionCoefficients, ONLY: CalcDetailedHcInForDVModel
  USE DataHVACGlobals, ONLY: SysTimeElapsed, PreviousTimeStep, ShortenTimeStepSysRoomAir
  USE DataUCSDSharedData
  USE General, ONLY: RoundSigDigits

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE VARIABLE DECLARATIONS:
  REAL(r64) :: HAT_MX     =0.0d0 ! HAT_MX Convection Coefficient times Area times Temperature for the upper subzone
  REAL(r64) :: HAT_MXWin  =0.0d0 ! HAT_MX Convection Coefficient times Area times Temperature for the upper subzone (windows only)
  REAL(r64) :: HA_MX      =0.0d0 ! HA_MX Convection Coefficient times Area for the upper subzone
  REAL(r64) :: HA_MXWin   =0.0d0 ! HA_MX Convection Coefficient times Area for the upper subzone (windows only)
  REAL(r64) :: HAT_OC     =0.0d0 ! HAT_OC Convection Coefficient times Area times Temperature for the lower subzone
  REAL(r64) :: HAT_OCWin  =0.0d0 ! HAT_OC Convection Coefficient times Area times Temperature for the lower subzone (windows only)
  REAL(r64) :: HA_OC      =0.0d0 ! HA_OC Convection Coefficient times Area for the lower subzone
  REAL(r64) :: HA_OCWin   =0.0d0 ! HA_OC Convection Coefficient times Area for the lower subzone (windows only)
  REAL(r64) :: HAT_FLOOR  =0.0d0 ! HAT_FLOOR Convection Coefficient times Area times Temperature for the floor(?) subzone
  REAL(r64) :: HA_FLOOR   =0.0d0 ! HA_FLOOR Convection Coefficient times Area for the floor(?) subzone
  REAL(r64) :: HeightFloorSubzoneTop = 0.2d0   ! Assumed thickness of floor subzone
  REAL(r64) :: ThickOccupiedSubzoneMin = 0.2d0 ! Minimum thickness of occupied subzone
  REAL(r64) :: HeightIntMass=0.0d0 ! Height of internal mass surfaces, assumed vertical, cannot exceed ceiling height
  REAL(r64) :: HeightIntMassDefault = 2.0d0 ! Default height of internal mass surfaces

          ! SUBROUTINE SPECIFICATIONS:
  PUBLIC ManageUCSDUFModels
  PRIVATE CalcUCSDUI
  PRIVATE CalcUCSDUE
  PRIVATE InitUCSDUF
  PRIVATE SizeUCSDUF
  PRIVATE HcUCSDUF

CONTAINS

SUBROUTINE ManageUCSDUFModels(ZoneNum, ZoneModelType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August, 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of the 2-node nonuniform zone models for underfloor air
          ! distribution systems (UFAD). Called from RoomAirManager, ManageAirModel

          ! METHODOLOGY EMPLOYED:
          ! uses Init and Calc routines in the standard EPlus manner to manage the calculation
          ! Note that much of the initialization is done in RoomAirManager, SharedDVCVUFDataInit

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals
  USE DataLoopNode
  USE DataEnvironment
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataSurfaces
  USE DataRoomAirModel
  USE ConvectionCoefficients, ONLY: CalcDetailedHcInForDVModel
  USE DataHVACGlobals, ONLY: SysTimeElapsed
  USE DataUCSDSharedData

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum       ! index number for the specified zone
  INTEGER, INTENT(IN) :: ZoneModelType ! type of zone model; UCSDUFI = 6

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ! input was obtained in RoomAirManager, GetUFADIntZoneData

  CALL InitUCSDUF(ZoneNum,ZoneModelType) ! initialize some module variables

  SELECT CASE (ZoneModelType)

    CASE(RoomAirModel_UCSDUFI) ! UCSD UFAD interior zone model
      ! simulate room airflow using the UCSDUFI model
      CALL CalcUCSDUI(ZoneNum)

    CASE(RoomAirModel_UCSDUFE) ! UCSD UFAD interior zone model
      ! simulate room airflow using the UCSDUFI model
      CALL CalcUCSDUE(ZoneNum)

  END SELECT

  RETURN

END SUBROUTINE ManageUCSDUFModels

SUBROUTINE InitUCSDUF(ZoneNum,ZoneModelType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! initialize arrays & variables used by the UCSD UFAD zone models

          ! METHODOLOGY EMPLOYED:
          ! Note that much of the initialization is done in RoomAirManager, SharedDVCVUFDataInit

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN) :: ZoneNum
  INTEGER, INTENT(IN) :: ZoneModelType ! type of zone model; UCSDUFI = 6

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  LOGICAL,SAVE            :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  REAL(r64)               :: NumShadesDown = 0.0d0
  INTEGER :: UINum              ! index to underfloor interior zone model data
  INTEGER :: Ctd=0              ! DO loop index
  INTEGER :: SurfNum = 0        ! surface data structure index

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    HeightFloorSubzoneTop = 0.2d0
    ThickOccupiedSubzoneMin= 0.2d0
    HeightIntMassDefault = 2.0d0
    MyOneTimeFlag = .FALSE.
    ALLOCATE(MySizeFlag(NumOfZones))
    MySizeFlag = .TRUE.
  END IF

  IF ( MySizeFlag(ZoneNum) ) THEN
    CALL SizeUCSDUF(ZoneNum,ZoneModelType)
    MySizeFlag(ZoneNum) = .FALSE.
  END IF

  ! initialize these variables every timestep

  HeightIntMass=HeightIntMassDefault
  ZoneUFGamma(ZoneNum) = 0.0d0
  ZoneUFPowInPlumes(ZoneNum) = 0.0d0
  NumShadesDown = 0.0d0
  DO Ctd = PosZ_Window((ZoneNum-1)*2+1),PosZ_Window((ZoneNum-1)*2+2)
    SurfNum = APos_Window(ctd)
    If (SurfNum == 0) CYCLE
    IF (Surface(SurfNum)%ExtBoundCond == ExternalEnvironment .or. Surface(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or. &
        Surface(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt .or. Surface(SurfNum)%ExtBoundCond == OtherSideCondModeledExt) THEN
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .or. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        NumShadesDown = NumShadesDown + 1
      END IF
    END IF
  END DO
  IF (ZoneModelType == RoomAirModel_UCSDUFE) THEN
    UINum = ZoneUFPtr(ZoneNum)
    IF (ZoneUCSDUE(UINum)%NumExtWin > 1.0d0) THEN
      IF (NumShadesDown/ZoneUCSDUE(UINum)%NumExtWin >= 0.5d0) THEN
        ZoneUCSDUE(UINum)%ShadeDown = .TRUE.
      ELSE
        ZoneUCSDUE(UINum)%ShadeDown = .FALSE.
      END IF
    ELSE
      ZoneUCSDUE(UINum)%ShadeDown = .FALSE.
    END IF
  END IF

  RETURN

END SUBROUTINE InitUCSDUF

SUBROUTINE SizeUCSDUF(ZoneNum,ZoneModelType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! set some smart defaults for UFAD systems

          ! METHODOLOGY EMPLOYED:
          ! use data from Center for Built Environment

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing, ONLY: AutoSize
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN) :: ZoneNum
  INTEGER, INTENT(IN) :: ZoneModelType ! type of zone model; UCSDUFI = 6

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UINum              ! index to underfloor interior zone model data
  INTEGER :: Ctd=0              ! DO loop index
  INTEGER :: SurfNum = 0        ! surface data structure index
  REAL(r64)    :: NumberOfOccupants = 0.0d0 ! design number of occupants in the zone
  REAL(r64)    :: NumberOfPlumes    = 0.0d0 ! design number of plumes in the zone
  REAL(r64)    :: ZoneElecConv      = 0.0d0 ! zone elec equip design convective gain [W]
  REAL(r64)    :: ZoneGasConv       = 0.0d0 ! zone gas equip design convective gain [W]
  REAL(r64)    :: ZoneOthEqConv     = 0.0d0 ! zone other equip design convective gain [W]
  REAL(r64)    :: ZoneHWEqConv      = 0.0d0 ! zone hot water equip design convective gain [W]
  REAL(r64)    :: ZoneSteamEqConv   = 0.0d0 ! zone steam equip design convective gain [W]

    IF (ZoneModelType == RoomAirModel_UCSDUFI) THEN
    UINum = ZoneUFPtr(ZoneNum)
    NumberOfOccupants = 0.0d0
    DO Ctd = 1,TotPeople
      IF(People(Ctd)%ZonePtr == ZoneNum) THEN
        NumberOfOccupants = NumberOfOccupants + People(Ctd)%NumberOfPeople
      ENDIF
    END DO
    IF (ZoneUCSDUI(UINum)%DiffArea == AutoSize) THEN
      IF (ZoneUCSDUI(UINum)%DiffuserType == Swirl) THEN
        ZoneUCSDUI(UINum)%DiffArea = 0.0075d0
      ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == VarArea) THEN
        ZoneUCSDUI(UINum)%DiffArea = .035d0
      ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == DisplVent) THEN
        ZoneUCSDUI(UINum)%DiffArea = 0.0060d0
      ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == LinBarGrille) THEN
      ! 4 ft x 4 inches; 75 cfm per linear foot; area is .025 m2/m
        ZoneUCSDUI(UINum)%DiffArea = .03d0
      ELSE
        ZoneUCSDUI(UINum)%DiffArea = 0.0075d0
      END IF
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionInterior', ZoneUCSDUI(UINum)%ZoneName, &
                              'Design effective area of diffuser', ZoneUCSDUI(UINum)%DiffArea)
    END IF
    IF (ZoneUCSDUI(UINum)%DiffAngle == AutoSize) THEN
      IF (ZoneUCSDUI(UINum)%DiffuserType == Swirl) THEN
        ZoneUCSDUI(UINum)%DiffAngle = 28.d0
      ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == VarArea) THEN
        ZoneUCSDUI(UINum)%DiffAngle = 45.d0
      ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == DisplVent) THEN
        ZoneUCSDUI(UINum)%DiffAngle = 73.d0
      ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == LinBarGrille) THEN
        ZoneUCSDUI(UINum)%DiffAngle = 15.d0
      ELSE
        ZoneUCSDUI(UINum)%DiffAngle = 28.d0
      END IF
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionInterior', ZoneUCSDUI(UINum)%ZoneName, &
                              'Angle between diffuser slots and the vertical', ZoneUCSDUI(UINum)%DiffAngle)
    END IF
    IF (ZoneUCSDUI(UINum)%TransHeight == AutoSize) THEN
      ZoneUCSDUI(UINum)%CalcTransHeight = .TRUE.
      ZoneUCSDUI(UINum)%TransHeight = 0.0d0
    ELSE
      ZoneUCSDUI(UINum)%CalcTransHeight = .FALSE.
    END IF
    IF (ZoneUCSDUI(UINum)%DiffuserType == Swirl) THEN
      IF (ZoneUCSDUI(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUI(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUI(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionInterior for Zone ' //  &
           TRIM(ZoneUCSDUI(UINum)%ZoneName) // &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = Swirl.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUI(UINum)%A_Kc = 0.0d0
      ZoneUCSDUI(UINum)%B_Kc = 0.0d0
      ZoneUCSDUI(UINum)%C_Kc = 0.6531d0
      ZoneUCSDUI(UINum)%D_Kc = 0.0069d0
      ZoneUCSDUI(UINum)%E_Kc = -0.00004d0
    ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == VarArea) THEN
      IF (ZoneUCSDUI(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUI(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUI(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionInterior for Zone ' //  &
           TRIM(ZoneUCSDUI(UINum)%ZoneName) // &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = VariableArea.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUI(UINum)%A_Kc = 0.0d0
      ZoneUCSDUI(UINum)%B_Kc = 0.0d0
      ZoneUCSDUI(UINum)%C_Kc = 0.88d0
      ZoneUCSDUI(UINum)%D_Kc = 0.0d0
      ZoneUCSDUI(UINum)%E_Kc = 0.0d0
    ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == DisplVent) THEN
      IF (ZoneUCSDUI(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUI(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUI(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionInterior for Zone ' //  &
           TRIM(ZoneUCSDUI(UINum)%ZoneName) // &
             ', input for ' // 'Coefficients A - E will be ignored when Floor Diffuser Type = HorizontalDisplacement.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUI(UINum)%A_Kc = 0.0d0
      ZoneUCSDUI(UINum)%B_Kc = 0.0d0
      ZoneUCSDUI(UINum)%C_Kc = 0.67d0
      ZoneUCSDUI(UINum)%D_Kc = 0.0d0
      ZoneUCSDUI(UINum)%E_Kc = 0.0d0
    ELSE IF (ZoneUCSDUI(UINum)%DiffuserType == LinBarGrille) THEN
      IF (ZoneUCSDUI(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUI(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUI(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUI(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionInterior for Zone ' //  &
           TRIM(ZoneUCSDUI(UINum)%ZoneName) // &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = LinearBarGrille.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUI(UINum)%A_Kc = 0.0d0
      ZoneUCSDUI(UINum)%B_Kc = 0.0d0
      ZoneUCSDUI(UINum)%C_Kc = 0.8d0
      ZoneUCSDUI(UINum)%D_Kc = 0.0d0
      ZoneUCSDUI(UINum)%E_Kc = 0.0d0
    ELSE
      IF (ZoneUCSDUI(UINum)%A_Kc .EQ. AutoCalculate .OR. ZoneUCSDUI(UINum)%B_Kc .EQ. AutoCalculate .OR.   &
          ZoneUCSDUI(UINum)%C_Kc .EQ. AutoCalculate .OR. ZoneUCSDUI(UINum)%D_Kc .EQ. AutoCalculate .OR.   &
             ZoneUCSDUI(UINum)%E_Kc .EQ. AutoCalculate) THEN
        CALL ShowFatalError('For RoomAirSettings:UnderFloorAirDistributionInterior for Zone ' //  &
           TRIM(ZoneUCSDUI(UINum)%ZoneName) //   &
           ', input for Coefficients A - E must be specified when Floor Diffuser Type = Custom.')
      END IF
    END IF
    IF (ZoneUCSDUI(UINum)%PowerPerPlume == AutoCalculate) THEN
        NumberOfPlumes = 0.0d0
      IF (NumberOfOccupants > 0.0d0) THEN
        NumberOfPlumes = NumberOfOccupants
      ELSE
        NumberOfPlumes = 1.0d0
      END IF
      ZoneElecConv = 0.0d0
      DO Ctd=1,TotElecEquip
        IF(ZoneElectric(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneElecConv = ZoneElecConv + ZoneElectric(Ctd)%DesignLevel * ZoneElectric(Ctd)%FractionConvected
        ENDIF
      END DO
      ZoneGasConv = 0.0d0
      DO Ctd=1,TotGasEquip
        IF(ZoneGas(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneGasConv = ZoneGasConv + ZoneGas(Ctd)%DesignLevel * ZoneGas(Ctd)%FractionConvected
        ENDIF
      END DO
      ZoneOthEqConv = 0.0d0
      DO Ctd=1,TotOthEquip
        IF(ZoneOtherEq(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneOthEqConv = ZoneOthEqConv + ZoneOtherEq(Ctd)%DesignLevel * ZoneOtherEq(Ctd)%FractionConvected
        ENDIF
      END DO
      ZoneHWEqConv = 0.0d0
      DO Ctd=1,TotHWEquip
        IF(ZoneHWEq(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneHWEqConv = ZoneHWEqConv + ZoneHWEq(Ctd)%DesignLevel * ZoneHWEq(Ctd)%FractionConvected
        ENDIF
      END DO
      DO Ctd=1,TotStmEquip
        ZoneSteamEqConv = 0.0d0
        IF(ZoneSteamEq(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneSteamEqConv = ZoneSteamEqConv + ZoneSteamEq(Ctd)%DesignLevel * ZoneSteamEq(Ctd)%FractionConvected
        ENDIF
      END DO
      ZoneUCSDUI(UINum)%PowerPerPlume = (NumberOfOccupants*73.0d0  + ZoneElecConv + ZoneGasConv + ZoneOthEqConv + ZoneHWEqConv + &
                                         ZoneSteamEqConv) / NumberOfPlumes
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionInterior', ZoneUCSDUI(UINum)%ZoneName, &
                              'Power per plume [W]', ZoneUCSDUI(UINum)%PowerPerPlume)
    END IF
    IF (ZoneUCSDUI(UINum)%DiffusersPerZone == AutoSize) THEN
      IF (NumberOfOccupants > 0.0d0) THEN
        ZoneUCSDUI(UINum)%DiffusersPerZone = NumberOfOccupants
      ELSE
        ZoneUCSDUI(UINum)%DiffusersPerZone = 1.0d0
      END IF
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionInterior', ZoneUCSDUI(UINum)%ZoneName, &
                              'Number of diffusers per zone', ZoneUCSDUI(UINum)%DiffusersPerZone)
    END IF

  END IF

  IF (ZoneModelType == RoomAirModel_UCSDUFE) THEN
    UINum = ZoneUFPtr(ZoneNum)
    ! calculate total window width in zone
    DO Ctd = PosZ_Window((ZoneNum-1)*2+1),PosZ_Window((ZoneNum-1)*2+2)
      SurfNum = APos_Window(ctd)
      If (SurfNum == 0) CYCLE
      IF (Surface(SurfNum)%ExtBoundCond == ExternalEnvironment .or. Surface(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or. &
          Surface(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt .or. Surface(SurfNum)%ExtBoundCond == OtherSideCondModeledExt) THEN
        ZoneUCSDUE(UINum)%WinWidth = ZoneUCSDUE(UINum)%WinWidth + Surface(SurfNum)%Width
        ZoneUCSDUE(UINum)%NumExtWin = ZoneUCSDUE(UINum)%NumExtWin + 1
      END IF
    END DO
    IF (ZoneUCSDUE(UINum)%WinWidth <= 0.0d0) THEN
      CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionExterior for Zone ' //  &
         TRIM(ZoneUCSDUE(UINum)%ZoneName) //   &
                            ' there are no exterior windows.')
      CALL ShowContinueError('  The zone will be treated as a UFAD interior zone')
    END IF
    NumberOfOccupants = 0.0d0
    DO Ctd = 1,TotPeople
      IF(People(Ctd)%ZonePtr == ZoneNum) THEN
        NumberOfOccupants = NumberOfOccupants + People(Ctd)%NumberOfPeople
      ENDIF
    END DO
    IF (ZoneUCSDUE(UINum)%DiffArea == AutoSize) THEN
      IF (ZoneUCSDUE(UINum)%DiffuserType == Swirl) THEN
        ZoneUCSDUE(UINum)%DiffArea = 0.0075d0
      ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == VarArea) THEN
        ZoneUCSDUE(UINum)%DiffArea = 0.035d0
      ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == DisplVent) THEN
        ZoneUCSDUE(UINum)%DiffArea = 0.0060d0
      ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == LinBarGrille) THEN
      ! 4 ft x 4 inches; eff area is 50% of total area; 75 cfm per linear foot.
        ZoneUCSDUE(UINum)%DiffArea = 0.03d0
      ELSE
        ZoneUCSDUE(UINum)%DiffArea = 0.0075d0
      END IF
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionExterior', ZoneUCSDUE(UINum)%ZoneName, &
                              'Design effective area of diffuser', ZoneUCSDUE(UINum)%DiffArea)
    END IF
    IF (ZoneUCSDUE(UINum)%DiffAngle == AutoSize) THEN
      IF (ZoneUCSDUE(UINum)%DiffuserType == Swirl) THEN
        ZoneUCSDUE(UINum)%DiffAngle = 28.d0
      ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == VarArea) THEN
        ZoneUCSDUE(UINum)%DiffAngle = 45.d0
      ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == DisplVent) THEN
        ZoneUCSDUE(UINum)%DiffAngle = 73.d0
      ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == LinBarGrille) THEN
        ZoneUCSDUE(UINum)%DiffAngle = 15.d0
      ELSE
        ZoneUCSDUE(UINum)%DiffAngle = 28.d0
      END IF
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionExterior', ZoneUCSDUE(UINum)%ZoneName, &
                              'Angle between diffuser slots and the vertical', ZoneUCSDUE(UINum)%DiffAngle)
    END IF
    IF (ZoneUCSDUE(UINum)%TransHeight == AutoSize) THEN
      ZoneUCSDUE(UINum)%CalcTransHeight = .TRUE.
      ZoneUCSDUE(UINum)%TransHeight = 0.0d0
    ELSE
      ZoneUCSDUE(UINum)%CalcTransHeight = .FALSE.
    END IF
    IF (ZoneUCSDUE(UINum)%DiffuserType == Swirl) THEN
      IF (ZoneUCSDUE(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUE(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUE(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionExterior for Zone ' //  &
           TRIM(ZoneUCSDUE(UINum)%ZoneName) // &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = Swirl.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUE(UINum)%A_Kc = 0.0d0
      ZoneUCSDUE(UINum)%B_Kc = 0.0d0
      ZoneUCSDUE(UINum)%C_Kc = 0.6531d0
      ZoneUCSDUE(UINum)%D_Kc = 0.0069d0
      ZoneUCSDUE(UINum)%E_Kc = -0.00004d0
    ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == VarArea) THEN
      IF (ZoneUCSDUE(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUE(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUE(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionExterior for Zone ' //  &
           TRIM(ZoneUCSDUE(UINum)%ZoneName) //  &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = VariableArea.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUE(UINum)%A_Kc = 0.0d0
      ZoneUCSDUE(UINum)%B_Kc = 0.0d0
      ZoneUCSDUE(UINum)%C_Kc = 0.83d0
      ZoneUCSDUE(UINum)%D_Kc = 0.0d0
      ZoneUCSDUE(UINum)%E_Kc = 0.0d0
    ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == DisplVent) THEN
      IF (ZoneUCSDUE(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUE(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUE(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionExterior for Zone ' //  &
           TRIM(ZoneUCSDUE(UINum)%ZoneName) //   &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = HorizontalDisplacement.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUE(UINum)%A_Kc = 0.0d0
      ZoneUCSDUE(UINum)%B_Kc = 0.0d0
      ZoneUCSDUE(UINum)%C_Kc = 0.67d0
      ZoneUCSDUE(UINum)%D_Kc = 0.0d0
      ZoneUCSDUE(UINum)%E_Kc = 0.0d0
    ELSE IF (ZoneUCSDUE(UINum)%DiffuserType == LinBarGrille) THEN
      IF (ZoneUCSDUE(UINum)%A_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%B_Kc .NE. AutoCalculate .OR.   &
          ZoneUCSDUE(UINum)%C_Kc .NE. AutoCalculate .OR. ZoneUCSDUE(UINum)%D_Kc .NE. AutoCalculate .OR.   &
             ZoneUCSDUE(UINum)%E_Kc .NE. AutoCalculate) THEN
        CALL ShowWarningError('For RoomAirSettings:UnderFloorAirDistributionExterior for Zone ' //  &
           TRIM(ZoneUCSDUE(UINum)%ZoneName) //  &
             ', input for Coefficients A - E will be ignored when Floor Diffuser Type = LinearBarGrille.')
        CALL ShowContinueError('  To input these Coefficients, use Floor Diffuser Type = Custom.')
      END IF
      ZoneUCSDUE(UINum)%A_Kc = 0.0d0
      ZoneUCSDUE(UINum)%B_Kc = 0.0d0
      ZoneUCSDUE(UINum)%C_Kc = 0.8214d0
      ZoneUCSDUE(UINum)%D_Kc = -0.0263d0
      ZoneUCSDUE(UINum)%E_Kc = 0.0014d0
    ELSE
      IF (ZoneUCSDUE(UINum)%A_Kc .EQ. AutoCalculate .OR. ZoneUCSDUE(UINum)%B_Kc .EQ. AutoCalculate .OR.   &
          ZoneUCSDUE(UINum)%C_Kc .EQ. AutoCalculate .OR. ZoneUCSDUE(UINum)%D_Kc .EQ. AutoCalculate .OR.   &
             ZoneUCSDUE(UINum)%E_Kc .EQ. AutoCalculate) THEN
        CALL ShowFatalError('For RoomAirSettings:UnderFloorAirDistributionExterior for Zone ' //  &
           TRIM(ZoneUCSDUE(UINum)%ZoneName) //   &
           ', input for Coefficients A - E must be specified when Floor Diffuser Type = Custom.')
      END IF
    END IF
    IF (ZoneUCSDUE(UINum)%PowerPerPlume == AutoCalculate) THEN
      IF (NumberOfOccupants > 0) THEN
        NumberOfPlumes = NumberOfOccupants
      ELSE
        NumberOfPlumes = 1.0d0
      END IF
      ZoneElecConv = 0.0d0
      DO Ctd=1,TotElecEquip
        IF(ZoneElectric(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneElecConv = ZoneElecConv + ZoneElectric(Ctd)%DesignLevel
        ENDIF
      END DO
      ZoneGasConv = 0.0d0
      DO Ctd=1,TotGasEquip
        IF(ZoneGas(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneGasConv = ZoneGasConv + ZoneGas(Ctd)%DesignLevel
        ENDIF
      END DO
      ZoneOthEqConv = 0.0d0
      DO Ctd=1,TotOthEquip
        IF(ZoneOtherEq(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneOthEqConv = ZoneOthEqConv + ZoneOtherEq(Ctd)%DesignLevel
        ENDIF
      END DO
      ZoneHWEqConv = 0.0d0
      DO Ctd=1,TotHWEquip
        IF(ZoneHWEq(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneHWEqConv = ZoneHWEqConv + ZoneHWEq(Ctd)%DesignLevel
        ENDIF
      END DO
      DO Ctd=1,TotStmEquip
        ZoneSteamEqConv = 0.0d0
        IF(ZoneSteamEq(Ctd)%ZonePtr == ZoneNum) THEN
          ZoneSteamEqConv = ZoneSteamEqConv + ZoneSteamEq(Ctd)%DesignLevel
        ENDIF
      END DO
      ZoneUCSDUE(UINum)%PowerPerPlume = (NumberOfOccupants*73.0d0  + ZoneElecConv + ZoneGasConv + ZoneOthEqConv + ZoneHWEqConv + &
                                         ZoneSteamEqConv) / NumberOfPlumes
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionExterior', ZoneUCSDUE(UINum)%ZoneName, &
                              'Power per plume [W]', ZoneUCSDUE(UINum)%PowerPerPlume)

    END IF
    IF (ZoneUCSDUE(UINum)%DiffusersPerZone == AutoSize) THEN
      IF (NumberOfOccupants > 0.0d0) THEN
        ZoneUCSDUE(UINum)%DiffusersPerZone = NumberOfOccupants
      ELSE
        ZoneUCSDUE(UINum)%DiffusersPerZone = 1.0d0
      END IF
      CALL ReportSizingOutput('RoomAirSettings:UnderFloorAirDistributionExterior', ZoneUCSDUE(UINum)%ZoneName, &
                              'Number of diffusers per zone', ZoneUCSDUE(UINum)%DiffusersPerZone)
    END IF

  END IF

  RETURN

END SUBROUTINE SizeUCSDUF

SUBROUTINE HcUCSDUF(ZoneNum,FractionHeight)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   February 2004
          !       MODIFIED       -
          !       RE-ENGINEERED  -

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main subroutine for convection calculation in the UCSD Displacement Ventilation model.
          ! It calls CalcDetailedHcInForDVModel for convection coefficient
          ! initial calculations and averages the final result comparing the position of the surface with
          ! the interface subzone height.

          ! METHODOLOGY EMPLOYED:
          ! -
          ! -
          ! -
          ! -

          ! REFERENCES:
          ! -
          ! -

          ! USE STATEMENTS:
  USE DataRoomAirModel ,                     ONLY: AirModel
  USE DataHeatBalFanSys
  USE DataEnvironment
  USE DataHeatBalance
  USE InputProcessor
  USE ScheduleManager,                       ONLY: GetScheduleIndex
  USE DataGlobals,                           ONLY: BeginEnvrnFlag


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN)                    :: ZoneNum             !
  REAL(r64),INTENT (IN)                       :: FractionHeight      !

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                :: Ctd                 ! DO loop counter for surfaces
  REAL(r64)                              :: HLD                 ! Convection coefficient for the lower area of surface
  REAL(r64)                              :: TmedDV              ! Average temperature for DV
  REAL(r64)                              :: Z1                  ! auxiliary var for lowest height
  REAL(r64)                              :: Z2                  ! auxiliary var for highest height
  REAL(r64)                              :: ZSupSurf            ! highest height for this surface
  REAL(r64)                              :: ZInfSurf            ! lowest height for this surface
  REAL(r64)                              :: HLU                 ! Convection coefficient for the upper area of surface
  REAL(r64)                              :: LayH                ! Height of the Occupied/Mixed subzone interface
  REAL(r64)                              :: LayFrac             ! Fraction height of the Occupied/Mixed subzone interface
  INTEGER                                :: SurfNum             ! Surface number
  ! Initialize HAT and HA
  HAT_MX    = 0.0d0
  HAT_OC    = 0.0d0
  HA_MX     = 0.0d0
  HA_OC     = 0.0d0
  HAT_FLOOR = 0.0d0
  HA_FLOOR  = 0.0d0
  HAT_MXWin = 0.0d0
  HAT_OCWin = 0.0d0
  HA_MXWin  = 0.0d0
  HA_OCWin  = 0.0d0

  ! Is the air flow model for this zone set to UCSDDV Displacement Ventilation?
  IF(IsZoneUI(ZoneNum)) THEN
    LayFrac = FractionHeight
    LayH    = FractionHeight*(ZoneCeilingHeight((ZoneNum-1)*2 + 2)- &
                                                  ZoneCeilingHeight((ZoneNum-1)*2 + 1))
    ! WALL Hc, HA and HAT calculation
    DO Ctd = PosZ_Wall((ZoneNum-1)*2 + 1),PosZ_Wall((ZoneNum-1)*2 + 2)
      SurfNum = APos_Wall(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      Z1 = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
      Z2 = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
      ZSupSurf = Z2 - ZoneCeilingHeight((ZoneNum-1)*2 + 1)
      ZInfSurf = Z1 - ZoneCeilingHeight((ZoneNum-1)*2 + 1)

      ! The Wall surface is in the upper subzone
      IF(ZInfSurf > LayH)THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HWall(Ctd)= UFHcIn(SurfNum)
        HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWall(Ctd) + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*HWall(Ctd) + HA_MX
      ENDIF

        ! The Wall surface is in the lower subzone
      IF(ZSupSurf < LayH)THEN
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HWall(Ctd)= UFHcIn(SurfNum)
        HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWall(Ctd) + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*HWall(Ctd) + HA_OC
      ENDIF

      IF (ABS(ZInfSurf-ZSupSurf) < 1.d-10) THEN
        CALL ShowSevereError('RoomAirModelUFAD:HcUCSDUF: Surface values will cause divide by zero.')
        CALL ShowContinueError('Zone="'//trim(Zone(Surface(SurfNum)%Zone)%Name)//'", Surface="'//trim(Surface(SurfNum)%Name)//'".')
        CALL ShowContinueError('ZInfSurf=['//trim(RoundSigDigits(ZInfSurf,4))//'], LayH=['//trim(RoundSigDigits(LayH,4))//'].')
        CALL ShowContinueError('ZSupSurf=['//trim(RoundSigDigits(ZSupSurf,4))//'], LayH=['//trim(RoundSigDigits(LayH,4))//'].')
        CALL ShowFatalError('...Previous condition causes termination.')
      ENDIF

      ! The Wall surface is partially in upper and partially in lower subzone
      IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HLU= UFHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HLD= UFHcIn(SurfNum)
        TmedDV    = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
        HWall(Ctd)= ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
        HAT_MX    = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                             TempSurfIn(SurfNum)*HLU + HAT_MX
        HA_MX     = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
        HAT_OC    = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                             TempSurfIn(SurfNum)*HLD + HAT_OC
        HA_OC     = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
        TempEffBulkAir(SurfNum) = TmedDV
      ENDIF

      UFHcIn(SurfNum) = HWall(Ctd)

    END DO  ! END WALL

    ! WINDOW Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Window((ZoneNum-1)*2 + 1),PosZ_Window((ZoneNum-1)*2 + 2)
      SurfNum = APos_Window(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      IF (Surface(SurfNum)%Tilt > 10.0d0 .AND. Surface(SurfNum)%Tilt < 170.0d0) THEN ! Window Wall
        Z1 = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2 = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        ZSupSurf = Z2-ZoneCeilingHeight((ZoneNum-1)*2 + 1)
        ZInfSurf = Z1-ZoneCeilingHeight((ZoneNum-1)*2 + 1)

        IF(ZInfSurf > LayH)THEN
          TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
          CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
          HWindow(Ctd)= UFHcIn(SurfNum)
          HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_MX
          HA_MX = Surface(SurfNum)%Area*HWindow(Ctd) + HA_MX
          HAT_MXWin = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_MXWin
          HA_MXWin = Surface(SurfNum)%Area*HWindow(Ctd) + HA_MXWin
        ENDIF

        IF(ZSupSurf < LayH)THEN
          TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
          CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
          HWindow(Ctd)= UFHcIn(SurfNum)
          HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_OC
          HA_OC = Surface(SurfNum)%Area*HWindow(Ctd) + HA_OC
          HAT_OCWin = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_OCWin
          HA_OCWin = Surface(SurfNum)%Area*HWindow(Ctd) + HA_OCWin
        ENDIF

        IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
           TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
           CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
           HLU= UFHcIn(SurfNum)
           TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
           CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
           HLD= UFHcIn(SurfNum)
           TmedDV = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
           HWindow(Ctd) = ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
           HAT_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                    TempSurfIn(SurfNum)*HLU + HAT_MX
           HA_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
           HAT_MXWin = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                    TempSurfIn(SurfNum)*HLU + HAT_MXWin
           HA_MXWin = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MXWin
           HAT_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                    TempSurfIn(SurfNum)*HLD + HAT_OC
           HA_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
           HAT_OCWin = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                    TempSurfIn(SurfNum)*HLD + HAT_OCWin
           HA_OCWin = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OCWin
           TempEffBulkAir(SurfNum) = TmedDV
         ENDIF
       ENDIF

       IF (Surface(SurfNum)%Tilt <= 10.0d0) THEN ! Window Ceiling
         TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
         HWindow(Ctd)= UFHcIn(SurfNum)
         HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_MX
         HA_MX  = Surface(SurfNum)%Area*HWindow(Ctd) + HA_MX
       ENDIF

       IF (Surface(SurfNum)%Tilt >= 170.0d0) THEN ! Window Floor
         TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
         HWindow(Ctd)= UFHcIn(SurfNum)
         HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_OC
         HA_OC  = Surface(SurfNum)%Area*HWindow(Ctd) + HA_OC
       ENDIF

       UFHcIn(SurfNum) = HWindow(Ctd)

    END DO ! END WINDOW

    ! DOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Door((ZoneNum-1)*2 + 1),PosZ_Door((ZoneNum-1)*2 + 2) ! DOOR
      SurfNum = APos_Door(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
        Z1 = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2 = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        ZSupSurf = Z2-ZoneCeilingHeight((ZoneNum-1)*2 + 1)
        ZInfSurf = Z1-ZoneCeilingHeight((ZoneNum-1)*2 + 1)

      IF(ZInfSurf > LayH)THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HDoor(Ctd)= UFHcIn(SurfNum)
        HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HDoor(Ctd) + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*HDoor(Ctd) + HA_MX
      ENDIF

      IF(ZSupSurf < LayH)THEN
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HDoor(Ctd)= UFHcIn(SurfNum)
        HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HDoor(Ctd) + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*HDoor(Ctd) + HA_OC
      ENDIF

      IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HLU= UFHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HLD= UFHcIn(SurfNum)
        TmedDV = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
        HDoor(Ctd) = ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
        HAT_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLU + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
        HAT_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLD + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
        TempEffBulkAir(SurfNum) = TmedDV
      ENDIF

      UFHcIn(SurfNum) = HDoor(Ctd)

   END DO  ! END DOOR

    ! INTERNAL Hc, HA and HAT CALCULATION
    HeightIntMass = MIN(HeightIntMassDefault,  &
        (ZoneCeilingHeight((ZoneNum-1)*2 + 2)- ZoneCeilingHeight((ZoneNum-1)*2 + 1)))
    DO Ctd = PosZ_Internal((ZoneNum-1)*2 + 1),PosZ_Internal((ZoneNum-1)*2 + 2)
      SurfNum = APos_Internal(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      ZSupSurf = HeightIntMass
      ZInfSurf = 0.0d0

      IF(ZSupSurf < LayH)THEN
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HInternal(Ctd)= UFHcIn(SurfNum)
        HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HInternal(Ctd) + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*HInternal(Ctd) + HA_OC
      ENDIF

      IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HLU= UFHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
        HLD= UFHcIn(SurfNum)
        TmedDV = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
        HInternal(Ctd) = ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
        HAT_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLU + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
        HAT_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLD + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
        TempEffBulkAir(SurfNum) = TmedDV
      ENDIF

      UFHcIn(SurfNum) = HInternal(Ctd)
    END DO  ! END INTERNAL

    ! CEILING Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Ceiling((ZoneNum-1)*2 + 1),PosZ_Ceiling((ZoneNum-1)*2 + 2)
      SurfNum = APos_Ceiling(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
      HCeiling(Ctd)= UFHcIn(SurfNum)
      HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HCeiling(Ctd) + HAT_MX
      HA_MX  = Surface(SurfNum)%Area*HCeiling(Ctd) + HA_MX
      UFHcIn(SurfNum) = HCeiling(Ctd)
    END DO  ! END CEILING

    ! FLOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Floor((ZoneNum-1)*2 + 1),PosZ_Floor((ZoneNum-1)*2 + 2)
      SurfNum = APos_Floor(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTFLOOR(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,UFHcIn)
      HFloor(Ctd)= UFHcIn(SurfNum)
      HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HFloor(Ctd) + HAT_OC
      HA_OC  = Surface(SurfNum)%Area*HFloor(Ctd) + HA_OC
      TempEffBulkAir(SurfNum) = ZTFLOOR(ZoneNum)
      UFHcIn(SurfNum) = HFloor(Ctd)
     END DO  ! END FLOOR

  ENDIF


END SUBROUTINE HcUCSDUF

SUBROUTINE CalcUCSDUI(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2005
          !       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Using the UCSD UFAD interior zone model, this subroutine calculates the  occupied subzone height,
          ! surface heat transfer coefficients, the occupied subzone temperature, and the upper subzone temperature.

          ! METHODOLOGY EMPLOYED:
          ! The zone is divided into 2 subzones with a variable transition height.

          ! REFERENCES:
          ! The model is described in the EnergyPlus Engineering Reference in Anna Liu's UCSD PhD thesis.

          ! USE STATEMENTS:
  USE ScheduleManager,            ONLY: GetCurrentScheduleValue
  USE DataZoneEquipment,          ONLY: ZoneEquipConfig
  USE Psychrometrics,             ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataHeatBalFanSys
  USE DataHVACGlobals,            ONLY: TimestepSys, UseZoneTimeStepHistory
  USE InternalHeatGains,          ONLY: SumInternalConvectionGainsByTypes, SumReturnAirConvectionGainsByTypes


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum       ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: MIXFLAG = .FALSE.  ! if true treat as a mixed zone
  REAL(r64)   :: CeilingHeight      ! zone ceiling height above floor [m]
  INTEGER :: UINum              ! index to underfloor interior zone model data
  REAL(r64)   :: GainsFrac          ! fraction of occupied subzone heat gains that remain in the subzone;
                                ! that is, don't go into the plumes
  ! REAL(r64)   :: NumPLPP            ! number of plumes per person
  REAL(r64)   :: HeightThermostat   ! height of the thermostat above the floor [m]
  REAL(r64)   :: HeightComfort      ! height at which comfort temperature is calculated
  REAL(r64)   :: TempDiffCritRep    ! Minimum temperature difference between upper and occupied subzones for reporting
  REAL(r64)   :: ConvGainsOccSubzone ! convective heat gains into the lower (occupied) subzone [W]
  REAL(r64)   :: ConvGainsUpSubzone  ! convective heat gains into the upper subzone [W]
  REAL(r64)   :: ConvGains           ! total zone convective gains (exclusing surfaces) [W]
  INTEGER   :: ZoneEquipConfigNum ! ZoneEquipConfig index for this UFAD zone
  REAL(r64)   :: SumSysMCp          ! Sum of system mass flow rate * specific heat for this zone [W/K]
  REAL(r64)   :: SumSysMCpT         ! Sum of system mass flow rate * specific heat * temperature for this zone [W]
  REAL(r64)   :: SumSysM            ! Sum of systems mass flow rate [kg/s]
  REAL(r64)   :: NodeTemp           ! inlet node temperature [K]
  REAL(r64)   :: MassFlowRate       ! system mass flow rate [kg/s]
  REAL(r64)   :: CpAir              ! specific heat of air [J/kgK]
  INTEGER :: InNodeIndex        ! inlet node index in ZoneEquipConfig
  REAL(r64)   :: SumMCp             ! mass flow rate * specific heat for this zone for infiltration, ventilation, mixing [W/K]
  REAL(r64)   :: SumMCpT            ! mass flow rate * specific heat* temp for this zone for infiltration, ventilation, mixing [W]
  REAL(r64)   :: MCP_Total          ! total mass flow rate * specific heat for this zone [W/K]
  REAL(r64)   :: MCpT_Total         ! total mass flow rate * specific heat* temp for this zone [W]
  REAL(r64)   :: NumberOfPlumes
  REAL(r64)   :: PowerInPlumes      ! [W]
  REAL(r64)   :: PowerPerPlume=0.0d0  ! power generating each plume [W]
  REAL(r64)   :: HeightFrac         ! Fractional height of transition between occupied and upper subzones
  REAL(r64)   :: TotSysFlow         ! [m3/s]
  REAL(r64)   :: NumDiffusersPerPlume
  REAL(r64)   :: NumDiffusers
  REAL(r64)   :: TSupK              ! supply yemperature [K]
  REAL(r64)   :: Gamma              ! dimensionless height parameter; higher gamma means interface height will be
                                ! higher, smaller gamma means interface height will be lower.
  REAL(r64)   :: DiffArea           ! diffuser effective area [m2]
  REAL(r64)   :: ThrowAngle         ! diffuser slot angle relative to vertical [radians]
  REAL(r64)   :: SourceHeight       ! height of plume sources above the floor [m]
  INTEGER :: Ctd
  REAL(r64)   :: AirCap
  REAL(r64)   :: TempHistTerm
  REAL(r64)   :: ZTAveraged
  REAL(r64)   :: HeightUpSubzoneAve     ! Height of center of upper air subzone
  REAL(r64)   :: HeightOccupiedSubzoneAve  ! Height of center of occupied air subzone
  REAL(r64)   :: ZoneMult     ! total zone multiplier
  INTEGER :: ZoneNodeNum  ! node number of the HVAC zone node
  REAL(r64)   :: TempDepCoef=0.0d0           ! Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
  REAL(r64)   :: TempIndCoef=0.0d0           ! Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
  INTEGER, DIMENSION(28) :: IntGainTypesOccupied = (/IntGainTypeOf_People, &
                                                    IntGainTypeOf_WaterHeaterMixed, &
                                                    IntGainTypeOf_WaterHeaterStratified, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterMixed, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterStratified, &
                                                    IntGainTypeOf_ElectricEquipment, &
                                                    IntGainTypeOf_GasEquipment, &
                                                    IntGainTypeOf_HotWaterEquipment, &
                                                    IntGainTypeOf_SteamEquipment, &
                                                    IntGainTypeOf_OtherEquipment, &
                                                    IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, &
                                                    IntGainTypeOf_GeneratorFuelCell, &
                                                    IntGainTypeOf_WaterUseEquipment, &
                                                    IntGainTypeOf_GeneratorMicroCHP, &
                                                    IntGainTypeOf_ElectricLoadCenterTransformer, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterSimple, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageBattery, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageSimple, &
                                                    IntGainTypeOf_PipeIndoor, &
                                                    IntGainTypeOf_RefrigerationCase, &
                                                    IntGainTypeOf_RefrigerationCompressorRack, &
                                                    IntGainTypeOf_RefrigerationSystemAirCooledCondenser ,&
                                                    IntGainTypeOf_RefrigerationSystemSuctionPipe, &
                                                    IntGainTypeOf_RefrigerationSecondaryReceiver, &
                                                    IntGainTypeOf_RefrigerationSecondaryPipe, &
                                                    IntGainTypeOf_RefrigerationWalkIn/)

  INTEGER, DIMENSION(2) :: IntGainTypesUpSubzone = (/IntGainTypeOf_DaylightingDeviceTubular , &
                                                        IntGainTypeOf_Lights/)
  REAL(r64)    :: RetAirGains

  ! Exact solution or Euler method
  If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
    If (ShortenTimeStepSysRoomAir .and. TimeStepSys .LT. TimeStepZone) Then
      If (PreviousTimeStep < TimeStepZone) Then
        Zone1OC(ZoneNum) = ZoneM2OC(ZoneNum)
        Zone1MX(ZoneNum) = ZoneM2MX(ZoneNum)
      Else
        Zone1OC(ZoneNum) = ZoneMXOC(ZoneNum)
        Zone1MX(ZoneNum) = ZoneMXMX(ZoneNum)
      End If
    Else
      Zone1OC(ZoneNum) = ZTOC(ZoneNum)
      Zone1MX(ZoneNum) = ZTMX(ZoneNum)
    End If
  End If

  MIXFLAG = .FALSE.
  UFHcIn = HConvIn
  SumSysMCp = 0.0d0
  SumSysMCpT = 0.0d0
  TotSysFlow = 0.0d0
  TSupK = 0.0d0
  SumSysM = 0.0d0
  ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
  CeilingHeight = ZoneCeilingHeight((ZoneNum-1)*2 + 2) - ZoneCeilingHeight((ZoneNum-1)*2 + 1)
  UINum = ZoneUFPtr(ZoneNum)
  HeightThermostat = ZoneUCSDUI(UINum)%ThermostatHeight
  HeightComfort = ZoneUCSDUI(UINum)%ComfortHeight
  TempDiffCritRep = ZoneUCSDUI(UINum)%TempTrigger
  DiffArea = ZoneUCSDUI(UINum)%DiffArea
  ThrowAngle = DegToRadians*ZoneUCSDUI(UINum)%DiffAngle
  SourceHeight = 0.0d0
  NumDiffusers = ZoneUCSDUI(UINum)%DiffusersPerZone
  PowerPerPlume = ZoneUCSDUI(UINum)%PowerPerPlume
  ! gains from occupants, task lighting, elec equip, gas equip, other equip, hot water equip, steam equip,
  ! baseboards (nonthermostatic), water heater skin loss
  CALL SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, ConvGainsOccSubzone)

  ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
  ! low or zero)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, RetAirGains)
    ConvGainsOccSubzone = ConvGainsOccSubzone + RetAirGains
  END IF

  ! gains from lights (ceiling), tubular daylighting devices, high temp radiant heaters

  CALL SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesUpSubzone, ConvGainsUpSubzone)
  ConvGainsUpSubzone = ConvGainsUpSubzone + SumConvHTRadSys(ZoneNum)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesUpSubzone, RetAirGains)
    ConvGainsUpSubzone = ConvGainsUpSubzone + RetAirGains
  END IF
  ConvGains = ConvGainsOccSubzone + ConvGainsUpSubzone + SysDepZoneLoadsLagged(ZoneNum)
  ZoneEquipConfigNum = ZoneUCSDUI(UINum)%ZoneEquipPtr
  IF (ZoneEquipConfigNum > 0) THEN
    DO InNodeIndex = 1,ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
      NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(InNodeIndex))%Temp
      MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(InNodeIndex))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
      SumSysMCp = SumSysMCp + MassFlowRate * CpAir
      SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
      TotSysFlow = TotSysFlow + MassFlowRate / PsyRhoAirFnPbTdbW(OutBaroPress,NodeTemp,ZoneAirHumRat(ZoneNum))
      TSupK = TSupK + MassFlowRate * NodeTemp
      SumSysM = SumSysM + MassFlowRate
    END DO
    IF (TotSysFlow > 0.0d0) THEN
      TSupK = TSupK/SumSysM + KelvinConv
    ELSE
      TSupK = 0.0d0
    END IF
  END IF
  ! mass flow times specific heat for infiltration, ventilation, mixing, earth tube
  SumMCp = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MdotCPOA(ZoneNum)
  ! mass flow times specific heat times temperature for infiltration, ventilation, mixing, earth tube
  SumMCpT = MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + &
            MdotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp
  MCP_Total = SumMCp + SumSysMCp
  MCpT_Total = SumMCpT + SumSysMCpT
  ! For the York MIT diffusers (variable area) the area varies with the flow rate. Assume 400 ft/min velocity
  ! at the diffuser, and a design flow rate of 150 cfm (.0708 m3/s). Then the design area for each diffuser is
  ! 150 ft3/min / 400 ft/min = .375 ft2 = .035 m2. This is adjusted each time step by
  !               (TotSysFlow/(NumDiffusers*.0708))*.035
  IF (ZoneUCSDUI(UINum)%DiffuserType .EQ. VarArea) THEN
    DiffArea = .035d0*TotSysFlow/(.0708d0*NumDiffusers)
  END IF
  ! initial estimate of convective transfer from surfaces; assume HeightFrac is 0.5.
  CALL HcUCSDUF(ZoneNum,0.5d0)
  PowerInPlumes = ConvGains + HAT_OC - HA_OC*ZTOC(ZoneNum) + HAT_MX - HA_MX*ZTMX(ZoneNum)
  IF (PowerPerPlume > 0.0d0 .AND. PowerInPlumes > 0.0d0) THEN
    NumberOfPlumes = PowerInPlumes / PowerPerPlume
    NumDiffusersPerPlume = NumDiffusers / NumberOfPlumes
  ELSE
    NumberOfPlumes = 1.0d0
    NumDiffusersPerPlume = 1.0d0
  END IF
  IF ((PowerInPlumes <= 0.0d0) .OR. (TotSysFlow .EQ. 0.0d0) .OR. (TsupK-KelvinConv) > MAT(ZoneNum)) THEN
    ! The system will mix
    HeightFrac = 0.0d0
  ELSE
    Gamma = (TotSysFlow*COS(ThrowAngle))**1.5d0 / (NumberOfPlumes*(NumDiffusersPerPlume*DiffArea)**1.25d0 &
            * (0.0281d0*0.001d0*PowerInPlumes)**0.5d0)
    IF (ZoneUCSDUI(UINum)%CalcTransHeight) THEN
      HeightFrac = ((NumDiffusersPerPlume*DiffArea)**0.5d0 * (7.43d0*log(Gamma) - 1.35d0) + 0.5d0*SourceHeight) / CeilingHeight
    ELSE
      HeightFrac = ZoneUCSDUI(UINum)%TransHeight / CeilingHeight
    END IF
    HeightFrac = MAX(0.0d0,MIN(1.0d0,HeightFrac))
    DO Ctd = 1,4
      CALL HcUCSDUF(ZoneNum,HeightFrac)
      PowerInPlumes = ConvGains + HAT_OC - HA_OC*ZTOC(ZoneNum) + HAT_MX - HA_MX*ZTMX(ZoneNum)
      IF (PowerPerPlume > 0.0d0 .AND. PowerInPlumes > 0.0d0) THEN
        NumberOfPlumes = PowerInPlumes / PowerPerPlume
        NumDiffusersPerPlume = NumDiffusers / NumberOfPlumes
      ELSE
        NumberOfPlumes = 1.0d0
        NumDiffusersPerPlume = 1.0d0
      END IF
      IF (PowerInPlumes .LE. 0.0d0) EXIT
      Gamma = (TotSysFlow*COS(ThrowAngle))**1.5d0 / (NumberOfPlumes*(NumDiffusersPerPlume*DiffArea)**1.25d0 &
              * (0.0281d0*0.001d0*PowerInPlumes)**0.5d0)
      IF (ZoneUCSDUI(UINum)%CalcTransHeight) THEN
        HeightFrac = ((NumDiffusersPerPlume*DiffArea)**0.5d0 * (7.43d0*log(Gamma) - 1.35d0) + 0.5d0*SourceHeight) / CeilingHeight
      ELSE
        HeightFrac = ZoneUCSDUI(UINum)%TransHeight / CeilingHeight
      END IF
      HeightFrac = MAX(0.0d0,MIN(1.0d0,HeightFrac))
      HeightTransition(ZoneNum) = HeightFrac * CeilingHeight
      GainsFrac = ZoneUCSDUI(UINum)%A_Kc * Gamma**ZoneUCSDUI(UINum)%B_Kc + ZoneUCSDUI(UINum)%C_Kc +   &
                  ZoneUCSDUI(UINum)%D_Kc * Gamma + ZoneUCSDUI(UINum)%E_Kc * Gamma**2
      GainsFrac = MAX(0.6d0,MIN(GainsFrac,1.0d0))
      AIRRATOC(ZoneNum) = Zone(ZoneNum)%Volume*(HeightTransition(ZoneNum)-MIN(HeightTransition(ZoneNum),0.2d0)) &
                        /CeilingHeight*ZoneVolCapMultpSens &
                        *PsyRhoAirFnPbTdbW(OutBaroPress,MATOC(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                        *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATOC(ZoneNum))/(TimeStepSys*SecInHour)
      AIRRATMX(ZoneNum) = Zone(ZoneNum)%Volume*(CeilingHeight-HeightTransition(ZoneNum)) &
                        /CeilingHeight*ZoneVolCapMultpSens &
                        *PsyRhoAirFnPbTdbW(OutBaroPress,MATMX(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                        *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATMX(ZoneNum))/(TimeStepSys*SecInHour)
      IF (UseZoneTimeStepHistory) THEN
        ZTM3OC(ZoneNum)    = XM3TOC(ZoneNum)
        ZTM2OC(ZoneNum)    = XM2TOC(ZoneNum)
        ZTM1OC(ZoneNum)    = XMATOC(ZoneNum)

        ZTM3MX(ZoneNum)    = XM3TMX(ZoneNum)
        ZTM2MX(ZoneNum)    = XM2TMX(ZoneNum)
        ZTM1MX(ZoneNum)    = XMATMX(ZoneNum)

      ELSE
        ZTM3OC(ZoneNum)    = DSXM3TOC(ZoneNum)
        ZTM2OC(ZoneNum)    = DSXM2TOC(ZoneNum)
        ZTM1OC(ZoneNum)    = DSXMATOC(ZoneNum)

        ZTM3MX(ZoneNum)    = DSXM3TMX(ZoneNum)
        ZTM2MX(ZoneNum)    = DSXM2TMX(ZoneNum)
        ZTM1MX(ZoneNum)    = DSXMATMX(ZoneNum)

      ENDIF

      AirCap = AIRRATOC(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1OC(ZoneNum)-(3.0d0/2.0d0)*ZTM2OC(ZoneNum)+(1.0d0/3.0d0)*ZTM3OC(ZoneNum))
      TempDepCoef = GainsFrac*HA_OC + MCP_Total
      TempIndCoef = GainsFrac*(ConvGains+HAT_OC+HAT_MX-HA_MX*ZTMX(ZoneNum))+MCPT_Total+NonAirSystemResponse(ZoneNum)/ZoneMult
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTOC(ZoneNum) = (TempHistTerm + GainsFrac*(ConvGains + HAT_OC + HAT_MX - HA_MX*ZTMX(ZoneNum)) &
                        + MCPT_Total + NonAirSystemResponse(ZoneNum)/ZoneMult) &
                        / ((11.0d0/6.0d0)*AirCap + GainsFrac*HA_OC + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTOC(ZoneNum) = Zone1OC(ZoneNum) + TempIndCoef/AirCap
          Else
            ZTOC(ZoneNum) = (Zone1OC(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
          End If
        CASE (UseEulerMethod)
          ZTOC(ZoneNum) = (AirCap*Zone1OC(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
      AirCap = AIRRATMX(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1MX(ZoneNum)-(3.0d0/2.0d0)*ZTM2MX(ZoneNum)+(1.0d0/3.0d0)*ZTM3MX(ZoneNum))
      TempDepCoef = (1.0d0-GainsFrac)*HA_MX + MCP_Total
      TempIndCoef = (1.0d0-GainsFrac)*(ConvGains + HAT_OC + HAT_MX - HA_OC*ZTOC(ZoneNum)) + ZTOC(ZoneNum)*MCP_Total
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTMX(ZoneNum) = (TempHistTerm + (1.0d0-GainsFrac)*(ConvGains + HAT_OC + HAT_MX - HA_OC*ZTOC(ZoneNum)) +   &
                       ZTOC(ZoneNum)*MCP_Total) / ((11.0d0/6.0d0)*AirCap + (1.0d0-GainsFrac)*HA_MX + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTMX(ZoneNum) = Zone1MX(ZoneNum) + TempIndCoef/AirCap
          Else
            ZTMX(ZoneNum) = (Zone1MX(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
          End If
        CASE (UseEulerMethod)
          ZTMX(ZoneNum) = (AirCap*Zone1MX(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
      ZTFLOOR(ZoneNum) = ZTOC(ZoneNum)
    END DO
    IF (PowerInPlumes .LE. 0.0d0) THEN
      HeightFrac = 0.0d0
      AirModel(ZoneNum)%SimAirModel = .FALSE.
      ZoneUFGamma(ZoneNum) = 0.0d0
      ZoneUFPowInPlumes(ZoneNum) = 0.0d0
    ELSE
      AirModel(ZoneNum)%SimAirModel = .TRUE.
      ZoneUFGamma(ZoneNum) = Gamma
      ZoneUFPowInPlumes(ZoneNum) = PowerInPlumes
    END IF
  END IF

  !=============================== M I X E D  Calculation ==============================================
  IF(ZTMX(ZoneNum) < ZTOC(ZoneNum) .or. MCP_Total <= 0.0d0 .or. &
      HeightFrac*CeilingHeight < ThickOccupiedSubzoneMin) THEN
      MIXFLAG = .TRUE.
      HeightFrac = 0.0d0
      AvgTempGrad(ZoneNum) = 0.0d0
      MaxTempGrad(ZoneNum) = 0.0d0
      AirModel(ZoneNum)%SimAirModel = .FALSE.
      AirCap = AIRRAT(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1(ZoneNum)-(3.0d0/2.0d0)*ZTM2(ZoneNum)+(1.0d0/3.0d0)*ZTM3(ZoneNum))

      DO Ctd = 1,3
        TempDepCoef = HA_MX + HA_OC + MCP_Total
        TempIndCoef = ConvGains + HAT_MX + HAT_OC + MCpT_Total
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + MCpT_Total)/ &
                    ((11.0d0/6.0d0)*AirCap + HA_MX + HA_OC + MCP_Total)
          CASE (UseAnalyticalSolution)
            If (TempDepCoef .eq. 0.0d0) Then ! B=0
              ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef/AirCap
            Else
              ZTAveraged = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
            End If
          CASE (UseEulerMethod)
            ZTAveraged = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
        END SELECT
        ZTOC(ZoneNum) = ZTAveraged
        ZTMX(ZoneNum) = ZTAveraged
        ZTFLOOR(ZoneNum) = ZTAveraged
        CALL HcUCSDUF(ZoneNum,HeightFrac)
        TempDepCoef = HA_MX + HA_OC + MCP_Total
        TempIndCoef = ConvGains + HAT_MX + HAT_OC + MCpT_Total
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + MCpT_Total)/ &
                    ((11.0d0/6.0d0)*AirCap + HA_MX + HA_OC + MCP_Total)
          CASE (UseAnalyticalSolution)
            If (TempDepCoef .eq. 0.0d0) Then ! B=0
              ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef/AirCap
            Else
              ZTAveraged = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
            End If
          CASE (UseEulerMethod)
            ZTAveraged = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
        END SELECT
        ZTOC(ZoneNum) = ZTAveraged
        ZTMX(ZoneNum) = ZTAveraged
        ZTFLOOR(ZoneNum) = ZTAveraged
      END DO

  END IF
  !=========================================================================================

! Comfort temperature and temperature at the thermostat/temperature control sensor

  HeightTransition(ZoneNum) = HeightFrac * CeilingHeight
  HeightUpSubzoneAve = (CeilingHeight + HeightTransition(ZoneNum)) / 2.d0
  HeightOccupiedSubzoneAve = HeightTransition(ZoneNum) / 2.d0
! Comfort temperature

  IF (MIXFLAG) THEN
    TCMF(ZoneNum) = ZTAveraged
  ELSE
    IF (HeightComfort < HeightOccupiedSubzoneAve) THEN
      TCMF(ZoneNum) = ZTOC(ZoneNum)
    ELSEIF (HeightComfort >= HeightOccupiedSubzoneAve .AND. HeightComfort < HeightUpSubzoneAve) THEN
      TCMF(ZoneNum) = (ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightComfort) &
                    + ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) &
                    / (HeightUpSubzoneAve - HeightOccupiedSubzoneAve)
    ELSEIF (HeightComfort >= HeightUpSubzoneAve .AND. HeightComfort <= CeilingHeight) THEN
      TCMF(ZoneNum) = ZTMX(ZoneNum)
    ELSE
      CALL ShowFatalError ('UFAD comfort height is above ceiling or below floor in Zone: '//  &
                            TRIM(Zone(ZoneNum)%Name))
    ENDIF
  ENDIF

! Temperature at the thermostat/temperature control sensor

  IF (MIXFLAG) THEN
    TempTstatAir(ZoneNum) = ZTAveraged
  ELSE
    IF (HeightThermostat < HeightOccupiedSubzoneAve) THEN
      TempTstatAir(ZoneNum) = ZTOC(ZoneNum)
    ELSEIF (HeightThermostat >= HeightOccupiedSubzoneAve .AND. HeightThermostat < HeightUpSubzoneAve) THEN
      TempTstatAir(ZoneNum) = (ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightThermostat) &
                            + ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) &
                            / (HeightUpSubzoneAve - HeightOccupiedSubzoneAve)
    ELSEIF (HeightThermostat >= HeightUpSubzoneAve .AND. HeightThermostat <= CeilingHeight) THEN
      TempTstatAir(ZoneNum) = ZTMX(ZoneNum)
    ELSE
      CALL ShowFatalError ('Underfloor air distribution thermostat height is above ceiling or below floor in Zone: '//  &
                            TRIM(Zone(ZoneNum)%Name))
    ENDIF
  ENDIF

! Temperature gradients
  IF ((HeightUpSubzoneAve - HeightOccupiedSubzoneAve) > 0.1d0) THEN
    AvgTempGrad(ZoneNum) = (ZTMX(ZoneNum)-ZTOC(ZoneNum))/(HeightUpSubzoneAve - HeightOccupiedSubzoneAve)
  ELSE
    AvgTempGrad(ZoneNum) = 0.0d0
  ENDIF

  IF (MIXFLAG) THEN
    ZoneUFMixedFlag(ZoneNum) = 1
    AirModel(ZoneNum)%SimAirModel = .FALSE.
  ELSE
    ZoneUFMixedFlag(ZoneNum) = 0
    AirModel(ZoneNum)%SimAirModel = .TRUE.
  ENDIF

  IF (ZoneEquipConfigNum > 0) THEN
    ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
    Node(ZoneNodeNum)%Temp = ZTMX(ZoneNum)
  ENDIF

  IF (MIXFLAG) THEN
    Phi(ZoneNum) = 1.0d0
  ELSE
    Phi(ZoneNum) = (ZTOC(ZoneNum) - (TSupK-KelvinConv)) / (ZTMX(ZoneNum) - (TSupK-KelvinConv))
  END IF

! Mixed for reporting purposes
  IF ((MIXFLAG) .OR. ((ZTMX(ZoneNum)-ZTOC(ZoneNum)).LT.TempDiffCritRep)) THEN
    ZoneUFMixedFlagRep(ZoneNum) = 1.d0
    HeightTransition(ZoneNum) = 0.0d0
    AvgTempGrad(ZoneNum) = 0.0d0
  ELSE
    ZoneUFMixedFlagRep(ZoneNum) = 0.0d0
  ENDIF


  RETURN

END SUBROUTINE CalcUCSDUI

SUBROUTINE CalcUCSDUE(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2006
          !       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Using the UCSD UFAD exterior zone model, this subroutine calculates the  occupied subzone height,
          ! surface heat transfer coefficients, the occupied subzone temperature, and the upper subzone temperature.

          ! METHODOLOGY EMPLOYED:
          ! The zone is divided into 2 subzones with a variable transition height.

          ! REFERENCES:
          ! The model is described in the EnergyPlus Engineering Reference in Anna Liu's UCSD PhD thesis.

          ! USE STATEMENTS:
  USE ScheduleManager,            ONLY: GetCurrentScheduleValue
  USE DataZoneEquipment,          ONLY: ZoneEquipConfig
  USE Psychrometrics,             ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataHeatBalFanSys
  USE DataHVACGlobals,            ONLY: TimestepSys, UseZoneTimeStepHistory
  USE InternalHeatGains,          ONLY: SumInternalConvectionGainsByTypes, SumReturnAirConvectionGainsByTypes

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum       ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: MIXFLAG = .FALSE.  ! if true treat as a mixed zone
  REAL(r64)   :: CeilingHeight      ! zone ceiling height above floor [m]
  INTEGER :: UINum              ! index to underfloor interior zone model data
  REAL(r64)   :: GainsFrac          ! fraction of occupied subzone heat gains that remain in the subzone;
                                ! that is, don't go into the plumes
  REAL(r64)   :: HeightThermostat   ! height of the thermostat above the floor [m]
  REAL(r64)   :: HeightComfort      ! height at which comfort temperature is calculated
  REAL(r64)   :: TempDiffCritRep    ! Minimum temperature difference between upper and occupied subzones for reporting
  REAL(r64)   :: ConvGainsOccSubzone ! convective heat gains into the lower (occupied) subzone [W]
  REAL(r64)   :: ConvGainsUpSubzone  ! convective heat gains into the upper subzone [W]
  REAL(r64)   :: ConvGains           ! total zone convective gains (excluding surfaces) [W]
  REAL(r64)   :: ConvGainsWindows    ! convective gain from windows [W]
  INTEGER   :: ZoneEquipConfigNum ! ZoneEquipConfig index for this UFAD zone
  REAL(r64)   :: SumSysMCp          ! Sum of system mass flow rate * specific heat for this zone [W/K]
  REAL(r64)   :: SumSysMCpT         ! Sum of system mass flow rate * specific heat * temperature for this zone [W]
  REAL(r64)   :: SumSysM            ! Sum of systems mass flow rate [kg/s]
  REAL(r64)   :: NodeTemp           ! inlet node temperature [K]
  REAL(r64)   :: MassFlowRate       ! system mass flow rate [kg/s]
  REAL(r64)   :: CpAir              ! specific heat of air [J/kgK]
  INTEGER :: InNodeIndex        ! inlet node index in ZoneEquipConfig
  REAL(r64)   :: SumMCp             ! mass flow rate * specific heat for this zone for infiltration, ventilation, mixing [W/K]
  REAL(r64)   :: SumMCpT            ! mass flow rate * specific heat* temp for this zone for infiltration, ventilation, mixing [W]
  REAL(r64)   :: MCP_Total          ! total mass flow rate * specific heat for this zone [W/K]
  REAL(r64)   :: MCpT_Total         ! total mass flow rate * specific heat* temp for this zone [W]
  REAL(r64)   :: NumberOfPlumes
  REAL(r64)   :: PowerInPlumes      ! [W]
  REAL(r64)   :: PowerPerPlume=0.0d0  ! power carried by each plume [W]
  REAL(r64)   :: PowerInPlumesPerMeter ! Power in Plumes per meter of window length [W/m]
  REAL(r64)   :: NumDiffusersPerPlume = 0.0d0
  REAL(r64)   :: HeightFrac         ! Fractional height of transition between occupied and upper subzones
  REAL(r64)   :: TotSysFlow         ! [m3/s]
  REAL(r64)   :: NumDiffusers
  REAL(r64)   :: TSupK              ! supply yemperature [K]
  REAL(r64)   :: Gamma              ! dimensionless height parameter; higher gamma means interface height will be
                                    ! higher, smaller gamma means interface height will be lower.
  REAL(r64)   :: DiffArea           ! diffuser effective area [m2]
  REAL(r64)   :: ThrowAngle         ! diffuser slot angle relative to vertical [radians]
  REAL(r64)   :: SourceHeight       ! height of plume sources above the floor [m]
  INTEGER :: Ctd
  REAL(r64)   :: AirCap
  REAL(r64)   :: TempHistTerm
  REAL(r64)   :: ZTAveraged
  REAL(r64)   :: HeightUpSubzoneAve        ! Height of center of upper air subzone
  REAL(r64)   :: HeightOccupiedSubzoneAve  ! Height of center of occupied air subzone
  REAL(r64)   :: ZoneMult     ! total zone multiplier
  INTEGER :: ZoneNodeNum               ! node number of the HVAC zone node
  REAL(r64)   :: TempDepCoef=0.0d0           ! Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
  REAL(r64)   :: TempIndCoef=0.0d0           ! Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
  INTEGER, DIMENSION(28) :: IntGainTypesOccupied = (/IntGainTypeOf_People, &
                                                    IntGainTypeOf_WaterHeaterMixed, &
                                                    IntGainTypeOf_WaterHeaterStratified, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterMixed, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterStratified, &
                                                    IntGainTypeOf_ElectricEquipment, &
                                                    IntGainTypeOf_GasEquipment, &
                                                    IntGainTypeOf_HotWaterEquipment, &
                                                    IntGainTypeOf_SteamEquipment, &
                                                    IntGainTypeOf_OtherEquipment, &
                                                    IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, &
                                                    IntGainTypeOf_GeneratorFuelCell, &
                                                    IntGainTypeOf_WaterUseEquipment, &
                                                    IntGainTypeOf_GeneratorMicroCHP, &
                                                    IntGainTypeOf_ElectricLoadCenterTransformer, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterSimple, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageBattery, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageSimple, &
                                                    IntGainTypeOf_PipeIndoor, &
                                                    IntGainTypeOf_RefrigerationCase, &
                                                    IntGainTypeOf_RefrigerationCompressorRack, &
                                                    IntGainTypeOf_RefrigerationSystemAirCooledCondenser ,&
                                                    IntGainTypeOf_RefrigerationSystemSuctionPipe, &
                                                    IntGainTypeOf_RefrigerationSecondaryReceiver, &
                                                    IntGainTypeOf_RefrigerationSecondaryPipe, &
                                                    IntGainTypeOf_RefrigerationWalkIn/)

  INTEGER, DIMENSION(2) :: IntGainTypesUpSubzone = (/IntGainTypeOf_DaylightingDeviceTubular , &
                                                        IntGainTypeOf_Lights/)
  REAL(r64)   :: RetAirGains


  ! Exact solution or Euler method
  If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
    If (ShortenTimeStepSysRoomAir .and. TimeStepSys .LT. TimeStepZone) Then
      If (PreviousTimeStep < TimeStepZone) Then
        Zone1OC(ZoneNum) = ZoneM2OC(ZoneNum)
        Zone1MX(ZoneNum) = ZoneM2MX(ZoneNum)
      Else
        Zone1OC(ZoneNum) = ZoneMXOC(ZoneNum)
        Zone1MX(ZoneNum) = ZoneMXMX(ZoneNum)
      End If
    Else
      Zone1OC(ZoneNum) = ZTOC(ZoneNum)
      Zone1MX(ZoneNum) = ZTMX(ZoneNum)
    End If
  End If

  HeightFrac = 0.0d0
  MIXFLAG = .FALSE.
  UFHcIn = HConvIn
  SumSysMCp = 0.0d0
  SumSysMCpT = 0.0d0
  TotSysFlow = 0.0d0
  TSupK = 0.0d0
  SumSysM = 0.0d0
  PowerInPlumes = 0.0d0
  ConvGainsWindows = 0.0d0
  Gamma = 0.0d0
  ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
  CeilingHeight = ZoneCeilingHeight((ZoneNum-1)*2 + 2) - ZoneCeilingHeight((ZoneNum-1)*2 + 1)
  UINum = ZoneUFPtr(ZoneNum)
  HeightThermostat = ZoneUCSDUE(UINum)%ThermostatHeight
  HeightComfort = ZoneUCSDUE(UINum)%ComfortHeight
  TempDiffCritRep = ZoneUCSDUE(UINum)%TempTrigger
  DiffArea = ZoneUCSDUE(UINum)%DiffArea
  ThrowAngle = DegToRadians*ZoneUCSDUE(UINum)%DiffAngle
  SourceHeight = ZoneUCSDUE(UINum)%HeatSrcHeight
  NumDiffusers = ZoneUCSDUE(UINum)%DiffusersPerZone
  PowerPerPlume = ZoneUCSDUE(UINum)%PowerPerPlume
  ! gains from occupants, task lighting, elec equip, gas equip, other equip, hot water equip, steam equip,
  ! baseboards (nonthermostatic), water heater skin loss
  CALL SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, ConvGainsOccSubzone)

  ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
  ! low or zero)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, RetAirGains)
    ConvGainsOccSubzone = ConvGainsOccSubzone + RetAirGains
  END IF

  ! gains from lights (ceiling), tubular daylighting devices, high temp radiant heaters
  CALL SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesUpSubzone, ConvGainsUpSubzone)
  ConvGainsUpSubzone = ConvGainsUpSubzone +  SumConvHTRadSys(ZoneNum)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesUpSubzone, RetAirGains)
    ConvGainsUpSubzone = ConvGainsUpSubzone + RetAirGains
  END IF
  ConvGains = ConvGainsOccSubzone + ConvGainsUpSubzone + SysDepZoneLoadsLagged(ZoneNum)
  ZoneEquipConfigNum = ZoneUCSDUE(UINum)%ZoneEquipPtr
  IF (ZoneEquipConfigNum > 0) THEN
    DO InNodeIndex = 1,ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
      NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(InNodeIndex))%Temp
      MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(InNodeIndex))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
      SumSysMCp = SumSysMCp + MassFlowRate * CpAir
      SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
      TotSysFlow = TotSysFlow + MassFlowRate / PsyRhoAirFnPbTdbW(OutBaroPress,NodeTemp,ZoneAirHumRat(ZoneNum))
      TSupK = TSupK + MassFlowRate * NodeTemp
      SumSysM = SumSysM + MassFlowRate
    END DO
    IF (TotSysFlow > 0.0d0) THEN
      TSupK = TSupK/SumSysM + KelvinConv
    ELSE
      TSupK = 0.0d0
    END IF
  END IF
  ! mass flow times specific heat for infiltration, ventilation, mixing
  SumMCp = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MdotCPOA(ZoneNum)
  ! mass flow times specific heat times temperature for infiltration, ventilation, mixing
  SumMCpT = MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MdotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp

  MCP_Total = SumMCp + SumSysMCp
  MCpT_Total = SumMCpT + SumSysMCpT

  ! For the York MIT diffusers (variable area) the area varies with the flow rate. Assume 400 ft/min velocity
  ! at the diffuser, and a design flow rate of 150 cfm (.0708 m3/s). Then the design area for each diffuser is
  ! 150 ft3/min / 400 ft/min = .375 ft2 = .035 m2. This is adjusted each time step by
  !               (TotSysFlow/(NumDiffusers*.0708))*.035
  IF (ZoneUCSDUE(UINum)%DiffuserType .EQ. VarArea) THEN
    DiffArea = .035d0*TotSysFlow/(.0708d0*NumDiffusers)
  END IF
  ! initial estimate of convective transfer from surfaces; assume HeightFrac is 0.5.
  CALL HcUCSDUF(ZoneNum,0.5d0)
  ConvGainsWindows = HAT_MXWin + HAT_OCWin - HA_MXWin*ZTMX(ZoneNum) - HA_OCWin*ZTOC(ZoneNum)
  PowerInPlumes = ConvGains + HAT_OC - HA_OC*ZTOC(ZoneNum) + HAT_MX - HA_MX*ZTMX(ZoneNum)
  ! NumberOfPlumes = PowerInPlumes / PowerPerPlume
  IF (PowerPerPlume > 0.0d0 .AND. PowerInPlumes > 0.0d0) THEN
    NumberOfPlumes = PowerInPlumes / PowerPerPlume
    NumDiffusersPerPlume = NumDiffusers / NumberOfPlumes
  ELSE
    NumberOfPlumes = 1.0d0
    NumDiffusersPerPlume = 1.0d0
  END IF
  IF ((PowerInPlumes <= 0.0d0) .OR. (TotSysFlow .EQ. 0.0d0) .OR. (TsupK-KelvinConv) > MAT(ZoneNum)) THEN
    ! The system will mix
    HeightFrac = 0.0d0
  ELSE
    IF (PowerInPlumes > 0.0d0) THEN
      IF (ZoneUCSDUE(UINum)%WinWidth > 0.0d0) THEN  ! exterior zone formula
        PowerInPlumesPerMeter = PowerInPlumes / ZoneUCSDUE(UINum)%WinWidth
        Gamma = (TotSysFlow*COS(ThrowAngle)) / (NumDiffusers*DiffArea &
                * (0.0281d0*0.001d0*PowerInPlumesPerMeter)**0.333333d0)
      ELSE  ! interior zone formula
        Gamma = (TotSysFlow*COS(ThrowAngle))**1.5d0 / (NumberOfPlumes*(NumDiffusersPerPlume*DiffArea)**1.25d0 &
                * (0.0281d0*0.001d0*PowerInPlumes)**0.5d0)
      END IF
    ELSE
      Gamma = 1000.d0
    END IF
    IF (ZoneUCSDUE(UINum)%CalcTransHeight) THEN
      IF (ZoneUCSDUE(UINum)%WinWidth > 0.0d0) THEN ! use exterior zone formula
        HeightFrac = (DiffArea**0.5d0 * (11.03d0*log(Gamma) - 10.73d0) + 0.5d0*SourceHeight) / CeilingHeight
      ELSE ! use interior zone formula
        HeightFrac = ((NumDiffusersPerPlume*DiffArea)**0.5d0 * (7.43d0*log(Gamma) - 1.35d0) + 0.5d0*SourceHeight) / CeilingHeight
      END IF
    ELSE
      HeightFrac = ZoneUCSDUE(UINum)%TransHeight / CeilingHeight
    END IF
    HeightFrac = MAX(0.0d0,MIN(1.0d0,HeightFrac))
    GainsFrac = ZoneUCSDUE(UINum)%A_Kc * Gamma**ZoneUCSDUE(UINum)%B_Kc + ZoneUCSDUE(UINum)%C_Kc +   &
                ZoneUCSDUE(UINum)%D_Kc * Gamma + ZoneUCSDUE(UINum)%E_Kc * Gamma**2
    GainsFrac = MAX(0.7d0,MIN(GainsFrac,1.0d0))
    IF (ZoneUCSDUE(UINum)%ShadeDown) THEN
      GainsFrac = GainsFrac - 0.2d0
    END IF
    ZoneUFPowInPlumes(ZoneNum) = PowerInPlumes
    DO Ctd = 1,4
      CALL HcUCSDUF(ZoneNum,HeightFrac)
      ConvGainsWindows = HAT_MXWin + HAT_OCWin - HA_MXWin*ZTMX(ZoneNum) - HA_OCWin*ZTOC(ZoneNum)
      ConvGainsWindows = MAX(ConvGainsWindows,0.0d0)
      PowerInPlumes = ConvGains + HAT_OC - HA_OC*ZTOC(ZoneNum) + HAT_MX - HA_MX*ZTMX(ZoneNum)
      ! NumberOfPlumes = PowerInPlumes / PowerPerPlume
      NumberOfPlumes = 1.0d0
      IF (PowerInPlumes .LE. 0.0d0) EXIT
      IF (ZoneUCSDUE(UINum)%WinWidth > 0.0d0) THEN ! use exterior zone formula
        PowerInPlumesPerMeter = PowerInPlumes / ZoneUCSDUE(UINum)%WinWidth
        Gamma = (TotSysFlow*COS(ThrowAngle)) / (NumDiffusers*DiffArea &
                * (0.0281d0*0.001d0*PowerInPlumesPerMeter)**0.333333d0)
      ELSE ! use interior zone formula
        Gamma = (TotSysFlow*COS(ThrowAngle))**1.5d0 / (NumberOfPlumes*(NumDiffusersPerPlume*DiffArea)**1.25d0 &
                * (0.0281d0*0.001d0*PowerInPlumes)**0.5d0)
      END IF
      IF (ZoneUCSDUE(UINum)%CalcTransHeight) THEN
        IF (ZoneUCSDUE(UINum)%WinWidth > 0.0d0) THEN ! exterior zone formula
          HeightFrac = (DiffArea**0.5d0 * (11.03d0*log(Gamma) - 10.73d0) + 0.5d0*SourceHeight) / CeilingHeight
        ELSE ! interior zone formula
          HeightFrac = ((NumDiffusersPerPlume*DiffArea)**0.5d0 * (7.43d0*log(Gamma) - 1.35d0) + 0.5d0*SourceHeight) / CeilingHeight
        END IF
      ELSE
        HeightFrac = ZoneUCSDUE(UINum)%TransHeight / CeilingHeight
      END IF
      HeightFrac = MIN(1.0d0,HeightFrac)
      HeightTransition(ZoneNum) = HeightFrac * CeilingHeight
      GainsFrac = ZoneUCSDUE(UINum)%A_Kc * Gamma**ZoneUCSDUE(UINum)%B_Kc + ZoneUCSDUE(UINum)%C_Kc +   &
                  ZoneUCSDUE(UINum)%D_Kc * Gamma + ZoneUCSDUE(UINum)%E_Kc * Gamma**2
      GainsFrac = MAX(0.7d0,MIN(GainsFrac,1.0d0))
      IF (ZoneUCSDUE(UINum)%ShadeDown) THEN
        GainsFrac = GainsFrac - 0.2d0
      END IF
      AIRRATOC(ZoneNum) = Zone(ZoneNum)%Volume*(HeightTransition(ZoneNum)-MIN(HeightTransition(ZoneNum),0.2d0)) &
                        /CeilingHeight*ZoneVolCapMultpSens &
                        *PsyRhoAirFnPbTdbW(OutBaroPress,MATOC(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                        *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATOC(ZoneNum))/(TimeStepSys*SecInHour)
      AIRRATMX(ZoneNum) = Zone(ZoneNum)%Volume*(CeilingHeight-HeightTransition(ZoneNum)) &
                        /CeilingHeight*ZoneVolCapMultpSens &
                        *PsyRhoAirFnPbTdbW(OutBaroPress,MATMX(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                        *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATMX(ZoneNum))/(TimeStepSys*SecInHour)
      IF (UseZoneTimeStepHistory) THEN
        ZTM3OC(ZoneNum)    = XM3TOC(ZoneNum)
        ZTM2OC(ZoneNum)    = XM2TOC(ZoneNum)
        ZTM1OC(ZoneNum)    = XMATOC(ZoneNum)

        ZTM3MX(ZoneNum)    = XM3TMX(ZoneNum)
        ZTM2MX(ZoneNum)    = XM2TMX(ZoneNum)
        ZTM1MX(ZoneNum)    = XMATMX(ZoneNum)

      ELSE
        ZTM3OC(ZoneNum)    = DSXM3TOC(ZoneNum)
        ZTM2OC(ZoneNum)    = DSXM2TOC(ZoneNum)
        ZTM1OC(ZoneNum)    = DSXMATOC(ZoneNum)

        ZTM3MX(ZoneNum)    = DSXM3TMX(ZoneNum)
        ZTM2MX(ZoneNum)    = DSXM2TMX(ZoneNum)
        ZTM1MX(ZoneNum)    = DSXMATMX(ZoneNum)

      ENDIF

      AirCap = AIRRATOC(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1OC(ZoneNum)-(3.0d0/2.0d0)*ZTM2OC(ZoneNum)+(1.0d0/3.0d0)*ZTM3OC(ZoneNum))
      TempDepCoef = GainsFrac*HA_OC + MCP_Total
      TempIndCoef = GainsFrac*(ConvGains+HAT_OC+HAT_MX-HA_MX*ZTMX(ZoneNum))+MCPT_Total+NonAirSystemResponse(ZoneNum)/ZoneMult
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTOC(ZoneNum) = (TempHistTerm + GainsFrac*(ConvGains + HAT_OC + HAT_MX - HA_MX*ZTMX(ZoneNum))&
                        + MCPT_Total + NonAirSystemResponse(ZoneNum)/ZoneMult) &
                        / ((11.0d0/6.0d0)*AirCap + GainsFrac*HA_OC + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTOC(ZoneNum) = Zone1OC(ZoneNum) + TempIndCoef/AirCap
          Else
            ZTOC(ZoneNum) = (Zone1OC(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
          End If
        CASE (UseEulerMethod)
          ZTOC(ZoneNum) = (AirCap*Zone1OC(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
      AirCap = AIRRATMX(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1MX(ZoneNum)-(3.0d0/2.0d0)*ZTM2MX(ZoneNum)+(1.0d0/3.0d0)*ZTM3MX(ZoneNum))
      TempDepCoef = (1.0d0-GainsFrac)*HA_MX + MCP_Total
      TempIndCoef = (1.0d0-GainsFrac)*(ConvGains + HAT_OC + HAT_MX - HA_OC*ZTOC(ZoneNum)) + ZTOC(ZoneNum)*MCP_Total
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTMX(ZoneNum) = (TempHistTerm + (1.0d0-GainsFrac)*(ConvGains + HAT_OC + HAT_MX - HA_OC*ZTOC(ZoneNum))   &
         + ZTOC(ZoneNum)*MCP_Total) / ((11.0d0/6.0d0)*AirCap + (1.0d0-GainsFrac)*HA_MX + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTMX(ZoneNum) = Zone1MX(ZoneNum) + TempIndCoef/AirCap
          Else
            ZTMX(ZoneNum) = (Zone1MX(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
          End If
        CASE (UseEulerMethod)
          ZTMX(ZoneNum) = (AirCap*Zone1MX(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
      ZTFLOOR(ZoneNum) = ZTOC(ZoneNum)
    END DO
    IF (PowerInPlumes .LE. 0.0d0) THEN
      HeightFrac = 0.0d0
      AirModel(ZoneNum)%SimAirModel = .FALSE.
      ZoneUFGamma(ZoneNum) = 0.0d0
      ZoneUFPowInPlumes(ZoneNum) = 0.0d0
      ZoneUFPowInPlumesfromWindows(ZoneNum) = 0.0d0
    ELSE
      AirModel(ZoneNum)%SimAirModel = .TRUE.
      ZoneUFGamma(ZoneNum) = Gamma
      ZoneUFPowInPlumes(ZoneNum) = PowerInPlumes
      ZoneUFPowInPlumesfromWindows(ZoneNum) = ConvGainsWindows
    END IF
  END IF

  !=============================== M I X E D  Calculation ==============================================
  IF(ZTMX(ZoneNum) < ZTOC(ZoneNum) .or. MCP_Total <= 0.0d0 .or. &
      HeightFrac*CeilingHeight < ThickOccupiedSubzoneMin) THEN
      MIXFLAG = .TRUE.
      HeightFrac = 0.0d0

      AvgTempGrad(ZoneNum) = 0.0d0
      MaxTempGrad(ZoneNum) = 0.0d0
      AirModel(ZoneNum)%SimAirModel = .FALSE.
      AirCap = AIRRAT(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1(ZoneNum)-(3.0d0/2.0d0)*ZTM2(ZoneNum)+(1.0d0/3.0d0)*ZTM3(ZoneNum))

      DO Ctd = 1,3
        TempDepCoef = HA_MX + HA_OC + MCP_Total
        TempIndCoef = ConvGains + HAT_MX + HAT_OC + MCpT_Total
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + MCpT_Total)/ &
                    ((11.0d0/6.0d0)*AirCap + HA_MX + HA_OC + MCP_Total)
          CASE (UseAnalyticalSolution)
            If (TempDepCoef .eq. 0.0d0) Then ! B=0
              ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef/AirCap
            Else
              ZTAveraged = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
            End If
          CASE (UseEulerMethod)
            ZTAveraged = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
        END SELECT
        ZTOC(ZoneNum) = ZTAveraged
        ZTMX(ZoneNum) = ZTAveraged
        ZTFLOOR(ZoneNum) = ZTAveraged
        CALL HcUCSDUF(ZoneNum,HeightFrac)
        TempDepCoef = HA_MX + HA_OC + MCP_Total
        TempIndCoef = ConvGains + HAT_MX + HAT_OC + MCpT_Total
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + MCpT_Total)/ &
                    ((11.0d0/6.0d0)*AirCap + HA_MX + HA_OC + MCP_Total)
          CASE (UseAnalyticalSolution)
            If (TempDepCoef .eq. 0.0d0) Then ! B=0
              ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef/AirCap
            Else
              ZTAveraged = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
            End If
          CASE (UseEulerMethod)
            ZTAveraged = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
        END SELECT
        ZTOC(ZoneNum) = ZTAveraged
        ZTMX(ZoneNum) = ZTAveraged
        ZTFLOOR(ZoneNum) = ZTAveraged
      END DO

  END IF
  !=========================================================================================

! Comfort temperature and temperature at the thermostat/temperature control sensor

  HeightUpSubzoneAve = (CeilingHeight + HeightTransition(ZoneNum)) / 2.d0
  HeightOccupiedSubzoneAve = HeightTransition(ZoneNum) / 2.d0
! Comfort temperature

  IF (MIXFLAG) THEN
    TCMF(ZoneNum) = ZTAveraged
  ELSE
    IF (HeightComfort < HeightOccupiedSubzoneAve) THEN
      TCMF(ZoneNum) = ZTOC(ZoneNum)
    ELSEIF (HeightComfort >= HeightOccupiedSubzoneAve .AND. HeightComfort < HeightUpSubzoneAve) THEN
      TCMF(ZoneNum) = (ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightComfort) &
                    + ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) &
                    / (HeightUpSubzoneAve - HeightOccupiedSubzoneAve)
    ELSEIF (HeightComfort >= HeightUpSubzoneAve .AND. HeightComfort <= CeilingHeight) THEN
      TCMF(ZoneNum) = ZTMX(ZoneNum)
    ELSE
      CALL ShowFatalError ('UFAD comfort height is above ceiling or below floor in Zone: '//  &
                            TRIM(Zone(ZoneNum)%Name))
    ENDIF
  ENDIF

! Temperature at the thermostat/temperature control sensor

  IF (MIXFLAG) THEN
    TempTstatAir(ZoneNum) = ZTAveraged
  ELSE
    IF (HeightThermostat < HeightOccupiedSubzoneAve) THEN
      TempTstatAir(ZoneNum) = ZTOC(ZoneNum)
    ELSEIF (HeightThermostat >= HeightOccupiedSubzoneAve .AND. HeightThermostat < HeightUpSubzoneAve) THEN
      TempTstatAir(ZoneNum) = (ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightThermostat) &
                            + ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) &
                            / (HeightUpSubzoneAve - HeightOccupiedSubzoneAve)
    ELSEIF (HeightThermostat >= HeightUpSubzoneAve .AND. HeightThermostat <= CeilingHeight) THEN
      TempTstatAir(ZoneNum) = ZTMX(ZoneNum)
    ELSE
      CALL ShowFatalError ('Underfloor air distribution thermostat height is above ceiling or below floor in Zone: '//  &
                            TRIM(Zone(ZoneNum)%Name))
    ENDIF
  ENDIF

! Temperature gradients
  IF ((HeightUpSubzoneAve - HeightOccupiedSubzoneAve) > 0.1d0) THEN
    AvgTempGrad(ZoneNum) = (ZTMX(ZoneNum)-ZTOC(ZoneNum))/(HeightUpSubzoneAve - HeightOccupiedSubzoneAve)
  ELSE
    AvgTempGrad(ZoneNum) = 0.0d0
  ENDIF

  IF (MIXFLAG) THEN
    ZoneUFMixedFlag(ZoneNum) = 1
    AirModel(ZoneNum)%SimAirModel = .FALSE.
  ELSE
    ZoneUFMixedFlag(ZoneNum) = 0
    AirModel(ZoneNum)%SimAirModel = .TRUE.
  ENDIF

  IF (ZoneEquipConfigNum > 0) THEN
    ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
    Node(ZoneNodeNum)%Temp = ZTMX(ZoneNum)
  ENDIF

  IF (MIXFLAG) THEN
    Phi(ZoneNum) = 1.0d0
  ELSE
    Phi(ZoneNum) = (ZTOC(ZoneNum) - (TSupK-KelvinConv)) / (ZTMX(ZoneNum) - (TSupK-KelvinConv))
  END IF

! Mixed for reporting purposes
  IF ((MIXFLAG) .OR. ((ZTMX(ZoneNum)-ZTOC(ZoneNum)).LT.TempDiffCritRep)) THEN
    ZoneUFMixedFlagRep(ZoneNum) = 1.d0
    HeightTransition(ZoneNum) = 0.0d0
    AvgTempGrad(ZoneNum) = 0.0d0
  ELSE
    ZoneUFMixedFlagRep(ZoneNum) = 0.0d0
  ENDIF


  RETURN

END SUBROUTINE CalcUCSDUE

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

END MODULE UFADManager

