MODULE SteamCoils

          ! Module containing the SteamCoil simulation routines

          ! MODULE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       na
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms required to
          ! manage the SteamCoil System Component.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! Use statements for data only modules
 USE DataPrecisionGlobals
 USE DataLoopNode
 USE DataGlobals
 USE DataInterfaces
 USE DataHVACGlobals
 USE Psychrometrics
 USE FluidProperties
 USE DataEnvironment, ONLY: StdBaroPress
 USE DataPlant,       ONLY: TypeOf_CoilSteamAirHeating, ScanPlantLoopsForObject, PlantLoop, MyPlantSizingIndex



          ! Use statements for access to subroutines in other modules
 USE ScheduleManager

 IMPLICIT NONE         ! Enforce explicit typing of all variables

 PRIVATE ! Everything private unless explicitly made public

         ! MODULE PARAMETER DEFINITIONS:
 INTEGER, PUBLIC, PARAMETER :: SteamCoil_AirHeating = 2
 INTEGER, PUBLIC, PARAMETER :: TemperatureSetpointControl = 1
 INTEGER, PUBLIC, PARAMETER :: ZoneLoadControl = 3

     ! DERIVED TYPE DEFINITIONS
 TYPE SteamCoilEquipConditions
     CHARACTER(len=MaxNameLength) :: Name           = ' ' ! Name of the SteamCoil
     CHARACTER(len=10)            :: SteamCoilTypeA = ' ' ! Type of SteamCoil ie. Heating or Cooling
     INTEGER      :: SteamCoilType                  = 0   ! Type of SteamCoil ie. Heating or Cooling
     INTEGER      :: SteamCoilModel                 = 0   ! Type of SteamCoil ie. Simple, Detailed, etc.
     INTEGER      :: SteamCoilType_Num              = 0   !
     CHARACTER(len=MaxNameLength) :: Schedule       = ' ' ! SteamCoil Operation Schedule
     INTEGER      :: SchedPtr                       = 0   ! Pointer to the correct schedule
     REAL(r64)    :: InletAirMassFlowRate           = 0.0d0 ! MassFlow through the SteamCoil being Simulated [kg/s]
     REAL(r64)    :: OutletAirMassFlowRate          = 0.0d0 ! MassFlow throught the SteamCoil being Simulated[kg/s]
     REAL(r64)    :: InletAirTemp                   = 0.0d0 ! Inlet Air Temperature Operating Condition [C]
     REAL(r64)    :: OutletAirTemp                  = 0.0d0 ! Outlet Air Temperature Operating Condition [C]
     REAL(r64)    :: InletAirHumRat                 = 0.0d0 ! Inlet Air Humidity Ratio Operating Condition
     REAL(r64)    :: OutletAirHumRat                = 0.0d0 ! Outlet Air Humidity Ratio Calculated Condition
     REAL(r64)    :: InletAirEnthalpy               = 0.0d0 ! Inlet Air enthalpy [J/kg]
     REAL(r64)    :: OutletAirEnthalpy              = 0.0d0 ! Outlet Air enthalpy [J/kg]
     REAL(r64)    :: TotSteamCoilLoad               = 0.0d0 ! Total Load on the Coil [W]
     REAL(r64)    :: SenSteamCoilLoad               = 0.0d0 ! Sensible Load on the Coil [W]
     REAL(r64)    :: TotSteamHeatingCoilEnergy      = 0.0d0 ! Total Heating Coil energy of the Coil [J]
     REAL(r64)    :: TotSteamCoolingCoilEnergy      = 0.0d0 ! Total Cooling Coil energy of the Coil [J]
     REAL(r64)    :: SenSteamCoolingCoilEnergy      = 0.0d0 ! Sensible Cooling Coil energy of the Coil [J]
     REAL(r64)    :: TotSteamHeatingCoilRate        = 0.0d0 ! Total Heating Coil Rate on the Coil [W]
     REAL(r64)    :: LoopLoss                       = 0.0d0 ! Loss in loop due to cond return to atm pressure
     REAL(r64)    :: TotSteamCoolingCoilRate        = 0.0d0 ! Total Cooling Coil Rate on the Coil [W]
     REAL(r64)    :: SenSteamCoolingCoilRate        = 0.0d0 ! Sensible Cooling Coil Rate on the Coil [W]
     REAL(r64)    :: LeavingRelHum                  = 0.0d0 ! Simple Coil Latent Model requires User input for leaving RH
     REAL(r64)    :: DesiredOutletTemp              = 0.0d0 ! Temp desired at the outlet (C)
     REAL(r64)    :: DesiredOutletHumRat            = 0.0d0 ! Humudity Ratio desired at outlet (C)
     REAL(r64)    :: InletSteamTemp                 = 0.0d0 ! Inlet Steam Temperature [C]
     REAL(r64)    :: OutletSteamTemp                = 0.0d0 ! Outlet Steam Temperature [C]
     REAL(r64)    :: InletSteamMassFlowRate         = 0.0d0 ! Inlet Steam Mass Flow Rate [Kg/s]
     REAL(r64)    :: OutletSteamMassFlowRate        = 0.0d0 ! Outlet Steam Mass Flow Rate [Kg/s]
     REAL(r64)    :: MaxSteamVolFlowRate            = 0.0d0 ! Maximum water Volume flow rate [m3/s]
     REAL(r64)    :: MaxSteamMassFlowRate           = 0.0d0 ! Maximum water mass flow rate [Kg/s]
     REAL(r64)    :: InletSteamEnthalpy             = 0.0d0 ! Inlet Water Enthalpy (J/Kg)
     REAL(r64)    :: OutletWaterEnthalpy            = 0.0d0 ! Outlet Water Enthalpy (J/kg)
     REAL(r64)    :: InletSteamPress                = 0.0d0 ! Pressure at steam inlet (Pa)
     REAL(r64)    :: InletSteamQuality              = 0.0d0 ! Quality of steam at inlet
     REAL(r64)    :: OutletSteamQuality             = 0.0d0 ! Quality of steam at outlet
     REAL(r64)    :: DegOfSubCooling                = 0.0d0
     REAL(r64)    :: LoopSubCoolReturn              = 0.0d0
     INTEGER      :: AirInletNodeNum                = 0   ! Inlet node number at air side
     INTEGER      :: AirOutletNodeNum               = 0   ! Outlet node number at air side
     INTEGER      :: SteamInletNodeNum              = 0   ! SteamInletNodeNum
     INTEGER      :: SteamOutletNodeNum             = 0   ! SteamOutletNodeNum
     INTEGER      :: TempSetPointNodeNum            = 0   ! If applicable : node number that the temp setpoint exists.
     INTEGER      :: TypeofCoil                     = 0   ! Control of Coil , temperature or Zone load
     INTEGER      :: FluidIndex                     = 0   ! Fluid index for FluidProperties (Steam)
     INTEGER      :: LoopNum                        = 0   ! index for plant loop with steam coil
     INTEGER      :: LoopSide                       = 0   ! index for plant loop side for steam coil
     INTEGER      :: BranchNum                      = 0   ! index for plant branch for steam coil
     INTEGER      :: CompNum                        = 0   ! index for plant component for steam coil
     INTEGER      :: Coil_PlantTypeNum              = 0   ! plant level index for coil type
     REAL(r64)    :: OperatingCapacity              = 0.0d0 ! capacity of steam coil at operating conditions (W)
 END TYPE SteamCoilEquipConditions

  ! INTERFACE DEFINITIONS
INTERFACE GetCoilSteamInletNode
  MODULE PROCEDURE iGetCoilSteamInletNode, cGetCoilSteamInletNode
END INTERFACE GetCoilSteamInletNode
INTERFACE GetCoilSteamOutletNode
  MODULE PROCEDURE iGetCoilSteamOutletNode, cGetCoilSteamOutletNode
END INTERFACE GetCoilSteamOutletNode
INTERFACE GetCoilAirOutletNode
  MODULE PROCEDURE iGetCoilAirOutletNode, cGetCoilAirOutletNode
END INTERFACE GetCoilAirOutletNode
          ! MODULE VARIABLE DECLARATIONS:
 INTEGER :: SteamIndex=0
 INTEGER :: NumSteamCoils=0   ! The Number of SteamCoils found in the Input
 TYPE (SteamCoilEquipConditions), ALLOCATABLE, DIMENSION(:) :: SteamCoil
 LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
 LOGICAL, ALLOCATABLE, DIMENSION(:) :: CoilWarningOnceFlag
 LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
 LOGICAL      :: GetSteamCoilsInputFlag = .True. ! Flag set to make sure you get input once

 ! Subroutine Specifications for the Module
 ! Driver/Manager Routines
 PUBLIC  SimulateSteamCoilComponents

 ! Get Input routines for module
 PRIVATE GetSteamCoilInput

 ! Initialization routines for module
 PRIVATE InitSteamCoil
 PRIVATE SizeSteamCoil

 ! Algorithms for the module
 PRIVATE CalcSteamAirCoil

 ! Update routine to check convergence and update nodes
 PRIVATE UpdateSteamCoil

 ! Reporting routines for module
 PRIVATE ReportSteamCoil

 ! Utility routines for module
 PUBLIC  CheckSteamCoilSchedule
 PUBLIC  GetCoilMaxWaterFlowRate
 PUBLIC  GetCoilMaxSteamFlowRate
 PUBLIC  GetCoilAirInletNode
 PUBLIC  GetCoilAirOutletNode
 PUBLIC  GetCoilSteamInletNode
 PUBLIC  GetCoilSteamOutletNode
 PUBLIC  GetCoilCapacity
 PUBLIC  GetTypeOfCoil
 PUBLIC  GetSteamCoilIndex
 PUBLIC  GetSteamCoilControlNodeNum
 PUBLIC  GetSteamCoilAvailScheduleIndex

CONTAINS

            ! MODULE SUBROUTINES:
 SUBROUTINE SimulateSteamCoilComponents(CompName,FirstHVACIteration,QCoilReq,CompIndex,QCoilActual,FanOpMode,PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       na
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages SteamCoil component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE InputProcessor, ONLY: FindItemInList
   USE General,        ONLY: TrimSigDigits

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   CHARACTER(len=*), INTENT(IN) :: CompName
   LOGICAL         , INTENT(IN) :: FirstHVACIteration
   INTEGER         , INTENT(INOUT)  :: CompIndex
   REAL(r64),INTENT (IN), OPTIONAL :: QCoilReq       ! coil load to be met
   REAL(r64),OPTIONAL :: QCoilActual    ! coil load actually delivered returned to calling component
   INTEGER, OPTIONAL, INTENT(IN)  :: FanOpMode
   REAL(r64), OPTIONAL, INTENT(IN)  :: PartLoadRatio


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64)     :: QCoilActualTemp    ! coil load actually delivered returned to calling component
   INTEGER  :: CoilNum      ! The SteamCoil that you are currently loading input into
   INTEGER  :: OpMode       ! fan operating mode
   REAL(r64)     :: PartLoadFrac ! part-load fraction of heating coil


   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   END IF

   ! Find the correct SteamCoilNumber with the Coil Name
   IF (CompIndex == 0) THEN
     CoilNum = FindItemInList(CompName,SteamCoil%Name,NumSteamCoils)
     IF (CoilNum == 0) THEN
       CALL ShowFatalError('SimulateSteamCoilComponents: Coil not found='//TRIM(CompName))
     ENDIF
     CompIndex=CoilNum
   ELSE
     CoilNum=CompIndex
     IF (CoilNum > NumSteamCoils .or. CoilNum < 1) THEN
       CALL ShowFatalError('SimulateSteamCoilComponents: Invalid CompIndex passed='//TRIM(TrimSigDigits(CoilNum))// &
                ', Number of Steam Coils='//TRIM(TrimSigDigits(NumSteamCoils))//', Coil name='//TRIM(CompName))
     ENDIF
     IF (CheckEquipName(CoilNum)) THEN
       IF (CompName /= SteamCoil(CoilNum)%Name) THEN
         CALL ShowFatalError('SimulateSteamCoilComponents: Invalid CompIndex passed='//TRIM(TrimSigDigits(CoilNum))// &
                  ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//TRIM(SteamCoil(CoilNum)%Name))
       ENDIF
       CheckEquipName(CoilNum)=.false.
     ENDIF
   ENDIF

   ! With the correct CoilNum Initialize
   CALL InitSteamCoil(CoilNum,FirstHVACIteration) ! Initialize all SteamCoil related parameters

   IF(PRESENT(FanOpMode))THEN
     OpMode = FanOpMode
   ELSE
     OpMode = ContFanCycCoil
   END IF
   IF(PRESENT(PartLoadRatio))THEN
     PartLoadFrac = PartLoadRatio
   ELSE
     PartLoadFrac = 1.0d0
   END IF

   IF(SteamCoil(CoilNum)%SteamCoilType_Num == SteamCoil_AirHeating) THEN
     CALL CalcSteamAirCoil(CoilNum,QCoilReq,QCoilActualTemp,OpMode,PartLoadFrac) !Objexx:OPTIONAL QCoilReq used without PRESENT check
     IF(PRESENT(QCoilActual))QCoilActual=QcoilActualTemp
   END IF

   ! Update the current SteamCoil to the outlet nodes
   CALL UpdateSteamCoil(CoilNum)

   ! Report the current SteamCoil
   CALL ReportSteamCoil(CoilNum)

   RETURN

 END SUBROUTINE SimulateSteamCoilComponents



    ! Get Input Section of the Module
 SUBROUTINE GetSteamCoilInput
          ! SUBROUTINE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       na
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for coils and stores it in coil data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE InputProcessor
   USE NodeInputManager,   ONLY: GetOnlySingleNode
   USE BranchNodeConnections, ONLY: TestCompSet
   USE FluidProperties ,   ONLY: FindRefrigerant
   USE GlobalNames, ONLY: VerifyUniqueCoilName

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
   CHARACTER(len=*), PARAMETER :: RoutineName='GetSteamCoilInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   INTEGER :: CoilNum               ! The SteamCoil that you are currently loading input into
   INTEGER :: NumStmHeat            !
   INTEGER :: StmHeatNum            !
   INTEGER :: NumAlphas             !
   INTEGER :: NumNums               !
   INTEGER :: IOSTAT                !
   LOGICAL :: ErrorsFound = .false. ! If errors detected in input
   LOGICAL :: IsNotOK               ! Flag to verify name
   LOGICAL :: IsBlank               ! Flag for blank name
   CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
   CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
   CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
   CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
   REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
   LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
   LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
   INTEGER                              :: TotalArgs=0       ! Total number of alpha and numeric arguments (max) for a
                                                             !  certain object in the input file
   LOGICAL :: errflag

   CurrentModuleObject = 'Coil:Heating:Steam'
   NumStmHeat   = GetNumObjectsFound(CurrentModuleObject)
   NumSteamCoils = NumStmHeat
   IF (NumSteamCoils.GT.0) THEN
       ALLOCATE(SteamCoil(NumSteamCoils))
       ALLOCATE(CheckEquipName(NumSteamCoils))
       CheckEquipName=.true.
   ENDIF

   CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNums)
   ALLOCATE(AlphArray(NumAlphas))
   AlphArray=' '
   ALLOCATE(cAlphaFields(NumAlphas))
   cAlphaFields=' '
   ALLOCATE(cNumericFields(NumNums))
   cNumericFields=' '
   ALLOCATE(NumArray(NumNums))
   NumArray=0.0d0
   ALLOCATE(lAlphaBlanks(NumAlphas))
   lAlphaBlanks=.true.
   ALLOCATE(lNumericBlanks(NumNums))
   lNumericBlanks=.true.

  ! Get the data for steam heating coils
   DO StmHeatNum = 1,  NumStmHeat

      CoilNum= StmHeatNum

      CALL GetObjectItem(CurrentModuleObject,StmHeatNum,AlphArray, &
                         NumAlphas,NumArray,NumNums,IOSTAT, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(AlphArray(1),SteamCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF
      SteamCoil(CoilNum)%Name                         = AlphArray(1)
      SteamCoil(CoilNum)%Schedule                     = AlphArray(2)
      IF (lAlphaBlanks(2)) THEN
        SteamCoil(CoilNum)%SchedPtr                     = ScheduleAlwaysOn
      ELSE
        SteamCoil(CoilNum)%SchedPtr                     = GetScheduleIndex(AlphArray(2))
        IF (SteamCoil(CoilNum)%SchedPtr == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'", invalid data.')
          CALL ShowContinueError(TRIM(cAlphaFields(2))//' not found='//TRIM(AlphArray(2)))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      SteamCoil(CoilNum)%SteamCoilTypeA      ='Heating'
      SteamCoil(CoilNum)%SteamCoilType_Num   = SteamCoil_AirHeating
      SteamCoil(CoilNum)%Coil_PlantTypeNum   = TypeOf_CoilSteamAirHeating
      SteamCoil(CoilNum)%MaxSteamVolFlowRate = NumArray(1)
      SteamCoil(CoilNum)%DegOfSubCooling     = NumArray(2)
      SteamCoil(CoilNum)%LoopSubCoolReturn   = NumArray(3)

      SteamCoil(CoilNum)%SteamInletNodeNum    = &
          GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Steam, &
                               NodeConnectionType_Inlet,2,ObjectIsNotParent)
      SteamCoil(CoilNum)%SteamOutletNodeNum   = &
          GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Steam, &
                               NodeConnectionType_Outlet,2,ObjectIsNotParent)
      SteamCoil(CoilNum)%AirInletNodeNum      = &
          GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                               NodeConnectionType_Inlet,1,ObjectIsNotParent)
      SteamCoil(CoilNum)%AirOutletNodeNum     = &
          GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                               NodeConnectionType_Outlet,1,ObjectIsNotParent)


      SELECT CASE (MakeUPPERCase(AlphArray(7)))
       !TEMPERATURE SETPOINT CONTROL or ZONE LOAD CONTROLLED Coils
        CASE ('TEMPERATURESETPOINTCONTROL')
          SteamCoil(CoilNum)%TypeofCoil = TemperatureSetpointControl
          SteamCoil(CoilNum)%TempSetPointNodeNum  = &
              GetOnlySingleNode(AlphArray(8),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air,  &
                                 NodeConnectionType_Sensor,1,ObjectIsNotParent)
          IF (SteamCoil(CoilNum)%TempSetPointNodeNum==0) Then
            CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(8))//' not found for '// &
                                 TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
            CALL ShowContinueError('..required for Temperature Setpoint Controlled Coils.')
            ErrorsFound=.true.
          ENDIF

       CASE ('ZONELOADCONTROL')
         SteamCoil(CoilNum)%TypeofCoil = ZoneloadControl

         IF (.NOT. lAlphaBlanks(8)) THEN
           CALL ShowWarningError(RoutineName//'ZoneLoad Controlled Coil, so '//TRIM(cAlphaFields(8))//' not needed')
           CALL ShowContinueError('for '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
           SteamCoil(CoilNum)%TempSetPointNodeNum = 0
         ENDIF

       CASE DEFAULT
         CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(7))//' ['//TRIM(AlphArray(7))//'] specified for '// &
                              TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
         ErrorsFound=.true.
      END SELECT

      CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(3),AlphArray(4),'Steam Nodes')
      CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(5),AlphArray(6),'Air Nodes')

      IF (SteamIndex == 0 .and. CoilNum == 1) THEN
        SteamIndex=FindRefrigerant('Steam')
        IF (SteamIndex == 0) THEN
          CALL ShowSevereError(RoutineName//'Steam Properties for '//TRIM(AlphArray(1))// &
                               ' not found.')
          CALL ShowContinueError('Steam Fluid Properties should have been included in the input file.')
          ErrorsFound=.true.
        ENDIF
      ENDIF

      SteamCoil(CoilNum)%FluidIndex=SteamIndex
   END DO

   DO CoilNum=1,NumStmHeat

      !Setup the Simple Heating Coil reporting variables
      CALL SetupOutputVariable('Heating Coil Heating Energy [J]', SteamCoil(CoilNum)%TotSteamHeatingCoilEnergy, &
                   'System','Sum',SteamCoil(CoilNum)%Name, &
                   ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
      CALL SetupOutputVariable('Heating Coil Heating Rate [W]', SteamCoil(CoilNum)%TotSteamHeatingCoilRate, &
                  'System','Average',SteamCoil(CoilNum)%Name)
      CALL SetupOutputVariable('Heating Coil Steam Mass Flow Rate [Kg/s]', SteamCoil(CoilNum)%OutletSteamMassFlowRate, &
                   'System','Average',SteamCoil(CoilNum)%Name)
      CALL SetupOutputVariable('Heating Coil Steam Inlet Temperature [C]', SteamCoil(CoilNum)%InletSteamTemp, &
                   'System','Average',SteamCoil(CoilNum)%Name)
      CALL SetupOutputVariable('Heating Coil Steam Outlet Temperature [C]', SteamCoil(CoilNum)%OutletSteamTemp, &
                   'System','Average',SteamCoil(CoilNum)%Name)
      CALL SetupOutputVariable('Heating Coil Steam Trap Loss Rate [W]', SteamCoil(CoilNum)%LoopLoss, &
                  'System','Average',SteamCoil(CoilNum)%Name)

   END DO

   IF (ErrorsFound) THEN
     CALL ShowFatalError(RoutineName//'Errors found in getting input.')
   ENDIF

   DEALLOCATE(AlphArray)
   DEALLOCATE(cAlphaFields)
   DEALLOCATE(cNumericFields)
   DEALLOCATE(NumArray)
   DEALLOCATE(lAlphaBlanks)
   DEALLOCATE(lNumericBlanks)

   RETURN

 END SUBROUTINE GetSteamCoilInput
      ! End of Get Input subroutines for the HB Module


      ! Beginning Initialization Section of the Module
 SUBROUTINE InitSteamCoil(CoilNum,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       na
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the SteamCoil Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE FluidProperties, ONLY:GetSatDensityRefrig,GetSatEnthalpyRefrig
   USE PlantUtilities, ONLY:InitComponentNodes

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN) :: CoilNum
   LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   INTEGER             :: AirInletNode
   INTEGER             :: SteamInletNode
   INTEGER             :: ControlNode
   INTEGER             :: AirOutletNode
   REAL(r64)           :: SteamDensity
   REAL(r64)           :: StartEnthSteam
   LOGICAL,SAVE        :: MyOneTimeFlag = .true.
   LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
   LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyPlantScanFlag
   LOGICAL             :: errFlag

   IF (MyOneTimeFlag) THEN
     ! initialize the environment and sizing flags
     ALLOCATE(MyEnvrnFlag(NumSteamCoils))
     ALLOCATE(MySizeFlag(NumSteamCoils))
     ALLOCATE(CoilWarningOnceFlag(NumSteamCoils))
     ALLOCATE(MyPlantScanFlag(NumSteamCoils))
     MyEnvrnFlag = .TRUE.
     MySizeFlag  = .TRUE.
     CoilWarningOnceFlag = .TRUE.
     MyPlantScanFlag = .TRUE.
     MyOneTimeFlag = .FALSE.
   END IF

   IF (MyPlantScanFlag(CoilNum) .AND. ALLOCATED(PlantLoop)) THEN
     errFlag=.false.
     CALL ScanPlantLoopsForObject( SteamCoil(CoilNum)%Name, &
                                   SteamCoil(CoilNum)%Coil_PlantTypeNum, &
                                   SteamCoil(CoilNum)%LoopNum,   &
                                   SteamCoil(CoilNum)%LoopSide,  &
                                   SteamCoil(CoilNum)%BranchNum, &
                                   SteamCoil(CoilNum)%CompNum,          &
                                   errFlag=errFlag)
     IF (errFlag) THEN
       CALL ShowFatalError('InitSteamCoil: Program terminated for previous conditions.')
     ENDIF
     MyPlantScanFlag(CoilNum) = .FALSE.
   ENDIF

   IF ( .NOT. SysSizingCalc .AND. MySizeFlag(CoilNum)) THEN
     ! for each coil, do the sizing once.
     CALL SizeSteamCoil(CoilNum)
     MySizeFlag(CoilNum) = .FALSE.
   END IF

   ! Do the Begin Environment initializations
   IF (BeginEnvrnFlag .and. MyEnvrnFlag(CoilNum)) THEN
     !Initialize all report variables to a known state at beginning of simulation
     SteamCoil(CoilNum)%TotSteamHeatingCoilEnergy = 0.0d0
     SteamCoil(CoilNum)%TotSteamCoolingCoilEnergy = 0.0d0
     SteamCoil(CoilNum)%SenSteamCoolingCoilEnergy = 0.0d0
     SteamCoil(CoilNum)%TotSteamHeatingCoilRate   = 0.0d0
     SteamCoil(CoilNum)%TotSteamCoolingCoilRate   = 0.0d0
     SteamCoil(CoilNum)%SenSteamCoolingCoilRate   = 0.0d0
     ! Initialize other module level variables
     SteamCoil(CoilNum)%InletAirMassFlowRate = 0.0d0
     SteamCoil(CoilNum)%OutletAirMassFlowRate = 0.0d0
     SteamCoil(CoilNum)%InletAirTemp = 0.0d0
     SteamCoil(CoilNum)%OutletAirTemp = 0.0d0
     SteamCoil(CoilNum)%InletAirHumRat = 0.0d0
     SteamCoil(CoilNum)%OutletAirHumRat = 0.0d0
     SteamCoil(CoilNum)%InletAirEnthalpy = 0.0d0
     SteamCoil(CoilNum)%OutletAirEnthalpy = 0.0d0
     SteamCoil(CoilNum)%TotSteamCoilLoad = 0.0d0
     SteamCoil(CoilNum)%SenSteamCoilLoad = 0.0d0
     SteamCoil(CoilNum)%LoopLoss = 0.0d0
     SteamCoil(CoilNum)%LeavingRelHum = 0.0d0
     SteamCoil(CoilNum)%DesiredOutletTemp = 0.0d0
     SteamCoil(CoilNum)%DesiredOutletHumRat = 0.0d0
     SteamCoil(CoilNum)%InletSteamTemp = 0.0d0
     SteamCoil(CoilNum)%OutletSteamTemp = 0.0d0
     SteamCoil(CoilNum)%InletSteamMassFlowRate = 0.0d0
     SteamCoil(CoilNum)%OutletSteamMassFlowRate = 0.0d0
     SteamCoil(CoilNum)%InletSteamEnthalpy = 0.0d0
     SteamCoil(CoilNum)%OutletWaterEnthalpy = 0.0d0
     SteamCoil(CoilNum)%InletSteamPress = 0.0d0
     SteamCoil(CoilNum)%InletSteamQuality = 0.0d0
     SteamCoil(CoilNum)%OutletSteamQuality = 0.0d0

     ! More Environment initializations
     AirInletNode                              = SteamCoil(CoilNum)%AirInletNodeNum
     SteamInletNode                            = SteamCoil(CoilNum)%SteamInletNodeNum
     ControlNode                               = SteamCoil(CoilNum)%TempSetPointNodeNum
     AirOutletNode                             = SteamCoil(CoilNum)%AirOutletNodeNum

     Node(SteamInletNode)%Temp                 = 100.0d0
     Node(SteamInletNode)%Press                = 101325.d0
     SteamDensity   =GetSatDensityRefrig ('STEAM',Node(SteamInletNode)%Temp,1.0d0,Node(SteamInletNode)%FluidIndex,'InitSteamCoil')
     StartEnthSteam =GetSatEnthalpyRefrig('STEAM',Node(SteamInletNode)%Temp,1.0d0,Node(SteamInletNode)%FluidIndex,'InitSteamCoil')
     Node(SteamInletNode)%Enthalpy             = StartEnthSteam
     Node(SteamInletNode)%Quality              = 1.0d0
     Node(SteamInletNode)%HumRat               = 0.0d0
     SteamCoil(CoilNum)%MaxSteamMassFlowRate   = SteamDensity*SteamCoil(CoilNum)%MaxSteamVolFlowRate
!     Node(SteamInletNode)%MassFlowRate         = SteamCoil(CoilNum)%MaxSteamMassFlowRate
!     Node(SteamInletNode)%MassFlowRateMinAvail = 0.0
!     Node(SteamInletNode)%MassFlowRateMaxAvail = SteamCoil(CoilNum)%MaxSteamMassFlowRate
     CALL InitComponentNodes(0.d0,SteamCoil(CoilNum)%MaxSteamMassFlowRate, &
                                  SteamCoil(CoilNum)%SteamInletNodeNum,    &
                                  SteamCoil(CoilNum)%SteamOutletNodeNum,   &
                                  SteamCoil(CoilNum)%LoopNum,              &
                                  SteamCoil(CoilNum)%LoopSide,             &
                                  SteamCoil(CoilNum)%BranchNum,            &
                                  SteamCoil(CoilNum)%CompNum )
     MyEnvrnFlag(CoilNum) = .FALSE.
   END IF  ! End If for the Begin Environment initializations

   IF (.not. BeginEnvrnFlag) THEN
     MyEnvrnFlag(CoilNum)=.true.
   ENDIF

   ! Do the Begin Day initializations
   ! NONE

   ! Do the begin HVAC time step initializations
   ! NONE

   ! Do the following initializations (every time step): This should be the info from
   ! the previous components outlets or the node data in this section.

   AirInletNode                              = SteamCoil(CoilNum)%AirInletNodeNum
   SteamInletNode                            = SteamCoil(CoilNum)%SteamInletNodeNum
   ControlNode                               = SteamCoil(CoilNum)%TempSetPointNodeNum
   AirOutletNode                             = SteamCoil(CoilNum)%AirOutletNodeNum

   ! First set the conditions for the air into the coil model

   ! If a temperature setpoint controlled coil must set the desired outlet temp everytime
   IF (ControlNode.EQ.0) THEN
       SteamCoil(CoilNum)%DesiredOutletTemp = 0.0d0
   ELSE IF (ControlNode.EQ.AirOutletNode) THEN
       SteamCoil(CoilNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint
   ELSE
       SteamCoil(CoilNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint - &
                                                   (Node(ControlNode)%Temp - Node(AirOutletNode)%Temp)
   END IF

   SteamCoil(CoilNum)%InletAirMassFlowRate    = Node(AirInletNode)%MassFlowRate
   SteamCoil(CoilNum)%InletAirTemp            = Node(AirInletNode)%Temp
   SteamCoil(CoilNum)%InletAirHumRat          = Node(AirInletNode)%HumRat
   SteamCoil(CoilNum)%InletAirEnthalpy        = Node(AirInletNode)%Enthalpy
   If(FirstHVACIteration) Then
     SteamCoil(CoilNum)%InletSteamMassFlowRate = SteamCoil(CoilNum)%MaxSteamMassFlowRate
   Else
     SteamCoil(CoilNum)%InletSteamMassFlowRate = Node(SteamInletNode)%MassFlowRate
   End If
   SteamCoil(CoilNum)%InletSteamTemp          = Node(SteamInletNode)%Temp
   SteamCoil(CoilNum)%InletSteamEnthalpy      = Node(SteamInletNode)%Enthalpy
   SteamCoil(CoilNum)%InletSteamPress         = Node(SteamInletNode)%Press
   SteamCoil(CoilNum)%InletSteamQuality       = Node(SteamInletNode)%Quality
   SteamCoil(CoilNum)%TotSteamHeatingCoilRate = 0.0d0
   SteamCoil(CoilNum)%TotSteamCoolingCoilRate = 0.0d0
   SteamCoil(CoilNum)%SenSteamCoolingCoilRate = 0.0d0
!   Node(SteamInletNode)%MassFlowRateMaxAvail = MIN(Node(SteamInletNode)%MassFlowRateMaxAvail,&
!                                                   SteamCoil(CoilNum)%MaxSteamMassFlowRate)

   RETURN
 END SUBROUTINE InitSteamCoil

 SUBROUTINE SizeSteamCoil(CoilNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rahul Chillar
          !       DATE WRITTEN   Jan 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Steam Coil Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataSizing
   USE PlantUtilities,   ONLY: RegisterPlantCompDesignFlow
   USE FluidProperties,  ONLY: GetSatEnthalpyRefrig,GetSatDensityRefrig
 !  USE BranchInputManager, ONLY: MyPlantSizingIndex
   USE ReportSizingManager, ONLY: ReportSizingOutput

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   INTEGER             :: PltSizNum     ! do loop index for plant sizing
   INTEGER             :: PltSizSteamNum ! index of plant sizing object for 1st steam loop
   LOGICAL             :: ErrorsFound   ! If errors detected in input
   REAL(r64)           :: CoilInTemp    !
   REAL(r64)           :: CoilOutTemp   !
   REAL(r64)           :: CoilOutHumRat !
   REAL(r64)           :: CoilInHumRat  !
   REAL(r64)           :: DesCoilLoad   !
   REAL(r64)           :: DesMassFlow   !
   REAL(r64)           :: DesVolFlow    !
   REAL(r64)           :: MinFlowFrac   !
   REAL(r64)           :: OutAirFrac    !
   REAL(r64)           :: TempSteamIn         !
   REAL(r64)           :: EnthSteamInDry      !
   REAL(r64)           :: EnthSteamOutWet     !
   REAL(r64)           :: LatentHeatSteam     !
   REAL(r64)           :: SteamDensity        !
   REAL(r64)           :: RhoAirStd           ! density of air at standard conditions
   REAL(r64)           :: CpAirStd            ! specific heat of air at std conditions
   REAL(r64)           :: CpWater             ! specific heat of water (condensed steam)


   ErrorsFound   = .FALSE.
   PltSizSteamNum = 0
   PltSizNum     = 0
   CoilInTemp    = 0.0d0
   CoilInHumRat  = 0.0d0
   CoilOutTemp   = 0.0d0
   DesCoilLoad   = 0.0d0
   MinFlowFrac   = 0.0d0
   DesMassFlow   = 0.0d0
   CpWater       = 0.0d0
   RhoAirStd     = PsyRhoAirFnPbTdbW(StdBaroPress,20.0d0,0.0d0)
   CpAirStd      = PsyCpAirFnWTdb(0.0d0,20.0d0)

   ! If this is a steam coil
   ! Find the appropriate steam Plant Sizing object
   IF (SteamCoil(CoilNum)%MaxSteamVolFlowRate == AutoSize) THEN
     PltSizSteamNum = MyPlantSizingIndex("steam heating coil", SteamCoil(CoilNum)%Name, SteamCoil(CoilNum)%SteamInletNodeNum, &
                                       SteamCoil(CoilNum)%SteamOutletNodeNum, ErrorsFound)
   ENDIF

   IF (PltSizSteamNum > 0) THEN
     ! If this is a central air system heating coil
     IF (CurSysNum > 0) THEN
       ! If the coil water volume flow rate needs autosizing, then do it
       IF (SteamCoil(CoilNum)%MaxSteamVolFlowRate == AutoSize) THEN
         CALL CheckSysSizing('Coil:Heating:Steam',SteamCoil(CoilNum)%Name)
         ! Set the duct flow rate
         SELECT CASE(CurDuctType)
           CASE(Main)
             DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesMainVolFlow
           CASE(Cooling)
             DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesCoolVolFlow
           CASE(Heating)
             DesVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
           CASE(Other)
             DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
           CASE DEFAULT
             DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
         END SELECT
         DesMassFlow = RhoAirStd*DesVolFlow
         ! get the outside air fraction
         IF (FinalSysSizing(CurSysNum)%HeatOAOption == MinOA) THEN
           IF (DesVolFlow > 0.0d0) THEN
             OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
           ELSE
             OutAirFrac = 1.0d0
           END IF
           OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
         ELSE
           OutAirFrac = 1.0d0
         END IF
         ! mixed air temp
         CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%HeatOutTemp + &
                        (1.0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
         ! coil load
         DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%HeatSupTemp - CoilInTemp)
         !AUTOSTEAMCOIL
         IF (DesCoilLoad >= SmallLoad) THEN
            !TempSteamIn=SteamCoil(CoilNum)%InletSteamTemp
            !TempSteamIn=PlantSizData(PltSizSteamNum)%ExitTemp
            TempSteamIn    =  100.0d0 ! DSU? Should be from the PlantSizing object (ExitTemp) instead of hardwired to 100?
            ! RefrigIndex is set during GetInput for this module
            EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')
            EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')
            LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
            SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')
            ! SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad/(SteamDensity * LatentHeatSteam)
!            CpWater  =  GetSpecificHeatGlycol('WATER',  &
!                                              TempSteamIn, &
!                                              PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
!                                             'SizeSteamCoil')
            CpWater = GetSatSpecificHeatRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')

            SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad / (SteamDensity*(LatentHeatSteam + &
                                                       SteamCoil(CoilNum)%DegOfSubCooling*CpWater))
!             PlantSizData(PltSizSteamNum)%DeltaT*CPHW(PlantSizData(PltSizSteamNum)%ExitTemp)))
         ELSE
           SteamCoil(CoilNum)%MaxSteamVolFlowRate = 0.0d0
           CALL ShowWarningError('The design coil load is zero for COIL:Heating:Steam ' &
                                 //TRIM(SteamCoil(CoilNum)%Name))
           !CALL ShowContinueError('The autosize value for max Steam flow rate is zero')
           !CALL ShowContinueError('To change this, input a value for UA, change the heating design day, or lower')
           !CALL ShowContinueError('  the system heating design supply air temperature')
         END IF
         CALL ReportSizingOutput('Coil:Heating:Steam',SteamCoil(CoilNum)%Name,&
                                 'Maximum Steam Flow Rate [m3/s]',SteamCoil(CoilNum)%MaxSteamVolFlowRate)
       END IF
     ! if this is a zone coil
     ELSE IF (CurZoneEqNum > 0) THEN
       CALL CheckZoneSizing('Coil:Heating:Steam',SteamCoil(CoilNum)%Name)
       ! autosize the coil steam volume flow rate if needed
       IF (SteamCoil(CoilNum)%MaxSteamVolFlowRate == AutoSize) THEN
         ! if coil is part of a terminal unit just use the terminal unit value
         IF (TermUnitSingDuct .OR. TermUnitPIU .OR. TermUnitIU) THEN
           SteamCoil(CoilNum)%MaxSteamVolFlowRate = TermUnitSizing(CurZoneEqNum)%MaxSTVolFlow
         ! if coil is part of a zonal unit, calc coil load to get hot Steam flow rate
         ELSE
           CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
           CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
           CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
           DesMassFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow
           DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                         * DesMassFlow * (CoilOutTemp-CoilInTemp)
           IF (DesCoilLoad >= SmallLoad) THEN
            TempSteamIn=100.0d0 ! DSU? Should be from the PlantSizing object (ExitTemp) instead of hardwired to 100?
            ! RefrigIndex is set during GetInput for this module
            EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')
            EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')
            LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
            SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')
            ! SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad/(SteamDensity * LatentHeatSteam)
 !           CpWater  =  GetSpecificHeatGlycol('WATER',  &
 !                                             TempSteamIn, &
 !                                             PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
 !                                            'SizeSteamCoil')
            CpWater = GetSatSpecificHeatRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')

            SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad / (SteamDensity*(LatentHeatSteam + &
                                                       SteamCoil(CoilNum)%DegOfSubCooling*CpWater))
!             PlantSizData(PltSizSteamNum)%DeltaT*CPHW(PlantSizData(PltSizSteamNum)%ExitTemp)))
           ELSE
           SteamCoil(CoilNum)%MaxSteamVolFlowRate = 0.0d0
           END IF
         END IF
         ! issue warning if hw coil has zero flow
         IF (SteamCoil(CoilNum)%MaxSteamVolFlowRate == 0.0d0) THEN
           CALL ShowWarningError('The design coil load is zero for COIL:Heating:Steam ' &
                                 //TRIM(SteamCoil(CoilNum)%Name))
           CALL ShowContinueError('The autosize value for max Steam flow rate is zero')
           !CALL ShowContinueError('To change this, input a value for UA, change the heating design day, or lower')
           !CALL ShowContinueError('  the system heating design supply air temperature')
         END IF
         CALL ReportSizingOutput('Coil:Heating:Steam',SteamCoil(CoilNum)%Name,&
                                 'Maximum Steam Flow Rate [m3/s]',SteamCoil(CoilNum)%MaxSteamVolFlowRate)
       END IF
     END IF ! end zone coil ELSE - IF

   ELSE
     ! if there is no heating Plant Sizing object and autosizng was requested, issue an error message
     IF (SteamCoil(CoilNum)%MaxSteamVolFlowRate == AutoSize) THEN
       CALL ShowSevereError('Autosizing of Steam coil requires a heating loop Sizing:Plant object')
       CALL ShowContinueError('Occurs in Steam coil object= '//TRIM(SteamCoil(CoilNum)%Name))
       ErrorsFound = .TRUE.
     END IF
   END IF ! end of heating Plant Sizing existence IF - ELSE

   ! save the design Steam volumetric flow rate for use by the Steam loop sizing algorithms
   CALL RegisterPlantCompDesignFlow(SteamCoil(CoilNum)%SteamInletNodeNum,SteamCoil(CoilNum)%MaxSteamVolFlowRate)

   IF (ErrorsFound) THEN
      CALL ShowFatalError('Preceding Steam coil sizing errors cause program termination')
   END IF

   RETURN

 END SUBROUTINE SizeSteamCoil
      ! End Initialization Section of the Module

      ! Begin Algorithm Section of the Module
 Subroutine CalcSteamAirCoil(CoilNum,QCoilRequested,QCoilActual,FanOpMode,PartLoadRatio)
          ! SUBROUTINE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       Sept. 2012, B. Griffith, add calls to SetComponentFlowRate for plant interactions
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simple Steam to air heat exchanger which,
          ! serves as an interface for distributing heat from boiler to zones.

          ! METHODOLOGY EMPLOYED:
          ! Steam coils are different, All of steam condenses in heat exchanger
          ! Steam traps allow only water to leave the coil,the degree of subcooling
          ! desired is input by the user, which is used to calculate water outlet temp.
          ! Heat exchange is = Latent Heat + Sensible heat,coil effectivness is 1.0

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataHVACGlobals, ONLY: TempControlTol
   USE PlantUtilities,  ONLY: SetComponentFlowRate

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: CoilNum
   REAL(r64),    Intent(IN) :: QCoilRequested ! requested coil load
   REAL(r64),    Intent(OUT):: QCoilActual    ! coil load actually delivered
   INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
   REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) :: SteamMassFlowRate =0.0d0
   REAL(r64) :: AirMassFlow       =0.0d0! [kg/sec]
   REAL(r64) :: TempAirIn         =0.0d0! [C]
   REAL(r64) :: TempAirOut        =0.0d0! [C]
   REAL(r64) :: Win               =0.0d0
   REAL(r64) :: TempSteamIn       =0.0d0
   REAL(r64) :: TempWaterOut      =0.0d0
   REAL(r64) :: CapacitanceAir    =0.0d0
   REAL(r64) :: HeatingCoilLoad   =0.0d0
   REAL(r64) :: CoilPress         =0.0d0
   REAL(r64) :: EnthSteamInDry    =0.0d0
   REAL(r64) :: EnthSteamOutWet   =0.0d0
   REAL(r64) :: LatentHeatSteam   =0.0d0
   REAL(r64) :: SubCoolDeltaTemp  =0.0d0
   REAL(r64) :: TempSetPoint      =0.0d0
   REAL(r64) :: QCoilReq          =0.0d0
   REAL(r64) :: QCoilCap          =0.0d0
   REAL(r64) :: QSteamCoilMaxHT   =0.0d0
   REAL(r64) :: TempWaterAtmPress =0.0d0
   REAL(r64) :: TempLoopOutToPump =0.0d0
   REAL(r64) :: EnergyLossToEnvironment=0.0d0
   REAL(r64) :: EnthCoilOutlet    =0.0d0
   REAL(r64) :: EnthPumpInlet     =0.0d0
   REAL(r64) :: EnthAtAtmPress    =0.0d0
   REAL(r64) :: CpWater           =0.0d0

   QCoilReq          = QCoilRequested
   TempAirIn         = SteamCoil(CoilNum)%InletAirTemp
   Win               = SteamCoil(CoilNum)%InletAirHumRat
   TempSteamIn       = SteamCoil(CoilNum)%InletSteamTemp
   CoilPress         = SteamCoil(CoilNum)%InletSteamPress
   SubCoolDeltaTemp  = SteamCoil(CoilNum)%DegOfSubCooling
   TempSetPoint      = SteamCoil(CoilNum)%DesiredOutletTemp

!  adjust mass flow rates for cycling fan cycling coil operation
   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     IF(PartLoadRatio .GT. 0.0d0)THEN
       AirMassFlow       = SteamCoil(CoilNum)%InletAirMassFlowRate / PartLoadRatio
       SteamMassFlowRate = MIN(SteamCoil(CoilNum)%InletSteamMassFlowRate / PartLoadRatio, &
                               SteamCoil(CoilNum)%MaxSteamMassFlowRate)
       QCoilReq          = QCoilReq / PartLoadRatio
     ELSE
       AirMassFlow = 0.0d0
       SteamMassFlowRate = 0.0d0
     END IF
   ELSE
     AirMassFlow         = SteamCoil(CoilNum)%InletAirMassFlowRate
     SteamMassFlowRate   = SteamCoil(CoilNum)%InletSteamMassFlowRate
   END IF

   IF (AirMassFlow .GT. 0.0d0) THEN     ! If the coil is operating
     CapacitanceAir=PsyCpAirFnWTdb(Win,TempAirIn)*AirMassFlow
   Else
     CapacitanceAir=0.0d0
   End If

     ! If the coil is operating there should be some heating capacitance
     !  across the coil, so do the simulation. If not set outlet to inlet and no load.
     !  Also the coil has to be scheduled to be available
     !  Control output to meet load QCoilReq. Load Controlled Coil.
   Select Case(SteamCoil(CoilNum)%TypeofCoil )

     Case(ZoneloadControl)
       IF((CapacitanceAir .GT. 0.0d0) .AND.((SteamCoil(CoilNum)%InletSteamMassFlowRate).GT.0.0d0).AND.  &
           (GetCurrentScheduleValue(SteamCoil(CoilNum)%SchedPtr).GT. 0.0d0 .OR. MySizeFlag(CoilNum)).and. &
           (QCoilReq .gt. 0.0d0)) THEN

          ! Steam heat exchangers would not have effectivness, since all of the steam is
          ! converted to water and only then the steam trap allows it to leave the heat
          ! exchanger, subsequently heat exchange is latent heat + subcooling.
          EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')
          EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')

          LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet

!          CpWater = GetSpecificHeatGlycol('WATER',  &
!                                           TempSteamIn, &
!                                           PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
!                                           'CalcSteamAirCoil')

          CpWater = GetSatSpecificHeatRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')

          ! Max Heat Transfer
          QSteamCoilMaxHT= SteamCoil(CoilNum)%MaxSteamMassFlowRate*(LatentHeatSteam+SubCoolDeltaTemp*CpWater)
          SteamCoil(CoilNum)%OperatingCapacity = QSteamCoilMaxHT

          ! Determine the Max coil capacity and check for the same.
          IF(QCoilReq > QSteamCoilMaxHT) Then
              QCoilCap = QSteamCoilMaxHT
          Else
              QCoilCap = QCoilReq
          End IF

          ! Steam Mass Flow Rate Required
          SteamMassFlowRate=QCoilCap/(LatentHeatSteam+SubCoolDeltaTemp*CpWater)

          CALL SetComponentFlowRate( SteamMassFlowRate, &
                                   SteamCoil(CoilNum)%SteamInletNodeNum, &
                                   SteamCoil(CoilNum)%SteamOutletNodeNum, &
                                   SteamCoil(CoilNum)%LoopNum, &
                                   SteamCoil(CoilNum)%LoopSide, &
                                   SteamCoil(CoilNum)%BranchNum, &
                                   SteamCoil(CoilNum)%CompNum )

          ! recalculate if mass flow rate changed in previous call.
          QCoilCap = SteamMassFlowRate*(LatentHeatSteam+SubCoolDeltaTemp*CpWater)

          ! In practice Sensible & Superheated heat transfer is negligible compared to latent part.
          ! This is required for outlet water temperature, otherwise it will be saturation temperature.
          ! Steam Trap drains off all the Water formed.
          ! Here Degree of Subcooling is used to calculate hot water return temperature.

          ! Calculating Water outlet temperature
          TempWaterOut=TempSteamIn-SubCoolDeltaTemp

          ! Total Heat Transfer to air
          HeatingCoilLoad =QCoilCap

          ! Temperature of air at outlet
          TempAirOut=TempAirIn+QCoilCap/(AirMassFlow*PsyCpAirFnWTdb(Win,TempAirIn))

          SteamCoil(CoilNum)%OutletSteamMassFlowRate = SteamMassFlowRate
          SteamCoil(CoilNum)%InletSteamMassFlowRate  = SteamMassFlowRate

          !************************* Loop Losses *****************************
          ! Loop pressure return considerations included in steam coil since the pipes are
          ! perfect and do not account for losses.
          ! Return water is condensate at atmoshperic pressure
          ! Process is considered constant enthalpy expansion
          ! No quality function in EnergyPlus hence no option left apart from
          ! considering saturated state.
!              StdBaroPress=101325

              TempWaterAtmPress=GetSatTemperatureRefrig('Steam',StdBaroPress,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')

              ! Point 4 at atm - loop delta subcool during return journery back to pump
              TempLoopOutToPump=TempWaterAtmPress-SteamCoil(CoilNum)%LoopSubCoolReturn

              ! Actual Steam Coil Outlet Enthalpy
              EnthCoilOutlet=GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,  &
                 SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil') - CpWater*SubCoolDeltaTemp

              ! Enthalpy at Point 4
              EnthAtAtmPress=GetSatEnthalpyRefrig('STEAM',TempWaterAtmPress,0.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')

              ! Reported value of coil outlet enthalpy at the node to match the node outlet temperature
              CpWater = GetSatSpecificHeatRefrig('STEAM',TempLoopOutToPump,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')

              EnthPumpInlet=EnthAtAtmPress-CpWater*SteamCoil(CoilNum)%LoopSubCoolReturn

              SteamCoil(CoilNum)%OutletWaterEnthalpy =EnthPumpInlet

              ! Point 3-Point 5,
              EnergyLossToEnvironment=SteamMassFlowRate*(EnthCoilOutlet-EnthPumpInlet)

              ! Loss to enviornment due to pressure drop
              SteamCoil(CoilNum)%LoopLoss=EnergyLossToEnvironment
          !************************* Loop Losses *****************************
       ELSE ! Coil is not running.

          TempAirOut                                 = TempAirIn
          TempWaterOut                               = TempSteamIn
          HeatingCoilLoad                            = 0.0d0
          SteamCoil(CoilNum)%OutletWaterEnthalpy     = SteamCoil(CoilNum)%InletSteamEnthalpy
          SteamCoil(CoilNum)%OutletSteamMassFlowRate = 0.0d0
          SteamCoil(CoilNum)%OutletSteamQuality      = 0.0d0
          SteamCoil(CoilNum)%LoopLoss                = 0.0d0
          TempLoopOutToPump                          = TempWaterOut
       END IF


     Case(TemperatureSetPointControl)
       ! Control coil output to meet a Setpoint Temperature.
       IF((CapacitanceAir .GT. 0.0d0) .AND.((SteamCoil(CoilNum)%InletSteamMassFlowRate).GT.0.0d0).AND.   &
           (GetCurrentScheduleValue(SteamCoil(CoilNum)%SchedPtr).GT. 0.0d0 .OR. MySizeFlag(CoilNum)).and. &
           (ABS(TempSetPoint-TempAirIn) .gt. TempControlTol) ) THEN

          ! Steam heat exchangers would not have effectivness, since all of the steam is
          ! converted to water and only then the steam trap allows it to leave the heat
          ! exchanger, subsequently heat exchange is latent heat + subcooling.
          EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')
          EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')
          LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet

!          CpWater = GetSpecificHeatGlycol('WATER',  &
!                                           TempSteamIn, &
!                                           PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
!                                           'CalcSteamAirCoil')
          CpWater = GetSatSpecificHeatRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')

          ! Max Heat Transfer
          QSteamCoilMaxHT= SteamCoil(CoilNum)%MaxSteamMassFlowRate*(LatentHeatSteam+SubCoolDeltaTemp*CpWater)

          ! Coil Load in case of temperature setpoint
          QCoilCap=CapacitanceAir*(TempSetPoint-TempAirIn)

          ! Check to see if setpoint above enetering temperature. If not, set
          ! output to zero.
          IF(QCoilCap .LE. 0.0d0) THEN
              QCoilCap = 0.0d0
              TempAirOut = TempAirIn

              ! Steam Mass Flow Rate Required
              SteamMassFlowRate=0.d0
              CALL SetComponentFlowRate( SteamMassFlowRate, &
                                       SteamCoil(CoilNum)%SteamInletNodeNum, &
                                       SteamCoil(CoilNum)%SteamOutletNodeNum, &
                                       SteamCoil(CoilNum)%LoopNum, &
                                       SteamCoil(CoilNum)%LoopSide, &
                                       SteamCoil(CoilNum)%BranchNum, &
                                       SteamCoil(CoilNum)%CompNum )
              ! Inlet equal to outlet when not required to run.
              TempWaterOut=TempSteamIn

              ! Total Heat Transfer to air
              HeatingCoilLoad = QCoilCap

              !The HeatingCoilLoad is the change in the enthalpy of the water
              SteamCoil(CoilNum)%OutletWaterEnthalpy = SteamCoil(CoilNum)%InletSteamEnthalpy

              ! Outlet flow rate set to inlet
              SteamCoil(CoilNum)%OutletSteamMassFlowRate = SteamMassFlowRate
              SteamCoil(CoilNum)%InletSteamMassFlowRate  = SteamMassFlowRate


          ELSEIF (QCoilCap .GT. QSteamCoilMaxHT) Then
              ! Setting to Maximum Coil Capacity
              QCoilCap = QSteamCoilMaxHT

              ! Temperature of air at outlet
              TempAirOut=TempAirIn+QCoilCap/(AirMassFlow*PsyCpAirFnWTdb(Win,TempAirIn))

              ! In practice Sensible & Superheated heat transfer is negligible compared to latent part.
              ! This is required for outlet water temperature, otherwise it will be saturation temperature.
              ! Steam Trap drains off all the Water formed.
              ! Here Degree of Subcooling is used to calculate hot water return temperature.

              ! Calculating Water outlet temperature
              TempWaterOut=TempSteamIn-SubCoolDeltaTemp

              ! Steam Mass Flow Rate Required
              SteamMassFlowRate=QCoilCap/(LatentHeatSteam+SubCoolDeltaTemp*CpWater)
              CALL SetComponentFlowRate( SteamMassFlowRate, &
                                       SteamCoil(CoilNum)%SteamInletNodeNum, &
                                       SteamCoil(CoilNum)%SteamOutletNodeNum, &
                                       SteamCoil(CoilNum)%LoopNum, &
                                       SteamCoil(CoilNum)%LoopSide, &
                                       SteamCoil(CoilNum)%BranchNum, &
                                       SteamCoil(CoilNum)%CompNum )

              ! recalculate in case previous call changed mass flow rate
              QCoilCap = SteamMassFlowRate*(LatentHeatSteam+SubCoolDeltaTemp*CpWater)
              TempAirOut=TempAirIn+QCoilCap/(AirMassFlow*PsyCpAirFnWTdb(Win,TempAirIn))

              ! Total Heat Transfer to air
              HeatingCoilLoad = QCoilCap

              !The HeatingCoilLoad is the change in the enthalpy of the water
              SteamCoil(CoilNum)%OutletWaterEnthalpy = SteamCoil(CoilNum)%InletSteamEnthalpy- &
                                                  HeatingCoilLoad/SteamMassFlowRate
              SteamCoil(CoilNum)%OutletSteamMassFlowRate = SteamMassFlowRate
              SteamCoil(CoilNum)%InletSteamMassFlowRate  = SteamMassFlowRate

          ELSE
              ! Temp air out is temperature Setpoint
              TempAirOut=TempSetPoint

              ! In practice Sensible & Superheated heat transfer is negligible compared to latent part.
              ! This is required for outlet water temperature, otherwise it will be saturation temperature.
              ! Steam Trap drains off all the Water formed.
              ! Here Degree of Subcooling is used to calculate hot water return temperature.

              ! Calculating Water outlet temperature
              TempWaterOut=TempSteamIn-SubCoolDeltaTemp

              ! Steam Mass Flow Rate Required
              SteamMassFlowRate=QCoilCap/(LatentHeatSteam+SubCoolDeltaTemp*CpWater)
              CALL SetComponentFlowRate( SteamMassFlowRate, &
                                       SteamCoil(CoilNum)%SteamInletNodeNum, &
                                       SteamCoil(CoilNum)%SteamOutletNodeNum, &
                                       SteamCoil(CoilNum)%LoopNum, &
                                       SteamCoil(CoilNum)%LoopSide, &
                                       SteamCoil(CoilNum)%BranchNum, &
                                       SteamCoil(CoilNum)%CompNum )

              ! recalculate in case previous call changed mass flow rate
              QCoilCap = SteamMassFlowRate*(LatentHeatSteam+SubCoolDeltaTemp*CpWater)
              TempAirOut=TempAirIn+QCoilCap/(AirMassFlow*PsyCpAirFnWTdb(Win,TempAirIn))



              ! Total Heat Transfer to air
              HeatingCoilLoad = QCoilCap

              SteamCoil(CoilNum)%OutletSteamMassFlowRate = SteamMassFlowRate
              SteamCoil(CoilNum)%InletSteamMassFlowRate  = SteamMassFlowRate

              !************************* Loop Losses *****************************
              ! Loop pressure return considerations included in steam coil since the pipes are
              ! perfect and do not account for losses.

              ! Return water is condensate at atmoshperic pressure
              ! Process is considered constant enthalpy expansion
              ! No quality function in EnergyPlus hence no option left apart from
              ! considering saturated state.
!              StdBaroPress=101325

              TempWaterAtmPress=GetSatTemperatureRefrig('Steam',StdBaroPress,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')

              ! Point 4 at atm - loop delta subcool during return journery back to pump
              TempLoopOutToPump=TempWaterAtmPress-SteamCoil(CoilNum)%LoopSubCoolReturn

              ! Actual Steam Coil Outlet Enthalpy
              EnthCoilOutlet=GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')-&
                                                                      CpWater*SubCoolDeltaTemp

              ! Enthalpy at Point 4
              EnthAtAtmPress=GetSatEnthalpyRefrig('STEAM',TempWaterAtmPress,0.0d0,SteamCoil(CoilNum)%FluidIndex,'CalcSteamAirCoil')

              CpWater = GetSatSpecificHeatRefrig('STEAM',TempLoopOutToPump,0.0d0,SteamCoil(CoilNum)%FluidIndex,'SizeSteamCoil')

              ! Reported value of coil outlet enthalpy at the node to match the node outlet temperature
              EnthPumpInlet=EnthAtAtmPress-CpWater*SteamCoil(CoilNum)%LoopSubCoolReturn

              SteamCoil(CoilNum)%OutletWaterEnthalpy =EnthPumpInlet

              ! Point 3-Point 5,
              EnergyLossToEnvironment=SteamMassFlowRate*(EnthCoilOutlet-EnthPumpInlet)

              ! Loss to enviornment due to pressure drop
              SteamCoil(CoilNum)%LoopLoss=EnergyLossToEnvironment
              !************************* Loop Losses *****************************
          END IF

       ELSE    ! If not running Conditions do not change across coil from inlet to outlet
          SteamMassFlowRate=0.d0
          CALL SetComponentFlowRate( SteamMassFlowRate, &
                                   SteamCoil(CoilNum)%SteamInletNodeNum, &
                                   SteamCoil(CoilNum)%SteamOutletNodeNum, &
                                   SteamCoil(CoilNum)%LoopNum, &
                                   SteamCoil(CoilNum)%LoopSide, &
                                   SteamCoil(CoilNum)%BranchNum, &
                                   SteamCoil(CoilNum)%CompNum )
          TempAirOut                                 = TempAirIn
          TempWaterOut                               = TempSteamIn
          HeatingCoilLoad                            = 0.0d0
          SteamCoil(CoilNum)%OutletWaterEnthalpy     = SteamCoil(CoilNum)%InletSteamEnthalpy
          SteamCoil(CoilNum)%OutletSteamMassFlowRate = 0.0d0
          SteamCoil(CoilNum)%OutletSteamQuality      = 0.0d0
          SteamCoil(CoilNum)%LoopLoss                = 0.0d0
          TempLoopOutToPump                          = TempWaterOut
       ENDIF

   END SELECT

   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     HeatingCoilLoad = HeatingCoilLoad*PartLoadRatio
   END IF

   ! Set the outlet conditions
   SteamCoil(CoilNum)%TotSteamHeatingCoilRate = HeatingCoilLoad
   SteamCoil(CoilNum)%OutletAirTemp           = TempAirOut
   SteamCoil(CoilNum)%OutletSteamTemp         = TempLoopOutToPump
   SteamCoil(CoilNum)%OutletSteamQuality      = 0.0d0
   QCoilActual = HeatingCoilLoad

   ! This SteamCoil does not change the moisture or Mass Flow across the component
   SteamCoil(CoilNum)%OutletAirHumRat       = SteamCoil(CoilNum)%InletAirHumRat
   SteamCoil(CoilNum)%OutletAirMassFlowRate = SteamCoil(CoilNum)%InletAirMassFlowRate
   !Set the outlet enthalpys for air and water
   SteamCoil(CoilNum)%OutletAirEnthalpy = PsyHFnTdbW(SteamCoil(CoilNum)%OutletAirTemp, &
                                                                  SteamCoil(CoilNum)%OutletAirHumRat)

   RETURN
 END Subroutine CalcSteamAirCoil

  ! Beginning of Update subroutines for the SteamCoil Module
 SUBROUTINE UpdateSteamCoil(CoilNum)
          ! SUBROUTINE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       na
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the coil outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the coil data structure to the coil outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataContaminantBalance, ONLY: Contaminant
   USE PlantUtilities,        ONLY: SafeCopyPlantNode

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(In) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   Integer             :: AirInletNode
   Integer             :: SteamInletNode
   Integer             :: AirOutletNode
   Integer             :: SteamOutletNode


   AirInletNode    = SteamCoil(CoilNum)%AirInletNodeNum
   SteamInletNode  = SteamCoil(CoilNum)%SteamInletNodeNum
   AirOutletNode   = SteamCoil(CoilNum)%AirOutletNodeNum
   SteamOutletNode = SteamCoil(CoilNum)%SteamOutletNodeNum

   ! Set the outlet air nodes of the SteamCoil
   Node(AirOutletNode)%MassFlowRate = SteamCoil(CoilNum)%OutletAirMassFlowRate
   Node(AirOutletNode)%Temp         = SteamCoil(CoilNum)%OutletAirTemp
   Node(AirOutletNode)%HumRat       = SteamCoil(CoilNum)%OutletAirHumRat
   Node(AirOutletNode)%Enthalpy     = SteamCoil(CoilNum)%OutletAirEnthalpy

   CALL SafeCopyPlantNode(SteamInletNode, SteamOutletNode)

   ! Set the outlet Steam nodes for the Coil
!   Node(SteamOutletNode)%MassFlowRate = SteamCoil(CoilNum)%OutletSteamMassFlowRate
   Node(SteamOutletNode)%Temp         = SteamCoil(CoilNum)%OutletSteamTemp
   Node(SteamOutletNode)%Enthalpy     = SteamCoil(CoilNum)%OutletWaterEnthalpy
   Node(SteamOutletNode)%Quality             = SteamCoil(CoilNum)%OutletSteamQuality
   !Node(SteamInletNode)%MassFlowRate  = SteamCoil(CoilNum)%OutletSteamMassFlowRate

   ! Set the outlet nodes for properties that just pass through & not used
   Node(AirOutletNode)%Quality             = Node(AirInletNode)%Quality
   Node(AirOutletNode)%Press               = Node(AirInletNode)%Press
   Node(AirOutletNode)%MassFlowRateMin     = Node(AirInletNode)%MassFlowRateMin
   Node(AirOutletNode)%MassFlowRateMax     = Node(AirInletNode)%MassFlowRateMax
   Node(AirOutletNode)%MassFlowRateMinAvail= Node(AirInletNode)%MassFlowRateMinAvail
   Node(AirOutletNode)%MassFlowRateMaxAvail= Node(AirInletNode)%MassFlowRateMaxAvail

   ! Set the outlet nodes for properties that just pass through & not used

   !Node(SteamOutletNode)%Press              = Node(SteamInletNode)%Press
!   Node(SteamOutletNode)%Press               = StdBaroPress  ! Water out at atm pressure
!   Node(SteamOutletNode)%HumRat              = Node(SteamInletNode)%HumRat
!   Node(SteamOutletNode)%MassFlowRateMin     = Node(SteamInletNode)%MassFlowRateMin
!   Node(SteamOutletNode)%MassFlowRateMax     = Node(SteamInletNode)%MassFlowRateMax
!   Node(SteamOutletNode)%MassFlowRateMinAvail= Node(SteamInletNode)%MassFlowRateMinAvail
!   Node(SteamOutletNode)%MassFlowRateMaxAvail= Node(SteamInletNode)%MassFlowRateMaxAvail

!   IF (SteamCoil(CoilNum)%InletSteamMassFlowRate.EQ.0.0) THEN
!     Node(SteamInletNode)%MassFlowRate         = 0.0
!     Node(SteamInletNode)%MassFlowRateMinAvail = 0.0
!     Node(SteamOutletNode)%MassFlowRateMinAvail= 0.0
!   END IF

   IF (Contaminant%CO2Simulation) Then
     Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
   End If
   IF (Contaminant%GenericContamSimulation) Then
     Node(AirOutletNode)%GenContam = Node(AirInletNode)%GenContam
   End If

   RETURN
 END Subroutine UpdateSteamCoil
     ! End of Update subroutines for the SteamCoil Module

     ! Beginning of Reporting subroutines for the SteamCoil Module
 SUBROUTINE ReportSteamCoil(CoilNum)
          ! SUBROUTINE INFORMATION:
          !   AUTHOR         Rahul Chillar
          !   DATE WRITTEN   Jan 2005
          !   MODIFIED       na
          !   RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the coils.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Report the SteamCoil energy from this component
   SteamCoil(CoilNum)%TotSteamHeatingCoilEnergy= SteamCoil(CoilNum)%TotSteamHeatingCoilRate*TimeStepSys*SecInHour

   RETURN
 END Subroutine ReportSteamCoil
        ! End of Reporting subroutines for the SteamCoil Module

        ! Utility subroutines for the SteamCoil Module

FUNCTION GetSteamCoilIndex(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the index for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: IndexNum   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilType == 'COIL:HEATING:STEAM') THEN
    IndexNum=FindItemInList(CoilName,SteamCoil%Name,NumSteamCoils)
  ELSE
    IndexNum=0
  ENDIF

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('GetSteamCoilIndex: Could not find CoilType="'//TRIM(CoilType)//  &
                         '" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetSteamCoilIndex

SUBROUTINE CheckSteamCoilSchedule(CompType,CompName,Value,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the correct schedule value for this coil

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName
  REAL(r64), INTENT(OUT)            :: Value
  INTEGER, INTENT(INOUT)       :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER CoilNum

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  ! Find the correct Coil number
  IF (CompIndex == 0) THEN
    CoilNum = FindItemInList(CompName,SteamCoil%Name,NumSteamCoils)
    IF (CoilNum == 0) THEN
      CALL ShowFatalError('CheckSteamCoilSchedule: Coil not found='//TRIM(CompName))
    ENDIF
    CompIndex=CoilNum
    Value=GetCurrentScheduleValue(SteamCoil(CoilNum)%SchedPtr)  ! not scheduled?
  ELSE
    CoilNum=CompIndex
    IF (CoilNum > NumSteamCoils .or. CoilNum < 1) THEN
      CALL ShowFatalError('SimulateSteamCoilComponents: Invalid CompIndex passed='//TRIM(TrimSigDigits(CoilNum))// &
               ', Number of Steam Coils='//TRIM(TrimSigDigits(NumSteamCoils))//', Coil name='//TRIM(CompName))
    ENDIF
    IF (CompName /= SteamCoil(CoilNum)%Name) THEN
      CALL ShowFatalError('SimulateSteamCoilComponents: Invalid CompIndex passed='//TRIM(TrimSigDigits(CoilNum))// &
               ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//TRIM(SteamCoil(CoilNum)%Name))
    ENDIF
    Value=GetCurrentScheduleValue(SteamCoil(CoilNum)%SchedPtr)  ! not scheduled?
  ENDIF

  RETURN

END SUBROUTINE CheckSteamCoilSchedule

FUNCTION GetCoilMaxWaterFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxWaterFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max water flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: MaxWaterFlowRate  ! returned max water flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER,SAVE :: ErrCount=0

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    WhichCoil=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
    IF (WhichCoil /= 0) THEN
      ! coil does not specify MaxWaterFlowRate
      MaxWaterFlowRate=0.0d0
      CALL ShowRecurringWarningErrorAtEnd('Requested Max Water Flow Rate from COIL:Heating:Steam N/A',ErrCount)
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilMaxWaterFlowRate: Could not find CoilType="'//TRIM(CoilType)//  &
                         '" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    MaxWaterFlowRate=-1000.d0
  ENDIF

  RETURN

END FUNCTION GetCoilMaxWaterFlowRate

FUNCTION GetCoilMaxSteamFlowRate(CoilIndex,ErrorsFound) RESULT(MaxSteamFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max steam flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and flow rate is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CoilIndex    ! must match coil types in this module
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: MaxSteamFlowRate  ! returned max steam flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilMaxSteamFlowRate: Could not find CoilType = "Coil:Heating:Steam"')
    ErrorsFound=.true.
    MaxSteamFlowRate=0.0d0
  ELSE
    MaxSteamFlowRate=SteamCoil(CoilIndex)%MaxSteamVolFlowRate
  ENDIF

  RETURN

END FUNCTION GetCoilMaxSteamFlowRate

FUNCTION GetCoilAirInletNode(CoilIndex,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the air inlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CoilIndex    ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilAirInletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    NodeNumber = 0
  ELSE
    NodeNumber=SteamCoil(CoilIndex)%AirInletNodeNum
  ENDIF

  RETURN

END FUNCTION GetCoilAirInletNode

FUNCTION iGetCoilAirOutletNode(CoilIndex,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the air outlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CoilIndex    ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilAirOutletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    NodeNumber = 0
  ELSE
    NodeNumber=SteamCoil(CoilIndex)%AirOutletNodeNum
  ENDIF

  RETURN

END FUNCTION iGetCoilAirOutletNode

FUNCTION cGetCoilAirOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the air outlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: IndexNum   ! returned air inlet node number of matched coil

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    IndexNum=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
  ELSE
    IndexNum=0
  ENDIF

  IF (IndexNum == 0) THEN
    NodeNumber = 0
  ELSE
    NodeNumber=SteamCoil(IndexNum)%AirOutletNodeNum
  ENDIF

  RETURN

END FUNCTION cGetCoilAirOutletNode

FUNCTION iGetCoilSteamInletNode(CoilIndex, CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam inlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CoilIndex    ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilSteamInletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    NodeNumber=0
  ELSE
    NodeNumber=SteamCoil(CoilIndex)%SteamInletNodeNum
  ENDIF

  RETURN

END FUNCTION iGetCoilSteamInletNode

FUNCTION cGetCoilSteamInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         L. Lawrie (based on R. Raustad)
          !       DATE WRITTEN   June 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam inlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: IndexNum   ! returned air inlet node number of matched coil

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    IndexNum=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
  ELSE
    IndexNum=0
  ENDIF

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('GetCoilSteamInletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    NodeNumber=0
  ELSE
    NodeNumber=SteamCoil(IndexNum)%SteamInletNodeNum
  ENDIF

  RETURN

END FUNCTION cGetCoilSteamInletNode

FUNCTION iGetCoilSteamOutletNode(CoilIndex, CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam inlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CoilIndex    ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilSteamInletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    NodeNumber=0
  ELSE
    NodeNumber=SteamCoil(CoilIndex)%SteamOutletNodeNum
  ENDIF

  RETURN

END FUNCTION iGetCoilSteamOutletNode

FUNCTION cGetCoilSteamOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         L. Lawrie (based on R. Raustad)
          !       DATE WRITTEN   June 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam inlet node number for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned air inlet node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: IndexNum   ! returned air inlet node number of matched coil

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    IndexNum=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
  ELSE
    IndexNum=0
  ENDIF

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('GetCoilSteamInletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    NodeNumber=0
  ELSE
    NodeNumber=SteamCoil(IndexNum)%SteamOutletNodeNum
  ENDIF

  RETURN

END FUNCTION cGetCoilSteamOutletNode

FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(Capacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam coils operating capacity and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: Capacity     ! returned operating capacity of matched coil (W)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    WhichCoil=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
    IF (WhichCoil /= 0) THEN
      ! coil does not specify MaxWaterFlowRate
      Capacity=SteamCoil(WhichCoil)%OperatingCapacity
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilSteamInletNode: Could not find CoilType="'//TRIM(CoilType)//  &
                         '" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    Capacity=0.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilCapacity

FUNCTION GetTypeOfCoil(CoilIndex,CoilName,ErrorsFound) RESULT(TypeOfCoil)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam coils operating capacity and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CoilIndex     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: TypeOfCoil   ! returned coil type of matched coil (W)
                                         ! 1 = TemperatureSetpointControl
                                         ! 3 = ZoneLoadControl

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

   ! Obtains and Allocates SteamCoil related parameters from input file
   IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
       CALL GetSteamCoilInput
       GetSteamCoilsInputFlag=.false.
   End If

  IF (CoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilSteamInletNode: Could not find CoilType = "Coil:Heating:Steam"'// &
                         ' with Name = '//TRIM(CoilName))
    ErrorsFound=.true.
    TypeOfCoil=0
  ELSE
    TypeOfCoil=SteamCoil(CoilIndex)%TypeofCoil
  ENDIF

  RETURN

END FUNCTION GetTypeOfCoil

FUNCTION GetSteamCoilControlNodeNum(CoilType,CoilName,ErrorFlag) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Nigusse, FSEC
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the steam coils and returns the steam control node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorFlag    ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates SteamCoil related parameters from input file
  IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
      CALL GetSteamCoilInput
      GetSteamCoilsInputFlag=.false.
  End If

  WhichCoil=0
  NodeNumber=0
  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    WhichCoil=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=SteamCoil(WhichCoil)%TempSetPointNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetSteamCoilControlNodeNum: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorFlag=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetSteamCoilControlNodeNum

FUNCTION GetSteamCoilAvailScheduleIndex(CoilType,CoilName,ErrorsFound) RESULT(AvailSchIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the availability schedule index.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: AvailSchIndex   ! returned availability schedule of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetSteamCoilsInputFlag) THEN  !First time subroutine has been entered
      CALL GetSteamCoilInput
      GetSteamCoilsInputFlag=.false.
  End If

  WhichCoil=0
  AvailSchIndex=0

  IF (SameString(CoilType,'Coil:Heating:Steam')) THEN
    WhichCoil=FindItem(CoilName,SteamCoil%Name,NumSteamCoils)
    IF (WhichCoil /= 0) THEN
      AvailSchIndex=SteamCoil(WhichCoil)%SchedPtr
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilAvailScheduleIndex: Could not find Coil, Type="'// &
                          TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    AvailSchIndex=0
  ENDIF

  RETURN

END FUNCTION GetSteamCoilAvailScheduleIndex

        ! End of Utility subroutines for the SteamCoil Module

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

END MODULE SteamCoils





