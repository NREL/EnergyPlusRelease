MODULE TranspiredCollector

          ! Module containing routines and data dealing with the Transpired Collectors

          ! MODULE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Ecapsulates data and routines for simulating unglazed transpired solar collectors (UTSC)
          !   as a component on the HVAC air system.


          ! METHODOLOGY EMPLOYED:
          ! Two modes, passive and active.  Active is when air is purposely drawn through collector.
          ! Passive is when air exchanges are driven by Natural Ventilation rather than outside air system
          !

          ! REFERENCES:
          ! Heat Exchange effectiveness relations:
          ! Kutscher, C.F. 1994. Heat exchange effectiveness and pressure drop for air flow through perforated plates
          !     with and without crosswind. Journal of Heat Transfer. May 1994, Vol. 116, p. 391.
          !     American Society of Mechanical Engineers.
          ! Van Decker, G.W.E., K.G.T. Hollands, and A.P. Brunger. 2001. Heat-exchange relations for unglazed transpired
          !     solar collectors with circular holes on a square of triangular pitch. Solar Energy. Vol. 71, No. 1. pp 33-45, 2001.
          ! .

          ! OTHER NOTES:
          ! EnergyPlus implementation is unique and adds new modeling not described in Literature.
          !   See EngineeringReference for details

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals,      ONLY: DegToRadians, KelvinConv, MaxNameLength, SecInHour
USE DataInterfaces,   ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueErrorTimeStamp, ShowMessage, &
                            ShowContinueError, ShowWarningMessage, ShowRecurringWarningErrorAtEnd
USE DataVectorTypes,  ONLY: Vector
USE DataHeatBalance , ONLY: QRadSWOutIncident, Construct, Material

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: Layout_Square=1
INTEGER, PARAMETER :: Layout_Triangle=2
INTEGER, PARAMETER :: Correlation_Kutscher1994=1
INTEGER, PARAMETER :: Correlation_VanDeckerHollandsBrunger2001=2

          ! DERIVED TYPE DEFINITIONS:
Type UTSCDataStruct
  ! from input data
  CHARACTER(len=MaxNameLength) :: Name             = ' ' !
  CHARACTER(len=MaxNameLength) :: OSCMName         = ' ' !OtherSideConditionsModel
  INTEGER                      :: OSCMPtr          = 0  ! OtherSideConditionsModel index
  INTEGER                      :: SchedPtr         = 0  ! Availablity schedule
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: InletNode     ! Air system node "pointer", should be set to outdoor air
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: OutletNode    ! Air system node "pointer", outlet from UTSC
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: ControlNode   ! Air system node "pointer", should have mixed air setpoint
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: ZoneNode      ! Air system node "pointer", should have zone node
  INTEGER                      :: Layout           = 0 ! 'Square' or 'Triangle'
  INTEGER                      :: Correlation      = 0 ! which heat exchanger effectiveness model
  REAL(r64)                    :: HoleDia          = 0.d0 ! Diameter of Perforations in Collector [m]
  REAL(r64)                    :: Pitch            = 0.d0 ! Distance between Perforations in Collector [m]
  REAL(r64)                    :: LWEmitt          = 0.d0 ! Thermal Emissivity of Collector Surface [dimensionless]
  REAL(r64)                    :: SolAbsorp        = 0.d0 ! Solar Absorbtivity of Collector Surface [dimensionless]
  INTEGER                      :: CollRoughness    = 1  ! surface roughness for exterior convection calcs.
  REAL(r64)                    :: PlenGapThick     = 0.d0 ! Depth of Plenum Behind Collector [m]
  REAL(r64)                    :: PlenCrossArea    = 0.d0 ! cross section area of plenum behind collector [m2]
  INTEGER                      :: NumSurfs         = 0  ! a single collector can have multiple surfaces underneath it
  INTEGER, ALLOCATABLE, DIMENSION(:) ::SurfPtrs    != 0  ! array of pointers for participating underlying surfaces
  REAL(r64)                    :: Height           = 0.d0 ! Overall Height of Collector  [m]
  REAL(r64)                    :: AreaRatio        = 0.d0 ! Ratio of actual surface are to projected surface area [dimensionless]
  REAL(r64)                    :: CollectThick     = 0.d0 ! Thickness of collector absorber plate material.  [m]
  REAL(r64)                    :: Cv               = 0.d0 ! volume-based effectiveness of openings for wind-driven vent when Passive
  REAL(r64)                    :: Cd               = 0.d0 ! discharge coefficient of openings for bouyancy-driven vent when Passive
  INTEGER                      :: NumOASysAttached = 0  ! =1 if no splitter, other wise set by Splitter object
  INTEGER                      :: FreeHeatSetpointSchedPtr = 0 ! used for controlling seperately from usual setpoint managers.
  INTEGER                      :: VsucErrIndex     = 0
 ! data from elswhere and calculated
  REAL(r64)                    :: ActualArea       = 0.d0 ! Overall Area of Collect with surface corrugations.
  REAL(r64)                    :: ProjArea         = 0.d0 ! Overall Area of Collector projected, as if flat [m2]
  TYPE (vector)                :: Centroid         = vector(0.0d0,0.0d0,0.0d0 )  ! computed centroid
  REAL(r64)                    :: Porosity         = 0.d0 ! fraction of absorber plate [--]
  LOGICAL                      :: isOn             = .false.  ! .true. means "on" or "ACTIVE" , .false means "off" or "PASSIVE
  REAL(r64)                    :: Tplen            = 0.d0 ! modeled drybulb temperature for air between collector and wall [C]
  REAL(r64)                    :: Tcoll            = 0.d0 ! modeled surface temperature for collector [C]
  REAL(r64)                    :: TplenLast        = 22.5d0 ! Old Value for modeled drybulb temp if air between collector and wall [C]
  REAL(r64)                    :: TcollLast        = 22.0d0 ! Old value for modeled surface temperature for collector [C]
  REAL(r64)                    :: HrPlen           = 0.0d0  ! Modeled radiation coef for OSCM [W/m2-C]
  REAL(r64)                    :: HcPlen           = 0.d0 ! Modeled Convection coef for OSCM [W/m2-C]
  REAL(r64)                    :: MdotVent         = 0.d0 ! air mass flow exchanging with ambient when passive.
  REAL(r64)                    :: HdeltaNPL        = 0.d0 ! lenth scale for bouyancy-driven vent when Passive [m]
  REAL(r64)                    :: TairHX           = 0.d0 ! air drybulb of air leaving collector when Active [C]
  REAL(r64)                    :: InletMDot        = 0.d0 ! flow rate from outdoor mixer controller
  REAL(r64)                    :: InletTempDB      = 0.d0
  REAL(r64)                    :: Tilt             = 0.d0 ! Tilt from area weighted average of underlying surfaces
  REAL(r64)                    :: Azimuth          = 0.d0 ! Azimuth from area weighted average of underlying surfaces
  REAL(r64)                    :: QdotSource       = 0.d0 ! Source/sink term
  ! reporting data
  REAL(r64)                    :: Isc              = 0.d0 ! total incident solar on collector [W]
  REAL(r64)                    :: HXeff            = 0.d0 ! heat exchanger effectiveness [--]
  REAL(r64)                    :: Vsuction         = 0.d0 ! Average suction face velocity [m/s]
  REAL(r64)                    :: PassiveACH       = 0.d0 ! air changes per hour when passive [1/hr]
  REAL(r64)                    :: PassiveMdotVent  = 0.d0 ! Total Nat Vent air change rate  [kg/s]
  REAL(r64)                    :: PassiveMdotWind  = 0.d0 ! Nat Vent air change rate from Wind-driven [kg/s]
  REAL(r64)                    :: PassiveMdotTherm = 0.d0 ! Nat. Vent air change rate from bouyancy-driven flow [kg/s]
  REAL(r64)                    :: PlenumVelocity   = 0.d0 ! effective velocity inside plenum [m/s]
  REAL(r64)                    :: SupOutTemp       = 0.d0 ! supply air outlet temperature [C]
  REAL(r64)                    :: SupOutHumRat     = 0.d0 ! supply air outlet humidity ratio [kg water/kg dry air]
  REAL(r64)                    :: SupOutEnth       = 0.d0 ! supply air outlet enthalpy [J/kg]
  REAL(r64)                    :: SupOutMassFlow   = 0.d0 ! supply air outlet mass flow rate [kg/s]
  REAL(r64)                    :: SensHeatingRate  = 0.d0 ! rate of sensible heat being added to the supply (primary) air [W]
  REAL(r64)                    :: SensHeatingEnergy= 0.d0 ! sensible heat added to the supply (primary) air [J]
  REAL(r64)                    :: SensCoolingRate  = 0.d0 ! rate of sensible heat being removed from the supply (primary) air [W]
  REAL(r64)                    :: SensCoolingEnergy= 0.d0 ! sensible heat removed from the supply (primary) air [J]
  REAL(r64)                    :: UTSCEfficiency   = 0.d0 ! Total Efficiency (with wall) SensHeatingRate/IncidentRadiation[--]
  REAL(r64)                    :: UTSCCollEff      = 0.d0 ! Collector-only Efficiency [--]
End Type UTSCDataStruct

          ! MODULE VARIABLE DECLARATIONS:
INTEGER  :: NumUTSC=0  ! number of transpired collectors in model
TYPE (UTSCDataStruct), ALLOCATABLE, DIMENSION(:) :: UTSC
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL      :: GetInputFlag = .true.  ! First time, input is gotten

          ! SUBROUTINE SPECIFICATIONS FOR MODULE TranspiredCollector:

PUBLIC  SimTranspiredCollector
PRIVATE GetTranspiredCollectorInput
PRIVATE InitTranspiredCollector
PRIVATE CalcActiveTranspiredCollector
PRIVATE CalcPassiveTranspiredCollector
PRIVATE UpdateTranspiredCollector
PUBLIC  SetUTSCQdotSource
PUBLIC  GetTranspiredCollectorIndex
PUBLIC  GetUTSCTsColl

CONTAINS

SUBROUTINE SimTranspiredCollector(CompName, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage simulation of Transpired Collectors

          ! METHODOLOGY EMPLOYED:
          ! Setup to avoid string comparisons after first call

          ! REFERENCES:
          !  none

          ! USE STATEMENTS:
  USE InputProcessor ,  ONLY: FindItemInList
  USE General,          ONLY: TrimSigDigits
  USE DataLoopNode ,    ONLY: Node
  USE ScheduleManager,  ONLY: GetCurrentScheduleValue
  USE DataHVACGlobals,  ONLY: TempControlTol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: CompName  !component name
  INTEGER, INTENT(INOUT)           :: CompIndex !component index (to reduce string compares during simulation)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER           :: UTSCNum = 0            ! local number index for UTSC

  IF (GetInputFlag) THEN
    CALL GetTranspiredCollectorInput
    GetInputFlag=.false.
  ENDIF

  ! Find the correct transpired collector with the Component name and/or index
  IF (CompIndex == 0) THEN
    UTSCNum = FindItemInList(CompName,UTSC%Name,NumUTSC)
    IF (UTSCNum == 0) THEN
      CALL ShowFatalError('Transpired Collector not found='//TRIM(CompName))
    ENDIF
    CompIndex=UTSCNum
  ELSE
    UTSCNum=CompIndex
    IF (UTSCNum > NumUTSC .or. UTSCNum < 1) THEN
      CALL ShowFatalError('SimTranspiredCollector: Invalid CompIndex passed='//TRIM(TrimSigDigits(UTSCNum))// &
                  ', Number of Transpired Collectors='//TRIM(TrimSigDigits(NumUTSC))//', UTSC name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(UTSCNum)) THEN
      IF (CompName /= UTSC(UTSCNum)%Name) THEN
        CALL ShowFatalError('SimTranspiredCollector: Invalid CompIndex passed='//TRIM(TrimSigDigits(UTSCNum))// &
                    ', Transpired Collector name='//TRIM(CompName)//', stored Transpired Collector Name for that index='//  &
                     TRIM(UTSC(UTSCNum)%Name))
      ENDIF
      CheckEquipName(UTSCNum)=.false.
    ENDIF
  ENDIF

  CALL InitTranspiredCollector(CompIndex)

  ! Control point of deciding if transpired collector is active or not.
  IF ( ( ANY((Node(UTSC(CompIndex)%InletNode)%Temp + TempControlTol) &
               < Node(UTSC(CompIndex)%ControlNode)%TempSetPoint) .OR. & ! heating required
       ( ANY((Node(UTSC(CompIndex)%InletNode)%Temp + TempControlTol) &  ! free heating helpful
              < GetCurrentScheduleValue(UTSC(CompIndex)%FreeHeatSetpointSchedPtr)).and. &
         ANY((Node(UTSC(CompIndex)%ZoneNode)%Temp+ TempControlTol)  &   ! free heating helpful
              < GetCurrentScheduleValue(UTSC(CompIndex)%FreeHeatSetpointSchedPtr)) ) ) .AND. &
       (GetCurrentScheduleValue(UTSC(CompIndex)%SchedPtr) > 0.0d0) .AND. &  !availability Schedule
       (UTSC(CompIndex)%InletMdot > 0.0d0)   )  THEN  ! OA system is setting mass flow

     UTSC(CompIndex)%isOn = .TRUE.
  ELSE
     UTSC(CompIndex)%isOn = .FALSE.
  ENDIF

  If (UTSC(UTSCNum)%isOn) then

    CALL CalcActiveTranspiredCollector(UTSCNum)

  ELSE

    CALL CalcPassiveTranspiredCollector(UTSCNum)

  ENDIF

  CALL UpdateTranspiredCollector(UTSCNum)

  RETURN

END SUBROUTINE SimTranspiredCollector

SUBROUTINE GetTranspiredCollectorInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Retrieve user input and set up data structure

          ! METHODOLOGY EMPLOYED:
          ! usual EnergyPlus input
          ! Extensible UTSC object for underlying heat transfer surfaces and for multisystem

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, GetObjectDefMaxArgs, FindItemInList , &
                              SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE DataGlobals,      ONLY: PI, ScheduleAlwaysOn
  USE DataInterfaces,   ONLY: ShowSevereError, SetupOutputVariable
  USE General,          ONLY: TrimSigDigits, RoundSigDigits
  USE DataSurfaces,     ONLY: Surface, OSCM, TotOSCM, TotSurfaces, OtherSideCondModeledExt
  USE ScheduleManager,  ONLY: GetScheduleIndex
  USE DataLoopNode,     ONLY: NodeType_Air, NodeConnectionType_Inlet, NodeConnectionType_Outlet, ObjectIsNotParent, &
                              NodeConnectionType_Sensor
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE DataHeatBalance,  ONLY: VeryRough, Rough, MediumRough, MediumSmooth, Smooth, VerySmooth
  USE BranchNodeConnections, ONLY: TestCompSet
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

  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: Alphas   ! Alpha items for extensible
                                                                     ! Solar Collectors:Unglazed Transpired object
  INTEGER                        :: Item    ! Item to be "gotten"
  REAL(r64),  DIMENSION(11)           :: Numbers    ! Numeric items for object
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: MaxNumAlphas !argumenet for call to GetObjectDefMaxArgs
  INTEGER                        :: MaxNumNumbers !argumenet for call to GetObjectDefMaxArgs
  INTEGER                        :: Dummy !argumenet for call to GetObjectDefMaxArgs
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: Found
  INTEGER                        :: AlphaOffset !local temp var
  CHARACTER(len=MaxNameLength)   :: Roughness
  INTEGER                        :: thisSurf  ! do loop counter
  REAL(r64)                      :: AvgAzimuth ! temp for error checking
  REAL(r64)                      :: AvgTilt    ! temp for error checking
  INTEGER                        :: SurfID  ! local surface "pointer"
  REAL(r64)                      :: TiltRads ! average tilt of collector in radians
  REAL(r64)                      :: tempHdeltaNPL ! temporary variable for bouyancy length scale
  INTEGER                        :: numUTSCSplitter !
 CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: AlphasSplit   ! Alpha items for extensible
                                                                         ! Solar Collectors:Unglazed Transpired object
  INTEGER                        :: ItemSplit    ! Item to be "gotten"
  REAL(r64) , DIMENSION(1) :: NumbersSplit    ! Numeric items for object
  INTEGER                        :: NumAlphasSplit  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbersSplit ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: MaxNumAlphasSplit !argumenet for call to GetObjectDefMaxArgs
  INTEGER                        :: MaxNumNumbersSplit !argumenet for call to GetObjectDefMaxArgs
  INTEGER                        :: IOStatusSplit   ! Used in GetObjectItem
  INTEGER                        :: NumOASys        ! do loop counter
  INTEGER                        :: ACountBase      ! counter for alhpasSplit
  Logical, allocatable, Dimension(:) :: SplitterNameOK  ! check for correct association of
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject  ! for ease in renaming.
  CHARACTER(len=MaxNameLength)   :: CurrentModuleMultiObject  ! for ease in renaming.

  CurrentModuleObject = 'SolarCollector:UnglazedTranspired'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Dummy, MaxNumAlphas,MaxNumNumbers)

  IF (MaxNumNumbers /= 11) THEN
    CALL ShowSevereError('GetTranspiredCollectorInput: '//TRIM(CurrentModuleObject)//' Object Definition indicates '// &
                         'not = 11 Number Objects, Number Indicated='//  &
                         TRIM(TrimSigDigits(MaxNumNumbers)))
    ErrorsFound=.true.
  ENDIF
  ALLOCATE(Alphas(MaxNumAlphas))
  Numbers = 0.0d0
  Alphas  = ' '

  numUTSC = GetNumObjectsFound(CurrentModuleObject)
  numUTSCSplitter = 0 !init
  CurrentModuleMultiObject =  'SolarCollector:UnglazedTranspired:Multisystem'
  numUTSCSplitter = GetNumObjectsFound(CurrentModuleMultiObject)

  ALLOCATE(UTSC(NumUTSC))
  ALLOCATE(CheckEquipName(NumUTSC))
  CheckEquipName=.true.
  ALLOCATE(SplitterNameOK(numUTSCSplitter))
  SplitterNameOK = .FALSE.

  DO Item=1,NumUTSC
    CALL GetObjectItem(CurrentModuleObject,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ! first handle alphas
    UTSC(Item)%Name     = Alphas(1)

    ! now check for multisystem
    If (numUTSCSplitter > 0) Then
       CALL GetObjectDefMaxArgs(TRIM(CurrentModuleMultiObject),Dummy, MaxNumAlphasSplit,MaxNumNumbersSplit)

      IF (MaxNumNumbersSplit /= 0) THEN
        CALL ShowSevereError('GetTranspiredCollectorInput: '//TRIM(CurrentModuleMultiObject)//' Object Definition '// &
                         'indicates not = 0 Number Objects, Number Indicated='//  &
                         TRIM(TrimSigDigits(MaxNumNumbersSplit)))
         ErrorsFound=.true.
      ENDIF
      IF (.NOT.ALLOCATED(AlphasSplit)) Allocate(AlphasSplit(MaxNumAlphasSplit))
      NumbersSplit = 0.0d0
      AlphasSplit  = ' '
      Do ItemSplit = 1, NumUTSCSplitter
         CALL GetObjectItem(CurrentModuleMultiObject,ItemSplit,AlphasSplit,NumAlphasSplit, &
                                NumbersSplit,NumNumbersSplit,IOStatusSplit)
         If (.NOT.( SameString(AlphasSplit(1),Alphas(1)) ) ) Cycle
         SplitterNameOK(ItemSplit) = .true.
         UTSC(Item)%NumOASysAttached = floor(NumAlphasSplit/4.0d0)
         IF (MOD((NumAlphasSplit),4) /= 1) THEN
           CALL ShowSevereError('GetTranspiredCollectorInput: '//TRIM(CurrentModuleMultiObject)//  &
                         ' Object Definition indicates not uniform quadtuples of nodes for '//  &
                         TRIM(AlphasSplit(1)) )
           ErrorsFound=.true.
         ENDIF
         ALLOCATE (UTSC(Item)%InletNode(UTSC(Item)%NumOASysAttached))
         UTSC(Item)%InletNode = 0
         ALLOCATE (UTSC(Item)%OutletNode(UTSC(Item)%NumOASysAttached))
         UTSC(Item)%OutletNode = 0
         ALLOCATE (UTSC(Item)%ControlNode(UTSC(Item)%NumOASysAttached))
         UTSC(Item)%ControlNode = 0
         ALLOCATE (UTSC(Item)%ZoneNode(UTSC(Item)%NumOASysAttached))
         UTSC(Item)%ZoneNode = 0
         Do NumOASys =  1, UTSC(Item)%NumOASysAttached
               ACountBase =  (NumOASys - 1)*4 + 2
               UTSC(Item)%InletNode(NumOASys) = &
                       GetOnlySingleNode(AlphasSplit(ACountBase),ErrorsFound,TRIM(CurrentModuleObject), &
                               AlphasSplit(1), NodeType_Air,NodeConnectionType_Inlet,NumOASys,ObjectIsNotParent)

               UTSC(Item)%OutletNode(NumOASys) = &
                       GetOnlySingleNode(AlphasSplit(ACountBase + 1),ErrorsFound,  &
                               TRIM(CurrentModuleObject), &
                               AlphasSplit(1), NodeType_Air,NodeConnectionType_Outlet,NumOASys,ObjectIsNotParent)
               CALL TestCompSet(TRIM(CurrentModuleObject),AlphasSplit(1),AlphasSplit(ACountBase), &
                     AlphasSplit(ACountBase + 1), 'Transpired Collector Air Nodes')  !appears that test fails by design??
               UTSC(Item)%ControlNode(NumOASys) = &
                       GetOnlySingleNode(AlphasSplit(ACountBase + 2),ErrorsFound,  &
                               TRIM(CurrentModuleObject), &
                               AlphasSplit(1), NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

               UTSC(Item)%ZoneNode(NumOASys) = &
                       GetOnlySingleNode(AlphasSplit(ACountBase + 3),ErrorsFound,  &
                               TRIM(CurrentModuleObject), &
                               AlphasSplit(1), NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

         ENDDO  ! Each OA System in a Multisystem
         ! DEALLOCATE(AlphasSplit)
      ENDDO ! each Multisystem present
    ENDIF ! any UTSC Multisystem present

    UTSC(Item)%OSCMName = Alphas(2)
    Found = FindItemInList(UTSC(Item)%OSCMName,OSCM%Name,TotOSCM)
    IF (Found == 0) THEN
        CALL ShowSevereError(TRIM(cAlphaFieldNames(2))//' not found='//TRIM(UTSC(Item)%OSCMName)// &
              ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
        ErrorsFound=.true.
    ENDIF
    UTSC(Item)%OSCMPtr = Found
    IF (lAlphaFieldBlanks(3)) THEN
      UTSC(Item)%SchedPtr = ScheduleAlwaysOn
    ELSE
      UTSC(Item)%SchedPtr = GetScheduleIndex(Alphas(3))
      IF (UTSC(Item)%SchedPtr == 0) THEN
        CALL ShowSevereError(TRIM(cAlphaFieldNames(3))//'not found='//TRIM(Alphas(3))// &
             ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
        ErrorsFound=.true.
        CYCLE
     ENDIF
   ENDIF

   !now if UTSC(Item)%NumOASysAttached still not set, assume no multisystem
    IF (UTSC(Item)%NumOASysAttached == 0) THEN
      UTSC(Item)%NumOASysAttached = 1
      ALLOCATE (UTSC(Item)%InletNode(1))
      UTSC(Item)%InletNode(1)  = 0
      ALLOCATE (UTSC(Item)%OutletNode(1))
      UTSC(Item)%OutletNode(1) = 0
      ALLOCATE (UTSC(Item)%ControlNode(1))
      UTSC(Item)%ControlNode(1)= 0
      ALLOCATE (UTSC(Item)%ZoneNode(1))
      UTSC(Item)%ZoneNode(1)= 0

      UTSC(Item)%InletNode(1) = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      UTSC(Item)%OutletNode(1) = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(4),Alphas(5), &
                       'Transpired Collector Air Nodes')

      UTSC(Item)%ControlNode(1) = &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      UTSC(Item)%ZoneNode(1) = &
               GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
    ENDIF !no splitter

    UTSC(Item)%FreeHeatSetpointSchedPtr = GetScheduleIndex(Alphas(8))
    IF (UTSC(Item)%FreeHeatSetpointSchedPtr == 0) THEN
       CALL ShowSevereError(TRIM(cAlphaFieldNames(8))//' not found='//TRIM(Alphas(8))// &
              ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
       ErrorsFound=.true.
       CYCLE
    ENDIF

    IF (SameString(Alphas(9),'Triangle')) THEN
      UTSC(Item)%layout      = Layout_Triangle
    ELSEIF (SameString(Alphas(9),'Square')) THEN
      UTSC(Item)%layout      = Layout_Square
    ELSE
      CALL ShowSevereError(TRIM(cAlphaFieldNames(9))//' has incorrect entry of '//Trim(alphas(9))// &
                   ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
      ErrorsFound = .TRUE.
      CYCLE
    ENDIF


    IF (SameString(Alphas(10),'Kutscher1994')) THEN
      UTSC(Item)%Correlation = Correlation_Kutscher1994
    ELSEIF (SameString(Alphas(10),'VanDeckerHollandsBrunger2001')) THEN
      UTSC(Item)%Correlation = Correlation_VanDeckerHollandsBrunger2001
    ELSE
      CALL ShowSevereError(TRIM(cAlphaFieldNames(10))//' has incorrect entry of '//Trim(alphas(9))// &
                   ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
      ErrorsFound = .TRUE.
      CYCLE
    ENDIF

    Roughness = Alphas(11)
    !Select the correct Number for the associated ascii name for the roughness type
    IF (SameString(Roughness,'VeryRough'))    UTSC(Item)%CollRoughness=VeryRough
    IF (SameString(Roughness,'Rough'))        UTSC(Item)%CollRoughness=Rough
    IF (SameString(Roughness,'MediumRough'))  UTSC(Item)%CollRoughness=MediumRough
    IF (SameString(Roughness,'MediumSmooth')) UTSC(Item)%CollRoughness=MediumSmooth
    IF (SameString(Roughness,'Smooth'))       UTSC(Item)%CollRoughness=Smooth
    IF (SameString(Roughness,'VerySmooth'))   UTSC(Item)%CollRoughness=VerySmooth

    ! Was it set?
    IF (UTSC(Item)%CollRoughness == 0) THEN
      CALL ShowSevereError(TRIM(cAlphaFieldNames(11))//' has incorrect entry of '//TRIM(Alphas(11))// &
                         ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
      ErrorsFound=.true.
    ENDIF

    AlphaOffset = 11
    UTSC(Item)%NumSurfs = NumAlphas - AlphaOffset
    IF (UTSC(Item)%NumSurfs == 0) THEN
      Call ShowSevereError('No underlying surfaces specified in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
      ErrorsFound = .true.
      CYCLE
    ENDIF
    ALLOCATE(UTSC(Item)%SurfPtrs(UTSC(Item)%NumSurfs))
    UTSC(Item)%SurfPtrs = 0
    DO thisSurf = 1, UTSC(Item)%NumSurfs
        Found = FindItemInList(Alphas(thisSurf + AlphaOffset), Surface%Name, TotSurfaces)
        If (Found == 0) Then
           CALL ShowSevereError('Surface Name not found='//TRIM(Alphas(thisSurf + AlphaOffset))// &
             ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
           ErrorsFound=.true.
           CYCLE
        ENDIF
        ! check that surface is appropriate, Heat transfer, Sun, Wind,
        IF (.not. surface(Found)%HeatTransSurf) then
            CALL ShowSevereError('Surface '//TRIM(Alphas(thisSurf + AlphaOffset))//' not of Heat Transfer type '// &
              ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
            ErrorsFound=.true.
            CYCLE
        ENDIF
        IF (.not. surface(found)%ExtSolar) then
            CALL ShowSevereError('Surface '//TRIM(Alphas(thisSurf + AlphaOffset))//' not exposed to sun '// &
             ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
            ErrorsFound=.true.
            CYCLE
        ENDIF
        IF (.not. surface(found)%ExtWind) then
            CALL ShowSevereError('Surface '//TRIM(Alphas(thisSurf + AlphaOffset))//' not exposed to wind '// &
              ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
            ErrorsFound=.true.
            CYCLE
        ENDIF
        If(surface(found)%ExtBoundCond /= OtherSideCondModeledExt) Then
            CALL ShowSevereError('Surface '//TRIM(Alphas(thisSurf + AlphaOffset))//' does not have OtherSideConditionsModel '// &
              'for exterior boundary conditions in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
            ErrorsFound=.true.
            CYCLE
        ENDIF
      ! check surface orientation, warn if upside down
      IF (( Surface(found)%Tilt < -95.0D0 ) .OR. (Surface(found)%Tilt > 95.0D0)) THEN
        CALL ShowWarningError('Suspected input problem with collector surface = '//TRIM(Alphas(thisSurf + AlphaOffset)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(UTSC(Item)%Name) )
        CALL ShowContinueError( 'Surface used for solar collector faces down')
        CALL ShowContinueError('Surface tilt angle (degrees from ground outward normal) = ' &
                                   //TRIM(RoundSigDigits(Surface(found)%Tilt,2)) )
      ENDIF

        UTSC(Item)%SurfPtrs(thisSurf) = Found

    ENDDO

    IF (ErrorsFound) CYCLE  ! previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

    ! now that we should have all the surfaces, do some preperations and checks.

    ! are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
    AvgAzimuth = SUM(Surface(UTSC(Item)%SurfPtrs)%Azimuth * Surface(UTSC(Item)%SurfPtrs)%Area) &
                /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
    AvgTilt    = SUM(Surface(UTSC(Item)%SurfPtrs)%Tilt * Surface(UTSC(Item)%SurfPtrs)%Area) &
                /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
    DO thisSurf = 1, UTSC(Item)%NumSurfs
       SurfID = UTSC(Item)%SurfPtrs(thisSurf)
       If (ABS(Surface(SurfID)%Azimuth - AvgAzimuth) > 15.d0 ) Then
            Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has Azimuth different from others in '// &
            'the group associated with '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
       ENDIF
       IF (ABS(Surface(SurfID)%Tilt - AvgTilt) > 10.d0 ) Then
            Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has Tilt different from others in '// &
            'the group associated with '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
       ENDIF

       !test that there are no windows.  Now allow windows
      ! If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
      !      Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
      !         //'subtracted in the group of surfaces associated with '//TRIM(UTSC(Item)%Name))
      ! endif

    ENDDO
    UTSC(Item)%Tilt    = AvgTilt
    UTSC(Item)%Azimuth = AvgAzimuth

    ! find area weighted centroid.
!    UTSC(Item)%Centroid%X = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%X*Surface(UTSC(Item)%SurfPtrs)%Area) &
!                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
!    UTSC(Item)%Centroid%Y = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%Y*Surface(UTSC(Item)%SurfPtrs)%Area) &
!                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
    UTSC(Item)%Centroid%Z = SUM(Surface(UTSC(Item)%SurfPtrs)%Centroid%Z*Surface(UTSC(Item)%SurfPtrs)%Area) &
                            /SUM(Surface(UTSC(Item)%SurfPtrs)%Area)

    !now handle numbers from input object
    UTSC(Item)%HoleDia       = Numbers(1)
    UTSC(Item)%Pitch         = Numbers(2)
    UTSC(Item)%LWEmitt       = Numbers(3)
    UTSC(Item)%SolAbsorp     = Numbers(4)
    UTSC(Item)%Height        = Numbers(5)
    UTSC(Item)%PlenGapThick  = Numbers(6)
    IF (UTSC(Item)%PlenGapThick <= 0.0d0) THEN
         CALL ShowSevereError('Plenum gap must be greater than Zero in '//TRIM(CurrentModuleObject)//' ='//TRIM(UTSC(Item)%Name))
         CYCLE
    ENDIF
    UTSC(Item)%PlenCrossArea = Numbers(7)
    UTSC(Item)%AreaRatio     = Numbers(8)
    UTSC(Item)%CollectThick  = Numbers(9)
    UTSC(Item)%Cv            = Numbers(10)
    UTSC(Item)%Cd            = Numbers(11)

    ! Fill out data we now know
    ! sum areas of HT surface areas
    UTSC(Item)%ProjArea      = SUM(Surface(UTSC(Item)%SurfPtrs)%Area)
    IF (UTSC(Item)%ProjArea == 0) THEN
         CALL ShowSevereError('Gross area of underlying surfaces is zero in '//TRIM(CurrentModuleObject)//  &
            ' ='//TRIM(UTSC(Item)%Name))
         CYCLE
    endif
    UTSC(Item)%ActualArea    = UTSC(Item)%ProjArea * UTSC(Item)%AreaRatio
    !  need to update this for slots as well as holes
    SELECT CASE (UTSC(Item)%Layout)
    CASE(Layout_Triangle)  ! 'TRIANGLE'
      UTSC(Item)%Porosity      = 0.907d0*(UTSC(Item)%HoleDia / UTSC(Item)%Pitch)**2.0d0        !Kutscher equation, Triangle layout
    CASE(Layout_Square)  ! 'SQUARE'
      UTSC(Item)%Porosity      = (PI/4.d0)*(UTSC(Item)%HoleDia**2.0d0)/(UTSC(Item)%Pitch**2.0d0) !Waterloo equation, square layout
    END SELECT
    TiltRads                 = ABS(AvgTilt) * DegToRadians
    TempHdeltaNPL            = SIN(TiltRads)*UTSC(Item)%Height / 4.0d0
    UTSC(Item)%HdeltaNPL     = MAX(tempHdeltaNPL, UTSC(Item)%PlenGapThick)


    CALL SetupOutputVariable('Solar Collector Heat Exchanger Effectiveness []',UTSC(Item)%HXeff, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Leaving Air Temperature [C]',UTSC(Item)%TairHX, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Outside Face Suction Velocity [m/s]',UTSC(Item)%Vsuction, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Surface Temperature [C]',UTSC(Item)%Tcoll, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Plenum Air Temperature [C]',UTSC(Item)%Tplen, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Sensible Heating Rate [W]',UTSC(Item)%SensHeatingRate, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Sensible Heating Energy [J]',UTSC(Item)%SensHeatingEnergy, &
                               'System','Sum',UTSC(Item)%Name, &
                                ResourceTypeKey='SolarAir' , EndUseKey='HeatProduced',GroupKey = 'System')

    CALL SetupOutputVariable('Solar Collector Natural Ventilation Air Change Rate [ACH]',UTSC(Item)%PassiveACH, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Natural Ventilation Mass Flow Rate [kg/s]',UTSC(Item)%PassiveMdotVent, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Wind Natural Ventilation Mass Flow Rate [kg/s]',UTSC(Item)%PassiveMdotWind, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Buoyancy Natural Ventilation Mass Flow Rate [kg/s]',UTSC(Item)%PassiveMdotTherm, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Incident Solar Radiation [W/m2]',UTSC(Item)%Isc, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector System Efficiency []',UTSC(Item)%UTSCEfficiency, &
                               'System','Average',UTSC(Item)%Name)
    CALL SetupOutputVariable('Solar Collector Surface Efficiency []',UTSC(Item)%UTSCCollEff, &
                               'System','Average',UTSC(Item)%Name)

  ENDDO

  Do ItemSplit = 1, NumUTSCSplitter
    If (.not.SplitterNameOK(ItemSplit)) then
     CALL ShowSevereError('Did not find a match, check names for Solar Collectors:Transpired Collector:Multisystem')
     ErrorsFound = .true.
    endif
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetTranspiredCollectorInput: Errors found in input')
  ENDIF

  DEALLOCATE(Alphas)


  RETURN

END SUBROUTINE GetTranspiredCollectorInput



SUBROUTINE InitTranspiredCollector(UTSCNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       B. Griffith, May 2009, added EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals
  USE DataHVACGlobals, ONLY:DoSetPointTest, SetPointErrorFlag
  USE DataLoopNode
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UTSCNum ! compindex already checked in calling routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  INTEGER             :: UTSCUnitNum
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  INTEGER             :: ControlNode
!unused  INTEGER             :: InletNode
  INTEGER             :: SplitBranch
  INTEGER             :: thisUTSC

  IF (MyOneTimeFlag) THEN
    ! do various one time setups and pitch adjustments across all UTSC
    DO thisUTSC=1, NumUTSC
      IF (UTSC(thisUTSC)%layout == Layout_Triangle) THEN
        SELECT CASE (UTSC(thisUTSC)%Correlation)
        CASE(Correlation_Kutscher1994)  ! Kutscher1994
           UTSC(thisUTSC)%Pitch = UTSC(thisUTSC)%Pitch
        CASE(Correlation_VanDeckerHollandsBrunger2001)  ! VanDeckerHollandsBrunger2001
           UTSC(thisUTSC)%Pitch = UTSC(thisUTSC)%Pitch/1.6d0
        END SELECT
      ENDIF
      IF (UTSC(thisUTSC)%layout == Layout_Square) THEN
        SELECT CASE (UTSC(thisUTSC)%Correlation)
        CASE(Correlation_Kutscher1994)  ! Kutscher1994
           UTSC(thisUTSC)%Pitch = UTSC(thisUTSC)%Pitch * 1.6d0
        CASE(Correlation_VanDeckerHollandsBrunger2001)  ! VanDeckerHollandsBrunger2001
           UTSC(thisUTSC)%Pitch = UTSC(thisUTSC)%Pitch
        END SELECT
      ENDIF
    ENDDO

    ALLOCATE(MyEnvrnFlag(NumUTSC))
    MyEnvrnFlag = .true.
    MyOneTimeFlag = .false.
  ENDIF !first time

  !Check that setpoint is active (from test by RJL in HVACEvapComponent)
  IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
    DO UTSCUnitNum = 1, NumUTSC
      DO SplitBranch = 1, UTSC(UTSCUnitNum)%NumOASysAttached
        ControlNode = UTSC(UTSCUnitNum)%ControlNode(SplitBranch)
        IF  (ControlNode > 0) THEN
          IF (Node(ControlNode)%TempSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('Missing temperature setpoint for UTSC ' // &
                                TRIM(UTSC(UTSCUnitNum)%Name))
              CALL ShowContinueError(' use a Setpoint Manager to establish a setpoint at the unit control node.')
              SetpointErrorFlag = .TRUE.
            ELSE
             ! need call to EMS to check node
              CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iTemperatureSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError('Missing temperature setpoint for UTSC ' // &
                                TRIM(UTSC(UTSCUnitNum)%Name))
                CALL ShowContinueError(' use a Setpoint Manager to establish a setpoint at the unit control node.')
                CALL ShowContinueError('Or add EMS Actuator to provide temperature setpoint at this node')
              ENDIF
            ENDIF
          END IF
        END IF
      END DO
    END DO
    MySetPointCheckFlag = .FALSE.
  END IF

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(UTSCNum)) THEN
    UTSC(UTSCNum)%TplenLast = 22.5d0
    UTSC(UTSCNum)%TcollLast = 22.0d0
    MyEnvrnFlag(UTSCNum) = .FALSE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(UTSCNum) = .TRUE.
  END IF

   !inits for each iteration
  UTSC(UTSCNum)%InletMdot   = Sum(Node(UTSC(UTSCNum)%InletNode)%MassFlowRate)

  UTSC(UTSCNum)%isOn     = .false.  ! intialize then turn on if appropriate
  UTSC(UTSCNum)%Tplen    = 0.0d0
  UTSC(UTSCNum)%Tcoll    = 0.0d0
  UTSC(UTSCNum)%MdotVent = 0.0d0
  UTSC(UTSCNum)%TairHX   = 0.0d0
  UTSC(UTSCNum)%HXeff    = 0.0d0
  UTSC(UTSCNum)%Isc      = 0.0d0

  UTSC(UTSCNum)%UTSCEfficiency  = 0.0d0
  UTSC(UTSCNum)%UTSCCollEff     = 0.0d0


  RETURN
END SUBROUTINE InitTranspiredCollector

SUBROUTINE CalcActiveTranspiredCollector(UTSCnum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment , ONLY: SkyTemp, OutHumRat, SunIsUp, OutBaroPress, IsRain
  USE Psychrometrics  , ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb, PsyHFnTdbW
  USE DataSurfaces    , ONLY: Surface
  USE DataHeatBalSurface, ONLY: TH
  USE DataHVACGlobals , ONLY: TimeStepSys
  USE ConvectionCoefficients, ONLY: InitExteriorConvectionCoeff
  USE General, ONLY: RoundSigDigits
  USE DataHeatBalance !, ONLY: QRadSWOutIncident, Construct, Material

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: UTSCNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: g          = 9.81d0           ! gravity constant (m/s**2)
  REAL(r64), PARAMETER  :: nu         = 15.66d-6       ! kinematic viscosity (m**2/s) for air at 300 K
                                                              ! (Mills 1999 Heat Transfer)
  REAL(r64), PARAMETER  :: k          = 0.0267d0         ! thermal conductivity (W/m K) for air at 300 K
                                                              ! (Mills 1999 Heat Transfer)
  REAL(r64), PARAMETER  :: Pr         = 0.71d0           ! Prandtl number for air
  REAL(r64), PARAMETER  :: Sigma      = 5.6697d-08     ! Stefan-Boltzmann constant
!  REAL(r64), PARAMETER  :: KelvinConv = KelvinConv         ! Conversion from Celsius to Kelvin
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  ! following arrays are used to temporarily hold results from multiple underlying surfaces
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HSkyARR  !
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HGroundARR
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HAirARR  !
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HPlenARR
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LocalWindArr
!  REAL(r64), ALLOCATABLE, DIMENSION(:) :: IscARR
!  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TsoARR

  ! working variables
!unused  INTEGER    :: InletNode  !
  REAL(r64)  :: RhoAir     ! density of air
  REAL(r64)  :: CpAir      ! specific heat of air
  REAL(r64)  :: holeArea   ! area of perforations, includes corrugation of surface
  REAL(r64)  :: Tamb       ! outdoor drybulb
  REAL(r64)  :: A          ! projected area of collector, from sum of underlying surfaces
  REAL(r64)  :: Vholes     ! mean velocity of air as it passes through collector holes
  REAL(r64)  :: Vsuction   ! mean velocity of air as is approaches the collector
  REAL(r64)  :: Vplen      ! mean velocity of air inside plenum
  REAL(r64)  :: HcPlen     ! surface convection heat transfer coefficient for plenum surfaces
  REAL(r64)  :: D          ! hole diameter
  REAL(r64)  :: ReD        ! Reynolds number for holes
  REAL(r64)  :: P          ! pitch, distance betweeen holes
  REAL(r64)  :: Por        ! porosity, area fraction of collector that is open because of holes
  REAL(r64)  :: Mdot       ! mass flow rate of suction air
  REAL(r64)  :: QdotSource ! energy flux for source/sink inside collector surface (for hybrid PV UTSC)
  INTEGER    :: thisSurf   ! do loop counter
  INTEGER    :: numSurfs   ! number of underlying HT surfaces associated with UTSC
  INTEGER    :: Roughness  ! parameters for surface roughness, defined in DataHeatBalance
  REAL(r64)  :: SolAbs     ! solar absorptivity of collector
  REAL(r64)  :: AbsExt     ! thermal emmittance of collector
  REAL(r64)  :: TempExt    ! collector temperature
  INTEGER    :: SurfPtr    ! index of surface in main surface structure
  REAL(r64)  :: HMovInsul  ! dummy for call to InitExteriorConvectionCoeff
  REAL(r64)  :: HExt       ! dummy for call to InitExteriorConvectionCoeff
  INTEGER    :: ConstrNum  ! index of construction in main construction structure
  REAL(r64)  :: AbsThermSurf ! thermal emmittance of underlying wall.
  REAL(r64)  :: TsoK       ! underlying surface temperature in Kelvin
  REAL(r64)  :: TscollK    ! collector temperature in Kelvin  (lagged)
  REAL(r64)  :: AreaSum    ! sum of contributing surfaces for area-weighted averages.
  REAL(r64)  :: Vwind      ! localized, and area-weighted average for wind speed
  REAL(r64)  :: HrSky      ! radiation coeff for sky, area-weighted average
  REAL(r64)  :: HrGround   ! radiation coeff for ground, area-weighted average
  REAL(r64)  :: HrAtm      ! radiation coeff for air (bulk atmosphere), area-weighted average
  REAL(r64)  :: Isc        ! Incoming combined solar radiation, area-weighted average
  REAL(r64)  :: HrPlen     ! radiation coeff for plenum surfaces, area-weighted average
  REAL(r64)  :: Tso        ! temperature of underlying surface, area-weighted average
  REAL(r64)  :: HcWind     ! convection coeff for high speed wind situations
  REAL(r64)  :: NuD        ! nusselt number for Reynolds based on hole
  REAL(r64)  :: U          ! overall heat exchanger coefficient
  REAL(r64)  :: HXeff      ! effectiveness for heat exchanger
  REAL(r64)  :: t          ! collector thickness
  REAL(r64)  :: ReS        ! Reynolds number based on suction velocity and pitch
  REAL(r64)  :: ReW        ! Reynolds number based on Wind and pitch
  REAL(r64)  :: ReB        ! Reynolds number based on hole velocity and pitch
  REAL(r64)  :: ReH        ! Reynolds number based on hole velocity and diameter
  REAL(r64)  :: Tscoll     ! temperature of collector
  REAL(r64)  :: TaHX       ! leaving air temperature from heat exchanger (entering plenum)
  REAL(r64)  :: Taplen     ! Air temperature in plen and outlet node.
  REAL(r64)  :: SensHeatingRate ! Rate at which the system is heating outdoor air
!  INTEGER, SAVE    :: VsucErrCount=0 !  warning message counter
!  CHARACTER(len=MaxNameLength) :: VsucErrString !  warning message counter string
  REAL(r64)  :: AlessHoles ! Area for Kutscher's relation

  !Active UTSC calculation
  ! first do common things for both correlations
  IF (.NOT. IsRain) Then
     Tamb       = SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%OutDryBulbTemp * Surface(UTSC(UTSCNum)%SurfPtrs)%Area) &
               / SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%Area)
  ELSE ! when raining we use wet bulb not drybulb
     Tamb       = SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%OutWetBulbTemp * Surface(UTSC(UTSCNum)%SurfPtrs)%Area) &
               / SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%Area)
  ENDIF

  RhoAir     = PsyRhoAirFnPbTdbW(OutBaroPress,Tamb, OutHumRat)

  CpAir      = PsyCpAirFnWTdb(OutHumRat,Tamb)

  holeArea   = UTSC(UTSCNum)%ActualArea*UTSC(UTSCNum)%Porosity



  A          = UTSC(UTSCNum)%ProjArea

  Vholes     = UTSC(UTSCNum)%InletMDot/RhoAir/holeArea

  Vplen      = UTSC(UTSCNum)%InletMDot/RhoAir/UTSC(UTSCNum)%PlenCrossArea

  Vsuction   = UTSC(UTSCNum)%InletMDot/RhoAir/A

  IF ((Vsuction < 0.001d0) .or. (Vsuction > 0.08d0)) THEN  ! warn that collector is not sized well
    IF (UTSC(UTSCNum)%VsucErrIndex == 0) THEN
       Call ShowWarningMessage('Solar Collector:Unglazed Transpired="'//Trim(UTSC(UTSCNum)%Name)// &
         '", Suction velocity is outside of range for a good design')
       Call ShowContinueErrorTimeStamp('Suction velocity ='//Trim(RoundSigDigits(Vsuction,4)) )
       If (Vsuction < 0.003d0) THEN
         CALL ShowContinueError('Velocity is low -- suggest decreasing area of transpired collector')
       ENDIF
       If (Vsuction > 0.08d0) THEN
         CALL ShowContinueError('Velocity is high -- suggest increasing area of transpired collector')
       ENDIF
       CALL ShowContinueError('Occasional suction velocity messages are not unexpected when simulating actual conditions')
    ENDIF
    CALL ShowRecurringWarningErrorAtEnd('Solar Collector:Unglazed Transpired="'//Trim(UTSC(UTSCNum)%Name)// &
         '", Suction velocity is outside of range',UTSC(UTSCNum)%VsucErrIndex,  &
         ReportMinOf=VSuction,ReportMinUnits='[m/s]',ReportMaxOf=VSuction,ReportMaxUnits='[m/s]')
  ENDIF

  HcPlen     = 5.62d0 + 3.92d0*Vplen

  D          = UTSC(UTSCNum)%holeDia

  ReD        = Vholes * D / nu

  P          = UTSC(UTSCNum)%pitch

  Por        = UTSC(UTSCNum)%Porosity

  Mdot       = UTSC(UTSCNum)%InletMdot

  QdotSource = UTSC(UTSCNum)%QdotSource  ! for hybrid PV transpired collectors

  !loop through underlying surfaces and collect needed data
    ! now collect average values for things associated with the underlying surface(s)
  NumSurfs = UTSC(UTSCNum)%numSurfs
  ALLOCATE(HSkyARR(NumSurfs))
  HSkyARR = 0.0d0
  ALLOCATE(HGroundARR(NumSurfs))
  HGroundARR = 0.0d0
  ALLOCATE(HAirARR(NumSurfs))
  HAirARR = 0.0d0
  ALLOCATE(LocalWindArr(NumSurfs))
  LocalWindArr = 0.0d0
 ! ALLOCATE(IscARR(NumSurfs))
 ! IscARR = 0.0
  Allocate(HPlenARR(NumSurfs))
  HPlenARR = 0.0d0
!  ALLOCATE(TsoARR(NumSurfs))
!  TsoARR = 0.0

  Roughness = UTSC(UTSCNum)%CollRoughness
  SolAbs    = UTSC(UTSCNum)%SolAbsorp
  AbsExt    = UTSC(UTSCNum)%LWEmitt
  TempExt   = UTSC(UTSCNum)%TcollLast
  Do thisSurf =1, NumSurfs
    SurfPtr = UTSC(UTSCNum)%SurfPtrs(thisSurf)
    ! Initializations for this surface
    HMovInsul     = 0.0d0
    HExt          = 0.0d0
    LocalWindArr(thisSurf) = Surface(SurfPtr)%WindSpeed
    CALL InitExteriorConvectionCoeff( SurfPtr,HMovInsul,Roughness,AbsExt,TempExt, &
                                HExt,HSkyARR(thisSurf),HGroundARR(thisSurf),HAirARR(thisSurf) )
    ConstrNum       = Surface(SurfPtr)%Construction
    AbsThermSurf = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal
    TsoK = TH(SurfPtr,1,1) + KelvinConv
    TscollK = UTSC(UTSCNum)%TcollLast + KelvinConv
    HPlenARR(thisSurf) = Sigma*AbsExt*AbsThermSurf*(TscollK**4 - TsoK**4)/(TscollK - TsoK)
  ENDDO
  AreaSum = SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%Area)
  ! now figure area-weighted averages from underlying surfaces.
  Vwind = Sum(LocalWindArr*Surface(UTSC(UTSCNum)%SurfPtrs)%Area)  /AreaSum
  DEALLOCATE(LocalWindArr)
  HrSky = Sum(HSkyARR*Surface(UTSC(UTSCNum)%SurfPtrs)%Area)       /AreaSum
  DEALLOCATE(HSkyARR)
  HrGround = Sum(HGroundARR*Surface(UTSC(UTSCNum)%SurfPtrs)%Area) /AreaSum
  DEALLOCATE(HGroundARR)
  HrAtm    = Sum(HAirARR*Surface(UTSC(UTSCNum)%SurfPtrs)%Area)    /AreaSum
  DEALLOCATE(HAirARR)
  HrPlen   = Sum(HPlenARR*Surface(UTSC(UTSCNum)%SurfPtrs)%Area)   /AreaSum
  DEALLOCATE(HPlenARR)

  Isc      = SUM(QRadSWOutIncident(UTSC(UTSCNum)%SurfPtrs)*Surface(UTSC(UTSCNum)%SurfPtrs)%Area) /AreaSum
  Tso      = SUM(TH((UTSC(UTSCNum)%SurfPtrs),1,1)*Surface(UTSC(UTSCNum)%SurfPtrs)%Area) /AreaSum

  IF (Vwind > 5.0d0) THEN
    Hcwind = 5.62d0 +3.9d0*(Vwind - 5.0d0)  !McAdams forced convection correlation
  ELSE
    Hcwind = 0.0d0
  ENDIF

  If (IsRain) Hcwind = 1000.0d0

  HXeff = 0.0d0 ! init

  SELECT CASE (UTSC(UTSCnum)%Correlation)


  CASE(Correlation_Kutscher1994)  ! Kutscher1994

    AlessHoles = A - holeArea

    NuD   = 2.75d0*( (((P/D)**(-1.2d0))*(ReD**0.43d0)) + (0.011d0 * Por * ReD*((Vwind/Vsuction)**0.48d0) ))
    U     = k * NuD/ D
    HXeff = 1.0d0 - exp(-1.d0*((U * AlessHoles)/ (mdot * CpAir)) )

  CASE(Correlation_VanDeckerHollandsBrunger2001)  ! VanDeckerHollandsBrunger2001
    t     = UTSC(UTSCNum)%CollectThick
    ReS   = Vsuction * P / nu
    ReW   = Vwind * P / nu
    ReB   = Vholes * P / nu
    ReH   = (Vsuction * D)/(nu * Por)
    IF (ReD > 0.0d0) THEN
      If (ReW > 0.0d0) THEN
        HXeff = (1.d0 - (1.d0 + ReS * MAX(1.733d0 * ReW**(-0.5d0), 0.02136d0) )**(-1.0d0) ) &
                * (1.d0 - (1.d0 + 0.2273d0 * (ReB**0.5d0))**(- 1.0d0) ) &
                * EXP( -0.01895d0*(P/D) - (20.62d0/ReH) * (t/D) )
      ELSE
        HXeff = (1.d0 - (1.d0 + ReS *  0.02136d0 )**(-1.0d0) ) &
                * (1.d0 - (1.d0 + 0.2273d0 * ReB**0.5d0)**(- 1.0d0) ) &
                * EXP( -0.01895d0*(P/D) - (20.62d0/ReH) * (t/D) )
      ENDIF
    ELSE
      HXeff = 0.0d0
    ENDIF
  END SELECT

  !now calculate collector temperature

  Tscoll = (Isc*SolAbs + HrAtm*Tamb + HrSky*SkyTemp + HrGround*Tamb + HrPlen*Tso + Hcwind*Tamb &
            + (Mdot*CpAir / A ) * Tamb - (Mdot*CpAir / A )*(1.d0 - HXeff)*Tamb + QdotSource) &
            /(HrAtm + HrSky + HrGround + Hrplen + Hcwind + (Mdot*CpAir / A )*HXeff)

  ! Heat exchanger leaving temperature
  TaHX  = HXeff*Tscoll + (1.d0-HXeff)*Tamb

  !now calculate plenum air temperature

  Taplen = (Mdot*CpAir*TaHX + HcPlen*A*Tso) / (Mdot*CpAir + HcPlen*A)

  ! calculate Sensible Heating Rate
  If (Taplen > Tamb) Then
    SensHeatingRate = Mdot*CpAir*(Taplen - Tamb)
  ELSE
    SensHeatingRate = 0.0d0
  endif

  !now fill results into derived types
  UTSC(UTSCNum)%Isc               = Isc
  UTSC(UTSCNum)%HXeff             = HXeff
  UTSC(UTSCNum)%Tplen             = Taplen
  UTSC(UTSCNum)%Tcoll             = Tscoll
  UTSC(UTSCNum)%HrPlen            = HrPlen
  UTSC(UTSCNum)%HcPlen            = HcPlen
  UTSC(UTSCNum)%TairHX            = TaHX
  UTSC(UTSCNum)%InletMdot         = Mdot
  UTSC(UTSCNum)%InletTempDB       = Tamb
  UTSC(UTSCNum)%Vsuction          = Vsuction
  UTSC(UTSCNum)%PlenumVelocity    = Vplen
  UTSC(UTSCNum)%SupOutTemp        = Taplen
  UTSC(UTSCNum)%SupOutHumRat      = OutHumRat  !stays the same with sensible heating
  UTSC(UTSCNum)%SupOutEnth        = PsyHFnTdbW(UTSC(UTSCNum)%SupOutTemp, &
                                             UTSC(UTSCNum)%SupOutHumRat)
  UTSC(UTSCNum)%SupOutMassFlow    = Mdot
  UTSC(UTSCNum)%SensHeatingRate   = SensHeatingRate
  UTSC(UTSCNum)%SensHeatingEnergy = SensHeatingRate * TimeStepSys * SecInHour
  UTSC(UTSCNum)%PassiveACH        = 0.0d0
  UTSC(UTSCNum)%PassiveMdotVent   = 0.0d0
  UTSC(UTSCNum)%PassiveMdotWind   = 0.0d0
  UTSC(UTSCNum)%PassiveMdotTherm  = 0.0d0
  IF (Isc > 10.0d0)  THEN
    UTSC(UTSCNum)%UTSCEfficiency  = SensHeatingRate / (Isc * A)
    IF (TaHX > Tamb) THen
       UTSC(UTSCNum)%UTSCCollEff     = Mdot*CpAir*(TaHX - Tamb) / (Isc * A)
    ELSE
       UTSC(UTSCNum)%UTSCCollEff     = 0.0d0
    ENDIF
  ELSE
    UTSC(UTSCNum)%UTSCEfficiency  = 0.0d0
    UTSC(UTSCNum)%UTSCCollEff     = 0.0d0
  ENDIF

  RETURN

END SUBROUTINE CalcActiveTranspiredCollector

SUBROUTINE CalcPassiveTranspiredCollector(UTSCNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! model the effect of the a ventilated baffle covering the outside of a heat transfer surface.

          ! METHODOLOGY EMPLOYED:
          ! All the work is done in a subroutine .

          ! REFERENCES:
          ! Nat. Vent. equations from ASHRAE HoF 2001 Chapt. 26

          ! USE STATEMENTS:

  USE DataEnvironment , ONLY: SunIsUp, OutBaroPress, OutEnthalpy
  USE Psychrometrics  , ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb, PsyWFnTdbTwbPb
  USE DataSurfaces    , ONLY: Surface
  USE DataHVACGlobals , ONLY: TimeStepSys
  USE ConvectionCoefficients, ONLY: InitExteriorConvectionCoeff

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: UTSCNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
    SUBROUTINE CalcPassiveExteriorBaffleGap(SurfPtrARR, VentArea, Cv, Cd, HdeltaNPL, SolAbs, AbsExt, Tilt, AspRat, GapThick, &
                                  Roughness,QdotSource, TsBaffle, TaGap, HcGapRpt,  HrGapRpt,IscRpt , MdotVentRpt, &
                                  VdotWindRpt, VdotBouyRpt)
      USE DataPrecisionGlobals
      INTEGER, INTENT(IN), DIMENSION(:)    :: SurfPtrARR  ! Array of indexes pointing to Surface structure in DataSurfaces
      REAL(r64), INTENT(IN)                :: VentArea    ! Area available for venting the gap [m2]
      REAL(r64), INTENT(IN)                :: Cv          ! Oriface coefficient for volume-based discharge, wind-driven [--]
      REAL(r64), INTENT(IN)                :: Cd          ! oriface coefficient for discharge,  bouyancy-driven [--]
      REAL(r64), INTENT(IN)                :: HdeltaNPL   ! Height difference from neutral pressure level [m]
      REAL(r64), INTENT(IN)                :: SolAbs      ! solar absorptivity of baffle [--]
      REAL(r64), INTENT(IN)                :: AbsExt      ! thermal absorptance/emittance of baffle material [--]
      REAL(r64), INTENT(IN)                :: Tilt        ! Tilt of gap [Degrees]
      REAL(r64), INTENT(IN)                :: AspRat      ! aspect ratio of gap  Height/gap [--]
      REAL(r64), INTENT(IN)                :: GapThick    ! Thickness of air space between baffle and underlying surface
      INTEGER, INTENT(IN)                  :: Roughness   ! Roughness index (1-6), see DataHeatBalance parameters
      REAL(r64), INTENT(IN)                :: QdotSource  ! Source/sink term, e.g. electricity exported from solar cell [W]
      REAL(r64), INTENT(INOUT)             :: TsBaffle    ! Temperature of baffle (both sides) use lagged value on input [C]
      REAL(r64), INTENT(INOUT)             :: TaGap       ! Temperature of air gap (assumed mixed) use lagged value on input [C]
      REAL(r64), INTENT(OUT), OPTIONAL     :: HcGapRpt       !
      REAL(r64), INTENT(OUT), OPTIONAL     :: HrGapRpt       !
      REAL(r64), INTENT(OUT), OPTIONAL     :: IscRpt
      REAL(r64), INTENT(OUT), OPTIONAL     :: MdotVentRpt
      REAL(r64), INTENT(OUT), OPTIONAL     :: VdotWindRpt
      REAL(r64), INTENT(OUT), OPTIONAL     :: VdotBouyRpt
    END SUBROUTINE CalcPassiveExteriorBaffleGap
  END INTERFACE
          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ! local working variables
  REAL(r64)  :: AspRat      ! Aspect Ratio of gap
  REAL(r64)  :: TmpTscoll
  REAL(r64)  :: TmpTaplen
  REAL(r64)  :: RhoAir
  REAL(r64)  :: holeArea
  REAL(r64)  :: Tamb
  REAL(r64)  :: HrPlen
  REAL(r64)  :: HcPlen
  REAL(r64)  :: Isc
  REAL(r64)  :: MdotVent
  REAL(r64)  :: VdotWind
  REAL(r64)  :: VdotThermal
  REAL(r64)  :: Twbamb
  REAL(r64)  :: OutHumRatAmb


  Tamb       = SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%OutDryBulbTemp * Surface(UTSC(UTSCNum)%SurfPtrs)%Area) &
               / SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%Area)
  Twbamb     = SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%OutWetBulbTemp * Surface(UTSC(UTSCNum)%SurfPtrs)%Area) &
               / SUM(Surface(UTSC(UTSCNum)%SurfPtrs)%Area)
  OutHumRatAmb = PsyWFnTdbTwbPb(Tamb, Twbamb, OutBaroPress)

  RhoAir     = PsyRhoAirFnPbTdbW(OutBaroPress,Tamb,OutHumRatAmb)
  holeArea   = UTSC(UTSCNum)%ActualArea*UTSC(UTSCNum)%Porosity

  AspRat     = UTSC(UTSCNum)%Height / UTSC(UTSCNum)%PlenGapThick
  TmpTscoll  = UTSC(UTSCNum)%TcollLast
  TmpTaplen  = UTSC(UTSCNum)%TplenLast

  ! all the work is done in this routine located in GeneralRoutines.f90

  Call CalcPassiveExteriorBaffleGap(UTSC(UTSCNum)%SurfPtrs,holeArea, UTSC(UTSCNum)%Cv, UTSC(UTSCNum)%Cd, UTSC(UTSCNum)%HdeltaNPL, &
                             UTSC(UTSCNum)%SolAbsorp, UTSC(UTSCNum)%LWEmitt, UTSC(UTSCNum)%Tilt, AspRat,   &
                             UTSC(UTSCNum)%PlenGapThick, UTSC(UTSCNum)%CollRoughness, UTSC(UTSCNum)%QdotSource, TmpTscoll,   &
                             TmpTaPlen, HcPlen , HrPlen , Isc, MdotVent,VdotWind,VdotThermal )


  !now fill results into derived types
  UTSC(UTSCNum)%Isc               = Isc
  UTSC(UTSCNum)%Tplen             = TmpTaPlen
  UTSC(UTSCNum)%Tcoll             = TmpTscoll
  UTSC(UTSCNum)%HrPlen            = HrPlen
  UTSC(UTSCNum)%HcPlen            = HcPlen
  UTSC(UTSCNum)%TairHX            = 0.0d0
  UTSC(UTSCNum)%InletMdot         = 0.0d0
  UTSC(UTSCNum)%InletTempDB       = Tamb
  UTSC(UTSCNum)%Vsuction          = 0.0d0
  UTSC(UTSCNum)%PlenumVelocity    = 0.0d0
  UTSC(UTSCNum)%SupOutTemp        = Tamb
  UTSC(UTSCNum)%SupOutHumRat      = OutHumRatAmb
  UTSC(UTSCNum)%SupOutEnth        = OutEnthalpy
  UTSC(UTSCNum)%SupOutMassFlow    = 0.0d0
  UTSC(UTSCNum)%SensHeatingRate   = 0.0d0
  UTSC(UTSCNum)%SensHeatingEnergy = 0.0d0
  UTSC(UTSCNum)%PassiveACH        = (MdotVent/RhoAir) *(1.d0/(UTSC(UTSCNum)%ProjArea*UTSC(UTSCNum)%PlenGapThick))*SecInHour
  UTSC(UTSCNum)%PassiveMdotVent   = MdotVent
  UTSC(UTSCNum)%PassiveMdotWind   = VdotWind * RhoAir
  UTSC(UTSCNum)%PassiveMdotTherm  = VdotThermal * RhoAir
  UTSC(UTSCNum)%UTSCEfficiency    = 0.0d0


  RETURN

END SUBROUTINE CalcPassiveTranspiredCollector


SUBROUTINE UpdateTranspiredCollector(UTSCNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,  ONLY: Node
  USE DataSurfaces,  ONLY: OSCM

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: UTSCNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   INTEGER   :: OutletNode
   INTEGER   :: InletNode
   INTEGER   :: thisOSCM
   INTEGER   :: thisOASys

   !update "last" values in Derived type
   UTSC(UTSCNum)%TplenLast = UTSC(UTSCNum)%Tplen
   UTSC(UTSCNum)%TcollLast = UTSC(UTSCNum)%Tcoll


   ! Set the outlet air nodes of the UTSC

   IF (UTSC(UTSCNum)%isOn) Then  ! Active
     If (UTSC(UTSCNum)%NumOASysAttached == 1) then
          OutletNode = UTSC(UTSCNum)%OutletNode(1)
          InletNode  = UTSC(UTSCNum)%InletNode(1)
          Node(OutletNode)%MassFlowRate  = UTSC(UTSCNum)%SupOutMassFlow
          Node(OutletNode)%Temp          = UTSC(UTSCNum)%SupOutTemp
          Node(OutletNode)%HumRat        = UTSC(UTSCNum)%SupOutHumRat
          Node(OutletNode)%Enthalpy      = UTSC(UTSCNum)%SupOutEnth
     ELSEIF (UTSC(UTSCNum)%NumOASysAttached > 1) THEN
          DO thisOASys=1, UTSC(UTSCNum)%NumOASysAttached
              Node(UTSC(UTSCNum)%OutletNode(thisOASys))%MassFlowRate &
                 = Node(UTSC(UTSCNum)%InletNode(thisOASys))%MassFlowRate  !system gets what it asked for at inlet
              Node(UTSC(UTSCNum)%OutletNode(thisOASys))%Temp     = UTSC(UTSCNum)%SupOutTemp
              Node(UTSC(UTSCNum)%OutletNode(thisOASys))%HumRat   = UTSC(UTSCNum)%SupOutHumRat
              Node(UTSC(UTSCNum)%OutletNode(thisOASys))%Enthalpy = UTSC(UTSCNum)%SupOutEnth

          ENDDO
     ENDIF
   ELSE    ! Passive and/or bypassed           Note Array assignments in following
     Node(UTSC(UTSCNum)%OutletNode)%MassFlowRate  = Node(UTSC(UTSCNum)%InletNode)%MassFlowRate
     Node(UTSC(UTSCNum)%OutletNode)%Temp          = Node(UTSC(UTSCNum)%InletNode)%Temp
     Node(UTSC(UTSCNum)%OutletNode)%HumRat        = Node(UTSC(UTSCNum)%InletNode)%HumRat
     Node(UTSC(UTSCNum)%OutletNode)%Enthalpy      = Node(UTSC(UTSCNum)%InletNode)%Enthalpy
   ENDIF

   ! update the OtherSideConditionsModel coefficients.
   thisOSCM  = UTSC(UTSCNum)%OSCMPtr

   OSCM(thisOSCM)%TConv   = UTSC(UTSCNum)%Tplen
   OSCM(thisOSCM)%HConv   = UTSC(UTSCNum)%HcPlen
   OSCM(thisOSCM)%TRad    = UTSC(UTSCNum)%Tcoll
   OSCM(thisOSCM)%HRad    = UTSC(UTSCNum)%HrPlen

  RETURN

END SUBROUTINE UpdateTranspiredCollector

SUBROUTINE SetUTSCQdotSource(UTSCNum, QSource)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Set" routine for updating sink term without exposing variables

          ! METHODOLOGY EMPLOYED:
          ! update derived type with new data , turn power into W/m2
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)  :: UTSCNum
  REAL(r64)    , INTENT(IN)  :: QSource  ! source term in Watts


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  UTSC(UTSCNum)%QdotSource = QSource / UTSC(UTSCNum)%ProjArea

  RETURN

END SUBROUTINE SetUTSCQdotSource

SUBROUTINE GetTranspiredCollectorIndex(SurfacePtr, UTSCIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Get" routine for establishing correct integer index from outside this module

          ! METHODOLOGY EMPLOYED:
          ! mine Surface derived type for correct index/number of surface
          ! mine UTSC derived type that has the surface.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor , ONLY: FindItemInList
  USE DataSurfaces   , ONLY: Surface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER     , INTENT(IN)     :: SurfacePtr
  INTEGER     , INTENT(OUT)    :: UTSCIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: UTSCNum ! temporary
  INTEGER     :: thisSurf ! temporary
  INTEGER     :: thisUTSC
  Logical     :: Found

  IF (GetInputFlag) THEN
    CALL GetTranspiredCollectorInput
    GetInputFlag=.false.
  ENDIF

  IF (SurfacePtr == 0) THEN
     CALL ShowFatalError('Invalid surface passed to GetTranspiredCollectorIndex, Surface name = ' &
         //TRIM(Surface(SurfacePtr)%Name))
  ENDIF

  UTSCNum = 0
  Found = .false.
  Do thisUTSC=1, NumUTSC
     Do thisSurf =1, UTSC(thisUTSC)%NumSurfs
        IF (SurfacePtr == UTSC(thisUTSC)%SurfPtrs(thisSurf)) then
          Found = .TRUE.
          UTSCNum = thisUTSC
        ENDIF
     ENDDO
  ENDDO

  IF (.NOT. Found) THEN
    CALL ShowFatalError('Did not find surface in UTSC description in GetTranspiredCollectorIndex, Surface name = '   &
         //TRIM(Surface(SurfacePtr)%Name))
  ELSE

    UTSCIndex=UTSCNum

  ENDIF

  RETURN

END SUBROUTINE GetTranspiredCollectorIndex

SUBROUTINE GetUTSCTsColl(UTSCNum, TsColl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Get" routine for collector surface temperature

          ! METHODOLOGY EMPLOYED:
          ! access derived type

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: UTSCNum
  REAL(r64), INTENT(OUT) :: TsColl

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TsColl = UTSC(UTSCNum)%Tcoll

  RETURN

END SUBROUTINE GetUTSCTsColl
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

END MODULE TranspiredCollector
