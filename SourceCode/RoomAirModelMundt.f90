MODULE MundtSimMgr

          ! MODULE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2002
          !       RE-ENGINEERED  June 2003, EnergyPlus Implementation (CC)
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

          ! PURPOSE OF THIS MODULE:
          ! This module is the main module for running the
          ! nodal air Mundt model...

          ! METHODOLOGY EMPLOYED:
          ! This module contains all subroutines required by the mundt model.
          ! The following modules from AirToolkit included in this module are:
          ! 1) MundtSimMgr Module,
          ! 2) MundtInputMgr Module, and
          ! 3) DataMundt Module,

          ! REFERENCES:
          ! AirToolkit source code

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals,                ONLY : MaxNameLength
    USE DataInterfaces,             ONLY : ShowWarningError, ShowSevereError, ShowFatalError
    USE InputProcessor,             ONLY : SameString

    IMPLICIT NONE         ! Enforce explicit typing of all variables

    PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
    REAL(r64) , PARAMETER                    :: CpAir    = 1005.0d0   ! Specific heat of air
    REAL(r64) , PARAMETER                    :: MinSlope = 0.001d0     ! Bound on result from Mundt model
    REAL(r64) , PARAMETER                    :: MaxSlope = 5.0d0       ! Bound on result from Mundt Model

          ! MODULE DERIVED TYPE DEFINITIONS:
    TYPE DefineLinearModelNode
        CHARACTER(Len=MaxNameLength)    :: AirNodeName    =' '  ! Name of air nodes
        INTEGER                         :: ClassType      =0    ! Type of air nodes
        REAL(r64)                       :: Height         =0.0d0  ! Z coordinates [m] node's Control Vol. center
        REAL(r64)                       :: Temp           =0.0d0  ! Surface temperature BC
        LOGICAL, ALLOCATABLE, DIMENSION(:)  :: SurfMask         ! Limit of 60 surfaces at current sizing
    END TYPE DefineLinearModelNode

    TYPE DefineSurfaceSettings
        REAL(r64)                       :: Area           =0.0d0  !m2
        REAL(r64)                       :: Temp           =0.0d0  !surface temperature BC
        REAL(r64)                       :: Hc             =0.0d0  !convective film coeff BC
        REAL(r64)                       :: TMeanAir       =0.0d0  !effective near-surface air temp from air model solution
    END TYPE DefineSurfaceSettings

    TYPE DefineZoneData
        INTEGER                         :: SurfFirst      =0    ! index for first surface of the zone
        INTEGER                         :: NumOfSurfs     =0    ! number of surfaces in the zone
        INTEGER                         :: MundtZoneIndex =0    ! index for zones using Mundt model
    END TYPE DefineZoneData

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
    TYPE(DefineZoneData), ALLOCATABLE, DIMENSION(:)             :: ZoneData         ! zone data
    TYPE(DefineLinearModelNode), ALLOCATABLE, DIMENSION(:,:)    :: LineNode         ! air nodes
    TYPE(DefineSurfaceSettings), ALLOCATABLE, DIMENSION(:,:)    :: MundtAirSurf     ! surfaces
    TYPE(DefineSurfaceSettings), ALLOCATABLE, DIMENSION(:)      :: FloorSurf        ! floor
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: FloorSurfSetIDs  ! fixed variable for floors
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: TheseSurfIDs     ! temporary working variable
    INTEGER                                                     :: MundtCeilAirID  =0 ! air node index in AirDataManager
    INTEGER                                                     :: MundtFootAirID  =0 ! air node index in AirDataManager
    INTEGER                                                     :: SupplyNodeID    =0 ! air node index in AirDataManager
    INTEGER                                                     :: TstatNodeID     =0 ! air node index in AirDataManager
    INTEGER                                                     :: ReturnNodeID    =0 ! air node index in AirDataManager
    INTEGER                                                     :: NumRoomNodes    =0 ! number of nodes connected to walls
    INTEGER                                                     :: NumFloorSurfs   =0 ! total number of surfaces for floor
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: RoomNodeIDS      ! ids of the first NumRoomNode Air Nodes
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: ID1dSurf         ! numbers used to identify surfaces
    INTEGER                                                     :: MundtZoneNum    =0 ! index of zones using Mundt model
    REAL(r64)                                                   :: ZoneHeight      =0.0d0 ! zone height
    REAL(r64)                                                   :: ZoneFloorArea   =0.0d0 ! zone floor area
    REAL(r64)                                                   :: QventCool       =0.0d0 ! heat gain due to ventilation
    REAL(r64)                                                   :: ConvIntGain     =0.0d0 ! heat gain due to internal gains
    REAL(r64)                                                   :: SupplyAirTemp   =0.0d0 ! supply air temperature
    REAL(r64)                                                   :: SupplyAirVolumeRate =0.0d0 ! supply air volume flowrate
    REAL(r64)                                                   :: ZoneAirDensity  =0.0d0  ! zone air density
    REAL(r64)                                                   :: QsysCoolTot     =0.0d0  ! zone sensible cooling load

          ! SUBROUTINE SPECIFICATIONS FOR MODULE MundtSimMgr

         ! main subsroutine
    PUBLIC ManageMundtModel

        ! Routines for transferring data between surface and air domains
    PRIVATE GetSurfHBDataForMundtModel
    PRIVATE SetSurfHBDataForMundtModel

        ! Routines for actual calculations in Mundt model
    PRIVATE InitMundtModel
    PRIVATE SetupMundtModel
    PRIVATE CalcMundtModel
    PRIVATE SetNodeResult
    PRIVATE SetSurfTmeanAir

    CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE ManageMundtModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chanvit Chantrasrisalai
          !       DATE WRITTEN   July 2003
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   manage the Mundt model

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)             :: ZoneNum      ! index number for the specified zone


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
    LOGICAL, SAVE                   :: FirstTimeFlag = .TRUE.  ! Used for allocating arrays
    LOGICAL :: ErrorsFound

          ! FLOW:

    ! initialize Mundt model data
    IF (FirstTimeFlag) THEN
        CALL  InitMundtModel
        FirstTimeFlag = .FALSE.
    END IF

    ! identify the current zone index for zones using Mundt model
    MundtZoneNum = ZoneData(ZoneNum)%MundtZoneIndex

    ! transfer data from surface domain to air domain for the specified zone
    CALL GetSurfHBDataForMundtModel(ZoneNum)

    ! use the Mundt model only for cooling case
    IF ((SupplyAirVolumeRate.GT.0.0001d0).AND.(QsysCoolTot.GT.0.0001d0)) THEN

        ! setup Mundt model
        ErrorsFound=.false.
        CALL SetupMundtModel(ZoneNum,ErrorsFound)
        IF (ErrorsFound) CALL ShowFatalError('ManageMundtModel: Errors in setting up Mundt Model. '//  &
           'Preceding condition(s) cause termination.')

        ! perform Mundt model calculations
        CALL CalcMundtModel(ZoneNum)

    END IF

    ! transfer data from air domain back to surface domain for the specified zone
    CALL SetSurfHBDataForMundtModel(ZoneNum)

    RETURN

END SUBROUTINE ManageMundtModel

!*****************************************************************************************

SUBROUTINE InitMundtModel

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chanvit Chantrasrisalai
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     initialize Mundt-model variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    USE DataGlobals,                ONLY : NumOfZones
    USE DataInterfaces,             ONLY : SetupOutputVariable
    USE DataRoomAirModel,           ONLY : TotNumOfAirNodes, TotNumOfZoneAirNodes, AirModel, &
                                           AirNode, RoomAirModel_Mundt, MundtRoomAirNode, FloorAirNode
    USE DataSurfaces,               ONLY : Surface
    USE DataHeatBalance,            ONLY : Zone

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER  :: SurfNum              ! index for surfaces
    INTEGER  :: SurfFirst            ! index number for the first surface in the specified zone
    INTEGER  :: NumOfSurfs           ! number of the first surface in the specified zone
    INTEGER  :: NodeNum              ! index for air nodes
    INTEGER  :: ZoneIndex            ! index for zones
    INTEGER  :: NumOfAirNodes        ! total number of nodes in each zone
    INTEGER  :: NumOfMundtZones      ! number of zones using the Mundt model
    INTEGER  :: MundtZoneIndex       ! index for zones using the Mundt model
    INTEGER  :: MaxNumOfSurfs        ! maximum of number of surfaces
    INTEGER  :: MaxNumOfFloorSurfs   ! maximum of number of surfaces
    INTEGER  :: MaxNumOfAirNodes     ! maximum of number of air nodes
    INTEGER  :: MaxNumOfRoomNodes    ! maximum of number of nodes connected to walls
    INTEGER  :: RoomNodesCount       ! number of nodes connected to walls
    INTEGER  :: FloorSurfCount       ! number of nodes connected to walls
    INTEGER  :: AirNodeBeginNum      ! index number of the first air node for this zone
    INTEGER  :: AirNodeNum           ! index for air nodes
    LOGICAL  :: AirNodeFoundFlag     ! flag used for error check
    LOGICAL  :: ErrorsFound          ! true if errors found in init

          ! FLOW:

    ! allocate and initialize zone data
    ALLOCATE (ZoneData(NumOfZones))
    ZoneData%SurfFirst = 0
    ZoneData%NumOfSurfs = 0
    ZoneData%MundtZoneIndex = 0

    ! get zone data
    NumOfMundtZones     = 0
    MaxNumOfSurfs       = 0
    MaxNumOfFloorSurfs  = 0
    MaxNumOfAirNodes    = 0
    MaxNumOfRoomNodes     = 0
    ErrorsFound=.false.
    DO ZoneIndex =1, NumOfZones
        IF (AirModel(ZoneIndex)%AirModelType.EQ.RoomAirModel_Mundt) THEN
            ! find number of zones using the Mundt model
            NumOfMundtZones = NumOfMundtZones + 1
            ! find maximum number of surfaces in zones using the Mundt model
            SurfFirst  = Zone(ZoneIndex)%SurfaceFirst
            NumOfSurfs = Zone(ZoneIndex)%SurfaceLast - SurfFirst + 1
            MaxNumOfSurfs = MAX(MaxNumOfSurfs, NumOfSurfs)
            ! fine maximum number of air nodes in zones using the Mundt model
            NumOfAirNodes = TotNumOfZoneAirNodes(ZoneIndex)
            MaxNumOfAirNodes = MAX(MaxNumOfAirNodes, NumOfAirNodes)
            ! assign zone data
            ZoneData(ZoneIndex)%SurfFirst       = SurfFirst
            ZoneData(ZoneIndex)%NumOfSurfs      = NumOfSurfs
            ZoneData(ZoneIndex)%MundtZoneIndex  = NumOfMundtZones
        END IF
    END DO

    ! allocate and initialize surface and air-node data
    ALLOCATE (ID1dSurf(MaxNumOfSurfs))
    ALLOCATE(TheseSurfIDs(MaxNumOfSurfs))
    ALLOCATE (MundtAirSurf(NumOfMundtZones,MaxNumOfSurfs))
    ALLOCATE (LineNode(NumOfMundtZones,MaxNumOfAirNodes))
    ID1dSurf=(/ (SurfNum, SurfNum = 1, MaxNumOfSurfs) /)
    MundtAirSurf%Area       = 0.0d0
    MundtAirSurf%Temp       = 25.0d0
    MundtAirSurf%Hc         = 0.0d0
    MundtAirSurf%TMeanAir   = 25.0d0
    LineNode%AirNodeName    = ' '
    LineNode%ClassType      = -1
    LineNode%Height         = 0.0d0
    LineNode%Temp           = 25.0d0

    ! get constant data (unchanged over time) for surfaces and air nodes
    DO MundtZoneIndex =1, NumOfMundtZones

        Zone_Loop: DO ZoneIndex =1, NumOfZones

            IF (ZoneData(ZoneIndex)%MundtZoneIndex.EQ.MundtZoneIndex) THEN
                ! get surface data
                DO SurfNum = 1, ZoneData(ZoneIndex)%NumOfSurfs
                    MundtAirSurf(MundtZoneIndex,SurfNum)%Area = Surface(ZoneData(ZoneIndex)%SurfFirst+SurfNum-1)%Area
                ENDDO

                ! get air node data
                RoomNodesCount = 0
                FloorSurfCount = 0
                DO NodeNum = 1, TotNumOfZoneAirNodes(ZoneIndex)

                    ALLOCATE (LineNode(MundtZoneIndex,NodeNum)%SurfMask(ZoneData(ZoneIndex)%NumOfSurfs))

                    IF (NodeNum.EQ.1) THEN
                        AirNodeBeginNum = NodeNum
                    END IF

                    ! error check for debugging
                    IF (AirNodeBeginNum.GT.TotNumOfAirNodes) THEN
                        CALL ShowFatalError('An array bound exceeded. Error in InitMundtModel subroutine of MundtSimMgr.')
                    END IF

                    AirNodeFoundFlag = .FALSE.
                    DO AirNodeNum = AirNodeBeginNum, TotNumOfAirNodes
                        IF (SameString(AirNode(AirNodeNum)%ZoneName,Zone(ZoneIndex)%Name)) THEN
                            LineNode(MundtZoneIndex,NodeNum)%ClassType      = AirNode(AirNodeNum)%ClassType
                            LineNode(MundtZoneIndex,NodeNum)%AirNodeName    = AirNode(AirNodeNum)%Name
                            LineNode(MundtZoneIndex,NodeNum)%Height         = AirNode(AirNodeNum)%Height
                            LineNode(MundtZoneIndex,NodeNum)%SurfMask       = AirNode(AirNodeNum)%SurfMask
                            CALL SetupOutputVariable('Room Air Node Air Temperature [C]', &
                                LineNode(MundtZoneIndex,NodeNum)%Temp,'HVAC','Average',  &
                                LineNode(MundtZoneIndex,NodeNum)%AirNodeName)

                            AirNodeBeginNum  = AirNodeNum + 1
                            AirNodeFoundFlag = .TRUE.


                            EXIT
                        END IF
                    END DO

                    ! error check for debugging
                    IF (.NOT.AirNodeFoundFlag) THEN
                        CALL ShowSevereError('InitMundtModel: Air Node in Zone="'//TRIM(Zone(ZoneIndex)%Name)//'" is not found.')
                        ErrorsFound=.true.
                        CYCLE
                    END IF

                    ! count air nodes connected to walls in each zone
                    IF (LineNode(MundtZoneIndex,NodeNum)%ClassType.EQ.MundtRoomAirNode) THEN
                        RoomNodesCount = RoomNodesCount + 1
                    END IF

                    ! count floors in each zone
                    IF (LineNode(MundtZoneIndex,NodeNum)%ClassType.EQ.FloorAirNode) THEN
                        FloorSurfCount = FloorSurfCount + COUNT(LineNode(MundtZoneIndex,NodeNum)%SurfMask)
                    END IF

                END DO
                ! got data for this zone so exit the zone loop
                IF (AirNodeFoundFlag) THEN
                  EXIT Zone_Loop
                ENDIF

            END IF

        END DO Zone_Loop

        MaxNumOfRoomNodes = MAX(MaxNumOfRoomNodes,RoomNodesCount)
        MaxNumOfFloorSurfs = MAX(MaxNumOfFloorSurfs,FloorSurfCount)

    END DO

    IF (ErrorsFound) CALL ShowFatalError('InitMundtModel: Preceding condition(s) cause termination.')

    ! allocate arrays
    ALLOCATE (RoomNodeIDS(MaxNumOfRoomNodes))
    ALLOCATE(FloorSurfSetIDs(MaxNumOfFloorSurfs))
    ALLOCATE(FloorSurf(MaxNumOfFloorSurfs))

    RETURN

END SUBROUTINE InitMundtModel

!*****************************************************************************************

SUBROUTINE GetSurfHBDataForMundtModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Weixiu Kong
          !       DATE WRITTEN   April 2003
          !       MODIFIED       July 2003 (CC)
          !                      February 2004, fix allocate-deallocate problem (CC)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     map data from surface domain to air domain for each particular zone

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals,                ONLY : NumOfZones
    USE DataLoopNode,               ONLY : Node
    USE DataEnvironment,            ONLY : OutBaroPress
    USE DataHeatBalFanSys,          ONLY : ZoneAirHumRat, MCPI, MAT, SumConvHTRadSys, SysDepZoneLoadsLagged, NonAirSystemResponse
    USE DataHeatBalSurface,         ONLY : TempSurfIn
    USE DataSurfaces,               ONLY : Surface
    USE DataHeatBalance,            ONLY : Zone, HConvIn, ZoneIntGain, RefrigCaseCredit
    USE DataZoneEquipment,          ONLY : ZoneEquipConfig
    USE Psychrometrics,             ONLY : PsyWFnTdpPb,PsyCpAirFnWTdb,PsyRhoAirFnPbTdbW
    USE InternalHeatGains,          ONLY : SumAllInternalConvectionGains, SumAllReturnAirConvectionGains

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)             :: ZoneNum               ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                         :: SurfNum               ! index for surfaces
    INTEGER                         :: NodeNum               ! index for air nodes
    REAL(r64)                       :: SumSysMCp             ! zone sum of air system MassFlowRate*Cp
    REAL(r64)                       :: SumSysMCpT            ! zone sum of air system MassFlowRate*Cp*T
    REAL(r64)                       :: MassFlowRate          ! mass flowrate
    REAL(r64)                       :: NodeTemp              ! node temperature
    REAL(r64)                       :: CpAir                 ! specific heat
    INTEGER                         :: ZoneNode              ! index number for specified zone node
    REAL(r64)                       :: ZoneMassFlowRate      ! zone mass flowrate
    INTEGER                         :: ZoneEquipConfigNum    ! index number for zone equipment configuration
    REAL(r64)                       :: ZoneMult              ! total zone multiplier
    REAL(r64)                       :: RetAirConvGain

          ! FLOW:

    ! determine ZoneEquipConfigNum for this zone
    ZoneEquipConfigNum = ZoneNum
    ! check whether this zone is a controlled zone or not
    IF (.NOT. Zone(ZoneNum)%IsControlled) THEN
      CALL ShowFatalError('Zones must be controlled for Mundt air model. No system serves zone '//TRIM(Zone(ZoneNum)%Name))
      RETURN
    END IF

    ! determine information required by Mundt model
    ZoneHeight = Zone(ZoneNum)%CeilingHeight
    ZoneFloorArea = Zone(ZoneNum)%FloorArea
    ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

    ! supply air flowrate is the same as zone air flowrate
    ZoneNode  = Zone(ZoneNum)%SystemZoneNodeNumber
    ZoneAirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneNum), PsyWFnTdpPb(MAT(ZoneNum), OutBaroPress))
    ZoneMassFlowRate = Node(ZoneNode)%MassFlowRate
    SupplyAirVolumeRate=ZoneMassFlowRate/ZoneAirDensity
    IF (ZoneMassFlowRate.LE.0.0001d0) THEN
        ! system is off
        QsysCoolTot = 0.0d0
    ELSE
        ! determine supply air conditions
        SumSysMCp = 0.0d0
        SumSysMCpT = 0.0d0
        DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
            NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
            MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
            CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
            SumSysMCp = SumSysMCp + MassFlowRate * CpAir
            SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
        END DO
        ! prevent dividing by zero due to zero supply air flow rate
        IF (SumSysMCp.LE.0.0d0) THEN
            SupplyAirTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(1))%Temp
        ELSE
            ! a weighted average of the inlet temperatures
            SupplyAirTemp = SumSysMCpT/SumSysMCp
        END IF
        ! determine cooling load
        CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), MAT(ZoneNum))
        QsysCoolTot = -(SumSysMCpT - ZoneMassFlowRate * CpAir * MAT(ZoneNum))
    END IF
    ! determine heat gains
    CALL SumAllInternalConvectionGains(ZoneNum, ConvIntGain)
    ConvIntGain = ConvIntGain  &
                + SumConvHTRadSys(ZoneNum) &
                + SysDepZoneLoadsLagged(ZoneNum) + NonAirSystemResponse(ZoneNum)/ZoneMult

    ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    ! low or zero)
    IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
      CALL SumAllReturnAirConvectionGains(ZoneNum, RetAirConvGain )
      ConvIntGain = ConvIntGain + RetAirConvGain
    END IF

    QventCool = - MCPI(ZoneNum)*(Zone(ZoneNum)%OutDryBulbTemp-MAT(ZoneNum))

    ! get surface data
    DO SurfNum = 1, ZoneData(ZoneNum)%NumOfSurfs
        MundtAirSurf(MundtZoneNum,SurfNum)%Temp = TempSurfIn(ZoneData(ZoneNum)%SurfFirst+SurfNum-1)
        MundtAirSurf(MundtZoneNum,SurfNum)%Hc   = HConvIn(ZoneData(ZoneNum)%SurfFirst+SurfNum-1)
    ENDDO

  RETURN

END SUBROUTINE GetSurfHBDataForMundtModel

!*****************************************************************************************

SUBROUTINE SetupMundtModel(ZoneNum,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Febraury 2002
          !       RE-ENGINEERED  June 2003, EnergyPlus Implementation (CC)
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

          ! PURPOSE OF THIS SUBROUTINE:
          !   Subroutine must be called once before main model calculation
          !   need to pass some zone characteristics only once
          !   initializes module level variables, collect info from Air Data Manager

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataRoomAirModel
    USE DataHeatBalance,            ONLY : Zone

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: ZoneNum          ! index number for the specified zone
    LOGICAL, INTENT(INOUT)              :: ErrorsFound      ! true if problems setting up model

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                             :: NodeNum          ! index for air nodes
    INTEGER                             :: SurfNum          ! index for surfaces

          ! FLOW:

    ! set up air node ID
    NumRoomNodes    = 0
    DO NodeNum = 1, TotNumOfZoneAirNodes(ZoneNum)
        SELECT CASE (LineNode(MundtZoneNum,NodeNum)%ClassType)
            CASE (InletAirNode) !inlet
                SupplyNodeID  = NodeNum
            CASE (FloorAirNode) ! floor
                MundtFootAirID = NodeNum
            CASE (ControlAirNode) ! thermostat
                TstatNodeID = NodeNum
            CASE (CeilingAirNode) ! ceiling
                MundtCeilAirID = NodeNum
            CASE (MundtRoomAirNode) ! wall
                NumRoomNodes = NumRoomNodes + 1
                RoomNodeIDS(NumRoomNodes) = NodeNum
            CASE (ReturnAirNode) ! return
                ReturnNodeID = NodeNum
            CASE DEFAULT
                CALL ShowSevereError('SetupMundtModel: Non-Standard Type of Air Node for Mundt Model')
                ErrorsFound=.true.
        END SELECT
    END DO

    !  get number of floors in the zone and setup FloorSurfSetIDs
    IF (MundtFootAirID > 0) THEN
      NumFloorSurfs = COUNT(LineNode(MundtZoneNum,MundtFootAirID)%SurfMask)
      FloorSurfSetIDs = PACK(ID1dsurf,LineNode(MundtZoneNum,MundtFootAirID)%SurfMask)
      ! initialize floor surface data (a must since NumFloorSurfs is varied among zones)
      FloorSurf%Temp = 25.0d0
      FloorSurf%Hc   = 0.0d0
      FloorSurf%Area = 0.0d0
      ! get floor surface data
      DO SurfNum = 1, NumFloorSurfs
        FloorSurf(SurfNum)%Temp = MundtAirSurf(MundtZoneNum,FloorSurfSetIDs(SurfNum))%Temp
        FloorSurf(SurfNum)%Hc   = MundtAirSurf(MundtZoneNum,FloorSurfSetIDs(SurfNum))%Hc
        FloorSurf(SurfNum)%Area = MundtAirSurf(MundtZoneNum,FloorSurfSetIDs(SurfNum))%Area
      ENDDO
    ELSE
      CALL ShowSevereError('SetupMundtModel: Mundt model has no FloorAirNode, Zone='//  &
          Trim(Zone(ZoneNum)%Name))
      ErrorsFound=.true.
    ENDIF


    RETURN

END SUBROUTINE SetupMundtModel

!*****************************************************************************************

SUBROUTINE CalcMundtModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   September 2001
          !       RE-ENGINEERED  July 2003, EnergyPlus Implementation (CC)
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

          ! PURPOSE OF THIS SUBROUTINE:
          !   Compute the simplified version of Mundt and store results in Air data Manager
          !   argument passing is plentiful but are IN and nothing out.
          !   these variables are scaler conditions at current HB day,timestep, and iteration
          !   This subroutine is USE'ed by heat balance driver (top level module)

          ! METHODOLOGY EMPLOYED:
          !   apply Mundt's simple model for delta Temp head-foot and update values in Air data manager.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataRoomAirModel,           ONLY : ConvectiveFloorSplit, InfiltratFloorSplit

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
    REAL(r64)                           :: TAirFoot         ! air temperature at the floor
    REAL(r64)                           :: TAirCeil         ! air temperature at the ceiling
    REAL(r64)                           :: TLeaving         ! air temperature leaving zone (= return air temp)
    REAL(r64)                           :: TControlPoint    ! air temperature at thermostat
    REAL(r64)                           :: Slope            ! vertical air temperature gradient (slope) from Mundt equations
    REAL(r64)                           :: QequipConvFloor  ! convective gain at the floor due to internal heat sources
    REAL(r64)                           :: QSensInfilFloor  ! convective gain at the floor due to infiltration
    REAL(r64)                           :: FloorSumHAT      ! sum of hci*area*temp at the floor
    REAL(r64)                           :: FloorSumHA       ! sum of hci*area at the floor
    REAL(r64)                           :: TThisNode        ! dummy variable for air node temp
    INTEGER                             :: NodeNum          ! index for air nodes
    INTEGER                             :: SurfNum          ! index for surfaces
    INTEGER                             :: SurfCounted      ! number of surfaces assciated with an air node

          ! FLOW:

    !   apply floor splits
    QequipConvFloor = ConvectiveFloorSplit(ZoneNum)*ConvIntGain
    QSensInfilfloor = - InfiltratFloorSplit(ZoneNum)*QventCool

    ! Begin computations for Mundt model

    ! do summations for floor surfaces of this zone
    FloorSumHAT = SUM(FloorSurf%Area*FloorSurf%Hc*FloorSurf%Temp)
    FloorSumHA  = SUM(FloorSurf%Area*FloorSurf%Hc)

    ! Eq 2.2 in ASHRAE RP 1222 Final report
    TAirFoot = ((ZoneAirDensity*CpAir*SupplyAirVolumeRate*SupplyAirTemp)+(FloorSumHAT)+QequipConvFloor+QSensInfilfloor) &
                 /((ZoneAirDensity*CpAir*SupplyAirVolumeRate)+(FloorSumHA))

    ! prevent dividing by zero due to zero cooling load (or zero supply air flow rate)
    IF (QsysCoolTot.LE.0.0d0) THEN
        TLeaving = SupplyAirTemp
    ELSE
    ! Eq 2.3 in ASHRAE RP 1222 Final report
        TLeaving = (QsysCoolTot/(ZoneAirDensity*CpAir*SupplyAirVolumeRate))+SupplyAirTemp
    END IF

    ! Eq 2.4 in ASHRAE RP 1222 Final report
    Slope = (TLeaving - TAirFoot)/(LineNode(MundtZoneNum,ReturnNodeID)%Height-LineNode(MundtZoneNum,MundtFootAirID)%Height)
    ! check slope
    IF (Slope > MaxSlope ) THEN
        Slope = MaxSlope
        TAirFoot =  TLeaving - (Slope*(LineNode(MundtZoneNum,ReturnNodeID)%Height-LineNode(MundtZoneNum,MundtFootAirID)%Height))
    END IF
    IF (Slope < MinSlope) THEN ! pretty much vertical
        Slope = MinSlope
        TAirFoot = TLeaving
    END IF

    ! Eq 2.4 in ASHRAE RP 1222 Final report
    TAirCeil    = TLeaving - (Slope*(LineNode(MundtZoneNum,ReturnNodeID)%Height-LineNode(MundtZoneNum,MundtCeilAirID)%Height))

    TControlPoint = TLeaving - (Slope*(LineNode(MundtZoneNum,ReturnNodeID)%Height-LineNode(MundtZoneNum,TstatNodeID)%Height))

    ! determine air node temperatures in this zone
    CALL SetNodeResult(SupplyNodeID,   SupplyAirTemp)
    CALL SetNodeResult(ReturnNodeID,   TLeaving)
    CALL SetNodeResult(MundtCeilAirID, TAirCeil)
    CALL SetNodeResult(MundtFootAirID, TAirFoot)
    CALL SetNodeResult(TstatNodeID,    TControlPoint)

    DO SurfNum = 1, NumFloorSurfs
        CALL SetSurfTmeanAir(FloorSurfSetIDs(SurfNum), TAirFoot)
    END DO

    SurfCounted = COUNT(LineNode(MundtZoneNum,MundtCeilAirID)%SurfMask)
    TheseSurfIDS = PACK(ID1dSurf,LineNode(MundtZoneNum,MundtCeilAirID)%SurfMask)
    DO SurfNum = 1, SurfCounted
        CALL SetSurfTmeanAir(TheseSurfIDS(SurfNum), TAirCeil)
    END DO

    DO NodeNum = 1, NumRoomNodes
        TThisNode = TLeaving - (Slope*(LineNode(MundtZoneNum,ReturnNodeID)%Height -  &
                                LineNode(MundtZoneNum,RoomnodeIDs(NodeNum))%Height))
        CALL SetNodeResult(RoomNodeIDS(NodeNum), TThisNode)
        SurfCounted = COUNT(LineNode(MundtZoneNum,RoomNodeIDs(NodeNum))%SurfMask)
        TheseSurfIDS = PACK(ID1dSurf,LineNode(MundtZoneNum,RoomNodeIDs(NodeNum))%SurfMask)
        DO SurfNum = 1, SurfCounted
            CALL SetSurfTmeanAir(TheseSurfIDS(SurfNum), TThisNode)
        END DO
    END DO

    RETURN

END SUBROUTINE CalcMundtModel

!*****************************************************************************************

SUBROUTINE SetNodeResult(NodeID, TempResult)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   September 2002
          !       RE-ENGINEERED  April 2003, Weixiu Kong, EnergyPlus Implementation
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

          ! PURPOSE OF THIS SUBROUTINE:
          !   provide set routine for reporting results
          !   to AirDataManager from air model

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: NodeID       ! node ID
    REAL(r64),    INTENT(IN)                 :: TempResult   ! temperature for the specified air node

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

    LineNode(MundtZoneNum,NodeID)%Temp = TempResult

    RETURN

END SUBROUTINE SetNodeResult

!*****************************************************************************************

SUBROUTINE SetSurfTmeanAir(SurfID, TeffAir)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   September 2002
          !       RE-ENGINEERED  April 2003, Wiexiu Kong, EnergyPlus Implementation
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)

          ! PURPOSE OF THIS SUBROUTINE:
          !   provide set routine for air model prediction of
          !   effective air for single surface

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)                 :: SurfID   ! surface ID
    REAL(r64),    INTENT(IN)                 :: TeffAir  ! temperature of air node adjacent to the specified surface

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

    MundtAirSurf(MundtZoneNum,SurfID)%TmeanAir = TeffAir

    RETURN

END SUBROUTINE  SetSurfTmeanAir

!*****************************************************************************************

SUBROUTINE SetSurfHBDataForMundtModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chanvit Chantrasrisalai
          !       DATE WRITTEN   July 2003
          !       MODIFIED       February 2004, fix allocate-deallocate problem (CC)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     map data from air domain back to surface domain for each particular zone

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    USE DataLoopNode,               ONLY : Node
    USE DataRoomAirModel,           ONLY : AirModel, DirectCoupling
    USE DataSurfaces,               ONLY : Surface, AdjacentAirTemp, ZoneMeanAirTemp
    USE DataHeatBalance,            ONLY : Zone, TempEffBulkAir
    USE DataZoneEquipment,          ONLY : ZoneEquipConfig
    USE DataHeatBalFanSys,          ONLY : MAT, ZT, TempZoneThermostatSetpoint, TempTstatAir

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)             :: ZoneNum      ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                         :: SurfNum      ! index for surfaces
    INTEGER                         :: SurfFirst    ! index number of the first surface in the zone
    INTEGER                         :: NumOfSurfs   ! number of surfaces in the zone
    INTEGER                         :: ZoneNodeNum  ! index number of the zone node
    REAL(r64)                       :: DeltaTemp    ! dummy variable for temperature difference
    REAL(r64)                       :: TRoomAverage ! dummy variable for mean air temperature
          ! FLOW:

    ! get surface info
    SurfFirst  = ZoneData(ZoneNum)%SurfFirst
    NumOfSurfs = ZoneData(ZoneNum)%NumOfSurfs

    IF ((SupplyAirVolumeRate.GT.0.0001d0).AND.(QsysCoolTot.GT.0.0001d0)) THEN ! Controlled zone when the system is on

        IF (AirModel(ZoneNum)%TempCoupleScheme.EQ.DirectCoupling) THEN
            ! Use direct coupling scheme to report air temperatures back to surface/system domains
            ! a) Bulk air temperatures -> TempEffBulkAir(SurfNum)
            DO SurfNum = 1, NumOfSurfs
                TempEffBulkAir(SurfFirst+SurfNum-1) = MundtAirSurf(MundtZoneNum,SurfNum)%TmeanAir
                ! set flag for reference air temperature
                Surface(SurfFirst+SurfNum-1)%TAirRef = AdjacentAirTemp
            ENDDO
            ! b) Average zone air temperature -> ZT(ZoneNum)
            ! For Mundt model, average room air is the weighted value of floor and ceiling air temps
            TRoomAverage = (LineNode(MundtZoneNum,MundtCeilAirID)%Temp+LineNode(MundtZoneNum,MundtFootAirID)%Temp) / 2
            !ZT(ZoneNum) = TRoomAverage
            ! c) Leaving-zone air temperature -> Node(ZoneNode)%Temp
            ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
            Node(ZoneNodeNum)%Temp = LineNode(MundtZoneNum,ReturnNodeID)%Temp
            ! d) Thermostat air temperature -> TempTstatAir(ZoneNum)
            TempTstatAir(ZoneNum) = LineNode(MundtZoneNum,TstatNodeID)%Temp
        ELSE
            ! Use indirect coupling scheme to report air temperatures back to surface/system domains
            ! a) Bulk air temperatures -> TempEffBulkAir(SurfNum)
            DO SurfNum = 1, NumOfSurfs
                DeltaTemp = MundtAirSurf(MundtZoneNum,SurfNum)%TmeanAir - LineNode(MundtZoneNum,TstatNodeID)%Temp
                TempEffBulkAir(SurfFirst+SurfNum-1) = TempZoneThermostatSetpoint(ZoneNum) + DeltaTemp
                ! set flag for reference air temperature
                Surface(SurfFirst+SurfNum-1)%TAirRef = AdjacentAirTemp
            ENDDO
            ! b) Average zone air temperature -> ZT(ZoneNum)
            ! For Mundt model, average room air is the weighted value of floor and ceiling air temps
            TRoomAverage = (LineNode(MundtZoneNum,MundtCeilAirID)%Temp+LineNode(MundtZoneNum,MundtFootAirID)%Temp) / 2
            DeltaTemp = TRoomAverage - LineNode(MundtZoneNum,TstatNodeID)%Temp
           ! ZT(ZoneNum) = TempZoneThermostatSetpoint(ZoneNum) + DeltaTemp
            ! c) Leaving-zone air temperature -> Node(ZoneNode)%Temp
            ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
            DeltaTemp = LineNode(MundtZoneNum,ReturnNodeID)%Temp - LineNode(MundtZoneNum,TstatNodeID)%Temp
            Node(ZoneNodeNum)%Temp = TempZoneThermostatSetpoint(ZoneNum) + DeltaTemp
            ! d) Thermostat air temperature -> TempTstatAir(ZoneNum)
            TempTstatAir(ZoneNum) = ZT(ZoneNum) ! for indirect coupling, control air temp is equal to mean air temp?
        END IF
        ! set flag to indicate that Mundt model is used for this zone at the present time
        AirModel(ZoneNum)%SimAirModel = .TRUE.
    ELSE    ! Controlled zone when the system is off --> Use the mixing model instead of the Mundt model
        ! Bulk air temperatures -> TempEffBulkAir(SurfNum)
        DO SurfNum = 1, NumOfSurfs
            TempEffBulkAir(SurfFirst+SurfNum-1) = MAT(ZoneNum)
            ! set flag for reference air temperature
            Surface(SurfFirst+SurfNum-1)%TAirRef = ZoneMeanAirTemp
        ENDDO
        ! set flag to indicate that Mundt model is NOT used for this zone at the present time
        AirModel(ZoneNum)%SimAirModel = .FALSE.
    END IF

  RETURN

END SUBROUTINE SetSurfHBDataForMundtModel

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
END MODULE MundtSimMgr
