MODULE DataRoomAirModel     ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Weixiu Kong
          !       DATE WRITTEN   March 2003
          !       MODIFIED       July 2003, CC
          !                      Jan 2004, CC
          !                      Aug 2005, BG -- added structures for user-defined patterns
          !                      June 2008, BG -- revised for system time step history terms
          !                      Aug 2013, Sam Brunswick -- added structures for improved RoomAirModelCrossVent
          !                     
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contain global variables needed in air models

          ! USE STATEMENTS:                       ! UCSD
    USE DataPrecisionGlobals
    USE DataGlobals,        ONLY : MaxNameLength

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC  ! By definition, all variables which are placed in this data-only
            ! module should be available to other modules and routines.  Thus,
            ! all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS
    CHARACTER(len=MaxNameLength), PARAMETER :: cUserDefinedControlObject       = &
                                                 'RoomAir:TemperaturePattern:UserDefined'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternConstGradientObject = &
                                                 'RoomAir:TemperaturePattern:ConstantGradient'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternTwoGradientObject   = &
                                                 'RoomAir:TemperaturePattern:TwoGradient'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternNDHeightObject      = &
                                                 'RoomAir:TemperaturePattern:NondimensionalHeight'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternSurfMapObject       = &
                                                 'RoomAir:TemperaturePattern:SurfaceMapping'


          ! Parameters to indicate room air model selected
    INTEGER, Parameter :: RoomAirModel_UserDefined = 1 ! user defined patterns
    INTEGER, PARAMETER :: RoomAirModel_Mixing    = 2 ! mixing air model
    INTEGER, PARAMETER :: RoomAirModel_Mundt     = 3 ! Mundt nodal model
    INTEGER, PARAMETER :: RoomAirModel_UCSDDV    = 4 ! UCSD Displacement Ventilation model
    INTEGER, PARAMETER :: RoomAirModel_UCSDCV    = 5 ! UCSD-CV
    INTEGER, PARAMETER :: RoomAirModel_UCSDUFI   = 6 ! UCSD UFAD interior zone model
    INTEGER, PARAMETER :: RoomAirModel_UCSDUFE   = 7 ! UCSD UFAD interior zone model
    CHARACTER(len=*), PARAMETER,   &
          DIMENSION(0:7) :: ChAirModel=(/'*Invalid*  ',  &
                                         'UserDefined', &
                                         'Mixing     ',  &
                                         'Mundt      ',  &
                                         'UCSD_DV    ',  &
                                         'UCSD_CV    ',  &
                                         'UCSD_UFI   ',  &
                                         'UCSD_UFE   ' /)

          ! Parameters to indicate air temperature coupling scheme
    INTEGER, PARAMETER :: DirectCoupling    = 1 ! direct coupling scheme
    INTEGER, PARAMETER :: IndirectCoupling  = 2 ! indirect coupling scheme

          ! Parameters to indicate type of air node, which is dependent on air models
    INTEGER, PARAMETER :: InletAirNode      = 0     ! air node at inlet (for Mundt and Rees&Haves Models)
    INTEGER, PARAMETER :: FloorAirNode      = 1     ! air node at floor (for Mundt and Rees&Haves Models)
    INTEGER, PARAMETER :: ControlAirNode    = 2     ! air node at control point (for Mundt Model)
    INTEGER, PARAMETER :: CeilingAirNode    = 3     ! air node at ceiling (for Mundt Model)
    INTEGER, PARAMETER :: MundtRoomAirNode  = 4     ! air node for vertical walls (for Mundt Model)
    INTEGER, PARAMETER :: ReturnAirNode     = 10    ! air node for return (for Mundt and Rees&Haves Models)
    INTEGER, PARAMETER :: PlumeAirNode1     = 2     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: PlumeAirNode2     = 3     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: PlumeAirNode3     = 4     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: PlumeAirNode4     = 5     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode1      = 6     ! air node for vertical walls (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode2      = 7     ! air node for vertical walls (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode3      = 8     ! air node for vertical walls (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode4      = 9     ! air node for vertical walls (for Rees&Haves Model)

     ! user-defined pattern two gradient interplotation modes
    INTEGER, PARAMETER :: OutdoorDrybulbMode = 21 !by outdoor air bulb.
    INTEGER, PARAMETER :: SensibleCoolingMode = 22 !by sensible cooling load
    INTEGER, PARAMETER :: SensibleHeatingMode = 23 !by sensible heating load
    INTEGER, PARAMETER :: ZoneAirTempMode = 24 !by zone air temperature
    INTEGER, PARAMETER :: DeltaOutdoorZone = 25 !by difference between zone and outdoor

     ! user defined temperature pattern types
    INTEGER, PARAMETER :: ConstGradTempPattern  = 31 ! constant gradient in vertical direction
    INTEGER, PARAMETER :: TwoGradInterpPattern  = 32 ! two gradient interpolation
    INTEGER, PARAMETER :: NonDimenHeightPattern = 33 ! non-dimensionalized height
    INTEGER, PARAMETER :: SurfMapTempPattern    = 34 ! arbitrary surface mappings

          ! Parameters to indicate type of control for the UCSD UFAD interior zone model
    ! INTEGER, PARAMETER :: ConsFlow          = 1     ! constant supply air flow
    ! INTEGER, PARAMETER :: VarFlowConsPress  = 2     ! variable supply air flow, constant supply plenum pressure
    ! INTEGER, PARAMETER :: VarFlowVarPress   = 3     ! variable supply air flow, variable supply plenum pressure

    ! parameters to indicate diffuser type
    INTEGER, PARAMETER :: Swirl                 = 1
    INTEGER, PARAMETER :: VarArea               = 2
    INTEGER, PARAMETER :: DisplVent             = 3
    INTEGER, PARAMETER :: LinBarGrille          = 4
    INTEGER, PARAMETER :: Custom                = 5

    ! parameters for comfort calculations
    INTEGER, PARAMETER :: VComfort_Invalid       = -1
    INTEGER, PARAMETER :: VComfort_Jet           = 1
    INTEGER, PARAMETER :: VComfort_Recirculation = 2

          ! DERIVED TYPE DEFINITIONS

    TYPE AirModelData
        CHARACTER(Len=MaxNameLength) :: AirModelName     =' '
        CHARACTER(Len=MaxNameLength) :: ZoneName         =' '
        INTEGER                      :: ZonePtr          =0   ! Pointer to the zone number for this statement
        INTEGER                      :: AirModelType     =RoomAirModel_Mixing   ! 1 = Mixing, 2 = Mundt, 3 = Rees and Haves,
                                                                   ! 4 = UCSDDV, 5 = UCSDCV, -1 = user defined
                                                                   ! 6 = UCSDUFI
        INTEGER                      :: TempCoupleScheme =DirectCoupling   ! 1 = absolute (direct),
                                                              ! 2 = relative air model temperature passing scheme (indirect)
        LOGICAL                      :: SimAirModel      =.false. ! FALSE if Mixing air model is currently used and
                                                                     ! TRUE if other air models are currently used
    END TYPE AirModelData

    ! Air Node Data
    TYPE AirNodeData
        CHARACTER(len=MaxNameLength)   :: Name              =' ' !name
        CHARACTER(len=MaxNameLength)   :: ZoneName          =' '
        INTEGER                        :: ZonePtr           =0   ! Pointer to the zone number for this statement
        INTEGER                        :: ClassType         =0   !depending on type of model
        REAL(r64)                      :: Height            =0.0d0 !height
        LOGICAL, ALLOCATABLE, DIMENSION(:) :: SurfMask           !limit of 60 surfaces at current sizing
    END TYPE AirNodeData

    ! UCSD
    TYPE DVData
        CHARACTER(len=MaxNameLength)   :: ZoneName          =' ' ! Name of zone
        INTEGER                        :: ZonePtr           =0   ! Pointer to the zone number for this statement
        INTEGER                        :: SchedGainsPtr     =-1  ! Schedule for internal gain fraction to occupied zone
        CHARACTER(len=MaxNameLength)   :: SchedGainsName    =' ' ! Gains Schedule name
        REAL(r64)                      :: NumPlumesPerOcc   =1.0d0 ! Effective number of plumes per occupant
        REAL(r64)                      :: ThermostatHeight  =0.0d0 ! Height of thermostat/ temperature control sensor
        REAL(r64)                      :: ComfortHeight     =0.0d0 ! Height at which air temperature is measured for comfort purposes
        REAL(r64)                      :: TempTrigger       =0.0d0 ! Minimum temperature difference between TOC TMX for stratification
    END TYPE DVData

    TYPE CVData
        CHARACTER(len=MaxNameLength)   :: ZoneName          =' ' ! Name of zone
        INTEGER                        :: ZonePtr           =-1   ! Pointer to the zone number for this statement
        INTEGER                        :: SchedGainsPtr     =-1   ! Schedule for internal gain fraction to occupied zone
        CHARACTER(len=MaxNameLength)   :: SchedGainsName    =' ' ! Gains Schedule name
        INTEGER                        :: VforComfort       =VComfort_Invalid   ! Use Recirculation or Jet velocity and temperatures
                                                                 ! for comfort models
    END TYPE CVData

    TYPE CVFlow
        INTEGER(r64)                   :: FlowFlag           =0 ! Equal to 1 if the opening has inflow, else equal to 0.
        REAL(r64)                      :: Width              =0.0d0 ! Width of the opening [m]
        REAL(r64)                      :: Area               =0.0d0 ! Area of the opening [m2]
        REAL(r64)                      :: Fin                =0.0d0 ! Inflow volume flux through the opening [m3/s]
        REAL(r64)                      :: Uin                =0.0d0 ! Inflow air velocity through the opening [m/s]
        REAL(r64)                      :: Vjet               =0.0d0 ! Average maximum jet velocity for the opening [m/s]
        REAL(r64)                      :: Yjet               =0.0d0 ! Y in "Y = aX + b" formula
        REAL(r64)                      :: Ujet               =0.0d0 ! Volume average jet region velocity [m/s]
        REAL(r64)                      :: Yrec               =0.0d0 ! Y in "Y = aX + b" formula
        REAL(r64)                      :: Urec               =0.0d0 ! Area-averaged velocity in the y-z plane with maximum flow [m/s]
        REAL(r64)                      :: YQrec              =0.0d0 ! Y in "Y = aX + b" formula
        REAL(r64)                      :: Qrec               =0.0d0 ! Total flow rate for the recirculation regions in the plane of maximum flow [m3/s]
    END TYPE CVFlow

    TYPE CVDVParameters
      REAL(r64)                        :: Width             =0.0d0
      REAL(r64)                        :: Height            =0.0d0
      Integer                          :: Shadow            =0
      REAL(r64)                        :: Zmin              =0.0d0
      REAL(r64)                        :: Zmax              =0.0d0
    END TYPE CVDVParameters

    TYPE UFIData
      CHARACTER(len=MaxNameLength)   :: ZoneName          =' '   ! Name of zone
      INTEGER    :: ZonePtr           =0     ! Pointer to the zone number for this statement
      INTEGER    :: ZoneEquipPtr      = 0    ! Pointer to zone equip for this UFAD zone
      REAL(r64)  :: DiffusersPerZone  =0.0d0   ! Number of diffusers in this zone
      REAL(r64)  :: PowerPerPlume     =0.0d0   ! Power in each plume [W]
      REAL(r64)  :: DiffArea          =0.0d0   ! Effective area of a diffuser [m2]
      REAL(r64)  :: DiffAngle         =0.0d0   ! angle between diffuser slots and vertical (degrees)
      REAL(r64)  :: HeatSrcHeight     =0.0d0   ! height of heat source above floor [m]
      REAL(r64)  :: ThermostatHeight  =0.0d0   ! Height of thermostat/ temperature control sensor [m]
      REAL(r64)  :: ComfortHeight     =0.0d0   ! Height at which air temperature is measured for
                                             ! comfort purposes [m]
      REAL(r64)  :: TempTrigger       =0.0d0   ! Minimum temperature difference between TOC TMX
                                             ! for stratification [deltaC]
      INTEGER    :: DiffuserType      =0     ! 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
      REAL(r64)  :: TransHeight       =0.0d0   ! user specified transition height [m]
      LOGICAL    :: CalcTransHeight   =.FALSE. ! flag to calc trans height or use user specified input
      REAL(r64)  :: A_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: B_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: C_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: D_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: E_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    END TYPE UFIData
    TYPE UFEData
      CHARACTER(len=MaxNameLength)   :: ZoneName          =' '   ! Name of zone
      INTEGER    :: ZonePtr           =0     ! Pointer to the zone number for this statement
      INTEGER    :: ZoneEquipPtr      = 0    ! Pointer to zone equip for this UFAD zone
      REAL(r64)  :: DiffusersPerZone  =0.0d0   ! Number of diffusers in this zone
      REAL(r64)  :: PowerPerPlume     =0.0d0   ! Power in each plume [W]
      REAL(r64)  :: DiffArea          =0.0d0   ! Effective area of a diffuser [m2]
      REAL(r64)  :: DiffAngle         =0.0d0   ! angle between diffuser slots and vertical (degrees)
      REAL(r64)  :: HeatSrcHeight     =0.0d0   ! height of heat source above floor [m]
      REAL(r64)  :: ThermostatHeight  =0.0d0   ! Height of thermostat/ temperature control sensor [m]
      REAL(r64)  :: ComfortHeight     =0.0d0   ! Height at which air temperature is measured for
                                             ! comfort purposes [m]
      REAL(r64)  :: TempTrigger       =0.0d0   ! Minimum temperature difference between TOC TMX
                                             ! for stratification [deltaC]
      INTEGER    :: DiffuserType      =0     ! 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
      REAL(r64)  :: TransHeight       =0.0d0   ! user specified transition height [m]
      LOGICAL    :: CalcTransHeight   =.FALSE. ! flag to calc trans height or use user specified input
      REAL(r64)  :: A_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: B_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: C_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: D_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: E_Kc              =0.0d0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL(r64)  :: WinWidth          =0.0d0   ! sum of widths of exterior windows in zone
      REAL(r64)  :: NumExtWin         =0.0d0   ! number of exterior windows in the zone
      LOGICAL    :: ShadeDown         =.TRUE. ! signals shade up or down
    END TYPE UFEData
    ! END UCSD

    ! begin NREL RoomAir DERIVED TYPES ******************************************
    TYPE SurfMapPattern ! nested structure in RoomAirPattern
      ! user variables
      CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfName  ! user defined name
      REAL(r64), ALLOCATABLE, DIMENSION(:)     :: DeltaTai   ! (Tai - MAT ) offset from mean air temp
      INTEGER                             :: numSurfs = 0 ! number of surfaces in this pattern
      !calculated and from elsewhere
      INTEGER , ALLOCATABLE, DIMENSION(:) :: SurfID     ! index in HB surface structure array
    END TYPE SurfMapPattern

    Type ConstGradPattern ! nested structure in RoomAirPattern
      !user variables
      CHARACTER(len=MaxNameLength)   :: Name            =' ' !name
      REAL(r64)                      :: Gradient        = 0.0d0 ! value of vertical gradient [C/m]
    END TYPE ConstGradPattern

    TYPE TwoVertGradInterpolPattern ! nested structure in RoomAirPattern
      !user variables
      CHARACTER(len=MaxNameLength)   :: Name           =' ' !name
      REAL(r64)                      :: TstatHeight    = 0.0d0 ! Height of thermostat/ temperature control sensor
      REAL(r64)                      :: TleavingHeight = 0.0d0 ! height of return air node where leaving zone
      REAL(r64)                      :: TexhaustHeight = 0.0d0 ! height of exhaust air node where leaving zone
      REAL(r64)                      :: LowGradient    = 0.0d0 ! lower value of vertical gradient [C/m]
      REAL(r64)                      :: HiGradient     = 0.0d0 ! upper value of vertical gradient [C/m]
      INTEGER                        :: InterpolationMode = 0 ! control for interpolation mode
      REAL(r64)                      :: UpperBoundTempScale = 0.0d0 ! temperature value for HiGradient
      REAL(r64)                      :: LowerBoundTempScale = 0.0d0 ! temperature value for LowGradient
      REAL(r64)                      :: UpperBoundHeatRateScale = 0.0d0 ! load value for HiGradient
      REAL(r64)                      :: LowerBoundHeatRateScale = 0.0d0 ! load value for lowGradient
    END TYPE

    TYPE TempVsHeightPattern   ! to be used as nested structure in RoomAirPattern
      REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZetaPatrn      ! non dimensional height from floor,
      REAL(r64), ALLOCATABLE, DIMENSION(:) :: DeltaTaiPatrn   ! Tai- MAT (TODO, check sign)
    END TYPE TempVsHeightPattern

    TYPE TemperaturePatternStruct !  RoomAirPattern
      CHARACTER(Len=MaxNameLength) :: Name          = ' ' ! unique identifier
      INTEGER                      :: PatrnID       = 0   ! control ID for referencing in Schedules
      INTEGER                      :: PatternMode   = 0   ! Control for what type of calcs in this pattern
      TYPE(ConstGradPattern)           :: GradPatrn    ! Constant gradient pattern
      TYPE(TwoVertGradInterpolPattern) :: TwoGradPatrn ! Two gradient interpolation pattern
      TYPE(TempVsHeightPattern)        :: VertPatrn    ! Vertical gradient profile pattern
      TYPE(SurfMapPattern )            :: MapPatrn     ! Generic Surface map pattern
      REAL(r64)                    :: DeltaTstat    = 0.0d0 ! (Tstat - MAT) offset   deg C
      REAL(r64)                    :: DeltaTleaving = 0.0d0 ! (Tleaving - MAT) deg C
      REAL(r64)                    :: DeltaTexhaust = 0.0d0 ! (Texhaust - MAT) deg C
    END TYPE TemperaturePatternStruct

    TYPE SurfaceAssocNestedStruct
      CHARACTER(Len=MaxNameLength) :: Name = ' ' ! unique identifier
      INTEGER   :: SurfID        = 0    ! id in HB surface structs
      REAL(r64) :: TadjacentAir  = 23.0d0  ! place to put resulting temperature value
      REAL(r64) :: Zeta          = 0.0d0  ! non-dimensional height in zone ot
    END TYPE SurfaceAssocNestedStruct


    TYPE AirPatternInfobyZoneStruct ! becomes AirPatternZoneInfo
      ! user variables
      Logical                      :: IsUsed       = .FALSE. !.true. if user-defined patterns used in zone
      CHARACTER(len=MaxNameLength) :: Name         = ' ' ! Name
      CHARACTER(len=MaxNameLength) :: ZoneName     = ' ' ! Zone name in building
      INTEGER                      :: ZoneID       = 0  ! Index of Zone in Heat Balance
      CHARACTER(len=MaxNameLength) :: AvailSched   = ' ' ! Name of availability schedule
      INTEGER                      :: AvailSchedID = 0  ! index of availability schedule
      CHARACTER(len=MaxNameLength) :: PatternCntrlSched = ' ' !name of schedule that selects pattern
      INTEGER                      :: PatternSchedID = 0 ! index of pattern selecting schedule
      !calculated and from elsewhere
      REAL(r64)                    :: ZoneHeight  = 0.0d0  ! in meters, from Zone%CeilingHeight
      INTEGER                      :: ReturnAirNodeID = 0 ! index in Node array
      INTEGER                      :: ZoneNodeID      = 0 ! index in Node array for this zone
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ExhaustAirNodeID ! indexes in Node array
      REAL(r64)                    :: TairMean  = 23.0d0 ! comes from MAT
      REAL(r64)                    :: Tstat     = 23.0d0 ! temperature for thermostat
      REAL(r64)                    :: Tleaving  = 23.0d0 ! temperature for return air node
      REAL(r64)                    :: Texhaust  = 23.0d0 ! temperature for exhaust air node
      TYPE(SurfaceAssocNestedStruct) , ALLOCATABLE, DIMENSION(:) :: Surf ! nested struct w/ surface info
      INTEGER                      :: totNumSurfs = 0 ! total surfs for this zone
      INTEGER                      :: firstSurfID = 0 ! Index of first surface
      !report
      REAL(R64)                    :: Gradient = 0.0D0 ! result for modeled gradient if using two-gradient interpolation
    END TYPE AirPatternInfobyZoneStruct
    ! end NREL room air derived types*********************************



          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
    TYPE (AirModelData), ALLOCATABLE, DIMENSION(:) :: AirModel
    TYPE (AirNodeData), ALLOCATABLE,  DIMENSION(:) :: AirNode
    TYPE (DVData),      ALLOCATABLE,  DIMENSION(:) :: ZoneUCSDDV      ! UCSD
    TYPE (CVData),      ALLOCATABLE,  DIMENSION(:) :: ZoneUCSDCV
    TYPE (UFIData),     ALLOCATABLE, DIMENSION(:) :: ZoneUCSDUI
    TYPE (UFEData),     ALLOCATABLE, DIMENSION(:) :: ZoneUCSDUE
    INTEGER                               :: TotNumOfAirNodes     = 0
    INTEGER, ALLOCATABLE,  DIMENSION(:)   :: TotNumOfZoneAirNodes
    REAL(r64), ALLOCATABLE,  DIMENSION(:)      :: ConvectiveFloorSplit
    REAL(r64), ALLOCATABLE,  DIMENSION(:)      :: InfiltratFloorSplit
    ! UCSD
    REAL(r64), ALLOCATABLE, DIMENSION  (:)     :: DVHcIn
    INTEGER                                    :: TotUCSDDV            = 0 ! Total number of UCSDDV zones
    LOGICAL,ALLOCATABLE,DIMENSION(:)           :: IsZoneDV           ! Is the air model for the zone UCSDDV?
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTOC               ! Temperature of occupied (lower) zone
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: AvgTempGrad        ! vertical Average Temperature Gradient in the room
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTMX               ! Temperature of the mixing(upper) layer
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: MaxTempGrad        ! maximum Average Temperature Gradient in the room
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: HVACAirTemp        ! HVAC system temperature (DEG C)
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: HVACMassFlow       ! HVAC system mass flow rate (KG/S)
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTFLOOR
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: HeightTransition
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: FracMinFlow
    INTEGER, ALLOCATABLE, DIMENSION(:)         :: ZoneDVMixedFlag
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneDVMixedFlagRep
    LOGICAL, ALLOCATABLE, DIMENSION(:)         :: ZoneAirSystemON
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: TCMF               ! comfort temperature
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneCeilingHeight
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: MATFloor           ! [C] floor level mean air temp
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XMATFloor          ! [C] floor level mean air temp at t minus 1 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM2TFloor          ! [C] floor level mean air temp at t minus 2 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM3TFloor          ! [C] floor level mean air temp at t minus 3 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM4TFloor          ! [C] floor level mean air temp at t minus 4 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXMATFloor        ! [C] floor level mean air temp at t minus 1 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM2TFloor        ! [C] floor level mean air temp at t minus 2 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM3TFloor        ! [C] floor level mean air temp at t minus 3 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM4TFloor        ! [C] floor level mean air temp at t minus 4 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: MATOC              ! [C] occupied mean air temp
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XMATOC             ! [C] occupied mean air temp at t minus 1 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM2TOC             ! [C] occupied mean air temp at t minus 2 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM3TOC             ! [C] occupied mean air temp at t minus 3 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM4TOC             ! [C] occupied mean air temp at t minus 4 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXMATOC           ! [C] occupied mean air temp at t minus 1 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM2TOC           ! [C] occupied mean air temp at t minus 2 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM3TOC           ! [C] occupied mean air temp at t minus 3 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM4TOC           ! [C] occupied mean air temp at t minus 4 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: MATMX              ! [C] mixed (upper) mean air temp
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XMATMX             ! [C] mixed (upper) mean air temp at t minus 1 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM2TMX             ! [C] mixed (upper) mean air temp at t minus 2 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM3TMX             ! [C] mixed (upper) mean air temp at t minus 3 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: XM4TMX             ! [C] mixed (upper) mean air temp at t minus 4 zone time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXMATMX           ! [C] mixed  mean air temp at t minus 1 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM2TMX           ! [C] mixed  mean air temp at t minus 2 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM3TMX           ! [C] mixed  mean air temp at t minus 3 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: DSXM4TMX           ! [C] mixed  mean air temp at t minus 4 system time step
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM1Floor          ! [C] difference equation's Floor air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM2Floor          ! [C] difference equation's Floor air temp at t minus 2
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM3Floor          ! [C] difference equation's Floor air temp at t minus 3
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM1OC             ! [C] difference equation's Occupied air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM2OC             ! [C] difference equation's Occupied air temp at t minus 2
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM3OC             ! [C] difference equation's Occupied air temp at t minus 3
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM1MX             ! [C] difference equation's Mixed  air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM2MX             ! [C] difference equation's Mixed  air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTM3MX             ! [C] difference equation's Mixed  air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: AIRRATFloor
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: AIRRATOC
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: AIRRATMX
    ! Euler and Exact solution algorithms
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Zone1Floor         ! [C] difference equation's Floor air temp at previous dt
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneMXFloor        ! [C] difference equation's Floor air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneM2Floor        ! [C] difference equation's Floor air temp at t minus 2
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Zone1OC            ! [C] difference equation's Occupied air temp at previous dt
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneMXOC           ! [C] difference equation's Occupied air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneM2OC           ! [C] difference equation's Occupied air temp at t minus 2
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Zone1MX            ! [C] difference equation's Mixed  air temp at previous dt
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneMXMX           ! [C] difference equation's Mixed  air temp at t minus 1
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneM2MX           ! [C] difference equation's Mixed  air temp at t minus 2
    ! UCSD-CV
    REAL(r64), ALLOCATABLE, DIMENSION  (:)     :: CVHcIn
    INTEGER                                    :: TotUCSDCV           =0 ! Total number of UCSDDV zones
    LOGICAL,ALLOCATABLE,DIMENSION(:)           :: IsZoneCV            ! Is the air model for the zone UCSDDV?
    REAL(r64),ALLOCATABLE,DIMENSION(:)         :: ZoneCVisMixing      ! Zone set to CV is actually using a mixing model
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTJET               ! Jet Temperatures
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZTREC               ! Recirculation Temperatures
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: RoomOutflowTemp     !Temperature of air flowing out of the room
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: JetRecAreaRatio
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Urec                ! Recirculation region average velocity
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Ujet                ! Jet region average velocity
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Qrec                ! Recirculation zone total flow rate
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Qtot                ! Total volumetric inflow rate through all active aperatures [m3/s]
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: RecInflowRatio      ! Ratio of the recirculation volumetric flow rate to the total inflow flow rate []
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Uhc
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Ain                 ! Inflow aperture area
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Droom               ! CV Zone average length
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Dstar               ! CV Zone average length, wind direction corrected
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Tin                 ! Inflow air temperature
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: TotArea             ! Sum of the areas of all apertures in the zone
    INTEGER, ALLOCATABLE, DIMENSION(:,:)       :: AirflowNetworkSurfaceUCSDCV  ! table for AirflowNetwork surfaces organization
    TYPE (CVFlow), ALLOCATABLE, DIMENSION(:,:) :: CVJetRecFlows       ! Jet and recirculation zone flows and properties
    TYPE (CVDVParameters), ALLOCATABLE,   &
                       DIMENSION(:)            :: SurfParametersCVDV  ! Surface parameters
    INTEGER                                    :: CVNumAirflowNetworkSurfaces =0 ! total number of AirFlowNetwork surfaces.
                                                                      ! Interzone surfaces counts twice.
    REAL(r64),ALLOCATABLE, DIMENSION(:)        :: Rfr                 ! Ration between inflow and recirculation air flows
    REAL(r64),ALLOCATABLE, DIMENSION(:)        :: ZoneCVhasREC        ! Airflow pattern is C(0), CR(1)
    LOGICAL                                    :: UCSDModelUsed = .false.
    LOGICAL                                    :: MundtModelUsed = .false.
    ! UCSD-UF
    INTEGER                                    :: TotUCSDUI           =0 ! total number of UCSDUI zones
    INTEGER                                    :: TotUCSDUE           =0 ! total number of UCSDUE zones
    LOGICAL,ALLOCATABLE,DIMENSION(:)           :: IsZoneUI               ! controls program flow, for interior or exterior UFAD model
    INTEGER,ALLOCATABLE,DIMENSION(:)           :: ZoneUFPtr
    REAL(r64), ALLOCATABLE, DIMENSION  (:)     :: UFHcIn
    INTEGER, ALLOCATABLE, DIMENSION(:)         :: ZoneUFMixedFlag
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneUFMixedFlagRep
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneUFGamma
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneUFPowInPlumes  ! [W]
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ZoneUFPowInPlumesfromWindows  ! [W]
    REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Phi                ! dimensionless measure of occupied subzone temperature

    ! END UCSD

    ! Begin NREL User-defined patterns
    TYPE(TemperaturePatternStruct), DIMENSION(:),              &
                              ALLOCATABLE :: RoomAirPattern  ! user defined patterns ,various types

    TYPE(AirPatternInfobyZoneStruct), DIMENSION(:),             &
                              ALLOCATABLE :: AirPatternZoneInfo !added zone information for user defined patterns
    INTEGER        :: numTempDistContrldZones =0 !count of zones with user-defined patterns
    INTEGER        :: NumAirTempPatterns  =0 !count of all different patterns in input file
    INTEGER        :: NumConstantGradient =0 !count of constant gradient patterns in input
    INTEGER        :: NumTwoGradientInterp=0  !count of two gradient interp patterns in input
    INTEGER        :: NumNonDimensionalHeight=0  !count of ND height profile patterns in input
    INTEGER        :: NumSurfaceMapping  =0 ! count of generic surface map patterns in input

    LOGICAL        :: UserDefinedUsed    = .false. ! true if user-defined model used anywhere
    ! End User-defined patterns


!**********************************************************************************************

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
END MODULE DataRoomAirModel
