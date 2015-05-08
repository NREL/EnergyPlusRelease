MODULE DataAirflowNetwork     ! EnergyPlus Data-Only Module


          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
          !       DATE WRITTEN   Aug. 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS MODULE:
          ! This module should contain the information that is needed to simulate
          ! performance of air distribution system, including pressure, temperature
          ! and moisture levels at each node, and airflow and sensible and latent energy losses
          ! at each element


          ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: MaxNameLength


IMPLICIT NONE   ! Enforce explicit typing of all variables


PUBLIC    ! By definition, all variables which are placed in this data-only
          ! module should be available to other modules and routines.  Thus,
          ! all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: CompTypeNum_DOP=1  ! Detailed large opening component
INTEGER, PARAMETER :: CompTypeNum_SOP=2  ! Simple opening component
INTEGER, PARAMETER :: CompTypeNum_SCR=3  ! Surface crack component
INTEGER, PARAMETER :: CompTypeNum_SEL=4  ! Surface effective leakage ratio component
INTEGER, PARAMETER :: CompTypeNum_PLR=5  ! Distribution system crack component
INTEGER, PARAMETER :: CompTypeNum_DWC=6  ! Distribution system duct component
INTEGER, PARAMETER :: CompTypeNum_CVF=7  ! Distribution system constant volume fan component
INTEGER, PARAMETER :: CompTypeNum_FAN=8  ! Distribution system detailed fan component
INTEGER, PARAMETER :: CompTypeNum_MRR=9  ! Distribution system multiple curve fit power law resistant flow component
INTEGER, PARAMETER :: CompTypeNum_DMP=10 ! Distribution system damper component
INTEGER, PARAMETER :: CompTypeNum_ELR=11 ! Distribution system effective leakage ratio component
INTEGER, PARAMETER :: CompTypeNum_CPD=12 ! Distribution system constant pressure drop component
INTEGER, PARAMETER :: CompTypeNum_COI=13 ! Distribution system coil component
INTEGER, PARAMETER :: CompTypeNum_TMU=14 ! Distribution system terminal unit component
INTEGER, PARAMETER :: CompTypeNum_EXF=15 ! Zone exhaust fan
INTEGER, PARAMETER :: CompTypeNum_HEX=16 ! Distribution system heat exchanger
INTEGER, PARAMETER :: CompTypeNum_HOP=17 ! Horizontal opening component
INTEGER, PARAMETER :: CompTypeNum_RVD=18 ! Reheat VAV terminal damper

! EPlus component Type
INTEGER, PARAMETER :: EPlusTypeNum_SCN=1 ! Supply connection
INTEGER, PARAMETER :: EPlusTypeNum_RCN=2 ! Return connection
INTEGER, PARAMETER :: EPlusTypeNum_RHT=3 ! Reheat terminal
INTEGER, PARAMETER :: EPlusTypeNum_FAN=4 ! Fan
INTEGER, PARAMETER :: EPlusTypeNum_COI=5 ! Heating or cooling coil
INTEGER, PARAMETER :: EPlusTypeNum_HEX=6 ! Heat ecxchanger
INTEGER, PARAMETER :: EPlusTypeNum_RVD=7 ! Reheat VAV terminal damper

! EPlus node type
INTEGER, PARAMETER :: EPlusTypeNum_ZIN=1 ! Zone inlet node
INTEGER, PARAMETER :: EPlusTypeNum_ZOU=2 ! Zone outlet node
INTEGER, PARAMETER :: EPlusTypeNum_SPL=3 ! Splitter node
INTEGER, PARAMETER :: EPlusTypeNum_MIX=4 ! Mixer node
INTEGER, PARAMETER :: EPlusTypeNum_OAN=5 ! Outside air system node
INTEGER, PARAMETER :: EPlusTypeNum_EXT=6 ! OA system inlet node
INTEGER, PARAMETER :: EPlusTypeNum_FIN=7 ! Fan Inlet node
INTEGER, PARAMETER :: EPlusTypeNum_FOU=8 ! Fan Outlet Node
INTEGER, PARAMETER :: EPlusTypeNum_COU=9 ! Coil Outlet Node
INTEGER, PARAMETER :: EPlusTypeNum_HXO=10 ! Heat exchanger Outlet Node
INTEGER, PARAMETER :: EPlusTypeNum_DIN=11 ! Damper Inlet node
INTEGER, PARAMETER :: EPlusTypeNum_DOU=12 ! Damper Outlet Node
INTEGER, PARAMETER :: EPlusTypeNum_SPI=13 ! Splitter inlet Node
INTEGER, PARAMETER :: EPlusTypeNum_SPO=14 ! Splitter Outlet Node

INTEGER, PARAMETER :: iWPCCntr_Input=1
INTEGER, PARAMETER :: iWPCCntr_SurfAvg=2

          ! DERIVED TYPE DEFINITIONS:
TYPE AirflowNetworkSimuProp ! Basic parameters for AirflowNetwork simulation
 CHARACTER(len=MaxNameLength)  :: AirflowNetworkSimuName = ' '  ! Provide a unique object name
 CHARACTER(len=MaxNameLength)  :: Control = 'NoMultizoneOrDistribution'  ! AirflowNetwork control: MULTIZONE WITH DISTRIBUTION,
                                         ! MULTIZONE WITHOUT DISTRIBUTION
                                         ! MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION,
                                         ! and NO MULTIZONE OR DISTRIBUTION
 CHARACTER(len=32)  :: WPCCntr = 'Input'     ! Wind pressure coefficient input control: "SURFACE-AVERAGE CALCULATION", or "INPUT"
 INTEGER  :: iWPCCntr    ! Integer equivalent for WPCCntr field
 CHARACTER(len=MaxNameLength)  :: CpArrayName = ' '  ! CP Array name at WPCCntr = "INPUT"
 CHARACTER(len=10)  :: BldgType = ' '        ! Building type: "LOWRISE" or "HIGHRISE" at WPCCntr = "SURFACE-AVERAGE CALCULATIO"
 CHARACTER(len=15)  :: HeightOption = ' '    ! Height Selection: "ExternalNode" or "OpeningHeight" at WPCCntr = "INPUT"
 INTEGER  :: MaxIteration = 500     ! Maximum number of iteration, defualt 500
 INTEGER  :: InitFlag = 0           ! Initialization flag
 REAL(r64)     :: RelTol = 1.0d-5        ! Relative airflow convergence
 REAL(r64)     :: AbsTol = 1.0d-5        ! Absolute airflow convergence
 REAL(r64)     :: ConvLimit = -0.5d0       ! Convergence acceleration limit
 REAL(r64)     :: MaxPressure = 500.0d0      ! Maximum pressure change in an element [Pa]
 REAL(r64)     :: Azimuth = 0.0d0          ! Azimuth Angle of Long Axis of Building, not used at WPCCntr = "INPUT"
 REAL(r64)     :: AspectRatio = 1.0d0      ! Ratio of Building Width Along Short Axis to Width Along Long Axis
 INTEGER  :: NWind = 0              ! Number of wind directions
 REAL(r64)     :: DiffP = 1.0d-4         ! Minimum pressure difference
 INTEGER  :: ExtLargeOpeningErrCount =0 ! Exterior large opening error count during HVAC system operation
 INTEGER  :: ExtLargeOpeningErrIndex =0 ! Exterior large opening error index during HVAC system operation
 INTEGER  :: OpenFactorErrCount =0   ! Large opening error count at Open factor > 1.0
 INTEGER  :: OpenFactorErrIndex =0   ! Large opening error error index at Open factor > 1.0
 CHARACTER(len=32)  :: InitType = 'ZeroNodePressures' ! Initialization flag type:
                                                        ! "ZeroNodePressures", or "LinearInitializationMethod"
END TYPE AirflowNetworkSimuProp

TYPE MultizoneZoneProp ! Zone information
 CHARACTER(len=MaxNameLength)  :: ZoneName =' '    ! Name of Associated EnergyPlus Thermal Zone
 CHARACTER(len=20)  :: VentControl='NoVent' ! Ventilation Control Mode: "TEMPERATURE", "ENTHALPIC", "CONSTANT", or "NOVENT"
 CHARACTER(len=MaxNameLength)  :: VentSchName =' ' ! Name of ventilation temperature control schedule
 REAL(r64) :: Height =0.0d0             ! Nodal height
 REAL(r64) :: OpenFactor = 1.0d0     ! Limit Value on Multiplier for Modulating Venting Open Factor,
                                     ! Not applicable if Vent Control Mode = CONSTANT or NOVENT
 REAL(r64) :: LowValueTemp = 0.0d0   ! Lower Value on Inside/Outside Temperature Difference for
                                     ! Modulating the Venting Open Factor with temp control
 REAL(r64) :: UpValueTemp = 100.0d0  ! Upper Value on Inside/Outside Temperature Difference for
                                     ! Modulating the Venting Open Factor with temp control
 REAL(r64) :: LowValueEnth = 0.0d0   ! Lower Value on Inside/Outside Temperature Difference for
                                     ! Modulating the Venting Open Factor with Enthalpic control
 REAL(r64) :: UpValueEnth = 300000.0d0  ! Upper Value on Inside/Outside Temperature Difference for
                                     ! Modulating the Venting Open Factor with Enthalpic control
 INTEGER  :: ZoneNum = 0             ! Zone number associated with ZoneName
 INTEGER  :: VentSchNum = 0          ! Zone ventilation schedule number associated with ventilation schedule name
 INTEGER  :: VentCtrNum = 0          ! Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
 CHARACTER(len=MaxNameLength)  :: VentingSchName =' '  ! Name of ventilation temperature control schedule
 INTEGER  :: VentingSchNum = 0       ! Ventilation schedule number
 INTEGER :: ASH55PeopleInd = 0       ! Index of people object with ASH55 comfort calcs for ventilation control
 INTEGER :: CEN15251PeopleInd = 0       ! Index of people object with CEN15251 comfort calcs for ventilation control
END TYPE MultizoneZoneProp

TYPE MultizoneSurfaceProp ! Surface information
 CHARACTER(len=MaxNameLength)  :: SurfName   =' '  ! Name of Associated EnergyPlus surface
 CHARACTER(len=MaxNameLength)  :: OpeningName=' ' ! Name of opening component, either simple or detailed large opening
 CHARACTER(len=MaxNameLength)  :: ExternalNodeName=' ' ! Name of external node, but not used at WPC="INPUT"
 REAL(r64)     :: Factor       =0.0d0       ! Crack Actual Value or Window Open Factor for Ventilation
 INTEGER  :: SurfNum      =0         ! Surface number
 INTEGER  :: NodeNums(2)  =0         ! Positive: Zone numbers; 0: External
 REAL(r64)     :: OpenFactor = 0.0d0        ! Surface factor
 LOGICAL       :: EMSOpenFactorActuated = .false.     ! True if EMS actuation is on
 REAL(r64)     :: EMSOpenFactor = 0.0d0        ! Surface factor value from EMS for override
 REAL(r64)     :: Height =     0.0d0        ! Surface Height
 REAL(r64)     :: Width =      0.0d0        ! Surface width
 REAL(r64)     :: CHeight =    0.0d0        ! Surface central height in z direction
 CHARACTER(len=20)  :: VentControl='ZONELEVEL' ! Ventilation Control Mode: TEMPERATURE, ENTHALPIC, CONSTANT, ZONELEVEL or NOVENT
 CHARACTER(len=MaxNameLength)  :: VentSchName=' ' ! ! Name of ventilation temperature control schedule
 REAL(r64)     :: ModulateFactor  =0.0d0    ! Limit Value on Multiplier for Modulating Venting Open Factor
 REAL(r64)     :: LowValueTemp = 0.0d0      ! Lower Value on Inside/Outside Temperature Difference for
                                            ! Modulating the Venting Open Factor with temp control
 REAL(r64)     :: UpValueTemp = 100.0d0     ! Upper Value on Inside/Outside Temperature Difference for
                                            ! Modulating the Venting Open Factor with temp control
 REAL(r64)     :: LowValueEnth = 0.0d0      ! Lower Value on Inside/Outside Temperature Difference for
                                            ! Modulating the Venting Open Factor with Enthalpic control
 REAL(r64)     :: UpValueEnth = 300000.0d0  ! Upper Value on Inside/Outside Temperature Difference for
                                            ! Modulating the Venting Open Factor with Enthalpic control
 CHARACTER(len=MaxNameLength)  :: VentingSchName =' '  ! Name of ventilation temperature control schedule
 INTEGER  :: VentSchNum = 0          ! Zone ventilation schedule number associated with ventilation schedule name
 INTEGER  :: VentSurfCtrNum = 0      ! Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
 INTEGER  :: VentingSchNum = 0       ! Ventilation schedule number
 INTEGER  :: ZonePtr = 0             ! Pointer to inside face zone
 LOGICAL  :: IndVentControl = .FALSE. ! Individual surface venting control
 INTEGER  :: ExtLargeOpeningErrCount =0 ! Exterior large opening error count during HVAC system operation
 INTEGER  :: ExtLargeOpeningErrIndex =0 ! Exterior large opening error index during HVAC system operation
 INTEGER  :: OpenFactorErrCount =0   ! Large opening error count at Open factor > 1.0
 INTEGER  :: OpenFactorErrIndex =0   ! Large opening error error index at Open factor > 1.0
 REAL(r64)     :: Multiplier   = 1.0d0      ! Window multiplier
 LOGICAL  :: HybridVentClose = .FALSE. ! Hybrid ventilation window close control logical
 LOGICAL  :: HybridCtrlGlobal = .FALSE. ! Hybrid ventilation global control logical
 LOGICAL  :: HybridCtrlMaster = .FALSE. ! Hybrid ventilation global control master
 REAL(r64)     :: WindModifier   = 1.0d0 ! Wind modifier from hybrid ventilation control
END TYPE MultizoneSurfaceProp

TYPE MultizoneCompDetOpeningProp ! Large detailed opening component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of large detailed opening component
 REAL(r64)     :: FlowCoef    = 0.0d0             ! Air Mass Flow Coefficient When Window or Door Is Closed
 REAL(r64)     :: FlowExpo    = 0.0d0             ! Air Mass Flow exponent When Window or Door Is Closed
 CHARACTER(len=32) :: TypeName = 'NONPIVOTED' ! Name of Large vertical opening type
 INTEGER  :: LVOType     = 0               ! Large vertical opening type number
 REAL(r64)     :: LVOValue    = 0.0d0             ! Extra crack length for LVO type 1 with multiple openable parts,
                                           ! or Height of pivoting axis for LVO type 2
 INTEGER  :: NumFac      = 0               ! Number of Opening Factor Values
 REAL(r64)     :: OpenFac1    = 0.0d0             ! Opening factor #1
 REAL(r64)     :: DischCoeff1 = 0.0d0             ! Discharge coefficient for opening factor #1
 REAL(r64)     :: WidthFac1   = 0.0d0             ! Width factor for for Opening factor #1
 REAL(r64)     :: HeightFac1  = 0.0d0             ! Height factor for opening factor #1
 REAL(r64)     :: StartHFac1  = 0.0d0             ! Start height factor for opening factor #1
 REAL(r64)     :: OpenFac2    = 0.0d0             ! Opening factor #2
 REAL(r64)     :: DischCoeff2 = 0.0d0             ! Discharge coefficient for opening factor #2
 REAL(r64)     :: WidthFac2   = 0.0d0             ! Width factor for for Opening factor #2
 REAL(r64)     :: HeightFac2  = 0.0d0             ! Height factor for opening factor #2
 REAL(r64)     :: StartHFac2  = 0.0d0             ! Start height factor for opening factor #2
 REAL(r64)     :: OpenFac3    = 0.0d0             ! Opening factor #3
 REAL(r64)     :: DischCoeff3 = 0.0d0             ! Discharge coefficient for opening factor #3
 REAL(r64)     :: WidthFac3   = 0.0d0             ! Width factor for for Opening factor #3
 REAL(r64)     :: HeightFac3  = 0.0d0             ! Height factor for opening factor #3
 REAL(r64)     :: StartHFac3  = 0.0d0             ! Start height factor for opening factor #3
 REAL(r64)     :: OpenFac4    = 0.0d0             ! Opening factor #4
 REAL(r64)     :: DischCoeff4 = 0.0d0             ! Discharge coefficient for opening factor #4
 REAL(r64)     :: WidthFac4   = 0.0d0             ! Width factor for for Opening factor #4
 REAL(r64)     :: HeightFac4  = 0.0d0             ! Height factor for opening factor #4
 REAL(r64)     :: StartHFac4  = 0.0d0             ! Start height factor for opening factor #4
 REAL(r64)     :: OpenFactor  = 0.0d0             ! Opening factor
 INTEGER  :: WidthErrCount = 0             ! Width error count
 INTEGER  :: WidthErrIndex = 0             ! Width error index
 INTEGER  :: HeightErrCount = 0            ! Height error count
 INTEGER  :: HeightErrIndex = 0            ! Height error index
END TYPE MultizoneCompDetOpeningProp

TYPE MultizoneCompSimpleOpeningProp ! Large simple opening component
 CHARACTER(len=MaxNameLength)  :: Name=' ' ! Name of large simple opening component
 REAL(r64)     :: FlowCoef     = 0.0d0            ! Air Mass Flow Coefficient When Window or Door Is Closed
 REAL(r64)     :: FlowExpo     = 0.0d0            ! Air Mass Flow exponent When Window or Door Is Closed
 REAL(r64)     :: MinRhoDiff   = 0.0d0            ! Minimum density difference for two-way flow
 REAL(r64)     :: DischCoeff   = 0.0d0            ! Discharge coefficient at full opening
 REAL(r64)     :: OpenFactor   = 0.0d0            ! Opening factor
END TYPE MultizoneCompSimpleOpeningProp

TYPE MultizoneCompHorOpeningProp ! Large horizontal opening component
 CHARACTER(len=MaxNameLength)  :: Name=' ' ! Name of large horizontal opening component
 REAL(r64)     :: FlowCoef     = 0.0d0            ! Air Mass Flow Coefficient When Window or Door Is Closed
 REAL(r64)     :: FlowExpo     = 0.0d0            ! Air Mass Flow exponent When Window or Door Is Closed
 REAL(r64)     :: Slope        = 0.0d0            ! Sloping plane angle
 REAL(r64)     :: DischCoeff   = 0.0d0            ! Discharge coefficient at full opening
END TYPE MultizoneCompHorOpeningProp

TYPE MultizoneSurfaceCrackStdCndns ! Surface crack standard conditions
 CHARACTER(len=MaxNameLength)  :: Name =' '    ! Name of standard conditions component
 REAL(r64)     :: StandardT    = 0.0d0            ! Standard temperature for crack data
 REAL(r64)     :: StandardP    = 0.0d0            ! Standard borometric pressure for crack data
 REAL(r64)     :: StandardW    = 0.0d0            ! Standard humidity ratio for crack data
END TYPE MultizoneSurfaceCrackStdCndns

TYPE MultizoneSurfaceCrackProp ! Surface crack component
 CHARACTER(len=MaxNameLength)  :: Name =' '    ! Name of crack component
 CHARACTER(len=MaxNameLength)  :: ExternalNodeNames = ' ' ! Name of external node.Not requird for internal surface
 REAL(r64)     :: FlowCoef     = 0.0d0            ! Air Mass Flow Coefficient When Window or Door Is Closed
 REAL(r64)     :: FlowExpo     = 0.0d0            ! Air Mass Flow exponent When Window or Door Is Closed
 REAL(r64)     :: StandardT    = 0.0d0            ! Standard temperature for crack data
 REAL(r64)     :: StandardP    = 0.0d0            ! Standard borometric pressure for crack data
 REAL(r64)     :: StandardW    = 0.0d0            ! Standard humidity ratio for crack data
END TYPE MultizoneSurfaceCrackProp

TYPE MultizoneSurfaceELAProp ! Surface effective leakage area component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of effective leakage area component
 REAL(r64)     :: ELA          = 0.0d0            ! Effective leakage area
 REAL(r64)     :: DischCoeff   = 0.0d0            ! Discharge coefficient
 REAL(r64)     :: RefDeltaP    = 0.0d0            ! Reference pressure difference
 REAL(r64)     :: FlowExpo     = 0.0d0            ! Air Mass Flow exponent When Window or Door Is Closed
 REAL(r64)     :: TestDeltaP   = 0.0d0            ! Testing pressure difference
 REAL(r64)     :: TestDisCoef  = 0.0d0            ! Testing Discharge coefficient
END TYPE MultizoneSurfaceELAProp

TYPE MultizoneCompExhaustFanProp           ! Zone exhaust fan component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of exhaust fan component
 REAL(r64)     :: FlowRate     = 0.0d0     ! mass flow rate
 INTEGER  :: SchedPtr     = 0              ! Schedule pointer
 REAL(r64)     :: FlowCoef     = 0.0d0     ! Air Mass Flow Coefficient
 REAL(r64)     :: FlowExpo     = 0.0d0     ! Air Mass Flow exponent
 REAL(r64)     :: StandardT    = 0.0d0     ! Standard temperature for crack data
 REAL(r64)     :: StandardP    = 0.0d0     ! Standard borometric pressure for crack data
 REAL(r64)     :: StandardW    = 0.0d0     ! Standard humidity ratio for crack data
 INTEGER  :: InletNode    = 0              ! Inlet node number
 INTEGER  :: OutletNode   = 0              ! Outlet node number
 INTEGER  :: EPlusZoneNum = 0              ! Zone number
END TYPE MultizoneCompExhaustFanProp

TYPE MultizoneExternalNodeProp ! External node properties
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of external node
 CHARACTER(len=MaxNameLength)  :: WPCName = ' ' ! Wind Pressure Coefficient Values Object Name
 REAL(r64)     :: Orien        = 0.0d0     ! Orientation
 REAL(r64)     :: height       = 0.0d0     ! Nodal height
 INTEGER  :: ExtNum       = 0              ! External node number
 INTEGER  :: CPVNum       = 0              ! CP Value number
 INTEGER  :: FacadeNum    = 0              ! Facade number
END TYPE MultizoneExternalNodeProp

TYPE MultizoneCPArrayProp ! CP Array
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of CP array
 INTEGER  :: NumWindDir   = 0              ! Number of wind directions
 REAL(r64), DIMENSION(:), ALLOCATABLE :: WindDir    ! Wind direction
END TYPE MultizoneCPArrayProp

TYPE MultizoneCPValueProp ! CP Value
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of CP Value
 CHARACTER(len=MaxNameLength)  :: CPArrayName = ' ' ! CP array Name
 REAL(r64), DIMENSION(:), ALLOCATABLE :: CPValue   ! CP Value
END TYPE MultizoneCPValueProp

TYPE DisSysNodeProp ! CP Value
 CHARACTER(len=MaxNameLength)  :: Name = ' '       ! Name of node
 CHARACTER(len=MaxNameLength)  :: EPlusName = ' '  ! EnergyPlus node name
 CHARACTER(len=MaxNameLength)  :: EPlusType  = ' ' ! EnergyPlus node type
 REAL(r64) :: Height = 0.0d0                        ! Nodal height
 INTEGER :: EPlusNodeNum = 0                 ! EPlus node number
END TYPE DisSysNodeProp

TYPE DisSysCompLeakProp ! duct leak component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of component leak
 REAL(r64)     :: FlowCoef = 0.0d0                  ! Air Mass Flow Coefficient
 REAL(r64)     :: FlowExpo = 0.0d0                  ! Air Mass Flow exponent
END TYPE DisSysCompLeakProp

TYPE DisSysCompELRProp ! effective leakage ratio component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of component leak
 REAL(r64)     :: ELR      = 0.0d0                  ! Value of effective leakage ratio
 REAL(r64)     :: FlowRate = 0.0d0                  ! Maximum airflow rate
 REAL(r64)     :: RefPres  = 0.0d0                  ! Reference pressure difference
 REAL(r64)     :: FlowExpo = 0.0d0                  ! Air Mass Flow exponent
END TYPE DisSysCompELRProp

TYPE DisSysCompDuctProp ! Duct component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of duct component
 REAL(r64)     :: L           = 0.0d0               ! Duct length [m]
 REAL(r64)     :: D           = 0.0d0               ! Hydrolic diameter [m]
 REAL(r64)     :: A           = 0.0d0               ! Cross section area [m2]
 REAL(r64)     :: Rough       = 0.0d0               ! Surface roughness [m]
 REAL(r64)     :: TurDynCoef  = 0.0d0               ! Turbulent dynamic loss coefficient
 REAL(r64)     :: UThermal    = 0.0d0               ! Overall heat transmittance [W/m2.K]
 REAL(r64)     :: UMoisture   = 0.0d0               ! Overall moisture transmittance [kg/m2]
 REAL(r64)     :: MThermal    = 0.0d0               ! Thermal capacity [J/K]
 REAL(r64)     :: MMoisture   = 0.0d0               ! Mositure capacity [kg]
 REAL(r64)     :: LamDynCoef  = 0.0d0               ! Laminar dynamic loss coefficient
 REAL(r64)     :: LamFriCoef  = 0.0d0               ! Laminar friction loss coefficient
 REAL(r64)     :: InitLamCoef = 0.0d0               ! Coefficient of linear initialization
 REAL(r64)     :: RelRough    = 0.0d0               ! e/D: relative roughness,
 REAL(r64)     :: RelL        = 0.0d0               ! L/D: relative length,
 REAL(r64)     :: g           = 0.0d0               ! 1/sqrt(Darcy friction factor),
 REAL(r64)     :: A1          = 0.0d0               ! 1.14 - 0.868589*ln(e/D),
END TYPE DisSysCompDuctProp

TYPE DisSysCompDamperProp ! Damper component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of damper component
 REAL(r64)     :: LTP         = 0.0d0               ! Value for laminar turbulent transition
 REAL(r64)     :: LamFlow     = 0.0d0               ! Laminar flow coefficient
 REAL(r64)     :: TurFlow     = 0.0d0               ! Turbulent flow coefficient
 REAL(r64)     :: FlowExpo    = 0.0d0               ! Air Mass Flow exponent
 REAL(r64)     :: FlowMin     = 0.0d0               ! Minimum control air mass rate
 REAL(r64)     :: FlowMax     = 0.0d0               ! Maximum control air mass rate
 REAL(r64)     :: A0          = 0.0d0               ! First polynomial coefficient of the control variable (constant coefficient)
 REAL(r64)     :: A1          = 0.0d0               ! Second polynomial coefficient of the control variable (linear coefficient)
 REAL(r64)     :: A2          = 0.0d0               ! Third polynomial coefficient of the control variable (quadratic coefficient)
 REAL(r64)     :: A3          = 0.0d0               ! Fourth polynomial coefficient of the control variable (cubic coefficient)
END TYPE DisSysCompDamperProp

TYPE DisSysCompCVFProp ! Constant volume fan component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of detailed fan component
 REAL(r64)     :: FlowRate    = 0.0d0               ! Air volume flow rate
 REAL(r64)     :: Ctrl        = 0.0d0               ! Control ratio
 INTEGER  :: FanTypeNum  = 0                 ! Fan type: Constant volume or ONOFF
 INTEGER  :: FanIndex    = 0                 ! Fan index
 INTEGER  :: InletNode   = 0                 ! Inlet node number
 INTEGER  :: OutletNode  = 0                 ! Outlet node number
 REAL(r64)     :: MaxAirMassFlowRate = 0.0d0        ! Max Specified MAss Flow Rate of Damper [kg/s]
END TYPE DisSysCompCVFProp

TYPE DisSysCompDetFanProp ! Detailed fan component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of constant volume fan component
 REAL(r64)     :: FlowCoef    = 0.0d0               ! Coefficient for linear initialization
 REAL(r64)     :: FlowExpo    = 0.0d0               ! Turbulent flow coefficient
 REAL(r64)     :: RhoAir      = 0.0d0               ! Reference air density
 REAL(r64)     :: Qfree       = 0.0d0               ! Free delivery flow at P=0
 REAL(r64)     :: Pshut       = 0.0d0               ! Shutoff pressure at Q=0
 REAL(r64)     :: TranRat     = 0.0d0               ! Flow coefficient at laminar/turbulent transition
 INTEGER  :: n                               ! Number of ranges for fan performance curve
 REAL(r64), DIMENSION(:), ALLOCATABLE :: Coeff        ! Coefficients of fan performance curve.
                                             !Each range has a min flow rate and 4 coeffieincts
END TYPE DisSysCompDetFanProp

TYPE DisSysCompCoilProp ! Coil component
 CHARACTER(len=MaxNameLength)  :: Name = ' '       ! Name of coil component
 CHARACTER(len=MaxNameLength)  :: EPlusType = ' '  ! EnergyPlus coil type
 REAL(r64)     :: L           = 0.0d0               ! Air path length
 REAL(r64)     :: D           = 0.0d0               ! Air path hydraulic diameter
END TYPE DisSysCompCoilProp

TYPE DisSysCompHXProp ! Coil component
 CHARACTER(len=MaxNameLength)  :: Name = ' '       ! Name of coil component
 CHARACTER(len=MaxNameLength)  :: EPlusType = ' '  ! EnergyPlus coil type
 REAL(r64)     :: L           = 0.0d0              ! Air path length
 REAL(r64)     :: D           = 0.0d0              ! Air path hydraulic diameter
 LOGICAL  :: CoilParentExists  = .FALSE.           ! Is a coil component
END TYPE DisSysCompHXProp

TYPE DisSysCompTermUnitProp ! Turminal unit component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of coil component
 CHARACTER(len=MaxNameLength)  :: EPlusType = ' '  ! EnergyPlus coil type
 REAL(r64)     :: L           = 0.0d0               ! Air path length
 REAL(r64)     :: D           = 0.0d0               ! Air path hydraulic diameter
 INTEGER       :: DamperInletNode = 0               ! Damper inlet node number
 INTEGER       :: DamperOutletNode = 0              ! Damper outlet node number
END TYPE DisSysCompTermUnitProp

TYPE DisSysCompCPDProp ! Constant pressure drop component
 CHARACTER(len=MaxNameLength)  :: Name = ' ' ! Name of constant pressure drop component
 REAL(r64)     :: A           = 0.0d0               ! cross section area
 REAL(r64)     :: DP          = 0.0d0               ! Pressure difference across the component
END TYPE DisSysCompCPDProp

TYPE DisSysLinkageProp ! Distribution system linkage data
 CHARACTER(len=MaxNameLength)  :: LinkName =' '       ! Name of distribution system linkage
 CHARACTER(len=MaxNameLength),DIMENSION(2)  :: NodeNames  =' '  ! Names of nodes (limited to 2)
 REAL(r64),DIMENSION(2)                          :: NodeHeights=0.0d0  ! Node heights
 CHARACTER(len=MaxNameLength)  :: CompName      =' '  ! Name of element
 INTEGER                       :: CompNum       =0    ! Element Number
 CHARACTER(len=MaxNameLength)  :: ZoneName      =' '  ! Name of zone
 INTEGER                       :: ZoneNum       =0    ! Zone Number
 INTEGER,DIMENSION(2)          :: NodeNums      =0    ! Node numbers
 INTEGER                       :: LinkNum       =0    ! Linkage number
END TYPE DisSysLinkageProp

TYPE AirflowNetworkNodeProp ! AirflowNetwork nodal data
 CHARACTER(len=MaxNameLength)  :: Name = ' '  ! Provide a unique node name
 CHARACTER(len=20)  :: NodeType                = ' ' ! Provide node type "External", "Thermal Zone" or "Other"
 CHARACTER(len=MaxNameLength)  :: EPlusNode= ' ' ! EnergyPlus node name
 REAL(r64)     :: NodeHeight                       =0.0d0  ! Node height [m]
 INTEGER  :: NodeNum                          =0    ! Node number
 INTEGER  :: NodeTypeNum                      =0    ! Node type with integer number
                                                    ! 0: Calculated, 1: Given pressure;
 CHARACTER(len=MaxNameLength)  :: EPlusZoneName = ' ' ! EnergyPlus node name
 INTEGER  :: EPlusZoneNum = 0                       ! E+ zone number
 INTEGER  :: EPlusNodeNum = 0
 INTEGER  :: ExtNodeNum   = 0
 INTEGER  :: EPlusTypeNum = 0
END TYPE AirflowNetworkNodeProp

TYPE AirflowNetworkCompProp ! AirflowNetwork element data
 CHARACTER(len=MaxNameLength)  :: Name  =' ' ! Provide a unique element name
 INTEGER   :: CompTypeNum    =0   ! Provide numeric equivalent for AirflowNetworkCompType
 INTEGER   :: TypeNum        =0   ! Component number under same component type
 INTEGER   :: CompNum        =0   ! General component number
 CHARACTER(len=MaxNameLength)  :: EPlusName  =' ' ! Provide a unique element name
 CHARACTER(len=MaxNameLength)  :: EPlusCompName  =' ' ! Provide EPlus component name or Other
 CHARACTER(len=MaxNameLength)  :: EPlusType  =' ' ! Provide EPlus type, such as terminal reheat, coil, etc. 9/30/03 or Other
 INTEGER    :: EPlusTypeNum  =0   ! Provide EPlus component type
END TYPE AirflowNetworkCompProp

TYPE AirflowNetworkLinkageProp ! AirflowNetwork linkage data
 CHARACTER(len=MaxNameLength)  :: Name   =' '  ! Provide a unique linkage name
 CHARACTER(len=MaxNameLength),DIMENSION(2)  :: NodeNames  =' '  ! Names of nodes (limited to 2)
 REAL(r64),DIMENSION(2)                          :: NodeHeights=0.0d0  ! Node heights
 CHARACTER(len=MaxNameLength)  :: CompName      =' '  ! Name of element
 INTEGER                       :: CompNum       =0    ! Element Number
 CHARACTER(len=MaxNameLength)  :: ZoneName      =' '  ! Name of zone
 INTEGER                       :: ZoneNum       =0    ! Zone Number
 INTEGER,DIMENSION(2)          :: NodeNums      =0    ! Node numbers
 INTEGER                       :: LinkNum       =0    ! Linkage number
 INTEGER                       :: DetOpenNum    =0    ! Large Opening number
 INTEGER                       :: ConnectionFlag = 0  ! Return and supply connection flag
 LOGICAL                       :: VAVTermDamper = .FALSE. ! True if this component is a damper for a VAV terminal
END TYPE AirflowNetworkLinkageProp

TYPE AirflowNetworkNodeSimuData ! Node variable for simulation
 REAL(r64)     :: TZ =0.0d0         ! Temperature [C]
 REAL(r64)     :: WZ =0.0d0         ! Humidity ratio [kg/kg]
 REAL(r64)     :: PZ =0.0d0         ! Pressure [Pa]
 REAL(r64)     :: CO2Z =0.0d0       ! CO2 [ppm]
 REAL(r64)     :: GCZ =0.0d0        ! Generic contaminant [ppm]
END TYPE AirflowNetworkNodeSimuData

TYPE AirflowNetworkLinkSimuData
 REAL(r64)     :: FLOW =     0.0d0  ! Mass flow rate [kg/s]
 REAL(r64)     :: FLOW2 =    0.0d0  ! Mass flow rate [kg/s] for two way flow
 REAL(r64)     :: DP =       0.0d0  ! Pressure difference across a component
 REAL(r64)     :: VolFLOW =  0.0d0  ! Mass flow rate [m3/s]
 REAL(r64)     :: VolFLOW2 = 0.0d0  ! Mass flow rate [m3/s] for two way flow
 REAL(r64)     :: DP1      = 0.0d0
END TYPE AirflowNetworkLinkSimuData

TYPE AirflowNetworkLinkReportData
 REAL(r64)     :: FLOW =     0.0d0  ! Mass flow rate [kg/s]
 REAL(r64)     :: FLOW2 =    0.0d0  ! Mass flow rate [kg/s] for two way flow
 REAL(r64)     :: VolFLOW =  0.0d0  ! Mass flow rate [m^3/s]
 REAL(r64)     :: VolFLOW2 = 0.0d0  ! Mass flow rate [m^3/s] for two way flow
 REAL(r64)     :: FLOWOFF =  0.0d0  ! Mass flow rate during OFF cycle [kg/s]
 REAL(r64)     :: FLOW2OFF = 0.0d0  ! Mass flow rate during OFF cycle [kg/s] for two way flow
 REAL(r64)     :: VolFLOWOFF =0.0d0 ! Mass flow rate during OFF cycle [m^3/s]
 REAL(r64)     :: VolFLOW2OFF=0.0d0 ! Mass flow rate during OFF cycle [m^3/s] for two way flow
 REAL(r64)     :: DP =       0.0d0  ! Average Pressure difference across a component
 REAL(r64)     :: DPON =     0.0d0  ! Pressure difference across a component with fan on
 REAL(r64)     :: DPOFF =    0.0d0  ! Pressure difference across a component with fan off
END TYPE AirflowNetworkLinkReportData

TYPE AirflowNetworkNodeReportData ! Node variable for simulation
 REAL(r64)     :: PZ    =0.0d0         ! Average Pressure [Pa]
 REAL(r64)     :: PZON  =0.0d0         ! Pressure with fan on [Pa]
 REAL(r64)     :: PZOFF =0.0d0         ! Pressure with fan off [Pa]
END TYPE AirflowNetworkNodeReportData

TYPE AirflowNetworkExchangeProp
 REAL(r64) :: MultiZoneSen = 0.0d0
 REAL(r64) :: MultiZoneLat = 0.0d0
 REAL(r64) :: LeakSen = 0.0d0
 REAL(r64) :: LeakLat = 0.0d0
 REAL(r64) :: CondSen = 0.0d0
 REAL(r64) :: DiffLat = 0.0d0
 REAL(r64) :: TotalSen = 0.0d0
 REAL(r64) :: TotalLat = 0.0d0
 REAL(r64) :: SumMCp = 0.0d0
 REAL(r64) :: SumMCpT = 0.0d0
 REAL(r64) :: SumMHr = 0.0d0
 REAL(r64) :: SumMHrW = 0.0d0
 REAL(r64) :: SumMMCp = 0.0d0
 REAL(r64) :: SumMMCpT = 0.0d0
 REAL(r64) :: SumMMHr = 0.0d0
 REAL(r64) :: SumMMHrW = 0.0d0
 REAL(r64) :: SumMHrCO = 0.0d0
 REAL(r64) :: SumMMHrCO = 0.0d0
 REAL(r64) :: TotalCO2 = 0.0d0
 REAL(r64) :: SumMHrGC = 0.0d0
 REAL(r64) :: SumMMHrGC = 0.0d0
 REAL(r64) :: TotalGC = 0.0d0
END TYPE AirflowNetworkExchangeProp

          ! MODULE VARIABLE DECLARATIONS:
! Node simulation variable in air distribution system
TYPE(AirflowNetworkNodeSimuData), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkNodeSimu
! Link simulation variable in air distribution system
TYPE(AirflowNetworkLinkSimuData), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkLinkSimu
! Sensible and latent exchange variable in air distribution system
TYPE(AirflowNetworkExchangeProp), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkExchangeData
TYPE(AirflowNetworkExchangeProp), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkMultiExchangeData
TYPE(AirflowNetworkLinkReportData), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkLinkReport
TYPE(AirflowNetworkNodeReportData), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkNodeReport
TYPE(AirflowNetworkLinkReportData), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkLinkReport1

TYPE (AirflowNetworkSimuProp), SAVE ::   &
   AirflowNetworkSimu=AirflowNetworkSimuProp  &
   (' ',  &       ! unique object name
    'NoMultizoneOrDistribution', & ! AirflowNetwork control
    'Input',  &   ! Wind pressure coefficient input control
    0,  &         ! Integer equivalent for WPCCntr field
    ' ',  &       ! CP Array name at WPCCntr = "INPUT"
    ' ',  &       ! Building type
    ' ',  &       ! Height Selection
    500,  &       ! Maximum number of iteration
    0,  &         ! Initialization flag
    1.0d-5,  &    ! Relative airflow convergence
    1.0d-5,  &    ! Absolute airflow convergence
    -0.5d0,  &    ! Convergence acceleration limit
    500.0d0, &    ! Maximum pressure change in an element [Pa]
    0.0d0,  &     ! Azimuth Angle of Long Axis of Building
    1.0d0,  &     ! Ratio of Building Width Along Short Axis to Width Along Long Axis
    0,  &         ! Number of wind directions
    1.0d-4,  &    ! Minimum pressure difference
    0,  &         ! Exterior large opening error count during HVAC system operation
    0,  &         ! Exterior large opening error index during HVAC system operation
    0,  &         ! Large opening error count at Open factor > 1.0
    0,  &         ! Large opening error error index at Open factor > 1.0
    'ZeroNodePressures') ! Initialization flag type

TYPE (AirflowNetworkNodeProp), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkNodeData
TYPE (AirflowNetworkCompProp), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkCompData
TYPE (AirflowNetworkLinkageProp), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkLinkageData

TYPE(MultizoneZoneProp), ALLOCATABLE, DIMENSION(:) :: MultizoneZoneData
TYPE(MultizoneSurfaceProp), ALLOCATABLE, DIMENSION(:) :: MultizoneSurfaceData
TYPE(MultizoneCompDetOpeningProp), ALLOCATABLE, DIMENSION(:) :: MultizoneCompDetOpeningData
TYPE(MultizoneCompSimpleOpeningProp), ALLOCATABLE, DIMENSION(:) :: MultizoneCompSimpleOpeningData
TYPE(MultizoneCompHorOpeningProp), ALLOCATABLE, DIMENSION(:) :: MultizoneCompHorOpeningData
TYPE(MultizoneSurfaceCrackStdCndns), ALLOCATABLE, DIMENSION(:) :: MultizoneSurfaceStdConditionsCrackData
TYPE(MultizoneSurfaceCrackProp), ALLOCATABLE, DIMENSION(:) :: MultizoneSurfaceCrackData
TYPE(MultizoneSurfaceELAProp), ALLOCATABLE, DIMENSION(:) :: MultizoneSurfaceELAData
TYPE(MultizoneExternalNodeProp), ALLOCATABLE, DIMENSION(:) :: MultizoneExternalNodeData
TYPE(MultizoneCPArrayProp), ALLOCATABLE, DIMENSION(:) :: MultizoneCPArrayData
TYPE(MultizoneCPValueProp), ALLOCATABLE, DIMENSION(:) :: MultizoneCPValueData
TYPE(MultizoneCompExhaustFanProp), ALLOCATABLE, DIMENSION(:) :: MultizoneCompExhaustFanData

TYPE(DisSysNodeProp), ALLOCATABLE, DIMENSION(:) :: DisSysNodeData
TYPE(DisSysCompLeakProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompLeakData
TYPE(DisSysCompELRProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompELRData
TYPE(DisSysCompDuctProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompDuctData
TYPE(DisSysCompDamperProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompDamperData
TYPE(DisSysCompCVFProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompCVFData
TYPE(DisSysCompDetFanProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompDetFanData

TYPE(DisSysCompCoilProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompCoilData
TYPE(DisSysCompHXProp), ALLOCATABLE, DIMENSION(:)   :: DisSysCompHXData
TYPE(DisSysCompTermUnitProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompTermUnitData
TYPE(DisSysCompCPDProp), ALLOCATABLE, DIMENSION(:) :: DisSysCompCPDData

INTEGER :: SimulateAirflowNetwork = 1
! Vent Control  DistSys Control  Flag    Description
!  NONE           NONE           0      No AirflowNetwork and SIMPLE
!  SIMPLE         NONE           1      Simple calculations only
!  MULTIZONE      NONE           2      Perform multizone calculations only
!  NONE           DISTSYS        3      Perform distribution system durin system on time only
!  SIMPLE         DISTSYS        4      Perform distribution system durin system on time and simple calculations during off time
!  MULTIZONE      DISTSYS        5      Perform distribution system durin system on time and multizone calculations during off time

INTEGER, PARAMETER :: AirflowNetworkControlSimple    = 1  ! Simple calculations only
INTEGER, PARAMETER :: AirflowNetworkControlMultizone = 2  ! Perform multizone calculations only
INTEGER, PARAMETER :: AirflowNetworkControlSimpleADS = 4  ! Perform distribution system durin system
                                                          ! on time and simple calculations during off time
INTEGER, PARAMETER :: AirflowNetworkControlMultiADS  = 5  ! Perform distribution system durin system on time
                                                          ! and multizone calculations during off time

LOGICAL, ALLOCATABLE, DIMENSION(:) :: AirflowNetworkZoneFlag

INTEGER :: NumOfNodesMultiZone    = 0    ! Number of nodes for multizone calculation
INTEGER :: NumOfNodesDistribution = 0    ! Number of nodes for distribution system calculation
INTEGER :: NumOfLinksMultiZone    = 0    ! Number of links for multizone calculation
INTEGER :: NumOfLinksDistribution = 0    ! Number of links for distribution system calculation

INTEGER :: AirflowNetworkNumOfNodes = 0  ! Number of nodes for AirflowNetwork calculation
                                         ! = NumOfNodesMultiZone+NumOfNodesDistribution
INTEGER :: AirflowNetworkNumOfComps = 0  ! Number of components for AirflowNetwork calculation
INTEGER :: AirflowNetworkNumOfLinks = 0  ! Number of links for AirflowNetwork calculation
                                         ! = NumOfLinksMultiZone+NumOfLinksDistribution
! RoomAirManager use
INTEGER :: AirflowNetworkNumOfSurfaces = 0 ! The number of surfaces for multizone calculation
INTEGER :: AirflowNetworkNumOfZones    = 0 ! The number of zones for multizone calculation

LOGICAL :: RollBackFlag = .FALSE.          ! Roll back flag when system time steo down shifting
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ANZT    ! Local zone air temperature for roll back use
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ANZW    ! Local zone air humidity ratio for roll back use
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ANCO    ! Local zone air CO2 for roll back use
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ANGC    ! Local zone air generic contaminant for roll back use
INTEGER :: AirflowNetworkNumOfExhFan = 0   ! Number of zone exhaust fans
LOGICAL, ALLOCATABLE, DIMENSION(:) :: AirflowNetworkZoneExhaustFan ! Logical to use zone exhaust fans
LOGICAL :: AirflowNetworkFanActivated = .FALSE. ! Supply fan activation flag
LOGICAL :: AirflowNetworkUnitarySystem = .FALSE. ! set to TRUE for unitary systems (to make answers equal, will remove eventually)
! Multispeed HP only
INTEGER :: MultiSpeedHPIndicator     = 0   ! Indicator for multispeed heat pump use
! Addiitonal airflow needed for an VAV fan to compensate the leakage losses and supply pathway pressure losses [kg/s]
REAL(r64) :: VAVTerminalRatio = 0.d0       ! The terminal flow ratio when a supply VAV fan reach its max flow rate
LOGICAL :: VAVSystem = .FALSE.             ! This flag is used to represent a VAV system

TYPE AiflowNetworkReportProp
 REAL(r64) :: MultiZoneInfiSenGainW =0.0d0
 REAL(r64) :: MultiZoneInfiSenGainJ =0.0d0
 REAL(r64) :: MultiZoneInfiSenLossW =0.0d0
 REAL(r64) :: MultiZoneInfiSenLossJ =0.0d0
 REAL(r64) :: MultiZoneMixSenGainW =0.0d0
 REAL(r64) :: MultiZoneMixSenGainJ =0.0d0
 REAL(r64) :: MultiZoneMixSenLossW =0.0d0
 REAL(r64) :: MultiZoneMixSenLossJ =0.0d0
 REAL(r64) :: MultiZoneInfiLatGainW =0.0d0
 REAL(r64) :: MultiZoneInfiLatGainJ =0.0d0
 REAL(r64) :: MultiZoneInfiLatLossW =0.0d0
 REAL(r64) :: MultiZoneInfiLatLossJ =0.0d0
 REAL(r64) :: MultiZoneMixLatGainW =0.0d0
 REAL(r64) :: MultiZoneMixLatGainJ =0.0d0
 REAL(r64) :: MultiZoneMixLatLossW =0.0d0
 REAL(r64) :: MultiZoneMixLatLossJ =0.0d0
 REAL(r64) :: LeakSenGainW =0.0d0
 REAL(r64) :: LeakSenGainJ =0.0d0
 REAL(r64) :: LeakSenLossW =0.0d0
 REAL(r64) :: LeakSenLossJ =0.0d0
 REAL(r64) :: LeakLatGainW =0.0d0
 REAL(r64) :: LeakLatGainJ =0.0d0
 REAL(r64) :: LeakLatLossW =0.0d0
 REAL(r64) :: LeakLatLossJ =0.0d0
 REAL(r64) :: CondSenGainW =0.0d0
 REAL(r64) :: CondSenGainJ =0.0d0
 REAL(r64) :: CondSenLossW =0.0d0
 REAL(r64) :: CondSenLossJ =0.0d0
 REAL(r64) :: DiffLatGainW =0.0d0
 REAL(r64) :: DiffLatGainJ =0.0d0
 REAL(r64) :: DiffLatLossW =0.0d0
 REAL(r64) :: DiffLatLossJ =0.0d0
 REAL(r64) :: TotalSenGainW =0.0d0
 REAL(r64) :: TotalSenGainJ =0.0d0
 REAL(r64) :: TotalSenLossW =0.0d0
 REAL(r64) :: TotalSenLossJ =0.0d0
 REAL(r64) :: TotalLatGainW =0.0d0
 REAL(r64) :: TotalLatGainJ =0.0d0
 REAL(r64) :: TotalLatLossW =0.0d0
 REAL(r64) :: TotalLatLossJ =0.0d0
END TYPE AiflowNetworkReportProp

TYPE(AiflowNetworkReportProp), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkReportData
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


END MODULE DataAirflowNetwork
