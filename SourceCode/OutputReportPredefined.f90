MODULE OutputReportPredefined

! MODULE INFORMATION:
!    AUTHOR         Jason Glazer of GARD Analytics, Inc.
!    DATE WRITTEN   August 2006 
!    MODIFIED       na
!    RE-ENGINEERED  na
!
! PURPOSE OF THIS MODULE:
!    Support the creation of predefined tabular output.
!
! METHODOLOGY EMPLOYED:
!
!
! REFERENCES:
!    None.
!
! OTHER NOTES:.
!
! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals ,   ONLY : MaxNameLength


INTERFACE PreDefTableEntry
   MODULE PROCEDURE RealPreDefTableEntry, &
                    CharPreDefTableEntry, &
                    IntPreDefTableEntry
END INTERFACE

PUBLIC

! The following section initializes the predefined column heading variables
! The variables get their value in AssignPredefined

! Climate Summary Report
INTEGER :: pdrClim
INTEGER :: pdstDesDay
INTEGER :: pdchDDmaxDB
INTEGER :: pdchDDrange
INTEGER :: pdchDDhumid
INTEGER :: pdchDDhumTyp
INTEGER :: pdchDDwindSp
INTEGER :: pdchDDwindDr
INTEGER :: pdstWthr
INTEGER :: pdchWthrVal

! HVAC Equipment Report
INTEGER :: pdrEquip
INTEGER :: pdstMech
INTEGER :: pdchMechType
INTEGER :: pdchMechNomCap
INTEGER :: pdchMechNomEff
! Fan subtable
INTEGER :: pdstFan
INTEGER :: pdchFanType
INTEGER :: pdchFanTotEff
INTEGER :: pdchFanDeltaP
INTEGER :: pdchFanVolFlow
INTEGER :: pdchFanMotorIn
INTEGER :: pdchFanEndUse
INTEGER :: pdchFanPwr
INTEGER :: pdchFanPwrPerFlow
! Pump subtable
INTEGER :: pdstPump
INTEGER :: pdchPumpType
INTEGER :: pdchPumpControl
INTEGER :: pdchPumpHead
INTEGER :: pdchPumpFlow
INTEGER :: pdchPumpPower
INTEGER :: pdchPumpPwrPerFlow
INTEGER :: pdchMotEff
! Cooling coil subtable
INTEGER :: pdstCoolCoil
INTEGER :: pdchCoolCoilType
INTEGER :: pdchCoolCoilTotCap
INTEGER :: pdchCoolCoilSensCap
INTEGER :: pdchCoolCoilLatCap
INTEGER :: pdchCoolCoilSHR
INTEGER :: pdchCoolCoilNomEff
INTEGER :: pdchCoolCoilUATotal
INTEGER :: pdchCoolCoilArea

! DX Cooling Coil subtable
INTEGER :: pdstDXCoolCoil            !
INTEGER :: pdchDXCoolCoilType        ! DX cooling coil type
INTEGER :: pdchDXCoolCoilSEERSI      ! SEER value in SI unit at AHRI std. 210/240 conditions [W/W]
INTEGER :: pdchDXCoolCoilSEERIP      ! SEER value in IP unit at AHRI std. 210/240 conditions [Btu/W-hr]
INTEGER :: pdchDXCoolCoilNetCap      ! Standard Rated (Net) Cooling Capacity [W]
INTEGER :: pdchDXCoolCoilEERSI       ! EER value in SI unit at AHRI std. 340/360 conditions [W/W]
INTEGER :: pdchDXCoolCoilEERIP       ! EER value in IP unit at AHRI std. 340/360 conditions [W/W]
INTEGER :: pdchDXCoolCoilIEERSI      ! IEER value in SI unit at AHRI std. 340/360 conditions [W/W]
INTEGER :: pdchDXCoolCoilIEERIP      ! IEER value in IP unit at AHRI std. 340/360 conditions [W/W]

! Heating Coil subtable
INTEGER :: pdstHeatCoil
INTEGER :: pdchHeatCoilType
INTEGER :: pdchHeatCoilNomCap
INTEGER :: pdchHeatCoilNomEff
! SWH subtable
INTEGER :: pdstSWH
INTEGER :: pdchSWHType
INTEGER :: pdchSWHVol
INTEGER :: pdchSWHHeatIn
INTEGER :: pdchSWHThEff
INTEGER :: pdchSWHRecEff
INTEGER :: pdchSWHEnFac

! Envelope Report
INTEGER :: pdrEnvelope
INTEGER :: pdstOpaque
INTEGER :: pdchOpCons
INTEGER :: pdchOpRefl
INTEGER :: pdchOpUfactFilm
INTEGER :: pdchOpUfactNoFilm
INTEGER :: pdchOpGrArea
INTEGER :: pdchOpAzimuth
INTEGER :: pdchOpTilt
INTEGER :: pdchOpDir
INTEGER :: pdstFen
INTEGER :: pdchFenCons
INTEGER :: pdchFenAreaOf1
INTEGER :: pdchFenGlassAreaOf1
INTEGER :: pdchFenFrameAreaOf1
INTEGER :: pdchFenDividerAreaOf1
INTEGER :: pdchFenArea
INTEGER :: pdchFenUfact
INTEGER :: pdchFenSHGC
INTEGER :: pdchFenVisTr
INTEGER :: pdchFenFrameConductance
INTEGER :: pdchFenDividerConductance
INTEGER :: pdchFenSwitchable
INTEGER :: pdchFenParent
INTEGER :: pdchFenAzimuth
INTEGER :: pdchFenDir
INTEGER :: pdstDoor
INTEGER :: pdchDrCons
INTEGER :: pdchDrUfactFilm
INTEGER :: pdchDrUfactNoFilm
INTEGER :: pdchDrGrArea
INTEGER :: pdchDrParent
INTEGER :: pdstIntFen
INTEGER :: pdchIntFenCons
INTEGER :: pdchIntFenAreaOf1
! Include these if interzone windows ever get frame and dividers
!INTEGER :: pdchIntFenGlassAreaOf1
!INTEGER :: pdchIntFenFrameAreaOf1
!INTEGER :: pdchIntFenDividerAreaOf1
!INTEGER :: pdchIntFenFrameConductance
!INTEGER :: pdchIntFenDividerConductance
INTEGER :: pdchIntFenArea
INTEGER :: pdchIntFenUfact
INTEGER :: pdchIntFenSHGC
INTEGER :: pdchIntFenVisTr
INTEGER :: pdchIntFenParent


! Shading Report
INTEGER :: pdrShading
INTEGER :: pdstSunlitFrac
INTEGER :: pdchSlfMar21_9
INTEGER :: pdchSlfMar21_12
INTEGER :: pdchSlfMar21_15
INTEGER :: pdchSlfJun21_9
INTEGER :: pdchSlfJun21_12
INTEGER :: pdchSlfJun21_15
INTEGER :: pdchSlfDec21_9
INTEGER :: pdchSlfDec21_12
INTEGER :: pdchSlfDec21_15
INTEGER :: pdstWindowControl
INTEGER :: pdchWscName
INTEGER :: pdchWscShading
INTEGER :: pdchWscShadCons
INTEGER :: pdchWscControl
INTEGER :: pdchWscGlare

! Lighting Report
INTEGER :: pdrLighting
INTEGER :: pdstInLite
INTEGER :: pdchInLtZone
INTEGER :: pdchInLtDens
INTEGER :: pdchInLtArea
INTEGER :: pdchInLtPower
INTEGER :: pdchInLtEndUse
INTEGER :: pdchInLtSchd
INTEGER :: pdchInLtAvgHrSchd
INTEGER :: pdchInLtAvgHrOper
INTEGER :: pdchInLtFullLoadHrs
INTEGER :: pdchInLtRetAir
INTEGER :: pdchInLtCond
INTEGER :: pdchInLtConsump
INTEGER :: pdstExtLite
INTEGER :: pdchExLtPower
INTEGER :: pdchExLtClock
INTEGER :: pdchExLtSchd
INTEGER :: pdchExLtAvgHrSchd
INTEGER :: pdchExLtAvgHrOper
INTEGER :: pdchExLtFullLoadHrs
INTEGER :: pdchexLtConsump
INTEGER :: pdstDaylight
INTEGER :: pdchDyLtZone
INTEGER :: pdchDyLtKind
INTEGER :: pdchDyLtCtrl
INTEGER :: pdchDyLtFrac
INTEGER :: pdchDyLtWInst
INTEGER :: pdchDyLtWCtrl

! Sizing Report
INTEGER :: pdrSizing
INTEGER :: pdstZoneClSize
INTEGER :: pdchZnClCalcDesLd
INTEGER :: pdchZnClUserDesLd
INTEGER :: pdchZnClCalcDesAirFlow
INTEGER :: pdchZnClUserDesAirFlow
INTEGER :: pdchZnClDesDay
INTEGER :: pdchZnClPkTime
INTEGER :: pdchZnClPkTemp
INTEGER :: pdchZnClPkHum
INTEGER :: pdstZoneHtSize
INTEGER :: pdchZnHtCalcDesLd
INTEGER :: pdchZnHtUserDesLd
INTEGER :: pdchZnHtCalcDesAirFlow
INTEGER :: pdchZnHtUserDesAirFlow
INTEGER :: pdchZnHtDesDay
INTEGER :: pdchZnHtPkTime
INTEGER :: pdchZnHtPkTemp
INTEGER :: pdchZnHtPkHum
INTEGER :: pdstSystemSize
INTEGER :: pdchSysSizCalcClAir
INTEGER :: pdchSysSizUserClAir
INTEGER :: pdchSysSizCalcHtAir
INTEGER :: pdchSysSizUserHtAir

!System summary
INTEGER :: pdrSystem
INTEGER :: pdstEconomizer
INTEGER :: pdchEcoKind
INTEGER :: pdchEcoMinOA
INTEGER :: pdchEcoMaxOA
INTEGER :: pdchEcoRetTemp
INTEGER :: pdchEcoRetEnth
INTEGER :: pdchEcoOATempLim
INTEGER :: pdchEcoOAEnthLim
INTEGER :: pdstDemCntlVent
INTEGER :: pdchDCVventMechName
INTEGER :: pdchDCVperPerson
INTEGER :: pdchDCVperArea

!added for new DCV
INTEGER :: pdchDCVZoneADEffCooling
INTEGER :: pdchDCVZoneADEffHeating
INTEGER :: pdchDCVZoneADEffSchName

INTEGER :: pdstSimpleComfort
INTEGER :: pdchSCwinterClothes
INTEGER :: pdchSCsummerClothes
INTEGER :: pdchSCeitherClothes
INTEGER :: pdstUnmetLoads
INTEGER :: pdchULnotMetHeat
INTEGER :: pdchULnotMetCool
INTEGER :: pdchULnotMetHeatOcc
INTEGER :: pdchULnotMetCoolOcc

! Outside Air Report
INTEGER :: pdrOutsideAir
INTEGER :: pdstOAavgOcc
INTEGER :: pdchOaoAvgNumOcc1
INTEGER :: pdchOaoNomNumOcc1
INTEGER :: pdchOaoZoneVol1
INTEGER :: pdchOaoAvgMechVent
INTEGER :: pdchOaoAvgInfil
INTEGER :: pdchOaoAvgSimpVent
INTEGER :: pdchOaoAvgTotVent
INTEGER :: pdstOAminOcc
INTEGER :: pdchOaoAvgNumOcc2
INTEGER :: pdchOaoNomNumOcc2
INTEGER :: pdchOaoZoneVol2
INTEGER :: pdchOaoMinMechVent
INTEGER :: pdchOaoMinInfil
INTEGER :: pdchOaoMinSimpVent
INTEGER :: pdchOaoMinTotVent

! Object Count Report
INTEGER :: pdrObjCnt
INTEGER :: pdstSurfCnt
INTEGER :: pdchSurfCntTot
INTEGER :: pdchSurfCntExt
INTEGER :: pdstHVACcnt
INTEGER :: pdchHVACcntVal
INTEGER :: pdstFieldCnt
INTEGER :: pdchFieldCntVal

! Sensible Heat Gas Component Report
INTEGER :: pdrSensibleGain
!annual
INTEGER :: pdstSHGSannual
INTEGER :: pdchSHGSAnHvacHt
INTEGER :: pdchSHGSAnHvacCl
INTEGER :: pdchSHGSAnSurfHt
INTEGER :: pdchSHGSAnSurfCl
INTEGER :: pdchSHGSAnPeoplAdd
INTEGER :: pdchSHGSAnLiteAdd
INTEGER :: pdchSHGSAnEquipAdd
INTEGER :: pdchSHGSAnWindAdd
INTEGER :: pdchSHGSAnIzaAdd
INTEGER :: pdchSHGSAnInfilAdd
INTEGER :: pdchSHGSAnOtherAdd
INTEGER :: pdchSHGSAnEquipRem
INTEGER :: pdchSHGSAnWindRem
INTEGER :: pdchSHGSAnIzaRem
INTEGER :: pdchSHGSAnInfilRem
INTEGER :: pdchSHGSAnOtherRem
!peak cooling
INTEGER :: pdstSHGSpkCl
INTEGER :: pdchSHGSClTimePeak
INTEGER :: pdchSHGSClHvacHt
INTEGER :: pdchSHGSClHvacCl
INTEGER :: pdchSHGSClSurfHt
INTEGER :: pdchSHGSClSurfCl
INTEGER :: pdchSHGSClPeoplAdd
INTEGER :: pdchSHGSClLiteAdd
INTEGER :: pdchSHGSClEquipAdd
INTEGER :: pdchSHGSClWindAdd
INTEGER :: pdchSHGSClIzaAdd
INTEGER :: pdchSHGSClInfilAdd
INTEGER :: pdchSHGSClOtherAdd
INTEGER :: pdchSHGSClEquipRem
INTEGER :: pdchSHGSClWindRem
INTEGER :: pdchSHGSClIzaRem
INTEGER :: pdchSHGSClInfilRem
INTEGER :: pdchSHGSClOtherRem
!peak heating
INTEGER :: pdstSHGSpkHt
INTEGER :: pdchSHGSHtTimePeak
INTEGER :: pdchSHGSHtHvacHt
INTEGER :: pdchSHGSHtHvacCl
INTEGER :: pdchSHGSHtSurfHt
INTEGER :: pdchSHGSHtSurfCl
INTEGER :: pdchSHGSHtPeoplAdd
INTEGER :: pdchSHGSHtLiteAdd
INTEGER :: pdchSHGSHtEquipAdd
INTEGER :: pdchSHGSHtWindAdd
INTEGER :: pdchSHGSHtIzaAdd
INTEGER :: pdchSHGSHtInfilAdd
INTEGER :: pdchSHGSHtOtherAdd
INTEGER :: pdchSHGSHtEquipRem
INTEGER :: pdchSHGSHtWindRem
INTEGER :: pdchSHGSHtIzaRem
INTEGER :: pdchSHGSHtInfilRem
INTEGER :: pdchSHGSHtOtherRem

! Internal data structures to store information provided by calls

INTEGER, PARAMETER :: sizeIncrement = 100

TYPE reportNameType
  CHARACTER(len=MaxNameLength) :: name = ''
  CHARACTER(len=MaxNameLength) :: abrev = ''
  LOGICAL                      :: show = .FALSE.
END TYPE
TYPE (reportNameType),ALLOCATABLE, DIMENSION(:)   :: reportName
TYPE (reportNameType),ALLOCATABLE, DIMENSION(:)   :: reportNameCopy
INTEGER                                           :: sizeReportName
INTEGER                                           :: numReportName

TYPE SubTableType
  CHARACTER(len=MaxNameLength)  :: name      = ''
  INTEGER                       :: indexReportName  = 0
  CHARACTER(len=MaxNameLength)  :: footnote  = ''
END TYPE
TYPE (SubTableType), ALLOCATABLE, DIMENSION(:) :: subTable
TYPE (SubTableType), ALLOCATABLE, DIMENSION(:) :: subTableCopy
INTEGER                                        :: sizeSubTable
INTEGER                                        :: numSubTable

TYPE ColumnTagType
  CHARACTER(len=MaxNameLength)  :: heading      = ''
  INTEGER                       :: indexSubTable = 0
END TYPE
TYPE (ColumnTagType), ALLOCATABLE, DIMENSION(:)   :: columnTag
TYPE (ColumnTagType), ALLOCATABLE, DIMENSION(:)   :: columnTagCopy
INTEGER                                           :: sizeColumnTag
INTEGER                                           :: numColumnTag

TYPE TableEntryType
  CHARACTER(len=MaxNameLength)  :: charEntry     = ''
  CHARACTER(len=MaxNameLength)  :: objectName    = ''
  INTEGER                       :: indexColumn   = 0
  INTEGER                       :: subTableIndex = 0
  INTEGER                       :: uniqueObjName = 0
  REAL(r64)                     :: origRealEntry = 0.0
  INTEGER                       :: significantDigits = 0
  LOGICAL                       :: origEntryIsReal = .FALSE.
END TYPE
TYPE (TableEntryType), ALLOCATABLE, DIMENSION(:) :: tableEntry
TYPE (TableEntryType), ALLOCATABLE, DIMENSION(:) :: tableEntryCopy
INTEGER                                          :: sizeTableEntry
INTEGER                                          :: numTableEntry

TYPE CompSizeTableEntryType
  CHARACTER(len=MaxNameLength)  :: typeField     = ''
  CHARACTER(len=MaxNameLength)  :: nameField     = ''
  CHARACTER(len=MaxNameLength)  :: description   = ''
  REAL(r64)                     :: valField      = 0.0
  LOGICAL                       :: active        = .false.
  LOGICAL                       :: written       = .false.
END TYPE
TYPE (CompSizeTableEntryType), ALLOCATABLE, DIMENSION(:) :: CompSizeTableEntry
TYPE (CompSizeTableEntryType), ALLOCATABLE, DIMENSION(:) :: CompSizeTableEntryCopy
INTEGER                                          :: sizeCompSizeTableEntry
INTEGER                                          :: numCompSizeTableEntry

TYPE ShadowRelateType
!  CHARACTER(len=MaxNameLength)  :: castSurf          = ''
!  CHARACTER(len=MaxNameLength)  :: recSurf           = ''
  INTEGER  :: castSurf          = 0
  INTEGER  :: recSurf           = 0
  INTEGER                       :: recKind           = 0
END TYPE
TYPE (ShadowRelateType), ALLOCATABLE, DIMENSION(:) :: ShadowRelate
TYPE (ShadowRelateType), ALLOCATABLE, DIMENSION(:) :: ShadowRelateCopy
INTEGER                                           :: sizeShadowRelate
INTEGER                                           :: numShadowRelate
INTEGER, PARAMETER                                :: recKindSurface = 1
INTEGER, PARAMETER                                :: recKindSubsurface = 2

REAL(r64) :: TotalNotMetHeatingOccupiedForABUPS = 0.0
REAL(r64) :: TotalNotMetCoolingOccupiedForABUPS = 0.0
REAL(r64) :: TotalTimeNotSimpleASH55EitherForABUPS = 0.0

CONTAINS

SUBROUTINE SetPredefinedTables
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates the structure of the predefined reports
          !   including the name and abreviation of the report
          !   the subtables involved and the column headings.
          !   The variables defined for the columns are then
          !   used throughout the program to assign values
          !   to the subtables.

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

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

! Climate Summary Report
pdrClim = newPreDefReport('ClimaticDataSummary','Clim')

pdstDesDay = newPreDefSubTable(pdrClim,'SizingPeriod:DesignDay')

pdchDDmaxDB =  newPreDefColumn(pdstDesDay,'Maximum Dry Bulb [C]')
pdchDDrange =  newPreDefColumn(pdstDesDay,'Daily Temperature Range [deltaC]')
pdchDDhumid =  newPreDefColumn(pdstDesDay,'Humidity Value')
pdchDDhumTyp = newPreDefColumn(pdstDesDay,'Humidity Type')
pdchDDwindSp = newPreDefColumn(pdstDesDay,'Wind Speed [m/s]')
pdchDDwindDr = newPreDefColumn(pdstDesDay,'Wind Direction')

pdstWthr = newPreDefSubTable(pdrClim,'Weather Statistics File')
pdchWthrVal = newPreDefColumn(pdstWthr,'Value')

! Envelope Report

pdrEnvelope =   newPreDefReport('EnvelopeSummary','Env')

pdstOpaque =    newPreDefSubTable(pdrEnvelope,'Opaque Exterior')

pdchOpCons =    newPreDefColumn(pdstOpaque,'Construction')
pdchOpRefl =    newPreDefColumn(pdstOpaque,'Reflectance')
pdchOpUfactFilm =   newPreDefColumn(pdstOpaque,'U-Factor with Film [W/m2-K]')
pdchOpUfactNoFilm = newPreDefColumn(pdstOpaque,'U-Factor no Film [W/m2-K]')
pdchOpGrArea =  newPreDefColumn(pdstOpaque,'Gross Area [m2]')
pdchOpAzimuth = newPreDefColumn(pdstOpaque,'Azimuth [deg]')
pdchOpTilt =    newPreDefColumn(pdstOpaque,'Tilt [deg]')
pdchOpDir =     newPreDefColumn(pdstOpaque,'Cardinal Direction')

pdstFen =    newPreDefSubTable(pdrEnvelope,'Exterior Fenestration')

pdchFenCons =   newPreDefColumn(pdstFen,'Construction')
pdchFenGlassAreaOf1 =   newPreDefColumn(pdstFen,'Glass Area [m2]')
pdchFenFrameAreaOf1 =   newPreDefColumn(pdstFen,'Frame Area [m2]')
pdchFenDividerAreaOf1 =   newPreDefColumn(pdstFen,'Divider Area [m2]')
pdchFenAreaOf1 =   newPreDefColumn(pdstFen,'Area of One Opening [m2]')
pdchFenArea =   newPreDefColumn(pdstFen,'Area of Multiplied Openings [m2]')
pdchFenUfact =  newPreDefColumn(pdstFen,'Glass U-Factor [W/m2-K]')
pdchFenSHGC =   newPreDefColumn(pdstFen,'Glass SHGC')
pdchFenVisTr =  newPreDefColumn(pdstFen,'Glass Visible Transmittance')
pdchFenFrameConductance =  newPreDefColumn(pdstFen,'Frame Conductance [W/m2-K]')
pdchFenDividerConductance =  newPreDefColumn(pdstFen,'Divider Conductance [W/m2-K]')
pdchFenSwitchable = newPreDefColumn(pdstFen,'Shade Control')
pdchFenParent = newPreDefColumn(pdstFen,'Parent Surface')
pdchFenAzimuth =    newPreDefColumn(pdstFen,'Azimuth [deg]')
pdchFenDir =     newPreDefColumn(pdstFen,'Cardinal Direction')

pdstIntFen =    newPreDefSubTable(pdrEnvelope,'Interior Fenestration')

pdchIntFenCons =   newPreDefColumn(pdstIntFen,'Construction')
pdchIntFenAreaOf1 =   newPreDefColumn(pdstIntFen,'Area of One Opening [m2]')
pdchIntFenArea =   newPreDefColumn(pdstIntFen,'Area of Openings [m2]')
pdchIntFenUfact =  newPreDefColumn(pdstIntFen,'Glass U-Factor [W/m2-K]')
pdchIntFenSHGC =   newPreDefColumn(pdstIntFen,'Glass SHGC')
pdchIntFenVisTr =  newPreDefColumn(pdstIntFen,'Glass Visible Transmittance')
!pdchIntFenGlassAreaOf1 =   newPreDefColumn(pdstIntFen,'Glass Area [m2]')
!pdchIntFenFrameAreaOf1 =   newPreDefColumn(pdstIntFen,'Frame Area [m2]')
!pdchIntFenDividerAreaOf1 =   newPreDefColumn(pdstIntFen,'Divider Area [m2]')
!pdchIntFenFrameConductance =  newPreDefColumn(pdstIntFen,'Frame Conductance [W/m2-K]')
!pdchIntFenDividerConductance =  newPreDefColumn(pdstIntFen,'Divider Conductance [W/m2-K]')
pdchIntFenParent = newPreDefColumn(pdstIntFen,'Parent Surface')

pdstDoor =    newPreDefSubTable(pdrEnvelope,'Exterior Door')

pdchDrCons =    newPreDefColumn(pdstDoor,'Construction')
pdchDrUfactFilm =   newPreDefColumn(pdstDoor,'U-Factor with Film [W/m2-K]')
pdchDrUfactNoFilm = newPreDefColumn(pdstDoor,'U-Factor no Film [W/m2-K]')
pdchDrGrArea =  newPreDefColumn(pdstDoor,'Gross Area [m2]')
pdchDrParent = newPreDefColumn(pdstDoor,'Parent Surface')

! Shading Report
pdrShading =       newPreDefReport('ShadingSummary','Shade')

pdstSunlitFrac  =   newPreDefSubTable(pdrShading,'Sunlit Fraction')

pdchSlfMar21_9  =   newPreDefColumn(pdstSunlitFrac,'March 21 9am')
pdchSlfMar21_12 =   newPreDefColumn(pdstSunlitFrac,'March 21 noon')
pdchSlfMar21_15 =   newPreDefColumn(pdstSunlitFrac,'March 21 3pm')
pdchSlfJun21_9  =   newPreDefColumn(pdstSunlitFrac,'June 21 9am')
pdchSlfJun21_12 =   newPreDefColumn(pdstSunlitFrac,'June 21 noon')
pdchSlfJun21_15 =   newPreDefColumn(pdstSunlitFrac,'June 21 3pm')
pdchSlfDec21_9  =   newPreDefColumn(pdstSunlitFrac,'December 21 9am')
pdchSlfDec21_12 =   newPreDefColumn(pdstSunlitFrac,'December 21 noon')
pdchSlfDec21_15 =   newPreDefColumn(pdstSunlitFrac,'December 21 3pm')

pdstWindowControl =  newPreDefSubTable(pdrShading,'Window Control')

pdchWscName    = newPreDefColumn(pdstWindowControl,'Name')
pdchWscShading = newPreDefColumn(pdstWindowControl,'Type')
pdchWscShadCons = newPreDefColumn(pdstWindowControl,'Shaded Construction')
pdchWscControl = newPreDefColumn(pdstWindowControl,'Control')
pdchWscGlare = newPreDefColumn(pdstWindowControl,'Glare Control')

! Lighting Report
pdrLighting =  newPreDefReport('LightingSummary','Light')

pdstInLite =   newPreDefSubTable(pdrLighting, 'Interior Lighting')

pdchInLtZone   = newPreDefColumn(pdstInLite,'Zone')
pdchInLtDens   = newPreDefColumn(pdstInLite,'Lighting Power Density [W/m2]')
pdchInLtArea   = newPreDefColumn(pdstInLite,'Zone Area [m2]')
pdchInLtPower  = newPreDefColumn(pdstInLite,'Total Power [W]')
pdchInLtEndUse = newPreDefColumn(pdstInLite,'End Use Subcategory')
pdchInLtSchd   = newPreDefColumn(pdstInLite,'Schedule Name')
pdchInLtAvgHrSchd  = newPreDefColumn(pdstInLite,'Scheduled Hours/Week [hr]')
pdchInLtAvgHrOper  = newPreDefColumn(pdstInLite,'Hours/Week > 1% [hr]')
pdchInLtFullLoadHrs  = newPreDefColumn(pdstInLite,'Full Load Hours/Week [hr]')
pdchInLtRetAir = newPreDefColumn(pdstInLite,'Return Air Fraction')
pdchInLtCond   = newPreDefColumn(pdstInLite,'Conditioned (Y/N)')
pdchInLtConsump = newPreDefColumn(pdstInLite,'Consumption [GJ]')

pdstDaylight =  newPreDefSubTable(pdrLighting, 'Daylighting')

pdchDyLtZone =  newPreDefColumn(pdstDaylight,'Zone')
pdchDyLtKind =  newPreDefColumn(pdstDaylight,'Daylighting Type') !detailed or DElight
pdchDyLtCtrl =  newPreDefColumn(pdstDaylight,'Control Type') !stepped or continuous
pdchDyLtFrac =  newPreDefColumn(pdstDaylight,'Fraction Controlled')
pdchDyLtWInst = newPreDefColumn(pdstDaylight,'Lighting Installed in Zone [W]')
pdchDyLtWCtrl = newPreDefColumn(pdstDaylight,'Lighting Controlled [W]')

pdstExtLite =   newPreDefSubTable(pdrLighting, 'Exterior Lighting')

pdchExLtPower = newPreDefColumn(pdstExtLite,'Total Watts')
pdchExLtClock = newPreDefColumn(pdstExtLite,'Astronomical Clock/Schedule')
pdchExLtSchd  = newPreDefColumn(pdstExtLite,'Schedule Name')
pdchExLtAvgHrSchd = newPreDefColumn(pdstExtLite,'Scheduled Hours/Week [hr]')
pdchExLtAvgHrOper  = newPreDefColumn(pdstExtLite,'Hours/Week > 1% [hr]')
pdchExLtFullLoadHrs  = newPreDefColumn(pdstExtLite,'Full Load Hours/Week [hr]')
pdchExLtConsump = newPreDefColumn(pdstExtLite,'Consumption [GJ]')

! HVAC Equipment Report

pdrEquip = newPreDefReport('EquipmentSummary','Equip')

pdstMech = newPreDefSubTable(pdrEquip,'Central Plant')

pdchMechType =      newPreDefColumn(pdstMech,'Type')
pdchMechNomCap =    newPreDefColumn(pdstMech,'Nominal Capacity [W]')
pdchMechNomEff =    newPreDefColumn(pdstMech,'Nominal Efficiency [W/W]')

! Ok Constant                        Object Name                            Module                   Example File
! -- ------------------------------- -------------------------------------- ------------------------ -----------------
! o  CoilDX_CoolingSingleSpeed       Coil:Cooling:DX:SingleSpeed            DXCoil                   FurnaceWithDXSystem
! x  CoilDX_CoolingTwoSpeed          Coil:Cooling:DX:TwoSpeed               DXCoil                   5ZoneAutoDXVAV
! o  CoilDX_CoolingTwoStageWHumControl    Coil:Cooling:DX:                 DXCoil                   SmOffPSZ-MultiModeDX
!                                    TwoStageWithHumidityControlMode
! o  CoilDX_MultiSpeedCooling        Coil:Cooling:DX:MultiSpeed             DXCoil                   MultispeedHeatPump
! o  Coil_CoolingWater               Coil:Cooling:Water                     HVACWaterCoilComponent   5ZoneAirCooled
! o  Coil_CoolingWaterDetailed       Coil:Cooling:Water:DetailedGeometry    HVACWaterCoilComponent   5zoneWaterSystems
! o  Coil_CoolingWaterToAirHP        Coil:Cooling:WaterToAirHeatPump:       HVACWaterToAir           5ZoneWaterLoopHeatPump
!                                      ParameterEstimation
! o  Coil_CoolingWaterToAirHPSimple  Coil:Cooling:WaterToAirHeatPump:       HVACWaterToAir           HeatPumpWaterToAirEquationFit
!                                      EquationFit

! o  CoilDX_HeatingEmpirical         Coil:Heating:DX:SingleSpeed            DXCoil                   HeatPumpAuto
! o  CoilDX_MultiSpeedHeating        Coil:Heating:DX:MultiSpeed             DXCoil                   MultispeedHeatPump
! o  Coil_HeatingGas                 Coil:Heating:Gas                       HVACHeatingCoils         5ZoneAutoDXVAV
! o  Coil_HeatingElectric            Coil:Heating:Electric                  HVACHeatingCoils         PackagedTerminalAirConditioner
! o  Coil_HeatingDesuperheater       Coil:Heating:Desuperheater             HVACHeatingCoils         SuperMarket_DesuperHeatingCoil
! o  Coil_HeatingWater               Coil:Heating:Water                     HVACWaterCoilComponent   5ZoneAirCooled
! o  Coil_HeatingWaterToAirHP        Coil:Heating:WaterToAirHeatPump:       HVACWaterToAir           5ZoneWaterLoopHeatPump
!                                      ParameterEstimation
! o  Coil_HeatingWaterToAirHPSimple  Coil:Heating:WaterToAirHeatPump:       HVACWaterToAir           HeatPumpWaterToAirEquationFit
!                                      EquationFit
! o  CoilDX_HeatPumpWaterHeater      Coil:WaterHeating:AirToWaterHeatPump   DXCoil                   HeatPumpWaterHeater

!NOT INCLUDED:
!    CoilDX_CoolingHXAssisted        CoilSystem:Cooling:DX:                 HVACHXAssistedCooolingCoil
!                                      HeatExchangerAssisted
!    CoilWater_CoolingHXAssisted     CoilSystem:Cooling:Water:              HVACHXAssistedCooolingCoil
!                                      HeatExchangerAssisted


pdstCoolCoil = newPreDefSubTable(pdrEquip,'Cooling Coils')

pdchCoolCoilType    = newPreDefColumn(pdstCoolCoil,'Type')
pdchCoolCoilTotCap  = newPreDefColumn(pdstCoolCoil,'Nominal Total Capacity [W]')
pdchCoolCoilSensCap = newPreDefColumn(pdstCoolCoil,'Nominal Sensible Capacity [W]')
pdchCoolCoilLatCap  = newPreDefColumn(pdstCoolCoil,'Nominal Latent Capacity [W]')
pdchCoolCoilSHR     = newPreDefColumn(pdstCoolCoil,'Nominal Sensible Heat Ratio')
pdchCoolCoilNomEff  = newPreDefColumn(pdstCoolCoil,'Nominal Efficiency [W/W]')
pdchCoolCoilUATotal = newPreDefColumn(pdstCoolCoil,'Nominal Coil UA Value [W/C]')
pdchCoolCoilArea    = newPreDefColumn(pdstCoolCoil,'Nominal Coil Surface Area [m2]')

pdstDXCoolCoil = newPreDefSubTable(pdrEquip,'DX Cooling Coils')
pdchDXCoolCoilType   = newPreDefColumn(pdstDXCoolCoil,'DX Cooling Coil Type')
pdchDXCoolCoilNetCap = newPreDefColumn(pdstDXCoolCoil,'Standard Rating (Net) Cooling Capacity [W]')
pdchDXCoolCoilSEERSI = newPreDefColumn(pdstDXCoolCoil,'SEER in SI Units [W/W]')
pdchDXCoolCoilSEERIP = newPreDefColumn(pdstDXCoolCoil,'SEER in IP Units [Btu/W-h]')
pdchDXCoolCoilEERSI = newPreDefColumn(pdstDXCoolCoil,'EER in SI Units [W/W]')
pdchDXCoolCoilEERIP = newPreDefColumn(pdstDXCoolCoil,'EER in IP Units [Btu/W-h]')
pdchDXCoolCoilIEERSI = newPreDefColumn(pdstDXCoolCoil,'IEER in SI Units [W/W]')
pdchDXCoolCoilIEERIP = newPreDefColumn(pdstDXCoolCoil,'IEER in IP Units [Btu/W-h]')


pdstHeatCoil = newPreDefSubTable(pdrEquip,'Heating Coils')

pdchHeatCoilType   = newPreDefColumn(pdstHeatCoil,'Type')
pdchHeatCoilNomCap = newPreDefColumn(pdstHeatCoil,'Nominal Total Capacity [W]')
pdchHeatCoilNomEff = newPreDefColumn(pdstHeatCoil,'Nominal Efficiency [W/W]')

pdstFan = newPreDefSubTable(pdrEquip, 'Fans')

pdchFanType =        newPreDefColumn(pdstFan,'Type')
pdchFanTotEff =      newPreDefColumn(pdstFan,'Total Efficiency [W/W]')
pdchFanDeltaP =      newPreDefColumn(pdstFan,'Delta Pressure [pa]')
pdchFanVolFlow =     newPreDefColumn(pdstFan,'Max Air Flow Rate [m3/s]')
pdchFanPwr =         newPreDefColumn(pdstFan,'Rated Electric Power [W]')
pdchFanPwrPerFlow =  newPreDefColumn(pdstFan,'Rated Power Per Max Air Flow Rate [W-s/m3]')
pdchFanMotorIn =     newPreDefColumn(pdstFan,'Motor Heat In Air Fraction')
pdchFanEndUse =      newPreDefColumn(pdstFan,'End Use')

pdstPump = newPreDefSubTable(pdrEquip, 'Pumps')
pdchPumpType =       newPreDefColumn(pdstPump,'Type')
pdchPumpControl =    newPreDefColumn(pdstPump,'Control')
pdchPumpHead =       newPreDefColumn(pdstPump,'Head [pa]')
pdchPumpFlow =       newPreDefColumn(pdstPump,'Water Flow [m3/s]')
pdchPumpPower =      newPreDefColumn(pdstPump,'Electric Power [W]')
pdchPumpPwrPerFlow = newPreDefColumn(pdstPump,'Power Per Water Flow Rate [W-s/m3]')
pdchMotEff =         newPreDefColumn(pdstPump,'Motor Efficiency [W/W]')

pdstSWH = newPreDefSubTable(pdrEquip, 'Service Water Heating')
pdchSWHType =        newPreDefColumn(pdstSWH,'Type')
pdchSWHVol =         newPreDefColumn(pdstSWH,'Storage Volume [m3]')
pdchSWHHeatIn =      newPreDefColumn(pdstSWH,'Input [W]')
pdchSWHThEff =       newPreDefColumn(pdstSWH,'Thermal Efficiency [W/W]')
pdchSWHRecEff =      newPreDefColumn(pdstSWH,'Recovery Efficiency [W/W]')
pdchSWHEnFac =       newPreDefColumn(pdstSWH,'Energy Factor')

! Sizing Report

pdrSizing =     newPreDefReport('HVACSizingSummary','Size')

pdstZoneClSize =  newPreDefSubTable(pdrSizing,'Zone Cooling')

pdchZnClCalcDesLd     = newPreDefColumn(pdstZoneClSize,'Calculated Design Load [W]')
pdchZnClUserDesLd     = newPreDefColumn(pdstZoneClSize,'User Design Load [W]')
pdchZnClCalcDesAirFlow = newPreDefColumn(pdstZoneClSize,'Calculated Design Air Flow [m3/s]')
pdchZnClUserDesAirFlow = newPreDefColumn(pdstZoneClSize,'User Design Air Flow [m3/s]')
pdchZnClDesDay         = newPreDefColumn(pdstZoneClSize,'Design Day Name')
pdchZnClPkTime         = newPreDefColumn(pdstZoneClSize,'Date/Time Of Peak')
pdchZnClPkTemp         = newPreDefColumn(pdstZoneClSize,'Temperature at Peak [C]')
pdchZnClPkHum          = newPreDefColumn(pdstZoneClSize,'Humidity Ratio at Peak [kgWater/kgAir]')

pdstZoneHtSize =  newPreDefSubTable(pdrSizing,'Zone Heating')

pdchZnHtCalcDesLd      = newPreDefColumn(pdstZoneHtSize,'Calculated Design Load [W]')
pdchZnHtUserDesLd      = newPreDefColumn(pdstZoneHtSize,'User Design Load [W]')
pdchZnHtCalcDesAirFlow = newPreDefColumn(pdstZoneHtSize,'Calculated Design Air Flow [m3/s]')
pdchZnHtUserDesAirFlow = newPreDefColumn(pdstZoneHtSize,'User Design Air Flow [m3/s]')
pdchZnHtDesDay         = newPreDefColumn(pdstZoneHtSize,'Design Day Name')
pdchZnHtPkTime         = newPreDefColumn(pdstZoneHtSize,'Date/Time Of Peak')
pdchZnHtPkTemp         = newPreDefColumn(pdstZoneHtSize,'Temperature at Peak [C]')
pdchZnHtPkHum          = newPreDefColumn(pdstZoneHtSize,'Humidity Ratio at Peak [kgWater/kgAir]')

pdstSystemSize  =  newPreDefSubTable(pdrSizing,'System Design Air Flow Rates')

pdchSysSizCalcClAir    = newPreDefColumn(pdstSystemSize,'Calculated cooling [m3/s]')
pdchSysSizUserClAir    = newPreDefColumn(pdstSystemSize,'User cooling [m3/s]')
pdchSysSizCalcHtAir    = newPreDefColumn(pdstSystemSize,'Calculated heating [m3/s]')
pdchSysSizUserHtAir    = newPreDefColumn(pdstSystemSize,'User heating [m3/s]')

! System Summary Report

pdrSystem =  newPreDefReport('SystemSummary','Sys')

pdstEconomizer = newPreDefSubTable(pdrSystem, 'Economizer')

pdchEcoKind            = newPreDefColumn(pdstEconomizer,'High Limit Shutoff Control')
pdchEcoMinOA           = newPreDefColumn(pdstEconomizer,'Minimum Outdoor Air [m3/s]')
pdchEcoMaxOA           = newPreDefColumn(pdstEconomizer,'Maximum Outdoor Air [m3/s]')
pdchEcoRetTemp         = newPreDefColumn(pdstEconomizer,'Return Air Temp Limit')
pdchEcoRetEnth         = newPreDefColumn(pdstEconomizer,'Return Air Enthalpy Limit')
pdchEcoOATempLim       = newPreDefColumn(pdstEconomizer,'Outdoor Air Temperature Limit [C]')
pdchEcoOAEnthLim       = newPreDefColumn(pdstEconomizer,'Outdoor Air Enthalpy Limit [C]')

pdstDemCntlVent = newPreDefSubTable(pdrSystem, 'Demand Controlled Ventilation using Controller:MechanicalVentilation')
pdchDCVventMechName    = newPreDefColumn(pdstDemCntlVent,'Controller:MechanicalVentilation Name')
pdchDCVperPerson       = newPreDefColumn(pdstDemCntlVent,'Outdoor Air Per Person [m3/s-person]')
pdchDCVperArea         = newPreDefColumn(pdstDemCntlVent,'Outdoor Air Per Area [m3/s-m2]')

! added for new DCV
pdchDCVZoneADEffCooling = newPreDefColumn(pdstDemCntlVent,'Air Distribution Effectiveness in Cooling Mode')
pdchDCVZoneADEffHeating = newPreDefColumn(pdstDemCntlVent,'Air Distribution Effectiveness in Heating Mode')
pdchDCVZoneADEffSchName = newPreDefColumn(pdstDemCntlVent,'Air Distribution Effectiveness Schedule')

pdstSimpleComfort = newPreDefSubTable(pdrSystem, 'Time Not Comfortable Based on Simple ASHRAE 55-2004')
pdchSCwinterClothes    = newPreDefColumn(pdstSimpleComfort,'Winter Clothes [hr]')
pdchSCsummerClothes    = newPreDefColumn(pdstSimpleComfort,'Summer Clothes [hr]')
pdchSCeitherClothes    = newPreDefColumn(pdstSimpleComfort,'Summer or Winter Clothes [hr]')

pdstUnmetLoads = newPreDefSubTable(pdrSystem, 'Time Setpoint Not Met')
pdchULnotMetHeat       = newPreDefColumn(pdstUnmetLoads,'During Heating [hr]')
pdchULnotMetCool       = newPreDefColumn(pdstUnmetLoads,'During Cooling [hr]')
pdchULnotMetHeatOcc    = newPreDefColumn(pdstUnmetLoads,'During Occupied Heating [hr]')
pdchULnotMetCoolOcc    = newPreDefColumn(pdstUnmetLoads,'During Occupied Cooling [hr]')

! Outside Air Report
pdrOutsideAir = newPreDefReport('OutdoorAirSummary','OA')

pdstOAavgOcc = newPreDefSubTable(pdrOutsideAir, 'Average Outdoor Air During Occupied Hours')

pdchOaoAvgNumOcc1 =   newPreDefColumn(pdstOAavgOcc,'Average Number of Occupants')
pdchOaoNomNumOcc1 =   newPreDefColumn(pdstOAavgOcc,'Nominal Number of Occupants')
pdchOaoZoneVol1 =     newPreDefColumn(pdstOAavgOcc,'Zone Volume [m3]')
pdchOaoAvgMechVent =  newPreDefColumn(pdstOAavgOcc,'Mechanical Ventilation [ach]')
pdchOaoAvgInfil =     newPreDefColumn(pdstOAavgOcc,'Infiltration [ach]')
pdchOaoAvgSimpVent =  newPreDefColumn(pdstOAavgOcc,'Simple Ventilation [ach]')
!pdchOaoAvgTotVent =   newPreDefColumn(pdstOAavgOcc,'Total Ventilation [ach]')

CALL addFootNoteSubTable(pdstOAavgOcc,'Values shown for a single zone without multipliers')

pdstOAminOcc = newPreDefSubTable(pdrOutsideAir, 'Minimum Outdoor Air During Occupied Hours')

pdchOaoAvgNumOcc2 =   newPreDefColumn(pdstOAminOcc,'Average Number of Occupants')
pdchOaoNomNumOcc2 =   newPreDefColumn(pdstOAminOcc,'Nominal Number of Occupants')
pdchOaoZoneVol2 =     newPreDefColumn(pdstOAminOcc,'Zone Volume [m3]')
pdchOaoMinMechVent =  newPreDefColumn(pdstOAminOcc,'Mechanical Ventilation [ach]')
pdchOaoMinInfil =     newPreDefColumn(pdstOAminOcc,'Infiltration [ach]')
pdchOaoMinSimpVent =  newPreDefColumn(pdstOAminOcc,'Simple Ventilation [ach]')
!pdchOaoMinTotVent =   newPreDefColumn(pdstOAminOcc,'Total Ventilation [ach]')
CALL addFootNoteSubTable(pdstOAminOcc,'Values shown for a single zone without multipliers')

! Object Count Report
pdrObjCnt = newPreDefReport('ObjectCountSummary','Count')

pdstSurfCnt = newPreDefSubTable(pdrObjCnt,'Surfaces by Class')
pdchSurfCntTot =  newPreDefColumn(pdstSurfCnt,'Total')
pdchSurfCntExt =  newPreDefColumn(pdstSurfCnt,'Outdoors')

pdstHVACcnt = newPreDefSubTable(pdrObjCnt,'HVAC')
pdchHVACcntVal =  newPreDefColumn(pdstHVACcnt,'Count')

pdstFieldCnt = newPreDefSubTable(pdrObjCnt,'Input Fields')
pdchFieldCntVal =  newPreDefColumn(pdstFieldCnt,'Count')

! Sensible Heat Gas Component Report
pdrSensibleGain = newPreDefReport('SensibleHeatGainSummary','SHGS')

pdstSHGSannual = newPreDefSubTable(pdrSensibleGain, 'Annual Building Sensible Heat Gain Components')

pdchSHGSAnHvacHt    =   newPreDefColumn(pdstSHGSannual,'HVAC Input Sensible Air Heating [GJ]')
pdchSHGSAnHvacCl    =   newPreDefColumn(pdstSHGSannual,'HVAC Input Sensible Air Cooling [GJ]')
pdchSHGSAnSurfHt    =   newPreDefColumn(pdstSHGSannual,'HVAC Input Heated Surface Heating [GJ]')
pdchSHGSAnSurfCl    =   newPreDefColumn(pdstSHGSannual,'HVAC Input Cooled Surface Cooling [GJ]')
pdchSHGSAnPeoplAdd  =   newPreDefColumn(pdstSHGSannual,'People Sensible Heat Addition [GJ]')
pdchSHGSAnLiteAdd   =   newPreDefColumn(pdstSHGSannual,'Lights Sensible Heat Addition [GJ]')
pdchSHGSAnEquipAdd  =   newPreDefColumn(pdstSHGSannual,'Equipment Sensible Heat Addition [GJ]')
pdchSHGSAnWindAdd   =   newPreDefColumn(pdstSHGSannual,'Window Heat Addition [GJ]')
pdchSHGSAnIzaAdd    =   newPreDefColumn(pdstSHGSannual,'Interzone Air Transfer Heat Addition [GJ]')
pdchSHGSAnInfilAdd  =   newPreDefColumn(pdstSHGSannual,'Infiltration Heat Addition [GJ]')
pdchSHGSAnOtherAdd  =   newPreDefColumn(pdstSHGSannual,'Opaque Surface Conduction and Other Heat Addition [GJ]')
pdchSHGSAnEquipRem  =   newPreDefColumn(pdstSHGSannual,'Equipment Sensible Heat Removal [GJ]')
pdchSHGSAnWindRem   =   newPreDefColumn(pdstSHGSannual,'Window Heat Removal [GJ]')
pdchSHGSAnIzaRem    =   newPreDefColumn(pdstSHGSannual,'Interzone Air Transfer Heat Removal [GJ]')
pdchSHGSAnInfilRem  =   newPreDefColumn(pdstSHGSannual,'Infiltration Heat Removal [GJ]')
pdchSHGSAnOtherRem  =   newPreDefColumn(pdstSHGSannual,'Opaque Surface Conduction and Other Heat Removal [GJ]')

pdstSHGSpkCl = newPreDefSubTable(pdrSensibleGain, 'Peak Cooling Sensible Heat Gain Components')

pdchSHGSClTimePeak  =   newPreDefColumn(pdstSHGSpkCl,'Time of Peak')
pdchSHGSClHvacHt    =   newPreDefColumn(pdstSHGSpkCl,'HVAC Input Sensible Air Heating [W]')
pdchSHGSClHvacCl    =   newPreDefColumn(pdstSHGSpkCl,'HVAC Input Sensible Air Cooling [W]')
pdchSHGSClSurfHt    =   newPreDefColumn(pdstSHGSpkCl,'HVAC Input Heated Surface Heating [W]')
pdchSHGSClSurfCl    =   newPreDefColumn(pdstSHGSpkCl,'HVAC Input Cooled Surface Cooling [W]')
pdchSHGSClPeoplAdd  =   newPreDefColumn(pdstSHGSpkCl,'People Sensible Heat Addition [W]')
pdchSHGSClLiteAdd   =   newPreDefColumn(pdstSHGSpkCl,'Lights Sensible Heat Addition [W]')
pdchSHGSClEquipAdd  =   newPreDefColumn(pdstSHGSpkCl,'Equipment Sensible Heat Addition [W]')
pdchSHGSClWindAdd   =   newPreDefColumn(pdstSHGSpkCl,'Window Heat Addition [W]')
pdchSHGSClIzaAdd    =   newPreDefColumn(pdstSHGSpkCl,'Interzone Air Transfer Heat Addition [W]')
pdchSHGSClInfilAdd  =   newPreDefColumn(pdstSHGSpkCl,'Infiltration Heat Addition [W]')
pdchSHGSClOtherAdd  =   newPreDefColumn(pdstSHGSpkCl,'Opaque Surface Conduction and Other Heat Addition [W]')
pdchSHGSClEquipRem  =   newPreDefColumn(pdstSHGSpkCl,'Equipment Sensible Heat Removal [W]')
pdchSHGSClWindRem   =   newPreDefColumn(pdstSHGSpkCl,'Window Heat Removal [W]')
pdchSHGSClIzaRem    =   newPreDefColumn(pdstSHGSpkCl,'Interzone Air Transfer Heat Removal [W]')
pdchSHGSClInfilRem  =   newPreDefColumn(pdstSHGSpkCl,'Infiltration Heat Removal [W]')
pdchSHGSClOtherRem  =   newPreDefColumn(pdstSHGSpkCl,'Opaque Surface Conduction and Other Heat Removal [W]')

pdstSHGSpkHt = newPreDefSubTable(pdrSensibleGain, 'Peak Heating Sensible Heat Gain Components')

pdchSHGSHtTimePeak  =   newPreDefColumn(pdstSHGSpkHt,'Time of Peak')
pdchSHGSHtHvacHt    =   newPreDefColumn(pdstSHGSpkHt,'HVAC Input Sensible Air Heating [W]')
pdchSHGSHtHvacCl    =   newPreDefColumn(pdstSHGSpkHt,'HVAC Input Sensible Air Cooling [W]')
pdchSHGSHtSurfHt    =   newPreDefColumn(pdstSHGSpkHt,'HVAC Input Heated Surface Heating [W]')
pdchSHGSHtSurfCl    =   newPreDefColumn(pdstSHGSpkHt,'HVAC Input Cooled Surface Cooling [W]')
pdchSHGSHtPeoplAdd  =   newPreDefColumn(pdstSHGSpkHt,'People Sensible Heat Addition [W]')
pdchSHGSHtLiteAdd   =   newPreDefColumn(pdstSHGSpkHt,'Lights Sensible Heat Addition [W]')
pdchSHGSHtEquipAdd  =   newPreDefColumn(pdstSHGSpkHt,'Equipment Sensible Heat Addition [W]')
pdchSHGSHtWindAdd   =   newPreDefColumn(pdstSHGSpkHt,'Window Heat Addition [W]')
pdchSHGSHtIzaAdd    =   newPreDefColumn(pdstSHGSpkHt,'Interzone Air Transfer Heat Addition [W]')
pdchSHGSHtInfilAdd  =   newPreDefColumn(pdstSHGSpkHt,'Infiltration Heat Addition [W]')
pdchSHGSHtOtherAdd  =   newPreDefColumn(pdstSHGSpkHt,'Opaque Surface Conduction and Other Heat Addition [W]')
pdchSHGSHtEquipRem  =   newPreDefColumn(pdstSHGSpkHt,'Equipment Sensible Heat Removal [W]')
pdchSHGSHtWindRem   =   newPreDefColumn(pdstSHGSpkHt,'Window Heat Removal [W]')
pdchSHGSHtIzaRem    =   newPreDefColumn(pdstSHGSpkHt,'Interzone Air Transfer Heat Removal [W]')
pdchSHGSHtInfilRem  =   newPreDefColumn(pdstSHGSpkHt,'Infiltration Heat Removal [W]')
pdchSHGSHtOtherRem  =   newPreDefColumn(pdstSHGSpkHt,'Opaque Surface Conduction and Other Heat Removal [W]')

END SUBROUTINE SetPredefinedTables

SUBROUTINE RealPreDefTableEntry(columnIndex,objName,tableEntryReal,numSigDigits)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates an entry for predefined tables when the entry
          !   is a real variable

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)                      :: columnIndex
CHARACTER(len=*),INTENT(IN)  :: objName
REAL(r64),INTENT(IN)                          :: tableEntryReal
INTEGER, INTENT(IN),OPTIONAL             :: numSigDigits

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: sigDigitCount
CHARACTER(len=1) :: digitString
CHARACTER(len=7) :: formatConvert
CHARACTER(len=12) :: stringEntry
INTEGER :: IOS

CALL incrementTableEntry
!check for number of significant digits
IF (PRESENT(numSigDigits)) THEN
  IF ((numSigDigits .LE. 9) .AND. (numSigDigits .GE. 0)) THEN
    sigDigitCount = numSigDigits
  ELSE
    sigDigitCount = 2
  END IF
ELSE
  sigDigitCount = 2
ENDIF
! convert the integer to a string for the number of digits
WRITE(FMT='(I1)', UNIT=digitString) sigDigitCount
! build up the format string
IF (tableEntryReal < 1d10) THEN
  formatConvert = '(F12.' // digitString // ')'
ELSE
  formatConvert = '(E12.' // digitString // ')'
ENDIF
WRITE(FMT=formatConvert, UNIT=stringEntry,IOSTAT=IOS) tableEntryReal
IF (IOS /= 0) stringEntry='  Too Big'
tableEntry(numTableEntry)%charEntry =   stringEntry
tableEntry(numTableEntry)%objectName =  objName
tableEntry(numTableEntry)%indexColumn = columnIndex
tableEntry(numTableEntry)%origRealEntry = tableEntryReal
tableEntry(numTableEntry)%significantDigits = sigDigitCount
tableEntry(numTableEntry)%origEntryIsReal = .TRUE.
END SUBROUTINE RealPreDefTableEntry

SUBROUTINE CharPreDefTableEntry(columnIndex,objName,tableEntryChar)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates an entry for predefined tables when the entry
          !   is a character variable

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)                      :: columnIndex
CHARACTER(len=*),INTENT(IN)  :: objName
CHARACTER(len=*),INTENT(IN)  :: tableEntryChar

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

CALL incrementTableEntry
tableEntry(numTableEntry)%charEntry =   tableEntryChar
tableEntry(numTableEntry)%objectName =  objName
tableEntry(numTableEntry)%indexColumn = columnIndex
END SUBROUTINE CharPreDefTableEntry


SUBROUTINE IntPreDefTableEntry(columnIndex,objName,tableEntryInt)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates an entry for predefined tables when the entry
          !   is a integer variable

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)                      :: columnIndex
CHARACTER(len=*),INTENT(IN)  :: objName
INTEGER,INTENT(IN)                       :: tableEntryInt

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(LEN=12)      :: stringEntry

CALL incrementTableEntry
! convert the integer to a string
WRITE(FMT=*, UNIT=stringEntry) tableEntryInt
tableEntry(numTableEntry)%charEntry =   stringEntry
tableEntry(numTableEntry)%objectName =  objName
tableEntry(numTableEntry)%indexColumn = columnIndex
END SUBROUTINE IntPreDefTableEntry

SUBROUTINE incrementTableEntry
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Manages the resizing of the TableEntry Array

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

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
IF (.NOT. ALLOCATED(tableEntry)) THEN
  ALLOCATE(tableEntry(sizeIncrement))
  sizeTableEntry = sizeIncrement
  numTableEntry = 1
ELSE
  numTableEntry = numTableEntry + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numTableEntry .GT. sizeTableEntry) THEN
    ALLOCATE(tableEntryCopy(sizeTableEntry))
    tableEntryCopy = tableEntry
    DEALLOCATE(tableEntry)
    ALLOCATE(tableEntry(sizeTableEntry + sizeIncrement))
    tableEntry(1:sizeTableEntry) = tableEntryCopy
    DEALLOCATE(tableEntryCopy)
    sizeTableEntry = sizeTableEntry + sizeIncrement
  END IF
END IF
END SUBROUTINE incrementTableEntry

SUBROUTINE AddCompSizeTableEntry(FieldType,FieldName,FieldDescription,FieldValue)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2007
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates an entry for component size tables.

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),INTENT(IN)  :: FieldType
CHARACTER(len=*),INTENT(IN)  :: FieldName
CHARACTER(len=*),INTENT(IN)  :: FieldDescription
REAL(r64),INTENT(IN)              :: FieldValue

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

IF (.NOT. ALLOCATED(CompSizeTableEntry)) THEN
  ALLOCATE(CompSizeTableEntry(sizeIncrement))
  sizeCompSizeTableEntry = sizeIncrement
  numCompSizeTableEntry = 1
ELSE
  numCompSizeTableEntry = numCompSizeTableEntry + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numCompSizeTableEntry .GT. sizeCompSizeTableEntry) THEN
    ALLOCATE(CompSizeTableEntryCopy(sizeCompSizeTableEntry))
    CompSizeTableEntryCopy = CompSizeTableEntry
    DEALLOCATE(CompSizeTableEntry)
    ALLOCATE(CompSizeTableEntry(sizeCompSizeTableEntry + sizeIncrement))
    CompSizeTableEntry(1:sizeCompSizeTableEntry) = CompSizeTableEntryCopy
    DEALLOCATE(CompSizeTableEntryCopy)
    sizeCompSizeTableEntry = sizeCompSizeTableEntry + sizeIncrement
  END IF
END IF
CompSizeTableEntry(numCompSizeTableEntry)%typeField =   FieldType
CompSizeTableEntry(numCompSizeTableEntry)%nameField =   FieldName
CompSizeTableEntry(numCompSizeTableEntry)%description = FieldDescription
CompSizeTableEntry(numCompSizeTableEntry)%valField =    FieldValue
END SUBROUTINE AddCompSizeTableEntry

SUBROUTINE AddShadowRelateTableEntry(castingField,receivingField,receivingKind)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2007
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates an entry for any shadow hierarchy tables that consist
          !   of items and one or more subitems for each item.

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
!CHARACTER(len=*),INTENT(IN)  :: castingField
!CHARACTER(len=*),INTENT(IN)  :: receivingField
INTEGER,INTENT(IN)  :: castingField
INTEGER,INTENT(IN)  :: receivingField
INTEGER, INTENT(IN)          :: receivingKind

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

IF (.NOT. ALLOCATED(ShadowRelate)) THEN
  ALLOCATE(ShadowRelate(sizeIncrement))
  sizeShadowRelate = sizeIncrement
  numShadowRelate = 1
ELSE
  numShadowRelate = numShadowRelate + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numShadowRelate .GT. sizeShadowRelate) THEN
    ALLOCATE(ShadowRelateCopy(sizeShadowRelate))
    ShadowRelateCopy = ShadowRelate
    DEALLOCATE(ShadowRelate)
    ALLOCATE(ShadowRelate(sizeShadowRelate + sizeIncrement))
    ShadowRelate(1:sizeShadowRelate) = ShadowRelateCopy
    DEALLOCATE(ShadowRelateCopy)
    sizeShadowRelate = sizeShadowRelate + sizeIncrement
  END IF
END IF
ShadowRelate(numShadowRelate)%castSurf =   castingField
ShadowRelate(numShadowRelate)%recSurf =    receivingField
ShadowRelate(numShadowRelate)%recKind =    receivingKind
END SUBROUTINE AddShadowRelateTableEntry


INTEGER FUNCTION newPreDefReport(inReportName,inReportAbrev)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates a new index for the next predefined report

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*),INTENT(IN)  :: inReportName
CHARACTER(len=*),INTENT(IN)  :: inReportAbrev

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (.NOT. ALLOCATED(ReportName)) THEN
  ALLOCATE(ReportName(sizeIncrement))
  sizeReportName = sizeIncrement
  numReportName = 1
ELSE
  numReportName = numReportName + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numReportName .GT. sizeReportName) THEN
    ALLOCATE(reportNameCopy(sizeReportName))
    reportNameCopy = reportName
    DEALLOCATE(reportName)
    ALLOCATE(reportName(sizeReportName + sizeIncrement))
    reportName(1:sizeReportName) = reportNameCopy
    DEALLOCATE(reportNameCopy)
    sizeReportName = sizeReportName + sizeIncrement
  END IF
END IF
! initialize new record
reportName(numReportName)%name = inReportName
reportName(numReportName)%abrev = inReportAbrev
reportName(numReportName)%show = .FALSE.
newPreDefReport = numReportName
END FUNCTION newPreDefReport

INTEGER FUNCTION newPreDefSubTable(reportIndex,subTableName)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Assigns the index for predefined sub-tables

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)                       :: reportIndex
CHARACTER(len=*),INTENT(IN)  :: subTableName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (.NOT. ALLOCATED(subTable)) THEN
  ALLOCATE(subTable(sizeIncrement))
  sizeSubTable = sizeIncrement
  numSubTable = 1
ELSE
  numSubTable = numSubTable + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numSubTable .GT. sizeSubTable) THEN
    ALLOCATE(subTableCopy(sizeSubTable))
    subTableCopy = subTable
    DEALLOCATE(subTable)
    ALLOCATE(subTable(sizeSubTable + sizeIncrement))
    subTable(1:sizeSubTable) = subTableCopy
    DEALLOCATE(subTableCopy)
    sizeSubTable = sizeSubTable + sizeIncrement
  END IF
END IF
! initialize new record)
subTable(numSubTable)%name = subTableName
subTable(numSubTable)%indexReportName = reportIndex
newPreDefSubTable = numSubTable
END FUNCTION newPreDefSubTable

SUBROUTINE addFootNoteSubTable(subTableIndex,footnoteText)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2008
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Adds a footnote to a subtable

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)           :: subTableIndex
CHARACTER(len=*),INTENT(IN)  :: footnoteText

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF ((subTableIndex .GE. 0) .AND. (subTableIndex .LE. numSubtable)) THEN
  subTable(subTableIndex)%footnote = footnoteText
END IF
END SUBROUTINE

INTEGER FUNCTION newPreDefColumn(subTableIndex,columnHeading)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Assigns the index variables for all predefined reports

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)                       :: subTableIndex
CHARACTER(len=*),INTENT(IN)  :: columnHeading

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (.NOT. ALLOCATED(columnTag)) THEN
  ALLOCATE(columnTag(sizeIncrement))
  sizeColumnTag = sizeIncrement
  numColumnTag = 1
ELSE
  numColumnTag = numColumnTag + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numColumnTag .GT. sizeColumnTag) THEN
    ALLOCATE(columnTagCopy(sizeColumnTag))
    columnTagCopy = columnTag
    DEALLOCATE(columnTag)
    ALLOCATE(columnTag(sizeColumnTag + sizeIncrement))
    columnTag(1:sizeColumnTag) = columnTagCopy
    DEALLOCATE(columnTagCopy)
    sizeColumnTag = sizeColumnTag + sizeIncrement
  END IF
END IF
! initialize new record)
columnTag(numColumnTag)%heading = columnHeading
columnTag(numColumnTag)%indexSubTable = subTableIndex
newPreDefColumn = numColumnTag
END FUNCTION newPreDefColumn

!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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

END MODULE OutputReportPredefined
