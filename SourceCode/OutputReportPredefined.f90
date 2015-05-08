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
INTEGER :: pdchMechIPLVSI
INTEGER :: pdchMechIPLVIP
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
INTEGER :: pdchCoolCoilDesCap
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

INTEGER :: pdchDXCoolCoilNetCapSI      ! Standard Rated (Net) Cooling Capacity [W]
INTEGER :: pdchDXCoolCoilCOP       ! EER/COP value in SI unit at AHRI std. 340/360 conditions [W/W]
INTEGER :: pdchDXCoolCoilSEERIP      ! SEER value in IP unit at AHRI std. 210/240 conditions [Btu/W-hr]
INTEGER :: pdchDXCoolCoilEERIP       ! EER value in IP unit at AHRI std. 340/360 conditions [Btu/W-h]
INTEGER :: pdchDXCoolCoilIEERIP      ! IEER value in IP unit at AHRI std. 340/360 conditions

! VAV DX Cooling Ratings Details
INTEGER :: pdstVAVDXCoolCoil   ! details for Packaged VAV rating under AHRI 340/360
INTEGER :: pdchVAVDXCoolCoilType
INTEGER :: pdchVAVDXFanName
INTEGER :: pdchVAVDXCoolCoilNetCapSI
INTEGER :: pdchVAVDXCoolCoilCOP
INTEGER :: pdchVAVDXCoolCoilIEERIP
INTEGER :: pdchVAVDXCoolCoilEERIP
INTEGER :: pdchVAVDXCoolCoilMdotA
INTEGER :: pdchVAVDXCoolCoilCOP_B
INTEGER :: pdchVAVDXCoolCoilEER_B_IP
INTEGER :: pdchVAVDXCoolCoilMdotB
INTEGER :: pdchVAVDXCoolCoilCOP_C
INTEGER :: pdchVAVDXCoolCoilEER_C_IP
INTEGER :: pdchVAVDXCoolCoilMdotC
INTEGER :: pdchVAVDXCoolCoilCOP_D
INTEGER :: pdchVAVDXCoolCoilEER_D_IP
INTEGER :: pdchVAVDXCoolCoilMdotD


! DX Heating Coil subtable
INTEGER :: pdstDXHeatCoil            !
INTEGER :: pdchDXHeatCoilType        ! DX Heating coil type
INTEGER :: pdchDXHeatCoilHighCap
INTEGER :: pdchDXHeatCoilLowCap
INTEGER :: pdchDXHeatCoilHSPFSI      ! HSPF value in SI unit at AHRI std. 340/360 conditions [W/W]
INTEGER :: pdchDXHeatCoilHSPFIP      ! HSPF value in IP unit at AHRI std. 340/360 conditions [Btu/W-hr]
INTEGER :: pdchDXHeatCoilRegionNum   ! Region number for which HSPF is calculated

! Heating Coil subtable
INTEGER :: pdstHeatCoil
INTEGER :: pdchHeatCoilType
INTEGER :: pdchHeatCoilDesCap
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
INTEGER :: pdchFenTilt
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
INTEGER :: pdchZnClUserDesLdPerArea
INTEGER :: pdchZnClCalcDesAirFlow
INTEGER :: pdchZnClUserDesAirFlow
INTEGER :: pdchZnClDesDay
INTEGER :: pdchZnClPkTime
INTEGER :: pdchZnClPkTstatTemp
INTEGER :: pdchZnClPkIndTemp
INTEGER :: pdchZnClPkIndHum
INTEGER :: pdchZnClPkOATemp
INTEGER :: pdchZnClPkOAHum
INTEGER :: pdstZoneHtSize
INTEGER :: pdchZnHtCalcDesLd
INTEGER :: pdchZnHtUserDesLd
INTEGER :: pdchZnHtUserDesLdPerArea
INTEGER :: pdchZnHtCalcDesAirFlow
INTEGER :: pdchZnHtUserDesAirFlow
INTEGER :: pdchZnHtDesDay
INTEGER :: pdchZnHtPkTime
INTEGER :: pdchZnHtPkTstatTemp
INTEGER :: pdchZnHtPkIndTemp
INTEGER :: pdchZnHtPkIndHum
INTEGER :: pdchZnHtPkOATemp
INTEGER :: pdchZnHtPkOAHum
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
INTEGER :: pdchOaoAvgAFNInfil
INTEGER :: pdchOaoAvgSimpVent
INTEGER :: pdchOaoAvgTotVent
INTEGER :: pdstOAminOcc
INTEGER :: pdchOaoAvgNumOcc2
INTEGER :: pdchOaoNomNumOcc2
INTEGER :: pdchOaoZoneVol2
INTEGER :: pdchOaoMinMechVent
INTEGER :: pdchOaoMinInfil
INTEGER :: pdchOaoMinAFNInfil
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

! Energy Meters Report
INTEGER :: pdrEnergyMeters

INTEGER :: pdstEMelecvalues
INTEGER :: pdchEMelecannual
INTEGER :: pdchEMelecminvalue
INTEGER :: pdchEMelecminvaluetime
INTEGER :: pdchEMelecmaxvalue
INTEGER :: pdchEMelecmaxvaluetime

INTEGER :: pdstEMgasvalues
INTEGER :: pdchEMgasannual
INTEGER :: pdchEMgasminvalue
INTEGER :: pdchEMgasminvaluetime
INTEGER :: pdchEMgasmaxvalue
INTEGER :: pdchEMgasmaxvaluetime

INTEGER :: pdstEMcoolvalues
INTEGER :: pdchEMcoolannual
INTEGER :: pdchEMcoolminvalue
INTEGER :: pdchEMcoolminvaluetime
INTEGER :: pdchEMcoolmaxvalue
INTEGER :: pdchEMcoolmaxvaluetime

INTEGER :: pdstEMwatervalues
INTEGER :: pdchEMwaterannual
INTEGER :: pdchEMwaterminvalue
INTEGER :: pdchEMwaterminvaluetime
INTEGER :: pdchEMwatermaxvalue
INTEGER :: pdchEMwatermaxvaluetime

INTEGER :: pdstEMotherJvalues
INTEGER :: pdchEMotherJannual
INTEGER :: pdchEMotherJminvalue
INTEGER :: pdchEMotherJminvaluetime
INTEGER :: pdchEMotherJmaxvalue
INTEGER :: pdchEMotherJmaxvaluetime

INTEGER :: pdstEMotherKGvalues
INTEGER :: pdchEMotherKGannual
INTEGER :: pdchEMotherKGminvalue
INTEGER :: pdchEMotherKGminvaluetime
INTEGER :: pdchEMotherKGmaxvalue
INTEGER :: pdchEMotherKGmaxvaluetime

INTEGER :: pdstEMotherM3values
INTEGER :: pdchEMotherM3annual
INTEGER :: pdchEMotherM3minvalue
INTEGER :: pdchEMotherM3minvaluetime
INTEGER :: pdchEMotherM3maxvalue
INTEGER :: pdchEMotherM3maxvaluetime

INTEGER :: pdstEMotherLvalues
INTEGER :: pdchEMotherLannual
INTEGER :: pdchEMotherLminvalue
INTEGER :: pdchEMotherLminvaluetime
INTEGER :: pdchEMotherLmaxvalue
INTEGER :: pdchEMotherLmaxvaluetime

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
!Standard62Report
INTEGER :: pdrStd62
INTEGER :: pdstS62sysVentReqCool
INTEGER :: pdchS62svrClSumVpz
INTEGER :: pdchS62svrClPs
INTEGER :: pdchS62svrClSumPz
INTEGER :: pdchS62svrClD
INTEGER :: pdchS62svrClVou
INTEGER :: pdchS62svrClVps
INTEGER :: pdchS62svrClXs
INTEGER :: pdchS62svrClEv
INTEGER :: pdchS62svrClVot
INTEGER :: pdchS62svrClPercOA

INTEGER :: pdstS62sysVentReqHeat
INTEGER :: pdchS62svrHTSumVpz
INTEGER :: pdchS62svrHtPs
INTEGER :: pdchS62svrHtSumPz
INTEGER :: pdchS62svrHtD
INTEGER :: pdchS62svrHtVou
INTEGER :: pdchS62svrHtVps
INTEGER :: pdchS62svrHtXs
INTEGER :: pdchS62svrHtEv
INTEGER :: pdchS62svrHtVot
INTEGER :: pdchS62svrHtPercOA

INTEGER :: pdstS62znVentPar
INTEGER :: pdchS62zvpAlN
INTEGER :: pdchS62zvpRp
INTEGER :: pdchS62zvpPz
INTEGER :: pdchS62zvpRa
INTEGER :: pdchS62zvpAz
INTEGER :: pdchS62zvpVbz
INTEGER :: pdchS62zvpClEz
INTEGER :: pdchS62zvpClVoz
INTEGER :: pdchS62zvpHtEz
INTEGER :: pdchS62zvpHtVoz

INTEGER :: pdstS62sysVentPar
INTEGER :: pdchS62svpRp
INTEGER :: pdchS62svpPz
INTEGER :: pdchS62svpRa
INTEGER :: pdchS62svpAz
INTEGER :: pdchS62svpVbz
INTEGER :: pdchS62svpClVoz
INTEGER :: pdchS62svpHtVoz

INTEGER :: pdstS62znCoolDes
INTEGER :: pdchS62zcdAlN
INTEGER :: pdchS62zcdBox
INTEGER :: pdchS62zcdVpz
INTEGER :: pdchS62zcdVps
INTEGER :: pdchS62zcdVsec
INTEGER :: pdchS62zcdVdz
INTEGER :: pdchS62zcdVpzmin
INTEGER :: pdchS62zcdVozclg
INTEGER :: pdchS62zcdZpz
INTEGER :: pdchS62zcdEp
INTEGER :: pdchS62zcdEr
INTEGER :: pdchS62zcdFa
INTEGER :: pdchS62zcdFb
INTEGER :: pdchS62zcdFc
INTEGER :: pdchS62zcdEvz

INTEGER :: pdstS62sysCoolDes
INTEGER :: pdchS62scdVpz
INTEGER :: pdchS62scdVps
INTEGER :: pdchS62scdVsec
INTEGER :: pdchS62scdVdz
INTEGER :: pdchS62scdVpzmin
INTEGER :: pdchS62scdVozclg
INTEGER :: pdchS62scdEvz

INTEGER :: pdstS62znHeatDes
INTEGER :: pdchS62zhdAlN
INTEGER :: pdchS62zhdBox
INTEGER :: pdchS62zhdVpz
INTEGER :: pdchS62zhdVps
INTEGER :: pdchS62zhdVsec
INTEGER :: pdchS62zhdVdz
INTEGER :: pdchS62zhdVpzmin
INTEGER :: pdchS62zhdVozhtg
INTEGER :: pdchS62zhdZpz
INTEGER :: pdchS62zhdEp
INTEGER :: pdchS62zhdEr
INTEGER :: pdchS62zhdFa
INTEGER :: pdchS62zhdFb
INTEGER :: pdchS62zhdFc
INTEGER :: pdchS62zhdEvz

INTEGER :: pdstS62sysHeatDes
INTEGER :: pdchS62shdVpz
INTEGER :: pdchS62shdVps
INTEGER :: pdchS62shdVsec
INTEGER :: pdchS62shdVdz
INTEGER :: pdchS62shdVpzmin
INTEGER :: pdchS62shdVozhtg
INTEGER :: pdchS62shdEvz

!  LEED Summary
INTEGER :: pdrLeed
INTEGER :: pdstLeedGenInfo
INTEGER :: pdchLeedGenData

INTEGER :: pdstLeedSpaceUsageType
INTEGER :: pdchLeedSutName
INTEGER :: pdchLeedSutSpArea
INTEGER :: pdchLeedSutOcArea
INTEGER :: pdchLeedSutUnArea
INTEGER :: pdchLeedSutHrsWeek

INTEGER :: pdstLeedAdvsMsg
INTEGER :: pdchLeedAmData

INTEGER :: pdstLeedEneTypSum
INTEGER :: pdchLeedEtsType
INTEGER :: pdchLeedEtsRtNm
INTEGER :: pdchLeedEtsVirt
INTEGER :: pdchLeedEtsEneUnt
INTEGER :: pdchLeedEtsDemUnt

INTEGER :: pdstLeedPerf
INTEGER :: pdchLeedPerfRot
INTEGER :: pdchLeedPerfElEneUse
INTEGER :: pdchLeedPerfElDem
INTEGER :: pdchLeedPerfGasEneUse
INTEGER :: pdchLeedPerfGasDem
INTEGER :: pdchLeedPerfOthEneUse
INTEGER :: pdchLeedPerfOthDem

INTEGER :: pdstLeedEneUseSum
INTEGER :: pdchLeedEusUnt
INTEGER :: pdchLeedEusProc
INTEGER :: pdchLeedEusTotal

INTEGER :: pdstLeedEneCostSum
INTEGER :: pdchLeedEcUnt
INTEGER :: pdchLeedEcsProc
INTEGER :: pdchLeedEcsTotal
REAL(r64) :: LEEDelecCostTotal
REAL(r64) :: LEEDgasCostTotal
REAL(r64) :: LEEDothrCostTotal

INTEGER :: pdstLeedRenewSum
INTEGER :: pdchLeedRenRatCap
INTEGER :: pdchLeedRenAnGen

INTEGER :: pdstLeedEneUseIntEl
INTEGER :: pdchLeedEuiElec
INTEGER :: pdstLeedEneUseIntNatG
INTEGER :: pdchLeedEuiNatG
INTEGER :: pdstLeedEneUseIntOthr
INTEGER :: pdchLeedEuiOthr

INTEGER :: pdstLeedEneUsePerc
INTEGER :: pdchLeedEupPerc


! Internal data structures to store information provided by calls

INTEGER, PARAMETER :: sizeIncrement = 100

TYPE reportNameType
  CHARACTER(len=MaxNameLength) :: name = ''
  CHARACTER(len=MaxNameLength) :: namewithspaces = ''  ! a "prettier version" than the key value
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
  CHARACTER(len=2*MaxNameLength)  :: footnote  = ''
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
  REAL(r64)                     :: origRealEntry = 0.0d0
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
  REAL(r64)                     :: valField      = 0.0d0
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

REAL(r64) :: TotalNotMetHeatingOccupiedForABUPS = 0.0d0
REAL(r64) :: TotalNotMetCoolingOccupiedForABUPS = 0.0d0
REAL(r64) :: TotalNotMetOccupiedForABUPS = 0.0d0
REAL(r64) :: TotalTimeNotSimpleASH55EitherForABUPS = 0.0d0

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
USE DataGlobals, ONLY: DoZoneSizing, DoSystemSizing

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
pdrClim = newPreDefReport('ClimaticDataSummary','Clim','Climatic Data Summary')

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

pdrEnvelope =   newPreDefReport('EnvelopeSummary','Env','Envelope Summary')

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
pdchFenTilt =    newPreDefColumn(pdstFen,'Tilt [deg]')
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
pdrShading =       newPreDefReport('ShadingSummary','Shade','Shading Summary')

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
pdrLighting =  newPreDefReport('LightingSummary','Light','Lighting Summary')

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

pdrEquip = newPreDefReport('EquipmentSummary','Equip','Equipment Summary')

pdstMech = newPreDefSubTable(pdrEquip,'Central Plant')

pdchMechType =      newPreDefColumn(pdstMech,'Type')
pdchMechNomCap =    newPreDefColumn(pdstMech,'Nominal Capacity [W]')
pdchMechNomEff =    newPreDefColumn(pdstMech,'Nominal Efficiency [W/W]')
pdchMechIPLVSI =    newPreDefColumn(pdstMech,'IPLV in SI Units [W/W]')
pdchMechIPLVIP =    newPreDefColumn(pdstMech,'IPLV in IP Units [Btu/W-h]')

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
pdchCoolCoilDesCap  = newPreDefColumn(pdstCoolCoil,'Design Coil Load [W]')
pdchCoolCoilTotCap  = newPreDefColumn(pdstCoolCoil,'Nominal Total Capacity [W]')
pdchCoolCoilSensCap = newPreDefColumn(pdstCoolCoil,'Nominal Sensible Capacity [W]')
pdchCoolCoilLatCap  = newPreDefColumn(pdstCoolCoil,'Nominal Latent Capacity [W]')
pdchCoolCoilSHR     = newPreDefColumn(pdstCoolCoil,'Nominal Sensible Heat Ratio')
pdchCoolCoilNomEff  = newPreDefColumn(pdstCoolCoil,'Nominal Efficiency [W/W]')
pdchCoolCoilUATotal = newPreDefColumn(pdstCoolCoil,'Nominal Coil UA Value [W/C]')
pdchCoolCoilArea    = newPreDefColumn(pdstCoolCoil,'Nominal Coil Surface Area [m2]')

pdstDXCoolCoil = newPreDefSubTable(pdrEquip,'DX Cooling Coils')
pdchDXCoolCoilType     = newPreDefColumn(pdstDXCoolCoil,'DX Cooling Coil Type')
pdchDXCoolCoilNetCapSI = newPreDefColumn(pdstDXCoolCoil,'Standard Rated Net Cooling Capacity [W]')

pdchDXCoolCoilCOP      = newPreDefColumn(pdstDXCoolCoil, 'Standard Rated Net COP [W/W]')
pdchDXCoolCoilEERIP    = newPreDefColumn(pdstDXCoolCoil,'EER [Btu/W-h]')
pdchDXCoolCoilSEERIP   = newPreDefColumn(pdstDXCoolCoil,'SEER [Btu/W-h]')
pdchDXCoolCoilIEERIP   = newPreDefColumn(pdstDXCoolCoil,'IEER [Btu/W-h]')


pdstDXHeatCoil          = newPreDefSubTable(pdrEquip,'DX Heating Coils')
pdchDXHeatCoilType      = newPreDefColumn(pdstDXHeatCoil,'DX Heating Coil Type')
pdchDXHeatCoilHighCap   = newPreDefColumn(pdstDXHeatCoil,'High Temperature Heating (net) Rating Capacity [W]')
pdchDXHeatCoilLowCap    = newPreDefColumn(pdstDXHeatCoil,'Low Temperature Heating (net) Rating Capacity [W]')
pdchDXHeatCoilHSPFIP    = newPreDefColumn(pdstDXHeatCoil,'HSPF [Btu/W-h]')
pdchDXHeatCoilRegionNum = newPreDefColumn(pdstDXHeatCoil,'Region Number')

pdstHeatCoil = newPreDefSubTable(pdrEquip,'Heating Coils')

pdchHeatCoilType   = newPreDefColumn(pdstHeatCoil,'Type')
pdchHeatCoilDesCap = newPreDefColumn(pdstHeatCoil,'Design Coil Load [W]')
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

pdrSizing =     newPreDefReport('HVACSizingSummary','Size','HVAC Sizing Summary')

pdstZoneClSize =  newPreDefSubTable(pdrSizing,'Zone Cooling')

pdchZnClCalcDesLd     = newPreDefColumn(pdstZoneClSize,'Calculated Design Load [W]')
pdchZnClUserDesLd     = newPreDefColumn(pdstZoneClSize,'User Design Load [W]')
pdchZnClUserDesLdPerArea = newPreDefColumn(pdstZoneClSize,'User Design Load per Area [W/m2]')
pdchZnClCalcDesAirFlow = newPreDefColumn(pdstZoneClSize,'Calculated Design Air Flow [m3/s]')
pdchZnClUserDesAirFlow = newPreDefColumn(pdstZoneClSize,'User Design Air Flow [m3/s]')
pdchZnClDesDay         = newPreDefColumn(pdstZoneClSize,'Design Day Name')
pdchZnClPkTime         = newPreDefColumn(pdstZoneClSize,'Date/Time Of Peak')
pdchZnClPkTstatTemp    = newPreDefColumn(pdstZoneClSize,'Thermostat Setpoint Temperature at Peak Load [C]')
pdchZnClPkIndTemp      = newPreDefColumn(pdstZoneClSize,'Indoor Temperature at Peak Load [C]')
pdchZnClPkIndHum       = newPreDefColumn(pdstZoneClSize,'Indoor Humidity Ratio at Peak Load [kgWater/kgAir]')
pdchZnClPkOATemp       = newPreDefColumn(pdstZoneClSize,'Outdoor Temperature at Peak Load [C]')
pdchZnClPkOAHum        = newPreDefColumn(pdstZoneClSize,'Outdoor Humidity Ratio at Peak Load [kgWater/kgAir]')

pdstZoneHtSize =  newPreDefSubTable(pdrSizing,'Zone Heating')

pdchZnHtCalcDesLd      = newPreDefColumn(pdstZoneHtSize,'Calculated Design Load [W]')
pdchZnHtUserDesLd      = newPreDefColumn(pdstZoneHtSize,'User Design Load [W]')
pdchZnHtUserDesLdPerArea = newPreDefColumn(pdstZoneHtSize,'User Design Load per Area [W/m2]')
pdchZnHtCalcDesAirFlow = newPreDefColumn(pdstZoneHtSize,'Calculated Design Air Flow [m3/s]')
pdchZnHtUserDesAirFlow = newPreDefColumn(pdstZoneHtSize,'User Design Air Flow [m3/s]')
pdchZnHtDesDay         = newPreDefColumn(pdstZoneHtSize,'Design Day Name')
pdchZnHtPkTime         = newPreDefColumn(pdstZoneHtSize,'Date/Time Of Peak')
pdchZnHtPkTstatTemp    = newPreDefColumn(pdstZoneHtSize,'Thermostat Setpoint Temperature at Peak Load [C]')
pdchZnHtPkIndTemp      = newPreDefColumn(pdstZoneHtSize,'Indoor Temperature at Peak Load [C]')
pdchZnHtPkIndHum       = newPreDefColumn(pdstZoneHtSize,'Indoor Humidity Ratio at Peak Load [kgWater/kgAir]')
pdchZnHtPkOATemp       = newPreDefColumn(pdstZoneHtSize,'Outdoor Temperature at Peak Load [C]')
pdchZnHtPkOAHum        = newPreDefColumn(pdstZoneHtSize,'Outdoor Humidity Ratio at Peak Load [kgWater/kgAir]')

pdstSystemSize  =  newPreDefSubTable(pdrSizing,'System Design Air Flow Rates')

pdchSysSizCalcClAir    = newPreDefColumn(pdstSystemSize,'Calculated cooling [m3/s]')
pdchSysSizUserClAir    = newPreDefColumn(pdstSystemSize,'User cooling [m3/s]')
pdchSysSizCalcHtAir    = newPreDefColumn(pdstSystemSize,'Calculated heating [m3/s]')
pdchSysSizUserHtAir    = newPreDefColumn(pdstSystemSize,'User heating [m3/s]')

! System Summary Report

pdrSystem =  newPreDefReport('SystemSummary','Sys','System Summary')

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
pdrOutsideAir = newPreDefReport('OutdoorAirSummary','OA','Outdoor Air Summary')

pdstOAavgOcc = newPreDefSubTable(pdrOutsideAir, 'Average Outdoor Air During Occupied Hours')

pdchOaoAvgNumOcc1 =   newPreDefColumn(pdstOAavgOcc,'Average Number of Occupants')
pdchOaoNomNumOcc1 =   newPreDefColumn(pdstOAavgOcc,'Nominal Number of Occupants')
pdchOaoZoneVol1 =     newPreDefColumn(pdstOAavgOcc,'Zone Volume [m3]')
pdchOaoAvgMechVent =  newPreDefColumn(pdstOAavgOcc,'Mechanical Ventilation [ach]')
pdchOaoAvgInfil =     newPreDefColumn(pdstOAavgOcc,'Infiltration [ach]')
pdchOaoAvgAFNInfil =  newPreDefColumn(pdstOAavgOcc,'AFN Infiltration [ach]')
pdchOaoAvgSimpVent =  newPreDefColumn(pdstOAavgOcc,'Simple Ventilation [ach]')
!pdchOaoAvgTotVent =   newPreDefColumn(pdstOAavgOcc,'Total Ventilation [ach]')

CALL addFootNoteSubTable(pdstOAavgOcc,'Values shown for a single zone without multipliers')

pdstOAminOcc = newPreDefSubTable(pdrOutsideAir, 'Minimum Outdoor Air During Occupied Hours')

pdchOaoAvgNumOcc2 =   newPreDefColumn(pdstOAminOcc,'Average Number of Occupants')
pdchOaoNomNumOcc2 =   newPreDefColumn(pdstOAminOcc,'Nominal Number of Occupants')
pdchOaoZoneVol2 =     newPreDefColumn(pdstOAminOcc,'Zone Volume [m3]')
pdchOaoMinMechVent =  newPreDefColumn(pdstOAminOcc,'Mechanical Ventilation [ach]')
pdchOaoMinInfil =     newPreDefColumn(pdstOAminOcc,'Infiltration [ach]')
pdchOaoMinAFNInfil =  newPreDefColumn(pdstOAminOcc,'AFN Infiltration [ach]')
pdchOaoMinSimpVent =  newPreDefColumn(pdstOAminOcc,'Simple Ventilation [ach]')
!pdchOaoMinTotVent =   newPreDefColumn(pdstOAminOcc,'Total Ventilation [ach]')
CALL addFootNoteSubTable(pdstOAminOcc,'Values shown for a single zone without multipliers')

! Object Count Report
pdrObjCnt = newPreDefReport('ObjectCountSummary','Count','Object Count Summary')

pdstSurfCnt = newPreDefSubTable(pdrObjCnt,'Surfaces by Class')
pdchSurfCntTot =  newPreDefColumn(pdstSurfCnt,'Total')
pdchSurfCntExt =  newPreDefColumn(pdstSurfCnt,'Outdoors')

pdstHVACcnt = newPreDefSubTable(pdrObjCnt,'HVAC')
pdchHVACcntVal =  newPreDefColumn(pdstHVACcnt,'Count')

pdstFieldCnt = newPreDefSubTable(pdrObjCnt,'Input Fields')
pdchFieldCntVal =  newPreDefColumn(pdstFieldCnt,'Count')

! Energy Meters report
pdrEnergyMeters = newPreDefReport('EnergyMeters','Meters','Energy Meters')

!pdstEMvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values')
!pdchEMannual = newPreDefColumn(pdstEMvalues,'Annual Value [GJ]')
!pdchEMminvalue = newPreDefColumn(pdstEMvalues,'Minimum Value [J]')
!pdchEMminvaluetime = newPreDefColumn(pdstEMvalues,'Timestamp of Minimum')
!pdchEMmaxvalue = newPreDefColumn(pdstEMvalues,'Maximum Value [J]')
!pdchEMmaxvaluetime = newPreDefColumn(pdstEMvalues,'Timestamp of Maximum')
! Electricity Sub Table
pdstEMelecvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Electricity')
pdchEMelecannual = newPreDefColumn(pdstEMelecvalues,'Electricity Annual Value [GJ]')
pdchEMelecminvalue = newPreDefColumn(pdstEMelecvalues,'Electricity Minimum Value [W]')
pdchEMelecminvaluetime = newPreDefColumn(pdstEMelecvalues,'Timestamp of Minimum')
pdchEMelecmaxvalue = newPreDefColumn(pdstEMelecvalues,'Electricity Maximum Value [W]')
pdchEMelecmaxvaluetime = newPreDefColumn(pdstEMelecvalues,'Timestamp of Maximum')

! Gas Sub Table
pdstEMgasvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Gas')
pdchEMgasannual = newPreDefColumn(pdstEMgasvalues,'Gas Annual Value [GJ]')
pdchEMgasminvalue = newPreDefColumn(pdstEMgasvalues,'Gas Minimum Value [W]')
pdchEMgasminvaluetime = newPreDefColumn(pdstEMgasvalues,'Timestamp of Minimum')
pdchEMgasmaxvalue = newPreDefColumn(pdstEMgasvalues,'Gas Maximum Value [W]')
pdchEMgasmaxvaluetime = newPreDefColumn(pdstEMgasvalues,'Timestamp of Maximum')

! Cool SubTable
pdstEMcoolvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Cooling')
pdchEMcoolannual = newPreDefColumn(pdstEMcoolvalues,'Cooling Annual Value [GJ]')
pdchEMcoolminvalue = newPreDefColumn(pdstEMcoolvalues,'Cooling Minimum Value [W]')
pdchEMcoolminvaluetime = newPreDefColumn(pdstEMcoolvalues,'Timestamp of Minimum')
pdchEMcoolmaxvalue = newPreDefColumn(pdstEMcoolvalues,'Cooling Maximum Value [W]')
pdchEMcoolmaxvaluetime = newPreDefColumn(pdstEMcoolvalues,'Timestamp of Maximum')

! Water SubTable
pdstEMwatervalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Water')
pdchEMwaterannual = newPreDefColumn(pdstEMwatervalues,'Annual Value [m3]')
pdchEMwaterminvalue = newPreDefColumn(pdstEMwatervalues,'Minimum Value [m3/s]')
pdchEMwaterminvaluetime = newPreDefColumn(pdstEMwatervalues,'Timestamp of Minimum')
pdchEMwatermaxvalue = newPreDefColumn(pdstEMwatervalues,'Maximum Value [m3/s]')
pdchEMwatermaxvaluetime = newPreDefColumn(pdstEMwatervalues,'Timestamp of Maximum')

! Other KG SubTable
pdstEMotherKGvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Other by Weight/Mass')
pdchEMotherKGannual = newPreDefColumn(pdstEMotherKGvalues,'Annual Value [kg]')
pdchEMotherKGminvalue = newPreDefColumn(pdstEMotherKGvalues,'Minimum Value [kg/s]')
pdchEMotherKGminvaluetime = newPreDefColumn(pdstEMotherKGvalues,'Timestamp of Minimum')
pdchEMotherKGmaxvalue = newPreDefColumn(pdstEMotherKGvalues,'Maximum Value [kg/s]')
pdchEMotherKGmaxvaluetime = newPreDefColumn(pdstEMotherKGvalues,'Timestamp of Maximum')

! Other M3 SubTable
pdstEMotherM3values = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Other Volumetric')
pdchEMotherM3annual = newPreDefColumn(pdstEMotherM3values,'Annual Value [m3]')
pdchEMotherM3minvalue = newPreDefColumn(pdstEMotherM3values,'Minimum Value [m3/s]')
pdchEMotherM3minvaluetime = newPreDefColumn(pdstEMotherM3values,'Timestamp of Minimum')
pdchEMotherM3maxvalue = newPreDefColumn(pdstEMotherM3values,'Maximum Value [m3/s]')
pdchEMotherM3maxvaluetime = newPreDefColumn(pdstEMotherM3values,'Timestamp of Maximum')

! Other M3 SubTable
pdstEMotherLvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Other Liquid/Gas')
pdchEMotherLannual = newPreDefColumn(pdstEMotherLvalues,'Annual Value [L]')
pdchEMotherLminvalue = newPreDefColumn(pdstEMotherLvalues,'Minimum Value [L]')
pdchEMotherLminvaluetime = newPreDefColumn(pdstEMotherLvalues,'Timestamp of Minimum')
pdchEMotherLmaxvalue = newPreDefColumn(pdstEMotherLvalues,'Maximum Value [L]')
pdchEMotherLmaxvaluetime = newPreDefColumn(pdstEMotherLvalues,'Timestamp of Maximum')

! Other J SubTable
pdstEMotherJvalues = newPreDefSubTable(pdrEnergyMeters,'Annual and Peak Values - Other')
pdchEMotherJannual = newPreDefColumn(pdstEMotherJvalues,'Annual Value [GJ]')
pdchEMotherJminvalue = newPreDefColumn(pdstEMotherJvalues,'Minimum Value [W]')
pdchEMotherJminvaluetime = newPreDefColumn(pdstEMotherJvalues,'Timestamp of Minimum')
pdchEMotherJmaxvalue = newPreDefColumn(pdstEMotherJvalues,'Maximum Value [W]')
pdchEMotherJmaxvaluetime = newPreDefColumn(pdstEMotherJvalues,'Timestamp of Maximum')

! Sensible Heat Gas Component Report
pdrSensibleGain = newPreDefReport('SensibleHeatGainSummary','SHGS','Sensible Heat Gain Summary')

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


!Standard62Report
IF (DoZoneSizing .OR. DoSystemSizing) THEN
  pdrStd62 = newPreDefReport('Standard62.1Summary','Std62','Standard 62.1 Summary')

  pdstS62sysVentReqCool = newPreDefSubTable(pdrStd62, 'System Ventilation Requirements for Cooling')

  pdchS62svrClSumVpz =    newPreDefColumn(pdstS62sysVentReqCool,'Sum of Zone Primary Air Flow - Vpz-sum [m3/s]')
  pdchS62svrClPs =        newPreDefColumn(pdstS62sysVentReqCool,'System Population - Ps')
  pdchS62svrClSumPz =     newPreDefColumn(pdstS62sysVentReqCool,'Sum of Zone Population - Pz-sum')
  pdchS62svrClD =         newPreDefColumn(pdstS62sysVentReqCool,'Occupant Diversity - D')
  pdchS62svrClVou =       newPreDefColumn(pdstS62sysVentReqCool,'Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]')
  pdchS62svrClVps =       newPreDefColumn(pdstS62sysVentReqCool,'System Primary Airflow - Vps [m3/s]')
  pdchS62svrClXs =        newPreDefColumn(pdstS62sysVentReqCool,'Average Outdoor Air Fraction - Xs')
  pdchS62svrClEv =        newPreDefColumn(pdstS62sysVentReqCool,'System Ventilation Efficiency - Ev')
  pdchS62svrClVot =       newPreDefColumn(pdstS62sysVentReqCool,'Outdoor Air Intake Flow - Vot [m3/s]')
  pdchS62svrClPercOA =    newPreDefColumn(pdstS62sysVentReqCool,'Percent Outdoor Air - %OA')

  pdstS62sysVentReqHeat = newPreDefSubTable(pdrStd62, 'System Ventilation Requirements for Heating')

  pdchS62svrHTSumVpz =    newPreDefColumn(pdstS62sysVentReqHeat,'Sum of Zone Primary Air Flow - Vpz-sum [m3/s]')
  pdchS62svrHtPs =        newPreDefColumn(pdstS62sysVentReqHeat,'System Population - Ps')
  pdchS62svrHtSumPz =     newPreDefColumn(pdstS62sysVentReqHeat,'Sum of Zone Population - Pz-sum')
  pdchS62svrHtD =         newPreDefColumn(pdstS62sysVentReqHeat,'Occupant Diversity - D')
  pdchS62svrHtVou =       newPreDefColumn(pdstS62sysVentReqHeat,'Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]')
  pdchS62svrHtVps =       newPreDefColumn(pdstS62sysVentReqHeat,'System Primary Airflow - Vps [m3/s]')
  pdchS62svrHtXs =        newPreDefColumn(pdstS62sysVentReqHeat,'Average Outdoor Air Fraction - Xs')
  pdchS62svrHtEv =        newPreDefColumn(pdstS62sysVentReqHeat,'System Ventilation Efficiency - Ev')
  pdchS62svrHtVot =       newPreDefColumn(pdstS62sysVentReqHeat,'Outdoor Air Intake Flow Vot [m3/s]')
  pdchS62svrHtPercOA =    newPreDefColumn(pdstS62sysVentReqHeat,'Percent Outdoor Air - %OA')

  pdstS62znVentPar =      newPreDefSubTable(pdrStd62, 'Zone Ventilation Parameters')

  pdchS62zvpAlN =         newPreDefColumn(pdstS62znVentPar,'AirLoop Name')
  pdchS62zvpRp =          newPreDefColumn(pdstS62znVentPar,'People Outdoor Air Rate - Rp [m3/s-person]')
  pdchS62zvpPz =          newPreDefColumn(pdstS62znVentPar,'Zone Population - Pz')
  pdchS62zvpRa =          newPreDefColumn(pdstS62znVentPar,'Area Outdoor Air Rate - Ra [m3/s-m2]')
  pdchS62zvpAz =          newPreDefColumn(pdstS62znVentPar,'Zone Floor Area - Az [m2]')
  pdchS62zvpVbz =         newPreDefColumn(pdstS62znVentPar,'Breathing Zone Outdoor Airflow - Vbz [m3/s]')
  pdchS62zvpClEz =        newPreDefColumn(pdstS62znVentPar,'Cooling Zone Air Distribution Effectiveness - Ez-clg')
  pdchS62zvpClVoz =       newPreDefColumn(pdstS62znVentPar,'Cooling Zone Outdoor Airflow - Voz-clg [m3/s]')
  pdchS62zvpHtEz =        newPreDefColumn(pdstS62znVentPar,'Heating Zone Air Distribution Effectiveness - Ez-htg')
  pdchS62zvpHtVoz =       newPreDefColumn(pdstS62znVentPar,'Heating Zone Outdoor Airflow - Voz-htg [m3/s]')

  pdstS62sysVentPar =     newPreDefSubTable(pdrStd62, 'System Ventilation Parameters')

  pdchS62svpRp =          newPreDefColumn(pdstS62sysVentPar,'People Outdoor Air Rate - Rp [m3/s-person]')
  pdchS62svpPz =          newPreDefColumn(pdstS62sysVentPar,'Sum of Zone Population - Pz-sum')
  pdchS62svpRa =          newPreDefColumn(pdstS62sysVentPar,'Area Outdoor Air Rate - Ra [m3/s-m2]')
  pdchS62svpAz =          newPreDefColumn(pdstS62sysVentPar,'Sum of Zone Floor Area - Az-sum [m2]')
  pdchS62svpVbz =         newPreDefColumn(pdstS62sysVentPar,'Breathing Zone Outdoor Airflow - Vbz [m3/s]')
  pdchS62svpClVoz =       newPreDefColumn(pdstS62sysVentPar,'Cooling Zone Outdoor Airflow - Voz-clg [m3/s]')
  pdchS62svpHtVoz =       newPreDefColumn(pdstS62sysVentPar,'Heating Zone Outdoor Airflow - Voz-htg [m3/s]')

  pdstS62znCoolDes =      newPreDefSubTable(pdrStd62, 'Zone Ventilation Calculations for Cooling Design')

  pdchS62zcdAlN =         newPreDefColumn(pdstS62znCoolDes,'AirLoop Name')
  pdchS62zcdBox =         newPreDefColumn(pdstS62znCoolDes,'Box Type')
  pdchS62zcdVpz =         newPreDefColumn(pdstS62znCoolDes,'Zone Primary Airflow - Vpz [m3/s]')
  !pdchS62zcdVps =         newPreDefColumn(pdstS62znCoolDes,'System Primary Airflow - Vps [m3/s]')
  !pdchS62zcdVsec =        newPreDefColumn(pdstS62znCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
  pdchS62zcdVdz =         newPreDefColumn(pdstS62znCoolDes,'Zone Discharge Airflow - Vdz [m3/s]')
  pdchS62zcdVpzmin =      newPreDefColumn(pdstS62znCoolDes,'Minimum Zone Primary Airflow - Vpz-min [m3/s]')
  pdchS62zcdVozclg =      newPreDefColumn(pdstS62znCoolDes,'Zone Outdoor Airflow Cooling - Voz-clg [m3/s]')
  pdchS62zcdZpz =         newPreDefColumn(pdstS62znCoolDes,'Primary Outdoor Air Fraction - Zpz')
  pdchS62zcdEp =          newPreDefColumn(pdstS62znCoolDes,'Primary Air Fraction - Ep')
  pdchS62zcdEr =          newPreDefColumn(pdstS62znCoolDes,'Secondary Recirculation Fraction- Er')
  pdchS62zcdFa =          newPreDefColumn(pdstS62znCoolDes,'Supply Air Fraction- Fa')
  pdchS62zcdFb =          newPreDefColumn(pdstS62znCoolDes,'Mixed Air Fraction - Fb')
  pdchS62zcdFc =          newPreDefColumn(pdstS62znCoolDes,'Outdoor Air Fraction - Fc')
  pdchS62zcdEvz =         newPreDefColumn(pdstS62znCoolDes,'Zone Ventilation Efficiency - Evz')

  pdstS62sysCoolDes =     newPreDefSubTable(pdrStd62, 'System Ventilation Calculations for Cooling Design')

  pdchS62scdVpz =         newPreDefColumn(pdstS62sysCoolDes,'Sum of Zone Primary Airflow - Vpz-sum [m3/s]')
  pdchS62scdVps =         newPreDefColumn(pdstS62sysCoolDes,'System Primary Airflow - Vps [m3/s]')
  !pdchS62scdVsec =        newPreDefColumn(pdstS62sysCoolDes,'Secondary Fan Airflow - Vsec [m3/s]')
  pdchS62scdVdz =         newPreDefColumn(pdstS62sysCoolDes,'Sum of Zone Discharge Airflow - Vdz-sum [m3/s]')
  pdchS62scdVpzmin =      newPreDefColumn(pdstS62sysCoolDes,'Sum of Min Zone Primary Airflow - Vpz-min [m3/s]')
  pdchS62scdVozclg =      newPreDefColumn(pdstS62sysCoolDes,'Zone Outdoor Airflow Cooling - Voz-clg [m3/s]')
  pdchS62scdEvz =         newPreDefColumn(pdstS62sysCoolDes,'Zone Ventilation Efficiency - Evz-min')

  pdstS62znHeatDes =      newPreDefSubTable(pdrStd62, 'Zone Ventilation Calculations for Heating Design')

  pdchS62zhdAlN =         newPreDefColumn(pdstS62znHeatDes,'AirLoop Name')
  pdchS62zhdBox =         newPreDefColumn(pdstS62znHeatDes,'Box Type')
  pdchS62zhdVpz =         newPreDefColumn(pdstS62znHeatDes,'Zone Primary Airflow - Vpz [m3/s]')
  !pdchS62zhdVps =         newPreDefColumn(pdstS62znHeatDes,'System Primary Airflow - Vps [m3/s]')
  !pdchS62zhdVsec =        newPreDefColumn(pdstS62znHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
  pdchS62zhdVdz =         newPreDefColumn(pdstS62znHeatDes,'Zone Discharge Airflow - Vdz [m3/s]')
  pdchS62zhdVpzmin =      newPreDefColumn(pdstS62znHeatDes,'Minimum Zone Primary Airflow - Vpz-min [m3/s]')
  pdchS62zhdVozhtg =      newPreDefColumn(pdstS62znHeatDes,'Zone Outdoor Airflow Heating - Voz-htg [m3/s]')
  pdchS62zhdZpz =         newPreDefColumn(pdstS62znHeatDes,'Primary Outdoor Air Fraction - Zpz')
  pdchS62zhdEp =          newPreDefColumn(pdstS62znHeatDes,'Primary Air Fraction - Ep')
  pdchS62zhdEr =          newPreDefColumn(pdstS62znHeatDes,'Secondary Recirculation Fraction- Er')
  pdchS62zhdFa =          newPreDefColumn(pdstS62znHeatDes,'Supply Air Fraction- Fa')
  pdchS62zhdFb =          newPreDefColumn(pdstS62znHeatDes,'Mixed Air Fraction - Fb')
  pdchS62zhdFc =          newPreDefColumn(pdstS62znHeatDes,'Outdoor Air Fraction - Fc')
  pdchS62zhdEvz =         newPreDefColumn(pdstS62znHeatDes,'Zone Ventilation Efficiency - Evz')

  pdstS62sysHeatDes =     newPreDefSubTable(pdrStd62, 'System Ventilation Calculations for Heating Design')

  pdchS62shdVpz =         newPreDefColumn(pdstS62sysHeatDes,'Sum of Zone Primary Airflow - Vpz-sum [m3/s]')
  pdchS62shdVps =         newPreDefColumn(pdstS62sysHeatDes,'System Primary Airflow - Vps [m3/s]')
  !pdchS62shdVsec =        newPreDefColumn(pdstS62sysHeatDes,'Secondary Fan Airflow - Vsec [m3/s]')
  pdchS62shdVdz =         newPreDefColumn(pdstS62sysHeatDes,'Sum of Zone Discharge Airflow - Vdz-sum [m3/s]')
  pdchS62shdVpzmin =      newPreDefColumn(pdstS62sysHeatDes,'Sum of Min Zone Primary Airflow - Vpz-min [m3/s]')
  pdchS62shdVozhtg =      newPreDefColumn(pdstS62sysHeatDes,'Zone Outdoor Airflow Heating - Voz-htg [m3/s]')
  pdchS62shdEvz =         newPreDefColumn(pdstS62sysHeatDes,'Zone Ventilation Efficiency - Evz-min')
END IF

pdrLeed =  newPreDefReport('LEEDsummary','LEED','LEED Summary')

pdstLeedGenInfo = newPreDefSubTable(pdrLeed, 'Sec1.1A-General Information')
! single column with rows of:
!    Principal Heating Source
!    Weather File
!    Climate Zone
!    Heating Degree Days
!    Cooling Degree Days
!    HDD and CDD data source
!    Total gross floor area
pdchLeedGenData = newPreDefColumn(pdstLeedGenInfo,'Data')

pdstLeedSpaceUsageType =  newPreDefSubTable(pdrLeed, 'EAp2-1. Space Usage Type')
pdchLeedSutSpArea = newPreDefColumn(pdstLeedSpaceUsageType,'Space Area [m2]')
pdchLeedSutOcArea = newPreDefColumn(pdstLeedSpaceUsageType,'Regularly Occupied Area [m2]')
pdchLeedSutUnArea = newPreDefColumn(pdstLeedSpaceUsageType,'Unconditioned Area [m2]')
pdchLeedSutHrsWeek = newPreDefColumn(pdstLeedSpaceUsageType,'Typical Hours/Week in Operation [hr/wk]')

pdstLeedAdvsMsg =  newPreDefSubTable(pdrLeed, 'EAp2-2. Advisory Messages')
! single column with rows of:
!    Number of hours heating loads not met
!    Number of hours cooling loads not met
!    Total
!    Difference
!    Number of warning messages
!    Number of error messages
!    Number of defaults overridden
pdchLeedAmData = newPreDefColumn(pdstLeedAdvsMsg,'Data')

pdstLeedEneTypSum =  newPreDefSubTable(pdrLeed, 'EAp2-3. Energy Type Summary')
! multiple columns with rows of
!    Electricity
!    Natural Gas
!    <additional fuels>
pdchLeedEtsRtNm = newPreDefColumn(pdstLeedEneTypSum,'Utility Rate')
pdchLeedEtsVirt = newPreDefColumn(pdstLeedEneTypSum,'Virtual Rate [$/unit energy]')
pdchLeedEtsEneUnt = newPreDefColumn(pdstLeedEneTypSum,'Units of Energy')
pdchLeedEtsDemUnt = newPreDefColumn(pdstLeedEneTypSum,'Units of Demand')

pdstLeedPerf =  newPreDefSubTable(pdrLeed, 'EAp2-4/5. Performance Rating Method Compliance')
! Multiple colums with rows of:
!     Interior Lighting
!     Exterior Lighting
!     Space Heating
!     Space Cooling
!     Pumps
!     Heat Rejection
!     Fans-Interior
!     Fans-Parking Garage
!     Service Water Heating
!     Receptacle Equipment
!     Interior Lighting (process)
!     Refrigeration Equipment
!     Cooking
!     Industrial Process
!     Elevators and Escalators
!     Total
pdchLeedPerfElEneUse = newPreDefColumn(pdstLeedPerf,'Electric Energy Use [GJ]')
pdchLeedPerfElDem = newPreDefColumn(pdstLeedPerf,'Electric Demand [W]')
pdchLeedPerfGasEneUse = newPreDefColumn(pdstLeedPerf,'Natural Gas Energy Use [GJ]')
pdchLeedPerfGasDem = newPreDefColumn(pdstLeedPerf,'Natural Gas Demand [W]')
pdchLeedPerfOthEneUse = newPreDefColumn(pdstLeedPerf,'Additional Energy Use [GJ]')
pdchLeedPerfOthDem = newPreDefColumn(pdstLeedPerf,'Additional Demand [W]')

pdstLeedEneUseSum =  newPreDefSubTable(pdrLeed, 'EAp2-6. Energy Use Summary')
! Multiple columns with rows of:
!    Electricity
!    Natural Gas
!    <additional fuels>
!    Total
pdchLeedEusProc = newPreDefColumn(pdstLeedEneUseSum,'Process Subtotal [GJ]')
pdchLeedEusTotal = newPreDefColumn(pdstLeedEneUseSum,'Total Energy Use [GJ]')

pdstLeedEneCostSum =  newPreDefSubTable(pdrLeed, 'EAp2-7. Energy Cost Summary')
! Multiple columns with rows of:
!    Electricity
!    Natural Gas
!    <additional fuels>
!    Total
pdchLeedEcsProc = newPreDefColumn(pdstLeedEneCostSum,'Process Subtotal [$]')
pdchLeedEcsTotal = newPreDefColumn(pdstLeedEneCostSum,'Total Energy Cost [$]')

pdstLeedRenewSum =  newPreDefSubTable(pdrLeed, 'L-1. Renewable Energy Source Summary')
! Multiple columns with rows of each renewable source
pdchLeedRenRatCap = newPreDefColumn(pdstLeedRenewSum,'Rated Capacity [kW]')
pdchLeedRenAnGen = newPreDefColumn(pdstLeedRenewSum,'Annual Energy Generated [GJ]')

pdstLeedEneUseIntEl =  newPreDefSubTable(pdrLeed, 'EAp2-17a. Energy Use Intensity - Electricity')
! Single column with rows of:
!    Interior lighting
!    Space heating
!    Space cooling
!    Fans-interior
!    Service water heating
!    Receptacle equipment
!    Miscellaneous
!    Subtotal
pdchLeedEuiElec = newPreDefColumn(pdstLeedEneUseIntEl,'Electricty [MJ/m2]')

pdstLeedEneUseIntNatG =  newPreDefSubTable(pdrLeed, 'EAp2-17b. Energy Use Intensity - Natural Gas')
! Single column with rows of:
!    Space heating
!    Service water heating
!    Miscellaneous
!    Subtotal
pdchLeedEuiNatG = newPreDefColumn(pdstLeedEneUseIntNatG,'Natural Gas [MJ/m2]')

pdstLeedEneUseIntOthr =  newPreDefSubTable(pdrLeed, 'EAp2-17c. Energy Use Intensity - Additional')
! Single column with rows of:
!    Miscellaneous
!    Subtotal
pdchLeedEuiOthr = newPreDefColumn(pdstLeedEneUseIntOthr,'Additional [MJ/m2]')

pdstLeedEneUsePerc =  newPreDefSubTable(pdrLeed, 'EAp2-18. End Use Percentage')
! single column with rows of:
!    Interior Lighting
!    Space heating
!    Space cooling
!    Fans-Interior
!    Service Water Heating
!    Receptacle Equipment
!    Miscellaneous
pdchLeedEupPerc = newPreDefColumn(pdstLeedEneUsePerc,'Percent [%]')


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


INTEGER FUNCTION newPreDefReport(inReportName,inReportAbrev,inReportNamewithSpaces)
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
CHARACTER(len=*),INTENT(IN)  :: inReportNamewithSpaces

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
reportName(numReportName)%namewithspaces = inReportNamewithSpaces
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

END MODULE OutputReportPredefined
