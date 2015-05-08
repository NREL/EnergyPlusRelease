MODULE CostEstimateManager

          ! Module containing the routines dealing with the Cost Estimation capability of EnergyPlus

          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April-May 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! produce a construction cost estimate report based on
          ! input and certain building calculations by Energygplus

          ! METHODOLOGY EMPLOYED:
          ! Routine gets called once, Just before tabular reports.
          ! Cost Estimate objects are child objects that will inherit from
          ! other input objects.
          ! Uses a Line Item methaphor where each Cost Estimate object is a line
          ! Create report using utility subroutines taken from OutputReportTabular (by J.Glazer)

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, KickOffSimulation
USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowContinueError
USE DataCostEstimate

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE
PUBLIC  SimCostEstimate
PRIVATE GetCostEstimateInput
PRIVATE CalcCostEstimate

CONTAINS

SUBROUTINE SimCostEstimate

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   April 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Entry point; manage calls to other subroutines

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
  LOGICAL, SAVE :: GetCostInput=.true.

  IF (GetCostInput) THEN
    CALL GetCostEstimateInput
    GetCostInput=.false.
  ENDIF

  ! Need to add check Costs before this will work properly

  IF (KickOffSimulation) RETURN

  IF (DoCostEstimate) then

    CALL CalcCostEstimate

  ENDIF

  RETURN

END SUBROUTINE SimCostEstimate

SUBROUTINE GetCostEstimateInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   April 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get Cost Estimation object input.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem ! might also use FindItemInList
  USE DataIPShortCuts
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
  INTEGER                        :: Item    ! Item to be "gotten"
  INTEGER                        :: NumCostAdjust
  INTEGER                        :: NumRefAdjust
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine

  NumLineItems=GetNumObjectsFound('ComponentCost:LineItem')

  IF (NumLineItems == 0) THEN
    DoCostEstimate = .FALSE.
    Return
  ELSE
    DoCostEstimate = .TRUE.
!    WriteTabularFiles = .TRUE.
  ENDIF

  IF (.NOT. ALLOCATED(CostLineItem)) then
    ALLOCATE(CostLineItem(NumLineItems))
  ENDIF

  cCurrentModuleObject = 'ComponentCost:LineItem'

  DO Item=1,NumLineItems
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
    CostLineItem(item)%LineName           = cAlphaArgs(1)
    CostLineItem(item)%LineType           = cAlphaArgs(2)
    CostLineItem(item)%ParentObjType      = cAlphaArgs(3)
    CostLineItem(item)%ParentObjName      = cAlphaArgs(4)
    CostLineItem(item)%ParentObjKey       = cAlphaArgs(5)
    CostLineItem(item)%PerEach            = rNumericArgs(1)
    CostLineItem(item)%PerSquareMeter     = rNumericArgs(2)
    CostLineItem(item)%PerKiloWattCap     = rNumericArgs(3)
    CostLineItem(item)%PerKWCapPerCOP     = rNumericArgs(4)
    CostLineItem(item)%PerCubicMeter      = rNumericArgs(5)
    CostLineItem(item)%PerCubMeterPerSec  = rNumericArgs(6)
    CostLineItem(item)%PerUAinWattperDelK = rNumericArgs(7)
    CostLineItem(item)%Qty                = rNumericArgs(8)
!    CostLineItem(item)%AnnualMaintFract   = rNumericArgs(9)
!    CostLineItem(item)%MinorOverhallFract = rNumericArgs(10)
!    CostLineItem(item)%MinorOverhallYears = rNumericArgs(11)
!    CostLineItem(item)%MajorOverhallFract = rNumericArgs(12)
!    CostLineItem(item)%MajorOverhallYears = rNumericArgs(13)
!    CostLineItem(item)%LifeYears          = rNumericArgs(14)
!    CostLineItem(item)%ValueAtReplacement = rNumericArgs(15)
  ENDDO

  !most input error checking to be performed later within Case construct in Calc routine.
  ! do inits that aren't in a derived type
  NumCostAdjust = 0
  NumRefAdjust  = 0

  cCurrentModuleObject = 'ComponentCost:Adjustments'
  NumCostAdjust = GetNumObjectsFound(cCurrentModuleObject)
  If (NumCostAdjust == 1) then
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
    CurntBldg%MiscCostperSqMeter  = rNumericArgs(1)
    CurntBldg%DesignFeeFrac       = rNumericArgs(2)
    CurntBldg%ContractorFeeFrac   = rNumericArgs(3)
    CurntBldg%ContingencyFrac     = rNumericArgs(4)
    CurntBldg%BondCostFrac        = rNumericArgs(5)
    CurntBldg%CommissioningFrac   = rNumericArgs(6)
    CurntBldg%RegionalModifier    = rNumericArgs(7)

  ELSEIF (NumCostAdjust > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Only one instance of this object is allowed.')
    ErrorsFound = .TRUE.
  END IF

  cCurrentModuleObject = 'ComponentCost:Reference'
  NumRefAdjust = GetNumObjectsFound(cCurrentModuleObject)
  If (NumRefAdjust == 1) then
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
    RefrncBldg%LineItemTot         = rNumericArgs(1)
    RefrncBldg%MiscCostperSqMeter  = rNumericArgs(2)
    RefrncBldg%DesignFeeFrac       = rNumericArgs(3)
    RefrncBldg%ContractorFeeFrac   = rNumericArgs(4)
    RefrncBldg%ContingencyFrac     = rNumericArgs(5)
    RefrncBldg%BondCostFrac        = rNumericArgs(6)
    RefrncBldg%CommissioningFrac   = rNumericArgs(7)
    RefrncBldg%RegionalModifier    = rNumericArgs(8)

  ELSEIF (NumRefAdjust > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//' : Only one instance of this object is allowed.')
    ErrorsFound = .TRUE.
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing cost estimate input')
  ENDIF

  CALL CheckCostEstimateInput(ErrorsFound)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing cost estimate input')
  ENDIF

  RETURN

END SUBROUTINE GetCostEstimateInput

SUBROUTINE CheckCostEstimateInput(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   April 2004
          !       MODIFIED       February 2005, M. J. Witte
          !                        Add subscript to DX coil variables due to new multimode DX coil
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the Cost Estimate based on inputs.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces,      ONLY: Surface , TotSurfaces
  USE DataGlobals ,      ONLY: NumOfZones
  USE DataHeatBalance,   ONLY: Construct, TotConstructs, Lights, zone ,totLights
  USE InputProcessor,    ONLY: FindItem
  USE DXCoils       ,    ONLY: DXCoil, NumDXCoils
  USE PlantChillers ,  ONLY: ElectricChiller, numElectricChillers
  USE DataPhotovoltaics, ONLY: PVarray, NumPVs, NumSimplePVModuleTypes, iSimplePVModel
  USE DataDaylighting
  USE HeatingCoils,      ONLY: HeatingCoil, NumHeatingCoils
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! Set to true if errors in input, fatal at end of routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: Item                 ! do-loop counter for line items
  INTEGER             :: ThisConstructID      ! hold result of FindItem searching for Construct name
  INTEGER             :: ThisSurfID           ! hold result from findItem
  INTEGER             :: ThisZoneID           ! hold result from findItem

  CHARACTER(len=MaxNameLength) ::ThisConstructStr

!  LOGICAL,ALLOCATABLE, DIMENSION(:) :: uniqueSurfMask !
!  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: SurfMultipleARR
!  INTEGER             :: surf ! do-loop counter for checking for surfaces for uniqueness
  INTEGER             :: thisCoil ! index of named coil in its derived type
  LOGICAL             :: WildcardObjNames
  INTEGER             :: thisChil
  INTEGER             :: ThisPV
  REAL(r64)           :: Multipliers

  !Setup working data structure for line items
  DO Item=1,NumLineItems  !Loop thru cost line items

    CostLineItem(item)%LineNumber = item

    SELECT CASE   (CostLineItem(Item)%ParentObjType)

    Case  ('GENERAL')

    Case  ('CONSTRUCTION')

      !test input for problems
      !  is PerSquareMeter non-zero? if it is are other cost per values set?
      !   issue warning that 'Cost Estimate requested for Constructions with zero cost per unit area
      !
      IF (CostLineItem(item)%PerSquareMeter == 0) then
        CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(item)%LineName)//  &
           '" Construction object needs non-zero construction costs per square meter')
        ErrorsFound = .TRUE.
      ENDIF

      ThisConstructStr = CostLineItem(Item)%ParentObjName
      ThisConstructID = FindItem(ThisConstructStr, Construct%Name, TotConstructs)
      IF (ThisConstructID == 0) THEN ! do any surfaces have the specified construction? If not issue warning.
        CALL ShowWarningError('ComponentCost:LineItem: "'//trim(CostLineItem(item)%LineName)//  &
           '" Construction="'//trim(CostLineItem(Item)%ParentObjName)//  &
           '", no surfaces have the Construction specified')
        CALL ShowContinueError('No costs will be calculated for this Construction.')
!        ErrorsFound = .TRUE.
        CYCLE
      ENDIF

    CASE  ('COIL:DX','COIL:COOLING:DX:SINGLESPEED')
      WildcardObjNames =  .FALSE.
      thisCoil  = 0
      ! test if too many pricing methods are set in user input
      IF ((CostLineItem(Item)%PerKiloWattCap > 0.0d0) .and. (CostLineItem(Item)%perEach > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
            '", Coil:DX, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF
      IF ((CostLineItem(Item)%PerKiloWattCap > 0.0d0) .and. (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
            '", Coil:DX, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF
      IF ((CostLineItem(Item)%perEach > 0.0d0) .and. (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
            '", Coil:DX, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF
      !  check for wildcard * in object name..
      If (TRIM(CostLineItem(Item)%ParentObjName) == '*') then ! wildcard, apply to all such components
        WildcardObjNames = .TRUE.

      ELSEIF (CostLineItem(Item)%ParentObjName == '') then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
            '", Coil:DX: need to specify a Reference Object Name ')
        ErrorsFound = .TRUE.

      ELSE ! assume name is probably useful
        thisCoil = FindItem(CostLineItem(Item)%ParentObjName, DXCoil%Name, NumDXCoils)
        IF (thisCoil == 0) THEN
          CALL ShowWarningError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Coil:DX, invalid coil specified')
          CALL ShowContinueError('Coil Specified="'//trim(CostLineItem(Item)%ParentObjName)//  &
             '", calculations will not be completed for this item.')
        ENDIF
      ENDIF

    CASE ('COIL:HEATING:GAS')

      WildcardObjNames =  .FALSE.
      thisCoil  = 0
      ! test if too many pricing methods are set in user input
      IF ((CostLineItem(Item)%PerKiloWattCap > 0.0d0) .and. (CostLineItem(Item)%perEach > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Coil:Heating:Gas, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF
      IF ((CostLineItem(Item)%PerKiloWattCap > 0.0d0) .and. (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Coil:Heating:Gas, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF
      IF ((CostLineItem(Item)%perEach > 0.0d0) .and. (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Coil:Heating:Gas, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF
      !  check for wildcard * in object name..
      If (TRIM(CostLineItem(Item)%ParentObjName) == '*') then ! wildcard, apply to all such components
        WildcardObjNames = .TRUE.

      ELSEIF (CostLineItem(Item)%ParentObjName == '') then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Coil:Heating:Gas, need to specify a Reference Object Name')
        ErrorsFound = .TRUE.

      ELSE ! assume name is probably useful
        thisCoil = FindItem(CostLineItem(Item)%ParentObjName, HeatingCoil%Name, NumHeatingCoils)
        IF (thisCoil == 0) THEN
          CALL ShowWarningError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Coil:Heating:Gas, invalid coil specified')
          CALL ShowContinueError('Coil Specified="'//trim(CostLineItem(Item)%ParentObjName)//  &
             '", calculations will not be completed for this item.')
        ENDIF
      ENDIF

    CASE ('CHILLER:ELECTRIC')
      !
      IF (CostLineItem(Item)%ParentObjName == '') then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Chiller:Electric, need to specify a Reference Object Name')
        ErrorsFound = .TRUE.
      endif

      thisChil = FindItem(CostLineItem(Item)%ParentObjName, ElectricChiller%Base%Name, NumElectricChillers )
      IF (thisChil == 0) THEN
        CALL ShowWarningError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
           '", Chiller:Electric, invalid chiller specified.')
        CALL ShowContinueError('Chiller Specified="'//trim(CostLineItem(Item)%ParentObjName)//  &
           '", calculations will not be completed for this item.')
      ENDIF

    CASE ('DAYLIGHTING:CONTROLS')
      WildcardObjNames =  .FALSE.

      IF (TRIM(CostLineItem(Item)%ParentObjName) == '*') THEN ! wildcard, apply to all such components
        WildcardObjNames = .TRUE.
      ELSEIF (CostLineItem(Item)%ParentObjName == '') then
        CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Daylighting:Controls, need to specify a Reference Object Name')
        ErrorsFound = .TRUE.
      ELSE
        thisZoneID = FindItem(CostLineItem(Item)%ParentObjName, zone%name, NumOfZones)
        IF (thisZoneID > 0) THEN
          CostLineItem(item)%Qty  = ZoneDaylight(thisZoneID)%TotalDaylRefPoints
        ELSE
          CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
              '", Daylighting:Controls, need to specify a valid zone name')
          CALL ShowContinueError('Zone specified="'//trim(CostLineItem(Item)%ParentObjName)//'".')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

    CASE ('SHADING:ZONE:DETAILED')
      IF (CostLineItem(Item)%ParentObjName /= '') then
        ThisSurfID = FindItem(CostLineItem(Item)%ParentObjName, Surface%Name, TotSurfaces)
        IF (ThisSurfID > 0) THEN
          ThisZoneID = FindItem(Surface(ThisSurfID)%ZoneName,Zone%name,NumOfZones)
          IF (ThisZoneID == 0) THEN
            CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
                '", Shading:Zone:Detailed, need to specify a valid zone name')
            CALL ShowContinueError('Zone specified="'//trim(Surface(ThisSurfID)%ZoneName)//'".')
            ErrorsFound = .TRUE.
          ENDIF
        ELSE
          CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
              '", Shading:Zone:Detailed, need to specify a valid surface name')
          CALL ShowContinueError('Surface specified="'//trim(CostLineItem(item)%ParentObjName)//'".')
          ErrorsFound = .TRUE.
        ENDIF
      ELSE
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Shading:Zone:Detailed, specify a Reference Object Name')
        ErrorsFound = .TRUE.
      ENDIF

    CASE ('LIGHTS')

      IF ((CostLineItem(Item)%PerKiloWattCap > 0.0d0) .and. (CostLineItem(Item)%perEach > 0.0d0)) then
         CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
             '", Lights, too many pricing methods specified')
         ErrorsFound = .TRUE.
      ENDIF

      IF (CostLineItem(item)%PerKiloWattCap /= 0.0d0) THEN
        IF (CostLineItem(Item)%ParentObjName /= '') THEN
          ThisZoneID = FindItem(CostLineItem(item)%ParentObjName, Zone%Name, NumOfZones)
          IF (ThisZoneID == 0) THEN
            CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
                '", Lights, need to specify a valid zone name')
            CALL ShowContinueError('Zone specified="'//trim(CostLineItem(item)%ParentObjName)//'".')
            ErrorsFound = .TRUE.
          ENDIF
        ELSE
          CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
              '", Lights, need to specify a Reference Object Name')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

    CASE ('GENERATOR:PHOTOVOLTAIC')

      IF (CostLineItem(item)%PerKiloWattCap /= 0.0d0) THEN
        IF (CostLineItem(Item)%ParentObjName /= '') THEN
          ThisPV    = FindItem(CostLineItem(Item)%ParentObjName, PVArray%Name, NumPVs)
          IF (ThisPV > 0) THEN
            ThisZoneID = FindItem(Surface(PVArray(thisPV)%SurfacePtr)%ZoneName,Zone%name,NumOfZones)
            IF (ThisZoneID == 0) THEN
              Multipliers=1.0d0
            ELSE
              Multipliers=Zone(ThisZoneID)%Multiplier * Zone(ThisZoneID)%ListMultiplier
            ENDIF
            IF (PVArray(thisPV)%PVModelType /= iSimplePVModel) THen
              CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
                  '", Generator:Photovoltaic, only available for model type PhotovoltaicPerformance:Simple')
              ErrorsFound = .TRUE.
            ENDIF
          ELSE
            CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
                '", Generator:Photovoltaic, need to specify a valid PV array')
            CALL ShowContinueError('PV Array specified="'//trim(CostLineItem(item)%ParentObjName)//'".')
            ErrorsFound = .TRUE.
          ENDIF
        ELSE
          CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
              '", Generator:Photovoltaic, need to specify a Reference Object Name')
          ErrorsFound = .TRUE.
        ENDIF
      ELSE
        CALL ShowSevereError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
            '", Generator:Photovoltaic, need to specify a per-kilowatt cost ')
          ErrorsFound = .TRUE.
      ENDIF

      CASE DEFAULT
        CALL ShowWarningError('ComponentCost:LineItem: "'//trim(CostLineItem(Item)%LineName)//  &
            '", invalid cost item -- not included in cost estimate.')
        CALL ShowContinueError('... invalid object type='//trim(CostLineItem(Item)%ParentObjType))

    END SELECT

  ENDDO

  RETURN

END SUBROUTINE CheckCostEstimateInput

SUBROUTINE CalcCostEstimate

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   April 2004
          !       MODIFIED       February 2005, M. J. Witte
          !                        Add subscript to DX coil variables due to new multimode DX coil
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the Cost Estimate based on inputs.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces,      ONLY: Surface , TotSurfaces
  USE DataGlobals ,      ONLY: NumOfZones
  USE DataHeatBalance,   ONLY: Construct, TotConstructs, Lights, zone ,totLights
  USE InputProcessor,    ONLY: FindItem
  USE DXCoils       ,    ONLY: DXCoil, NumDXCoils
  USE PlantChillers ,  ONLY: ElectricChiller, numElectricChillers
  USE DataPhotovoltaics, ONLY: PVarray, NumPVs, NumSimplePVModuleTypes, iSimplePVModel
  USE DataDaylighting
  USE HeatingCoils,      ONLY: HeatingCoil, NumHeatingCoils
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
  INTEGER             :: Item                 ! do-loop counter for line items
  INTEGER             :: ThisConstructID      ! hold result of FindItem searching for Construct name
  INTEGER             :: ThisSurfID           ! hold result from findItem
  INTEGER             :: ThisZoneID           ! hold result from findItem

  CHARACTER(len=MaxNameLength) ::ThisConstructStr

  LOGICAL,ALLOCATABLE, DIMENSION(:) :: uniqueSurfMask !
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: SurfMultipleARR
  INTEGER             :: surf ! do-loop counter for checking for surfaces for uniqueness
  INTEGER             :: thisCoil ! index of named coil in its derived type
  LOGICAL             :: WildcardObjNames
  INTEGER             :: thisChil
  INTEGER             :: ThisPV
  REAL(r64)           :: Multipliers

  !Setup working data structure for line items
  DO Item=1,NumLineItems  !Loop thru cost line items

    CostLineItem(item)%LineNumber = item

    SELECT CASE   (CostLineItem(Item)%ParentObjType)

    Case  ('GENERAL')

      CostLineItem(item)%units        = 'Ea.'
      CostLineItem(item)%ValuePer     = CostLineItem(item)%PerEach
      CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer

    Case  ('CONSTRUCTION')

      ThisConstructStr = CostLineItem(Item)%ParentObjName
      ThisConstructID = FindItem(ThisConstructStr, Construct%Name, TotConstructs)
      ! need to determine unique surfacs... some surfaces are shared by zones and hence doubled
      ALLOCATE(uniqueSurfMask(TotSurfaces))
      uniqueSurfMask = .true.  !init to true and change duplicates to false
      AllOCATE(SurfMultipleARR(TotSurfaces))
      SurfMultipleARR = 1.0d0
      DO surf=1, TotSurfaces
        IF (surface(surf)%ExtBoundCond >= 1) THEN
          IF (surface(surf)%ExtBoundCond < surf) THEN !already cycled through
            uniqueSurfMask(surf) = .false.
          ENDIF
        ENDIF
        IF ( surface(surf)%Construction == 0) THEN  !throw out others for now
          uniqueSurfMask(surf) = .false.
        ENDIF
        IF (surface(surf)%Zone > 0 ) THEN
          SurfMultipleARR(surf) = zone(surface(surf)%Zone)%Multiplier * &
                                  zone(surface(surf)%Zone)%ListMultiplier

        ENDIF
      ENDDO
      !determine which surfaces have the construction type  and if any are duplicates..
      CostLineItem(item)%Qty          = sum(Surface%area*SurfMultipleARR, &
              mask=(uniqueSurfMask .AND. (surface%construction == ThisConstructID) ))
      CostLineItem(item)%units        = 'm2'
      CostLineItem(item)%ValuePer     = CostLineItem(item)%PerSquareMeter
      CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer

      deallocate(uniqueSurfMask)
      deallocate(SurfMultipleARR)

    CASE  ('COIL:DX','COIL:COOLING:DX:SINGLESPEED')
      WildcardObjNames =  .FALSE.
      thisCoil  = 0
      !  check for wildcard * in object name..
      If (TRIM(CostLineItem(Item)%ParentObjName) == '*') then ! wildcard, apply to all such components
        WildcardObjNames = .TRUE.
      ELSEIF (CostLineItem(Item)%ParentObjName /= '') then
        thisCoil = FindItem(CostLineItem(Item)%ParentObjName, DXCoil%Name, NumDXCoils)
      ENDIF

      If (CostLineItem(Item)%PerKiloWattCap > 0.0d0) then !
        If (WildCardObjNames) then
          CostLineItem(item)%Qty = sum(DXCoil%RatedTotCap(1))/1000.0d0
          CostLineItem(item)%units = 'kW (tot cool cap.)'
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        endif
        If (thisCoil > 0) then
          CostLineItem(item)%Qty = DXCoil(thisCoil)%RatedTotCap(1)/1000.0d0
          CostLineItem(item)%units = 'kW (tot cool cap.)'
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        endif
      endif

      If (CostLineItem(Item)%perEach > 0.0d0) then
          If (WildCardObjNames) CostLineItem(item)%Qty = REAL(NumDXCoils,r64)
          If (thisCoil > 0) CostLineItem(item)%Qty = 1.0d0
          CostLineItem(item)%ValuePer = CostLineItem(Item)%perEach
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
          CostLineItem(item)%units = 'Ea.'
      endif

      IF (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0) then
         If (WildCardObjNames) then
          CostLineItem(item)%Qty = sum(DXCoil%RatedCOP(1)*DXCoil%RatedTotCap(1))/1000.0d0
          CostLineItem(item)%units = 'kW*COP (total, rated) '
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKWCapPerCOP
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        ENDIF
        If (thisCoil > 0) then
          CostLineItem(item)%Qty = DXCoil(thisCoil)%RatedCOP(1) * DXCoil(thisCoil)%RatedTotCap(1)/1000.0d0
          CostLineItem(item)%units = 'kW*COP (total, rated) '
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKWCapPerCOP
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        ENDIF
      ENDIF

    CASE ('COIL:HEATING:GAS')
      WildcardObjNames =  .FALSE.
      thisCoil  = 0
      !  check for wildcard * in object name..
      If (TRIM(CostLineItem(Item)%ParentObjName) == '*') then ! wildcard, apply to all such components
        WildcardObjNames = .TRUE.
      ELSEIF (CostLineItem(Item)%ParentObjName /= '') then
        thisCoil = FindItem(CostLineItem(Item)%ParentObjName, HeatingCoil%Name, NumHeatingCoils)
      ENDIF

      If (CostLineItem(Item)%PerKiloWattCap > 0.0d0) then !
        If (WildCardObjNames) then

          CostLineItem(item)%Qty = sum(HeatingCoil%NominalCapacity, &
              mask=(HeatingCoil%HCoilType_Num == 1) )/1000.0d0
          CostLineItem(item)%units = 'kW (tot heat cap.)'
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        endif
        If (thisCoil > 0) then
          CostLineItem(item)%Qty = HeatingCoil(thisCoil)%NominalCapacity/1000.0d0
          CostLineItem(item)%units = 'kW (tot heat cap.)'
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        endif
      endif

      If (CostLineItem(Item)%perEach > 0.0d0) then
          If (WildCardObjNames) CostLineItem(item)%Qty = NumHeatingCoils
          If (thisCoil > 0) CostLineItem(item)%Qty = 1.0d0
          CostLineItem(item)%ValuePer = CostLineItem(Item)%perEach
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
          CostLineItem(item)%units = 'Ea.'
      endif

      IF (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0) then
         If (WildCardObjNames) then
          CostLineItem(item)%Qty = sum(HeatingCoil%Efficiency*HeatingCoil%NominalCapacity ,&
              mask=(HeatingCoil%HCoilType_Num == 1) )/1000.0d0
          CostLineItem(item)%units = 'kW*Eff (total, rated) '
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKWCapPerCOP
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        ENDIF
        If (thisCoil > 0) then
          CostLineItem(item)%Qty = HeatingCoil(thisCoil)%Efficiency*HeatingCoil(thisCoil)%NominalCapacity/1000.0d0
          CostLineItem(item)%units = 'kW*Eff (total, rated) '
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKWCapPerCOP
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
        ENDIF
      ENDIF

    CASE ('CHILLER:ELECTRIC')
      !
      thisChil = FindItem(CostLineItem(Item)%ParentObjName, ElectricChiller%Base%Name, NumElectricChillers )
      If ((thisChil > 0) .AND. (CostLineItem(Item)%PerKiloWattCap > 0.0d0)) then
          CostLineItem(item)%Qty = ElectricChiller(thisChil)%Base%NomCap/1000.0d0
          CostLineItem(item)%units = 'kW (tot cool cap.)'
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
      ENDIF
      If ((thisChil > 0) .AND. (CostLineItem(Item)%PerKWCapPerCOP > 0.0d0)) then
          CostLineItem(item)%Qty = ElectricChiller(thisChil)%Base%COP * ElectricChiller(thisChil)%Base%NomCap/1000.0d0
          CostLineItem(item)%units = 'kW*COP (total, rated) '
          CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKWCapPerCOP
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
      ENDIF
      IF ((thisChil > 0) .AND. (CostLineItem(Item)%PerEach > 0.0d0)) then
          CostLineItem(item)%Qty = 1.0d0
          CostLineItem(item)%units = 'Ea.'
          CostLineItem(item)%ValuePer = CostLineItem(Item)%perEach
          CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
      ENDIF

    CASE ('DAYLIGHTING:CONTROLS')
      WildcardObjNames =  .FALSE.

      IF (TRIM(CostLineItem(Item)%ParentObjName) == '*') THEN ! wildcard, apply to all such components
        WildcardObjNames = .TRUE.
        CostLineItem(item)%Qty = SUM(ZoneDaylight%TotalDaylRefPoints)
      ELSEIF (CostLineItem(Item)%ParentObjName /= '') then
        thisZoneID = FindItem(CostLineItem(Item)%ParentObjName, zone%name, NumOfZones)
        IF (thisZoneID > 0) THEN
          CostLineItem(item)%Qty  = ZoneDaylight(thisZoneID)%TotalDaylRefPoints
        ENDIF
      ENDIF

      CostLineItem(item)%units    = 'Ea.'
      CostLineItem(item)%ValuePer = CostLineItem(Item)%PerEach
      CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer


    CASE ('SHADING:ZONE:DETAILED')
      IF (CostLineItem(Item)%ParentObjName /= '') then
        ThisSurfID = FindItem(CostLineItem(Item)%ParentObjName, Surface%Name, TotSurfaces)
        IF (ThisSurfID > 0) THEN
          ThisZoneID = FindItem(Surface(ThisSurfID)%ZoneName,Zone%name,NumOfZones)
          IF (ThisZoneID > 0) THEN
            CostLineItem(item)%Qty = Surface(ThisSurfID)%area &
                                     * zone(ThisZoneID)%Multiplier &
                                     * zone(ThisZoneID)%ListMultiplier
            CostLineItem(item)%units = 'm2'
            CostLineItem(item)%ValuePer = CostLineItem(Item)%PerSquareMeter
            CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
          ENDIF
        ENDIF
      ENDIF

    CASE ('LIGHTS')

      IF (CostLineItem(item)%PerEach /= 0.0d0) THEN
        CostLineItem(item)%Qty = 1.0d0
        CostLineItem(item)%units = 'Ea.'
        CostLineItem(item)%ValuePer = CostLineItem(item)%PerEach
        CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
      ENDIF

      IF (CostLineItem(item)%PerKiloWattCap /= 0.0d0) THEN
        IF (CostLineItem(Item)%ParentObjName /= '') THEN
          ThisZoneID = FindItem(CostLineItem(item)%ParentObjName, Zone%Name, NumOfZones)
          IF (ThisZoneID > 0) THEN
            CostLineItem(item)%Qty = SUM(  zone(ThisZoneID)%Multiplier      &
                                         * zone(ThisZoneID)%ListMultiplier  &
                                         * Lights%DesignLevel/1000.0,       &
                  mask=Lights%zonePtr == ThisZoneID)  !this handles more than one light object per zone.
            CostLineItem(item)%units = 'kW'
            CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
            CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
          ENDIF
        ENDIF
      ENDIF

    CASE ('GENERATOR:PHOTOVOLTAIC')

      IF (CostLineItem(item)%PerKiloWattCap /= 0.0d0) THEN
        IF (CostLineItem(Item)%ParentObjName /= '') THEN
          ThisPV    = FindItem(CostLineItem(Item)%ParentObjName, PVArray%Name, NumPVs)
          IF (ThisPV > 0) THEN
            ThisZoneID = FindItem(Surface(PVArray(thisPV)%SurfacePtr)%ZoneName,Zone%name,NumOfZones)
            IF (ThisZoneID == 0) THEN
              Multipliers=1.0d0
            ELSE
              Multipliers=Zone(ThisZoneID)%Multiplier * Zone(ThisZoneID)%ListMultiplier
            ENDIF
            IF (PVArray(thisPV)%PVModelType == iSimplePVModel) THen
              CostLineItem(item)%Qty = 1000.0d0 * PVArray(thisPV)%SimplePVModule%AreaCol &
                                              * PVArray(thisPV)%SimplePVModule%PVEfficiency &
                                              * Multipliers  / 1000.0d0

            ENDIF
            CostLineItem(item)%units = 'kW (rated)'
            CostLineItem(item)%ValuePer = CostLineItem(Item)%PerKiloWattCap
            CostLineItem(item)%LineSubTotal = CostLineItem(item)%Qty * CostLineItem(item)%ValuePer
          ENDIF
        ENDIF
      ENDIF


    END SELECT

  ENDDO

  !now sum up the line items, result for the current building

  CurntBldg%LineItemTot = sum(CostLineItem%LineSubTotal)


  RETURN

END SUBROUTINE CalcCostEstimate

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

END MODULE CostEstimateManager

