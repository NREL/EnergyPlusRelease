MODULE HighTempRadiantSystem

  ! Module containing the routines dealing with the high temperature radiant systems

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   February 2001
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate high temperature radiant systems.
  ! It is the intention of this module to cover all types of high temperature
  ! radiant systems (gas and electric)

  ! METHODOLOGY EMPLOYED:
  ! Based on work done in BLAST, the EnergyPlus low temperature radiant system
  ! model, this model has similar inherent challenges that are similar to the
  ! low temperature radiant system.  Because it is a system that directly
  ! effects the surface heat balances, it must be a part of both the heat
  ! balance routines and linked in with the HVAC system.
  ! REFERENCES:
  ! Building Systems Laboratory, BLAST User's Guide/Reference.
  ! Maloney, Dan. 1987. "Development of a radiant heater model and the
  !   incorporation of thermal comfort considerations into the BLAST
  !   energy analysis program", M.S. thesis, University of Illinois at
  !   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,       ONLY : MaxNameLength, BeginTimeStepFlag, SysSizingCalc, ScheduleAlwaysOn, DisplayExtraWarnings
USE DataInterfaces 
USE DataHVACGlobals,   ONLY: SmallLoad

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
CHARACTER(LEN=*), PARAMETER :: cGas = 'Gas'
CHARACTER(LEN=*), PARAMETER :: cNaturalGas = 'NaturalGas'
CHARACTER(LEN=*), PARAMETER :: cElectric = 'Electric'
CHARACTER(LEN=*), PARAMETER :: cElectricity = 'Electricity'
INTEGER, PARAMETER :: Gas=1
INTEGER, PARAMETER :: Electric=2
CHARACTER(len=*), PARAMETER :: cMATControl = 'MeanAirTemperature'                   ! Control for using mean air temperature
CHARACTER(len=*), PARAMETER :: cMRTControl = 'MeanRadiantTemperature'               ! Control for using mean radiant temperature
CHARACTER(len=*), PARAMETER :: cOperativeControl = 'OperativeTemperature'           ! Control for using operative temperature
CHARACTER(len=*), PARAMETER :: cMATSPControl = 'MeanAirTemperatureSetpoint'         ! Control for to MAT setpoint
CHARACTER(len=*), PARAMETER :: cMRTSPControl = 'MeanRadiantTemperatureSetpoint'     ! Control for to MRT setpoint
CHARACTER(len=*), PARAMETER :: cOperativeSPControl = 'OperativeTemperatureSetpoint' ! Control for operative temperature setpoint
INTEGER, PARAMETER :: MATControl=1001
INTEGER, PARAMETER :: MRTControl=1002
INTEGER, PARAMETER :: OperativeControl=1003
INTEGER, PARAMETER :: MATSPControl=1004
INTEGER, PARAMETER :: MRTSPControl=1005
INTEGER, PARAMETER :: OperativeSPControl=1006


  ! DERIVED TYPE DEFINITIONS:
TYPE HighTempRadiantSystemData
  ! Input data
  CHARACTER(len=MaxNameLength)   :: Name              =' ' ! name of hydronic radiant system
  CHARACTER(len=MaxNameLength)   :: SchedName         =' ' ! availability schedule
  INTEGER                        :: SchedPtr          =0   ! index to schedule
  CHARACTER(len=MaxNameLength)   :: ZoneName          =' ' ! Name of zone the system is serving
  INTEGER                        :: ZonePtr           =0   ! Point to this zone in the Zone derived type
  INTEGER                        :: HeaterType        =0   ! Type of heater (gas or electric)
  REAL(r64)                      :: MaxPowerCapac     =0.0d0 ! Maximum capacity of the radiant heater in Watts
  REAL(r64)                      :: CombustionEffic   =0.0d0 ! Combustion efficiency (only valid for a gas heater)
  REAL(r64)                      :: FracRadiant       =0.0d0 ! Fraction of heater power that is given off as radiant heat
  REAL(r64)                      :: FracLatent        =0.0d0 ! Fraction of heater power that is given off as latent heat
  REAL(r64)                      :: FracLost          =0.0d0 ! Fraction of heater power that is lost to the outside environment
  REAL(r64)                      :: FracConvect       =0.0d0 ! Fraction of heater power that is given off as convective heat
                                                           ! (by definition this is 1 minus the sum of all other fractions)
  INTEGER                        :: ControlType       =0   ! Control type for the system (MAT, MRT, or op temp)
  REAL(r64)                      :: ThrottlRange      =0.0d0 ! Throttling range for heating [C]
  CHARACTER(len=MaxNameLength)   :: SetptSched        =' ' ! Schedule name for the zone setpoint temperature
  INTEGER                        :: SetptSchedPtr     =0   ! Schedule index for the zone setpoint temperature
  REAL(r64)                      :: FracDistribPerson =0.0d0 ! Fraction of fraction radiant incident on a "person" in the space
  INTEGER                        :: TotSurfToDistrib  =0   ! Total number of surfaces the heater sends radiation to
  CHARACTER(len=MaxNameLength), &
           ALLOCATABLE, DIMENSION(:) :: SurfaceName       ! Surface name in the list of surfaces heater sends radiation to
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SurfacePtr        ! Surface number in the list of surfaces heater sends radiation to
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: FracDistribToSurf ! Fraction of fraction radiant incident on the surface
  ! Other parameters
  ! Report data
  REAL(r64)                    :: ElecPower           =0.0d0 ! system electric consumption in Watts
  REAL(r64)                    :: ElecEnergy          =0.0d0 ! system electric consumption in Joules
  REAL(r64)                    :: GasPower            =0.0d0 ! system gas consumption in Watts
  REAL(r64)                    :: GasEnergy           =0.0d0 ! system gas consumption in Joules
  REAL(r64)                    :: HeatPower           =0.0d0 ! actual heating sent to zone (convective and radiative) in Watts
  REAL(r64)                    :: HeatEnergy          =0.0d0 ! actual heating sent to zone (convective and radiative) in Joules
END TYPE HighTempRadiantSystemData

TYPE(HighTempRadiantSystemData), DIMENSION(:), ALLOCATABLE :: HighTempRadSys

  ! MODULE VARIABLE DECLARATIONS:
  ! Standard, run-of-the-mill variables...
INTEGER :: NumOfHighTempRadSys=0 ! Number of hydronic low tempererature radiant systems
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QHTRadSource ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QHTRadSrcAvg ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZeroSourceSumHATsurf ! Equal to the SumHATsurf for all the walls in a zone with no source
  ! Record keeping variables used to calculate QHTRadSrcAvg locally
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastQHTRadSrc        ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastSysTimeElapsed   ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:):: LastTimeStepSys      ! Need to keep the last value in case we are still iterating
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE HighTempRadiantSystem
PUBLIC  SimHighTempRadiantSystem
PRIVATE GetHighTempRadiantSystem
PRIVATE InitHighTempRadiantSystem
PRIVATE SizeHighTempRadiantSystem
PRIVATE CalcHighTempRadiantSystem
PRIVATE CalcHighTempRadiantSystemSP
PRIVATE UpdateHighTempRadiantSystem
PUBLIC  UpdateHTRadSourceValAvg
PRIVATE DistributeHTRadGains
PRIVATE ReportHighTempRadiantSystem
PRIVATE SumHATsurf

CONTAINS

SUBROUTINE SimHighTempRadiantSystem(CompName,FirstHVACIteration,LoadMet,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the "manager" for the high temperature radiant
          ! system model.  It is called from the outside and controls the
          ! actions and subroutine calls to lower levels as appropriate.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus manager subroutine layout

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the low temperature radiant system
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),             INTENT(OUT) :: LoadMet             ! load met by the radiant system, in Watts
  INTEGER,          INTENT(INOUT):: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag = .TRUE.  ! First time, input is "gotten"
  INTEGER       :: RadSysNum              ! Radiant system number/index in local derived types

          ! FLOW:
  IF (GetInputFlag) THEN
    CALL GetHighTempRadiantSystem
    GetInputFlag=.FALSE.
  ENDIF

  ! Find the correct ZoneHVAC:HighTemperatureRadiant
  IF (CompIndex == 0) THEN
    RadSysNum = FindItemInList(CompName,HighTempRadSys%Name,NumOfHighTempRadSys)
    IF (RadSysNum == 0) THEN
      CALL ShowFatalError('SimHighTempRadiantSystem: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=RadSysNum
  ELSE
    RadSysNum=CompIndex
    IF (RadSysNum > NumOfHighTempRadSys .or. RadSysNum < 1) THEN
      CALL ShowFatalError('SimHighTempRadiantSystem:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(RadSysNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfHighTempRadSys))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(RadSysNum)) THEN
      IF (CompName /= HighTempRadSys(RadSysNum)%Name) THEN
        CALL ShowFatalError('SimHighTempRadiantSystem: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(RadSysNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(HighTempRadSys(RadSysNum)%Name))
      ENDIF
      CheckEquipName(RadSysNum)=.false.
    ENDIF
  ENDIF

  CALL InitHighTempRadiantSystem(FirstHVACIteration,RadSysNum)

  SELECT CASE (HighTempRadSys(RadSysNum)%ControlType)
      CASE (MATControl,MRTControl,OperativeControl)
        CALL CalcHighTempRadiantSystem(RadSysNum)
      CASE (MATSPControl,MRTSPControl,OperativeSPControl)
        CALL CalcHighTempRadiantSystemSP(FirstHVACIteration,RadSysNum)
  END SELECT

  CALL UpdateHighTempRadiantSystem(RadSysNum,LoadMet)

  CALL ReportHighTempRadiantSystem(RadSysNum)

  RETURN

END SUBROUTINE SimHighTempRadiantSystem


SUBROUTINE GetHighTempRadiantSystem

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for high temperature radiant systems
          ! from the user input file.  This will contain all of the information
          ! needed to simulate a high temperature radiant system.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,      ONLY : NumOfZones
  USE DataHeatBalance,  ONLY : Zone
  USE DataSurfaces,     ONLY : Surface, TotSurfaces
  USE InputProcessor,   ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList, SameString, VerifyName, GetObjectDefMaxArgs
  USE ScheduleManager,  ONLY : GetScheduleIndex
  USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  REAL(r64),             PARAMETER :: MaxCombustionEffic = 1.00d0  ! Limit the combustion efficiency to perfection
  REAL(r64),             PARAMETER :: MaxFraction = 1.0d0          ! Limit the highest allowed fraction for heat transfer parts
  REAL(r64),             PARAMETER :: MinCombustionEffic = 0.01d0  ! Limit the minimum combustion efficiency
  REAL(r64),             PARAMETER :: MinFraction = 0.0d0          ! Limit the lowest allowed fraction for heat transfer parts
  REAL(r64),             PARAMETER :: MinThrottlingRange = 0.5d0   ! Smallest throttling range allowed in degrees Celsius
!  INTEGER,          PARAMETER :: MaxDistribSurfaces = 20    ! Maximum number of surfaces that a radiant heater can radiate to
  CHARACTER(len=*), PARAMETER   :: RoutineName='GetHighTempRadiantSystem: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: AllFracsSummed  ! Sum of the fractions radiant, latent, and lost (must be <= 1)
  LOGICAL    :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER    :: IOStatus   ! Used in GetObjectItem
  INTEGER    :: Item       ! Item to be "gotten"
  INTEGER    :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER    :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER    :: SurfNum    ! Surface number DO loop counter
  LOGICAL    :: IsNotOK               ! Flag to verify name
  LOGICAL    :: IsBlank               ! Flag for blank name

          ! FLOW:
          ! Initializations and allocations
  NumOfHighTempRadSys = GetNumObjectsFound('ZoneHVAC:HighTemperatureRadiant')

  ALLOCATE(HighTempRadSys(NumOfHighTempRadSys))
  ALLOCATE(CheckEquipName(NumOfHighTempRadSys))
  CheckEquipName=.true.

  ! extensible object, do not need max args because using IPShortCuts

  cCurrentModuleObject = 'ZoneHVAC:HighTemperatureRadiant'
          ! Obtain all of the user data related to high temperature radiant systems...
  DO Item = 1, NumOfHighTempRadSys

    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),HighTempRadSys%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
          ! General user input data
    HighTempRadSys(Item)%Name = cAlphaArgs(1)

    HighTempRadSys(Item)%SchedName = cAlphaArgs(2)
    IF (lAlphaFieldBlanks(2)) THEN
      HighTempRadSys(Item)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      HighTempRadSys(Item)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
      IF (HighTempRadSys(Item)%SchedPtr == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
                            ' entered ='//TRIM(cAlphaArgs(2))// &
                            ' for '//TRIM(cAlphaFieldNames(1))//' = '//TRIM(cAlphaArgs(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF

    HighTempRadSys(Item)%ZoneName = cAlphaArgs(3)
    HighTempRadSys(Item)%ZonePtr  = FindIteminList(cAlphaArgs(3),Zone%Name,NumOfZones)
    IF (HighTempRadSys(Item)%ZonePtr == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      ErrorsFound=.TRUE.
    END IF

    HighTempRadSys(Item)%MaxPowerCapac = rNumericArgs(1)

    IF (SameString(cAlphaArgs(4),cNaturalGas)) THEN
      HighTempRadSys(Item)%HeaterType = Gas
    ELSE IF (SameString(cAlphaArgs(4),cElectricity)) THEN
      HighTempRadSys(Item)%HeaterType = Electric
    ELSE IF (SameString(cAlphaArgs(4),cGas)) THEN
      HighTempRadSys(Item)%HeaterType = Gas
    ELSE IF (SameString(cAlphaArgs(4),cElectric)) THEN
      HighTempRadSys(Item)%HeaterType = Electric
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      ErrorsFound=.TRUE.
    END IF

    IF (HighTempRadSys(Item)%HeaterType == Gas) THEN
      HighTempRadSys(Item)%CombustionEffic = rNumericArgs(2)
          ! Limit the combustion efficiency to between zero and one...
      IF (HighTempRadSys(Item)%CombustionEffic < MinCombustionEffic) THEN
        HighTempRadSys(Item)%CombustionEffic = MinCombustionEffic
        CALL ShowWarningError(TRIM(cNumericFieldNames(2))//' was less than the allowable minimum, reset to minimum value.')
        CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      END IF
      IF (HighTempRadSys(Item)%CombustionEffic > MaxCombustionEffic) THEN
        HighTempRadSys(Item)%CombustionEffic = MaxCombustionEffic
        CALL ShowWarningError(TRIM(cNumericFieldNames(2))//' was greater than the allowable maximum, reset to maximum value.')
        CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      END IF
    ELSE
      HighTempRadSys(Item)%CombustionEffic = MaxCombustionEffic ! No inefficiency in the heater
    END IF

    HighTempRadSys(Item)%FracRadiant = rNumericArgs(3)
    IF (HighTempRadSys(Item)%FracRadiant < MinFraction) THEN
      HighTempRadSys(Item)%FracRadiant = MinFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(3))//' was less than the allowable minimum, reset to minimum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF
    IF (HighTempRadSys(Item)%FracRadiant > MaxFraction) THEN
      HighTempRadSys(Item)%FracRadiant = MaxFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(3))//' was greater than the allowable maximum, reset to maximum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF

    HighTempRadSys(Item)%FracLatent = rNumericArgs(4)
    IF (HighTempRadSys(Item)%FracLatent < MinFraction) THEN
      HighTempRadSys(Item)%FracLatent = MinFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(4))//' was less than the allowable minimum, reset to minimum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF
    IF (HighTempRadSys(Item)%FracLatent > MaxFraction) THEN
      HighTempRadSys(Item)%FracLatent = MaxFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(4))//' was greater than the allowable maximum, reset to maximum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF

    HighTempRadSys(Item)%FracLost = rNumericArgs(5)
    IF (HighTempRadSys(Item)%FracLost < MinFraction) THEN
      HighTempRadSys(Item)%FracLost = MinFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(5))//' was less than the allowable minimum, reset to minimum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF
    IF (HighTempRadSys(Item)%FracLost > MaxFraction) THEN
      HighTempRadSys(Item)%FracLost = MaxFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(5))//' was greater than the allowable maximum, reset to maximum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF

          ! Based on the input for fractions radiant, latent, and lost, determine the fraction convective (remaining fraction)
    AllFracsSummed = HighTempRadSys(Item)%FracRadiant + HighTempRadSys(Item)%FracLatent + HighTempRadSys(Item)%FracLost
    IF (AllFracsSummed > MaxFraction) THEN
      CALL ShowSevereError('Fractions radiant, latent, and lost sum up to greater than 1 for'//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
      HighTempRadSys(Item)%FracConvect = 0.0d0
    ELSE
      HighTempRadSys(Item)%FracConvect = 1.0d0 - AllFracsSummed
    END IF

          ! Process the temperature control type
    IF (SameString(cAlphaArgs(5),cMATControl)) THEN
      HighTempRadSys(Item)%ControlType = MATControl
    ELSE IF (SameString(cAlphaArgs(5),cMRTControl)) THEN
      HighTempRadSys(Item)%ControlType = MRTControl
    ELSE IF (SameString(cAlphaArgs(5),cOperativeControl)) THEN
      HighTempRadSys(Item)%ControlType = OperativeControl
    ELSE IF (SameString(cAlphaArgs(5),cMATSPControl)) THEN
      HighTempRadSys(Item)%ControlType = MATSPControl
    ELSE IF (SameString(cAlphaArgs(5),cMRTSPControl)) THEN
      HighTempRadSys(Item)%ControlType = MRTSPControl
    ELSE IF (SameString(cAlphaArgs(5),cOperativeSPControl)) THEN
      HighTempRadSys(Item)%ControlType = OperativeSPControl
    ELSE
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)))
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Control reset to OPERATIVE control for this '//TRIM(cCurrentModuleObject))
      HighTempRadSys(Item)%ControlType = OperativeControl
    END IF

    HighTempRadSys(Item)%ThrottlRange = rNumericArgs(6)
    IF (HighTempRadSys(Item)%ThrottlRange < MinThrottlingRange) THEN
      HighTempRadSys(Item)%ThrottlRange = 1.0d0
      CALL ShowWarningError(TRIM(cNumericFieldNames(6))//' is below the minimum allowed.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Thus, the throttling range value has been reset to 1.0')
    END IF

    HighTempRadSys(Item)%SetptSched    = cAlphaArgs(6)
    HighTempRadSys(Item)%SetptSchedPtr = GetScheduleIndex(cAlphaArgs(6))
    IF ((HighTempRadSys(Item)%SetptSchedPtr == 0).AND.(.NOT. lAlphaFieldBlanks(6))) THEN
      CALL ShowSevereError(TRIM(cAlphaFieldNames(6))//' not found: '//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      ErrorsFound=.TRUE.
    END IF

    HighTempRadSys(Item)%FracDistribPerson = rNumericArgs(7)
    IF (HighTempRadSys(Item)%FracDistribPerson < MinFraction) THEN
      HighTempRadSys(Item)%FracDistribPerson = MinFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(7))//' was less than the allowable minimum, reset to minimum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF
    IF (HighTempRadSys(Item)%FracDistribPerson > MaxFraction) THEN
      HighTempRadSys(Item)%FracDistribPerson = MaxFraction
      CALL ShowWarningError(TRIM(cNumericFieldNames(7))//' was greater than the allowable maximum, reset to maximum value.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF

    HighTempRadSys(Item)%TotSurfToDistrib = NumNumbers - 7
!    IF (HighTempRadSys(Item)%TotSurfToDistrib > MaxDistribSurfaces) THEN
!      CALL ShowSevereError('Trying to distribute radiant energy to too many surfaces for heater '//TRIM(cAlphaArgs(1)))
!      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
!      ErrorsFound=.true.
!    END IF
    ALLOCATE(HighTempRadSys(Item)%SurfaceName(HighTempRadSys(Item)%TotSurfToDistrib))
    ALLOCATE(HighTempRadSys(Item)%SurfacePtr(HighTempRadSys(Item)%TotSurfToDistrib))
    ALLOCATE(HighTempRadSys(Item)%FracDistribToSurf(HighTempRadSys(Item)%TotSurfToDistrib))

    AllFracsSummed = HighTempRadSys(Item)%FracDistribPerson
    DO SurfNum = 1, HighTempRadSys(Item)%TotSurfToDistrib
      HighTempRadSys(Item)%SurfaceName(SurfNum)       = cAlphaArgs(SurfNum+6)
      HighTempRadSys(Item)%SurfacePtr(SurfNum)        = FindIteminList(cAlphaArgs(SurfNum+6),Surface%Name,TotSurfaces)
      HighTempRadSys(Item)%FracDistribToSurf(SurfNum) = rNumericArgs(SurfNum+7)
          ! Error trap for surfaces that do not exist or surfaces not in the zone the radiant heater is in
      IF (HighTempRadSys(Item)%SurfacePtr(SurfNum) == 0) THEN
        CALL ShowSevereError(RoutineName//'Invalid Surface name = '//TRIM(HighTempRadSys(Item)%SurfaceName(SurfNum)))
        CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSE IF (Surface(HighTempRadSys(Item)%SurfacePtr(SurfNum))%Zone /= HighTempRadSys(Item)%ZonePtr) THEN
        CALL ShowWarningError('Surface referenced in ZoneHVAC:HighTemperatureRadiant not in same zone as Radiant System,'//  &
           'surface='//TRIM(HighTempRadSys(Item)%SurfaceName(SurfNum)))
        CALL ShowContinueError('Surface is in Zone='//TRIM(Zone(Surface(HighTempRadSys(Item)%SurfacePtr(SurfNum))%Zone)%Name)// &
                         ' ZoneHVAC:HighTemperatureRadiant in Zone='//TRIM(cAlphaArgs(3)))
        CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      END IF
          ! Error trap for fractions that are out of range
      IF (HighTempRadSys(Item)%FracDistribToSurf(SurfNum) < MinFraction) THEN
        HighTempRadSys(Item)%FracDistribToSurf(SurfNum) = MinFraction
        CALL ShowWarningError(TRIM(cNumericFieldNames(SurfNum+7))//' was less than the allowable minimum, reset to minimum value.')
        CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      END IF
      IF (HighTempRadSys(Item)%FracDistribToSurf(SurfNum) > MaxFraction) THEN
        HighTempRadSys(Item)%FracDistribToSurf(SurfNum) = MaxFraction
        CALL ShowWarningError(TRIM(cNumericFieldNames(SurfNum+7))//  &
           ' was greater than the allowable maximum, reset to maximum value.')
        CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      END IF

      IF (HighTempRadSys(Item)%SurfacePtr(SurfNum) /= 0) THEN
        Surface( HighTempRadSys(Item)%SurfacePtr(SurfNum) )%IntConvSurfGetsRadiantHeat = .TRUE.
      ENDIF

      AllFracsSummed = AllFracsSummed + HighTempRadSys(Item)%FracDistribToSurf(SurfNum)

    END DO  ! ...end of DO loop through surfaces that the heater radiates to.

          ! Error trap if the fractions add up to greater than 1.0
    IF (AllFracsSummed > (MaxFraction + 0.01d0) ) THEN
      CALL ShowSevereError('Fraction of radiation distributed to surfaces sums up to greater than 1 for '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF
    IF (AllFracsSummed < (MaxFraction - 0.01d0) ) THEN ! User didn't distribute all of the radiation warn that some will be lost
      CALL ShowWarningError('Fraction of radiation distributed to surfaces sums up to less than 1 for '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('As a result, some of the radiant energy delivered by the high temp radiant heater will be lost.')
      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
    END IF

  END DO    ! ...end of DO loop through all of the high temperature radiant heaters

          ! Set up the output variables for high temperature radiant heaters
  DO Item = 1, NumOfHighTempRadSys
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Rate [W]',   &
                              HighTempRadSys(Item)%HeatPower,'System','Average',  &
                              HighTempRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Energy [J]',    &
                              HighTempRadSys(Item)%HeatEnergy,'System','Sum', &
                              HighTempRadSys(Item)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
    IF (HighTempRadSys(Item)%HeaterType == Gas) THEN
      CALL SetupOutputVariable('Zone Radiant HVAC Gas Rate [W]', &
                                HighTempRadSys(Item)%GasPower,'System','Average',  &
                                HighTempRadSys(Item)%Name)
      CALL SetupOutputVariable('Zone Radiant HVAC Gas Energy [J]',  &
                                HighTempRadSys(Item)%GasEnergy,'System','Sum', &
                                HighTempRadSys(Item)%Name,                     &
                                ResourceTypeKey='Gas',EndUseKey='Heating',GroupKey='System')
    ELSE IF (HighTempRadSys(Item)%HeaterType == Electric) THEN
      CALL SetupOutputVariable('Zone Radiant HVAC Electric Power [W]',       &
                                HighTempRadSys(Item)%ElecPower,'System','Average', &
                                HighTempRadSys(Item)%Name)
      CALL SetupOutputVariable('Zone Radiant HVAC Electric Energy [J]', &
                                HighTempRadSys(Item)%ElecEnergy,'System','Sum',    &
                                HighTempRadSys(Item)%Name,                         &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='Heating',GroupKey='System')
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE GetHighTempRadiantSystem


SUBROUTINE InitHighTempRadiantSystem(FirstHVACIteration,RadSysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes variables relating to high temperature
          ! radiant heating systems.

          ! METHODOLOGY EMPLOYED:
          ! Simply initializes whatever needs initializing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY : NumOfZones, BeginEnvrnFlag
  USE DataLoopNode,      ONLY : Node
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT(IN) :: RadSysNum  ! Index for the low temperature radiant system under consideration within the derived types

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: FirstTime = .TRUE.   ! For one-time initializations
  INTEGER       :: ZoneNum              ! Intermediate variable for keeping track of the zone number
  LOGICAL, SAVE :: MyEnvrnFlag=.true.
  LOGICAL,SAVE  :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer       :: Loop

          ! FLOW:
  IF (FirstTime) THEN
    ALLOCATE(ZeroSourceSumHATsurf(NumOfZones))
    ZeroSourceSumHATsurf = 0.0D0
    ALLOCATE(QHTRadSource(NumOfHighTempRadSys))
    QHTRadSource = 0.0D0
    ALLOCATE(QHTRadSrcAvg(NumOfHighTempRadSys))
    QHTRadSrcAvg = 0.0D0
    ALLOCATE(LastQHTRadSrc(NumOfHighTempRadSys))
    LastQHTRadSrc = 0.0D0
    ALLOCATE(LastSysTimeElapsed(NumOfHighTempRadSys))
    LastSysTimeElapsed = 0.0D0
    ALLOCATE(LastTimeStepSys(NumOfHighTempRadSys))
    LastTimeStepSys = 0.0D0
    ALLOCATE(MySizeFlag(NumOfHighTempRadSys))
    MySizeFlag = .TRUE.
    FirstTime       = .FALSE.
  END IF

  ! need to check all units to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumOfHighTempRadSys
      IF (CheckZoneEquipmentList('ZoneHVAC:HighTemperatureRadiant',HighTempRadSys(Loop)%Name)) CYCLE
      CALL ShowSevereError('InitHighTempRadiantSystem: Unit=[ZoneHVAC:HighTemperatureRadiant,'//TRIM(HighTempRadSys(Loop)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(RadSysNum)) THEN
    ! for each radiant systen do the sizing once.
    CALL SizeHighTempRadiantSystem(RadSysNum)
    MySizeFlag(RadSysNum) = .FALSE.
  END IF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    ZeroSourceSumHATsurf       =0.0D0
    QHTRadSource               =0.0D0
    QHTRadSrcAvg               =0.0D0
    LastQHTRadSrc              =0.0D0
    LastSysTimeElapsed         =0.0D0
    LastTimeStepSys            =0.0D0
    MyEnvrnFlag=.false.
  ENDIF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  IF (BeginTimeStepFlag .AND. FirstHVACIteration) THEN ! This is the first pass through in a particular time step
    ZoneNum = HighTempRadSys(RadSysNum)%ZonePtr
    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum) ! Set this to figure out what part of the load the radiant system meets
    QHTRadSrcAvg(RadSysNum)       = 0.0D0  ! Initialize this variable to zero (radiant system defaults to off)
    LastQHTRadSrc(RadSysNum)      = 0.0D0  ! At the beginning of a time step, reset to zero so average calculation can start again
    LastSysTimeElapsed(RadSysNum) = 0.0D0  ! At the beginning of a time step, reset to zero so average calculation can start again
    LastTimeStepSys(RadSysNum)    = 0.0D0  ! At the beginning of a time step, reset to zero so average calculation can start again
  END IF

  RETURN

END SUBROUTINE InitHighTempRadiantSystem

SUBROUTINE SizeHighTempRadiantSystem(RadSysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing high temperature radiant components for which max power input has not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains design heating load from the zone sizing arrays

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: RadSysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
  REAL(r64) :: MaxPowerCapacDes    ! Design maximum capacity for reproting
  REAL(r64) :: MaxPowerCapacUser   ! User hard-sized maximum capacity for reproting
  LOGICAL   :: IsAutosize          ! Indicator to autosizing nominal capacity

  IsAutosize = .FALSE.
  MaxPowerCapacDes = 0.0d0
  MaxPowerCapacUser = 0.0d0

  IF (HighTempRadSys(RadSysNum)%MaxPowerCapac == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation should continue
      IF (HighTempRadSys(RadSysNum)%MaxPowerCapac > 0.0d0) THEN
        CALL ReportSizingOutput('ZoneHVAC:HighTemperatureRadiant', HighTempRadSys(RadSysNum)%Name, &
                              'User-Specified Maximum Power Input [W]', HighTempRadSys(RadSysNum)%MaxPowerCapac)
      END IF
    ELSE ! Autosize or hard-size with sizing run
      CALL CheckZoneSizing('ZoneHVAC:HighTemperatureRadiant', HighTempRadSys(RadSysNum)%Name)
      IF ((CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor) >= SmallLoad) THEN
        MaxPowerCapacDes =   &
          (CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor) / &
          (HighTempRadSys(RadSysNum)%FracRadiant + HighTempRadSys(RadSysNum)%FracConvect)
      ELSE
        MaxPowerCapacDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        HighTempRadSys(RadSysNum)%MaxPowerCapac = MaxPowerCapacDes
        CALL ReportSizingOutput('ZoneHVAC:HighTemperatureRadiant', HighTempRadSys(RadSysNum)%Name, &
                              'Design Size Maximum Power Input [W]', MaxPowerCapacDes)
      ELSE ! Hard-size with sizing data
        IF (HighTempRadSys(RadSysNum)%MaxPowerCapac > 0.0d0 .AND. MaxPowerCapacDes > 0.0d0) THEN
          MaxPowerCapacUser = HighTempRadSys(RadSysNum)%MaxPowerCapac
          CALL ReportSizingOutput('ZoneHVAC:HighTemperatureRadiant', HighTempRadSys(RadSysNum)%Name, &
                              'Design Size Maximum Power Input [W]', MaxPowerCapacDes, &
                              'User-Specified Maximum Power Input [W]', MaxPowerCapacUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxPowerCapacDes - MaxPowerCapacUser)/MaxPowerCapacUser ) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeHighTempRadiantSystem: Potential issue with equipment sizing for ' &
                                      // 'ZoneHVAC:HighTemperatureRadiant = "'//  &
                                      TRIM(HighTempRadSys(RadSysNum)%Name)//'".')
              CALL ShowContinueError('User-Specified Maximum Power Input of '// &
                                      TRIM(RoundSigDigits(MaxPowerCapacUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Maximum Power Input of ' // &
                                      TRIM(RoundSigDigits(MaxPowerCapacDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  RETURN
END SUBROUTINE SizeHighTempRadiantSystem


SUBROUTINE CalcHighTempRadiantSystem(RadSysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a high temperature radiant heating system.

          ! METHODOLOGY EMPLOYED:
          ! Follows the methods used by many other pieces of zone equipment except
          ! that we are controlling the input to the heater element.  Note that
          ! cooling is not allowed for such a system.  Controls are very basic at
          ! this point using just a linear interpolation between being off at
          ! one end of the throttling range, fully on at the other end, and varying
          ! linearly in between.

          ! REFERENCES:
          ! Other EnergyPlus modules
          ! Building Systems Laboratory, BLAST User's Guide/Reference.
          ! Fanger, P.O. "Analysis and Applications in Environmental Engineering",
          !   Danish Technical Press, 1970.
          ! Maloney, Dan. 1987. "Development of a radiant heater model and the
          !   incorporation of thermal comfort considerations into the BLAST
          !   energy analysis program", M.S. thesis, University of Illinois at
          !   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

          ! USE STATEMENTS:
  USE DataHeatBalance,   ONLY : MRT
  USE DataHeatBalFanSys, ONLY : MAT
  USE DataHVACGlobals,   ONLY : SmallLoad
  USE DataZoneEnergyDemands
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: RadSysNum           ! name of the low temperature radiant system

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: HeatFrac       ! fraction of maximum energy input to radiant system [dimensionless]
  REAL(r64)    :: OffTemp        ! Temperature above which the radiant system should be completely off [C]
  REAL(r64)    :: OpTemp         ! Operative temperature [C]
!  REAL(r64)    :: QZnReq         ! heating or cooling needed by zone [Watts]
  REAL(r64)    :: SetPtTemp      ! Setpoint temperature [C]
  INTEGER :: ZoneNum        ! number of zone being served

          ! FLOW:
          ! initialize local variables
  ZoneNum  = HighTempRadSys(RadSysNum)%ZonePtr
  HeatFrac = 0.0d0

  IF (GetCurrentScheduleValue(HighTempRadSys(RadSysNum)%SchedPtr) <= 0) THEN

          ! Unit is off or has no load upon it; set the flow rates to zero and then
          ! simulate the components with the no flow conditions
    QHTRadSource(RadSysNum) = 0.0D0

  ELSE    ! Unit might be on-->this section is intended to control the output of the
          ! high temperature radiant heater (temperature controlled)

          ! Determine the current setpoint temperature and the temperature at which the unit should be completely off
    SetptTemp = GetCurrentScheduleValue(HighTempRadSys(RadSysNum)%SetptSchedPtr)
    OffTemp   = SetptTemp + 0.5d0*HighTempRadSys(RadSysNum)%ThrottlRange
    OpTemp    = (MAT(ZoneNum) + MRT(ZoneNum))/2.0d0 ! Approximate the "operative" temperature

          ! Determine the fraction of maximum power to the unit (limiting the fraction range from zero to unity)
    SELECT CASE (HighTempRadSys(RadSysNum)%ControlType)
      CASE (MATControl)
        HeatFrac = (OffTemp - MAT(ZoneNum))/HighTempRadSys(RadSysNum)%ThrottlRange
      CASE (MRTControl)
        HeatFrac = (OffTemp - MRT(ZoneNum))/HighTempRadSys(RadSysNum)%ThrottlRange
      CASE (OperativeControl)
        OpTemp   = 0.5d0*(MAT(ZoneNum)+MRT(ZoneNum))
        HeatFrac = (OffTemp - OpTemp)/HighTempRadSys(RadSysNum)%ThrottlRange
    END SELECT
    IF (HeatFrac < 0.0d0) HeatFrac = 0.0d0
    IF (HeatFrac > 1.0d0) HeatFrac = 1.0d0

          ! Set the heat source for the high temperature electric radiant system
    QHTRadSource(RadSysNum) = HeatFrac*HighTempRadSys(RadSysNum)%MaxPowerCapac

  END IF

  RETURN

END SUBROUTINE CalcHighTempRadiantSystem


SUBROUTINE CalcHighTempRadiantSystemSP(FirstHVACIteration,RadSysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2008
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a high temperature radiant heating system using setpoint temperature control.

          ! METHODOLOGY EMPLOYED:
          ! Follows the methods used by many other pieces of zone equipment except
          ! that we are controlling the input to the heater element.  Note that
          ! cooling is not allowed for such a system.  Controls are very basic and
          ! use an iterative approach to get close to what we need.

          ! REFERENCES:
          ! Other EnergyPlus modules
          ! Building Systems Laboratory, BLAST User's Guide/Reference.
          ! Fanger, P.O. "Analysis and Applications in Environmental Engineering",
          !   Danish Technical Press, 1970.
          ! Maloney, Dan. 1987. "Development of a radiant heater model and the
          !   incorporation of thermal comfort considerations into the BLAST
          !   energy analysis program", M.S. thesis, University of Illinois at
          !   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

          ! USE STATEMENTS:
  USE DataHeatBalance,   ONLY : MRT
  USE DataHeatBalFanSys, ONLY : MAT
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE DataInterfaces,    ONLY : CalcHeatBalanceOutsideSurf, CalcHeatBalanceInsideSurf

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration ! true if this is the first HVAC iteration at this system time step !unused1208
  INTEGER, INTENT(IN) :: RadSysNum          ! name of the low temperature radiant system

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER    :: TempConvToler = 0.1d0 ! Temperature controller tries to converge to within 0.1C
  INTEGER, PARAMETER :: MaxIterations = 10  ! Maximum number of iterations to achieve temperature control
                                            ! (10 interval halvings achieves control to 0.1% of capacity)
                                            ! These two parameters are intended to achieve reasonable control
                                            ! without excessive run times.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL       :: ConvergFlag    ! convergence flag for temperature control
!unused  INTEGER, SAVE :: ErrIterCount=0   ! number of time max iterations has been exceeded
  REAL          :: HeatFrac       ! fraction of maximum energy input to radiant system [dimensionless]
  REAL          :: HeatFracMax    ! maximum range of heat fraction
  REAL          :: HeatFracMin    ! minimum range of heat fraction
  INTEGER       :: IterNum        ! iteration number
  REAL(r64)     :: SetPtTemp      ! Setpoint temperature [C]
  INTEGER       :: ZoneNum        ! number of zone being served
  REAL(r64)     :: ZoneTemp       ! zone temperature (MAT, MRT, or Operative Temperature, depending on control type) [C]

          ! FLOW:
          ! initialize local variables
  ZoneNum                 = HighTempRadSys(RadSysNum)%ZonePtr
  QHTRadSource(RadSysNum) = 0.0D0

  IF (GetCurrentScheduleValue(HighTempRadSys(RadSysNum)%SchedPtr) > 0) THEN

          ! Unit is scheduled on-->this section is intended to control the output of the
          ! high temperature radiant heater (temperature controlled)

          ! Determine the current setpoint temperature and the temperature at which the unit should be completely off
    SetptTemp = GetCurrentScheduleValue(HighTempRadSys(RadSysNum)%SetptSchedPtr)

          ! Now, distribute the radiant energy of all systems to the appropriate
          ! surfaces, to people, and the air; determine the latent portion
    CALL DistributeHTRadGains

          ! Now "simulate" the system by recalculating the heat balances
    CALL CalcHeatBalanceOutsideSurf(ZoneNum)
    CALL CalcHeatBalanceInsideSurf(ZoneNum)

          ! First determine whether or not the unit should be on
          ! Determine the proper temperature on which to control
    SELECT CASE (HighTempRadSys(RadSysNum)%ControlType)
      CASE (MATSPControl)
        ZoneTemp = MAT(ZoneNum)
      CASE (MRTSPControl)
        ZoneTemp = MRT(ZoneNum)
      CASE (OperativeSPControl)
        ZoneTemp = 0.5d0*(MAT(ZoneNum)+MRT(ZoneNum))
    END SELECT

    IF (ZoneTemp < (SetptTemp-TempConvToler)) THEN

          ! Use simple interval halving to find the best operating fraction to achieve proper temperature control
      IterNum     = 0
      ConvergFlag = .FALSE.
      HeatFracMax = 1.0d0
      HeatFracMin = 0.0d0

      DO WHILE ( (IterNum <= MaxIterations) .AND. (.NOT. ConvergFlag) )

          ! In the first iteration (IterNum=0), try full capacity and see if that is the best solution
        IF (IterNum == 0) THEN
          HeatFrac = 1.0d0
        ELSE
          HeatFrac = (HeatFracMin + HeatFracMax) / 2.0d0
        END IF

          ! Set the heat source for the high temperature radiant system
        QHTRadSource(RadSysNum) = HeatFrac*HighTempRadSys(RadSysNum)%MaxPowerCapac

          ! Now, distribute the radiant energy of all systems to the appropriate
          ! surfaces, to people, and the air; determine the latent portion
        CALL DistributeHTRadGains

          ! Now "simulate" the system by recalculating the heat balances
        CALL CalcHeatBalanceOutsideSurf(ZoneNum)
        CALL CalcHeatBalanceInsideSurf(ZoneNum)

          ! Redetermine the current value of the controlling temperature
        SELECT CASE (HighTempRadSys(RadSysNum)%ControlType)
          CASE (MATControl)
            ZoneTemp = MAT(ZoneNum)
          CASE (MRTControl)
            ZoneTemp = MRT(ZoneNum)
          CASE (OperativeControl)
            ZoneTemp = 0.5d0*(MAT(ZoneNum)+MRT(ZoneNum))
        END SELECT

        IF ( (ABS(ZoneTemp-SetptTemp)) <= TempConvToler ) THEN
          ! The radiant heater has controlled the zone temperature to the appropriate level--stop iterating
          ConvergFlag = .TRUE.
        ELSEIF (ZoneTemp < SetptTemp) THEN
          ! The zone temperature is too low--try increasing the radiant heater output
          IF (IterNum == 0) THEN
          ! Heater already at capacity--this is the best that we can do
            ConvergFlag = .TRUE.
          ELSE
            HeatFracMin = HeatFrac
          END IF
        ELSE ! (ZoneTemp > SetptTemp)
          ! The zone temperature is too high--try decreasing the radiant heater output
          IF (IterNum > 0) HeatFracMax = HeatFrac
        END IF

        IterNum = IterNum + 1

      END DO

    END IF

  END IF

  RETURN

END SUBROUTINE CalcHighTempRadiantSystemSP


SUBROUTINE UpdateHighTempRadiantSystem(RadSysNum,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does any updating that needs to be done for high
          ! temperature radiant heating systems.  This routine has two functions.
          ! First, it needs to keep track of the average high temperature
          ! radiant source.  The method for doing this is similar to low
          ! temperature systems except that heat input is kept locally on
          ! a system basis rather than a surface basis.  This is because a high
          ! temperature system affects many surfaces while a low temperature
          ! system directly affects only one surface.  This leads to the second
          ! function of this subroutine which is to account for the affect of
          ! all high temperature radiant systems on each surface.  This
          ! distribution must be "redone" every time to be sure that we have
          ! properly accounted for all of the systems.

          ! METHODOLOGY EMPLOYED:
          ! For the source average update, if the system time step elapsed is
          ! still what it used to be, then either we are still iterating or we
          ! had to go back and shorten the time step.  As a result, we have to
          ! subtract out the previous value that we added.  If the system time
          ! step elapsed is different, then we just need to add the new values
          ! to the running average.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY : TimeStepZone,BeginEnvrnFlag
  USE DataHeatBalFanSys, ONLY : SumConvHTRadSys
  USE DataHVACGlobals,   ONLY : TimeStepSys, SysTimeElapsed

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: RadSysNum ! Index for the low temperature radiant system under consideration within the derived types
  REAL(r64),    INTENT(OUT) :: LoadMet   ! load met by the radiant system, in Watts

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneNum    ! Zone index number for the current radiant system
  logical,save :: myenvrnflag=.true.

          ! FLOW:
  if (beginenvrnflag .and. myenvrnflag) then
    myenvrnflag=.false.
  endif
  if (.not. beginenvrnflag) then
    myenvrnflag=.true.
  endif
          ! First, update the running average if necessary...
  IF (LastSysTimeElapsed(RadSysNum) == SysTimeElapsed) THEN
          ! Still iterating or reducing system time step, so subtract old values which were
          ! not valid
    QHTRadSrcAvg(RadSysNum) = QHTRadSrcAvg(RadSysNum) &
                             -LastQHTRadSrc(RadSysNum)*LastTimeStepSys(RadSysNum)/TimeStepZone
  END IF

          ! Update the running average and the "last" values with the current values of the appropriate variables
  QHTRadSrcAvg(RadSysNum) = QHTRadSrcAvg(RadSysNum) &
                           +QHTRadSource(RadSysNum)*TimeStepSys/TimeStepZone

  LastQHTRadSrc(RadSysNum)      = QHTRadSource(RadSysNum)
  LastSysTimeElapsed(RadSysNum) = SysTimeElapsed
  LastTimeStepSys(RadSysNum)    = TimeStepSys

  SELECT CASE (HighTempRadSys(RadSysNum)%ControlType)
      CASE (MATControl,MRTControl,OperativeControl)
          ! Only need to do this for the non-SP controls (SP has already done this enough)
          ! Now, distribute the radiant energy of all systems to the appropriate
          ! surfaces, to people, and the air; determine the latent portion
        CALL DistributeHTRadGains

          ! Now "simulate" the system by recalculating the heat balances
        ZoneNum = HighTempRadSys(RadSysNum)%ZonePtr
        CALL CalcHeatBalanceOutsideSurf(ZoneNum)
        CALL CalcHeatBalanceInsideSurf(ZoneNum)
  END SELECT

  IF (QHTRadSource(RadSysNum) <= 0.0d0) THEN
    LoadMet  = 0.0d0      ! System wasn't running so it can't meet a load
  ELSE
    ZoneNum = HighTempRadSys(RadSysNum)%ZonePtr
    LoadMet = (SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)) + SumConvHTRadSys(ZoneNum)
  END IF

  RETURN

END SUBROUTINE UpdateHighTempRadiantSystem


SUBROUTINE UpdateHTRadSourceValAvg(HighTempRadSysOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To transfer the average value of the heat source over the entire
          ! zone time step back to the heat balance routines so that the heat
          ! balance algorithms can simulate one last time with the average source
          ! to maintain some reasonable amount of continuity and energy balance
          ! in the temperature and flux histories.

          ! METHODOLOGY EMPLOYED:
          ! All of the record keeping for the average term is done in the Update
          ! routine so the only other thing that this subroutine does is check to
          ! see if the system was even on.  If any average term is non-zero, then
          ! one or more of the radiant systems was running.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(OUT) :: HighTempRadSysOn ! .TRUE. if the radiant system has run this zone time step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RadSysNum    ! DO loop counter for surface index

          ! FLOW:
  HighTempRadSysOn = .FALSE.

          ! If this was never allocated, then there are no radiant systems in this input file (just RETURN)
  IF (.NOT.ALLOCATED(QHTRadSrcAvg)) RETURN

          ! If it was allocated, then we have to check to see if this was running at all...
  DO RadSysNum = 1, NumOfHighTempRadSys
    IF (QHTRadSrcAvg(RadSysNum) /= 0.0D0) THEN
      HighTempRadSysOn = .TRUE.
      EXIT !DO loop
    END IF
  END DO

  QHTRadSource = QHTRadSrcAvg

  CALL DistributeHTRadGains ! QHTRadSource has been modified so we need to redistribute gains

  RETURN

END SUBROUTINE UpdateHTRadSourceValAvg


SUBROUTINE DistributeHTRadGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       April 2010 Brent Griffith, max limit to protect surface temperature calcs
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To distribute the gains from the high temperature radiant heater
          ! as specified in the user input file.  This includes distribution
          ! of long wavelength radiant gains to surfaces and "people" as well
          ! as latent, lost, and convective portions of the total gain.

          ! METHODOLOGY EMPLOYED:
          ! We must cycle through all of the radiant systems because each
          ! surface could feel the effect of more than one radiant system.
          ! Note that the energy radiated to people is assumed to affect them
          ! but them it is assumed to be convected to the air.  This is why
          ! the convective portion shown below has two parts to it.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY : NumOfZones
  USE DataHeatBalance,   ONLY : Zone
  USE DataHeatBalFanSys, ONLY : SumConvHTRadSys, SumLatentHTRadSys, &
                                QHTRadSysToPerson, QHTRadSysSurf, MaxRadHeatFlux
  USE DataSurfaces,      ONLY : Surface, TotSurfaces
  USE General,           ONLY : RoundSigDigits
  USE DataInterfaces,    ONLY : ShowContinueError, ShowWarningError, ShowSevereError, &
                                ShowFatalError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SmallestArea = 0.001d0   ! Smallest area in meters squared (to avoid a divide by zero)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RadSurfNum ! Counter for surfaces receiving radiation from radiant heater
  INTEGER :: RadSysNum  ! Counter for the radiant systems
  INTEGER :: SurfNum    ! Pointer to the Surface derived type
  INTEGER :: ZoneNum    ! Pointer to the Zone derived type
  REAL(R64) :: ThisSurfIntensity ! temporary for W/m2 term for rad on a surface


          ! FLOW:
          ! Initialize arrays
  SumConvHTRadSys   = 0.0D0
  SumLatentHTRadSys = 0.0D0
  QHTRadSysSurf     = 0.0D0
  QHTRadSysToPerson = 0.0D0

  DO RadSysNum = 1, NumOfHighTempRadSys

    ZoneNum = HighTempRadSys(RadSysNum)%ZonePtr

    QHTRadSysToPerson(ZoneNum) = QHTRadSource(RadSysNum)               &
                                *HighTempRadSys(RadSysNum)%FracRadiant &
                                *HighTempRadSys(RadSysNum)%FracDistribPerson

    SumConvHTRadSys(ZoneNum) = SumConvHTRadSys(ZoneNum) &
                              +QHTRadSource(RadSysNum)*HighTempRadSys(RadSysNum)%FracConvect

    SumLatentHTRadSys(ZoneNum) = SumLatentHTRadSys(ZoneNum) &
                                +QHTRadSource(RadSysNum)*HighTempRadSys(RadSysNum)%FracLatent

    DO RadSurfNum = 1, HighTempRadSys(RadSysNum)%TotSurfToDistrib
      SurfNum = HighTempRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      IF (Surface(SurfNum)%Area > SmallestArea)  THEN
        ThisSurfIntensity = (QHTRadSource(RadSysNum)                &
                                  *HighTempRadSys(RadSysNum)%FracRadiant &
                                  *HighTempRadSys(RadSysNum)%FracDistribToSurf(RadSurfNum) &
                                  /Surface(SurfNum)%Area )
        QHTRadSysSurf(SurfNum) = QHTRadSysSurf(SurfNum) + ThisSurfIntensity

        IF (ThisSurfIntensity > MaxRadHeatFlux) THEN ! CR 8074, trap for excessive intensity (throws off surface balance )
          CALL ShowSevereError('DistributeHTRadGains:  excessive thermal radiation heat flux intensity detected')
          CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
          CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
          CALL ShowContinueError('Occurs in ZoneHVAC:HighTemperatureRadiant = '//Trim(HighTempRadSys(RadSysNum)%Name))
          CALL ShowContinueError('Radiation intensity = '//Trim(RoundSigDigits(ThisSurfIntensity,2))//' [W/m2]')
          CALL ShowContinueError('Assign a larger surface area or more surfaces in ZoneHVAC:HighTemperatureRadiant' )
          CALL ShowFatalError('DistributeHTRadGains:  excessive thermal radiation heat flux intensity detected')
        ENDIF
      ELSE ! small surface
        CALL ShowSevereError('DistributeHTRadGains:  surface not large enough to receive thermal radiation heat flux')
        CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
        CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
        CALL ShowContinueError('Occurs in ZoneHVAC:HighTemperatureRadiant = '//Trim(HighTempRadSys(RadSysNum)%Name))
        CALL ShowContinueError('Assign a larger surface area or more surfaces in ZoneHVAC:HighTemperatureRadiant' )
        CALL ShowFatalError('DistributeHTRadGains:  surface not large enough to receive thermal radiation heat flux')

      ENDIF

    END DO

  END DO

          ! Here an assumption is made regarding radiant heat transfer to people.
          ! While the QHTRadSysToPerson array will be used by the thermal comfort
          ! routines, the energy transfer to people would get lost from the perspective
          ! of the heat balance.  So, to avoid this net loss of energy which clearly
          ! gets added to the zones, we must account for it somehow.  This assumption
          ! that all energy radiated to people is converted to convective energy is
          ! not very precise, but at least it conserves energy.
  DO ZoneNum = 1, NumOfZones
    SumConvHTRadSys(ZoneNum) = SumConvHTRadSys(ZoneNum) + QHTRadSysToPerson(ZoneNum)
  END DO

  RETURN

END SUBROUTINE DistributeHTRadGains


SUBROUTINE ReportHighTempRadiantSystem(RadSysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simply produces output for the high temperature radiant system.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour,outputfiledebug
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE DataLoopNode,    ONLY : Node
  USE DataSurfaces,    ONLY : Surface

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum  ! Index for the low temperature radiant system under consideration within the derived types

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: NotOperating = -9999.d0   ! Some unreasonable value that should clue the user in that this is not running

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  IF (HighTempRadSys(RadSysNum)%HeaterType == Gas) THEN
    HighTempRadSys(RadSysNum)%GasPower   = QHTRadSource(RadSysNum)/HighTempRadSys(RadSysNum)%CombustionEffic
    HighTempRadSys(RadSysNum)%GasEnergy  = HighTempRadSys(RadSysNum)%GasPower*TimeStepSys*SecInHour
    HighTempRadSys(RadSysNum)%ElecPower  = 0.0d0
    HighTempRadSys(RadSysNum)%ElecEnergy = 0.0d0
  ELSE IF (HighTempRadSys(RadSysNum)%HeaterType == Electric) THEN
    HighTempRadSys(RadSysNum)%GasPower   = 0.0d0
    HighTempRadSys(RadSysNum)%GasEnergy  = 0.0d0
    HighTempRadSys(RadSysNum)%ElecPower  = QHTRadSource(RadSysNum)
    HighTempRadSys(RadSysNum)%ElecEnergy = HighTempRadSys(RadSysNum)%ElecPower*TimeStepSys*SecInHour
  ELSE
    CALL ShowWarningError('Someone forgot to add a high temperature radiant heater type to the reporting subroutine')
  END IF
  HighTempRadSys(RadSysNum)%HeatPower  = QHTRadSource(RadSysNum)
  HighTempRadSys(RadSysNum)%HeatEnergy = HighTempRadSys(RadSysNum)%HeatPower*TimeStepSys*SecInHour

  RETURN

END SUBROUTINE ReportHighTempRadiantSystem


REAL(r64) FUNCTION SumHATsurf(ZoneNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
          ! The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
          ! and should be updated accordingly.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces
  USE DataHeatBalance
  USE DataHeatBalSurface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum     ! Zone number

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: SurfNum     ! Surface number
  REAL(r64)           :: Area        ! Effective surface area

          ! FLOW:
  SumHATsurf = 0.0d0

  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    Area = Surface(SurfNum)%Area

    IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        ! The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
        Area = Area + SurfaceWindow(SurfNum)%DividerArea
      END IF

      IF (SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
        ! Window frame contribution
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea &
          * (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn) * SurfaceWindow(SurfNum)%FrameTempSurfIn
      END IF

      IF (SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntShadeOn &
           .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntBlindOn) THEN
        ! Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea &
          * (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn) * SurfaceWindow(SurfNum)%DividerTempSurfIn
      END IF
    END IF

    SumHATsurf = SumHATsurf + HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum)
  END DO

  RETURN

END FUNCTION SumHATsurf

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

END MODULE HighTempRadiantSystem

