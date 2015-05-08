MODULE DELIGHTMANAGERF

        ! MODULE INFORMATION
        !       AUTHOR         Robert J. Hitchcock
        !       DATE WRITTEN   August 2003
        !       MODIFIED       January 2004
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS MODULE:

        ! Defines INTERFACE Statements for Fortran calls to the DElightManagerC.cpp module.
        ! The DElightManager.cpp module in turn defines the C/C++ calls to the DElight DLL.
        ! Also, contains subroutines for performing associated operations.

        ! METHODOLOGY EMPLOYED:

        ! C Language Implementation of DOE2.1d and Superlite 3.0
        ! Daylighting Algorithms with new Complex Fenestration System
        ! analysis algorithms.
        ! The original DOE2 daylighting algorithms and implementation
        ! in FORTRAN were developed by F.C. Winkelmann at the
        ! Lawrence Berkeley National Laboratory.
        ! The original Superlite algorithms and implementation in FORTRAN
        ! were developed by Michael Modest and Jong-Jin Kim
        ! under contract with Lawrence Berkeley National Laboratory.
        !
        ! REFERENCES:

        ! "Daylighting Calculation in DOE-2," F.C.Winkelmann, LBL-11353, May 1983
        ! "Daylighting Simulation in the DOE-2 Building Energy Analysis Program,"
        ! F.C. Winkelmann and S. Selkowitz, Energy and Buildings 8(1985)271-286

        ! OTHER NOTES:

        ! INTERFACE Statements:
        !       SUBROUTINE DElightDaylightCoefficients
        !       Calculates Daylight Factors due to both simple and complex fenestration systems (CFS)
        !       for each reference point in each zone with a DElight Daylighting Object.

        !       SUBROUTINE DElightElecLtgCtrl
        !       Calculates Interior Daylight Illuminance at each reference point in each zone for given time step,
        !       and Power Reduction Factor for electric lighting in response to Interior Daylight Illuminance.

        !       SUBROUTINE DElightFreeMemory
        !       Frees dynamic memory allocated by the DElight processes.

        ! SUBROUTINEs:
        !       SUBROUTINE DElightInputGenerator
        !       Generates a DElight input file from EnergyPlus processed input.

        !       SUBROUTINE SetupDElightOutput4EPlus
        !       Sets up EnergyPlus output variables for DElight related variables.

        ! FUNCTIONs:
        !       FUNCTION ReplaceBlanksWithUnderscores
        !       Returns a representation of the InputString with blanks replaced with underscores.
        !       Used in generating DElight input file, which does not accept blanks in user name strings.


        ! USE STATEMENTS:
        ! <use statements for data only modules>
USE DataPrecisionGlobals
USE DataDElight

IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC DElightInputGenerator
PUBLIC GenerateDElightDaylightCoefficients

        ! MODULE VARIABLE DECLARATIONS:

        ! INTERFACE BLOCK SPECIFICATIONS
        ! INTERFACE Statements for Fortran calls to DElightManagerC.cpp module
INTERFACE
    SUBROUTINE DElightDaylightCoefficients (dBldgLat, iErrorFlag)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Robert J. Hitchcock
        !       DATE WRITTEN   August 2003
        !       MODIFIED       February 2004 - Remove ProgramPath and StringLength arguments
        !                       RJH - Jul 2004 - Add error handling for errors detected in DElight
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        !       Interface to DElight DLL for routine that
        !       Calculates Daylight Factors due to both simple and complex fenestration systems (CFS)
        !       for each reference point in each zone with a DElight Daylighting Object.

        ! METHODOLOGY EMPLOYED:
        ! na

        !DEC$ ATTRIBUTES C :: DElightDaylightCoefficients
        ! parameters have the VALUE attribute by default because
        ! the subroutine has the C attribute
        USE DataPrecisionGlobals
        REAL(r64) :: dBldgLat
        INTEGER iErrorFlag
        !DEC$ ATTRIBUTES REFERENCE :: iErrorFlag
        ! pass return value for iErrorFlag by reference
    END SUBROUTINE
END INTERFACE

INTERFACE
    SUBROUTINE DElightElecLtgCtrl (iNameLength, cZoneName, dBldgLat, &
                                dHISKF, dHISUNF, dCloudFraction, dSOLCOSX, dSOLCOSY, dSOLCOSZ, &
                                pdPowerReducFac, iErrorFlag)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Robert J. Hitchcock
        !       DATE WRITTEN   August 2003
        !       MODIFIED       RJH - Jul 2004 - Add error handling for errors detected in DElight
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        !       Interface to DElight DLL for routine that
        !       Calculates Interior Daylight Illuminance at each reference point in each zone for given time step,
        !       and Power Reduction Factor for electric lighting in response to Interior Daylight Illuminance.

        ! METHODOLOGY EMPLOYED:
        ! na
        USE DataPrecisionGlobals

        !DEC$ ATTRIBUTES C :: DElightElecLtgCtrl
        ! parameters have the VALUE attribute by default because
        ! the subroutine has the C attribute
        INTEGER iNameLength
        CHARACTER(len=*) cZoneName
        !DEC$ ATTRIBUTES REFERENCE :: cZoneName
        ! pass character strings by reference
        REAL(r64) :: dBldgLat
        REAL(r64) :: dHISKF
        REAL(r64) :: dHISUNF
        REAL(r64) :: dCloudFraction
        REAL(r64) :: dSOLCOSX
        REAL(r64) :: dSOLCOSY
        REAL(r64) :: dSOLCOSZ
        REAL(r64) :: pdPowerReducFac
        !DEC$ ATTRIBUTES REFERENCE :: pdPowerReducFac
        ! pass return value for pdPowerReducFac by reference
        INTEGER iErrorFlag
        !DEC$ ATTRIBUTES REFERENCE :: iErrorFlag
        ! pass return value for iErrorFlag by reference
    END SUBROUTINE
END INTERFACE

INTERFACE
    SUBROUTINE DElightFreeMemory ()

        !   SUBROUTINE INFORMATION:
        !       AUTHOR         Robert J. Hitchcock
        !       DATE WRITTEN   August 2003
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        !   PURPOSE OF THIS SUBROUTINE:
        !       Interface to DElight DLL for routine that
        !       Frees dynamic memory allocated by the DElight processes.

        !   METHODOLOGY EMPLOYED:
        !   na

        !DEC$ ATTRIBUTES C :: DElightFreeMemory
        ! parameters have the VALUE attribute by default because
        ! the subroutine has the C attribute
    END SUBROUTINE
END INTERFACE

INTERFACE
    SUBROUTINE DElightOutputGenerator (iOutputFlag)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Robert J. Hitchcock
        !       DATE WRITTEN   January 2004
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        !       Interface to DElight DLL for routine that
        !       Generates output data file based on output flag

        ! METHODOLOGY EMPLOYED:
        ! na

        !DEC$ ATTRIBUTES C :: DElightOutputGenerator
        ! parameters have the VALUE attribute by default because
        ! the subroutine has the C attribute
        INTEGER iOutputFlag
    END SUBROUTINE
END INTERFACE

CONTAINS

          ! SUBROUTINE SPECIFICATIONS FOR MODULE DElightManagerF
          ! MODULE SUBROUTINES:


SUBROUTINE DElightInputGenerator

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Robert J. Hitchcock
          !       DATE WRITTEN   August 2003
          !       MODIFIED       February 2004 - Changes to accomodate mods in DElight IDD
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates a DElight input file from EnergyPlus processed input.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals     ! Gives access to too many things to keep track of
    USE DataHeatBalance ! Gives access to Building, Zone(izone)%var and Lights(ilights) data
    USE DataEnvironment ! Gives access to Site data
    USE DataSurfaces        ! Gives access to Surface data
    USE DataStringGlobals   ! Gives access to Program Path and Current Time/Date
    USE DataIPShortCuts     ! Gives access to commonly dimensioned field names, etc for getinput
    USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList
    USE DataDaylighting
    USE OutputReportPredefined
    USE General, ONLY: RoundSigDigits
    USE InternalHeatGains, ONLY: GetDesignLightingLevelForZone,CheckLightsReplaceableMinMaxForZone
    USE DataInterfaces

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: cModuleObjectDElight='Daylighting:DELight:Controls'
    CHARACTER(len=*), PARAMETER :: cModuleObjectCFS='Daylighting:DELight:ComplexFenestration'
    CHARACTER(len=*), PARAMETER :: cModuleObjectRefPt='Daylighting:DELight:ReferencePoint'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    integer unit                ! Unit number on which to write file
    integer iNumDElightObjs     ! Counter for Daylighting:DElight objects
    integer iDElight            ! Loop variable for Daylighting:DElight objects
    integer iNumDElightZones    ! Counter for Thermal Zones with hosted Daylighting:DElight objects
    integer izone               ! Loop variable for zones
    integer iNumOpaqueSurfs     ! Counter for opaque surfaces in each zone
    integer isurf               ! Loop variable for surfaces
    integer iSurfaceFirst       ! starting loop variable for surfaces
    integer iSurfaceLast        ! ending loop variable for surfaces
    integer iNumWindows         ! Counter for windows hosted in each surface
    integer iwndo               ! Loop variable for windows
    integer iwndo2              ! Loop variable for windows
    integer iwndo3              ! Loop variable for windows
    integer iNumVertices        ! Counter for surface vertices
    integer ivert               ! Loop variable for surface vertices
    integer iconstruct          ! Index for construction type of surfaces
    integer iconst              ! Loop variable for construction type of surfaces
    integer iMatlLayer          ! Index for the outside (i.e., 1st) Material Layer for a Construction
    integer iNumRefPts          ! Counter for reference points
    integer irefpt              ! Loop variable for reference points
    integer iLtgCtrlType        ! Integer converter for Lighting Control Type
    integer,external :: getnewunitnumber    ! External function for a new unit number
    REAL(r64) rExtVisRefl            ! Exterior visible reflectance of a material
    REAL(r64) rLightLevel            ! installed lighting level for current zone
    REAL(r64) CosBldgRelNorth        ! Cosine of Building rotation
    REAL(r64) SinBldgRelNorth        ! Sine of Building rotation
    REAL(r64) CosZoneRelNorth        ! Cosine of Zone rotation
    REAL(r64) SinZoneRelNorth        ! Sine of Zone rotation
    REAL(r64) Xb                     ! temp var for transformation calc
    REAL(r64) Yb                     ! temp var for transformation calc
    REAL(r64) rTotalZoneFraction     ! Zone Fraction sum for all RefPts in each Zone
    REAL(r64), DIMENSION(3)          :: RefPt_WCS_Coord
    CHARACTER(len=MaxNameLength), DIMENSION(2)  :: AlphaArrayDElight
    REAL(r64), DIMENSION(6)                     :: RealNumArrayDElight
    INTEGER                                     :: IOSTAT
    INTEGER                                     :: NumAlphasDElight
    INTEGER                                     :: NumNumsDElight
    CHARACTER(len=MaxNameLength), DIMENSION(4)  :: AlphaArrayCFS
    REAL(r64), DIMENSION(1)                     :: RealNumArrayCFS
    INTEGER                                     :: NumAlphasCFS
    INTEGER                                     :: NumNumsCFS
    CHARACTER(len=MaxNameLength), DIMENSION(2)  :: AlphaArrayRefPt
    REAL(r64), DIMENSION(5)                     :: RealNumArrayRefPt
    INTEGER                                     :: NumAlphasRefPt
    INTEGER                                     :: NumNumsRefPt
    INTEGER                                     :: iNumWndoConsts
    INTEGER, DIMENSION(100)                     :: iWndoConstIndexes
    LOGICAL                                     :: lWndoConstFound     ! Flag for non-unique window const index
    CHARACTER(len=MaxNameLength) cNameWOBlanks  ! Name without blanks
    LOGICAL                                     :: ErrorsFound
    INTEGER                                     :: iTotNumDElightRefPtObjs
    INTEGER                                     :: iNumDElightCFS
    INTEGER                                     :: iHostedCFS
    INTEGER                                     :: iCFS
    LOGICAL                                     :: lWndoIsDoppelganger ! Flag for doppelganger window test
    INTEGER                                     :: iDoppelganger
    LOGICAL                                     :: ldoTransform
    REAL(r64)                                   :: roldAspectRatio,rnewAspectRatio
    REAL(r64) :: Xo, XnoRot, Xtrans
    REAL(r64) :: Yo, YnoRot, Ytrans

    ! Init the ErrorsFound flag
    ErrorsFound = .false.

    CALL CheckForGeometricTransform(ldoTransform,roldAspectRatio,rnewAspectRatio)

    ! Init the counter for Thermal Zones with hosted Daylighting:DElight objects
    iNumDElightZones = 0

    ! Init the counter for Window Construction types for writing to Library Data section of DElight input file
    iNumWndoConsts = 0

    ! Open a file for writing DElight input from EnergyPlus data
    unit=getnewunitnumber()

    ! Hardwire file name to eplusout.delightin in the current working directory
    open(unit,file='eplusout.delightin',action='write',err=999)

    ! Start of DElight input file
    write(unit,901) CurrentDateTime
    901 format('Version EPlus : DElight input generated from EnergyPlus processed input ',A)

    ! Building Data Section retrieved from DataHeatBalance and DataEnvironment modules
    ! Remove any blanks from the Building Name for ease of input to DElight
    cNameWOBlanks = ReplaceBlanksWithUnderscores(BuildingName)
    write(unit,902) cNameWOBlanks,  &
                    Latitude,  &
                    Longitude,  &
                    Elevation*M2FT,  &
                    BuildingAzimuth,  &
                    TimeZoneNumber
    902 format(/,'Building_Name ',A,/,  &
                'Site_Latitude  ',f12.4,/,  &
                'Site_Longitude ',f12.4,/,  &
                'Site_Altitude  ',f12.4,/,  &
                'Bldg_Azimuth   ',f12.4,/,  &
                'Site_Time_Zone ',f12.4,/,  &
                'Atm_Moisture  0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07',/,  &
                'Atm_Turbidity 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12')

    ! Calc cos and sin of Building Relative North values for later use in transforming Reference Point coordinates
    CosBldgRelNorth = COS(-BuildingAzimuth*DegToRadians)
    SinBldgRelNorth = SIN(-BuildingAzimuth*DegToRadians)

    ! Get the set of Daylighting:DElight objects
    iNumDElightObjs = GetNumObjectsFound(cModuleObjectDElight)

    ! Loop through the Daylighting:DElight objects checking for a host Zone
    DO iDElight = 1, iNumDElightObjs

        ! Get the data items for the current DElight object
        CALL GetObjectItem(cModuleObjectDElight,iDElight,AlphaArrayDElight,NumAlphasDElight, &
                            RealNumArrayDElight,NumNumsDElight,IOSTAT)

        izone=FindItemInList(AlphaArrayDElight(2),Zone%Name,NumOfZones)
        IF (izone == 0) THEN
            CALL ShowSevereError('DElightInputGenerator: Illegal Zone Name='//TRIM(AlphaArrayDElight(2)))
            CALL ShowContinueError('..in Daylighting:DElight, User Supplied DElight Zone Name='//TRIM(AlphaArrayDElight(1)))
            ErrorsFound=.true.
        ELSE  ! valid zone

            ! Count the number of DElight Reference Point objects input for this Thermal Zone.
            iNumRefPts = 0

            ! Get the set of all Daylighting:DElight:Reference Point objects in the IDF
            iTotNumDElightRefPtObjs = GetNumObjectsFound(cModuleObjectRefPt)

            ! Loop through the Daylighting:DElight:Reference Point objects checking for the current DElight Zone host
            rTotalZoneFraction = 0.0d0    ! init Zone Fraction accumulator
            DO iRefPt = 1, iTotNumDElightRefPtObjs

                ! Get the data items for the current DElight Reference Point object
                CALL GetObjectItem(cModuleObjectRefPt,iRefPt,AlphaArrayRefPt,NumAlphasRefPt, &
                                    RealNumArrayRefPt,NumNumsRefPt,IOSTAT)

                ! Is this RefPt hosted by current DElight Zone?
                IF (AlphaArrayRefPt(2) == AlphaArrayDElight(1)) THEN

                    ! Count this correctly hosted RefPt
                    iNumRefPts = iNumRefPts + 1

                    ! Sum Fractions of Zone controlled by RefPt
                    rTotalZoneFraction = rTotalZoneFraction + RealNumArrayRefPt(4)

                ENDIF

            ENDDO

            ! Register Error if 0 DElight RefPts have been input for valid DElight object
            IF (iNumRefPts < 1) THEN
                CALL ShowSevereError('No Reference Points input for DElight Zone =' &
                //TRIM(AlphaArrayDElight(1)))
                ErrorsFound=.true.
            ENDIF

            ! If this zone already assigned a daylighting type, error
            IF (ZoneDaylight(izone)%DaylightType /= NoDaylighting) THEN
              CALL ShowSevereError('GetDElightDaylighting: Attempted to apply DElight Daylighting to a Zone with '//  &
                                   'Previous Daylighting')
              CALL ShowContinueError('Error discovered in "Daylighting:DElight" for Zone='//TRIM(AlphaArrayDElight(2)))
              CALL ShowContinueError('Previously applied Daylighting Type='//  &
                                       TRIM(DaylightTypes(ZoneDaylight(izone)%DaylightType)))
              ErrorsFound=.true.
            ENDIF
            ! Init the DElight members of the ZoneDaylight structure for this Thermal Zone
            ! ZoneDaylight(izone)%TotalDElightRefPts > 0 is the trigger for DElight calcs
            ZoneDaylight(izone)%TotalDElightRefPts = iNumRefPts

            ! ZoneDaylight(izone)%DaylightType is another trigger for DElight calcs
            ZoneDaylight(izone)%DaylightType = DElightDaylighting

            ! Register Warning if more than 100 DElight RefPts have been input for valid DElight object
            IF (iNumRefPts > 100) THEN
                ! Restrict to 100 Ref Pt maximum
                ZoneDaylight(izone)%TotalDElightRefPts = 100
                CALL ShowWarningError('Maximum of 100 Reference Points exceeded for DElight Zone =' &
                //TRIM(AlphaArrayDElight(1)))
                CALL ShowWarningError('  Only first 100 Reference Points included in DElight analysis')
            ENDIF
            ALLOCATE(ZoneDayLight(izone)%DaylRefPtAbsCoord(ZoneDaylight(izone)%TotalDElightRefPts,3))
            ZoneDayLight(izone)%DaylRefPtAbsCoord=0.0d0

            ! RJH 2008-03-07: Allocate and Init DaylIllumAtRefPt array for this DElight zone
            ALLOCATE(ZoneDaylight(izone)%DaylIllumAtRefPt(ZoneDaylight(izone)%TotalDElightRefPts))
            ZoneDaylight(izone)%DaylIllumAtRefPt=0.0d0
            ! following not used in DElight but allocated for convenience
            ALLOCATE(ZoneDaylight(izone)%GlareIndexAtRefPt(ZoneDaylight(izone)%TotalDElightRefPts))
            ZoneDaylight(izone)%GlareIndexAtRefPt = 0.0d0

            ! Register Warning if total Zone Fraction for all DElight RefPts < 1.0
            IF (rTotalZoneFraction < 1.0d0) THEN
                CALL ShowWarningError('Total Electric Lighting Zone Fraction less than 1.0 for DElight Zone =' &
                //TRIM(AlphaArrayDElight(1)))
            ENDIF

            ! Register Error if total Zone Fraction for all DElight RefPts > 1.0
            IF (rTotalZoneFraction > 1.0d0) THEN
                CALL ShowSevereError('Total Electric Lighting Zone Fraction greater than 1.0 for DElight Zone =' &
                //TRIM(AlphaArrayDElight(1)))
                ErrorsFound=.true.
            ENDIF

            ! Increment counter of Thermal Zones with valid hosted DElight object
            iNumDElightZones = iNumDElightZones + 1

        END IF
    ENDDO

    ! Get the number of input Complex Fenestration objects for reference throughout this subroutine
    iNumDElightCFS = GetNumObjectsFound(cModuleObjectCFS)

    ! Zone Data Section
    write(unit,903) iNumDElightZones
    903 format(/,'ZONES',/,'N_Zones ',I4)

    ! Loop through the Daylighting:DElight objects searching for a match to the current Zone
    DO iDElight = 1, iNumDElightObjs

        ! Get the data items for the current DElight object
        CALL GetObjectItem(cModuleObjectDElight,iDElight,AlphaArrayDElight,NumAlphasDElight, &
                        RealNumArrayDElight,NumNumsDElight,IOSTAT)

        izone=FindItemInList(AlphaArrayDElight(2),Zone%Name,NumOfZones)
        IF (izone /= 0) THEN

            rLightLevel=GetDesignLightingLevelForZone(izone)
            CALL CheckLightsReplaceableMinMaxForZone(izone)

            ! Write this Zone to the DElight input file
            ! Remove any blanks from the Zone Name for ease of input to DElight
            cNameWOBlanks = ReplaceBlanksWithUnderscores(Zone(izone)%Name)
            write(unit,904) trim(cNameWOBlanks),  &
                            Zone(izone)%OriginX*M2FT, Zone(izone)%OriginY*M2FT, Zone(izone)%OriginZ*M2FT,  &
                            Zone(izone)%RelNorth,  &
                            Zone(izone)%Multiplier*Zone(izone)%ListMultiplier,  &
                            Zone(izone)%FloorArea*M22FT2,  &
                            Zone(izone)%Volume*M32FT3,  &
                            rLightLevel / (Zone(izone)%FloorArea*M22FT2 + 0.00001d0),  &
                            RealNumArrayDElight(2),  &
                            RealNumArrayDElight(3),  &
                            int(RealNumArrayDElight(4)),  &
                            RealNumArrayDElight(5),  &
                            RealNumArrayDElight(6)*M22FT2

            904 format(/,'ZONE DATA',/,  &
                        'Zone ',A,/,  &
                        'BldgSystem_Zone_Origin ', f12.4, f12.4, f12.4,/,  &
                        'Zone_Azimuth    ', f12.4,/,  &
                        'Zone_Multiplier ', I5,/,  &
                        'Zone_Floor_Area ', f12.4,/,  &
                        'Zone_Volume     ', f12.4,/,  &
                        'Zone_Installed_Lighting ', f12.4,/,  &
                        'Min_Input_Power    ', f12.4,/,  &
                        'Min_Light_Fraction ', f12.4,/,  &
                        'Light_Ctrl_Steps   ', I3,/,  &
                        'Light_Ctrl_Prob    ', f12.4,/,  &
                        'View_Azimuth  0.0',/, &
                        'Max_Grid_Node_Area ', f12.4)

            ! Calc cos and sin of Zone Relative North values for later use in transforming Reference Point coordinates
            CosZoneRelNorth = COS(-Zone(izone)%RelNorth*DegToRadians)
            SinZoneRelNorth = SIN(-Zone(izone)%RelNorth*DegToRadians)

            ! Zone Lighting Schedule Data Section
            ! NOTE: Schedules are not required since hourly values are retrieved from EnergyPlus as needed
            write(unit,905)
            905 format(/,'ZONE LIGHTING SCHEDULES',/,'N_Lt_Scheds 0')

            ! Zone Surface Data Section
            ! Count the number of opaque surfaces bounding the current zone
            iNumOpaqueSurfs = 0
            iSurfaceFirst = Zone(izone)%SurfaceFirst
            iSurfaceLast = Zone(izone)%SurfaceLast

            DO isurf = iSurfaceFirst,iSurfaceLast
                IF (Surface(isurf)%class == SurfaceClass_Wall) iNumOpaqueSurfs = iNumOpaqueSurfs + 1
                IF (Surface(isurf)%class == SurfaceClass_Roof) iNumOpaqueSurfs = iNumOpaqueSurfs + 1
                IF (Surface(isurf)%class == SurfaceClass_Floor) iNumOpaqueSurfs = iNumOpaqueSurfs + 1
            ENDDO ! Zone Opaque Surface loop

            write(unit,906) iNumOpaqueSurfs
            906 format(/,'ZONE SURFACES',/,'N_Surfaces ',I4)

            ! Write each opaque bounding Surface to the DElight input file
            DO isurf = iSurfaceFirst,iSurfaceLast

                ! Only process "opaque bounding" surface types
                IF ((Surface(isurf)%class == SurfaceClass_Wall) .OR. &
                    (Surface(isurf)%class == SurfaceClass_Roof) .OR. &
                    (Surface(isurf)%class == SurfaceClass_Floor)) THEN

                    ! Get the Construction index for this Surface
                    iconstruct = Surface(isurf)%Construction

                    ! Is this Surface exposed to the exterior?
                    IF (Surface(isurf)%ExtSolar) THEN
                        ! Get the index for the outside (i.e., 1st) Material Layer for this Construction
                        iMatlLayer = Construct(iconstruct)%LayerPoint(1)
                        ! Get the outside visible reflectance of this material layer
                        ! (since Construct(iconstruct)%ReflectVisDiffFront always appears to == 0.0)
                        rExtVisRefl = 1.0d0 - Material(iMatlLayer)%AbsorpVisible
                    ELSE
                        rExtVisRefl = 0.0d0
                    ENDIF

                    ! Remove any blanks from the Surface Name for ease of input to DElight
                    cNameWOBlanks = ReplaceBlanksWithUnderscores(Surface(isurf)%Name)
                    write(unit,907) cNameWOBlanks,  &
                                    Surface(isurf)%Azimuth,  &
                                    Surface(isurf)%Tilt,  &
                                    Construct(iconstruct)%ReflectVisDiffBack,  &
                                    rExtVisRefl,  &
                                    Surface(isurf)%Sides

                    907 format(/,'ZONE SURFACE DATA',/,  &
                                'Surface ',A,/,  &
                                'WCS_Azimuth ',f12.4,/,  &
                                'WCS_Tilt    ',f12.4,/,  &
                                'Vis_Refl    ',f12.4,/,  &
                                'Ext_Refl    ',f12.4,/,  &
                                'Gnd_Refl     0.2',/,  &
                                'N_WCS_Vertices ',I6)

                    ! Write out the vertex coordinates for each vertex
                    iNumVertices = Surface(isurf)%Sides
                    DO ivert=1,iNumVertices
                        write(unit,908) Surface(isurf)%vertex(ivert)%x*M2FT, &
                                        Surface(isurf)%vertex(ivert)%y*M2FT, &
                                        Surface(isurf)%vertex(ivert)%z*M2FT
                    ENDDO
                    908 format('Vertex ', f12.4, f12.4, f12.4)

                    ! Count each Window hosted by the current opaque bounding Surface
                    iNumWindows = 0
                    DO iwndo = iSurfaceFirst,iSurfaceLast
                        IF (Surface(iwndo)%class == SurfaceClass_Window) THEN
                            IF (Surface(iwndo)%BaseSurfName == Surface(isurf)%Name) THEN

                              ! Error if window has multiplier > 1 since this causes incorrect illuminance calc
                              IF (Surface(iwndo)%Multiplier > 1.0d0) THEN
                                CALL ShowSevereError('Multiplier > 1.0 for window '//TRIM(Surface(iwndo)%Name)// &
                                 ' not allowed since it is in a zone with DElight daylighting.')
                                ErrorsFound=.true.
                              ENDIF

                              ! Error if window has a shading device (blind/shade/screen) since
                              ! DElight cannot perform dynamic shading device deployment
                              IF (Surface(iwndo)%WindowShadingControlPtr > 0) THEN
                                CALL ShowSevereError('Shading Device on window '//TRIM(Surface(iwndo)%Name)// &
                                 ' dynamic control is not supported in a zone with DElight daylighting.')
                                ErrorsFound=.true.
                              ENDIF

                              ! Loop through all Doppelganger Surface Names to ignore these Windows
                              lWndoIsDoppelganger = .FALSE.
                              DO iCFS = 1, iNumDElightCFS

                                  ! Get the data items for the current CFS object
                                  CALL GetObjectItem(cModuleObjectCFS,iCFS,AlphaArrayCFS, &
                                                  NumAlphasCFS,RealNumArrayCFS,NumNumsCFS,IOSTAT)

                                  ! Is the current Window Surface a Doppelganger?
                                  IF (Surface(iwndo)%Name == AlphaArrayCFS(4)) THEN
                                      ! Ignore this Doppelganger Window
                                      lWndoIsDoppelganger = .TRUE.
                                  ENDIF

                              ENDDO ! CFS object loop A

                              IF (.NOT.lWndoIsDoppelganger) THEN
                                  iNumWindows = iNumWindows + 1
                              ENDIF

                            ENDIF ! Surface hosts Window test
                        ENDIF ! Window test
                    ENDDO ! Window loop

                    write(unit,909) iNumWindows
                    909 format(/,'SURFACE WINDOWS',/,'N_Windows ',I6)

                    ! If the current opaque bounding Surface hosts Windows,
                    ! then write each hosted Window to the DElight input file
                    ! and track the Window Construction type for later writing
                    IF (iNumWindows > 0) THEN
                        DO iwndo2 = iSurfaceFirst,iSurfaceLast
                            IF (Surface(iwndo2)%class == SurfaceClass_Window) THEN
                                IF (Surface(iwndo2)%BaseSurfName == Surface(isurf)%Name) THEN

                                    ! Loop through all Doppelganger Surface Names to ignore these Windows
                                    lWndoIsDoppelganger = .FALSE.
                                    DO iCFS = 1, iNumDElightCFS

                                        ! Get the data items for the current CFS object
                                        CALL GetObjectItem(cModuleObjectCFS,iCFS,AlphaArrayCFS, &
                                                            NumAlphasCFS,RealNumArrayCFS,NumNumsCFS,IOSTAT)

                                        ! Is the current Window Surface a Doppelganger?
                                        IF (Surface(iwndo2)%Name == AlphaArrayCFS(4)) THEN
                                            ! Ignore this Doppelganger Window
                                            lWndoIsDoppelganger = .TRUE.
                                        ENDIF

                                    ENDDO ! CFS object loop A

                                    IF (.NOT.lWndoIsDoppelganger) THEN

                                        ! Track unique window construction types here for later writing to
                                        ! the library section of DElight input file

                                        ! Get the Construction index for this Window Surface
                                        iconstruct = Surface(iwndo2)%Construction

                                        ! Has the current Construction index been encountered before?
                                        lWndoConstFound = .FALSE.
                                        DO iconst = 1, iNumWndoConsts
                                            IF (iconstruct == iWndoConstIndexes(iconst)) lWndoConstFound = .TRUE.
                                        ENDDO
                                        IF (.NOT.lWndoConstFound) THEN
                                            iNumWndoConsts = iNumWndoConsts + 1
                                            iWndoConstIndexes(iNumWndoConsts) = iconstruct
                                        ENDIF

                                        ! Write this Window to the DElight input file
                                        ! Remove any blanks from the Window Surface Name for ease of input to DElight
                                        cNameWOBlanks = ReplaceBlanksWithUnderscores(Surface(iwndo2)%Name)
                                        write(unit,910) cNameWOBlanks, &
                                                        iconstruct + 10000, &
                                                        Surface(iwndo2)%Sides
                                        ! Use WndoConstIndex + 10000 as the Glass Type Name
                                        ! to differentiate EPlus glass types within DElight

                                        910 format(/,'SURFACE WINDOW DATA',/, &
                                                    'Window     ',A,/, &
                                                    'Glass_Type ',I8,/, &
                                                    'Shade_Flag   0',/, &
                                                    'Overhang_Fin_Depth    0.0 0.0 0.0',/, &
                                                    'Overhang_Fin_Distance 0.0 0.0 0.0',/, &
                                                    'N_WCS_Vertices ',I4)

                                        ! Write out the vertex coordinates for each vertex
                                        iNumVertices = Surface(iwndo2)%Sides
                                        DO ivert=1,iNumVertices
                                            write(unit,908) Surface(iwndo2)%vertex(ivert)%x*M2FT, &
                                                            Surface(iwndo2)%vertex(ivert)%y*M2FT, &
                                                            Surface(iwndo2)%vertex(ivert)%z*M2FT
                                        ENDDO
                                    ENDIF !.NOT.lWndoIsDoppelganger

                                ENDIF ! Surface hosts Window2 test
                            ENDIF ! Window2 Class test
                        ENDDO ! Window2 loop
                    ENDIF ! Hosted Windows test

                    ! Write the number of CFS hosted by the current Opaque Bounding Surface
                    iHostedCFS = 0

                    ! Loop through the input CFS objects searching for a match to the current Opaque Bounding Surface
                    DO iCFS = 1, iNumDElightCFS

                        ! Get the data items for the current CFS object
                        CALL GetObjectItem(cModuleObjectCFS,iCFS,AlphaArrayCFS,NumAlphasCFS, &
                                            RealNumArrayCFS,NumNumsCFS,IOSTAT)

                        ! Does the current Opaque Bounding Surface host the current CFS object?
                        IF (Surface(isurf)%Name == AlphaArrayCFS(3)) THEN
                            ! Count this hosted CFS
                            iHostedCFS = iHostedCFS + 1
                        ENDIF

                    ENDDO ! CFS object loop 1

                    write(unit,911) iHostedCFS
                    911 format(/,'SURFACE CFS',/,'N_CFS ',I6)

                    ! Now write each of the hosted CFS data
                    ! Loop through the input CFS objects searching for a match to the current Opaque Bounding Surface
                    DO iCFS = 1, iNumDElightCFS

                        ! Get the data items for the current CFS object
                        CALL GetObjectItem(cModuleObjectCFS,iCFS,AlphaArrayCFS,NumAlphasCFS, &
                                            RealNumArrayCFS,NumNumsCFS,IOSTAT)

                        ! Does the current Opaque Bounding Surface host the current CFS object?
                        IF (Surface(isurf)%Name == AlphaArrayCFS(3)) THEN

                            ! Get the Doppelganger surface for this CFS
                            iDoppelganger = 0
                            DO iwndo3 = iSurfaceFirst,iSurfaceLast
                                IF (Surface(iwndo3)%class == SurfaceClass_Window) THEN

                                    ! Is the current Window Surface the Doppelganger for the current CFS?
                                    IF (Surface(iwndo3)%Name == AlphaArrayCFS(4)) THEN
                                        ! Store the window surface index for future reference
                                        iDoppelganger = iwndo3
                                    ENDIF
                                ENDIF
                            ENDDO

                            ! Make sure that a valid Doppelganger surface exists
                            IF (iDoppelganger > 0) THEN

                                ! Write the data for this hosted CFS

                                ! Remove any blanks from the CFS Name for ease of input to DElight
                                cNameWOBlanks = ReplaceBlanksWithUnderscores(AlphaArrayCFS(1))
                                iNumVertices = Surface(iDoppelganger)%Sides
                                write(unit,915) cNameWOBlanks, &
                                    AlphaArrayCFS(2), &
                                    RealNumArrayCFS(1), &
                                    iNumVertices

                                915 format(/,'COMPLEX FENESTRATION DATA',/, &
                                            'CFS_Name   ',A,/, &
                                            'CFS_Type   ',A,/, &
                                            'Fenestration_Rotation ',f12.4,/,  &
                                            'N_WCS_Vertices ',I4)

                                ! Write out the vertex coordinates for each vertex
                                DO ivert=1,iNumVertices
                                write(unit,908) Surface(iDoppelganger)%vertex(ivert)%x*M2FT, &
                                                Surface(iDoppelganger)%vertex(ivert)%y*M2FT, &
                                                Surface(iDoppelganger)%vertex(ivert)%z*M2FT
                                ENDDO
                            ENDIF
                            ! Register Error if there is no valid Doppelganger for current Complex Fenestration
                            IF (iDoppelganger == 0) THEN
                                CALL ShowSevereError('No Doppelganger Window Surface found for Complex Fenestration =' &
                                //TRIM(AlphaArrayCFS(1)))
                                ErrorsFound=.true.
                            ENDIF

                        ENDIF ! The current Opaque Bounding Surface hosts the current CFS object?

                    ENDDO ! CFS object loop 2

                ENDIF ! Opaque Bounding Surface test

            ENDDO ! Zone Surface loop

            ! Write ZONE REFERENCE POINTS
            write(unit,912) ZoneDaylight(izone)%TotalDElightRefPts
            912 format(/,'ZONE REFERENCE POINTS',/,'N_Ref_Pts ',I4)

            ! Keep an incremental count number of valid DElight Reference Points have been input for this DElight Zone
            iNumRefPts = 0

            ! Get the set of all Daylighting:DElight:Reference Point objects
            iTotNumDElightRefPtObjs = GetNumObjectsFound(cModuleObjectRefPt)

            ! Loop through the Daylighting:DElight:Reference Point objects checking for the current DElight Zone host
            DO iRefPt = 1, iTotNumDElightRefPtObjs

                ! Get the data items for the current DElight Reference Point object
                CALL GetObjectItem(cModuleObjectRefPt,iRefPt,AlphaArrayRefPt,NumAlphasRefPt, &
                                    RealNumArrayRefPt,NumNumsRefPt,IOSTAT)

                ! Is this RefPt hosted by current DElight Zone?
                IF (AlphaArrayRefPt(2) == AlphaArrayDElight(1)) THEN

                    ! Count this correctly hosted RefPt
                    iNumRefPts = iNumRefPts + 1

                    ! Limit to maximum of 100 RefPts
                    IF (iNumRefPts <= 100) THEN

                        IF (DaylRefWorldCoordSystem) THEN
                          RefPt_WCS_Coord(1) = RealNumArrayRefPt(1)
                          RefPt_WCS_Coord(2) = RealNumArrayRefPt(2)
                          RefPt_WCS_Coord(3) = RealNumArrayRefPt(3)
                        ELSE
                          !Transform reference point coordinates into building coordinate system
                          Xb = RealNumArrayRefPt(1)*CosZoneRelNorth &
                              - RealNumArrayRefPt(2)*SinZoneRelNorth &
                              + Zone(izone)%OriginX
                          Yb = RealNumArrayRefPt(1)*SinZoneRelNorth &
                              + RealNumArrayRefPt(2)*CosZoneRelNorth &
                              + Zone(izone)%OriginY
                          !Transform into World Coordinate System
                          RefPt_WCS_Coord(1) = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
                          RefPt_WCS_Coord(2) = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
                          RefPt_WCS_Coord(3) = RealNumArrayRefPt(3) + &
                                                  Zone(izone)%OriginZ
                          IF (ldoTransform) THEN  ! Geometry transform
                            Xo = RefPt_WCS_Coord(1) ! world coordinates.... shifted by relative north angle...
                            Yo = RefPt_WCS_Coord(2)
                            ! next derotate the building
                            XnoRot=Xo * CosBldgRelNorth + Yo * SinBldgRelNorth
                            YnoRot=Yo * CosBldgRelNorth - Xo * SinBldgRelNorth
                            ! translate
                            Xtrans = XnoRot * SQRT(rnewAspectRatio/roldAspectRatio)
                            Ytrans = YnoRot * SQRT(roldAspectRatio/rnewAspectRatio)
                            ! rerotate
                            RefPt_WCS_Coord(1) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth

                            RefPt_WCS_Coord(2) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth
                          ENDIF
                        ENDIF
                        ZoneDayLight(izone)%DaylRefPtAbsCoord(iNumRefPts,1:3)=RefPt_WCS_Coord(1:3)

                        ! Validate that Reference Point coordinates are within the host Zone
                        IF (RefPt_WCS_Coord(1) < Zone(izone)%MinimumX .or.  &
                            RefPt_WCS_Coord(1) > Zone(izone)%MaximumX) THEN
                          CALL ShowWarningError('DElightInputGenerator:'// &
                                'Reference point X Value outside Zone Min/Max X, Zone='//TRIM(Zone(izone)%Name))
                          CALL ShowSevereError('...X Reference Point= '// &
                                 TRIM(RoundSigDigits(RefPt_WCS_Coord(1),2))//', Zone Minimum X= '// &
                                 TRIM(RoundSigDigits(Zone(izone)%MinimumX,2))//', Zone Maximum X= '// &
                                 TRIM(RoundSigDigits(Zone(izone)%MaximumX,2)))
                                 ErrorsFound=.true.
                        ENDIF
                        IF (RefPt_WCS_Coord(2) < Zone(izone)%MinimumY .or.  &
                            RefPt_WCS_Coord(2) > Zone(izone)%MaximumY) THEN
                          CALL ShowWarningError('DElightInputGenerator:'//  &
                               'Reference point Y Value outside Zone Min/Max Y, Zone='// TRIM(Zone(izone)%Name))
                          CALL ShowSevereError('...Y Reference Point= '// &
                                 TRIM(RoundSigDigits(RefPt_WCS_Coord(2),2))//', Zone Minimum Y= '// &
                                 TRIM(RoundSigDigits(Zone(izone)%MinimumY,2))//', Zone Maximum Y= '// &
                                 TRIM(RoundSigDigits(Zone(izone)%MaximumY,2)))
                                 ErrorsFound=.true.
                        ENDIF
                        IF (RefPt_WCS_Coord(3) < Zone(izone)%MinimumZ .or.  &
                            RefPt_WCS_Coord(3) > Zone(izone)%MaximumZ) THEN
                          CALL ShowWarningError('DElightInputGenerator:'//  &
                               'Reference point Z Value outside Zone Min/Max Z, Zone='// TRIM(Zone(izone)%Name))
                          CALL ShowSevereError('...Z Reference Point= '// &
                                 TRIM(RoundSigDigits(RefPt_WCS_Coord(3),2))//', Zone Minimum Z= '// &
                                 TRIM(RoundSigDigits(Zone(izone)%MinimumZ,2))//', Zone Maximum Z= '// &
                                 TRIM(RoundSigDigits(Zone(izone)%MaximumZ,2)))
                                 ErrorsFound=.true.
                        ENDIF

                        ! Write this RefPt to the DElight input file

                        ! Remove any blanks from the RefPt Name for ease of input to DElight
                        cNameWOBlanks = ReplaceBlanksWithUnderscores(AlphaArrayRefPt(1))
                        iLtgCtrlType = RealNumArrayDElight(1)
!                            write(unit,913) iNumRefPts, &
                        write(unit,913) cNameWOBlanks, &
                                        RefPt_WCS_Coord(1)*M2FT, &
                                        RefPt_WCS_Coord(2)*M2FT, &
                                        RefPt_WCS_Coord(3)*M2FT,  &
                                        RealNumArrayRefPt(4),  &
                                        RealNumArrayRefPt(5)*LUX2FC,  &
                                        iLtgCtrlType

                        913 format(/,'ZONE REFERENCE POINT DATA',/, &
!                                        'Reference_Point ',I4,/, &
                                    'Reference_Point ',A,/, &
                                    'RefPt_WCS_Coords ', f12.4, f12.4, f12.4,/,  &
                                    'Zone_Fraction ',f12.4,/, &
                                    'Light_Set_Pt ',f12.4,/, &
                                    'Light_Ctrl_Type ',I4)

                      ! RJH 2008-03-07: Set up DaylIllumAtRefPt for output for this DElight zone RefPt

                      ! CurrentModuleObject='Daylighting:DELight:ReferencePoint'
                      CALL SetupOutputVariable('Daylighting Reference Point Illuminance [lux]', &
                        ZoneDaylight(izone)%DaylIllumAtRefPt(iNumRefPts), 'Zone', 'Average', &
                        AlphaArrayRefPt(1))

                      ! Predefined Reporting For Lighting Summary Report
                      CALL PreDefTableEntry(pdchDyLtZone,AlphaArrayRefPt(1),AlphaArrayDElight(2))
                      CALL PreDefTableEntry(pdchDyLtKind,AlphaArrayRefPt(1),'DElight')
                      ! (1=continuous, 2=stepped, 3=continuous/off)
                      SELECT CASE (INT(RealNumArrayDElight(1)))
                        CASE (1)
                          CALL PreDefTableEntry(pdchDyLtCtrl,AlphaArrayRefPt(1),'Continuous')
                        CASE (2)
                          CALL PreDefTableEntry(pdchDyLtCtrl,AlphaArrayRefPt(1),'Stepped')
                        CASE (3)
                          CALL PreDefTableEntry(pdchDyLtCtrl,AlphaArrayRefPt(1),'Continuous/Off')
                      END SELECT
                      CALL PreDefTableEntry(pdchDyLtFrac,AlphaArrayRefPt(1),RealNumArrayRefPt(4))
                      CALL PreDefTableEntry(pdchDyLtWInst, AlphaArrayRefPt(1), rLightLevel)
                      CALL PreDefTableEntry(pdchDyLtWCtrl, AlphaArrayRefPt(1), rLightLevel * RealNumArrayRefPt(4))

                    ENDIF ! Max 100 RefPt test

                ENDIF ! RefPt in current DElight Zone test

            ENDDO ! Ref Pt loop
        ENDIF ! Zone hosts DElight object test
    ENDDO ! Daylighting:DElight object loop

    ! Write BUILDING SHADES
    write(unit,914)
    914 format(/,'BUILDING SHADES',/,'N_BShades 0')

    ! Write LIBRARY DATA
    write(unit,920) iNumWndoConsts
    920 format(/,'LIBRARY DATA',/,'GLASS TYPES',/,'N_Glass_Types ',I4)

    ! Write GLASS TYPES
    ! VisBeamCoeffs are processed in EPlus by POLYF() function
    ! Use WndoConstIndex + 10000 as the Glass Type Name to differentiate EPlus glass types within DElight
    DO iconst = 1, iNumWndoConsts
        write(unit,921) iWndoConstIndexes(iconst) + 10000, &
                        Construct(iWndoConstIndexes(iconst))%TransDiffVis, &
                        Construct(iWndoConstIndexes(iconst))%ReflectVisDiffBack, &
                        Construct(iWndoConstIndexes(iconst))%TransVisBeamCoef(1), &
                        Construct(iWndoConstIndexes(iconst))%TransVisBeamCoef(2), &
                        Construct(iWndoConstIndexes(iconst))%TransVisBeamCoef(3), &
                        Construct(iWndoConstIndexes(iconst))%TransVisBeamCoef(4), &
                        Construct(iWndoConstIndexes(iconst))%TransVisBeamCoef(5), &
                        Construct(iWndoConstIndexes(iconst))%TransVisBeamCoef(6)

        921 format(/,'GLASS TYPE DATA',/, &
                    'Name ',I6,/, &
                    'EPlusDiffuse_Transmittance   ',f12.4,/, &
                    'EPlusDiffuse_Int_Reflectance ',f12.4,/, &
                    'EPlus_Vis_Trans_Coeff_1 ',f17.9,/, &
                    'EPlus_Vis_Trans_Coeff_2 ',f17.9,/, &
                    'EPlus_Vis_Trans_Coeff_3 ',f17.9,/, &
                    'EPlus_Vis_Trans_Coeff_4 ',f17.9,/, &
                    'EPlus_Vis_Trans_Coeff_5 ',f17.9,/, &
                    'EPlus_Vis_Trans_Coeff_6 ',f17.9)
    ENDDO ! Glass Type loop

    IF (ErrorsFound) CALL ShowFatalError('Problems with Daylighting:DElight input, see previous error messages')

    close(unit)

    return

    999 CALL ShowFatalError('DElightInputGenerator: Could not open file "eplusout.delightin" for output (write).')
    return

END SUBROUTINE DElightInputGenerator

SUBROUTINE GenerateDElightDaylightCoefficients (dLatitude, iErrorFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to provide an envelop to the DElightDaylightCoefficients
          ! routine (and remove IEEE calls from main EnergyPlus core routines).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE, INTRINSIC :: ieee_exceptions
  USE, INTRINSIC :: ieee_arithmetic
  USE, INTRINSIC :: ieee_features

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) :: dLatitude
  INTEGER iErrorFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

 CALL IEEE_SET_HALTING_MODE(IEEE_OVERFLOW,.FALSE.)
 CALL IEEE_SET_HALTING_MODE(IEEE_INVALID,.FALSE.)
 CALL IEEE_SET_HALTING_MODE(IEEE_DIVIDE_BY_ZERO,.FALSE.)
 CALL DElightDaylightCoefficients(dLatitude, iErrorFlag)
 CALL IEEE_SET_FLAG(ieee_all,.false.)
 CALL IEEE_SET_HALTING_MODE(IEEE_OVERFLOW,.TRUE.)
 CALL IEEE_SET_HALTING_MODE(IEEE_INVALID,.TRUE.)
 CALL IEEE_SET_HALTING_MODE(IEEE_DIVIDE_BY_ZERO,.TRUE.)

  RETURN

END SUBROUTINE GenerateDElightDaylightCoefficients

SUBROUTINE CheckForGeometricTransform(DoTransform,OldAspectRatio,NewAspectRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! check for geometrytransform in the daylighting access for reference points
          !

          ! METHODOLOGY EMPLOYED:
          ! once reference points  have been converted to WCS,
          !  change them to reflect a different aspect
          ! ratio for the entire building based on user input.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE DataDaylighting, ONLY: ZoneDaylight
  USE DataSurfaces, ONLY: WorldCoordSystem,AspectTransform
  USE DataInterfaces, ONLY: ShowWarningError

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Logical,  INTENT(INOUT)  :: doTransform
  REAL(r64), INTENT(INOUT) :: OldAspectRatio
  REAL(r64), INTENT(INOUT) :: NewAspectRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='GeometryTransform'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), DIMENSION(1) :: cAlphas
  REAL(r64), DIMENSION(2) :: rNumerics
  INTEGER            :: NAlphas
  INTEGER            :: NNum
  INTEGER            :: IOSTAT
  CHARACTER(len=2)   :: transformPlane

  !begin execution
  !get user input...
  doTransform=.false.
  OldAspectRatio = 1.0d0
  NewAspectRatio = 1.0d0

  IF (GetNumObjectsFound(CurrentModuleObject) == 1) then
     CALL GetObjectItem(CurrentModuleObject,1,cAlphas,NAlphas,rNumerics,NNum,IOSTAT,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
     OldAspectRatio = rNumerics(1)
     NewAspectRatio = rNumerics(2)
     transformPlane = cAlphas(1)
     IF (transformPlane /= 'XY') then
       CALL ShowWarningError(CurrentModuleObject//': invalid '//TRIM(cAlphaFieldNames(1))//  &
         '="'//TRIM(cAlphas(1))//'...ignored.')
     ENDIF
     doTransform = .true.
     AspectTransform = .true.
  Endif
  IF (WorldCoordSystem) THEN
    doTransform=.false.
    AspectTransform=.false.
  ENDIF

  Return

END SUBROUTINE CheckForGeometricTransform

SUBROUTINE SetupDElightOutput4EPlus

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Robert J. Hitchcock
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up EnergyPlus output variables for DElight related variables.
          ! RJH 2008-03-07: Only lighting power reduction factor is setup here
          ! Reference Point Daylight Illuminance [lux] is setup within DElightInputGenerator

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals     ! Gives access to NumOfZones
    USE DataInterfaces
    USE DataDaylighting, ONLY: ZoneDaylight
    USE DataHeatBalance, ONLY: Zone ! Gives access to Zone(izone)%var

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: iZone   ! DO loop counter for zones

    DO iZone = 1, NumOfZones
        IF (ZoneDaylight(iZone)%TotalDElightRefPts >= 1) THEN
            ! Set up lighting power reduction factor for output for this DElight zone
            ! CurrentModuleObject='Daylighting:DELight:Controls'
            CALL SetupOutputVariable('Daylighting Lighting Power Multiplier []', &
                ZoneDaylight(iZone)%ZonePowerReductionFactor, 'Zone', 'Average', &
                Zone(iZone)%Name)
        END IF
    END DO

    return

END SUBROUTINE SetupDElightOutput4EPlus

FUNCTION ReplaceBlanksWithUnderscores(InputString) RESULT (ResultString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Robert J. Hitchcock
          !       DATE WRITTEN   August 2003
          !       MODIFIED       From MakeUPPERCase function by Linda K. Lawrie
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function returns a representation of the InputString with blanks replaced with underscores.

          ! METHODOLOGY EMPLOYED:
          ! Uses the Intrinsic SCAN function to scan for Blank characters
          ! and replaces any found with underscore characters.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals, ONLY: MaxNameLength

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: InputString ! Input String
    CHARACTER(len=MaxNameLength) ResultString   ! Result String

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER Count              ! Loop Counter
    INTEGER Pos                ! Position in String representation
    INTEGER LengthInputString  ! Length (trimmed) of InputString

    ResultString=' '
    Pos=SCAN(InputString,' ')
    IF (POS /= 0) THEN
    LengthInputString=LEN_TRIM(InputString)
    DO Count=1,LengthInputString
        Pos=SCAN(' ',InputString(Count:Count))
        IF (Pos /= 0) THEN
            ResultString(Count:Count)='_'
        ELSE
            ResultString(Count:Count)=InputString(Count:Count)
        ENDIF
    END DO
    ResultString=TRIM(ResultString)
    ELSE
        ! String has no Blanks
        ResultString=TRIM(InputString)
    ENDIF

    RETURN

END FUNCTION ReplaceBlanksWithUnderscores

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

END MODULE DELIGHTMANAGERF





