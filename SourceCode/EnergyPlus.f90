      PROGRAM EnergyPlus


!      NOTICE

!      Copyright © 1996-2013 The Board of Trustees of the University of Illinois and The Regents of the
!      University of California through Ernest Orlando Lawrence Berkeley National Laboratory.  All rights
!      reserved.

!      Portions of the EnergyPlus(tm) software package have been developed and copyrighted by other
!      individuals, companies and institutions.  These portions have been incorporated into the EnergyPlus
!      software package under license.

!      In addition to the primary authorship of the LBNL Simulation Research Group (
!      http://simulationresearch.lbl.gov/) and the UIUC Building Systems Laboratory (
!      http://www.bso.uiuc.edu/), the following have contributed to EnergyPlus V1.0:

!      Portions of the EnergyPlus weather processor were developed by US Department of Energy, Building
!      Technologies Program, www.energyplus.gov

!      Portions of the input processing, output processing, weather processor, BLAST Translator were
!      developed by US Army Corps of Engineers, Construction Engineering Research Laboratories, 2902 Newmark
!      Drive, Champaign IL  61821. www.cecer.army.mil

!      Portions of this software package were developed by Linda Lawrie of DHL Consulting LLC.

!      Portions of this software package were developed by C.O. Pedersen Associates.

!      Portions of the EnergyPlus utility software (EP-Launch, IDFEditor, DOE2Translator, HVAC-Diagram,
!      ExpandObjects, CSVProc, System Templates, and convertESOMTR) were developed by GARD Analytics, Inc.
!      1028 Busse Highway, Park Ridge, Illinois 60068-1802, USA (847) 698-5690, www.gard.com.  GARD Analytics
!      performed independent verification and validation testing of the software after developing the testing
!      strategy and plan.  GARD Analytics was also responsible for gas absorption chiller, desiccant
!      dehumidifier, ice storage (simple), table reports and economics.

!      Portions of flow resolver, chiller models (absorption, electric, const cop, engine-driven, gas-
!      turbine), generator models (diesel electric, gas turbine), furnace models, heat recovery loop, plant
!      loop, plant condenser loop, air-change dependent inside film coefficients were developed by Oklahoma
!      State University, 110 Engineering North, Stillwater, OK 74078.

!      Portions of EnergyPlus related to the models for EMPD moisture calculations, DX coils, furnace/unitary
!      systems, cooling towers, air-to-air heat pumps, air distribution systems, refrigerated cases, electric
!      EIR chillers, packaged terminal heat pumps, desuperheater air and water heating coils, and heat pump
!      water heaters were developed by University of Central Florida, Florida Solar Energy Center (FSEC),
!      1679 Clearlake Road, Cocoa, FL  32922, www.fsec.ucf.edu/.

!      Portions of EnergyPlus were developed by the National Renewable Energy Laboratory (NREL), 1617 Cole
!      Blvd, Golden, CO 80401.

!      EnergyPlus v1.0.1, v1.0.2, v1.0.3, v1.1, v1.1.1 (Wintel platform) includes a link to TRNSYS (The Transient
!      Energy System Simulation Tool) for photovoltaic calculations developed by Thermal Energy System Specialists,
!      2916 Marketplace Drive, Suite 104, Madison, WI 53719; Tel: (608) 274-2577. EnergyPlus v1.2 and later
!      includes Photovoltaic calculations implemented in EnergyPlus by Thermal Energy System Specialists.
!      This model was originally developed by Oystein Ulleberg, Institute for Energy Technology, Norway -- based on
!      the Duffie and Beckman equivalent one-diode model.

!      Portions of this software package that convert certain stand-alone heat transfer models for slab-on-
!      grade and basement foundations were developed by William Bahnfleth, Cynthia Cogil, and Edward
!      Clements, Department of Architectural Engineering, Pennsylvania State University, 224 Engineering Unit
!      A, University Park, Pennsylvania  16802-1416, (814) 863-2076.

!      The concept and initial implementation for the EnergyPlus COM/DLL version (Wintel platform) was made
!      possible through cooperation with DesignBuilder Software, Ltd, Andy Tindale - an EnergyPlus
!      collaborative developer.

!      The thickness, conductivity, density and specific heat values of the material layers for the
!      constructions in the Composite Wall Construction reference data set have been taken from the ASHRAE
!      report "Modeling Two- and Three-Dimensional Heat Transfer through Composite Wall and Roof Assemblies
!      in Hourly  Energy Simulation Programs (1145-TRP)," by Enermodal Engineering Limited, Oak Ridge
!      National Laboratory, and the Polish Academy of Sciences, January 2001.

!      EnergyPlus v1.2 contains DELight2 (wintel platform), a simulation engine for daylighting and electric
!      lighting system analysis developed at Ernest Orlando Lawrence Berkeley National Laboratory.

!      Portions of the EnergyPlus v1.2 air distribution system calculations were written by George Walton of
!      the National Institute for Standards and Technology (NIST), 100 Bureau Drive, Gaithersburg, MD 20899,
!      (301) 975-6478.  The EnergyPlus AirflowNetwork model also includes portions of an early version of COMIS
!      (Conjunction Of Multizone Infiltration Specialists) developed by a multinational, multi-institutional
!      effort under the auspices of the International Energy Agency's Buildings and Community Systems Agreement
!      working group focusing on multizone air flow modeling (Annex 23) and now administered by the Swiss Federal
!      Laboratories for Materials Testing and Research (EMPA), Division 175, Überlandstrasse 129, CH-8600 Dübendorf,
!      Switzerland.

!      The EnergyPlus v1.2 model for displacement ventilation and cross-ventilation was developed
!      by Guilherme Carrilho da Graça and Paul Linden of the Department of Mechanical and Aerospace
!      Engineering, University of California, San Diego.

!      The EnergyPlus models for UFAD served zones were developed by Anna Liu and Paul Linden at the Department
!      of Mechanical and Aerospace Engineering, University of California, San Diego.

!      ASHRAE research project 1254-RP supported the development of the following features first added in
!      EnergyPlus v1.2.2:
!         DXSystem:AirLoop enhancements (valid as OA system equipment, new humidity control options);
!         New set point managers: SET POINT MANAGER:SINGLE ZONE HEATING, SET POINT MANAGER:SINGLE ZONE COOLING,
!                 and SET POINT MANAGER:OUTSIDE AIR PRETREAT;
!         New 2-stage DX coil with enhanced dehumidification option (COIL:DX:MultiMode:CoolingEmpirical);
!         Additional DESICCANT DEHUMIDIFIER:SOLID setpoint control option;
!      American Society of Heating Refrigerating and Air-Conditioning Engineers, Inc,,
!      1791 Tullie Circle, N.E., Atlanta, GA 30329. www.ashrae.org
!      Work performed by GARD Analytics, Inc., 1028 Busse Highway, Park Ridge, Illinois 60068-1802, USA.
!      www.gard.com, November 2004.

!      EnergyPlus v1.2.2 and later versions (wintel platform) contains links to SPARK, a simulation engine for
!      detailed system modeling developed at Ernest Orlando Lawrence Berkeley National Laboratory in
!      conjunction with Ayres Sowell Associates, Inc.  SPARK was removed in V3.1 - April 2009 release.

!      The Ecoroof (Green Roof) model, first introduced in EnergyPlus v2.0, was developed at Portland State University,
!      by David Sailor and his students. It is based on the FASST vegetation models developed by Frankenstein and
!      Koenig for the US Army Corps of Engineers.

!      The HAMT (Heat And Moisture Transfer) model, first introduced in EnergyPlus v3.0.0 was developed by Phillip Biddulph,
!      Complex Built Environment Systems, The Bartlett School of Graduate Studies, University College London, Gower Street,
!      London WC1E 6BT, United Kingdom. http://www.cbes.ucl.ac.uk/.

!      The SQLite output module, first introduced in EnergyPlus v3.0.0, was developed by Gregory B. Stark, P.E.,
!      Building Synergies, LLC, 1860 Washington Street, Suite 208, Denver, Colorado 80203, United States.
!      http://www.buildingsynergies.com/

!      Refrigeration compressor performance data and refrigeration practices were provided by CDH Energy, Cazenovia, NY 12035.

!      NOTICE: The U.S. Government is granted for itself and others acting on its behalf a paid-up,
!      nonexclusive, irrevocable, worldwide license in this data to reproduce, prepare derivative works, and
!      perform publicly and display publicly.  Beginning five (5) years after permission to assert copyright
!      is granted, subject to two possible five year renewals, the U.S. Government is granted for itself and
!      others acting on its behalf a paid-up, non-exclusive, irrevocable worldwide license in this data to
!      reproduce, prepare derivative works, distribute copies to the public, perform publicly and display
!      publicly, and to permit others to do so.

!      TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

!      Other Acknowledgments

!      This work was supported by the Assistant Secretary for Energy Efficiency and Renewable Energy, Office
!      of Building Technologies Program of the US Department of Energy.

!      Additional support was provided by the Gas Technology Institute and the California Energy Commission.

!      The ice thermal storage module development was supported by the U.S. Department of Energy Office of
!      Electricity Delivery and Energy Reliability.

!      The HAMT (Heat And Moisture Transfer) model was supported by the Engineering and Physical Sciences Research Council (EPSRC),
!      the UK government agency for funding research and training in engineering and the physical sciences.

!      The SQLite output module was funded by Building Synergies, LLC and was made possible by inclusion of software code
!      from the SQLite project (http://www.sqlite.org/).

          ! PROGRAM INFORMATION:
          !       AUTHOR         Linda K. Lawrie, et al
          !       DATE WRITTEN   January 1997.....
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS PROGRAM:
          ! This program implements the calls for EnergyPlus (originally configured
          ! as the merger of BLAST/IBLAST and DOE-2 energy analysis programs).

          ! METHODOLOGY EMPLOYED:
          ! The method used in EnergyPlus is to simplify the main program as much
          ! as possible and contain all "simulation" code in other modules and files.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! data only modules
USE DataPrecisionGlobals
USE DataStringGlobals
USE DataGlobals
USE DataInterfaces
USE DataSystemVariables
USE DataTimings
USE DataEnvironment, ONLY: IgnoreSolarRadiation, IgnoreBeamRadiation, IgnoreDiffuseRadiation
          ! routine modules
USE InputProcessor
USE SimulationManager
USE ScheduleManager, ONLY: ReportOrphanSchedules
USE FluidProperties, ONLY: ReportOrphanFluids
USE Psychrometrics, ONLY: ShowPsychrometricSummary


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! PROGRAM PARAMETER DEFINITIONS:
          ! Note: General Parameters for the entire EnergyPlus program are contained
          ! in "DataGlobals.f90"
  CHARACTER(len=*), PARAMETER :: EPlusiniFormat="(/,'[',A,']',/,'dir=',A)"
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! PROGRAM LOCAL VARIABLE DECLARATIONS:
    INTEGER LFN  ! Unit Number for reads
    INTEGER, EXTERNAL :: GetNewUnitNumber
    LOGICAL EPlusINI
    INTEGER TempIndx
    CHARACTER(len=10)  :: cEnvValue=' '
    INTEGER :: iostatus
    LOGICAL :: FileExists

!
!
!                           INITIALIZE VARIABLES
!
                             Time_Start=epElapsedTime()
#ifdef EP_Detailed_Timings
                             CALL epStartTime('EntireRun=')
#endif
      CALL CreateCurrentDateTimeString(CurrentDateTime)
      VerString=TRIM(VerString)//','//TRIM(CurrentDateTime)
      cEnvValue=' '
      CALL Get_Environment_Variable(DDOnlyEnvVar,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      DDOnly=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(ReverseDDEnvVar,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      ReverseDD=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(FullAnnualSimulation,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      FullAnnualRun=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cDisplayAllWarnings,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      DisplayAllWarnings=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True
      IF (DisplayAllWarnings) THEN
        DisplayAllWarnings=.true.
        DisplayExtraWarnings=.true.
        DisplayUnusedSchedules=.true.
        DisplayUnusedObjects=.true.
      ENDIF

      cEnvValue=' '
      CALL Get_Environment_Variable(cDisplayExtraWarnings,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        DisplayExtraWarnings=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cDisplayUnusedObjects,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        DisplayUnusedObjects=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cDisplayUnusedSchedules,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        DisplayUnusedSchedules=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cDisplayZoneAirHeatBalanceOffBalance,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        DisplayZoneAirHeatBalanceOffBalance=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cDisplayAdvancedReportVariables,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        DisplayAdvancedReportVariables=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cReportDuringWarmup,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        ReportDuringWarmup=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cIgnoreSolarRadiation,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        IgnoreSolarRadiation=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cMinimalSurfaceVariables,cEnvValue)
      cEnvValue=MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        CreateMinimalSurfaceVariables=(cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cSortIDD,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        SortedIDD = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(MinReportFrequencyEnvVar,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        cMinReportFrequency=cEnvValue   ! turned into value later

      cEnvValue=' '
      CALL Get_Environment_Variable(cDeveloperFlag,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        DeveloperFlag = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cIgnoreBeamRadiation,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        IgnoreBeamRadiation = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cIgnoreDiffuseRadiation,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        IgnoreDiffuseRadiation = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cSutherlandHodgman,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        SutherlandHodgman = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cMinimalShadowing,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        lMinimalShadowing = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(cTimingFlag,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        TimingFlag = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      ! Initialize env flags for air loop simulation debugging
      cEnvValue=' '
      CALL Get_Environment_Variable(TrackAirLoopEnvVar,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        TrackAirLoopEnvFlag  = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(TraceAirLoopEnvVar,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        TraceAirLoopEnvFlag  = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      cEnvValue=' '
      CALL Get_Environment_Variable(TraceHVACControllerEnvVar,cEnvValue)
      cEnvValue = MakeUPPERCase(cEnvValue)
      IF (cEnvValue /= Blank) &
        TraceHVACControllerEnvFlag  = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='T')  ! Yes or True

      INQUIRE(File='eplusout.end',EXIST=FileExists)
      IF (FileExists) THEN
        LFN=GetNewUnitNumber()
        open(unit=LFN,file='eplusout.end',action='read', iostat=iostatus)
        IF (iostatus /= 0) THEN
          CALL ShowFatalError('EnergyPlus: Could not open file "eplusout.end" for input (read).')
        ENDIF
        close(unit=LFN,status='delete')
      ENDIF

      INQUIRE(File='Energy+.ini',EXIST=EPlusINI)
      IF (EPlusINI) THEN
        LFN=GetNewUnitNumber()
        OPEN(UNIT=LFN,FILE='Energy+.ini',Action='read',IOSTAT=iostatus)
        IF (iostatus /= 0) THEN
         CALL ShowFatalError('EnergyPlus: Could not open file "Energy+.ini" for input (read).')
        ENDIF
        INQUIRE(UNIT=LFN,NAME=CurrentWorkingFolder)
        ! Relying on compiler to supply full path name here
        TempIndx=INDEX(CurrentWorkingFolder,PathChar,.true.)
        IF (TempIndx == 0) THEN
          CurrentWorkingFolder=' '
        ELSE
          CurrentWorkingFolder=CurrentWorkingFolder(1:TempIndx)
        ENDIF
                              !       Get directories from ini file
        CALL ReadINIFile(LFN,'program','dir',ProgramPath)

        CLOSE(LFN)
      ELSE
        CALL DisplayString('Missing Energy+.ini')
        ProgramPath='  '
        LFN=GetNewUnitNumber()
        OPEN(UNIT=LFN,File='Energy+.ini',Action='write',IOSTAT=iostatus)
        IF (iostatus /= 0) THEN
         CALL ShowFatalError('EnergyPlus: Could not open file "Energy+.ini" for output (write).')
        ENDIF
        ! Relying on compiler to supply full path name here
        INQUIRE(UNIT=LFN,NAME=CurrentWorkingFolder)
        TempIndx=INDEX(CurrentWorkingFolder,PathChar,.true.)
        IF (TempIndx == 0) THEN
          CurrentWorkingFolder=' '
        ELSE
          CurrentWorkingFolder=CurrentWorkingFolder(1:TempIndx)
        ENDIF
        WRITE(LFN,EPlusiniFormat) 'program',ProgramPath
        CLOSE(LFN)
      ENDIF
      TestAllPaths=.true.

      CALL DisplayString('EnergyPlus Starting')
      CALL DisplayString(VerString)


      OutputFileDebug=GetNewUnitNumber()
      OPEN (OutputFileDebug,FILE='eplusout.dbg',ACTION='write',IOSTAT=iostatus)
      IF (iostatus /= 0) THEN
       CALL ShowFatalError('EnergyPlus: Could not open file "eplusout.dbg" for output (write).')
      ENDIF

        !Call ProcessInput to produce the IDF file which is read by all of the
        ! Get input routines in the rest of the simulation

      CALL ProcessInput

      CALL ManageSimulation

      CALL ShowMessage('Simulation Error Summary *************')

      CALL GenOutputVariablesAuditReport

      CALL ShowPsychrometricSummary

      CALL ReportOrphanRecordObjects
      CALL ReportOrphanFluids
      CALL ReportOrphanSchedules

      CALL EndEnergyPlus
!



CONTAINS

SUBROUTINE CreateCurrentDateTimeString(CurrentDateTimeString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Be able to supply a current date/time string from intrinsic calls so
          ! that one is always available.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) :: CurrentDateTimeString

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   integer, dimension(8) :: value
   character(len=15) datestring  ! supposedly returns blank when no date available.
!value(1)   Current year
!value(2)   Current month
!value(3)   Current day
!value(4)   Time difference with respect to UTC in minutes (0-59)
!value(5)   Hour of the day (0-23)
!value(6)   Minutes (0-59)
!value(7)   Seconds (0-59)
!value(8)   Milliseconds (0-999)

  CALL DATE_AND_TIME(date=datestring,values=value)
  IF (datestring /= Blank) THEN
    WRITE(CurrentDateTimeString,'(1X,"YMD=",I4,".",I2.2,".",I2.2,1X,I2.2,":",I2.2)')  &
       value(1),value(2),value(3),value(5),value(6)
  ELSE
    CurrentDateTimeString=' unknown date/time'
  ENDIF

  RETURN

END SUBROUTINE CreateCurrentDateTimeString

SUBROUTINE ReadINIFile(UnitNumber,Heading,KindofParameter,DataOut)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine reads the .ini file and retrieves
          ! the path names for the files from it.

          ! METHODOLOGY EMPLOYED:
          ! Duplicate the kind of reading the Windows "GetINISetting" would
          ! do.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataStringGlobals
USE DataSystemVariables


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


  ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER UnitNumber                 ! Unit number of the opened INI file
  CHARACTER(len=*) Heading           ! Heading for the parameters ('[heading]')
  CHARACTER(len=*) KindofParameter   ! Kind of parameter to be found (String)
  CHARACTER(len=*) DataOut           ! Output from the retrieval

  ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: LineLength=PathLimit+10

  ! INTERFACE BLOCK SPECIFICATIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      CHARACTER(len=LineLength) :: LINE=' '
      CHARACTER(len=LineLength) :: LINEOut=' '
      CHARACTER(len=20) Param
      integer IHEAD,ILB,IRB,IEQ,IPAR,IPOS,ILEN
      INTEGER ReadStat
      LOGICAL EndofFile
      LOGICAL Found
      LOGICAL NewHeading

      DataOut='           '


            ! I tried ADJUSTL(TRIM(KindofParameter)) and got an internal compiler error

      Param=TRIM(KindofParameter)
      Param=ADJUSTL(Param)
      ILEN=LEN_TRIM(Param)
      REWIND(UnitNumber)
      EndofFile=.false.
      Found=.false.
      NewHeading=.false.

 700  FORMAT(A)

      DO WHILE (.not. EndofFile .and. .not. Found)
        READ(UnitNumber,700,IOSTAT=ReadStat) LINE
        IF (ReadStat < GoodIOStatValue) THEN
          EndofFile=.true.
          EXIT
        ENDIF

        IF (LEN_TRIM(LINE) == 0) CYCLE      ! Ignore Blank Lines

        CALL ConvertCasetoLower(LINE,LINEOut)    ! Turn line into lower case
!        LINE=LINEOut

        IHEAD=INDEX(LINEOut,Heading)
        IF (IHEAD .EQ. 0) CYCLE

!                                  See if [ and ] are on line
        ILB=INDEX(LINEOut,'[')
        IRB=INDEX(LINEOut,']')
        IF (ILB == 0 .AND. IRB == 0) CYCLE
        IF (INDEX(LINEOut,'['//TRIM(Heading)//']') == 0) CYCLE    ! Must be really correct heading line
        ILB=0
        IRB=0

!                                  Heading line found, now looking for Kind
        DO WHILE (.not. EndofFile .and. .not. NewHeading)
          READ(UnitNumber,700,IOSTAT=ReadStat) LINE
          IF (ReadStat < GoodIOStatValue) THEN
            EndofFile=.true.
            EXIT
          ENDIF
          LINE=ADJUSTL(LINE)

          IF (LEN_TRIM(LINE) == 0) CYCLE      ! Ignore Blank Lines

          CALL ConvertCasetoLower(LINE,LINEOut)    ! Turn line into lower case
!         LINE=LINEOut

          ILB=INDEX(LINEOut,'[')
          IRB=INDEX(LINEOut,']')
          NewHeading=(ILB /= 0 .and. IRB /= 0)

!                                  Should be a parameter line
!                                  KindofParameter = string
          IEQ=INDEX(LINEOut,'=')
          IPAR=INDEX(LINEOut,TRIM(Param))
          IF (IEQ == 0) CYCLE
          IF (IPAR == 0) CYCLE
          IF (IPAR /= 1) CYCLE
          IF (INDEX(LINEOut,TRIM(Param)//'=') == 0) CYCLE      ! needs to be param=

!                                  = found and parameter found.
          IF (IPAR > IEQ) CYCLE

!                                  parameter = found
!                                  Set output string to start with non-blank character

          DataOut=ADJUSTL(LINE(IEQ+1:))
          Found=.true.
          EXIT

        END DO

      END DO


      SELECT CASE (Param)

        CASE('dir')
          IPOS=LEN_TRIM(DataOut)
          IF (IPOS /= 0) THEN
                             ! Non-blank make sure last position is valid path character
                             !  (Set in DataStringGlobals)

            IF (DataOut(IPOS:IPOS) /= PathChar) THEN
              DataOut(IPOS+1:IPOS+1)=PathChar
            ENDIF

          ENDIF


        CASE DEFAULT
      END SELECT


RETURN


END SUBROUTINE ReadINIFile

END PROGRAM EnergyPlus


