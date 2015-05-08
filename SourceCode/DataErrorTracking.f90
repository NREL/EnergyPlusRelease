MODULE DataErrorTracking      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for summary "errors" that are tracked
          ! and used in a summary at the end of a successful run.  Also other variables such
          ! as might be used during "fatal/end" processing.

          ! METHODOLOGY EMPLOYED:          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: SearchCounts=20
  CHARACTER(len=*), PARAMETER, DIMENSION(SearchCounts) ::  &
               MessageSearch=(/'InterZone Surface Areas',  &
                               'CAUTION -- Interzone   ',  &
                               'Node Connection Error  ',  &
                               'InterZone Surface Azimu',  &
                               'InterZone Surface Tilts',  &
                               'Suspected non-planar   ',  &
                               'Deprecated             ',  &
                               'Floor Tilt=            ',  &
                               'Roof/Ceiling Tilt=     ',  &
                               'View factors not       ',  &
                               'Unbalanced exhaust air ',  &
                               'Loads Initialization   ',  &
                               'CalcDaylightMapPoints: ',  &
                               'Zone Air Heat Balance  ',  &
                               'occupant density is ext',  &
                               'Temperature (low) out o',  &
                               'Temperature (high) out ',  &
                               'nominally unused       ',  &
                               'InfraredTransparent    ',  &
                               'No reporting elements  '/)
  CHARACTER(len=*), PARAMETER, DIMENSION(SearchCounts) ::  &
               Summaries    =(/'InterZone Surface Areas -- mismatch   ',  &
                               'Interzone surfaces - different zones  ',  &
                               'Node Connection Errors                ',  &
                               'InterZone Surface Azimuths -- mismatch',  &
                               'InterZone Surface Tilts -- mismatch   ',  &
                               'Likely non-planar surfaces            ',  &
                               'Deprecated Features or Key Values     ',  &
                               'Incorrect Floor Tilt                  ',  &
                               'Incorrect Roof/Ceiling Tilt           ',  &
                               'Incomplete View factors               ',  &
                               'Unbalanced exhaust air flow           ',  &
                               'Loads Initialization did not Converge ',  &
                               'CalcDaylightMapPoints: Window         ',  &
                               'Zone Air Heat Balance Warnings        ',  &
                               'Occupant density is extremely high    ',  &
                               'Temperature (low) out of bounds       ',  &
                               'Temperature (high) out of bounds      ',  &
                               'Nominally Unused Constructions        ',  &
                               'Material:InfraredTransparent usage    ',  &
                               'No Reporting Elements requested       '/)
  ! in below -- simple line end <CR>.  End of Whole message <CRE>
  INTEGER, PARAMETER :: iDTEstrLeng=693
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! InterZone Surface Areas -- mismatch
         MoreDetails_1 = 'Area mismatch errors happen when the interzone surface in zone A is<CR>'//  &
               'not the same size as it''s companion in zone B.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Interzone surfaces - different zones
         MoreDetails_2 = ' '
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Node Connection Errors
         MoreDetails_3 = 'Node connection errors are often caused by spelling mistakes in a node name field.<CR>'//  &
               'To track down the problem, search the idf file for each node name listed to see if it<CR>'// &
               'occurs in the expected input fields.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! InterZone Surface Azimuths -- mismatch
         MoreDetails_4 = 'The azimuths (outward facing angle) of two interzone surfaces should not be the same.<CR>'//  &
               'Normally, the absolute difference between the two azimuths will be 180 degrees.<CR>'//  &
               'You can turn on the report: Output:Surfaces:List,Details; to inspect your surfaces.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! InterZone Surface Tilts -- mismatch
         MoreDetails_5 = ' '
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Likely non-planar surfaces
         MoreDetails_6 = 'EnergyPlus Surfaces should be planar. If the error indicates a small increment for the<CR>'//  &
               'out of planar bounds, then the calculations are likely okay though you should try to fix<CR>'//  &
               'the problem. If a greater increment, the calculations will likely be incorrect.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Deprecated Features or Key Values
         MoreDetails_7 = 'A deprecated feature warning/severe error indicates that you are using a feature which will be<CR>'//  &
               'removed in a future release. The new feature is likely included in the EnergyPlus version you are<CR>'//  &
               'using.  Consider switching now to avoid future problems.<CR>'//  &
               'A deprecated key value message indicates you are using an out-dated key value in your input file.<CR>'//  &
               'While EnergyPlus may continue to accept these values, some other input file readers may not.<CR>'//  &
               'Consider changing to values that are included as valid in the Energy+.idd for these objects.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Incorrect Floor Tilt
         MoreDetails_8 = 'Floors are usually flat and "tilted" 180 degrees.  If you get this error message,<CR>'//   &
               'it''s likely that you need to reverse the vertices of the surface to remove the error.<CR>'//  &
               'EnergyPlus will attempt to fix the vertices for the running simulation.<CR>'//   &
               'You can turn on the report: Output:Surfaces:List,Details; to inspect your surfaces.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Incorrect Roof/Ceiling Tilt
         MoreDetails_9 = 'Flat roofs/ceilings are "tilted" 0 degrees. Pitched roofs should be "near" 0 degrees.<CR>'//  &
               'If you get this error message, it''s likely that you need to reverse the vertices of <CR>'//  &
               'the surface to remove the error. EnergyPlus will attempt to fix the vertices for the <CR"'//  &
               'running simulation. You can turn on the report: Output:Surfaces:List,Details; <CR>'//  &
               'to inspect your surfaces.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Incomplete View factors
         MoreDetails_10 = 'Incomplete view factors can result from incorrect floor specifications (such as tilting 0<CR>'//  &
               'instead of 180) or not enough surfaces in a zone to make an enclosure.  The error message<CR>'//  &
               'also shows an enforced repciprocity value.  You can decide if you need to make geometry<CR>'//  &
               'changes based on that value.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Unbalanced exhaust air flow
         MoreDetails_11 = 'Unbalanced exhaust air flow errors can occur when exhaust fans are running but there is no<CR>'//  &
               'supply air. Turn off exhaust fans when the system is not running may help resolve the problem.<CR>'//  &
               'Time shown is first occurrence of error.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Loads Initialization did not Converge
         MoreDetails_12 = '1) very high thermal mass such as very thick concrete (solution: increase max number of warmup<CR>'//  &
               '   days in the BUILDING object);<CR>'//  &
               '2) moderate mass and inadequate space conditioning such that the building keeps getting warmer<CR>'//  &
               '   and warmer on successive days (solution: add HVAC, check building thermal properties,<CR>'// &
               '   check if infiltration is included, make sure HVAC properly controlled);<CR>'//  &
               '3) a soil layer modeled below the concrete slab - (solution remove this layer and read about<CR>'//  &
               '   ground temperatures in the Auxiliary Programs document).<CR>'//  &
               '4) unreasonable (too small) limits in the BUILDING object for temperature (.4 default) or<CR>'//  &
               '   loads tolerances (.04 default)<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! CalcDaylightMapPoints: Window
         MoreDetails_13 = 'Window is too close to map points for accurate calculation.  Suggested change is to create<CR>'//  &
               'Output:IlluminanceMap coordinates (x,y,z) that are more "inside" the zone<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Zone Air Heat Balance Warnings
         MoreDetails_14 = 'Zone Air Heat Balance out of Balance warnings are currently used by developers.<CR>'//  &
               'Users can safely ignore these warnings.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Occupant density is extremely high
         MoreDetails_15 = 'The occupant density warning is provided to alert you to potential conditions that can cause<CR>'//  &
               'problems with the heat balance calculations. Too high a density could be cause for severe<CR>'//  &
               'temperature out of bounds errors in a zone leading to program termination.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Temperature (low) out of bounds AND Temperature (high) out of bounds
         MoreDetails_16 = 'A temperature out of bounds problem can be caused by several things. The user should check:<CR>'//  &
               '1) the weather environment (including the horizontal IR from sky)<CR>'//  &
               '2) the level of interal gains with respect to the zone<CR>'//  &
               '3) the thermal properties of their materials.  And other things.<CR>'//  &
               'A common cause is a building with no or little thermal mass - all materials with Material:NoMass'//  &
               'definitions.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! Nominally unused constructions
         MoreDetails_18 = 'The nominally unused constructions warning is provided to alert you to potential conditions'//  &
            ' that can cause<CR>'//  &
               'extra time during simulation. Each construction is calculated by the algorithm indicated in the '//  &
               'HeatBalanceAlgorithm<CR>'//  &
               'object. You may remove the constructions indicated (when you use the DisplayExtraWarnings option).<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! InfraredTransparent constructions in non-interzone surfaces
         MoreDetails_19 = 'Using Material:InfraredTransparent materials in constructions are correctly used '//  &
               'in interzone surface<CR>'//  &
               'constructions. Warnings are given if they are used in other kinds of surfaces.<CR>'//  &
               'They CANNOT currently be used with ConductionFiniteDifference algorithms.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER ::  &  ! No reporting elements requested
         MoreDetails_20 = 'No Reporting elements have been requested. You will see no output values from your run.<CR>'//  &
               'Add Output:Variable, Output:Meter, Output:Table:SummaryReports, Output:Table:Monthly, '//  &
               'Output:Table:TimeBins<CR>'//  &
               'objects to your input file to receive output values from the simulation.<CRE>'
  CHARACTER(len=iDTEstrLeng), PARAMETER, DIMENSION(SearchCounts) ::  &
          MoreDetails = (/MoreDetails_1,   &
                          MoreDetails_2,   &
                          MoreDetails_3,   &
                          MoreDetails_4,   &
                          MoreDetails_5,   &
                          MoreDetails_6,   &
                          MoreDetails_7,   &
                          MoreDetails_8,   &
                          MoreDetails_9,   &
                          MoreDetails_10,  &
                          MoreDetails_11,  &
                          MoreDetails_12,  &
                          MoreDetails_13,  &
                          MoreDetails_14,  &
                          MoreDetails_15,  &
                          MoreDetails_16,  &   ! Details 16 applies to both temperature out of bounds
                          MoreDetails_16,  &
                          MoreDetails_18,  &
                          MoreDetails_19,  &
                          MoreDetails_20/)     ! errors.

  INTEGER, PARAMETER :: MaxRecurringErrorMsgLength = 250  ! Maximum error message length for recurring error messages

          ! DERIVED TYPE DEFINITIONS
  TYPE RecurringErrorData

    CHARACTER(len=MaxRecurringErrorMsgLength) :: Message  = ' '  ! Message to be written to "error file" at end of simulation
    INTEGER  :: Count    = 0   ! Count of total times this recurring error message has been called
    INTEGER  :: WarmupCount = 0 ! Count of times this recurring error message has been called during warmup
    INTEGER  :: SizingCount = 0 ! Count of times this recurring error message has been called during sizing
    REAL(r64)     :: MaxValue = 0.0d0 ! Max of the values passed for this recurring error message
    REAL(r64)     :: MinValue = 0.0d0 ! Min of the values passed for this recurring error message
    REAL(r64)     :: SumValue = 0.0d0 ! Sum of the values passed for this recurring error message
    CHARACTER(len=32) :: MaxUnits=' '  ! units for Max values
    CHARACTER(len=32) :: MinUnits=' '  ! units for Min values
    CHARACTER(len=32) :: SumUnits=' '  ! units for Sum values
    LOGICAL  :: ReportMax= .FALSE. ! Flag to report max value
    LOGICAL  :: ReportMin= .FALSE. ! Flag to report min value
    LOGICAL  :: ReportSum= .FALSE. ! Flag to report sum value

  END TYPE RecurringErrorData

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(SearchCounts) :: MatchCounts=0
  LOGICAL :: AbortProcessing=.false.    ! Flag used to if currently in "abort processing"
  INTEGER :: NumRecurringErrors = 0     ! Number of stored recurring error messages
  TYPE (RecurringErrorData),   ALLOCATABLE, DIMENSION(:) :: RecurringErrors
  INTEGER :: TotalSevereErrors = 0 ! Counter
  INTEGER :: TotalWarningErrors = 0 ! Counter
  INTEGER :: TotalSevereErrorsDuringWarmup = 0 ! Counter
  INTEGER :: TotalWarningErrorsDuringWarmup = 0 ! Counter
  INTEGER :: TotalSevereErrorsDuringSizing = 0 ! Counter
  INTEGER :: TotalWarningErrorsDuringSizing = 0 ! Counter
  INTEGER :: TotalMultipliedWindows = 0 ! Counter
  INTEGER :: TotalCoincidentVertices = 0 ! Counter
  INTEGER :: TotalDegenerateSurfaces = 0 ! Counter
  INTEGER :: TotalReceivingNonConvexSurfaces = 0 ! Counter
  INTEGER :: TotalCastingNonConvexSurfaces = 0 ! Counter
  INTEGER :: TotalRoomAirPatternTooLow = 0 ! Counter
  INTEGER :: TotalRoomAirPatternTooHigh = 0 ! Counter
  LOGICAL :: AskForConnectionsReport=.false.  ! Flag used to tell when connections should be reported
  LOGICAL :: AskForSurfacesReport=.false.  ! Flag used to tell when surfaces should be reported
  LOGICAL :: AskForPlantCheckOnAbort = .false. ! flag used to tell if plant structure can be checked
  LOGICAL :: ExitDuringSimulations = .false. ! flag used to tell if program is in simulation mode when fatal occurs
  CHARACTER(len=250) :: LastSevereError=' '

!     NOTICE
!
!     Copyright � 1996-2013 The Board of Trustees of the University of Illinois
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

END MODULE DataErrorTracking
