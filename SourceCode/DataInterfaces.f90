MODULE DataInterfaces

          ! Module containing the routines dealing with Interfaces needed by some routines in EnergyPlus.

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Provide a global public area for interface statements that are used by routines in EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
 USE DataPrecisionGlobals

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

    INTERFACE
      SUBROUTINE ControlCompOutput(CompName,CompType,CompNum,FirstHVACIteration,QZnReq, &
                                   ActuatedNode,MaxFlow,MinFlow,TempInNode,TempOutNode, &
                                   ControlOffSet,AirMassFlow,Action,ControlCompTypeNum, &
                                   CompErrIndex,EquipIndex,LoopNum, LoopSide, BranchIndex, &
                                   ControlledZoneIndex)
        USE DataPrecisionGlobals

        CHARACTER(len=*), INTENT (IN)           :: CompName            ! The component Name
        CHARACTER(len=*), INTENT (IN)           :: CompType            ! Type of component
        INTEGER, INTENT (INOUT)                 :: CompNum             ! Index of component in component array
        LOGICAL, INTENT (IN)                    :: FirstHVACIteration  ! Flag for 1st HVAV iteration in the time step
        REAL(r64),    INTENT (IN)               :: QZnReq              ! Zone load to be met
        INTEGER, INTENT (IN)                    :: ActuatedNode        ! Node that controls unit output
        REAL(r64),    INTENT (IN)               :: MaxFlow             ! Maximum water flow
        REAL(r64),    INTENT (IN)               :: MinFlow             ! Minimum water flow
        INTEGER, INTENT (IN), OPTIONAL          :: TempInNode          ! Inlet node for output calculation
        INTEGER, INTENT (IN), OPTIONAL          :: TempOutNode         ! Outlet node for output calculation
        REAL(r64),    INTENT (IN)               :: ControlOffset       ! Tolerance
        REAL(r64),    INTENT (IN), OPTIONAL     :: AirMassFlow         ! Air mass flow rate
        INTEGER, INTENT (IN), OPTIONAL          :: Action              ! 1=reverse; 2=normal
        INTEGER, INTENT (INOUT)                 :: ControlCompTypeNum  ! Internal control comp for unit
        INTEGER, INTENT (INOUT)                 :: CompErrIndex        ! Error count for recurring error
        INTEGER, INTENT (IN), OPTIONAL          :: EquipIndex          ! Identifier for equipment of Outdoor Air Unit "ONLY"
        INTEGER, INTENT (IN), OPTIONAL          :: LoopNum             ! for plant components, plant loop index
        INTEGER, INTENT (IN), OPTIONAL          :: LoopSide            ! for plant components, plant loop side index
        INTEGER, INTENT (IN), OPTIONAL          :: BranchIndex         ! for plant components, plant branch index
        INTEGER, INTENT (IN), OPTIONAL          :: ControlledZoneIndex ! controlled zone index for the zone containing the component
      END SUBROUTINE ControlCompOutput


    END INTERFACE

  INTERFACE GetVariableKeyCountandType
    SUBROUTINE GetVariableKeyCountandType(varName,numKeys,varType,varAvgSum,varStepType,varUnits)
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)                  :: varName      ! Standard variable name
      INTEGER, INTENT(OUT)                          :: varType      ! 0=not found, 1=integer, 2=real, 3=meter
      INTEGER, INTENT(OUT)                          :: numKeys      ! Number of keys found
      INTEGER, INTENT(OUT)                          :: varAvgSum    ! Variable  is Averaged=1 or Summed=2
      INTEGER, INTENT(OUT)                          :: varStepType  ! Variable time step is Zone=1 or HVAC=2
      CHARACTER(len=*), INTENT(OUT)     :: varUnits     ! Units sting, may be blank
    END SUBROUTINE
  END INTERFACE

  INTERFACE GetVariableKeys
    SUBROUTINE GetVariableKeys(varName,varType,keyNames,keyVarIndexes)
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)                  :: varName       ! Standard variable name
      INTEGER, INTENT(IN)                           :: varType       ! 1=integer, 2=real, 3=meter
      CHARACTER(len=*), INTENT(OUT), DIMENSION(:)   :: keyNames      ! Specific key name
      INTEGER, INTENT(OUT), DIMENSION(:)            :: keyVarIndexes ! Array index for
    END SUBROUTINE
  END INTERFACE

  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    !  Use when you want to create your own message for the error file.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowContinueError(Message,Unit1,Unit2)
    !  Use when you are "continuing" an error message over several lines.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowContinueErrorTimeStamp(Message,Unit1,Unit2)
    !  Use when you are "continuing" an error message and want to show the environment, day and time.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowFatalError(Message,Unit1,Unit2)
    !  Use when you want the program to terminate after writing messages
    !  to appropriate files
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowSevereError(Message,Unit1,Unit2)
    !  Use for "severe" error messages.  Might have several severe tests and then terminate.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowSevereMessage(Message,Unit1,Unit2)
    !  Use for "severe" error messages.  that don't bump counts (recurring are used)
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowWarningError(Message,Unit1,Unit2)
    !  Use for "warning" error messages.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowWarningMessage(Message,Unit1,Unit2)
    !  Use for "warning" error messages that don't bump error counts
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE

  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

  INTERFACE
    SUBROUTINE ShowRecurringSevereErrorAtEnd(Message,Index,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                           ReportMaxUnits,ReportMinUnits,ReportSumUnits)
    USE DataPrecisionGlobals
    !  Use for recurring "severe" error messages shown once at end of simulation
    !  with count of occurences and optional max, min, sum
    CHARACTER(len=*) :: Message     ! Message automatically written to "error file" at end of simulation
    INTEGER, INTENT(INOUT)        :: Index       ! Recurring message index, if zero, next available index is assigned
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportMaxOf ! Track and report the max of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportMinOf ! Track and report the min of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportSumOf ! Track and report the sum of the values passed to this argument
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMaxUnits ! optional char string (<=15 length) of units for max value
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMinUnits ! optional char string (<=15 length) of units for min value
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportSumUnits ! optional char string (<=15 length) of units for sum value
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowRecurringWarningErrorAtEnd(Message,Index,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                            ReportMaxUnits,ReportMinUnits,ReportSumUnits)
    USE DataPrecisionGlobals
    !  Use for recurring "warning" error messages shown once at end of simulation
    !  with count of occurences and optional max, min, sum
    CHARACTER(len=*) :: Message     ! Message automatically written to "error file" at end of simulation
    INTEGER, INTENT(INOUT)        :: Index       ! Recurring message index, if zero, next available index is assigned
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportMaxOf ! Track and report the max of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportMinOf ! Track and report the min of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportSumOf ! Track and report the sum of the values passed to this argument
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMaxUnits ! optional char string (<=15 length) of units for max value
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMinUnits ! optional char string (<=15 length) of units for min value
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportSumUnits ! optional char string (<=15 length) of units for sum value
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowRecurringContinueErrorAtEnd(Message,Index,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                             ReportMaxUnits,ReportMinUnits,ReportSumUnits)
    USE DataPrecisionGlobals
    !  Use when "continuing" a recurring error messages (shown once at end of simulation)
    !  over several lines with optional max, min, sum
    CHARACTER(len=*) :: Message     ! Message automatically written to "error file" at end of simulation
    INTEGER, INTENT(INOUT)        :: Index       ! Recurring message index, if zero, next available index is assigned
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportMaxOf ! Track and report the max of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportMinOf ! Track and report the min of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ReportSumOf ! Track and report the sum of the values passed to this argument
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMaxUnits ! optional char string (<=15 length) of units for max value
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMinUnits ! optional char string (<=15 length) of units for min value
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportSumUnits ! optional char string (<=15 length) of units for sum value
    END SUBROUTINE
  END INTERFACE

  INTERFACE SetupOutputVariable
    SUBROUTINE SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
               ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult,IndexGroupKey)
    USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL(r64), INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseSubKey    ! Meter End Use Sub Key (General Lights, Task Lights, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ZoneKey         ! Meter Zone Key (zone name)
      INTEGER, INTENT(IN), OPTIONAL :: ZoneMult ! Zone Multiplier, defaults to 1
      INTEGER, INTENT(IN), OPTIONAL :: ZoneListMult ! Zone List Multiplier, defaults to 1
      INTEGER, INTENT(IN), OPTIONAL :: IndexGroupKey ! Group identifier for SQL output
    END SUBROUTINE
    SUBROUTINE SetupIntegerOutputVariable(VariableName,IntActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue, &
                                           ReportFreq,IndexGroupKey)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      INTEGER, INTENT(IN), TARGET  :: IntActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq     ! Internal use -- causes reporting at this freqency
      INTEGER, INTENT(IN), OPTIONAL :: IndexGroupKey ! Group identifier for SQL output
    END SUBROUTINE
    SUBROUTINE SetupRealOutputVariable_IntKey(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
               ReportFreq,ResourceTypeKey,EndUseKey,EndUseSubKey,GroupKey,ZoneKey,ZoneMult,ZoneListMult,IndexGroupKey)
    USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL(r64), INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      INTEGER, INTENT(IN)          :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseSubKey    ! Meter End Use Sub Key (General Lights, Task Lights, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ZoneKey         ! Meter Zone Key (zone name)
      INTEGER, INTENT(IN), OPTIONAL :: ZoneMult ! Zone Multiplier, defaults to 1
      INTEGER, INTENT(IN), OPTIONAL :: ZoneListMult ! Zone List Multiplier, defaults to 1
      INTEGER, INTENT(IN), OPTIONAL :: IndexGroupKey ! Group identifier for SQL output
    END SUBROUTINE
  END INTERFACE

  INTERFACE GetInternalVariableValue
    FUNCTION GetInternalVariableValue(varType, keyVarIndex) RESULT (resultVal)
      USE DataPrecisionGlobals
      INTEGER, INTENT(IN)      :: keyVarIndex  ! Array index (from call to GetVariableKeys
      INTEGER, INTENT(IN)      :: varType      ! 1=integer, 2=REAL(r64), 3=meter, from call to GetVariableKeyCountandType
      REAL(r64)                :: resultVal    ! value returned
    END FUNCTION
  END INTERFACE

  INTERFACE GetInternalVariableValueExternalInterface !CR - 8481 fix - 08/19/2011
    FUNCTION GetInternalVariableValueExternalInterface(varType, keyVarIndex) RESULT (resultVal)
      USE DataPrecisionGlobals
      INTEGER, INTENT(IN)      :: keyVarIndex  ! Array index (from call to GetVariableKeys
      INTEGER, INTENT(IN)      :: varType      ! 1=integer, 2=REAL(r64), 3=meter, from call to GetVariableKeyCountandType
      REAL(r64)                :: resultVal    ! value returned
    END FUNCTION
  END INTERFACE

  INTERFACE SetupEMSActuator
    SUBROUTINE SetupEMSRealActuator (cComponentTypeName, cUniqueIDName, cControlTypeName, cUnits, lEMSActuated, rValue )
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)  :: cComponentTypeName ! general actuator name registered
      CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName      ! unique id for actuator
      CHARACTER(len=*), INTENT(IN)  :: cControlTypeName   ! control type id for actuator
      CHARACTER(len=*), INTENT(IN)  :: cUnits             ! control value units
      LOGICAL,  TARGET, INTENT(IN)  :: lEMSActuated       ! pointer target for remote actuation control logical
      REAL(r64),TARGET, INTENT(IN)  :: rValue             ! pointer target for remote real value to be used
    END SUBROUTINE SetupEMSRealActuator
    SUBROUTINE SetupEMSIntegerActuator (cComponentTypeName, cUniqueIDName, cControlTypeName, cUnits, lEMSActuated, iValue )
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)  :: cComponentTypeName ! general actuator name registered
      CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName      ! unique id for actuator
      CHARACTER(len=*), INTENT(IN)  :: cControlTypeName   ! control type id for actuator
      CHARACTER(len=*), INTENT(IN)  :: cUnits             ! control value units
      LOGICAL, TARGET,  INTENT(IN)  :: lEMSActuated       ! pointer target for remote actuation control logical
      INTEGER, TARGET,  INTENT(IN)  :: iValue             ! pointer target for remote integer value to be used
    END SUBROUTINE SetupEMSIntegerActuator
    SUBROUTINE SetupEMSLogicalActuator (cComponentTypeName, cUniqueIDName, cControlTypeName, cUnits, lEMSActuated, lValue )
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)  :: cComponentTypeName ! general actuator name registered
      CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName      ! unique id for actuator
      CHARACTER(len=*), INTENT(IN)  :: cControlTypeName   ! control type id for actuator
      CHARACTER(len=*), INTENT(IN)  :: cUnits             ! control value units
      LOGICAL, TARGET,  INTENT(IN)  :: lEMSActuated       ! pointer target for remote actuation control logical
      LOGICAL, TARGET,  INTENT(IN)  :: lValue             ! pointer target for remote logical value to be used
    END SUBROUTINE SetupEMSLogicalActuator
  END INTERFACE SetupEMSActuator

  INTERFACE SetupEMSInternalVariable
    SUBROUTINE SetupEMSRealInternalVariable(cDataTypeName, cUniqueIDName, cUnits,  rValue )
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)  :: cDataTypeName      ! general internal variable name
      CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName      ! unique id for internal var
      CHARACTER(len=*), INTENT(IN)  :: cUnits             ! registered units
      REAL(r64), TARGET, INTENT(IN) :: rValue             ! pointer target for remote real value to be accessed
    END SUBROUTINE SetupEMSRealInternalVariable
    SUBROUTINE SetupEMSIntegerInternalVariable(cDataTypeName, cUniqueIDName, cUnits,  iValue )
      USE DataPrecisionGlobals
      CHARACTER(len=*), INTENT(IN)  :: cDataTypeName      ! general internal variable name
      CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName      ! unique id for internal var
      CHARACTER(len=*), INTENT(IN)  :: cUnits             ! registered units
      INTEGER, TARGET, INTENT(IN)   :: iValue             ! pointer target for remote integer value to be accessed
    END SUBROUTINE SetupEMSIntegerInternalVariable
  END INTERFACE SetupEMSInternalVariable

  INTERFACE CalcHeatBalanceOutsideSurf
    SUBROUTINE CalcHeatBalanceOutsideSurf(ZoneToResimulate)
      INTEGER, INTENT(IN), OPTIONAL :: ZoneToResimulate
    END SUBROUTINE CalcHeatBalanceOutsideSurf
  END INTERFACE

  INTERFACE CalcHeatBalanceInsideSurf
    SUBROUTINE CalcHeatBalanceInsideSurf(ZoneToResimulate)
      INTEGER, INTENT(IN), OPTIONAL :: ZoneToResimulate
    END SUBROUTINE CalcHeatBalanceInsideSurf
  END INTERFACE

INTERFACE SetupZoneInternalGain
  SUBROUTINE SetupZoneInternalGain(ZoneNum, cComponentObject , cComponentName, &
                                   IntGainComp_TypeOfNum, ConvectionGainRate , &
                                   ReturnAirConvectionGainRate, ThermalRadiationGainRate, &
                                   LatentGainRate, ReturnAirLatentGainRate, &
                                   CarbonDioxideGainRate, GenericContamGainRate)
    USE DataPrecisionGlobals
    INTEGER,           INTENT(IN) :: ZoneNum
    CHARACTER(len=*),  INTENT(IN) :: cComponentObject ! class name for device contributing internal gain
    CHARACTER(len=*),  INTENT(IN) :: cComponentName  ! user unique name for device
    INTEGER         ,  INTENT(IN) :: IntGainComp_TypeOfNum ! integer identify for device
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ConvectionGainRate          ! target convection gain value (W)
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ReturnAirConvectionGainRate ! target return air sensible gain (W)
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ThermalRadiationGainRate  ! target IR radiation gain value (W)
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: LatentGainRate           ! target latent (energy) gain value (W)
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ReturnAirLatentGainRate ! target return air latent gain (W)
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: CarbonDioxideGainRate ! target CO2 gain value (m3/s)
    REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: GenericContamGainRate ! target generic air contaminant value (m3/s)
  END SUBROUTINE
END INTERFACE

INTERFACE SetATMixerPriFlow
  SUBROUTINE SetATMixerPriFlow(ATMixerNum,PriAirMassFlowRate)
    USE DataPrecisionGlobals
    INTEGER, INTENT(IN)              :: ATMixerNum            ! Air terminal mixer index
    REAL(r64), INTENT (IN), OPTIONAL :: PriAirMassFlowRate    ! Air terminal mixer primary air mass flow rate [kg/s]
  END SUBROUTINE SetATMixerPriFlow
END INTERFACE

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

END MODULE DataInterfaces

