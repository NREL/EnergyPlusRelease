MODULE HVACInterfaceManager

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contains one or more routines for checking the convergence
          ! of the various HVAC loops and passing information across interface
          ! boundaries.

          ! METHODOLOGY EMPLOYED:
          ! The upper level HVAC managers call the routine(s) contained in this
          ! module as a last step.  The node information is passed across the
          ! interface boundary and the logical flag is set if the nodes across
          ! from each other are not within tolerance.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE    ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
    !Common Pipe Recirc Flow Directions
  INTEGER, PARAMETER               :: NoRecircFlow      = 0
  INTEGER, PARAMETER               :: PrimaryRecirc     = 1       ! flow from Supply-outlet/Demand-inlet to Supply-inlet/demand-outlet
  INTEGER, PARAMETER               :: SecondaryRecirc   = 2        ! flow from Supply-inlet/Demand-oulet to Supply-outlet/demand-inlet

  INTEGER, PARAMETER               :: FlowTypeNotSet = 9
  INTEGER, PARAMETER               :: ConstantFlow = 10
  INTEGER, PARAMETER               :: VariableFlow = 11


          ! DERIVED TYPE DEFINITIONS:
TYPE, PUBLIC :: CommonPipeData
  INTEGER                          :: CommonPipeType        = 0       ! type of common pipe used if any
  INTEGER                          :: SupplySideInletPumpType = FlowTypeNotSet
  INTEGER                          :: DemandSideInletPumpType = FlowTypeNotSet
!Following report variables are used in uncontrolled common pipe
  INTEGER                          :: FlowDir               = 0       ! Direction in which flow is in Common Pipe
  REAL(r64)                        :: Flow                  = 0.0d0   ! Flow in the Common Pipe
  REAL(r64)                        :: Temp                  = 0.0d0
!Following report variables are used in two way common pipe
  REAL(r64)                        :: SecCPLegFlow          = 0.0d0   ! Mass flow in the secondary side Common pipe leg
  REAL(r64)                        :: PriCPLegFlow          = 0.0d0   ! Mass flow in the primary side Common pipe leg
  REAL(r64)                        :: SecToPriFlow          = 0.0d0   ! Mass flow in the pipe from Secondary to primary side
  REAL(r64)                        :: PriToSecFlow          = 0.0d0   ! Mass flow in the pipe from primary to Secondary side
  REAL(r64)                        :: PriInTemp             = 0.0d0   ! Temperature at primary inlet node
  REAL(r64)                        :: PriOutTemp            = 0.0d0   ! Temperature at primary outlet node
  REAL(r64)                        :: SecInTemp             = 0.0d0   ! Temperature at secondary inlet node
  REAL(r64)                        :: SecOutTemp            = 0.0d0   ! Temperature at secondary outlet node
  REAL(r64)                        :: PriInletSetPoint      = 0.0d0   ! Set point at Primary inlet node
  REAL(r64)                        :: SecInletSetPoint      = 0.0d0   ! Set point at Secondary inlet node
  LOGICAL                          :: PriInletControlled    = .FALSE. !True if Primary inlet node is controlled
  LOGICAL                          :: SecInletControlled    = .FALSE. !True if secondary inlet is controlled
  REAL(r64)                        :: PriFlowRequest        = 0.d0    ! total flow request on supply side.
END TYPE CommonPipeData


TYPE (CommonPipeData),  PUBLIC,  ALLOCATABLE, DIMENSION(:) :: PlantCommonPipe

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  LOGICAL :: CommonPipeSetupFinished = .FALSE.

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ConductionTransferFunctionCalc

PUBLIC  UpdateHVACInterface         ! air side update
PUBLIC  UpdatePlantLoopInterface
PRIVATE UpdateHalfLoopInletTemp
PRIVATE UpdateCommonPipe
PRIVATE ManageTwoWayCommonPipe
PRIVATE ManageSingleCommonPipe
PRIVATE SetupCommonPipes


CONTAINS

          ! MODULE SUBROUTINES:
SUBROUTINE UpdateHVACInterface(OutletNode,InletNode,OutOfToleranceFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages any generic HVAC loop interface.

          ! METHODOLOGY EMPLOYED:
          ! This is a simple "forward" interface where all of the properties
          ! from the outlet of one side of the loop get transfered directly
          ! to the inlet node of the corresponding other side of the loop.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY : Node
  USE DataConvergParams
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: OutletNode          ! Node number for the outlet of the side of the loop just simulated
  INTEGER, INTENT(IN)    :: InletNode           ! Node number for the inlet of the side that needs the outlet node data
  LOGICAL, INTENT(INOUT) :: OutOfToleranceFlag  ! True when the other side of the loop need to be (re)simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)       :: DeltaEnergy

          ! FLOW:

  !Calculate the approximate energy difference across interface for comparison
  DeltaEnergy = HVACCpApprox*((Node(OutletNode)%MassFlowRate*Node(OutletNode)%Temp) -  &
                          (Node(InletNode)%MassFlowRate*Node(InletNode)%Temp))
  HVACEnthTolFlag = 0
  HVACFlowTolFlag = 0
  HVACHumTolFlag = 0
  HVACPressTolFlag = 0
  HVACQualTolFlag = 0
  HVACEnergyTolFlag = 0
  HVACTempTolFlag = 0

  ! HVACQuePtr points to latest "diff"
  HVACQuePtr=HVACQuePtr+1
  IF (HVACQuePtr > 5) HVACQuePtr=1

  HVACOutletNodeTolOut(HVACQuePtr) = OutletNode

  HVACEnthTolValue(HVACQuePtr)  = ABS(Node(OutletNode)%Enthalpy-Node(InletNode)%Enthalpy)
  IF (HVACEnthTolValue(HVACQuePtr)  > HVACEnthalpyToler) THEN
    HVACEnthTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF
  HVACFlowTolValue(HVACQuePtr)  = ABS(Node(OutletNode)%MassFlowRate-Node(InletNode)%MassFlowRate)
  IF (HVACFlowTolValue(HVACQuePtr)  > HVACFlowRateToler) THEN
    HVACFlowTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF
  HVACHumTolValue(HVACQuePtr)   = ABS(Node(OutletNode)%HumRat-Node(InletNode)%HumRat)
  IF (HVACHumTolValue(HVACQuePtr)   > HVACHumRatToler) THEN
    HVACHumTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF
  HVACPressTolValue(HVACQuePtr) = ABS(Node(OutletNode)%Press-Node(InletNode)%Press)
  IF (HVACPressTolValue(HVACQuePtr) > HVACPressToler) THEN
    HVACPressTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF
  HVACQualTolValue(HVACQuePtr)  = ABS(Node(OutletNode)%Quality-Node(InletNode)%Quality)
  IF (HVACQualTolValue(HVACQuePtr)  > HVACQualityToler) THEN
    HVACQualTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF
  HVACEnergyTolValue(HVACQuePtr)= DeltaEnergy
  IF (ABS(DeltaEnergy)          > HVACEnergyToler) THEN
    HVACEnergyTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF
  HVACTempTolValue(HVACQuePtr)  = ABS(Node(OutletNode)%Temp-Node(InletNode)%Temp)
  IF (HVACTempTolValue(HVACQuePtr)  > HVACTemperatureToler) THEN
    HVACTempTolFlag = 1
    OutOfToleranceFlag = .TRUE. ! Something has changed--resimulate the other side of the loop
  END IF

          ! Always update the new inlet conditions
  Node(InletNode)%Temp                 = Node(OutletNode)%Temp
  Node(InletNode)%MassFlowRate         = Node(OutletNode)%MassFlowRate
  Node(InletNode)%MassFlowRateMinAvail = Node(OutletNode)%MassFlowRateMinAvail
  Node(InletNode)%MassFlowRateMaxAvail = Node(OutletNode)%MassFlowRateMaxAvail
  Node(InletNode)%Quality              = Node(OutletNode)%Quality
  Node(InletNode)%Press                = Node(OutletNode)%Press
  Node(InletNode)%Enthalpy             = Node(OutletNode)%Enthalpy
  Node(InletNode)%HumRat               = Node(OutletNode)%HumRat

  IF (Contaminant%CO2Simulation) Then
    Node(InletNode)%CO2 = Node(OutletNode)%CO2
  End If

  RETURN

END SUBROUTINE UpdateHVACInterface


!***************
SUBROUTINE UpdatePlantLoopInterface(LoopNum,ThisLoopSideNum,ThisLoopSideOutletNode,OtherLoopSideInletNode,  &
   OutOfToleranceFlag,FirstHVACIteration,CommonPipeType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  Brent Griffith, Sept. 2010
          !       RE-ENGINEERED  Dan Fisher,     Sept. 2010

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages any generic HVAC loop interface.

          ! METHODOLOGY EMPLOYED:
          ! This is a simple "forward" interface where all of the properties
          ! from the outlet of one side of the loop get transfered
          ! to the inlet node of the corresponding other side of the loop.
          ! Temperatures are 'lagged' by loop capacitance (i.e. a 'tank')
          ! between the outlet and inlet nodes.
          ! the update from the demand side to the supply side always triggers
          ! resimulation of the supply side if any state variable (or energy) is
          ! out of tolerance.  Remsimulation of the demand side is only triggered if
          ! flow or energy are out of tolerance.  This in effect checks flow and
          ! ~.25C temperature difference.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY : Node
  USE DataConvergParams
  USE DataPlant,    ONLY : PlantLoop, SupplySide,DemandSide
  USE FluidProperties,      ONLY : GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: OtherLoopSideInletNode          ! Node number for the outlet of the side of the loop just simulated
  INTEGER, INTENT(IN)    :: ThisLoopSideOutletNode           ! Node number for the inlet of the side that needs the outlet node data
  LOGICAL, INTENT(INOUT) :: OutOfToleranceFlag  ! True when the other side of the loop need to be (re)simulated
  INTEGER, INTENT(IN)    :: LoopNum             ! The 'inlet/outlet node' loop number
  INTEGER, INTENT(IN)    :: ThisLoopSideNum   ! The 'outlet node' loopside number
  INTEGER, OPTIONAL, INTENT(IN) :: CommonPipeType
  LOGICAL, INTENT(IN)   :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)       :: DeltaEnergy
  REAL(r64)       :: OldTankOutletTemp
  REAL(r64)       :: OldTankOutletEnthalpy
  REAL(r64)       :: TankOutletTemp
  REAL(r64)       :: Cp
  REAL(r64)       :: MixedOutletTemp
  INTEGER         :: ThisLoopSideInletNode

          ! FLOW:

    !reset out of tolerance flags
  PlantEnthTolFlag = 0
  PlantFlowTolFlag = 0
  PlantPressTolFlag = 0
  PlantQualTolFlag = 0
  PlantEnergyTolFlag = 0
  PlantTempTolFlag = 0

    ! updatePlantQuePtr points to latest "diff"
  PlantQuePtr=PlantQuePtr+1
  IF (PlantQuePtr > 5) PlantQuePtr=1

    !humidity ratio not applicable to hydronic loops
  PlantHumTolValue(PlantQuePtr) = 0

    !quality is also not applicable because it is arbitrarily set (to 1 or 0) and is not calculated by property routines
  PlantQualTolValue(PlantQuePtr)  = 0

    ! set the node for reporting
  PlantOutletNodeTolOut(PlantQuePtr) = ThisLoopSideOutletNode

    !set the loopside inlet node
  ThisLoopSideInletNode  = PlantLoop(LoopNum)%LoopSide(ThisLoopSideNum)%NodeNumIn

    !save the inlet node temp for DeltaEnergy check
  OldTankOutletTemp     = Node(OtherLoopSideInletNode)%Temp
  OldTankOutletEnthalpy = Node(OtherLoopSideInletNode)%enthalpy

    !calculate the specific heat
  Cp = GetSpecificHeatGlycol(PlantLoop(loopNum)%FluidName,OldTankOutletTemp, &
                             PlantLoop(loopNum)%FluidIndex,'UpdatePlantLoopInterface')

    !update the enthalpy
  Node(OtherLoopSideInletNode)%Enthalpy = Cp * Node(OtherLoopSideInletNode)%Temp

    !Calculate the approximate energy difference across interface for comparison
    !=cp((newflowrate*newtemp)-(oldflowrate*oldtemp))
  DeltaEnergy = PlantCpApprox*((Node(ThisLoopSideOutletNode)%MassFlowRate*Node(OtherLoopSideInletNode)%Temp) -  &
                          (Node(ThisLoopSideInletNode)%MassFlowRate*OldTankOutletTemp))

    !update the temperatures and flow rates
  IF(CommonPipeType == 1 .OR. CommonPipeType ==2)THEN
        !update the temperature
    CALL UpdateCommonPipe(LoopNum, ThisLoopSideNum, CommonPipeType,FirstHVACIteration, MixedOutletTemp)
    Node(OtherLoopSideInletNode)%Temp = MixedOutletTemp
    TankOutletTemp = MixedOutletTemp
        !Set the flow tolerance array.  For a common pipe the half loop always sets it's own flow, so this check is not needed.
    PlantFlowTolValue(PlantQuePtr)  = 0
        !Set the flow rate.  Continuity requires that the flow rates at the half loop inlet and outlet match
    Node(ThisLoopSideInletNode)%MassFlowRate = Node(ThisLoopSideOutletNode)%MassFlowRate
        !Update this loopside inlet node Min/MaxAvail to this loopside outlet node Min/MaxAvail
    Node(ThisLoopSideInletNode)%MassFlowRateMinAvail = Node(ThisLoopSideOutletNode)%MassFlowRateMinAvail
    Node(ThisLoopSideInletNode)%MassFlowRateMaxAvail = Node(ThisLoopSideOutletNode)%MassFlowRateMaxAvail

  ELSE !no common pipe
    CALL UpdateHalfLoopInletTemp(LoopNum, ThisLoopSideNum,TankOutletTemp)
        !update the temperature
    Node(OtherLoopSideInletNode)%Temp = TankOutletTemp
        !Set the flow tolerance array
    PlantFlowTolValue(PlantQuePtr)  = ABS(Node(ThisLoopSideOutletNode)%MassFlowRate-Node(OtherLoopSideInletNode)%MassFlowRate)
        !Set the flow rate
    Node(OtherLoopSideInletNode)%MassFlowRate         = Node(ThisLoopSideOutletNode)%MassFlowRate
        !update the MIN/MAX available flow rates
    Node(OtherLoopSideInletNode)%MassFlowRateMinAvail = Node(ThisLoopSideOutletNode)%MassFlowRateMinAvail
    Node(OtherLoopSideInletNode)%MassFlowRateMaxAvail = Node(ThisLoopSideOutletNode)%MassFlowRateMaxAvail
        !update Quality.  DSU? Note: This update assumes that STEAM cannot be used with common pipes.
    Node(OtherLoopSideInletNode)%Quality = Node(ThisLoopSideOutletNode)%Quality
        !pressure update  DSU? Note: This update assumes that PRESSURE SIMULATION cannot be used with common pipes.
    IF (PlantLoop(LoopNum)%HasPressureComponents) THEN
        !Don't update pressure, let the pressure simulation handle pressures
      PlantPressTolValue(PlantQuePtr) = 0
    ELSE
        !Do update pressure!
      PlantPressTolValue(PlantQuePtr) = ABS(Node(ThisLoopSideOutletNode)%Press-Node(OtherLoopSideInletNode)%Press)
      Node(OtherLoopSideInletNode)%Press = Node(ThisLoopSideOutletNode)%Press
    END IF
  ENDIF

    !Set out of tolerance flags
    !enthalpy
  PlantEnthTolValue(PlantQuePtr)  = ABS(OldTankOutletEnthalpy-Node(OtherLoopSideInletNode)%Enthalpy)
  IF (PlantEnthTolValue(PlantQuePtr)  > PlantEnthalpyToler) PlantEnthTolFlag = 1

    !flow rate
  IF (PlantFlowTolValue(PlantQuePtr)  > PlantFlowRateToler) PlantFlowTolFlag = 1

    !loop pressure
  IF (PlantPressTolValue(PlantQuePtr) > PlantPressToler) PlantPressTolFlag = 1

    !quality
  IF (PlantQualTolValue(PlantQuePtr)  > PlantQualityToler) PlantQualTolFlag = 1

    !energy
  PlantEnergyTolValue(PlantQuePtr)= DeltaEnergy
  IF (ABS(DeltaEnergy)  > PlantEnergyToler) PlantEnergyTolFlag = 1

    !temperature
  PlantTempTolValue(PlantQuePtr)  = ABS(OldTankOutletTemp-Node(OtherLoopSideInletNode)%Temp)
  IF (PlantTempTolValue(PlantQuePtr)  > PlantTemperatureToler) PlantTempTolFlag = 1

    !Evaluate tolerance flags
  IF(ThisLoopSideNum == DemandSide)THEN
    If (PlantEnthTolFlag    == 1 .OR. &
        PlantFlowTolFlag    == 1 .OR. &
        PlantPressTolFlag   == 1 .OR. &
        PlantQualTolFlag    == 1 .OR. &
        PlantEnergyTolFlag  == 1 .OR. &
        PlantTempTolFlag    == 1)OutOfToleranceFlag = .TRUE.

  ELSE !SupplySide
    If (PlantFlowTolFlag    == 1)OutOfToleranceFlag = .TRUE.
!    If (PlantFlowTolFlag    == 1 .OR. &
!        PlantEnergyTolFlag  == 1)OutOfToleranceFlag = .TRUE.
  ENDIF

  RETURN

END SUBROUTINE UpdatePlantLoopInterface
!***************


SUBROUTINE UpdateHalfLoopInletTemp(LoopNum, TankInletLoopSide, TankOutletTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   September 2001
          !       MODIFIED       Simon Rees, July 2007
          !                      Brent Griffith, Feb. 2010, add LoopNum arg
          !       RE-ENGINEERED  Brent Griffith, Sept 2010, generalize for both loop sides
          !                                           add pump heat from other loop
          !                      B.Griffith and L.Gu, Oct 2011, solve via analytical soln, use average over timestep

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the new loop side inlet temperature
          ! based on the previous temperature of the mixed tank, mass flow rate and the new
          ! outlet temperature on the supply side.  The temperature does not
          ! pass directly across because the loop has some capacitance. It is
          ! called separately but used for both supply-to-demand, and demand-to-supply

          ! METHODOLOGY EMPLOYED:
          ! This uses a analytical solution for changes in the
          ! fluid loop temperature.  The user defines some volume of fluid
          ! for the loop which gets converted to a fixed amount of mass.
          ! The loop side inlet node is modeled as the outlet of a fully mixed
          ! tank. Note that this routine is called repeatedly to re calculate
          ! loop capacitance based on current plant conditions


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,      ONLY : TimeStepSys, SysTimeElapsed
  USE DataLoopNode,         ONLY : Node
  USE DataPlant,            ONLY : PlantLoop
  USE DataGlobals,          ONLY : SecInHour, TimeStepZone, TimeStep, HourOfDay
  USE FluidProperties,      ONLY : GetSpecificHeatGlycol
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  INTEGER, INTENT(IN)  :: LoopNum
  INTEGER, INTENT(IN)  :: TankInletLoopSide
  REAL(r64),INTENT(OUT) :: TankOutletTemp


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER     ::     FracTotLoopMass = 0.5d0   !Fraction of total loop mass assigned to the half loop

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: TankOutletLoopSide                ! inlet loopsidenumber
  INTEGER       :: TankInletNode          ! inlet loop side outlet node
  INTEGER       :: TankOutletNode          ! inlet loop side outlet node
  REAL(r64)     :: TankInletTemp   ! temporary variable
  REAL(r64)     :: LastTankOutletTemp   ! temporary variable
  REAL(r64)     :: Cp                           ! specific heat
  REAL(r64)     :: TimeElapsed ! temporary value based on current clock time during simulation, fractional hours

  REAL(r64)     :: TimeStepSeconds
  REAL(r64)     :: MassFlowRate
  REAL(r64)     :: PumpHeat
  REAL(r64)     :: ThisTankMass
  REAL(r64)     :: TankFinalTemp
  REAL(r64)     :: TankAverageTemp

          ! FLOW:

    !find tank inlet and outlet nodes
  TankOutletLoopSide = 3 - TankInletLoopSide
  TankInletNode  = PlantLoop(LoopNum)%LoopSide(TankInletLoopSide)%NodeNumOut
  TankOutletNode  = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%NodeNumIn

  TankInletTemp = Node(TankInletNode)%Temp


! This needs to be based on time to deal with system downstepping and repeated timesteps
!  IF (FirstHVACIteration .AND. PlantLoop(LoopNum)%LoopSide(TankInletLoopSide)%DoTankUpdate)THEN
  TimeElapsed = (HourOfDay-1) + TimeStep * TimeStepZone + SysTimeElapsed
  IF (PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TimeElapsed /= TimeElapsed) THEN
    PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%LastTempInterfaceTankOutlet &
       = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TempInterfaceTankOutlet
    PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TimeElapsed  = TimeElapsed
  ENDIF

  LastTankOutletTemp = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%LastTempInterfaceTankOutlet

    !calculate the specific heat for the capacitance calculation
  Cp = GetSpecificHeatGlycol(PlantLoop(loopNum)%FluidName,  &
                             LastTankOutletTemp, &
                             PlantLoop(loopNum)%FluidIndex,'UpdateHalfLoopInletTemp')
    !set the fraction of loop mass assigned to each half loop outlet capacitance ('tank') calculation

    !calculate new loop inlet temperature.  The calculation is a simple 'tank' (thermal capacitance) calculation that includes:
        !--half of loop mass.  The other half is accounted for at the other half loop interface
        !--pump heat.  Pump heat for a single loop setpoint with pumps only on the supply side is added at the supply side inlet.
        !   Pump heat for a dual setpoint loop is added to each loop side inlet
        !  The previous tank temperature value is used to prevent accumulation of pump heat during iterations while recalculating 
        ! tank conditions each call.
        ! Analytical solution for ODE, formulated for both final tank temp and average tank temp.
        

  TimeStepSeconds = TimeStepSys * SecInHour
  MassFlowRate = Node(TankInletNode)%MassFlowRate
  PumpHeat = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TotalPumpHeat
  ThisTankMass = FracTotLoopMass * PlantLoop(LoopNum)%Mass

  IF (ThisTankMass <= 0.d0) THEN ! no mass, no plant loop volume
    IF (MassFlowRate > 0.d0) THEN
      TankFinalTemp = TankInletTemp + PumpHeat/(MassFlowRate * Cp)
      TankAverageTemp = (TankFinalTemp + LastTankOutletTemp)/2.0
    ELSE
      TankFinalTemp = LastTankOutletTemp
      TankAverageTemp = LastTankOutletTemp
    END IF

  ELSE ! tank has mass
    IF (MassFlowRate > 0.d0 ) THEN
      TankFinalTemp = (LastTankOutletTemp - (MassFlowRate * Cp * TankInletTemp + PumpHeat)/(MassFlowRate * Cp)) * &
                       exp(-(MassFlowRate * Cp) / (ThisTankMass*Cp)*TimeStepSeconds) + &
                       (MassFlowRate * Cp * TankInletTemp + PumpHeat)/ (MassFlowRate * Cp)
      TankAverageTemp = ((ThisTankMass*Cp)/(MassFlowRate * Cp)*(LastTankOutletTemp - &
                        (MassFlowRate * Cp * TankInletTemp + PumpHeat)/(MassFlowRate * Cp)) * &
                        (1.0-exp(-(MassFlowRate * Cp) / (ThisTankMass*Cp)*TimeStepSeconds))/TimeStepSeconds + &
                        (MassFlowRate * Cp * TankInletTemp + PumpHeat)/(MassFlowRate * Cp))
    ELSE

      TankFinalTemp = PumpHeat/(ThisTankMass*Cp)*TimeStepSeconds + LastTankOutletTemp
      TankAverageTemp = (TankFinalTemp + LastTankOutletTemp)/2.0

    END IF
  ENDIF

      !update last tank outlet temperature
  PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TempInterfaceTankOutlet = TankFinalTemp

    ! update report variable
  PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%LoopSideInlet_TankTemp = TankAverageTemp
  
  TankOutletTemp = TankAverageTemp

  RETURN

END SUBROUTINE UpdateHalfLoopInletTemp

SUBROUTINE UpdateCommonPipe(LoopNum,TankInletLoopSide,CommonPipeType,FirstHVACIteration, MixedOutletTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   September 2001
          !       MODIFIED       Simon Rees, July 2007
          !                      Brent Griffith, Feb. 2010, add LoopNum arg
          !       RE-ENGINEERED  Brent Griffith, Sept 2010, generalize for both loop sides
          !                                           add pump heat from other loop
          !                      B.Griffith and L.Gu, Oct 2011, solve via analytical soln, use average over timestep

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the new loop side inlet temperature
          ! based on the previous temperature of the mixed tank, mass flow rate and the new
          ! outlet temperature on the supply side.  The temperature does not
          ! pass directly across because the loop has some capacitance. It is
          ! called separately but used for both supply-to-demand, and demand-to-supply

          ! METHODOLOGY EMPLOYED:
          ! This uses a analytical solution for changes in the
          ! fluid loop temperature.  The user defines some volume of fluid
          ! for the loop which gets converted to a fixed amount of mass.
          ! The loop side inlet node is modeled as the outlet of a fully mixed
          ! tank. Note that this routine is called repeatedly to re calculate
          ! loop capacitance based on current plant conditions



          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,      ONLY : TimeStepSys, SysTimeElapsed
  USE DataLoopNode,         ONLY : Node
  USE DataPlant,            ONLY : PlantLoop,CommonPipe_Single,CommonPipe_TwoWay, DemandSide
  USE DataGlobals,          ONLY : SecInHour, TimeStepZone, TimeStep, HourOfDay
  USE FluidProperties,      ONLY : GetSpecificHeatGlycol
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  INTEGER, INTENT(IN)   :: LoopNum
  INTEGER, INTENT(IN)   :: CommonPipeType
  INTEGER, INTENT(IN)   :: TankInletLoopSide
  REAL(r64),INTENT(OUT) :: MixedOutletTemp
  LOGICAL, INTENT(IN)   :: FirstHVACIteration


          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: TankOutletLoopSide                ! inlet loopsidenumber
  INTEGER       :: TankInletNode          ! inlet loop side outlet node
  INTEGER       :: TankOutletNode          ! inlet loop side outlet node
  REAL(r64)     :: TankInletTemp   ! temporary variable
  REAL(r64)     :: LastTankOutletTemp   ! temporary variable
  REAL(r64)     :: Cp                           ! specific heat
  REAL(r64)     :: TimeElapsed ! temporary value based on current clock time during simulation, fractional hours

  REAL(r64)     :: FracTotLoopMass       !Fraction of total loop mass assigned to the half loop
  REAL(r64)     :: TimeStepSeconds
  REAL(r64)     :: MassFlowRate
  REAL(r64)     :: PumpHeat
  REAL(r64)     :: ThisTankMass
  REAL(r64)     :: TankFinalTemp
  REAL(r64)     :: TankAverageTemp

          ! FLOW:

    !find tank inlet and outlet nodes
  TankOutletLoopSide = 3 - TankInletLoopSide
  TankInletNode  = PlantLoop(LoopNum)%LoopSide(TankInletLoopSide)%NodeNumOut
  TankOutletNode  = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%NodeNumIn

  TankInletTemp = Node(TankInletNode)%Temp

  IF (TankInletLoopSide == DemandSide) THEN
    ! for common pipe loops, assume 75% of plant loop volume is on the demand side
    FracTotLoopMass = 0.25
  ELSE
    FracTotLoopMass = 0.75
  ENDIF

! This needs to be based on time to deal with system downstepping and repeated timesteps
!  IF (FirstHVACIteration .AND. PlantLoop(LoopNum)%LoopSide(TankInletLoopSide)%DoTankUpdate)THEN
  TimeElapsed = (HourOfDay-1) + TimeStep * TimeStepZone + SysTimeElapsed
  IF (PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TimeElapsed /= TimeElapsed) THEN
    PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%LastTempInterfaceTankOutlet &
       = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TempInterfaceTankOutlet
    PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TimeElapsed  = TimeElapsed
  ENDIF

  LastTankOutletTemp = PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%LastTempInterfaceTankOutlet

    !calculate the specific heat for the capacitance calculation
  Cp = GetSpecificHeatGlycol(PlantLoop(loopNum)%FluidName,  &
                             LastTankOutletTemp, &
                             PlantLoop(loopNum)%FluidIndex,'UpdateCommonPipe')
                             
    !set the fraction of loop mass assigned to each half loop outlet capacitance ('tank') calculation

    !calculate new loop inlet temperature.  The calculation is a simple 'tank' (thermal capacitance) calculation that includes:
        !--half of loop mass.  The other half is accounted for at the other half loop interface
        !--pump heat.  Pump heat for a single loop setpoint with pumps only on the supply side is added at the supply side inlet.
        !Pump heat for a dual setpoint loop is added to each loop side inlet
    !The previous inlet side temp,'ThisLoopSideTankOutletTemp' is used to prevent accumulation of pump heat during iterations.
    !The placement of the 'tank' for common pipes is *after* the outlet node and *before* the flow split or flow mixing.
    !This requires no logical check in the code since for purposes of temperature calculations, it is identical to the
    !no common pipe case.
    ! calculation is separated because for common pipe, a different split for mass fraction is applied 
    ! The pump heat source is swapped around here compared to no common pipe (so pump heat sort stays on its own side). 
  TimeStepSeconds = TimeStepSys * SecInHour
  MassFlowRate = Node(TankInletNode)%MassFlowRate
  PumpHeat = PlantLoop(LoopNum)%LoopSide(TankInletLoopSide)%TotalPumpHeat
  ThisTankMass = FracTotLoopMass * PlantLoop(LoopNum)%Mass

  IF (ThisTankMass <= 0.d0) THEN ! no mass, no plant loop volume
    IF (MassFlowRate > 0.d0) THEN
      TankFinalTemp = TankInletTemp + PumpHeat/(MassFlowRate * Cp)
      TankAverageTemp = (TankFinalTemp + LastTankOutletTemp)/2.0
    ELSE
      TankFinalTemp = LastTankOutletTemp
      TankAverageTemp = LastTankOutletTemp
    END IF

  ELSE ! tank has mass
    IF (MassFlowRate > 0.d0 ) THEN
      TankFinalTemp = (LastTankOutletTemp - (MassFlowRate * Cp * TankInletTemp + PumpHeat)/(MassFlowRate * Cp)) * &
                    exp(-(MassFlowRate * Cp) / (ThisTankMass*Cp)*TimeStepSeconds) + &
                    (MassFlowRate * Cp * TankInletTemp + PumpHeat)/ (MassFlowRate * Cp)
      TankAverageTemp = ((ThisTankMass*Cp)/(MassFlowRate * Cp)*(LastTankOutletTemp - &
                        (MassFlowRate * Cp * TankInletTemp + PumpHeat)/(MassFlowRate * Cp)) * &
                        (1.0-exp(-(MassFlowRate * Cp) / (ThisTankMass*Cp)*TimeStepSeconds))/TimeStepSeconds + &
                        (MassFlowRate * Cp * TankInletTemp + PumpHeat)/(MassFlowRate * Cp))
    ELSE

        TankFinalTemp = PumpHeat/(ThisTankMass*Cp)*TimeStepSeconds + LastTankOutletTemp
        TankAverageTemp = (TankFinalTemp + LastTankOutletTemp)/2.0

    END IF
  ENDIF
            !Common Pipe Simulation
  IF(CommonPipeType == CommonPipe_Single) THEN
    CALL ManageSingleCommonPipe(LoopNum,TankOutletLoopSide,TankAverageTemp, MixedOutletTemp)
        !2-way (controlled) common pipe simulation
  ELSE IF(CommonPipeType == CommonPipe_TwoWay) THEN
!    CALL CheckTwoWayCommonPipeConditions(LoopNum,TankInletLoopSide,TankOutletTemp, PriLoopTempIn, SecLoopTempIn,&
!                                   FirstHVACIteration)

    CALL ManageTwoWayCommonPipe(LoopNum, TankOutletLoopSide, TankAverageTemp, FirstHVACIteration)
    MixedOutletTemp = Node(TankOutletNode)%Temp
  END IF

  PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%TempInterfaceTankOutlet = TankFinalTemp

  PlantLoop(LoopNum)%LoopSide(TankOutletLoopSide)%LoopSideInlet_TankTemp  = TankAverageTemp
  RETURN

END SUBROUTINE UpdateCommonPipe


SUBROUTINE ManageSingleCommonPipe(LoopNum,LoopSide,TankOutletTemp, MixedOutletTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   November 2006
          !       MODIFIED       B. Griffith, Jan 2010 clean up setup to allow mixing common pipe modes
          !                      B. Griffith, Mar 2010 add LoopNum arg and simplify
          !       RE-ENGINEERED  D. Fisher, Sept. 2010
          !                      B. Griffitth, Oct 2011, major rewrite for plant upgrade

          ! PURPOSE OF THIS SUBROUTINE:
          ! To determine the conditions in common pipe viz., the flow flow temperature and direction of flow.

          ! METHODOLOGY EMPLOYED:
          ! Determine the flow on both sides of the common pipe. Decide if flow is coming into common pipe
          ! or going out of common pipe. After that determine which interface calls the subroutine, i.e. if
          ! called from "Demand to Supply" interface or "Supply to Demand" interface. Update the node temperatures
          ! accordingly.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY : BeginEnvrnFlag
  USE DataLoopNode,    ONLY: Node
  USE DataPlant

          !SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN)    :: LoopNum          !plant loop number
  INTEGER, INTENT(IN)    :: LoopSide          !plant loop side number
  REAL(r64), INTENT(IN)  :: TankOutletTemp      !inlet temperature to the common pipe passed in from the capacitance calculation
  REAL(r64), INTENT(OUT) :: MixedOutletTemp      !inlet temperature to the common pipe passed in from the capacitance calculation


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
  REAL(r64)  :: MdotPri      = 0.d0 ! flow rate on primary side kg/s
  REAL(r64)  :: MdotSec      = 0.d0 ! flow rate on secondary side kg/s
  REAL(r64)  :: MdotPriRCLeg = 0.d0 ! flow rate of primary recirculation thru common pipe kg/s
  REAL(r64)  :: MdotSecRCLeg = 0.d0 ! flow rate of secondary recirculation thru common pipe kg/s
  REAL(r64)  :: TempSecInlet = 0.d0 ! temperature at secondary inlet deg C
  REAL(r64)  :: TempPriInlet = 0.d0 ! temperature at primary inlet deg C
  REAL(r64)  :: TempPriOutTankOut = 0.d0
  REAL(r64)  :: TempSecOutTankOut = 0.d0
  INTEGER    :: NodeNumPriOut = 0
  INTEGER    :: NodeNumSecOut = 0
  INTEGER    :: NodeNumPriIn  = 0
  INTEGER    :: NodeNumSecIn  = 0
  INTEGER       :: CPFlowDir  !flow direction in single common pipe
  LOGICAL,SAVE,ALLOCATABLE, DIMENSION(:)   :: MyEnvrnFlag
  LOGICAL,SAVE  :: OneTimeData = .TRUE.
  REAL(r64)     :: CommonPipeTemp

    !One time call to set up report variables and set common pipe 'type' flag
  IF (OneTimeData) THEN
    IF ( .NOT. CommonPipeSetupFinished ) CALL SetupCommonPipes
    ALLOCATE(MyEnvrnFlag(TotNumLoops))
    MyEnvrnFlag = .TRUE.
    OneTimeData = .FALSE.
  END IF

  !fill local node indexes
  NodeNumPriIn  = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn
  NodeNumPriOut = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut
  NodeNumSecIn  = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn
  NodeNumSecOut = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumOut

  IF (MyEnvrnFlag(LoopNum) .and. BeginEnvrnFlag) THEN
    PlantCommonPipe(LoopNum)%Flow = 0.d0
    PlantCommonPipe(LoopNum)%Temp = 0.d0
    PlantCommonPipe(LoopNum)%FlowDir = NoRecircFlow
    MyEnvrnFlag(LoopNum)  = .FALSE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(LoopNum)=.TRUE.
  END IF
  
  ! every time inits
  MdotSec      = Node(NodeNumSecOut)%MassFlowRate
  MdotPri      = Node(NodeNumPriOut)%MassFlowRate
  
  IF (LoopSide == SupplySide) THEN
    TempSecOutTankOut = TankOutletTemp
    TempPriOutTankOut = PlantLoop(LoopNum)%LoopSide(DemandSide)%LoopSideInlet_TankTemp
  ELSE
    TempPriOutTankOut = TankOutletTemp
    TempSecOutTankOut = PlantLoop(LoopNum)%LoopSide(SupplySide)%LoopSideInlet_TankTemp
  ENDIF
  
  ! first do mass balances and find common pipe flow rate and direction
  IF (MdotPri > MdotSec) THEN
    MdotPriRCLeg = MdotPri - MdotSec
    IF (MdotPriRCLeg <  MassFlowTol) THEN
      MdotPriRCLeg = 0.d0
      CPFlowDir = NoRecircFlow
    ELSE
      CPFlowDir = PrimaryRecirc
    ENDIF
    MdotSecRCLeg = 0.d0
    CommonPipeTemp = TempPriOutTankOut
  ELSEIF (MdotPri < MdotSec) THEN
    MdotSecRCLeg = MdotSec - MdotPri
    IF (MdotSecRCLeg < MassFlowTol) THEN
      MdotSecRCLeg = 0.d0
      CPFlowDir = NoRecircFlow
    ELSE
      CPFlowDir = SecondaryRecirc
    ENDIF
    MdotPriRCLeg = 0.d0
    CommonPipeTemp = TempSecOutTankOut
  ELSE ! equal
    MdotPriRCLeg = 0.d0
    MdotSecRCLeg = 0.d0
    CPFlowDir = NoRecircFlow
    CommonPipeTemp = (TempPriOutTankOut + TempSecOutTankOut) / 2.d0 
  ENDIF
  
  ! now calculate inlet temps
  
  IF (MdotSec > 0.d0) THEN
    TempSecInlet = (MdotPri*TempPriOutTankOut + MdotSecRCLeg*TempSecOutTankOut - MdotPriRCLeg * TempPriOutTankOut) &
                   / (MdotSec)
  ELSE
    TempSecInlet = TempPriOutTankOut
  ENDIF
  IF (MdotPri > 0.d0) THEN
    TempPriInlet = (MdotSec*TempSecOutTankOut + MdotPriRCLeg*TempPriOutTankOut - MdotSecRCLeg*TempSecOutTankOut) &
                    / (MdotPri)
  ELSE
    TempPriInlet = TempSecOutTankOut
  ENDIF

  !Update the Common Pipe Data structure for reporting purposes.
  PlantCommonPipe(LoopNum)%Flow = MAX(MdotPriRCLeg, MdotSecRCLeg)
  PlantCommonPipe(LoopNum)%Temp = CommonPipeTemp
  PlantCommonPipe(LoopNum)%FlowDir = CPFlowDir
  Node(NodeNumSecIn)%Temp = TempSecInlet
  Node(NodeNumPriIn)%Temp = TempPriInlet
  
 IF (LoopSide == SupplySide) THEN
   MixedOutletTemp = TempPriInlet
 ELSE
   MixedOutletTemp = TempSecInlet
 ENDIF
  
  RETURN

END SUBROUTINE ManageSingleCommonPipe

SUBROUTINE ManageTwoWayCommonPipe(LoopNum,LoopSide, TankOutletTemp, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, Oct 2011.  rewrite

          ! PURPOSE OF THIS SUBROUTINE:
          ! manage two-way common pipe modeling at half-loop interface

          ! METHODOLOGY EMPLOYED:
          ! calculate mixed temperatures and various flow rates
          ! sequential subsitution of system of equations

          ! REFERENCES:
          ! reimplementation of CheckTwoWayCommonPipeConditions by Sankaranarayanan K P Jan 2007

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY : BeginEnvrnFlag
  USE DataPlant,      ONLY : PlantLoop, SupplySide, DemandSide, TotNumLoops, MassFlowTol, DeltaTemptol, &
                             PlantReport
  USE DataLoopNode,   ONLY : Node
  USE PlantUtilities, ONLY : SetActuatedBranchFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: LoopNum
  INTEGER, INTENT(IN)    :: LoopSide
  REAL(r64), INTENT(IN)  :: TankOutletTemp
  LOGICAL, INTENT(IN)    :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: DemandLedPrimaryInletUpdate   = 101
  INTEGER, PARAMETER :: DemandLedSecondaryInletUpdate = 102
  INTEGER, PARAMETER :: SupplyLedPrimaryInletUpdate   = 103
  INTEGER, PARAMETER :: SupplyLedSecondaryInletUpdate = 104
  INTEGER, PARAMETER :: BothLedPrimaryInletUpdate = 105
  INTEGER, PARAMETER :: BothLedSecondaryInletUpdate = 106

  INTEGER, PARAMETER :: NeedsMoreFlow    = 201
  INTEGER, PARAMETER :: NeedsLessFlow    = 202
  INTEGER, PARAMETER :: NeedsSameFlow    = 203

  REAL(r64), PARAMETER :: MdotPerturbFactor = 0.02D0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE,ALLOCATABLE, DIMENSION(:)   :: MyEnvrnFlag
  LOGICAL,SAVE     :: OneTimeData =.TRUE.
  INTEGER  :: CurCallingCase ! local temporary
  REAL(r64)  :: MdotPri      = 0.d0 ! flow rate on primary side kg/s
  REAL(r64)  :: MdotSec      = 0.d0 ! flow rate on secondary side kg/s
  REAL(r64)  :: MdotPriToSec = 0.d0 ! flow rate between primary and secondary side kg/s
  REAL(r64)  :: MdotPriRCLeg = 0.d0 ! flow rate on primary recirculation common pipe kg/s
  REAL(r64)  :: MdotSecRCLeg = 0.d0 ! flow rate on secondary recirculation common pipe kg/s
  REAL(r64)  :: TempSecInlet = 0.d0 ! temperature at secondary inlet deg C
  REAL(r64)  :: TempPriInlet = 0.d0 ! temperature at primary inlet deg C
  REAL(r64)  :: TempPriOutTankOut = 0.d0
  REAL(r64)  :: TempSecOutTankOut = 0.d0
  REAL(r64)  :: TempCPPrimaryCntrlSetpoint = 0.d0
 ! REAL(r64)  :: TempCPCntrlCurrent  = 0.d0
  REAL(r64)  :: TempCPSecondaryCntrlSetpoint = 0.d0
 ! REAL(r64)  :: TempCPCntrlCurrent  = 0.d0
  INTEGER    :: NodeNumPriOut = 0
  INTEGER    :: NodeNumSecOut = 0
  INTEGER    :: NodeNumPriIn  = 0
  INTEGER    :: NodeNumSecIn  = 0
  
  INTEGER    :: MaxIterLimitCaseA = 8
  INTEGER    :: MaxIterLimitCaseB = 4

  INTEGER    :: loop ! interation loop counter
!  INTEGER    :: loop2

  ! one time setups
  IF (OneTimeData) THEN
    IF ( .NOT. CommonPipeSetupFinished )    CALL SetupCommonPipes
    ALLOCATE(MyEnvrnFlag(TotNumLoops))
    MyEnvrnFlag = .TRUE.
    OneTimeData = .FALSE.
  END IF

  !fill local node indexes
  NodeNumPriIn  = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn
  NodeNumPriOut = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut
  NodeNumSecIn  = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn
  NodeNumSecOut = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumOut

  ! begin environment inits
  IF (MyEnvrnFlag(LoopNum) .and. BeginEnvrnFlag) THEN
    PlantCommonPipe(LoopNum)%PriToSecFlow = 0.d0
    PlantCommonPipe(LoopNum)%SecToPriFlow = 0.d0
    PlantCommonPipe(LoopNum)%PriCPLegFlow = 0.d0
    PlantCommonPipe(LoopNum)%SecCPLegFlow = 0.d0
    MyEnvrnFlag(LoopNum)  = .FALSE.
  ENDIF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(LoopNum)=.TRUE.
  END IF

  ! every time inits
  MdotSec      = Node(NodeNumSecOut)%MassFlowRate ! assume known and fixed by demand side operation
  TempCPPrimaryCntrlSetpoint   = Node(NodeNumPriIn)%TempSetpoint
  TempCPSecondaryCntrlSetpoint = Node(NodeNumSecIn)%TempSetpoint
  
  ! 6 unknowns follow, fill with current values
  MdotPriToSec = PlantCommonPipe(LoopNum)%PriToSecFlow
  MdotPriRCLeg = PlantCommonPipe(LoopNum)%PriCPLegFlow
  MdotSecRCLeg = PlantCommonPipe(LoopNum)%SecCPLegFlow
  TempSecInlet = Node(NodeNumSecIn)%Temp
  TempPriInlet = Node(NodeNumPriIn)%Temp
  MdotPri      = Node(NodeNumPriOut)%MassFlowRate !may or may not be an unknown, If variable speed primary side, then unknown

  IF (LoopSide == SupplySide) THEN
    TempSecOutTankOut = TankOutletTemp
    TempPriOutTankOut = PlantLoop(LoopNum)%LoopSide(DemandSide)%LoopSideInlet_TankTemp
  ELSE
    TempPriOutTankOut = TankOutletTemp
    TempSecOutTankOut = PlantLoop(LoopNum)%LoopSide(SupplySide)%LoopSideInlet_TankTemp
  ENDIF

  ! determine current case
  ! which side is being updated
  ! commonpipe control point is the inlet of one of the half loops
  IF (LoopSide == SupplySide) THEN !update primary inlet
    IF ( PlantLoop(loopnum)%Loopside(SupplySide)%InletNodeSetPt .AND.  &
        .NOT. PlantLoop(loopnum)%Loopside(DemandSide)%InletNodeSetPt ) THEN
      CurCallingCase    =  SupplyLedPrimaryInletUpdate

    ELSEIF (.NOT. PlantLoop(loopnum)%Loopside(SupplySide)%InletNodeSetPt .AND.  &
              PlantLoop(loopnum)%Loopside(DemandSide)%InletNodeSetPt ) THEN
      CurCallingCase    =  DemandLedPrimaryInletUpdate

    ENDIF
  ELSE ! update secondary inlet
    IF (PlantLoop(loopnum)%Loopside(SupplySide)%InletNodeSetPt .AND.  &
        .NOT. PlantLoop(loopnum)%Loopside(DemandSide)%InletNodeSetPt ) THEN
      CurCallingCase    = SupplyLedSecondaryInletUpdate 


    ELSEIF (.NOT. PlantLoop(loopnum)%Loopside(SupplySide)%InletNodeSetPt .AND.  &
              PlantLoop(loopnum)%Loopside(DemandSide)%InletNodeSetPt ) THEN
      CurCallingCase    = DemandLedSecondaryInletUpdate

    ENDIF
  ENDIF

  SELECT CASE (CurCallingCase)

  CASE (SupplyLedPrimaryInletUpdate, SupplyLedSecondaryInletUpdate)
   ! CASE A, Primary/Supply Led 
      ! six equations and six unknowns (although one has a setpoint)
    DO loop = 1, MaxIterLimitCaseA

      ! eq 1
      IF (ABS(TempSecOutTankOut -  TempCPPrimaryCntrlSetpoint )  > DeltaTemptol) THEN
        MdotPriToSec = MdotPriRCLeg * (TempCPPrimaryCntrlSetpoint - TempPriOutTankOut) &
                                 / ( TempSecOutTankOut -  TempCPPrimaryCntrlSetpoint)
        IF (MdotPriToSec < MassFlowTol) MdotPriToSec = 0.d0
        IF (MdotPriToSec > MdotSec) MdotPriToSec = MdotSec
      ELSE
        MdotPriToSec = MdotSec !  what to do (?) 
      ENDIF
      ! eq. 5
      MdotPriRCLeg = MdotPri - MdotPriToSec
      IF (MdotPriRCLeg < MassFlowTol) MdotPriRCLeg = 0.d0
      
      ! eq. 4
      MdotSecRCLeg = MdotSec - MdotPriToSec
      IF (MdotSecRCLeg < MassFlowTol) MdotSecRCLeg = 0.d0
      
      ! eq  6
      IF ((MdotPriToSec + MdotSecRCLeg) > MassFlowTol) THEN
        TempSecInlet = (MdotPriToSec * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut) &
                      / (MdotPriToSec + MdotSecRCLeg)
      ELSE
        TempSecInlet = TempPriOutTankOut
      ENDIF
      
      ! eq. 3
      IF ((PlantCommonPipe(LoopNum)%SupplySideInletPumpType == VariableFlow) &
         !  .AND. FirstHVACIteration 
           .AND. (CurCallingCase == SupplyLedPrimaryInletUpdate) )THEN
        ! MdotPri is a variable to be calculated and flow request needs to be made
        IF (ABS(TempCPPrimaryCntrlSetpoint  ) > DeltaTemptol) THEN
!          Do loop2 = 1, MaxIterLimitCaseA
!            MdotPri = (MdotSec *  TempSecInlet +  MdotPriRCLeg *TempPriOutTankOut - MdotSecRCLeg * TempSecOutTankOut )  &
!                          /  (TempPriOutTankOut )
            
            MdotPri = (MdotPriRCLeg * TempPriOutTankOut + MdotPriToSec * TempSecOutTankOut) &
                        / (TempCPPrimaryCntrlSetpoint)
          
       !   ENDDO
          IF (MdotPri < MassFlowTol) MdotPri = 0.d0
        ELSE
          MdotPri = MdotSec
        ENDIF
        CALL SetActuatedBranchFlowRate(MdotPri,NodeNumPriIn,LoopNum,SupplySide, 1, .FALSE.)
      ENDIF

      
      ! eq. 2
      IF ((MdotPriToSec + MdotPriRCLeg) > MassFlowTol ) THEN
        TempPriInlet = (MdotPriToSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut) &
                      / (MdotPriToSec + MdotPriRCLeg)
      ELSE
        TempPriInlet = TempSecOutTankOut
      ENDIF
      

    ENDDO
  CASE (DemandLedPrimaryInletUpdate, DemandLedSecondaryInletUpdate)
    ! case B. Secondary/demand led

      ! six equations and six unknowns (although one has a setpoint)
    DO Loop = 1, MaxIterLimitCaseB
      ! eq 1, 
      IF (ABS(TempPriOutTankOut   - TempSecOutTankOut )  > DeltaTemptol) THEN
        MdotPriToSec = MdotSec * (TempCPSecondaryCntrlSetpoint - TempSecOutTankOut ) &
                             / (TempPriOutTankOut   - TempSecOutTankOut )
        IF (MdotPriToSec < MassFlowTol) MdotPriToSec = 0.d0
        IF (MdotPriToSec > MdotSec) MdotPriToSec = MdotSec
      ELSE
        MdotPriToSec = MdotSec
      ENDIF

      ! eq. 2, 
      IF ((MdotPriToSec + MdotPriRCLeg) > MassFlowTol ) THEN
        TempPriInlet = (MdotPriToSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut) &
                      / (MdotPriToSec + MdotPriRCLeg)
      ELSE
        TempPriInlet = TempSecOutTankOut
      ENDIF

      ! eq. 3
      IF ((PlantCommonPipe(LoopNum)%SupplySideInletPumpType == VariableFlow) &
          !  .AND. FirstHVACIteration  &
            .AND. (CurCallingCase == DemandLedPrimaryInletUpdate)) THEN
        ! MdotPri is a variable to be calculated and flow request made
        IF (ABS(TempPriOutTankOut - TempPriInlet ) > DeltaTemptol) THEN
          MdotPri = MdotSec * ( TempCPSecondaryCntrlSetpoint -  TempSecOutTankOut )  &
                        /  (TempPriOutTankOut - TempPriInlet )
          IF (MdotPri < MassFlowTol) MdotPri = 0.d0
        ELSE
          MdotPri = MdotSec
        ENDIF
        CALL SetActuatedBranchFlowRate(MdotPri,NodeNumPriIn,LoopNum,SupplySide, 1, .FALSE.)
      ENDIF
      
      ! eq. 4
      MdotSecRCLeg = MdotSec - MdotPriToSec
      IF (MdotSecRCLeg < MassFlowTol) MdotSecRCLeg = 0.d0
      
      ! eq. 5
      MdotPriRCLeg = MdotPri - MdotPriToSec
      IF (MdotPriRCLeg < MassFlowTol) MdotPriRCLeg = 0.d0
      
      ! eq  6
      IF ((MdotPriToSec + MdotSecRCLeg) > MassFlowTol) THEN
        TempSecInlet = (MdotPriToSec * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut) &
                      / (MdotPriToSec + MdotSecRCLeg)
      ELSE
        TempSecInlet = TempPriOutTankOut
      ENDIF

    ENDDO

  END SELECT

  !update
  PlantCommonPipe(LoopNum)%PriToSecFlow = MdotPriToSec
  PlantCommonPipe(LoopNum)%SecToPriFlow = MdotPriToSec
  PlantCommonPipe(LoopNum)%PriCPLegFlow = MdotPriRCLeg
  PlantCommonPipe(LoopNum)%SecCPLegFlow = MdotSecRCLeg
  Node(NodeNumSecIn)%Temp = TempSecInlet
  Node(NodeNumPriIn)%Temp = TempPriInlet


  RETURN

END SUBROUTINE ManageTwoWayCommonPipe


SUBROUTINE SetupCommonPipes

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan. 2010
          !       MODIFIED       B. Griffith Oct. 2011
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! collect allocation, outputs, and other set up for common pipes

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE DataInterfaces
  
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
  INTEGER :: CurLoopNum ! local do loop counter

  ALLOCATE(PlantCommonPipe(TotNumLoops))

  DO CurLoopNum = 1, TotNumLoops
    SELECT CASE (PlantLoop(CurLoopNum)%CommonPipeType)
      CASE (CommonPipe_No)
        PlantCommonPipe(CurLoopNum)%CommonPipeType     = CommonPipe_No
        
      CASE (CommonPipe_Single)!Uncontrolled ('single') common pipe
        PlantCommonPipe(CurLoopNum)%CommonPipeType     = CommonPipe_Single
        CALL SetupOutputVariable('Common Pipe Mass Flow Rate [Kg/s]', &
          PlantCommonPipe(CurLoopNum)%Flow,'System','Average',PlantLoop(CurLoopNum)%Name)
        CALL SetupOutputVariable('Common Pipe Flow Temperature [C]', &
          PlantCommonPipe(CurLoopNum)%Temp,'System','Average',PlantLoop(CurLoopNum)%Name)
        CALL SetupOutputVariable('Common Pipe Flow Direction []', &
          PlantCommonPipe(CurLoopNum)%FlowDir,'System','Average',PlantLoop(CurLoopNum)%Name)
        
        IF (PlantLoop(CurLoopNum)%LoopSide(SupplySide)%Branch(1)%Comp(1)%TypeOf_Num == TypeOf_PumpVariableSpeed) THEN
         ! If/when the model supports variable-pumping primary, this can be removed.
          CALL ShowWarningError('SetupCommonPipes: detected variable speed pump on supply inlet of CommonPipe plant loop')
          CALL ShowContinueError('Occurs on plant loop name = '//TRIM(PlantLoop(CurLoopNum)%Name) )
          CALL ShowContinueError('The common pipe model does not support varying the flow rate on the primary/supply side')
          CALL ShowContinueError('The primary/supply side will operate as if constant speed, and the simulation continues')
         
        ENDIF
        
      CASE (CommonPipe_TwoWay)!Controlled ('two-way') common pipe
        PlantCommonPipe(CurLoopNum)%CommonPipeType     = CommonPipe_TwoWay
        CALL SetupOutputVariable('Primary Side Common Pipe Mass Flow Rate [Kg/s]', &
          PlantCommonPipe(CurLoopNum)%PriCPLegFlow,'System','Average',PlantLoop(CurLoopNum)%Name)
        CALL SetupOutputVariable('Secondary Side Common Pipe Mass Flow Rate [Kg/s]', &
          PlantCommonPipe(CurLoopNum)%SecCPLegFlow,'System','Average',PlantLoop(CurLoopNum)%Name)
        CALL SetupOutputVariable('Primary to Secondary Mass Flow Rate [Kg/s]', &
           PlantCommonPipe(CurLoopNum)%PriToSecFlow,'System','Average',PlantLoop(CurLoopNum)%Name)
        CALL SetupOutputVariable('Secondary to Primary Mass Flow Rate [Kg/s]', &
           PlantCommonPipe(CurLoopNum)%SecToPriFlow,'System','Average',PlantLoop(CurLoopNum)%Name)
           
        ! check type of pump on supply side inlet
        IF (PlantLoop(CurLoopNum)%LoopSide(SupplySide)%Branch(1)%Comp(1)%TypeOf_Num == TypeOf_PumpConstantSpeed) THEN
             PlantCommonPipe(CurLoopNum)%SupplySideInletPumpType = ConstantFlow
        ELSEIF (PlantLoop(CurLoopNum)%LoopSide(SupplySide)%Branch(1)%Comp(1)%TypeOf_Num == TypeOf_PumpVariableSpeed) THEN
             PlantCommonPipe(CurLoopNum)%SupplySideInletPumpType = VariableFlow
             ! If/when the model supports variable-pumping primary, this can be removed.
          CALL ShowWarningError('SetupCommonPipes: detected variable speed pump on supply inlet of TwoWayCommonPipe plant loop')
          CALL ShowContinueError('Occurs on plant loop name = '//TRIM(PlantLoop(CurLoopNum)%Name) )
          CALL ShowContinueError('The common pipe model does not support varying the flow rate on the primary/supply side')
          CALL ShowContinueError('The primary/supply side will operate as if constant speed, and the simulation continues')
             
        ENDIF
        ! check type of pump on demand side inlet
         IF (PlantLoop(CurLoopNum)%LoopSide(DemandSide)%Branch(1)%Comp(1)%TypeOf_Num == TypeOf_PumpConstantSpeed) THEN
             PlantCommonPipe(CurLoopNum)%DemandSideInletPumpType = ConstantFlow
         ELSEIF (PlantLoop(CurLoopNum)%LoopSide(DemandSide)%Branch(1)%Comp(1)%TypeOf_Num == TypeOf_PumpVariableSpeed) THEN
             PlantCommonPipe(CurLoopNum)%DemandSideInletPumpType = VariableFlow
         ENDIF
         
    END SELECT
  END DO

  CommonPipeSetupFinished = .TRUE.

  RETURN

END SUBROUTINE SetupCommonPipes



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
!

END MODULE HVACInterfaceManager
