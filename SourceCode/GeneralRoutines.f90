SUBROUTINE ControlCompOutput(CompName,CompType,CompNum,FirstHVACIteration,QZnReq, &
                             ActuatedNode,MaxFlow,MinFlow,TempInNode,TempOutNode, &
                             ControlOffSet,AirMassFlow,Action,ControlCompTypeNum, &
                             CompErrIndex,EquipIndex,LoopNum, LoopSide, BranchIndex, &
                             ControlledZoneIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   April 2000
          !       MODIFIED       Brent Griffith, Sept 2010 update plant interactions
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !The purpose of this subroutine is to control the output of heating or cooling
          !meet the zone load.

          ! METHODOLOGY EMPLOYED:
          ! Currently this is using an intervasl halving scheme to a control tolerance

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataLoopNode
  USE DataGlobals,            ONLY : WarmUpFlag
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE DataInterfaces,         ONLY : ShowWarningError,ShowFatalError,ShowContinueError,ShowContinueErrorTimeStamp,  &
                                     ShowRecurringWarningErrorAtEnd,ShowWarningMessage,ShowSevereError
  USE InputProcessor,         ONLY : FindItemInSortedList
  USE WaterCoils,             ONLY : SimulateWaterCoilComponents
  USE FanCoilUnits,           ONLY : Calc4PipeFanCoil
  USE UnitVentilator,         ONLY : CalcUnitVentilatorComponents
  USE UnitHeater,             ONLY : CalcUnitHeaterComponents
  USE HWBaseboardRadiator,    ONLY : CalcHWBaseboard
  USE BaseboardRadiator,      ONLY : SimHWConvective
  USE Psychrometrics,         ONLY : PsyCpAirFnWTdb
  USE VentilatedSlab,         ONLY : CalcVentilatedSlabComps
  USE InputProcessor,         ONLY : MakeUPPERCase
  USE General,                ONLY : TrimSigDigits, RoundSigDigits
  USE SteamBaseboardRadiator, ONLY : CalcSteamBaseboard
  USE OutdoorAirUnit,         ONLY : CalcOAUnitCoilComps
  USE PlantUtilities,         ONLY : SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT (IN)    :: CompName    ! the component Name
CHARACTER(len=*), INTENT (IN)    :: CompType    ! Type of component
INTEGER, INTENT (INOUT)          :: CompNum     ! Index of component in component array
LOGICAL, INTENT (IN)             :: FirstHVACIteration  ! flag for 1st HVAV iteration in the time step
REAL(r64), INTENT (IN)           :: QZnReq              ! zone load to be met
INTEGER, INTENT (IN)             :: ActuatedNode        ! node that controls unit output
REAL(r64), INTENT (IN)           :: MaxFlow             ! maximum water flow
REAL(r64), INTENT (IN)           :: MinFlow             ! minimum water flow
INTEGER, INTENT (IN), OPTIONAL   :: TempInNode  ! inlet node for output calculation
INTEGER, INTENT (IN), OPTIONAL   :: TempOutNode ! outlet node for output calculation
REAL(r64), INTENT (IN)           :: ControlOffset       ! really the tolerance
REAL(r64), INTENT (IN), OPTIONAL :: AirMassFlow ! air mass flow rate
INTEGER, INTENT (IN), OPTIONAL   :: Action      ! 1=reverse; 2=normal
INTEGER, INTENT (INOUT)          :: ControlCompTypeNum  ! Internal type num for CompType
INTEGER, INTENT (INOUT)          :: CompErrIndex  ! for Recurring error call
INTEGER, INTENT (IN), OPTIONAL   :: EquipIndex      ! Identifier for equipment of Outdoor Air Unit "ONLY"
INTEGER, INTENT (IN), OPTIONAL   :: LoopNum ! for plant components, plant loop index
INTEGER, INTENT (IN), OPTIONAL   :: LoopSide ! for plant components, plant loop side index
INTEGER, INTENT (IN), OPTIONAL   :: BranchIndex ! for plant components, plant branch index
INTEGER, INTENT (IN), OPTIONAL   :: ControlledZoneIndex ! controlled zone index for the zone containing the component

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !Iteration maximum for reheat control
INTEGER, PARAMETER ::    MaxIter             =25
INTEGER, PARAMETER ::    iReverseAction      =1
INTEGER, PARAMETER ::    iNormalAction       =2

! Note - order in routine must match order below
!  Plus -- order in ListOfComponents array must be in sorted order.
INTEGER, PARAMETER :: NumComponents=11
CHARACTER(len=*), DIMENSION(NumComponents), PARAMETER :: ListOfComponents=(/  &
     'AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT ',     &
     'AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT   ',     &
     'COIL:HEATING:WATER                        ',     &
     'ZONEHVAC:BASEBOARD:CONVECTIVE:WATER       ',     &
     'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM',     &
     'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER',     &
     'ZONEHVAC:FOURPIPEFANCOIL                  ',     &
     'ZONEHVAC:OUTDOORAIRUNIT                   ',     &
     'ZONEHVAC:UNITHEATER                       ',     &
     'ZONEHVAC:UNITVENTILATOR                   ',     &
     'ZONEHVAC:VENTILATEDSLAB                   '/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
!Interval Half Type used for Controller
TYPE IntervalHalf
  REAL(r64)     ::MaxFlow
  REAL(r64)     ::MinFlow
  REAL(r64)     ::MaxResult
  REAL(r64)     ::MinResult
  REAL(r64)     ::MidFlow
  REAL(r64)     ::MidResult
  Logical  ::MaxFlowCalc
  Logical  ::MinFlowCalc
  Logical  ::MinFlowResult
  Logical  ::NormFlowCalc
END TYPE IntervalHalf

TYPE ZoneEquipControllerProps
  REAL(r64)    :: SetPoint     ! Desired setpoint;
  REAL(r64)    :: MaxSetPoint  ! The maximum setpoint; either user input or reset per time step by simulation
  REAL(r64)    :: MinSetPoint  ! The minimum setpoint; either user input or reset per time step by simulation
  REAL(r64)    :: SensedValue  ! The sensed control variable of any type
  REAL(r64)    :: CalculatedSetPoint ! The Calculated SetPoint or new control actuated value
END TYPE ZoneEquipControllerProps


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER          :: Iter =0     ! Iteration limit for the interval halving process
REAL(r64)        :: CpAir       ! specific heat of air (J/kg-C)
LOGICAL          :: Converged
REAL(r64)        :: Denom       ! the denominator of the control signal
REAL(r64)        :: LoadMet     ! Actual output of unit (watts)
!INTEGER, SAVE    :: ErrCount=0  ! Number of times that the maximum iterations was exceeded
!INTEGER, SAVE    :: ErrCount1=0 ! for recurring error
LOGICAL          :: WaterCoilAirFlowControl ! True if controlling air flow through water coil, water flow fixed
INTEGER          :: SimCompNum  ! internal number for case statement
TYPE (IntervalHalf), SAVE :: ZoneInterHalf=IntervalHalf(0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,.false.,.false.,.false.,.false.)
TYPE (ZoneEquipControllerProps), SAVE :: ZoneController=ZoneEquipControllerProps(0.0d0,0.0d0,0.0d0,0.0d0,0.0d0)
REAL(r64)        :: HalvingPrec = 0.0d0 ! precision of halving algorithm

 IF (ControlCompTypeNum /= 0) THEN
   SimCompNum=ControlCompTypeNum
 ELSE
   SimCompNum=FindItemInSortedList(CompType,ListOfComponents,NumComponents)
   ControlCompTypeNum=SimCompNum
 ENDIF

 Iter = 0
 Converged = .False.
 WaterCoilAirFlowControl = .FALSE.
 LoadMet = 0.0d0
 HalvingPrec = 0.0d0

 !At the beginning of every time step the value is reset to the User Input
 ZoneController%SetPoint = 0.0d0

 !Set to converged controller
 ZoneInterHalf%MaxFlowCalc = .True.
 ZoneInterHalf%MinFlowCalc = .False.
 ZoneInterHalf%NormFlowCalc = .False.
 ZoneInterHalf%MinFlowResult = .False.
 ZoneInterHalf%MaxResult    = 1.0d0
 ZoneInterHalf%MinResult    = 0.0d0

!Start the Solution Iteration
Do While (.Not. Converged)

 If(FirstHVACIteration) Then
   Node(ActuatedNode)%MassFlowRateMaxAvail = MaxFlow
   Node(ActuatedNode)%MassFlowRateMinAvail = MinFlow
   !Check to make sure that the Minimum Flow rate is less than the max.
   If(MinFlow .gt. MaxFlow)Then
     CALL ShowSevereError('ControlCompOutput:'//TRIM(CompType)//':'//TRIM(CompName)//  &
        ', Min Control Flow is > Max Control Flow')
     CALL ShowContinueError('Acuated Node='//trim(NodeID(ActuatedNode))//  &
        ' MinFlow=['//trim(TrimSigDigits(MinFlow,3))//'], Max Flow='//trim(TrimSigDigits(MaxFlow,3)))
     CALL ShowContinueErrorTimeStamp(' ')
     CALL ShowFatalError('Program terminates due to preceding condition.')
   End If
 End If  ! End of FirstHVACIteration Conditional If
 !The interface managers can reset the Max or Min to available values during the time step
 ! and these will then be the new setpoint limits for the controller to work within.
 IF ( (SimCompNum ==3) .AND. ( .NOT. PRESENT(AirMassFlow)) ) THEN
   ZoneController%MaxSetPoint = Node(ActuatedNode)%MassFlowRateMaxAvail
   ZoneController%MinSetPoint = Node(ActuatedNode)%MassFlowRateMinAvail
 ELSE
   ZoneController%MaxSetPoint = MIN(Node(ActuatedNode)%MassFlowRateMaxAvail,Node(ActuatedNode)%MassFlowRateMax)
   ZoneController%MinSetPoint = MAX(Node(ActuatedNode)%MassFlowRateMinAvail,Node(ActuatedNode)%MassFlowRateMin)
 END IF
 ! The first time through run at maximum flow rate and find results
 If(ZoneInterHalf%MaxFlowcalc) Then
    ZoneController%CalculatedSetPoint = ZoneController%MaxSetPoint
    ZoneInterHalf%MaxFlow = ZoneController%MaxSetPoint
    ZoneInterHalf%MaxFlowcalc = .False.
    ZoneInterHalf%MinFlowCalc = .True.
 ! Record the maximum flow rates and set the flow to the minimum and find results
 Else If(ZoneInterHalf%MinFlowCalc) Then
    ZoneInterHalf%MaxResult = ZoneController%SensedValue
    ZoneController%CalculatedSetPoint = ZoneController%MinSetPoint
    ZoneInterHalf%MinFlow = ZoneController%MinSetPoint
    ZoneInterHalf%MinFlowCalc = .False.
    ZoneInterHalf%MinFlowResult = .True.
 !Record the minimum results and set flow to half way between the max and min and find results
 Else If(ZoneInterHalf%MinFlowResult) Then
    ZoneInterHalf%MinResult = ZoneController%SensedValue
    HalvingPrec = (ZoneInterHalf%MaxResult-ZoneInterHalf%MinResult) * (1.0d0/REAL(2**(MaxIter-3)))
    ZoneInterHalf%MidFlow = (ZoneInterHalf%MaxFlow + &
                                         ZoneInterHalf%MinFlow)/2.0d0
    ZoneController%CalculatedSetPoint = (ZoneInterHalf%MaxFlow + &
                                                     ZoneInterHalf%MinFlow)/2.0d0
    ZoneInterHalf%MinFlowResult = .False.
    ZoneInterHalf%NormFlowCalc = .True.
 ! Record the Mid results and check all possibilities and start interval halving procedure
 Else If(ZoneInterHalf%NormFlowCalc) Then
    ZoneInterHalf%MidResult = ZoneController%SensedValue

    ! First check to see if the component is running; if not converge and return
    IF(ZoneInterHalf%MaxResult == ZoneInterHalf%MinResult) Then
       !Set to converged controller
       Converged = .True.
       ZoneInterHalf%MaxFlowCalc = .True.
       ZoneInterHalf%MinFlowCalc = .False.
       ZoneInterHalf%NormFlowCalc = .False.
       ZoneInterHalf%MinFlowResult = .False.
       ZoneInterHalf%MaxResult    = 1.0d0
       ZoneInterHalf%MinResult    = 0.0d0
       SELECT CASE (SimCompNum)
       CASE (4:6) !hot water baseboards use min flow
         ZoneController%CalculatedSetPoint = 0.0d0                                     !CR7253
       CASE Default
         ZoneController%CalculatedSetPoint = ZoneInterHalf%MaxFlow                    !CR7253
       END SELECT
       !Set the Actuated node massflowrate with zero value
       IF (PRESENT(LoopNum)) THEN ! this is a plant component
         CALL SetActuatedBranchFlowRate(ZoneController%CalculatedSetPoint,ActuatedNode,LoopNum,LoopSide, BranchIndex, .FALSE.) !Objexx:OPTIONAL LoopSide, BranchIndex used without PRESENT check
       ELSE ! assume not a plant component
         Node(ActuatedNode)%MassFlowRate = ZoneController%CalculatedSetPoint
       ENDIF
       Return
    End If


    ! The next series of checks is to determine what interval the current solution is in
    !   comparison to the setpoint and then respond appropriately.

    ! Normal controller assumes that MaxResult will be greater than MinResult. First check
    ! to make sure that this is the case
      If(ZoneInterHalf%MaxResult .le. ZoneInterHalf%MinResult) Then
         IF (WaterCoilAirFlowControl) THEN
           ZoneController%CalculatedSetPoint = ZoneInterHalf%MaxFlow
         ELSE
           ZoneController%CalculatedSetPoint = ZoneInterHalf%MinFlow
         END IF
         !set to converged controller
         Converged = .True.
         ZoneInterHalf%MaxFlowCalc = .True.
         ZoneInterHalf%MinFlowCalc = .False.
         ZoneInterHalf%NormFlowCalc = .False.
         ZoneInterHalf%MinFlowResult = .False.
         ZoneInterHalf%MaxResult    = 1.0d0
         ZoneInterHalf%MinResult    = 0.0d0
      ! MaxResult is greater than MinResult so simulation control algorithm may proceed normally
      ElseIf(ZoneInterHalf%MaxResult .gt. ZoneInterHalf%MinResult) Then
      !Now check to see if the setpoint is outside the endpoints of the control range
      ! First check to see if the water is too cold and if so set to the minimum flow.
        If(ZoneController%SetPoint .le. ZoneInterHalf%MinResult) Then
           ZoneController%CalculatedSetPoint = ZoneInterHalf%MinFlow
           !Set to Converged Controller
           Converged = .True.
           ZoneInterHalf%MaxFlowCalc = .True.
           ZoneInterHalf%MinFlowCalc = .False.
           ZoneInterHalf%NormFlowCalc = .False.
           ZoneInterHalf%MinFlowResult = .False.
           ZoneInterHalf%MaxResult    = 1.0d0
           ZoneInterHalf%MinResult    = 0.0d0
        ! Then check if too hot and if so set it to the maximum flow
        Else If(ZoneController%SetPoint .ge. ZoneInterHalf%MaxResult) Then
           ZoneController%CalculatedSetPoint = ZoneInterHalf%MaxFlow
           !Set to Converged Controller
           Converged = .True.
           ZoneInterHalf%MaxFlowCalc = .True.
           ZoneInterHalf%MinFlowCalc = .False.
           ZoneInterHalf%NormFlowCalc = .False.
           ZoneInterHalf%MinFlowResult = .False.
           ZoneInterHalf%MaxResult    = 1.0d0
           ZoneInterHalf%MinResult    = 0.0d0
        ! If between the max and mid set to new flow and raise min to mid
        Else If((ZoneController%SetPoint .lt. ZoneInterHalf%MaxResult) .and. &
            (ZoneController%SetPoint .ge. ZoneInterHalf%MidResult)) Then
          ZoneController%CalculatedSetPoint = (ZoneInterHalf%MaxFlow + &
                                                         ZoneInterHalf%MidFlow)/2.0d0
          ZoneInterHalf%MinFlow = ZoneInterHalf%MidFlow
          ZoneInterHalf%MinResult = ZoneInterHalf%MidResult
          ZoneInterHalf%MidFlow = (ZoneInterHalf%MaxFlow + &
                                               ZoneInterHalf%MidFlow)/2.0d0
        ! If between the min and mid set to new flow and lower Max to mid
        Else If((ZoneController%SetPoint .lt. ZoneInterHalf%MidResult) .and. &
            (ZoneController%SetPoint .gt. ZoneInterHalf%MinResult)) Then
          ZoneController%CalculatedSetPoint = (ZoneInterHalf%MinFlow + &
                                                         ZoneInterHalf%MidFlow)/2.0d0
          ZoneInterHalf%MaxFlow = ZoneInterHalf%MidFlow
          ZoneInterHalf%MaxResult = ZoneInterHalf%MidResult
          ZoneInterHalf%MidFlow = (ZoneInterHalf%MinFlow + &
                                             ZoneInterHalf%MidFlow)/2.0d0

        End IF ! End of the Conditional for the actual interval halving scheme itself
      EndIf  ! end of max > min check

 End If ! End of the Conditinal for the first 3 iterations for the interval halving


 ! Make sure that the Calculated setpoint falls between the minimum and maximum allowed
 If(ZoneController%CalculatedSetPoint .gt.  &
        ZoneController%MaxSetPoint) Then
    ZoneController%CalculatedSetPoint = ZoneController%MaxSetPoint
    Converged = .True.
    ZoneInterHalf%MaxFlowCalc = .True.
    ZoneInterHalf%MinFlowCalc = .False.
    ZoneInterHalf%NormFlowCalc = .False.
    ZoneInterHalf%MinFlowResult = .False.
    ZoneInterHalf%MaxResult    = 1.0d0
    ZoneInterHalf%MinResult    = 0.0d0
 Else If(ZoneController%CalculatedSetPoint .lt.  &
         ZoneController%MinSetPoint) Then
    ZoneController%CalculatedSetPoint = ZoneController%MinSetPoint
    Converged = .True.
    ZoneInterHalf%MaxFlowCalc = .True.
    ZoneInterHalf%MinFlowCalc = .False.
    ZoneInterHalf%NormFlowCalc = .False.
    ZoneInterHalf%MinFlowResult = .False.
    ZoneInterHalf%MaxResult    = 1.0d0
    ZoneInterHalf%MinResult    = 0.0d0
 End IF

  ! check if hunting down around the limit of a significant mass flow in systems.
  IF ((Iter > MaxIter/2) .AND. (ZoneController%CalculatedSetPoint < MassFlowTolerance) ) THEN
    ZoneController%CalculatedSetPoint = ZoneController%MinSetPoint
    Converged = .True.
    ZoneInterHalf%MaxFlowCalc = .True.
    ZoneInterHalf%MinFlowCalc = .False.
    ZoneInterHalf%NormFlowCalc = .False.
    ZoneInterHalf%MinFlowResult = .False.
    ZoneInterHalf%MaxResult    = 1.0d0
    ZoneInterHalf%MinResult    = 0.0d0
  ENDIF

  !Set the Actuated node massflowrate with the new value
  IF (PRESENT(LoopNum)) THEN ! this is a plant component
    CALL SetActuatedBranchFlowRate(ZoneController%CalculatedSetPoint,ActuatedNode,LoopNum,LoopSide, BranchIndex, .FALSE.) !Objexx:OPTIONAL LoopSide, BranchIndex used without PRESENT check
  ELSE ! assume not a plant component, leave alone
    Node(ActuatedNode)%MassFlowRate = ZoneController%CalculatedSetPoint
  ENDIF

 ! The denominator of the control signal should be no less than 100 watts
 Denom = SIGN( MAX( ABS(QZnReq), 100.d0), QZnReq)
 IF (PRESENT(Action)) THEN
   IF (Action .eq. iNormalAction) THEN
     Denom = MAX(ABS(QZnReq),100.d0)
   ELSE IF (Action .eq. iReverseAction) THEN
     Denom = -MAX(ABS(QZnReq),100.d0)
   ELSE
     CALL ShowFatalError('ControlCompOutput: Illegal Action argument =['//trim(TrimSigDigits(Action))//']')
   END IF
 END IF

 SELECT CASE(SimCompNum)

   CASE(1) ! 'AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT'
     ! simulate series piu reheat coil
     CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompNum)
     ! Calculate the control signal (the variable we are forcing to zero)
     CpAir = PsyCpAirFnWTdb(Node(TempOutNode)%HumRat,0.5d0*(Node(TempOutNode)%Temp + Node(TempInNode)%Temp)) !Objexx:OPTIONAL TempInNode, TempOutNode used without PRESENT check
     LoadMet = CpAir*Node(TempOutNode)%MassFlowRate*(Node(TempOutNode)%Temp - Node(TempInNode)%Temp) !Objexx:OPTIONAL TempInNode, TempOutNode used without PRESENT check
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(2) ! 'AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT'
     ! simulate series piu reheat coil
     CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompNum)
     ! Calculate the control signal (the variable we are forcing to zero)
     CpAir = PsyCpAirFnWTdb(Node(TempOutNode)%HumRat,0.5d0*(Node(TempOutNode)%Temp + Node(TempInNode)%Temp)) !Objexx:OPTIONAL TempInNode, TempOutNode used without PRESENT check
     LoadMet = CpAir*Node(TempOutNode)%MassFlowRate*(Node(TempOutNode)%Temp - Node(TempInNode)%Temp) !Objexx:OPTIONAL TempInNode, TempOutNode used without PRESENT check
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(3) ! 'COIL:HEATING:WATER'
     ! Simulate reheat coil for the VAV system
     CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompNum)
     ! Calculate the control signal (the variable we are forcing to zero)
     CpAir = PsyCpAirFnWTdb(Node(TempOutNode)%HumRat,Node(TempOutNode)%Temp)
     IF (PRESENT(AirMassFlow)) THEN
       LoadMet = AirMassflow * CpAir * Node(TempOutNode)%Temp
       ZoneController%SensedValue = (LoadMet - QZnReq) / Denom
     ELSE
       WaterCoilAirFlowControl = .TRUE.
       LoadMet = Node(TempOutNode)%MassFlowRate*CpAir*(Node(TempOutNode)%Temp - Node(TempInNode)%Temp) !Objexx:OPTIONAL TempInNode, TempOutNode used without PRESENT check
       ZoneController%SensedValue = (LoadMet - QZnReq) / Denom
     END IF

   CASE(4) ! 'ZONEHVAC:BASEBOARD:CONVECTIVE:WATER'
     ! Simulate baseboard
     CALL SimHWConvective(CompNum,LoadMet)
     ! Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(5) ! 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM'
     ! Simulate baseboard
     CALL CalcSteamBaseboard(CompNum, LoadMet)
     ! Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(6) ! 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER'
     ! Simulate baseboard
     CALL CalcHWBaseboard(CompNum, LoadMet)
     ! Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(7) ! 'ZONEHVAC:FOURPIPEFANCOIL'
     ! Simulate fancoil unit
     CALL Calc4PipeFanCoil(CompNum,ControlledZoneIndex,FirstHVACIteration,LoadMet)
     !Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(8) !'ZONEHVAC:OUTDOORAIRUNIT'
     ! Simulate outdoor air unit components
     CALL CalcOAUnitCoilComps(CompNum,FirstHVACIteration,EquipIndex,LoadMet) !Objexx:OPTIONAL EquipIndex used without PRESENT check
     !Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(9) ! 'ZONEHVAC:UNITHEATER'
     ! Simulate unit heater components
     CALL CalcUnitHeaterComponents(CompNum,FirstHVACIteration,LoadMet)
     !Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(10) ! 'ZONEHVAC:UNITVENTILATOR'
     ! Simulate unit ventilator components
     CALL CalcUnitVentilatorComponents(CompNum,FirstHVACIteration,LoadMet)
     !Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom

   CASE(11) ! 'ZONEHVAC:VENTILATEDSLAB'
     ! Simulate unit ventilator components
     CALL CalcVentilatedSlabComps(CompNum,FirstHVACIteration,LoadMet)
     !Calculate the control signal (the variable we are forcing to zero)
     ZoneController%SensedValue = (LoadMet - QZnReq) / Denom


   CASE DEFAULT
     CALL ShowFatalError('ControlCompOutput: Illegal Component Number argument =['//trim(TrimSigDigits(SimCompNum))//']')

 END SELECT

 ! Check for Controller convergence to see if within the offset
 IF(ABS(ZoneController%SensedValue) .le. ControlOffset .or. ABS(ZoneController%SensedValue) .le. HalvingPrec) Then
      !Set to converged controller
      Converged = .True.
      ZoneInterHalf%MaxFlowCalc = .True.
      ZoneInterHalf%MinFlowCalc = .False.
      ZoneInterHalf%NormFlowCalc = .False.
      ZoneInterHalf%MinFlowResult = .False.
      ZoneInterHalf%MaxResult    = 1.0d0
      ZoneInterHalf%MinResult    = 0.0d0
      Exit
 End If

 Iter = Iter + 1
 IF ((Iter > MaxIter).AND.(.NOT.WarmUpFlag)) THEN
   ! IF (CompErrIndex == 0) THEN
     CALL ShowWarningMessage ('ControlCompOutput: Maximum iterations exceeded for '//TRIM(CompType)//' = '//TRIM(CompName))
     CALL ShowContinueError('... Load met       = '//TRIM(TrimSigDigits(LoadMet,5))//' W.')
     CALL ShowContinueError('... Load requested = '//TRIM(TrimSigDigits(QZnReq,5))//' W.')
     CALL ShowContinueError('... Error          = '//TRIM(TrimSigDigits(ABS((LoadMet-QZnReq)*100.d0/Denom),8))//' %.')
     CALL ShowContinueError('... Tolerance      = '//TRIM(TrimSigDigits(ControlOffset*100.d0,8))//' %.')
     CALL ShowContinueError('... Error          = (Load met - Load requested) / MAXIMUM(Load requested, 100)')
     CALL ShowContinueError('... Actuated Node Mass Flow Rate =' &
           //TRIM(RoundSigDigits(Node(ActuatedNode)%MassFlowRate, 9))//' kg/s')
     CALL ShowContinueErrorTimeStamp(' ')
     CALL ShowRecurringWarningErrorAtEnd('ControlCompOutput: Maximum iterations error for '//TRIM(CompType)//  &
           ' = '//TRIM(CompName),CompErrIndex,ReportMaxOf=ABS((LoadMet-QZnReq)*100.d0/Denom),ReportMaxUnits='%',  &
           ReportMinOf=ABS((LoadMet-QZnReq)*100.d0/Denom),ReportMinUnits='%')
   ! ENDIF
   CALL ShowRecurringWarningErrorAtEnd('ControlCompOutput: Maximum iterations error for '//TRIM(CompType)//  &
           ' = '//TRIM(CompName),CompErrIndex,ReportMaxOf=ABS((LoadMet-QZnReq)*100.d0/Denom),ReportMaxUnits='%',  &
           ReportMinOf=ABS((LoadMet-QZnReq)*100.d0/Denom),ReportMinUnits='%')
   EXIT  ! It will not converge this time
 ELSEIF (Iter > MaxIter*2) THEN
   EXIT
 END IF

End Do  ! End of the Convergence Iteration

RETURN

END SUBROUTINE ControlCompOutput


SUBROUTINE CheckSysSizing(CompType,CompName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   October 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine is called when an "autosize" input is encountered in a component
          ! sizing routine to check that the system sizing calculations have been done.

          ! METHODOLOGY EMPLOYED:
          ! Checks SysSizingRunDone flag. If false throws a fatal error.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: DoSystemSizing
  USE DataInterfaces, ONLY: ShowSevereError,ShowContinueError,ShowFatalError
  USE DataSizing,  ONLY: SysSizingRunDone,NumSysSizInput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType     ! Component Type (e.g. Chiller:Electric)
  CHARACTER(len=*), INTENT(IN) :: CompName     ! Component Name (e.g. Big Chiller)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF ( .NOT. SysSizingRunDone) THEN
    CALL ShowSevereError('For autosizing of ' // TRIM(CompType) // ' ' // TRIM(CompName) // &
                       ', a system sizing run must be done.')
    IF (NumSysSizInput == 0) THEN
      CALL ShowContinueError('No "Sizing:System" objects were entered.')
    ENDIF
    IF (.not. DoSystemSizing) THEN
      CALL ShowContinueError('The "SimulationControl" object did not have the field "Do System Sizing Calculation" set to Yes.')
    ENDIF
    CALL ShowFatalError('Program terminates due to previously shown condition(s).')
  END IF

  RETURN

END SUBROUTINE CheckSysSizing

SUBROUTINE CheckThisAirSystemForSizing(AirLoopNum, AirLoopWasSized )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   October 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing,  ONLY: SysSizingRunDone,NumSysSizInput, SysSizInput

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: AirLoopNum
  LOGICAL, INTENT (OUT) :: AirLoopWasSized
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  INTEGER :: ThisAirSysSizineInputLoop

  IF (SysSizingRunDone) THEN
    DO ThisAirSysSizineInputLoop = 1, NumSysSizInput
      IF (SysSizInput(ThisAirSysSizineInputLoop)%AirLoopNum == AirLoopNum) THEN
        AirLoopWasSized = .TRUE.
        EXIT
      ELSE
        AirLoopWasSized = .FALSE.
      ENDIF
    ENDDO
  ELSE
    AirLoopWasSized = .FALSE.
  ENDIF 

  RETURN

END SUBROUTINE CheckThisAirSystemForSizing

SUBROUTINE CheckZoneSizing(CompType,CompName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   October 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine is called when an "autosize" input is encountered in a component
          ! sizing routine to check that the zone sizing calculations have been done.

          ! METHODOLOGY EMPLOYED:
          ! Checks ZoneSizingRunDone flag. If false throws a fatal error.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: DoZoneSizing
  USE DataInterfaces, ONLY: ShowSevereError,ShowContinueError,ShowFatalError
  USE DataSizing,  ONLY: ZoneSizingRunDone,NumZoneSizingInput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType     ! Component Type (e.g. Chiller:Electric)
  CHARACTER(len=*), INTENT(IN) :: CompName     ! Component Name (e.g. Big Chiller)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF ( .NOT. ZoneSizingRunDone) THEN
    CALL ShowSevereError('For autosizing of ' // TRIM(CompType) // ' ' // TRIM(CompName) // &
                       ', a zone sizing run must be done.')
    IF (NumZoneSizingInput == 0) THEN
      CALL ShowContinueError('No "Sizing:Zone" objects were entered.')
    ENDIF
    IF (.not. DoZoneSizing) THEN
      CALL ShowContinueError('The "SimulationControl" object did not have the field "Do Zone Sizing Calculation" set to Yes.')
    ENDIF
    CALL ShowFatalError('Program terminates due to previously shown condition(s).')
  END IF

  RETURN

END SUBROUTINE CheckZoneSizing

SUBROUTINE CheckThisZoneForSizing(ZoneNum, ZoneWasSized)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Oct 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! utility routine to see if a particular zone has a Sizing:Zone object for it 
          ! and that sizing was done. 

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing, ONLY: ZoneSizingRunDone, NumZoneSizingInput, ZoneSizingInput

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ZoneNum ! zone index to be checked
  LOGICAL, INTENT(OUT) :: ZoneWasSized

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ThisSizingInput

  IF (ZoneSizingRunDone) THEN
    DO ThisSizingInput =  1, NumZoneSizingInput
      IF (ZoneSizingInput(ThisSizingInput)%ZoneNum == ZoneNum) THEN
        ZoneWasSized =  .TRUE.
        EXIT
      ELSE 
        ZoneWasSized =  .FALSE.
      ENDIF
    ENDDO
  ELSE
    ZoneWasSized =  .FALSE.
  ENDIF

  RETURN

END SUBROUTINE CheckThisZoneForSizing

SUBROUTINE ValidateComponent(CompType,CompName,IsNotOK,CallString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine can be called to validate the component type-name pairs that
          ! are so much a part of the EnergyPlus input.  The main drawback to this validation
          ! has been that the "GetInput" routine may not have been called and/or exists in
          ! another module from the one with the list.  This means that validation must be
          ! done later, perhaps after simulation has already started or perhaps raises an
          ! array bound error instead.

          ! METHODOLOGY EMPLOYED:
          ! Uses existing routines in InputProcessor.  GetObjectItemNum uses the "standard"
          ! convention of the Name of the item/object being the first Alpha Argument.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetObjectItemNum
  USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType     ! Component Type (e.g. Chiller:Electric)
  CHARACTER(len=*), INTENT(IN) :: CompName     ! Component Name (e.g. Big Chiller)
  LOGICAL, INTENT(OUT)         :: IsNotOK      ! .true. if this component pair is invalid
  CHARACTER(len=*), INTENT(IN) :: CallString   ! Context of this pair -- for error message

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ItemNum

  IsNotOK=.false.

  ItemNum=GetObjectItemNum(CompType,CompName)

  IF (ItemNum < 0) THEN
    CALL ShowSevereError('During '//TRIM(CallString)//' Input, Invalid Component Type input='//TRIM(CompType))
    CALL ShowContinueError('Component name='//TRIM(CompName))
    IsNotOK=.true.
  ELSEIF (ItemNum == 0) THEN
    CALL ShowSevereError('During '//TRIM(CallString)//' Input, Invalid Component Name input='//TRIM(CompName))
    CALL ShowContinueError('Component type='//TRIM(CompType))
    IsNotOK=.true.
  ENDIF

  RETURN

END SUBROUTINE ValidateComponent

SUBROUTINE CalcPassiveExteriorBaffleGap(SurfPtrARR, VentArea, Cv, Cd, HdeltaNPL, SolAbs, AbsExt, Tilt, AspRat, GapThick, &
                                  Roughness,QdotSource, TsBaffle, TaGap, HcGapRpt,  HrGapRpt,IscRpt , MdotVentRpt, &
                                  VdotWindRpt, VdotBouyRpt)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.T. Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       BG March 2007 outdoor conditions from surface for height-dependent conditions
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! model the effect of the a ventilated baffle covering the outside of a heat transfer surface.
          ! return calculated temperatures and certain intermediate values for reporting

          ! METHODOLOGY EMPLOYED:
          ! Heat balances on baffle and air space.
          ! Natural ventilation calculations use bouyancy and wind.

          ! REFERENCES:
          ! Nat. Vent. equations from ASHRAE HoF 2001 Chapt. 26

          ! USE STATEMENTS:

  USE DataPrecisionGlobals
  USE DataEnvironment , ONLY: SkyTemp, WindSpeedAt, SunIsUp, OutBaroPress,  IsRain
 ! USE DataLoopNode    , ONLY: Node
  USE Psychrometrics  , ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb, PsyWFnTdbTwbPb
  USE DataSurfaces    , ONLY: Surface
  USE DataHeatBalSurface, ONLY: TH
  USE DataHeatBalance , ONLY: Material, Construct, QRadSWOutIncident
  USE ConvectionCoefficients, ONLY: InitExteriorConvectionCoeff
  USE SolarCollectors,        ONLY: Collector
  USE DataGlobals,            ONLY: BeginEnvrnFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN), DIMENSION(:) :: SurfPtrARR  ! Array of indexes pointing to Surface structure in DataSurfaces
  REAL(r64), INTENT(IN)             :: VentArea    ! Area available for venting the gap [m2]
  REAL(r64), INTENT(IN)             :: Cv          ! Oriface coefficient for volume-based discharge, wind-driven [--]
  REAL(r64), INTENT(IN)             :: Cd          ! oriface coefficient for discharge,  bouyancy-driven [--]
  REAL(r64), INTENT(IN)             :: HdeltaNPL   ! Height difference from neutral pressure level [m]
  REAL(r64), INTENT(IN)             :: SolAbs      ! solar absorptivity of baffle [--]
  REAL(r64), INTENT(IN)             :: AbsExt      ! thermal absorptance/emittance of baffle material [--]
  REAL(r64), INTENT(IN)             :: Tilt        ! Tilt of gap [Degrees]
  REAL(r64), INTENT(IN)             :: AspRat      ! aspect ratio of gap  Height/gap [--]
  REAL(r64), INTENT(IN)             :: GapThick    ! Thickness of air space between baffle and underlying heat transfer surface
  INTEGER, INTENT(IN)               :: Roughness   ! Roughness index (1-6), see DataHeatBalance parameters
  REAL(r64), INTENT(IN)             :: QdotSource  ! Source/sink term, e.g. electricity exported from solar cell [W]
  REAL(r64), INTENT(INOUT)          :: TsBaffle    ! Temperature of baffle (both sides) use lagged value on input [C]
  REAL(r64), INTENT(INOUT)          :: TaGap       ! Temperature of air gap (assumed mixed) use lagged value on input [C]
  REAL(r64), INTENT(OUT), OPTIONAL  :: HcGapRpt       !
  REAL(r64), INTENT(OUT), OPTIONAL  :: HrGapRpt       !
  REAL(r64), INTENT(OUT), OPTIONAL  :: IscRpt
  REAL(r64), INTENT(OUT), OPTIONAL  :: MdotVentRpt
  REAL(r64), INTENT(OUT), OPTIONAL  :: VdotWindRpt
  REAL(r64), INTENT(OUT), OPTIONAL  :: VdotBouyRpt

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: g          = 9.807d0      ! gravitational constant (m/s**2)
  REAL(r64), PARAMETER  :: nu         = 15.66d-6   ! kinematic viscosity (m**2/s) for air at 300 K (Mills 1999 Heat Transfer)
  REAL(r64), PARAMETER  :: k          = 0.0267d0     ! thermal conductivity (W/m K) for air at 300 K (Mills 1999 Heat Transfer)
  REAL(r64), PARAMETER  :: Pr         = 0.71d0       ! Prandtl number for air
  REAL(r64), PARAMETER  :: Sigma      = 5.6697d-08 ! Stefan-Boltzmann constant
  REAL(r64), PARAMETER  :: KelvinConv = 273.15d0     ! Conversion from Celsius to Kelvin
          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ! following arrays are used to temporarily hold results from multiple underlying surfaces
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HSkyARR  !
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HGroundARR
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HAirARR  !
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HPlenARR
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HExtARR  !
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LocalWindArr

  ! local working variables
  REAL(r64)  :: RhoAir     ! density of air
  REAL(r64)  :: CpAir      ! specific heat of air
  REAL(r64)  :: Tamb       ! outdoor drybulb
  REAL(r64)  :: A          ! projected area of baffle from sum of underlying surfaces
  REAL(r64)  :: HcPlen     ! surface convection heat transfer coefficient for plenum surfaces
  INTEGER    :: thisSurf   ! do loop counter
  INTEGER    :: numSurfs   ! number of underlying HT surfaces associated with UTSC
  REAL(r64)  :: TmpTsBaf   ! baffle temperature
  INTEGER    :: SurfPtr    ! index of surface in main surface structure
  REAL(r64)  :: HMovInsul  ! dummy for call to InitExteriorConvectionCoeff
  REAL(r64)  :: HExt       ! dummy for call to InitExteriorConvectionCoeff
  INTEGER    :: ConstrNum  ! index of construction in main construction structure
  REAL(r64)  :: AbsThermSurf ! thermal emmittance of underlying wall.
  REAL(r64)  :: TsoK       ! underlying surface temperature in Kelvin
  REAL(r64)  :: TsBaffK    ! baffle temperature in Kelvin  (lagged)
  REAL(r64)  :: Vwind      ! localized, and area-weighted average for wind speed
  REAL(r64)  :: HrSky      ! radiation coeff for sky, area-weighted average
  REAL(r64)  :: HrGround   ! radiation coeff for ground, area-weighted average
  REAL(r64)  :: HrAtm      ! radiation coeff for air (bulk atmosphere), area-weighted average
  REAL(r64)  :: Isc        ! Incoming combined solar radiation, area-weighted average
  REAL(r64)  :: HrPlen     ! radiation coeff for plenum surfaces, area-weighted average
  REAL(r64)  :: Tso        ! temperature of underlying surface, area-weighted average
  REAL(r64)  :: TmeanK       ! average of surface temps , for Beta in Grashoff no.
  REAL(r64)  :: Gr          ! Grasshof number for natural convection calc
  REAL(r64)  :: VdotWind    ! volume flow rate of nat. vent due to wind
  REAL(r64)  :: VdotThermal ! Volume flow rate of nat. vent due to bouyancy
  REAL(r64)  :: VdotVent    ! total volume flow rate of nat vent
  REAL(r64)  :: MdotVent    ! total mass flow rate of nat vent
  REAL(r64)  :: NuPlen      ! Nusselt No. for plenum Gap
  REAL(r64)  :: LocalOutDryBulbTemp ! OutDryBulbTemp for here
  REAL(r64)  :: LocalWetBulbTemp ! OutWetBulbTemp for here
  REAL(r64)  :: LocalOutHumRat ! OutHumRat for here
  LOGICAL    :: ICSCollectorIsOn  =.FALSE.  ! ICS collector has OSCM on
  INTEGER    :: CollectorNum         ! current solar collector index
  REAL(r64)  :: ICSWaterTemp         ! ICS solar collector water temp
  REAL(r64)  :: ICSULossbottom       ! ICS solar collector bottom loss Conductance
  LOGICAL, SAVE  :: MyICSEnvrnFlag = .TRUE.  ! Local environment flag for ICS


  LocalOutDryBulbTemp = Sum(Surface(SurfPtrARR)%Area * Surface(SurfPtrARR)%OutDryBulbTemp) &
                        / Sum(Surface(SurfPtrARR)%Area)

  LocalWetBulbTemp = Sum(Surface(SurfPtrARR)%Area * Surface(SurfPtrARR)%OutWetBulbTemp) &
                        / Sum(Surface(SurfPtrARR)%Area)

  LocalOutHumRat   = PsyWFnTdbTwbPb(LocalOutDryBulbTemp,localWetBulbTemp,OutBaroPress,calledfrom='CalcPassiveExteriorBaffleGap')

  RhoAir     = PsyRhoAirFnPbTdbW(OutBaroPress,LocalOutDryBulbTemp,LocalOutHumRat,calledfrom='CalcPassiveExteriorBaffleGap')
  CpAir      = PsyCpAirFnWTdb(LocalOutHumRat,LocalOutDryBulbTemp,calledfrom='CalcPassiveExteriorBaffleGap')
  If (.NOT. IsRain) Then
    Tamb       = LocalOutDryBulbTemp
  ELSE ! when raining we use wetbulb not drybulb
    Tamb       = LocalWetBulbTemp
  ENDIF
  A          = Sum(Surface(SurfPtrARR)%Area)
  TmpTsBaf  = TsBaffle

  !loop through underlying surfaces and collect needed data
  NumSurfs =  size(SurfPtrARR)
  ALLOCATE(HSkyARR(NumSurfs))
  HSkyARR = 0.0d0
  ALLOCATE(HGroundARR(NumSurfs))
  HGroundARR = 0.0d0
  ALLOCATE(HAirARR(NumSurfs))
  HAirARR = 0.0d0
  ALLOCATE(LocalWindArr(NumSurfs))
  LocalWindArr = 0.0d0
  Allocate(HPlenARR(NumSurfs))
  HPlenARR = 0.0d0
  ALLOCATE(HExtARR(NumSurfs))
  HExtARR = 0.0d0

  Do thisSurf =1, NumSurfs
    SurfPtr = SurfPtrARR(thisSurf)
    ! Initializations for this surface
    HMovInsul     = 0.0d0
    LocalWindArr(thisSurf) = Surface(SurfPtr)%WindSpeed
    CALL InitExteriorConvectionCoeff(SurfPtr,HMovInsul,Roughness,AbsExt,TmpTsBaf, &
                                HExtARR(thisSurf),HSkyARR(thisSurf),HGroundARR(thisSurf),HAirARR(thisSurf))
    ConstrNum       = Surface(SurfPtr)%Construction
    AbsThermSurf = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal
    TsoK = TH(SurfPtr,1,1) + KelvinConv
    TsBaffK = TmpTsBaf + KelvinConv
    If (TsBaffK == TsoK) Then ! avoid divide by zero
       HPlenARR(thisSurf) = 0.0d0  ! no net heat transfer if same temperature
    ELSE
       HPlenARR(thisSurf) = Sigma*AbsExt*AbsThermSurf*(TsBaffK**4 - TsoK**4)/(TsBaffK - TsoK)
    ENDIF
    ! Added for ICS collector OSCM
    IF ( Surface(SurfPtr)%IsICS ) THEN
         ICSCollectorIsOn = .TRUE.
         CollectorNum = Surface(SurfPtr)%ICSPtr
    ENDIF
  ENDDO

  IF (ICSCollectorIsOn) THEN
   IF(BeginEnvrnFlag .AND. MyICSEnvrnFlag) THEN
     ICSULossbottom = 0.40d0
     ICSWaterTemp = 20.0d0
   ELSE
     ICSULossbottom = Collector(CollectorNum)%UbLoss
     ICSWaterTemp = Collector(CollectorNum)%TempOfWater
     MyICSEnvrnFlag = .FALSE.
   ENDIF
  ENDIF
  IF ( .NOT. BeginEnvrnFlag )THEN
      MyICSEnvrnFlag = .TRUE.
  ENDIF
  If (A == 0.0d0) then  ! should have been caught earlier

  ENDIF
  ! now figure area-weighted averages from underlying surfaces.
  Vwind = Sum(LocalWindArr*Surface(SurfPtrARR)%Area)  /A
  DEALLOCATE(LocalWindArr)
  HrSky = Sum(HSkyARR*Surface(SurfPtrARR)%Area)       /A
  DEALLOCATE(HSkyARR)
  HrGround = Sum(HGroundARR*Surface(SurfPtrARR)%Area) /A
  DEALLOCATE(HGroundARR)
  HrAtm    = Sum(HAirARR*Surface(SurfPtrARR)%Area)    /A
  DEALLOCATE(HAirARR)
  HrPlen   = Sum(HPlenARR*Surface(SurfPtrARR)%Area)   /A
  DEALLOCATE(HPlenARR)
  HExt     = Sum(HExtARR*Surface(SurfPtrARR)%Area)    /A
  DEALLOCATE(HExtARR)

  If (IsRain) HExt = 1000.0d0

  Tso      = SUM(TH((SurfPtrARR),1,1)*Surface(SurfPtrARR)%Area)          /A
  Isc      = SUm(QRadSWOutIncident(SurfPtrARR)*Surface(SurfPtrARR)%Area) /A

  TmeanK = 0.5d0*(TmpTsBaf + Tso)+ KelvinConv

  gr = g * GapThick**3 * ABS(Tso - TmpTsBaf) * RhoAir**2 / (TmeanK * nu**2)

  CALL PassiveGapNusseltNumber(AspRat,Tilt ,TmpTsBaf,Tso, Gr, NuPlen)  !intentionally switch Tso to Tsi

  HcPlen = NuPlen *(k / GapThick)

  ! now model natural ventilation of plenum gap.
  VdotWind = Cv * (VentArea / 2.0d0) * Vwind

  IF (TaGap > Tamb) Then
    VdotThermal = Cd * (VentArea / 2.d0)*(2.d0*g*HdeltaNPL*(TaGap - Tamb)/(TaGap + KelvinConv) )**0.5d0
  ELSEIF ( TaGap == Tamb) then
    VdotThermal = 0.0d0
  ELSE
    IF ((ABS(tilt) < 5.0d0) .OR. (ABS(Tilt - 180) < 5.0d0)) Then
      VdotThermal = 0.0d0   ! stable bouyancy situation
    ELSE
      VdotThermal = Cd * (VentArea / 2.d0)*(2.d0*g*HdeltaNPL*(Tamb - TaGap )/(Tamb+ KelvinConv))**0.5d0
    ENDIF
  ENDIF

  VdotVent = VdotWind + VdotThermal
  MdotVent = VdotVent * RhoAir

  !now calculate baffle temperature
  IF ( .NOT. ICSCollectorIsOn ) THEN
     TsBaffle = (Isc*SolAbs + HExt*Tamb + HrAtm*Tamb + HrSky*SkyTemp + HrGround*Tamb + HrPlen*Tso + &
                Hcplen*TaGap  + QdotSource) &
                /(HExt + HrAtm + HrSky + HrGround + Hrplen + Hcplen)
  ELSE

     TsBaffle = (ICSULossbottom*ICSWaterTemp + HrPlen*Tso +  Hcplen*TaGap  + QdotSource) &
              / (ICSULossbottom + Hrplen + Hcplen)
  ENDIF
  !now calculate gap air temperature

  TaGap = (HcPlen*A*Tso + MdotVent*CpAir*Tamb + HcPlen*A*TsBaffle) / (HcPlen*A + MdotVent*CpAir + HcPlen*A)

  IF (PRESENT(HcGapRpt))       HcGapRpt = Hcplen
  IF (PRESENT(HrGapRpt))       HrGapRpt = Hrplen
  IF (PRESENT(IscRpt))         IscRpt   = Isc
  IF (PRESENT(MdotVentRpt)) MdotVentRpt = MdotVent
  IF (PRESENT(VdotWindRpt)) VdotWindRpt = VdotWind
  IF (PRESENT(VdotBouyRpt)) VdotBouyRpt = VdotThermal

  RETURN

END SUBROUTINE CalcPassiveExteriorBaffleGap

   !****************************************************************************

SUBROUTINE PassiveGapNusseltNumber(AspRat,Tilt ,Tso,Tsi, Gr, gNu)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by B. Griffith from Fred Winkelmann's from NusseltNumber in WindowManager.f90
          !       DATE WRITTEN   September 2001
          !       MODIFIED       B. Griffith November 2004  (same models but slightly different for general use)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Finds the Nusselt number for air-filled gaps between isothermal solid layers.

          ! METHODOLOGY EMPLOYED:
          ! Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
          ! "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
          ! The equation numbers below correspond to those in the standard.

          ! REFERENCES:
          ! Window5 source code; ISO 15099

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, Only: DegToRadians

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  REAL(r64), INTENT(IN)                     :: AspRat            ! Aspect Ratio of Gap height to gap width
  REAL(r64), INTENT(IN)                     :: Tilt              ! Tilt of gap, degrees
  REAL(r64), INTENT(IN)                     :: tso               ! Temperature of gap surface closest to outside (K)
  REAL(r64), INTENT(IN)                     :: tsi               ! Temperature of gap surface closest to zone (K)
  REAL(r64), INTENT(IN)                     :: Gr                ! Gap gas Grashof number
  REAL(r64), INTENT(OUT)                    :: gNu               ! Gap gas Nusselt number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Pr = 0.71d0          ! Prandtl number for air

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
  REAL(r64)                :: Ra                             ! Rayleigh number
  REAL(r64)                :: gnu901,gnu902,gnu90,gnu601     ! Nusselt number temporary variables for
  REAL(r64)                :: gnu602,gnu60,gnu601a,gnua,gnub !  different tilt and Ra ranges
  REAL(r64)                :: cra,a,b,g,ang                  ! Temporary variables
  REAL(r64)                :: tiltr

  tiltr = Tilt * DegToRadians
  Ra = Gr * Pr

  if (ra > 2.0d6)  THen

  ! write(*,*)' error, outside range of Rayleigh number'
  endif


      if(ra <= 1.0d4) Then
        gnu901 = 1.d0 + 1.7596678d-10 * ra**2.2984755d0   ! eq. 51
      endif
      if(ra > 1.0d4 .and. ra <= 5.0d4) gnu901 =      0.028154d0      * ra**0.4134d0      ! eq. 50
      if(ra > 5.0d4)                   gnu901 =      0.0673838d0     * ra**(1.0d0/3.0d0)   ! eq. 49

      gnu902 = 0.242d0 * (ra/AspRat)**.272d0               ! eq. 52
      gnu90 = MAX(gnu901,gnu902)

      if(tso > tsi)then   ! window heated from above
        gnu = 1.0d0 + (gnu90-1.0d0)*sin(tiltr)                  ! eq. 53
      else                ! window heated from below
        if (Tilt >= 60.0d0) then
          g       = 0.5d0 * (1.0d0+(ra/3160.d0)**20.6d0)**(-0.1d0)    ! eq. 47
          gnu601a = 1.0d0 + (0.0936d0*(ra**0.314d0)/(1.0d0+g))**7   ! eq. 45
          gnu601  = gnu601a**0.142857d0

          ! For any aspect ratio
          gnu602  = (0.104d0+0.175d0/AspRat) * ra**0.283d0           ! eq. 46
          gnu60   = MAX(gnu601,gnu602)

          ! linear interpolation for layers inclined at angles between 60 and 90 deg
          gnu     = ((90.0d0-Tilt)*gnu60 + (Tilt-60.0d0)*gnu90)/30.0d0
        endif
        if (Tilt < 60.0d0) then                               ! eq. 42
          cra  = ra*cos(tiltr)
          a    = 1.0d0 - 1708.0d0/cra
          b    = (cra/5830.0d0)**0.33333d0-1.0d0
          gnua = (abs(a)+a)/2.0d0
          gnub = (abs(b)+b)/2.0d0
          ang  = 1708.0d0 * (sin(1.8d0*tiltr))**1.6d0
          gnu  = 1.0d0 + 1.44d0*gnua*(1.0d0-ang/cra) + gnub
        endif
      endif
      RETURN
END SUBROUTINE PassiveGapNusseltNumber

SUBROUTINE CalcBasinHeaterPower(Capacity,SchedulePtr,SetPointTemp,Power)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   Feb 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate basin heater power when the evaporative cooled equipment is not operating
          ! and outdoor air dry-bulb temperature is below the set-point

          ! METHODOLOGY EMPLOYED:
          ! Checks to see whether schedule for basin heater exists or not. If the schedule exists,
          ! the basin heater is operated for the schedule specified otherwise the heater runs
          ! for the entire simulation timestep whenever the outdoor temperature is below setpoint
          ! and water is not flowing through the evaporative cooled equipment.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataEnvironment, ONLY: OutDryBulbTemp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER  ,INTENT(IN)  :: SchedulePtr    ! Pointer to basin heater schedule
  REAL(r64),INTENT(IN)  :: Capacity       ! Basin heater capacity per degree C below setpoint (W/C)
  REAL(r64),INTENT(IN)  :: SetPointTemp   ! setpoint temperature for basin heater operation (C)
  REAL(r64),INTENT(OUT) :: Power          ! Basin heater power (W)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: BasinHeaterSch                  ! Schedule for basin heater operation
  Power = 0.0d0
  ! Operate basin heater anytime outdoor temperature is below setpoint and water is not flowing through the equipment
  ! IF schedule exists, basin heater performance can be scheduled OFF
  IF(SchedulePtr .GT. 0)THEN
    BasinHeaterSch     = GetCurrentScheduleValue(SchedulePtr)
    IF(Capacity .GT. 0.0d0 .AND. BasinHeaterSch .GT. 0.0d0)THEN
      Power = MAX(0.0d0,Capacity * (SetPointTemp-OutDryBulbTemp))
    END IF
  ELSE
  ! IF schedule does not exist, basin heater operates anytime outdoor dry-bulb temp is below setpoint
    IF(Capacity .GT. 0.0d0)THEN
      Power = MAX(0.0d0,Capacity * (SetPointTemp-OutDryBulbTemp))
    END IF
  END IF
RETURN
END SUBROUTINE CalcBasinHeaterPower

SUBROUTINE TestAirPathIntegrity(ErrFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine tests supply, return and overall air path integrity.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength, OutputFileBNDetails
  USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError
  USE DataLoopNode
  USE DataHVACGlobals, ONLY: NumPrimaryAirSys
  USE DataAirLoop, ONLY: AirToZoneNodeInfo

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          INTERFACE
            SUBROUTINE TestReturnAirPathIntegrity(ErrFound,ValRetAPaths)
              LOGICAL(KIND=4), INTENT(INOUT) :: ErrFound
              INTEGER(KIND=4) :: ValRetAPaths(:,:)
            END SUBROUTINE TestReturnAirPathIntegrity
          END INTERFACE
        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 29 07:54:46 2011
          INTERFACE
            SUBROUTINE TestSupplyAirPathIntegrity(ErrFound)
              LOGICAL(KIND=4), INTENT(INOUT) :: ErrFound
            END SUBROUTINE TestSupplyAirPathIntegrity
          END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: Loop2
  INTEGER :: Loop3
  INTEGER :: Count
  INTEGER :: TestNode
  LOGICAL :: ErrFlag
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ValRetAPaths
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: NumRAPNodes
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ValSupAPaths
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: NumSAPNodes


  ALLOCATE(NumSAPNodes(NumPrimaryAirSys,NumOfNodes))
  ALLOCATE(NumRAPNodes(NumPrimaryAirSys,NumOfNodes))
  ALLOCATE(ValRetAPaths(NumPrimaryAirSys,NumOfNodes))
  ALLOCATE(ValSupAPaths(NumPrimaryAirSys,NumOfNodes))
  NumSAPNodes=0
  NumRAPNodes=0
  ValRetAPaths=0
  ValSupAPaths=0

  CALL TestSupplyAirPathIntegrity(ErrFlag)
  IF (ErrFlag) ErrFound=.true.
  CALL TestReturnAirPathIntegrity(ErrFlag,ValRetAPaths)
  IF (ErrFlag) ErrFound=.true.

  ! Final tests, look for duplicate nodes
  DO Loop=1,NumPrimaryAirSys
    IF (ValRetAPaths(Loop,1) /= 0) CYCLE
    IF (AirToZoneNodeInfo(Loop)%NumReturnNodes <= 0) CYCLE
    ValRetAPaths(Loop,1)=AirToZoneNodeInfo(Loop)%ZoneEquipReturnNodeNum(1)
  ENDDO

  DO Loop=1,NumPrimaryAirSys
    DO Loop1=1,NumOfNodes
      TestNode=ValRetAPaths(Loop,Loop1)
      Count=0
      DO Loop2=1,NumPrimaryAirSys
        DO Loop3=1,NumOfNodes
          IF (Loop2 == Loop .and. Loop1 == Loop3) CYCLE  ! Don't count test node
          IF (ValRetAPaths(Loop2,Loop3) == 0) EXIT
          IF (ValRetAPaths(Loop2,Loop3) == TestNode) Count=Count+1
        ENDDO
      ENDDO
      IF (Count > 0) THEN
        CALL ShowSevereError('Duplicate Node detected in Return Air Paths')
        CALL ShowContinueError('Test Node='//TRIM(NodeID(TestNode)))
        CALL ShowContinueError('In Air Path='//TRIM(AirToZoneNodeInfo(Loop)%AirLoopName))
        ErrFound=.true.
      ENDIF
    ENDDO
  ENDDO

  DEALLOCATE(NumSAPNodes)
  DEALLOCATE(NumRAPNodes)
  DEALLOCATE(ValRetAPaths)
  DEALLOCATE(ValSupAPaths)

  RETURN

END SUBROUTINE TestAirPathIntegrity

SUBROUTINE TestSupplyAirPathIntegrity(ErrFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine tests supply air path integrity and displays the loop for each branch.
          ! Also, input and output nodes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength, OutputFileBNDetails
  USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError
  USE DataLoopNode
  USE SplitterComponent, ONLY: SplitterCond,NumSplitters,GetZoneSplitterInput=>GetSplitterInput
  USE DataZoneEquipment
  USE ZonePlenum
  USE DataAirLoop,       ONLY: AirToZoneNodeInfo
  USE DataHVACGlobals,   ONLY: NumPrimaryAirSys
  USE InputProcessor,    ONLY: SameString,MakeUPPERCase,GetNumObjectsFound

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  CHARACTER(len=MaxNameLength) &
                        :: AirPathNodeName ! Air Path Inlet Node Name
  CHARACTER(len=MaxNameLength) &
                        :: PrimaryAirLoopName ! Air Loop to which this supply air path is connected
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: FoundSupplyPlenum
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: FoundZoneSplitter
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: FoundNames
  INTEGER       :: NumErr                ! Error Counter
  INTEGER BCount
  INTEGER Found
  CHARACTER(len=20) ChrOut
  INTEGER Count1
  INTEGER Count2
  INTEGER WAirLoop


  ! Do by Paths
  CALL ShowMessage('Testing Individual Supply Air Path Integrity')
  ErrFound=.false.

  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,700)
  WRITE(ChrOut,*) NumSupplyAirPaths
  WRITE(OutputFileBNDetails,701) ' #Supply Air Paths,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,702)
  WRITE(OutputFileBNDetails,703)
  WRITE(OutputFileBNDetails,704)
  WRITE(OutputFileBNDetails,707)
  WRITE(OutputFileBNDetails,708)

 700 FORMAT('! <#Supply Air Paths>,<Number of Supply Air Paths>')
 701 FORMAT(A)
 702 FORMAT('! <Supply Air Path>,<Supply Air Path Count>,<Supply Air Path Name>,<AirLoopHVAC Name>')
 703 FORMAT('! <#Components on Supply Air Path>,<Number of Components>')
 704 FORMAT('! <Supply Air Path Component>,<Component Count>,<Component Type>,<Component Name>,', &
               '<AirLoopHVAC Name>')
 705 FORMAT('! <#Nodes on Supply Air Path>,<Number of Nodes>')
 706 FORMAT('! <Supply Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>')
 707 FORMAT('! <#Outlet Nodes on Supply Air Path Component>,<Number of Nodes>')
 708 FORMAT('! <Supply Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,', &
               '<Inlet Node Name>,<Outlet Node Name>,<AirLoopHVAC Name>')

  DO BCount=1,NumSupplyAirPaths

          ! Determine which air loop this supply air path is connected to
    Found = 0
    DO Count1 = 1, NumPrimaryAirSys
      PrimaryAirLoopName = TRIM(AirToZoneNodeInfo(Count1)%AirLoopName)
      Found = 0
      DO Count2=1,AirToZoneNodeInfo(Count1)%NumSupplyNodes
        IF (SupplyAirPath(Bcount)%InletNodeNum == AirToZoneNodeInfo(Count1)%ZoneEquipSupplyNodeNum(Count2)) Found = Count2
      ENDDO
      IF (Found /= 0) EXIT
    ENDDO
    IF (Found == 0) PrimaryAirLoopName = '**Unknown**'

    WRITE(ChrOut,*) BCount
    WRITE(OutputFileBNDetails,701) ' Supply Air Path,'//TRIM(ADJUSTL(ChrOut))//','// &
          TRIM(SupplyAirPath(BCount)%Name)//','// &
          TRIM(PrimaryAirLoopName)

    WRITE(ChrOut,*) SupplyAirPath(BCount)%NumOfComponents
    WRITE(OutputFileBNDetails,701) '   #Components on Supply Air Path,'//TRIM(ADJUSTL(ChrOut))

    AirPathNodeName=NodeID(SupplyAirPath(Bcount)%InletNodeNum)

    WAirLoop=0

    DO Count=1,SupplyAirPath(BCount)%NumOfComponents

      WRITE(ChrOut,*) Count
      ChrOut=ADJUSTL(ChrOut)
      WRITE(OutputFileBNDetails,701) '   Supply Air Path Component,'//TRIM(ChrOut)//','// &
            TRIM(SupplyAirPath(BCount)%ComponentType(Count))//','// &
            TRIM(SupplyAirPath(BCount)%ComponentName(Count))//','// &
            TRIM(PrimaryAirLoopName)

      SELECT CASE(MakeUPPERCase(SupplyAirPath(BCount)%ComponentType(Count)))

      CASE ('AIRLOOPHVAC:SUPPLYPLENUM')
        DO Count2=1,NumZoneSupplyPlenums
          IF (ZoneSupPlenCond(Count2)%ZonePlenumName /= SupplyAirPath(BCount)%ComponentName(Count)) CYCLE
          IF (Count == 1 .AND. AirPathNodeName /= NodeID(ZoneSupPlenCond(Count2)%InletNode)) THEN
            CALL ShowSevereError('Error in AirLoopHVAC:SupplyPath='//TRIM(SupplyAirPath(BCount)%Name))
            CALL ShowContinueError('For AirLoopHVAC:SupplyPlenum='//TRIM(ZoneSupPlenCond(Count2)%ZonePlenumName))
            CALL ShowContinueError('Expected inlet node (supply air path)='//TRIM(AirPathNodeName))
            CALL ShowContinueError('Encountered node name (supply plenum)='//TRIM(NodeID(ZoneSupPlenCond(Count2)%OutletNode(1))))
            ErrFound=.true.
            NumErr=NumErr+1
          ENDIF
          WRITE(ChrOut,*) ZoneSupPlenCond(Count2)%NumOutletNodes
          WRITE(OutputFileBNDetails,701) '     #Outlet Nodes on Supply Air Path Component,'//TRIM(ADJUSTL(ChrOut))
          DO Count1=1,ZoneSupPlenCond(Count2)%NumOutletNodes
            WRITE(ChrOut,*) Count1
          WRITE(OutputFileBNDetails,701) '     Supply Air Path Component Nodes,'//TRIM(ADJUSTL(ChrOut))//','//&
                TRIM(SupplyAirPath(BCount)%ComponentType(Count))//','// &
                TRIM(SupplyAirPath(BCount)%ComponentName(Count))//','// &
                TRIM(NodeID(ZoneSupPlenCond(Count2)%InletNode))//','// &
                TRIM(NodeID(ZoneSupPlenCond(Count2)%OutletNode(Count1)))//','// &
                TRIM(PrimaryAirLoopName)
          ENDDO
        ENDDO

      CASE ('AIRLOOPHVAC:ZONESPLITTER')
        DO Count2=1,NumSplitters
          IF (SplitterCond(Count2)%SplitterName /= SupplyAirPath(BCount)%ComponentName(Count)) CYCLE
          IF (Count == 1 .AND. AirPathNodeName /= NodeID(SplitterCond(Count2)%InletNode)) THEN
            CALL ShowSevereError('Error in AirLoopHVAC:SupplyPath='//TRIM(SupplyAirPath(BCount)%Name))
            CALL ShowContinueError('For AirLoopHVAC:ZoneSplitter='//TRIM(SplitterCond(Count2)%SplitterName))
            CALL ShowContinueError('Expected inlet node (supply air path)='//TRIM(AirPathNodeName))
            CALL ShowContinueError('Encountered node name (zone splitter)='//TRIM(NodeID(SplitterCond(Count2)%InletNode)))
            ErrFound=.true.
            NumErr=NumErr+1
          ENDIF
          WRITE(ChrOut,*) SplitterCond(Count2)%NumOutletNodes
          WRITE(OutputFileBNDetails,701) '     #Outlet Nodes on Supply Air Path Component,'//TRIM(ADJUSTL(ChrOut))
          DO Count1=1,SplitterCond(Count2)%NumOutletNodes
            WRITE(ChrOut,*) Count1
            WRITE(OutputFileBNDetails,701) '     Supply Air Path Component Nodes,'//TRIM(ADJUSTL(ChrOut))//','//&
                  TRIM(SupplyAirPath(BCount)%ComponentType(Count))//','// &
                  TRIM(SupplyAirPath(BCount)%ComponentName(Count))//','// &
                  TRIM(NodeID(SplitterCond(Count2)%InletNode))//','// &
                  TRIM(NodeID(SplitterCond(Count2)%OutletNode(Count1)))//','// &
                  TRIM(PrimaryAirLoopName)
          ENDDO
        ENDDO

      CASE DEFAULT
        CALL ShowSevereError('Invalid Component Type in Supply Air Path='//TRIM(SupplyAirPath(BCount)%ComponentType(Count)))
        ErrFound=.true.
        NumErr=NumErr+1

      END SELECT
    ENDDO

    IF (SupplyAirPath(BCount)%NumNodes > 0) THEN
      WRITE(OutputFileBNDetails,705)
      WRITE(OutputFileBNDetails,706)
      WRITE(ChrOut,*) SupplyAirPath(BCount)%NumNodes
      ChrOut=ADJUSTL(ChrOut)
      WRITE(OutputFileBNDetails,701) '#Nodes on Supply Air Path,'//TRIM(ChrOut)
      DO Count2=1,SupplyAirPath(BCount)%NumNodes
        WRITE(ChrOut,*) Count2
        ChrOut=ADJUSTL(ChrOut)
        IF (SupplyAirPath(BCount)%NodeType(Count2) == PathInlet) THEN
          WRITE(OutputFileBNDetails,701) '   Supply Air Path Node,Inlet Node,'//TRIM(ChrOut)//','// &
                                    TRIM(NodeID(SupplyAirPath(BCount)%Node(Count2)))//','// &
                                    TRIM(PrimaryAirLoopName)
        ELSE IF (SupplyAirPath(BCount)%NodeType(Count2) == Intermediate) THEN
          WRITE(OutputFileBNDetails,701) '   Supply Air Path Node,Through Node,'//TRIM(ChrOut)//','// &
                                    TRIM(NodeID(SupplyAirPath(BCount)%Node(Count2)))//','// &
                                    TRIM(PrimaryAirLoopName)
        ELSE IF (SupplyAirPath(BCount)%NodeType(Count2) == Outlet) THEN
          WRITE(OutputFileBNDetails,701) '   Supply Air Path Node,Outlet Node,'//TRIM(ChrOut)//','//  &
                                    TRIM(NodeID(SupplyAirPath(BCount)%Node(Count2)))//','// &
                                    TRIM(PrimaryAirLoopName)
        END IF
      ENDDO
    ENDIF
  ENDDO

  IF (NumSplitters == 0) THEN
    IF (GetNumObjectsFound('AirLoopHVAC:ZoneSplitter') > 0) THEN
      CALL GetZoneSplitterInput
    ENDIF
  ENDIF
  IF (NumZoneSupplyPlenums == 0 .and. NumZoneReturnPlenums == 0) THEN
    IF (GetNumObjectsFound('AirLoopHVAC:SupplyPlenum') > 0) THEN
      CALL GetZonePlenumInput
    ENDIF
  ENDIF

  ! now the reverse.  is every zone splitter and supply plenum on supply air path
  ALLOCATE(FoundSupplyPlenum(NumZoneSupplyPlenums))
  FoundSupplyPlenum=.false.
  ALLOCATE(FoundZoneSplitter(NumSplitters))
  FoundZoneSplitter=.false.
  ALLOCATE(FoundNames(NumZoneSupplyPlenums))
  FoundNames=' '
  DO Count1=1,NumZoneSupplyPlenums
    DO BCount=1,NumSupplyAirPaths
      DO Count=1,SupplyAirPath(BCount)%NumOfComponents
        IF (ZoneSupPlenCond(Count1)%ZonePlenumName /= SupplyAirPath(BCount)%ComponentName(Count) .or. &
            SupplyAirPath(BCount)%ComponentType(Count) /= 'AIRLOOPHVAC:SUPPLYPLENUM') CYCLE
        IF (FoundSupplyPlenum(Count1)) THEN
          CALL ShowSevereError('AirLoopHVAC:SupplyPlenum="'//TRIM(ZoneSupPlenCond(Count1)%ZonePlenumName)//  &
                '", duplicate entry.')
          CALL ShowContinueError('already exists on AirLoopHVAC:SupplyPath="'//trim(FoundNames(Count1))//'".')
          ErrFound=.true.
        ELSE
          ! record use
          FoundSupplyPlenum(Count1)=.true.
          FoundNames(Count1)=trim(SupplyAirPath(BCount)%Name)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(FoundNames)
  ALLOCATE(FoundNames(NumSplitters))
  FoundNames=' '
  DO Count1=1,NumSplitters
    DO BCount=1,NumSupplyAirPaths
      DO Count=1,SupplyAirPath(BCount)%NumOfComponents
        IF (SplitterCond(Count1)%SplitterName /= SupplyAirPath(BCount)%ComponentName(Count) .or.  &
            SupplyAirPath(BCount)%ComponentType(Count) /= 'AIRLOOPHVAC:ZONESPLITTER') CYCLE
        IF (FoundZoneSplitter(Count1)) THEN
          CALL ShowSevereError('AirLoopHVAC:ZoneSplitter="'//TRIM(SplitterCond(Count1)%SplitterName)//  &
                '", duplicate entry.')
          CALL ShowContinueError('already exists on AirLoopHVAC:SupplyPath="'//trim(FoundNames(Count1))//'".')
          ErrFound=.true.
        ELSE
          ! record use
          FoundZoneSplitter(Count1)=.true.
          FoundNames(Count1)=trim(SupplyAirPath(BCount)%Name)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(FoundNames)

  IF (.not. ALL(FoundSupplyPlenum)) THEN
    DO Count1=1,NumZoneSupplyPlenums
      IF (FoundSupplyPlenum(Count1)) CYCLE
      CALL ShowSevereError('AirLoopHVAC:SupplyPlenum="'//TRIM(ZoneSupPlenCond(Count1)%ZonePlenumName)//  &
         '", not found on any AirLoopHVAC:SupplyPath.')
!      ErrFound=.true.
    ENDDO
  ENDIF

  IF (.not. ALL(FoundZoneSplitter)) THEN
    DO Count1=1,NumSplitters
      IF (FoundZoneSplitter(Count1)) CYCLE
      CALL ShowSevereError('AirLoopHVAC:ZoneSplitter="'//TRIM(SplitterCond(Count1)%SplitterName)//  &
         '", not found on any AirLoopHVAC:SupplyPath.')
!      ErrFound=.true.
    ENDDO
  ENDIF

  DEALLOCATE(FoundSupplyPlenum)
  DEALLOCATE(FoundZoneSplitter)

  IF (ErrFound) THEN
    CALL ShowSevereError('Supply Air Path(s) did not pass integrity testing')
  ELSE
    CALL ShowMessage('All Supply Air Paths passed integrity testing')
  ENDIF

  RETURN

END SUBROUTINE TestSupplyAirPathIntegrity

SUBROUTINE TestReturnAirPathIntegrity(ErrFound,ValRetAPaths)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine tests return air path integrity and displays the loop for each branch.
          ! Also, input and output nodes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Return Air Path Validity Rules:
          !  Last component (zone mixer or zone return plenum) must resolve to
          !  be the outlet node for the return air path.  Inlets to this component must be outlets from
          !  previous components or "controlled zone outlets"?.
          !  (though converse not true -- each outlet in previous components do not
          !  have to be inlets on this item -- though they must be inputs somewhere in the stream).
          !
          !  If multiple components and no mixer, then a zone return plenums "outlet" must
          !  be represented as an inlet on a later plenum.  i.e. some zone return plenums are
          !  really acting as "mixers" in a sense.  These do not need to be stepwise in succession.
          !  Same caveat for inlets from previous item.
          !
          !  If multiple components and mixer, then prior condition (nested plenums) is allowed as long as
          !  those aren't duplicated as mixer inlets.  (i.e. zone rp 1 => zone rp 2 => zone mixer but
          !  zone rp 1 outlet should not also be inlet to mixer.
          !
          !  Can have (nzrp -- nested zone return plenum, pzrp -- parallel zone return plenum):
          !  nzrp 1 => nzrp 2 & pzrp 3 => zm (inlets from nzrp 2 and pzrp 3).  Or, likewise:
          !  pzrp 1 & pzrp 2 => zm => pzrp 3 (outlets from pzrp 1/2 are inlets to zm whose outlet is an
          !  inlet to pzrp 3 whose outlet is the outlet for the return air path.

          !  Cannot have duplicate nodes in the "inlet" stream?  (i.e. cannot have same zone feeding two independent
          !  plenums, for example).  Similarly, Same return plenum can't be in two air loops nor as two independent
          !  return plenums in one return air path.



          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength, OutputFileBNDetails
  USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError
  USE DataLoopNode
  USE DataZoneEquipment
  USE DataAirLoop, ONLY: AirToZoneNodeInfo
  USE ZonePlenum
  USE DataHVACGlobals,   ONLY: NumPrimaryAirSys
  USE InputProcessor,    ONLY: SameString,MakeUPPERCase,GetNumObjectsFound
  USE MixerComponent, ONLY: MixerCond,NumMixers,GetZoneMixerInput=>GetMixerInput
  USE PoweredInductionUnits, ONLY: PIUnitHasMixer
  USE HVACSingleDuctInduc, ONLY: FourPipeInductionUnitHasMixer

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrFound
  INTEGER, DIMENSION(:,:) :: ValRetAPaths

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER Count
  CHARACTER(len=MaxNameLength) &
                        :: AirPathNodeName ! Air Path Inlet Node Name
  CHARACTER(len=MaxNameLength) &
                        :: PrimaryAirLoopName ! Air Loop to which this return air path is connected
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: FoundReturnPlenum
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: FoundZoneMixer
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: FoundNames
  INTEGER       :: NumErr                ! Error Counter
  INTEGER BCount
  INTEGER Found
  CHARACTER(len=20) ChrOut
  INTEGER Count1
  INTEGER Count2
  LOGICAL HasMixer
  INTEGER MixerComp
  INTEGER, ALLOCATABLE, DIMENSION(:) :: AllNodes
  INTEGER MixerCount
  INTEGER Count3
  INTEGER NumComp
  INTEGER CountNodes
  INTEGER WAirLoop

  ! Do by Paths
  CALL ShowMessage('Testing Individual Return Air Path Integrity')
  ErrFound=.false.
  NumErr=0

  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,700)
  WRITE(ChrOut,*) NumReturnAirPaths
  WRITE(OutputFileBNDetails,701) ' #Return Air Paths,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,702)
  WRITE(OutputFileBNDetails,703)
  WRITE(OutputFileBNDetails,704)
  WRITE(OutputFileBNDetails,707)
  WRITE(OutputFileBNDetails,708)

 700 FORMAT('! <#Return Air Paths>,<Number of Return Air Paths>')
 701 FORMAT(A)
 702 FORMAT('! <Return Air Path>,<Return Air Path Count>,<Return Air Path Name>,<AirLoopHVAC Name>')
 703 FORMAT('! <#Components on Return Air Path>,<Number of Components>')
 704 FORMAT('! <Return Air Path Component>,<Component Count>,<Component Type>,<Component Name>,<AirLoopHVAC Name>')
 705 FORMAT('! <#Nodes on Return Air Path>,<Number of Nodes>')
 706 FORMAT('! <Return Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>')
 707 FORMAT('! <#Inlet Nodes on Return Air Path Component>,<Number of Nodes>')
 708 FORMAT('! <Return Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,', &
               '<Inlet Node Name>,<Outlet Node Name>,<AirLoopHVAC Name>')

  ALLOCATE(AllNodes(NumOfNodes))

  DO BCount=1,NumReturnAirPaths
!             Determine which air loop this supply air path is connected to
    Found = 0
    DO Count1 = 1, NumPrimaryAirSys
      PrimaryAirLoopName = TRIM(AirToZoneNodeInfo(Count1)%AirLoopName)
      Found = 0
      DO Count2=1,AirToZoneNodeInfo(Count1)%NumReturnNodes
        IF (ReturnAirPath(Bcount)%OutletNodeNum == AirToZoneNodeInfo(Count1)%ZoneEquipReturnNodeNum(Count2)) Found = Count2
      ENDDO
      IF (Found /= 0) EXIT
    ENDDO
    IF (Found == 0) PrimaryAirLoopName = '**Unknown**'

    WRITE(ChrOut,*) BCount
    WRITE(OutputFileBNDetails,701) ' Return Air Path,'//TRIM(ADJUSTL(ChrOut))//','// &
          TRIM(ReturnAirPath(BCount)%Name)//','// &
          TRIM(PrimaryAirLoopName)

    NumComp=ReturnAirPath(BCount)%NumOfComponents
    WRITE(ChrOut,*) NumComp
    WRITE(OutputFileBNDetails,701) '   #Components on Return Air Path,'//TRIM(ADJUSTL(ChrOut))

    AirPathNodeName=NodeID(ReturnAirPath(Bcount)%OutletNodeNum)

    HasMixer=.false.
    MixerComp=0
    MixerCount=0
    DO Count=1,NumComp
      WRITE(ChrOut,*) Count
      ChrOut=ADJUSTL(ChrOut)
      WRITE(OutputFileBNDetails,701) '   Return Air Path Component,'//TRIM(ChrOut)//','// &
            TRIM(ReturnAirPath(BCount)%ComponentType(Count))//','// &
            TRIM(ReturnAirPath(BCount)%ComponentName(Count))//','// &
            TRIM(PrimaryAirLoopName)

      IF (SameString(ReturnAirPath(BCount)%ComponentType(Count),'AirLoopHVAC:ZoneMixer')) THEN
        HasMixer=.true.
        MixerComp=Count
        MixerCount=MixerCount+1
      ENDIF
    ENDDO

    IF (MixerCount > 1) THEN
      CALL ShowSevereError('Too many zone mixers in Return Air Path='//TRIM(ReturnAirPath(BCount)%Name))
      ErrFound=.true.
      NumErr=NumErr+1
      CYCLE
    ENDIF

    AllNodes=0
    CountNodes=0
    WAirLoop=0

    IF (NumComp > 0) THEN

      SELECT CASE(MakeUPPERCase(ReturnAirPath(BCount)%ComponentType(NumComp)))

        CASE ('AIRLOOPHVAC:ZONEMIXER')
          DO Count2=1,NumMixers
            IF (ReturnAirPath(BCount)%ComponentName(NumComp) /= MixerCond(Count2)%MixerName) CYCLE
            ! Found correct Mixer (by name), check outlet node vs. return air path outlet node
            IF (AirPathNodeName /= NodeID(MixerCond(Count2)%OutletNode)) THEN
              CALL ShowSevereError('Error in Return Air Path='//TRIM(ReturnAirPath(BCount)%Name))
              CALL ShowContinueError('For Connector:Mixer='//ReturnAirPath(BCount)%ComponentName(NumComp))
              CALL ShowContinueError('Expected outlet node (return air path)='//TRIM(AirPathNodeName))
              CALL ShowContinueError('Encountered node name (mixer)='//TRIM(NodeID(MixerCond(Count2)%OutletNode)))
              ErrFound=.true.
              NumErr=NumErr+1
            ELSE
              CountNodes=CountNodes+1
              AllNodes(CountNodes)=MixerCond(Count2)%OutletNode
              DO Loop=1,MixerCond(Count2)%NumInletNodes
                CountNodes=CountNodes+1
                AllNodes(CountNodes)=MixerCond(Count2)%InletNode(Loop)
              ENDDO
            ENDIF
            WRITE(ChrOut,*) MixerCond(Count2)%NumInletNodes
            WRITE(OutputFileBNDetails,701) '     #Inlet Nodes on Return Air Path Component,'//TRIM(ADJUSTL(ChrOut))
            DO Count1=1,MixerCond(Count2)%NumInletNodes
              WRITE(ChrOut,*) Count1
              WRITE(OutputFileBNDetails,701) '     Return Air Path Component Nodes,'//TRIM(ADJUSTL(ChrOut))//','//&
                    TRIM(ReturnAirPath(BCount)%ComponentType(NumComp))//','// &
                    TRIM(ReturnAirPath(BCount)%ComponentName(NumComp))//','// &
                    TRIM(NodeID(MixerCond(Count2)%InletNode(Count1)))//','// &
                    TRIM(NodeID(MixerCond(Count2)%OutletNode))//','// &
                    TRIM(PrimaryAirLoopName)
            ENDDO
          ENDDO

        CASE ('AIRLOOPHVAC:RETURNPLENUM')
          DO Count2=1,NumZoneReturnPlenums
            IF (ReturnAirPath(BCount)%ComponentName(NumComp) /= ZoneRetPlenCond(Count2)%ZonePlenumName) CYCLE
            IF (AirPathNodeName /= NodeID(ZoneRetPlenCond(Count2)%OutletNode)) THEN
              CALL ShowSevereError('Error in Return Air Path='//TRIM(ReturnAirPath(BCount)%Name))
              CALL ShowContinueError('For AirLoopHVAC:ReturnPlenum='//ReturnAirPath(BCount)%ComponentName(NumComp))
              CALL ShowContinueError('Expected outlet node (return air path)='//TRIM(AirPathNodeName))
              CALL ShowContinueError('Encountered node name (zone return plenum)='//  &
                                     TRIM(NodeID(ZoneRetPlenCond(Count2)%OutletNode)))
              ErrFound=.true.
              NumErr=NumErr+1
            ELSE
              CountNodes=CountNodes+1
              AllNodes(CountNodes)=ZoneRetPlenCond(Count2)%OutletNode
              DO Loop=1,ZoneRetPlenCond(Count2)%NumInletNodes
                CountNodes=CountNodes+1
                AllNodes(CountNodes)=ZoneRetPlenCond(Count2)%InletNode(Loop)
              ENDDO
            ENDIF
            WRITE(ChrOut,*) ZoneRetPlenCond(Count2)%NumInletNodes
            WRITE(OutputFileBNDetails,701) '     #Inlet Nodes on Return Air Path Component,'//TRIM(ADJUSTL(ChrOut))
            DO Count1=1,ZoneRetPlenCond(Count2)%NumInletNodes
              WRITE(ChrOut,*) Count1
              WRITE(OutputFileBNDetails,701) '     Return Air Path Component Nodes,'//TRIM(ADJUSTL(ChrOut))//','//&
                    TRIM(ReturnAirPath(BCount)%ComponentType(NumComp))//','// &
                    TRIM(ReturnAirPath(BCount)%ComponentName(NumComp))//','// &
                    TRIM(NodeID(ZoneRetPlenCond(Count2)%InletNode(Count1)))//','// &
                    TRIM(NodeID(ZoneRetPlenCond(Count2)%OutletNode))//','// &
                    TRIM(PrimaryAirLoopName)
            ENDDO
          ENDDO

        CASE DEFAULT
          ! This already validated in GetReturnAirPath

      END SELECT

    ENDIF

    IF (NumComp > 1) THEN
      DO Count3=1,NumComp-1
        SELECT CASE(MakeUPPERCase(ReturnAirPath(BCount)%ComponentType(Count3)))

          CASE ('AIRLOOPHVAC:ZONEMIXER')
            DO Count2=1,NumMixers
              IF (ReturnAirPath(BCount)%ComponentName(Count3) /= MixerCond(Count2)%MixerName) CYCLE
              DO Loop=1,MixerCond(Count2)%NumInletNodes
                CountNodes=CountNodes+1
                AllNodes(CountNodes)=MixerCond(Count2)%InletNode(Loop)
              ENDDO
            ENDDO

          CASE ('AIRLOOPHVAC:RETURNPLENUM')
            DO Count2=1,NumZoneReturnPlenums
              IF (ReturnAirPath(BCount)%ComponentName(Count3) /= ZoneRetPlenCond(Count2)%ZonePlenumName) CYCLE
              DO Loop=1,ZoneRetPlenCond(Count2)%NumInletNodes
                CountNodes=CountNodes+1
                AllNodes(CountNodes)=ZoneRetPlenCond(Count2)%InletNode(Loop)
              ENDDO
            ENDDO

          CASE DEFAULT
            ! This already validated in GetReturnAirPath

        END SELECT

      ENDDO
    ENDIF
    IF (CountNodes > 0) THEN
      WRITE(OutputFileBNDetails,705)
      WRITE(OutputFileBNDetails,706)
      WRITE(ChrOut,*) CountNodes
      ChrOut=ADJUSTL(ChrOut)
      WRITE(OutputFileBNDetails,701) '   #Nodes on Return Air Path,'//TRIM(ChrOut)
      DO Count2=1,CountNodes
        WRITE(ChrOut,*) Count2
        ChrOut=ADJUSTL(ChrOut)
        IF (Count2 == 1) THEN
          WRITE(OutputFileBNDetails,701) '   Return Air Path Node,Outlet Node,'//TRIM(ChrOut)//','// &
                TRIM(NodeID(AllNodes(Count2)))//','// &
                TRIM(PrimaryAirLoopName)
        ELSE
          WRITE(OutputFileBNDetails,701) '   Return Air Path Node,Inlet Node,'//TRIM(ChrOut)//','// &
                TRIM(NodeID(AllNodes(Count2)))//','// &
                TRIM(PrimaryAirLoopName)
        ENDIF
      ENDDO
    ENDIF
    ! Determine Air Loop this Return Air Path is on
    DO Count2=1,NumPrimaryAirSys
     IF (AirToZoneNodeInfo(Count2)%NumReturnNodes > 0) THEN
      IF (AllNodes(1) == AirToZoneNodeInfo(Count2)%ZoneEquipReturnNodeNum(1)) THEN
        WAirLoop=Count2
        ValRetAPaths(WAirLoop,:)=0
        ValRetAPaths(WAirLoop,1:CountNodes)=AllNodes(1:CountNodes)
        EXIT
      ENDIF
     ELSE
        CALL ShowWarningError('TestReturnAirPathIntegrity: Air Loop has no Zone Equipment Return Node='//  &
              TRIM(AirToZoneNodeInfo(Count2)%AirLoopName))
     ENDIF
    ENDDO

  ENDDO

  DEALLOCATE(AllNodes)

  IF (NumMixers == 0) THEN
    IF (GetNumObjectsFound('AirLoopHVAC:ZoneMixer') > 0) THEN
      CALL GetZoneMixerInput
    ENDIF
  ENDIF
  IF (NumZoneSupplyPlenums == 0 .and. NumZoneReturnPlenums == 0) THEN
    IF (GetNumObjectsFound('AirLoopHVAC:ReturnPlenum') > 0) THEN
      CALL GetZonePlenumInput
    ENDIF
  ENDIF

  ! now the reverse.  is every zone Mixer and Return plenum on Return air path
  ALLOCATE(FoundReturnPlenum(NumZoneReturnPlenums))
  FoundReturnPlenum=.false.
  ALLOCATE(FoundZoneMixer(NumMixers))
  FoundZoneMixer=.false.
  ALLOCATE(FoundNames(NumZoneReturnPlenums))
  FoundNames=' '
  DO Count1=1,NumZoneReturnPlenums
    DO BCount=1,NumReturnAirPaths
      DO Count=1,ReturnAirPath(BCount)%NumOfComponents
        IF (ZoneRetPlenCond(Count1)%ZonePlenumName /= ReturnAirPath(BCount)%ComponentName(Count) .or. &
            ReturnAirPath(BCount)%ComponentType(Count) /= 'AIRLOOPHVAC:RETURNPLENUM') CYCLE
        IF (FoundReturnPlenum(Count1)) THEN
          CALL ShowSevereError('AirLoopHVAC:ReturnPlenum="'//TRIM(ZoneRetPlenCond(Count1)%ZonePlenumName)//  &
                '", duplicate entry.')
          CALL ShowContinueError('already exists on AirLoopHVAC:ReturnPath="'//trim(FoundNames(Count1))//'".')
          ErrFound=.true.
        ELSE
          ! record use
          FoundReturnPlenum(Count1)=.true.
          FoundNames(Count1)=trim(ReturnAirPath(BCount)%Name)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(FoundNames)
  ALLOCATE(FoundNames(NumMixers))
  FoundNames=' '
  DO Count1=1,NumMixers
    DO BCount=1,NumReturnAirPaths
      DO Count=1,ReturnAirPath(BCount)%NumOfComponents
        IF (MixerCond(Count1)%MixerName /= ReturnAirPath(BCount)%ComponentName(Count) .or.  &
            ReturnAirPath(BCount)%ComponentType(Count) /= 'AIRLOOPHVAC:ZONEMIXER') CYCLE
        IF (FoundZoneMixer(Count1)) THEN
          CALL ShowSevereError('AirLoopHVAC:ZoneMixer="'//TRIM(MixerCond(Count1)%MixerName)//  &
                '", duplicate entry.')
          CALL ShowContinueError('already exists on AirLoopHVAC:ReturnPath="'//trim(FoundNames(Count1))//'".')
          ErrFound=.true.
        ELSE
          ! record use
          FoundZoneMixer(Count1)=.true.
          FoundNames(Count1)=trim(ReturnAirPath(BCount)%Name)
        ENDIF
      ENDDO
    ENDDO
    IF (.not. FoundZoneMixer(Count1)) THEN  ! could be as child on other items
      ! PIU Units
      IF (PIUnitHasMixer(MixerCond(Count1)%MixerName)) FoundZoneMixer(Count1)=.true.
    ENDIF
    IF (.not. FoundZoneMixer(Count1)) THEN  ! could be as child on other items
      ! fourPipeInduction units
      IF (FourPipeInductionUnitHasMixer(MixerCond(Count1)%MixerName)) FoundZoneMixer(Count1)=.true.
    ENDIF
  ENDDO
  DEALLOCATE(FoundNames)

  IF (.not. ALL(FoundReturnPlenum)) THEN
    DO Count1=1,NumZoneReturnPlenums
      IF (FoundReturnPlenum(Count1)) CYCLE
      CALL ShowSevereError('AirLoopHVAC:ReturnPlenum="'//TRIM(ZoneRetPlenCond(Count1)%ZonePlenumName)//  &
         '", not found on any AirLoopHVAC:ReturnPath.')
!      ErrFound=.true.
    ENDDO
  ENDIF

  IF (.not. ALL(FoundZoneMixer)) THEN
    DO Count1=1,NumMixers
      IF (FoundZoneMixer(Count1)) CYCLE
      CALL ShowSevereError('AirLoopHVAC:ZoneMixer="'//TRIM(MixerCond(Count1)%MixerName)//  &
         '", not found on any AirLoopHVAC:ReturnPath, AirTerminal:SingleDuct:SeriesPIU:Reheat,')
      CALL ShowContinueError('AirTerminal:SingleDuct:ParallelPIU:Reheat or '//  &
         'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction.')
!      ErrFound=.true.
    ENDDO
  ENDIF

  DEALLOCATE(FoundReturnPlenum)
  DEALLOCATE(FoundZoneMixer)

  IF (ErrFound) THEN
    CALL ShowSevereError('Return Air Path(s) did not pass integrity testing')
  ELSE
    CALL ShowMessage('All Return Air Paths passed integrity testing')
  ENDIF

  RETURN

END SUBROUTINE TestReturnAirPathIntegrity

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

