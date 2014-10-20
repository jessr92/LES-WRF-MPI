program esmfExampleCoupling
use ESMF
use esmfHelpers
use componentOne
use componentTwo
use couplerComponent
implicit none

call main()

contains

subroutine main()
    implicit none
    integer :: rc, npets, pet_id
    type(ESMF_VM) :: vm
    type(ESMF_GridComp) :: componentOne, componentTwo
    type(ESMF_CplComp) :: couplerComponent
    type(ESMF_State) :: importStateOne, importStateTwo
    type(ESMF_State) :: exportStateOne, exportStateTwo
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: startTime, endTime
    type(ESMF_TimeInterval) :: timeInterval
    ! Initialise the ESMF Framework
    call ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                         defaultlogfilename="esmfExampleCouplingLog", &
                         logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    call checkRC(rc, "Error occurred initialising ESMF")
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    call checkRC(rc, "Error getting VM information")
    write(*,*), npets, pet_id
    ! Create the required components
    componentOne = ESMF_GridCompCreate(name="Component One", petList=(/0, 2/), rc=rc)
    call checkRC(rc, "Error occurred creating component one")
    componentTwo = ESMF_GridCompCreate(name="Component Two", petList=(/1, 3/), rc=rc)
    call checkRC(rc, "Error occurred creating component two")
    couplerComponent = ESMF_CplCompCreate(name="Coupler Component", petList=(/0/), rc=rc)
    call checkRC(rc, "Error occurred creating coupler component")
    ! Create the import/export states
    importStateOne = ESMF_StateCreate(name="Component One Import State", &
                                      stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    call checkRC(rc, "Error occurred creating component one's import state")
    importStateTwo = ESMF_StateCreate(name="Component Two Import State", &
                                      stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    call checkRC(rc, "Error occurred creating component two's import state")
    exportStateOne = ESMF_StateCreate(name="Component One Export State", &
                                      stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    call checkRC(rc, "Error occurred creating component one's export state")
    exportStateTwo = ESMF_StateCreate(name="Component Two Export State", &
                                      stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    call checkRC(rc, "Error occurred creating component two's export state")
    ! Setup init/run/finalise methods for each component
    call ESMF_GridCompSetVM(componentOne, userRoutine=componentOneSetVM, rc=rc)
    call checkRC(rc, "Error occurred while setting component one's VM")
    call ESMF_GridCompSetServices(componentOne, componentOneSetServices, rc=rc)
    call checkRC(rc, "Error occurred while setting component one's services")
    call ESMF_GridCompSetVM(componentTwo, userRoutine=componentTwoSetVM, rc=rc)
    call checkRC(rc, "Error occurred while settting component two's VM")
    call ESMF_GridCompSetServices(componentTwo, componentTwoSetServices, rc=rc)
    call checkRC(rc, "Error occured while setting component two's services")
    call ESMF_CplCompSetVM(couplerComponent, couplerSetVM, rc=rc)
    call checkRC(rc, "Error occurred while setting coupler component's VM")
    call ESMF_CplCompSetServices(couplerComponent, couplerSetServices, rc=rc)
    call checkRC(rc, "Error occurred while setting coupler component's services")
    ! Initialise clock
    call ESMF_TimeSet(startTime, yy=2014, mm=1, dd=1, h=0, m=0, s=0, rc=rc)
    call checkRC(rc, "Error occurred while setting start time")
    call ESMF_TimeSet(endTime, yy=2014, mm=1, dd=2, h=0, m=0, s=0, rc=rc)
    call checkRC(rc, "Error occurred while setting stop time")
    call ESMF_TimeIntervalSet(timeInterval, s=3600, rc=rc)
    call checkRC(rc, "Error occurred while setting time interval")
    clock = ESMF_ClockCreate(timeStep=timeInterval, startTime=startTime, &
                             stopTime=endTime, rc=rc)
    call checkRC(rc, "Error occurred while creating clock")
    ! Initialise Components
    call ESMF_GridCompInitialize(componentOne, importstate=importStateOne, &
                                 exportstate=exportStateOne, clock=clock, rc=rc)
    call checkRC(rc, "Error occurred while calling init for Component One")
    call ESMF_GridCompInitialize(componentTwo, importstate=importStateTwo, &
                                 exportstate=exportStateTwo, clock=clock, rc=rc)
    call checkRC(rc, "Error occurred while calling init for Component Two")
    call ESMF_CplCompInitialize(couplerComponent, importstate=importStateTwo, &
                                 exportstate=exportStateTwo, clock=clock, rc=rc)
    call checkRC(rc, "Error occurred while calling init for Coupler Component")
    ! Clock stepping loop
    do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))
        call checkRC(rc, "Error occurred while checking if we're at clock stop time")
        ! Component one and two run in parallel
        call ESMF_GridCompRun(componentOne, importstate=importStateOne, &
                              exportstate=exportStateOne, &
                              syncFlag=ESMF_SYNC_NONBLOCKING, clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running component one")
        call ESMF_GridCompRun(componentTwo, importstate=importstatetwo, &
                              exportstate=exportStateTwo, &
                              syncFlag=ESMF_SYNC_NONBLOCKING, clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running component two")
        call ESMF_GridCompWait(componentOne, rc=rc)
        call checkRC(rc, "Error occurred while waiting for component one")
        call ESMF_GridCompWait(componentTwo, rc=rc)
        call checkRC(rc, "Error occurred while waiting for component two")
        ! Transfer component one's export data into component two
        call ESMF_CplCompRun(couplerComponent, importstate=exportStateOne, &
                              exportstate=importStateTwo, phase=1, &
                              clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running coupler for one->two")
        ! Transfer component two's export data into component one
        call ESMF_CplCompRun(couplerComponent, importstate=exportStateTwo, &
                             exportstate=exportStateTwo, phase=2, &
                             clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running coupler for two->one")
        call ESMF_ClockAdvance(clock, rc=rc)
        call checkRC(rc, "Error occurred while advancing the clock")
    end do
    ! Finalise Components
    call ESMF_GridCompFinalize(componentOne, rc=rc)
    call checkRC(rc, "Error occurred while finalising Component One")
    call ESMF_GridCompFinalize(componentTwo, rc=rc)
    call checkRC(rc, "Error occurred while finalising Component Two")
    call ESMF_CplCompFinalize(couplerComponent, rc=rc)
    call checkRC(rc, "Error occurred while finalising Coupler Component")
    ! Cleanup Components
    call ESMF_GridCompDestroy(componentOne, rc=rc)
    call checkRC(rc, "Error occurred while destroying Component One")
    call ESMF_GridCompDestroy(componentTwo, rc=rc)
    call checkRC(rc, "Error occurred while destroying Component Two")
    call ESMF_CplCompDestroy(couplerComponent, rc=rc)
    call checkRC(rc, "Error occurred while destroying Coupler Component")
    call ESMF_StateDestroy(importStateOne, rc=rc)
    call checkRC(rc, "Error occurred while destroying Import State One")
    call ESMF_StateDestroy(importStateTwo, rc=rc)
    call checkRC(rc, "Error occurred while destroying Import State Two")
    call ESMF_StateDestroy(exportStateOne, rc=rc)
    call checkRC(rc, "Error occurred while destroying Export State One")
    call ESMF_StateDestroy(exportStateTwo, rc=rc)
    call checkRC(rc, "Error occurred while destroying Export State Two")
    call ESMF_ClockDestroy(clock, rc=rc)
    call checkRC(rc, "Error occurred while destroying Clock")
    ! Finalise the ESMF Framework
    call ESMF_LogWrite("Completed!", ESMF_LOGMSG_INFO)
    call ESMF_Finalize()
end subroutine main

end program esmfExampleCoupling
