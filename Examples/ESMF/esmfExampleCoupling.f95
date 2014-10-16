program esmfExampleCoupling
use ESMF
use esmfHelpers
use componentOne, only : componentOneSetServices
use componentTwo, only : componentTwoSetServices
use couplerComponent, only : couplerSetServices
implicit none

call main()

contains

subroutine main()
    implicit none
    integer :: rc
    type(ESMF_GridComp) :: componentOne, componentTwo
    type(ESMF_CplComp) :: couplerComponent
    type(ESMF_State) :: importStateOne, importStateTwo, exportStateOne, exportStateTwo
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: startTime, endTime
    type(ESMF_TimeInterval) :: timeInterval
    ! Initialise the ESMF Framework
    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, & 
                         logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    call checkRC(rc, "Error occurred initialising ESMF")
    ! Create the required components
    componentOne = ESMF_GridCompCreate(name="Component One", rc=rc)
    call checkRC(rc, "Error occurred creating component one")
    componentTwo = ESMF_GridCompCreate(name="Component Two", rc=rc)
    call checkRC(rc, "Error occurred creating component two")
    couplerComponent = ESMF_CplCompCreate(name="Coupler Component", rc=rc)
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
    call ESMF_GridCompSetServices(componentOne, componentOneSetServices, rc=rc)
    call ESMF_GridCompSetServices(componentTwo, componentTwoSetServices, rc=rc)
    call ESMF_CplCompSetServices(couplerComponent, couplerSetServices, rc=rc)
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
        call ESMF_GridCompRun(componentOne, importstate=importStateOne, &
                              exportstate=exportStateOne, clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running component one")
        call ESMF_CplCompRun(couplerComponent, importstate=exportStateOne, &
                              exportstate=importStateTwo, clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running coupler for one->two")
        call ESMF_GridCompRun(componentTwo, importstate=importstatetwo, &
                              exportstate=exportStateTwo, clock=clock, rc=rc)
        call checkRC(rc, "Error occurred while running component two")
        call ESMF_CplCompRun(couplerComponent, importstate=exportStateTwo, &
                             exportstate=exportStateTwo, clock=clock, rc=rc)
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
    call ESMF_Finalize()
    write(*, "(A)") "Completed!"
end subroutine main

end program esmfExampleCoupling
