program esmfExampleCoupling
use ESMF
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
    call componentOneInit1(componentOne, importStateOne, exportStateOne, clock, rc)
    call ESMF_GridCompInitialize(componentOne, importstate=importStateOne, &
                                 exportstate=exportStateOne, clock=clock, &
                                 phase=1, rc=rc)
    call checkRC(rc, "Error occurred while calling init1 for Component One")
    call ESMF_GridCompInitialize(componentOne, importstate=importStateOne, &
                                 exportstate=exportStateOne, clock=clock, &
                                 phase=2, rc=rc)
    call checkRC(rc, "Error occurred while calling init2 for Component One")
    call ESMF_GridCompInitialize(componentTwo, importstate=importStateTwo, &
                                 exportstate=exportStateTwo, clock=clock, &
                                 phase=1, rc=rc)
    call checkRC(rc, "Error occurred while calling init1 for Component Two")
    call ESMF_GridCompInitialize(componentTwo, importstate=importStateTwo, &
                                 exportstate=exportStateTwo, clock=clock, &
                                 phase=2, rc=rc)
    call checkRC(rc, "Error occurred while calling init2 for Component Two")
    ! Finalise the ESMF Framework
    call ESMF_Finalize()
    write(*, "(A)") "Completed!"
end subroutine main

subroutine checkRC(rc, message)
    implicit none
    integer, intent(in) :: rc
    character (len=*), intent(in), optional :: message
    if (rc /= ESMF_SUCCESS) then
        if (present(message)) then
            write(*, "(A)") message
        else
            write(*, "(A)") "Unspecified error occurred."
        end if
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if    
end subroutine checkRC

end program esmfExampleCoupling
