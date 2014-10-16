module couplerComponent
use ESMF
use esmfHelpers
implicit none

public :: couplerSetServices

private
    character (len=*), parameter :: componentName = "Coupler Component"

contains

subroutine couplerSetServices(component, rc)
    type(ESMF_CplComp) :: component
    integer :: rc
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_INITIALIZE, couplerInit, rc=rc)
    call checkRC(rc, "Error occurred setting init method for "//componentName)
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_RUN, couplerRun, rc=rc)
    call checkRC(rc, "Error occurred setting run method for "//componentName)
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_FINALIZE, couplerFinal, rc=rc)
    call checkRC(rc, "Error occurred setting finalize method for "//componentName)
end subroutine couplerSetServices

subroutine couplerInit(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Coupler Init subroutine called"
end subroutine couplerInit

subroutine couplerRun(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Coupler Run subroutine called"
end subroutine couplerRun

subroutine couplerFinal(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Coupler Final subroutine called"
end subroutine couplerFinal

end module couplerComponent
