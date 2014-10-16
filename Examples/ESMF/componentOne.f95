module componentone
use ESMF
implicit none

public :: componentOneSetServices

private
    character (len=*), parameter :: componentName = "Component One"

contains

subroutine componentOneSetServices(component, rc)
    type(ESMF_GridComp) :: component
    integer :: rc
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_INITIALIZE, componentOneInit, rc=rc)
    call checkRC(rc, "Error occurred setting init method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_RUN, componentOneRun, rc=rc)
    call checkRC(rc, "Error occurred setting run method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_FINALIZE, componentOneFinal, rc=rc)
    call checkRC(rc, "Error occurred setting finalize method for "//componentName)
    rc = ESMF_SUCCESS
end subroutine componentOneSetServices

subroutine componentOneInit(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Component One Init subroutine called"
end subroutine componentOneInit

subroutine componentOneRun(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Component One Run subroutine called"
end subroutine componentOneRun

subroutine componentOneFinal(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Component One Final subroutine called"
end subroutine componentOneFinal

end module componentone
