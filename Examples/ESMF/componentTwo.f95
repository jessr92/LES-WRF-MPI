module componenttwo
use ESMF
use esmfHelpers
implicit none

public :: componentTwoSetServices

private
    character (len=*), parameter :: componentName = "Component Two"

contains

subroutine componentTwoSetServices(component, rc)
    type(ESMF_GridComp) :: component
    integer, intent(out) :: rc
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_INITIALIZE, componentTwoInit, rc=rc)
    call checkRC(rc, "Error occurred setting init method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_RUN, componentTwoRun, rc=rc)
    call checkRC(rc, "Error occurred setting run method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_FINALIZE, componentTwoFinal, rc=rc)
    call checkRC(rc, "Error occurred setting finalize method for "//componentName)
end subroutine componentTwoSetServices

subroutine componentTwoInit(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Component Two Init subroutine called", ESMF_LOGMSG_INFO)
end subroutine componentTwoInit

subroutine componentTwoRun(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Component Two Run subroutine called", ESMF_LOGMSG_INFO)
end subroutine componentTwoRun

subroutine componentTwoFinal(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Component Two Final subroutine called", ESMF_LOGMSG_INFO)
end subroutine componentTwoFinal

end module componenttwo
