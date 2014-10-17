module componenttwo
use ESMF
use esmfHelpers
implicit none

public :: componentTwoSetServices, componentTwoSetVM

private
    character (len=*), parameter :: componentName = "Component Two"

contains

subroutine componentTwoSetVM(component, rc)
    type(ESMF_GridComp) :: component
    integer, intent(out) :: rc
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
    call ESMF_VMGetGlobal(vm, rc=rc)
    call checkRC(rc, "Error occurred while trying to get global VM for "//componentName)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    call checkRC(rc, "Error occurred while trying to get pthreads enabled info for "//componentName)
    if (pthreadsEnabled) then
        call ESMF_GridCompSetVMMinThreads(component, rc=rc)
        call checkRC(rc, "Error occurred while trying to set minimum threads for "//componentName)
    endif
    rc=ESMF_SUCCESS
end subroutine componentTwoSetVM

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
    integer :: petCount
    type(ESMF_VM) :: vm
    call ESMF_VMGetGlobal(vm, rc=rc)
    call checkRC(rc, "Error occurred while trying to get global VM for "//componentName)
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    call checkRC(rc, "Error occurred while trying to get pet count info for "//componentName)
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
