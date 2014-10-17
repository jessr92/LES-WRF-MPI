module couplerComponent
use ESMF
use esmfHelpers
implicit none

public :: couplerSetServices, couplerSetVM

private
    character (len=*), parameter :: componentName = "Coupler Component"

contains

subroutine couplerSetVM(component, rc)
    type(ESMF_CplComp) :: component
    integer, intent(out) :: rc
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
    call ESMF_VMGetGlobal(vm, rc=rc)
    call checkRC(rc, "Error occurred while trying to get global VM for "//componentName)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    call checkRC(rc, "Error occurred while trying to get pthreads enabled info for "//componentName)
    if (pthreadsEnabled) then
        call ESMF_CplCompSetVMMinThreads(component, rc=rc)
        call checkRC(rc, "Error occurred while trying to set minimum threads for "//componentName)
    endif
    rc=ESMF_SUCCESS
end subroutine couplerSetVM

subroutine couplerSetServices(component, rc)
    type(ESMF_CplComp) :: component
    integer, intent(out) :: rc
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_INITIALIZE, couplerInit, rc=rc)
    call checkRC(rc, "Error occurred setting init method for "//componentName)
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_RUN, couplerRun1, phase=1, rc=rc)
    call checkRC(rc, "Error occurred setting run1 method for "//componentName)
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_RUN, couplerRun2, phase=2, rc=rc)
    call checkRC(rc, "Error occurred setting run2 method for "//componentName)
    call ESMF_CplCompSetEntryPoint(component, ESMF_METHOD_FINALIZE, couplerFinal, rc=rc)
    call checkRC(rc, "Error occurred setting finalize method for "//componentName)
end subroutine couplerSetServices

subroutine couplerInit(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    integer :: localPET
    type(ESMF_VM) :: vm
    call ESMF_VMGetGlobal(vm, rc=rc)
    call checkRC(rc, "Error occurred while trying to get global VM for "//componentName)
    call ESMF_VMGet(vm, localPET=localPET, rc=rc)
    call checkRC(rc, "Error occurred while trying to get local PET id info for "//componentName)
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Coupler Init subroutine called", ESMF_LOGMSG_INFO)
end subroutine couplerInit

subroutine couplerRun1(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Coupler Run1 (one->two) subroutine called", ESMF_LOGMSG_INFO)
end subroutine couplerRun1

subroutine couplerRun2(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Coupler Run2 (two->one) subroutine called", ESMF_LOGMSG_INFO)
end subroutine couplerRun2

subroutine couplerFinal(cplcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_CplComp) :: cplcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    call ESMF_LogWrite("Coupler Final subroutine called", ESMF_LOGMSG_INFO)
end subroutine couplerFinal

end module couplerComponent
