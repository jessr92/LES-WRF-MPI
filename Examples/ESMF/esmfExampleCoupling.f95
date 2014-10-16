program esmfExampleCoupling
use ESMF
use componentOne, ONLY: componentOneInit1, componentOneInit2, componentOneRun, componentOneFinal
use componentTwo, ONLY: componentTwoInit1, componentTwoInit2, componentTwoRun, componentTwoFinal
use couplerComponent, ONLY: couplerInit, couplerRun, couplerFinal
implicit none

call main()

contains

subroutine main()
    implicit none
    integer :: rc
    type(ESMF_GridComp) :: componentOne, componentTwo
    type(ESMF_CplComp) :: couplerComponent
    type(ESMF_State) :: importStateOne, importStateTwo, exportStateOne, exportStateTwo
    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, & 
                         logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    call checkRC(rc, "Error occurred initialising ESMF")
    componentOne = ESMF_GridCompCreate(name="Component One", rc=rc)
    call checkRC(rc, "Error occurred creating component one")
    componentTwo = ESMF_GridCompCreate(name="Component Two", rc=rc)
    call checkRC(rc, "Error occurred creating component two")
    couplerComponent = ESMF_CplCompCreate(name="Coupler Component", rc=rc)
    call checkRC(rc, "Error occurred creating coupler component")
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
    call setupGridComponentServices(componentOne, componentOneInit1, & 
                                    componentOneInit2, componentOneRun, &
                                    componentOneFinal, "Component One")
    call setupGridComponentServices(componentTwo, componentTwoInit1, & 
                                    componentTwoInit2, componentTwoRun, &
                                    componentTwoFinal, "Component Two")
    call setupCouplerServices(couplerComponent, couplerInit, couplerRun, &
                                    couplerFinal, "Coupler Component")
    call ESMF_Finalize()
    write(*, "(A)") "Completed!"
end subroutine main

subroutine setupGridComponentServices(component, init1, init2, run, final, componentName)
    implicit none
    type(ESMF_GridComp) :: component
    external :: init1, init2, run, final
    character (len=*), optional :: componentName
    integer :: rc
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_INITIALIZE, init1, &
                                    phase=1, rc=rc)
    call checkRC(rc, "Error occurred setting init1 method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_INITIALIZE, init2, &
                                    phase=2, rc=rc)
    call checkRC(rc, "Error occurred setting init2 method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_RUN, run, rc=rc)
    call checkRC(rc, "Error occurred setting run method for "//componentName)
    call ESMF_GridCompSetEntryPoint(component, ESMF_METHOD_FINALIZE, final, rc=rc)
    call checkRC(rc, "Error occurred setting finalize method for "//componentName)
end subroutine setupGridComponentServices

subroutine setupCouplerServices(coupler, init, run, final, couplerName)
    implicit none
    type(ESMF_CplComp) :: coupler
    external :: init, run, final
    character (len=*), optional :: couplerName
    integer :: rc
    call ESMF_CplCompSetEntryPoint(coupler, ESMF_METHOD_INITIALIZE, init, rc=rc)
    call checkRC(rc, "Error occurred setting init method for "//couplerName)
    call ESMF_CplCompSetEntryPoint(coupler, ESMF_METHOD_RUN, run, rc=rc)
    call checkRC(rc, "Error occurred setting run method for "//couplerName)
    call ESMF_CplCompSetEntryPoint(coupler, ESMF_METHOD_FINALIZE, final, rc=rc)
    call checkRC(rc, "Error occurred setting finalize method for "//couplerName)
end subroutine setupCouplerServices

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
