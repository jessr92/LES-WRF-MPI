module couplerComponent
use ESMF
implicit none

contains

subroutine couplerInit(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Coupler Init subroutine called"
end subroutine couplerInit

subroutine couplerRun
    write(*, "(A)") "Coupler Run subroutine called"
end subroutine couplerRun

subroutine couplerFinal
    write(*, "(A)") "Coupler Final subroutine called"
end subroutine couplerFinal

end module couplerComponent
