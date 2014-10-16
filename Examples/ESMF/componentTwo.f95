module componenttwo
use ESMF
implicit none

contains

subroutine componentTwoInit1(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Component Two Init1 subroutine called"
end subroutine componentTwoInit1

subroutine componentTwoInit2
    write(*, "(A)") "Component Two Init2 subroutine called"
end subroutine componentTwoInit2

subroutine componentTwoRun
    write(*, "(A)") "Component Two Run subroutine called"
end subroutine componentTwoRun

subroutine componentTwoFinal
    write(*, "(A)") "Component Two Final subroutine called"
end subroutine componentTwoFinal

end module componenttwo
