module componentone
use ESMF
implicit none

contains

subroutine componentOneInit1(gridcomp, importState, exportState, clock, rc)
    implicit none
    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    write(*, "(A)") "Component One Init1 subroutine called"
end subroutine componentOneInit1

subroutine componentOneInit2
    write(*, "(A)") "Component One Init2 subroutine called"
end subroutine componentOneInit2

subroutine componentOneRun
    write(*, "(A)") "Component One Run subroutine called"
end subroutine componentOneRun

subroutine componentOneFinal
    write(*, "(A)") "Component One Final subroutine called"
end subroutine componentOneFinal

end module componentone
