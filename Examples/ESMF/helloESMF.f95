program helloESMF
use ESMF
implicit none
include 'mpif.h'
call main()

contains

subroutine main()
    integer :: rc
    call ESMF_Initialize(rc=rc)
    if (rc /= ESMF_SUCCESS) then
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if
    print *, "Hello ESMF World"
    call ESMF_Finalize()

end subroutine main

end program helloESMF
