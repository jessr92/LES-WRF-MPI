program oasisExampleA
use netcdf
use mod_oasis
implicit none

character(len=6), parameter :: componentName = "ModelA"

call main()

contains

subroutine main()
    implicit none
    integer :: componentId, ierror
    call oasis_init_comp(componentId, componentName, ierror)
    end subroutine main

end program oasisExampleA
