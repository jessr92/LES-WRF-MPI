module params_common_sn
#ifdef MPI
    use mpi_helper
    integer, parameter :: procPerRow = 2, procPerCol = 2
#endif
    integer, parameter :: ipmax = 150, jpmax = 150
    !integer, parameter :: ipmax = 254, jpmax = 253
#ifndef TEST_SMALL_DOMAIN
#ifdef MPI
    integer, parameter :: ip = ipmax/procPerRow, jp=jpmax/procPerCol, kp=90
#else
    integer, parameter :: ip = 150, jp = 150, kp = 90
#endif
    !integer, parameter :: ip = 254, jp = 253, kp = 94
#else
    integer, parameter :: ip = 25, jp = 25, kp = 90
#endif
end module params_common_sn

