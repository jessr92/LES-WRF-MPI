module params_common_sn
#ifdef MPI
    use mpi_helper
    integer, parameter :: procPerRow = PROC_PER_ROW, procPerCol = PROC_PER_COL, dimensions = 2
    integer, dimension(dimensions), parameter :: dimensionSizes = (/procPerCol, procPerRow/)
    integer, dimension(dimensions), parameter :: periodicDimensions = (/0, 0/)
    integer, dimension(dimensions) :: coordinates
    integer, dimension(2*dimensions) :: neighbours
#endif
    integer, parameter :: ipmax = 150, jpmax = 150
    !integer, parameter :: ipmax = 254, jpmax = 253
#ifndef TEST_SMALL_DOMAIN
#ifdef MPI
    integer, parameter :: ip = 150/PROC_PER_ROW, jp=150/PROC_PER_COL, kp=90
#else
    integer, parameter :: ip = 150, jp = 150, kp = 90
#endif
    !integer, parameter :: ip = 254, jp = 253, kp = 94
#else
    integer, parameter :: ip = 25, jp = 25, kp = 90
#endif
end module params_common_sn

