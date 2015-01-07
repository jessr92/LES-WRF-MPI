module params_common_sn
#ifdef MPI
    use mpi_helper
    integer, parameter :: procPerRow = PROC_PER_ROW, procPerCol = PROC_PER_COL, dimensions = 2
    integer :: dimensionSizes(dimensions)
    logical :: periodicDimensions(dimensions)
    integer :: coordinates(dimensions), neighbours(2*dimensions)
    logical :: reorder
    data dimensionSizes /procPerCol,procPerRow/, periodicDimensions /.false.,.false./, &
    reorder /.false./
#endif
#ifdef EXPANDING_AREA
    integer, parameter :: ipmax = 150*PROC_PER_COL
    integer, parameter :: jpmax = 150*PROC_PER_ROW
#else
    integer, parameter :: ipmax = 150, jpmax = 150
    !integer, parameter :: ipmax = 254, jpmax = 253
#endif
#ifndef TEST_SMALL_DOMAIN
#ifdef MPI
#ifdef EXPANDING_AREA
    integer, parameter :: ip = 150
    integer, parameter :: jp = 150
    integer, parameter :: kp=90
#else
    integer, parameter :: ip = 150/PROC_PER_COL ! rows per process
    integer, parameter :: jp = 150/PROC_PER_ROW ! columns per process
    integer, parameter :: kp=90
#endif
#else
    integer, parameter :: ip = 150, jp = 150, kp = 90
#endif
    !integer, parameter :: ip = 254, jp = 253, kp = 94
#else
    integer, parameter :: ip = 25, jp = 25, kp = 90
#endif
end module params_common_sn

