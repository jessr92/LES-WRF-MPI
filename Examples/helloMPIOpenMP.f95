program helloMPIOpenMP
use mpi_helper
implicit none
call main()

contains

subroutine main()
    implicit none
    integer nthreads, tid, omp_get_num_threads, omp_get_thread_num
    call initialise_mpi()
    print*, 'node', rank, ': Hello world'
    !$OMP PARALLEL PRIVATE(nthreads, tid)
    tid = omp_get_thread_num()
    if (tid .EQ. 0) then
        nthreads = omp_get_num_threads()
        print *, 'Number of threads = ', nthreads, 'in node', rank
    end if
    !$OMP END PARALLEL
    call finalise_mpi()
end subroutine main

end program helloMPIOpenMP
