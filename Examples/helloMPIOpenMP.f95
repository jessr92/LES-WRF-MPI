program helloMPIOpenMP
implicit none
include 'mpif.h'
call main()

contains

subroutine main()
    implicit none
    integer rank, size, ierror
    integer nthreads, tid, omp_get_num_threads, omp_get_thread_num
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    print*, 'node', rank, ': Hello world'
    !$OMP PARALLEL PRIVATE(nthreads, tid)
    tid = omp_get_thread_num()
    if (tid .EQ. 0) then
        nthreads = omp_get_num_threads()
        print *, 'Number of threads = ', nthreads, 'in node', rank
    end if
    !$OMP END PARALLEL
    call MPI_FINALIZE(ierror)
end subroutine main

end program helloMPIOpenMP
