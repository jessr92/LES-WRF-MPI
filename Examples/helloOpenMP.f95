program helloOpenMP
implicit none
call main()

contains

subroutine main()
    implicit none
    integer nthreads, tid, omp_get_num_threads, omp_get_thread_num
    ! Fork a team of threads giving htem their own copies of variables
    !$OMP PARALLEL PRIVATE(nthreads, tid)
    ! Obtain thread number
    tid = OMP_GET_THREAD_NUM()
    print *, 'Hello World from thread = ', tid
    ! Only master thread does this
    if (tid .EQ. 0) then
        nthreads = omp_get_num_threads()
        print *, 'Number of threads = ', nthreads
    end if
    ! All threads join master thread and disband
    !$OMP END PARALLEL
end subroutine main

end program helloOpenMP

