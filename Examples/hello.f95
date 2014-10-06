program hello
implicit none
include 'mpif.h'
call main()

contains

subroutine main()
    implicit none
    integer rank, size, ierror
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    print*, 'node', rank, ': Hello world'
    call MPI_FINALIZE(ierror)
end subroutine main

end program hello
