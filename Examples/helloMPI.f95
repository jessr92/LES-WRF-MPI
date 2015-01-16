program helloMPI
use communication_helper
implicit none
call main()

contains

subroutine main()
    implicit none
    integer :: test, source, destination, tag, elements
    call initialise_mpi()
    source = 0
    destination = 1
    tag = 1234
    test = 0
    elements = 1
    print*, 'rank ', rank, ' size ', mpi_size
    if (rank .eq. source) then
        test = 1
        call MPI_SEND(test, elements, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierror)
    end if
    if (rank .eq. destination) then
        call MPI_RECV(test, elements, MPI_INTEGER, source, tag, MPI_COMM_WORLD, status, ierror)
        print*, 'test ', test
    end if
    print*, 'node', rank, ': Hello world'
    call finalise_mpi()
end subroutine main

end program helloMPI
