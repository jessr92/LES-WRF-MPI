program helloMPI
implicit none
include 'mpif.h'
call main()

contains

subroutine main()
    implicit none
    integer(4) :: rank, size, ierror, test, recv, source, destination, tag, elements
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    source = 0
    destination = 1
    tag = 1234
    test = 0
    elements = 1

    print*, 'rank ', rank, ' size ', size

    if (rank .eq. source) then
        test = 1
        !call MPI_SEND(test, elements, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierror)
    end if
    if (rank .eq. destination) then
        !call MPI_RECV(recv, elements, MPI_INTEGER, source, tag, MPI_COMM_WORLD, ierror)
    end if
    print*, 'node', rank, ': Hello world'
    call MPI_FINALIZE(ierror)
end subroutine main

end program helloMPI
