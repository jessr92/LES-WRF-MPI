module mpi_helper
implicit none
include 'mpif.h'
integer :: rank, mpi_size, ierror
public :: initialise_mpi, finalise_mpi, rank, mpi_size, ierror

contains

subroutine initialise_mpi()
    implicit none
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
end subroutine initialise_mpi

subroutine finalise_mpi()
    implicit none
    call MPI_FINALIZE(ierror)
end subroutine

subroutine checkMPIError()
    implicit none
    if (ierror .ne. MPI_SUCCESS) then
        print*, ierror, "MPI error"
    end if
end subroutine checkMPIError

end module mpi_helper

