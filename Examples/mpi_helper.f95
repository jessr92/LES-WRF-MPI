module mpi_helper
use mpi
implicit none
integer(kind=4) :: rank, mpi_size, ierror, status(MPI_STATUS_SIZE)
integer, parameter :: topTag = 1
integer, parameter :: bottomTag = 2
integer, parameter :: leftTag = 3
integer, parameter :: rightTag = 4
public :: initialise_mpi, finalise_mpi, checkMPIError
public :: rank, mpi_size, ierror
public :: topTag, bottomTag, leftTag, rightTag

contains

subroutine initialise_mpi()
    implicit none
    call MPI_Init(ierror)
    call checkMPIError()
    call MPI_COMM_Rank(MPI_COMM_WORLD, rank, ierror)
    call checkMPIError()
    call MPI_COMM_Size(MPI_COMM_WORLD, mpi_size, ierror)
    call checkMPIError()
end subroutine initialise_mpi

subroutine finalise_mpi()
    implicit none
    call MPI_Finalize(ierror)
    call checkMPIError()
end subroutine

subroutine checkMPIError()
    implicit none
    integer :: abortError
    if (ierror .ne. MPI_SUCCESS) then
        print*, ierror, " MPI error!"
        call MPI_Abort(MPI_COMM_WORLD, ierror, abortError)
    end if
end subroutine checkMPIError

end module mpi_helper

