module mpi_helper
implicit none
include 'mpif.h'
integer :: rank, mpi_size, ierror
public :: initialise_mpi, finalise_mpi, rank, mpi_size, ierror

contains

subroutine initialise_mpi()
    implicit none
    call MPI_Init(ierror)
    call checkMPIError()
    call MPI_COMM_Size(MPI_COMM_WORLD, mpi_size, ierror)
    call checkMPIError()
    call MPI_COMM_Rank(MPI_COMM_WORLD, rank, ierror)
    call checkMPIError()
end subroutine initialise_mpi

subroutine finalise_mpi()
    implicit none
    call MPI_Finalize(ierror)
    call checkMPIError()
end subroutine

subroutine checkMPIError()
    implicit none
    if (ierror .ne. MPI_SUCCESS) then
        print*, ierror, " MPI error!"
        call MPI_Abort(MPI_COMM_WORLD, ierror)
    end if
end subroutine checkMPIError

end module mpi_helper

