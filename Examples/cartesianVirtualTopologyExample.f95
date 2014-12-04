program cartesianVirtualTopologyExample
use mpi_helper
implicit none
integer, parameter :: procPerRow = 3, procPerCol = 4, dimensions = 2
integer, dimension(dimensions), parameter :: dimensionSizes = (/procPerCol, procPerRow/)
integer, dimension(dimensions), parameter :: periodicDimensions = (/0, 0/)
integer, dimension(dimensions) :: coordinates
integer, dimension(2*dimensions) :: neighbours
call main()

contains

subroutine main
    implicit none
    call initialise_mpi()
    if (.not. (mpi_size .eq. (procperRow * procPerCol))) then
        call finalise_mpi()
        return
    end if
    call setupCartesianVirtualTopology(dimensions, dimensionSizes, periodicDimensions, coordinates, neighbours)
    call MPI_Barrier(communicator, ierror)
    call sleep(rank)
    print*, rank, ' ', cartRank, ' row ', coordinates(1), ' col ', coordinates(2)
    print*, rank, ' left ', neighbours(leftNeighbour), ' right ', neighbours(rightNeighbour)
    print*, rank, ' top ', neighbours(topNeighbour), ' bottom ', neighbours(bottomNeighbour)
    call finalise_mpi()
end subroutine main

end program
