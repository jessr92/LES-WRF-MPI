program cartesianVirtualTopologyExample
use mpi_helper
implicit none
integer, parameter :: procPerRow = 3, procPerCol = 4, dimensions = 2
integer :: dimensionSizes(dimensions)
logical :: periodicDimensions(dimensions)
integer :: coordinates(dimensions), neighbours(2*dimensions)
logical :: reorder
data dimensionSizes /procPerCol,procPerRow/, periodicDimensions /.false.,.false./, &
reorder /.false./
call main()

contains

subroutine main
    implicit none
    call initialise_mpi()
    if (.not. (mpi_size .eq. (procperRow * procPerCol))) then
        call finalise_mpi()
        return
    end if
    call setupCartesianVirtualTopology(dimensions, dimensionSizes, periodicDimensions, coordinates, neighbours, reorder)
    call MPI_Barrier(communicator, ierror)
    call sleep(rank)
    print*, 'rank ', rank, ' ', cartRank, ' row ', coordinates(1), ' col ', coordinates(2)
    print*, 'rank ', rank, ' left neighbour ', neighbours(leftNeighbour), ' right neighbour ', neighbours(rightNeighbour)
    print*, 'rank ', rank, ' top neighbour ', neighbours(topNeighbour), ' bottom neighbour ', neighbours(bottomNeighbour)
    call finalise_mpi()
end subroutine main

end program
