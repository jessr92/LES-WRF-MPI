program haloExchangeExample
use mpi_helper
implicit none
integer, parameter :: rows = 30, columns = 40, depthSize=2, dimensions = 2
integer, parameter :: procPerRow = 3, procPerCol = 4
integer, dimension(dimensions), parameter :: dimensionSizes = (/procPerCol, procPerRow/)
integer, dimension(dimensions), parameter :: periodicDimensions = (/0, 0/)
integer, dimension(dimensions) :: coordinates
integer, dimension(2*dimensions) :: neighbours
! Ignoring the halo boundaries, actual sizes will be + 2
integer, parameter :: rowSize = rows / procPerRow
integer, parameter :: colSize = columns / procPerCol

integer :: leftThickness, rightThickness, topThickness, bottomThickness

call main()

contains

subroutine main()
    implicit none
    integer, dimension(:,:,:), allocatable :: processArray
    leftThickness = 3
    rightThickness = 2
    topThickness = 2
    bottomThickness = 3
    call initialise_mpi()
    if (.NOT. (mpi_size .EQ. (procPerRow * procPerCol))) then
        call finalise_mpi()
        return
    endif
    call setupCartesianVirtualTopology(dimensions, dimensionSizes, periodicDimensions, coordinates, neighbours)
    allocate(processArray(rowSize + topThickness + bottomThickness, &
                          colSize + leftThickness + rightThickness, &
                          depthSize))
    call initArray(processArray)
    call exchangeIntegerHalos(processArray, procPerRow, neighbours, leftThickness, rightThickness, topThickness, bottomThickness)
    deallocate(processArray)
    call finalise_mpi()
end subroutine main

subroutine initArray(processArray)
    implicit none
    integer, dimension(:,:,:), intent(out) :: processArray
    integer :: col, row, depth
    do row = 1, size(processArray, 1)
        do col = 1, size(processArray, 2)
            do depth = 1, size(processArray, 3)
                processArray(row, col, depth) = -1
            end do
        end do
    end do
    do row = topThickness + 1, size(processArray, 1) - bottomThickness
        do col = leftThickness + 1, size(processArray, 2) - rightThickness
            do depth = 1, size(processArray, 3)
                processArray(row, col, depth) = rank
            end do
        end do
    end do
end subroutine initArray

end program haloExchangeExample

