program sideflowExample
use mpi_helper
implicit none
integer, parameter :: rows = 30, columns = 40, depthSize=2, dimensions = 2
integer, parameter :: procPerCol = 3, procPerRow = 4
integer :: dimensionSizes(dimensions)
logical :: periodicDimensions(dimensions)
integer :: coordinates(dimensions), neighbours(2*dimensions)
logical :: reorder
data dimensionSizes /procPerCol,procPerRow/, periodicDimensions /.false.,.false./, &
reorder /.false./
integer, parameter :: rowCount = rows / procPerCol
integer, parameter :: colCount = columns / procPerRow

integer :: leftThickness, rightThickness, topThickness, bottomThickness
integer :: colToSendRightLeft, colToRecvRightLeft
integer :: colToSendLeftRight, colToRecvLeftRight

call main()

contains

subroutine main()
    implicit none
    integer, dimension(:,:,:), allocatable :: processArray
    integer :: i
    leftThickness = 3
    rightThickness = 2
    topThickness = 2
    bottomThickness = 3
    call initialise_mpi()
    if (.NOT. (mpi_size .EQ. (procPerRow * procPerCol))) then
        call finalise_mpi()
        return
    endif
    call setupCartesianVirtualTopology(dimensions, dimensionSizes, periodicDimensions, coordinates, neighbours, reorder)
    allocate(processArray(rowCount + topThickness + bottomThickness, &
                          colCount + leftThickness + rightThickness, &
                          depthSize))
    call initArray(processArray)
    colToSendRightLeft = size(processArray, 2) - rightThickness
    colToRecvRightLeft = leftThickness
    colToSendLeftRight = leftThickness+1
    colToRecvLeftRight = size(processArray, 2) - rightThickness + 1
    call sideflowRightLeftInteger(processArray, procPerRow, colToSendRightLeft, colToRecvRightLeft, topThickness, bottomThickness)
    call sideflowLeftRightInteger(processArray, procPerRow, colToSendLeftRight, colToRecvLeftRight, topThickness, bottomThickness)
    do i=1, depthSize
        call MPI_Barrier(communicator, ierror)
        call checkMPIError()
        call sleep(rank+1)
        call outputArray(processArray(:,:,i))
    end do
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

end program sideflowExample

