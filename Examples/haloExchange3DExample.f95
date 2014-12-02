program haloExchangeExample
use mpi_helper
implicit none
integer, parameter :: rows = 18, columns = 12, depthSize=2
integer, parameter :: procPerRow = 3, procPerCol = 4

! Ignoring the halo boundaries, actual sizes will be + 2
integer, parameter :: rowSize = rows / procPerRow
integer, parameter :: colSize = columns / procPerCol

call main()

contains

subroutine main()
    implicit none
    integer, dimension(:,:,:), allocatable :: processArray
    call initialise_mpi()
    if (.NOT. (mpi_size .EQ. (procPerRow * procPerCol))) then
        call finalise_mpi()
        return
    endif
    allocate(processArray(rowSize + 2, colSize + 2, depthSize))
    call initArray(processArray)
    call exchangeIntegerHalos(processArray, procPerRow)
    deallocate(processArray)
    call finalise_mpi()
end subroutine main

subroutine initArray(processArray)
    implicit none
    integer, dimension(rowSize + 2, colSize + 2, depthSize), intent(out) :: processArray
    integer :: col, row, depth
    do row = 1, rowSize + 2
        do col = 1, colSize + 2
            do depth = 1, depthSize
                processArray(row, col, depth) = -1
            end do
        end do
    end do
    do row = 2, rowSize + 1
        do col = 2, colSize + 1
            do depth = 1, depthSize
                processArray(row, col, depth) = rank
            end do
        end do
    end do
end subroutine initArray

end program haloExchangeExample

