program haloExchangeExample
use mpi_helper
implicit none
integer, parameter :: rows = 20, columns = 30, depth=10
integer, parameter :: procPerRow = 4, procPerCol = 6

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
    allocate(processArray(rowSize + 2, colSize + 2, depth))
    call initArray(processArray)
    call exchange2DHalos3DArray(processArray, rowSize, colSize, depth, procPerRow, 1)
    deallocate(processArray)
    call finalise_mpi()
end subroutine main

subroutine initArray(processArray)
    implicit none
    integer, dimension(rowSize + 2, colSize + 2, depth), intent(out) :: processArray
    integer :: col, row
    do row = 1, rowSize + 2
        do col = 1, colSize + 2
            processArray(row, col, 1) = -1
        end do
    end do
    do row = 2, rowSize + 1
        do col = 2, colSize + 1
            processArray(row, col, 1) = rank
        end do
    end do
end subroutine initArray

end program haloExchangeExample

