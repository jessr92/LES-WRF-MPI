program haloExchangeExample
use mpi_helper
implicit none

call main()

contains

subroutine main()
    implicit none
    integer, dimension(:,:), allocatable :: processArray
    call initialise_mpi()
    if (.NOT. (mpi_size .EQ. 24)) then
        call finalise_mpi()
        return
    endif
    allocate(processArray(rowSize + 2, colSize + 2))
    call initArray(processArray)
    call exchange2DHalos(processArray)
    deallocate(processArray)
    call finalise_mpi()
end subroutine main

subroutine initArray(processArray)
    implicit none
    integer, dimension(rowSize + 2, colSize + 2), intent(out) :: processArray
    integer :: col, row
    do row = 1, rowSize + 2
        do col = 1, colSize + 2
            processArray(row, col) = -1
        end do
    end do
    do row = 2, rowSize + 1
        do col = 2, colSize + 1
            processArray(row, col) = rank
        end do
    end do
end subroutine initArray

end program haloExchangeExample
