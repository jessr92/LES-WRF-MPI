program haloExchangeExample
use mpi_helper
implicit none
integer, parameter :: rows = 10
integer, parameter :: columns = 8
integer :: colSize, rowSize ! Ignoring the halo boundaries, actual sizes will be + 2

! Mapping desired (excluding boundaries)
! 0 0 0 0 0 1 1 1 1 1
! 0 0 0 0 0 1 1 1 1 1
! 0 0 0 0 0 1 1 1 1 1
! 2 2 2 2 2 3 3 3 3 3
! 2 2 2 2 2 3 3 3 3 3
! 2 2 2 2 2 3 3 3 3 3

call main()

contains

subroutine main()
    implicit none
    integer, dimension(:,:), allocatable :: processArray
    call initialise_mpi()
    if (.NOT. (mpi_size .EQ. 4)) then
        call finalise_mpi()
        return
    endif
    colSize = columns/2
    rowSize = rows/2
    allocate(processArray(rowSize + 2, colSize + 2))
    call haloExchange(processArray)
    deallocate(processArray)
    call finalise_mpi()
end subroutine main

subroutine haloExchange(processArray)
    implicit none
    integer :: colDim, rowDim
    integer, dimension(rowSize + 2, colSize + 2) :: processArray    
    call getWorkingGridValues(colDim, rowDim)
    call initArray(processArray)
    call exchange2DHalos(processArray, colDim, rowDim)
end subroutine haloExchange

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

! Top left coordinates
subroutine getWorkingGridValues(colDim, rowDim)
    implicit none
    integer, intent(out) :: colDim, rowDim
    colDim = (colSize * modulo(rank, 2)) + 1
    rowDim = (rowSize * (rank / 2)) + 1
end subroutine getWorkingGridValues

logical function isTopRow(rowDim)
    implicit none
    integer :: rowDim
    isTopRow = rowDim .eq. 1
end function isTopRow

logical function isBottomRow(rowDim)
    implicit none
    integer :: rowDim
    isBottomRow = (rowDim + rowSize - 1) .eq. rows
end function isBottomRow

logical function isLeftmostColumn(colDim)
    implicit none
    integer :: colDim
    isLeftmostColumn = colDim .eq. 1
end function isLeftmostColumn

logical function isRightmostColumn(colDim)
    implicit none
    integer :: colDim
    isRightmostColumn = (colDim + colSize - 1) .eq. columns
end function isRightmostColumn

subroutine exchange2DHalos(processArray, colDim, rowDim)
    implicit none
    integer, dimension(rowSize + 2, colSize + 2), intent(inout) :: processArray
    integer, intent(in) :: colDim, rowDim
    integer :: communicateWith, colType, rowType
    call MPI_TYPE_CONTIGUOUS(rowSize, MPI_INT, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    call MPI_TYPE_VECTOR(colSize, 1, rowSize+2, MPI_INT, rowType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(rowType, ierror)
    call checkMPIError()
    if (.not. isTopRow(rowDim)) then
        ! Top edge to send, bottom edge to receive
        communicateWith = rank - (rows / rowSize)
        print*, 'Process ', rank, ' needs to send top edge to ', communicateWith
        call MPI_SendRecv(processArray(2, 2), 1, rowType, communicateWith, topTag, & 
                          processArray(1, 2), 1, rowType, communicateWith, bottomTag, &
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isBottomRow(rowDim)) then
        ! Bottom edge to send, top edge to receive
        communicateWith = rank + (rows / rowSize)
        print*, 'Process ', rank, ' needs to send bottom edge to ', communicateWith
        call MPI_SendRecv(processArray(rowSize+1, 2), 1, rowType, communicateWith, bottomTag, & 
                          processArray(rowSize+2, 2), 1, rowType, communicateWith, topTag, & 
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isLeftmostColumn(colDim)) then
        ! Left edge to send, right edge to receive
        communicateWith = rank - 1
        print*, 'Process ', rank, ' needs to send left edge to ', communicateWith
        call MPI_SendRecv(processArray(2, 2), 1, colType, communicateWith, leftTag, & 
                          processArray(2, 1), 1, colType, communicateWith, rightTag, &
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isRightmostColumn(colDim)) then
        ! Right edge to send, left edge to receive
        communicateWith = rank + 1
        print*, 'Process ', rank, ' needs to send right edge to ', communicateWith
        call MPI_SendRecv(processArray(2, colSize+1), 1, colType, communicateWith, rightTag, & 
                          processArray(2, colSize+2), 1, colType, communicateWith, leftTag, & 
                          MPI_COMM_WORLD, status, ierror)
    end if
    call sleep(rank) ! to try and prevent process output being mangled by each other
    call outputArray(processArray)
    call MPI_Type_Free(rowType, ierror)
    call checkMPIError()
    call MPI_Type_Free(colType, ierror)
    call checkMPIError()
end subroutine exchange2DHalos

subroutine outputArray(array)
    implicit none
    integer, dimension(:,:), intent(in) :: array
    integer :: col, row
    do row = 1, size(array, 1)
        do col = 1, size(array,2)
            if (array(row, col) .ne. -1) then
                write(*,"(I4)",advance="no") array(row,col)
            else
                write(*,"(A4)",advance="no") '-'
            end if
        end do
        write (*,*)
    end do
    write (*,*)
end subroutine outputArray

end program haloExchangeExample
