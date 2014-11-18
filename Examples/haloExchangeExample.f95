program haloExchangeExample
use mpi_helper
implicit none
integer, parameter :: rows = 4
integer, parameter :: columns = 8
integer :: colSize, rowSize ! Ignoring the halo boundaries, actual sizes will be + 2
integer, parameter :: rowTag = 1
integer, parameter :: colTag = 2

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
    call initialise_mpi()
    if (.NOT. (mpi_size .EQ. 4)) then
        call finalise_mpi()
        return
    endif
    colSize = columns/(mpi_size/2)
    rowSize = rows/(mpi_size/2)
    call haloExchange()
    call finalise_mpi()
end subroutine main

subroutine haloExchange()
    implicit none
    integer :: colDim, rowDim
    integer, dimension(colSize + 2, rowSize + 2) :: processArray    
    call getWorkingGridValues(colDim, rowDim)
    call initArray(processArray)
    call exchange2DHalos(processArray, colDim, rowDim)
end subroutine haloExchange

subroutine initArray(processArray)
    implicit none
    integer, dimension(:,:), intent(out) :: processArray
    integer :: col, row
    do col = 1, size(processArray, 1)
        do row = 1, size(processArray, 2)
            processArray(col, row) = -1
        end do
    end do
    do col = 2, size(processArray,1) - 1
        do row = 2, size(processArray,2) - 1
            processArray(col, row) = rank
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

subroutine exchange2DHalos(processArray, colDim, rowDim)
    implicit none
    integer, dimension(colSize + 2,rowSize + 2), intent(inout) :: processArray
    integer, intent(in) :: colDim, rowDim
    integer :: communicateWith, colType, rowType
    call MPI_TYPE_CONTIGUOUS(colSize, MPI_INT, rowType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(rowType, ierror)
    call checkMPIError()
    call MPI_TYPE_VECTOR(rowSize, 1, colSize+2, MPI_INT, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    
    if (rowDim .ne. 1) then
        ! Top edge to send, bottom edge to receive
        communicateWith = rank - (rows / rowSize)
        print*, 'Process ', rank, ' needs to send top edge to ', communicateWith
        call mpi_sendrecv(processArray(2, 2), 1, rowType, communicateWith, rowTag, & 
                          processArray(2, 1), 1, rowType, communicateWith, rowTag, &
                          MPI_COMM_WORLD, ierror)
    end if
    if (rowDim + rowSize - 1 .ne. rows) then
        ! Bottom edge to send, top edge to receive
        communicateWith = rank + (rows / rowSize)
        if (communicateWith .lt. mpi_size) then
        print*, 'Process ', rank, ' needs to send bottom edge to ', communicateWith
        call mpi_sendrecv(processArray(2, rowSize+1), 1, rowType, communicateWith, rowTag, & 
                          processArray(2, rowSize+2), 1, rowType, communicateWith, rowTag, & 
                          MPI_COMM_WORLD, ierror)
        end if
    end if
    if (colDim .ne. 1) then
        ! Left edge to send, right edge to receive
        communicateWith = rank - 1
        print*, 'Process ', rank, ' needs to send left edge to ', communicateWith
        call mpi_sendrecv(processArray(2, 2), 1, colType, communicateWith, colTag, & 
                          processArray(1, 2), 1, colType, communicateWith, colTag, &
                          MPI_COMM_WORLD, ierror)
    end if
    if (colDim + colSize - 1 .ne. columns) then
        ! Right edge to send, left edge to receive
        communicateWith = rank + 1
        print*, 'Process ', rank, ' needs to send right edge to ', communicateWith
        call mpi_sendrecv(processArray(colSize+1, 2), 1, colType, communicateWith, colTag, & 
                          processArray(colSize+2, 2), 1, colType, communicateWith, colTag, & 
                          MPI_COMM_WORLD, ierror)
    end if
    call sleep(rank)
    call outputArray(processArray)
end subroutine exchange2DHalos

subroutine outputArray(array)
    implicit none
    integer, dimension(:,:), intent(in) :: array
    integer :: col, row
    do col = 1, size(array,1)
        do row = 1, size(array, 2)
            if (array(col,row) .ne. -1) then
                write(*,"(I4)",advance="no") array(col,row)
            else
                write(*,"(A4)",advance="no") '-'
            end if
        end do
        write (*,*)
    end do
    write (*,*)
end subroutine outputArray

end program haloExchangeExample
