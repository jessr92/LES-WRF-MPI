module mpi_helper
use mpi
implicit none
integer(kind=4) :: rank, mpi_size, ierror, status(MPI_STATUS_SIZE)

integer, parameter :: topTag = 1
integer, parameter :: bottomTag = 2
integer, parameter :: leftTag = 3
integer, parameter :: rightTag = 4

integer, parameter :: rows = 20
integer, parameter :: columns = 30

integer, parameter :: procPerRow = 4
integer, parameter :: procPerCol = 6

! Ignoring the halo boundaries, actual sizes will be + 2
integer, parameter :: rowSize = rows / procPerRow
integer, parameter :: colSize = columns / procPerCol

! Process Mapping
! 0 1 2 3 4 5
! 6 7 8 9 . .
! . . . . . .

contains

subroutine initialise_mpi()
    implicit none
    call MPI_Init(ierror)
    call checkMPIError()
    call MPI_COMM_Rank(MPI_COMM_WORLD, rank, ierror)
    call checkMPIError()
    call MPI_COMM_Size(MPI_COMM_WORLD, mpi_size, ierror)
    call checkMPIError()
end subroutine initialise_mpi

subroutine finalise_mpi()
    implicit none
    call MPI_Finalize(ierror)
    call checkMPIError()
end subroutine

subroutine checkMPIError()
    implicit none
    integer :: abortError
    if (ierror .ne. MPI_SUCCESS) then
        print*, ierror, " MPI error!"
        call MPI_Abort(MPI_COMM_WORLD, ierror, abortError)
    end if
end subroutine checkMPIError

logical function isTopRow()
    implicit none
    isTopRow = rank .lt. procPerRow
end function isTopRow

logical function isBottomRow()
    implicit none
    isBottomRow = rank .gt. (mpi_size - procPerRow - 1)
end function isBottomRow

logical function isLeftmostColumn()
    implicit none
    isLeftmostColumn = modulo(rank, procPerRow) .eq. 0
end function isLeftmostColumn

logical function isRightmostColumn()
    implicit none
    isRightmostColumn = modulo(rank, procPerRow) .eq. (procPerRow - 1)
end function isRightmostColumn

subroutine exchange2DHalos(processArray)
    implicit none
    integer, dimension(rowSize + 2, colSize + 2), intent(inout) :: processArray
    integer :: communicateWith, colType, rowType
    call MPI_TYPE_CONTIGUOUS(rowSize, MPI_INT, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    call MPI_TYPE_VECTOR(colSize, 1, rowSize+2, MPI_INT, rowType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(rowType, ierror)
    call checkMPIError()
    if (.not. isTopRow()) then
        ! Top edge to send, bottom edge to receive
        communicateWith = rank - procPerRow
        print*, 'Process ', rank, ' needs to send top edge to ', communicateWith
        call MPI_SendRecv(processArray(2, 2), 1, rowType, communicateWith, topTag, & 
                          processArray(1, 2), 1, rowType, communicateWith, bottomTag, &
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isBottomRow()) then
        ! Bottom edge to send, top edge to receive
        communicateWith = rank + procPerRow
        print*, 'Process ', rank, ' needs to send bottom edge to ', communicateWith
        call MPI_SendRecv(processArray(rowSize+1, 2), 1, rowType, communicateWith, bottomTag, & 
                          processArray(rowSize+2, 2), 1, rowType, communicateWith, topTag, & 
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isLeftmostColumn()) then
        ! Left edge to send, right edge to receive
        communicateWith = rank - 1
        print*, 'Process ', rank, ' needs to send left edge to ', communicateWith
        call MPI_SendRecv(processArray(2, 2), 1, colType, communicateWith, leftTag, & 
                          processArray(2, 1), 1, colType, communicateWith, rightTag, &
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isRightmostColumn()) then
        ! Right edge to send, left edge to receive
        communicateWith = rank + 1
        print*, 'Process ', rank, ' needs to send right edge to ', communicateWith
        call MPI_SendRecv(processArray(2, colSize+1), 1, colType, communicateWith, rightTag, & 
                          processArray(2, colSize+2), 1, colType, communicateWith, leftTag, & 
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isTopRow() .and. .not. isLeftmostColumn()) then
        ! There is a top left corner to specify
        processArray(1,1) = (processArray(2, 1) + processArray(1, 2)) / 2
    end if
    if (.not. isTopRow() .and. .not. isRightmostColumn()) then
        ! There is a top right corner to specify
        processArray(1, colSize + 2) = (processArray(2, colSize + 2) + &
                                        processArray(1, colSize + 1)) / 2
    end if
    if (.not. isBottomRow() .and. .not. isLeftmostColumn()) then
        ! There is a bottom left corner to specify
        processArray(rowSize + 2, 1) = (processArray(rowSize + 2, 1) + &
                                        processArray(rowSize + 2, 2)) / 2
    end if
    if (.not. isBottomRow() .and. .not. isRightmostColumn()) then
        ! There is a bottom right corner to specify
        processArray(rowSize + 2, colSize + 2) = (processArray(rowSize + 2, colSize + 1) + &
                                                  processArray(rowSize + 1, colSize + 2)) / 2
    end if
    call sleep(rank + 1) ! to try and prevent process output being mangled by each other
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

end module mpi_helper

