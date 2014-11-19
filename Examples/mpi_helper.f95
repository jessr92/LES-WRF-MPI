module mpi_helper
use mpi
implicit none

integer(kind=4) :: rank, mpi_size, ierror, status(MPI_STATUS_SIZE)

integer, parameter :: topTag = 1, bottomTag = 2, leftTag = 3, rightTag = 4
integer, parameter :: rows = 20, columns = 30
integer, parameter :: procPerRow = 4, procPerCol = 6

! Ignoring the halo boundaries, actual sizes will be + 2
integer, parameter :: rowSize = rows / procPerRow
integer, parameter :: colSize = columns / procPerCol

! Process Mapping
! 0 1 2 3
! 4 5 6 7
! 8 9 . .
! . . . .

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

subroutine exchange2DHalos(array)
    implicit none
    integer, dimension(rowSize + 2, colSize + 2), intent(inout) :: array
    integer :: commWith, colType, rowType
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
        commWith = rank - procPerRow
        print*, 'Process ', rank, ' needs to send top edge to ', commWith
        call MPI_SendRecv(array(2, 2), 1, rowType, commWith, topTag, & 
                          array(1, 2), 1, rowType, commWith, bottomTag, &
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isBottomRow()) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        print*, 'Process ', rank, ' needs to send bottom edge to ', commWith
        call MPI_SendRecv(array(rowSize+1, 2), 1, rowType, commWith, bottomTag, & 
                          array(rowSize+2, 2), 1, rowType, commWith, topTag, & 
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isLeftmostColumn()) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        print*, 'Process ', rank, ' needs to send left edge to ', commWith
        call MPI_SendRecv(array(2, 2), 1, colType, commWith, leftTag, & 
                          array(2, 1), 1, colType, commWith, rightTag, &
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isRightmostColumn()) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        print*, 'Process ', rank, ' needs to send right edge to ', commWith
        call MPI_SendRecv(array(2, colSize+1), 1, colType, commWith, rightTag, & 
                          array(2, colSize+2), 1, colType, commWith, leftTag, & 
                          MPI_COMM_WORLD, status, ierror)
    end if
    if (.not. isTopRow() .and. .not. isLeftmostColumn()) then
        ! There is a top left corner to specify
        array(1,1) = (array(2, 1) + array(1, 2)) / 2
    end if
    if (.not. isTopRow() .and. .not. isRightmostColumn()) then
        ! There is a top right corner to specify
        array(1, colSize + 2) = (array(2, colSize + 2) + &
                                 array(1, colSize + 1)) / 2
    end if
    if (.not. isBottomRow() .and. .not. isLeftmostColumn()) then
        ! There is a bottom left corner to specify
        array(rowSize + 2, 1) = (array(rowSize + 1, 1) + &
                                 array(rowSize + 2, 2)) / 2
    end if
    if (.not. isBottomRow() .and. .not. isRightmostColumn()) then
        ! There is a bottom right corner to specify
        array(rowSize + 2, colSize + 2) = (array(rowSize + 2, colSize + 1) + &
                                           array(rowSize + 1, colSize + 2)) / 2
    end if
    call sleep(rank + 1) ! to try and prevent process output being mangled by each other
    call outputArray(array)
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

