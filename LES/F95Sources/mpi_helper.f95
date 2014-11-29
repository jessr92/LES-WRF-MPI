module mpi_helper
use mpi
use fortran_helper
implicit none

integer(kind=4) :: rank, mpi_size, ierror, status(MPI_STATUS_SIZE)
integer :: communicator
integer, parameter :: topTag = 1, bottomTag = 2, leftTag = 3, rightTag = 4
integer, parameter :: zbmTag = 5
integer, parameter :: leftSideTag = 6, rightSideTag = 7

contains

subroutine initialise_mpi()
    implicit none
    logical :: alreadyInitialised
    communicator = MPI_COMM_WORLD
    call MPI_Initialized(alreadyInitialised, ierror)
    call checkMPIError()
    if (.not. alreadyInitialised) then
        call MPI_Init(ierror)
        call checkMPIError()
    end if
    call MPI_COMM_Rank(communicator, rank, ierror)
    call checkMPIError()
    call MPI_COMM_Size(communicator, mpi_size, ierror)
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
        call MPI_Abort(communicator, ierror, abortError)
    end if
end subroutine checkMPIError

logical function isMaster()
    implicit none
    isMaster = rank .eq. 0
end function isMaster

logical function isTopRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isTopRow = rank .lt. procPerRow
end function isTopRow

logical function isBottomRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isBottomRow = rank .gt. (mpi_size - procPerRow - 1)
end function isBottomRow

logical function isLeftmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isLeftmostColumn = modulo(rank, procPerRow) .eq. 0
end function isLeftmostColumn

logical function isRightmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isRightmostColumn = modulo(rank, procPerRow) .eq. (procPerRow - 1)
end function isRightmostColumn

integer function topLeftRowValue(process, procPerRow, rowSize)
    implicit none
    integer, intent(in) :: process, procPerRow, rowSize
    topLeftRowValue = process / procPerRow * rowSize
end function topLeftRowValue

integer function topLeftColValue(process, procPerRow, colSize)
    implicit none
    integer, intent(in) :: process, procPerRow, colSize
    topLeftColValue = modulo(process, procPerRow) * colSize
end function topLeftColValue

subroutine calculateCorners(array, rowSize, colSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, procPerRow
    integer, dimension(rowSize + 2, colSize + 2), intent(inout) :: array
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a top left corner to specify
        array(1,1) = (array(2, 1) + array(1, 2) - array(2,2)) / 2
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a top right corner to specify
        array(1, colSize + 2) = (array(2, colSize + 2) + &
                                 array(1, colSize + 1) - &
                                 array(2, colSize + 1)) / 2
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a bottom left corner to specify
        array(rowSize + 2, 1) = (array(rowSize + 1, 1) + &
                                 array(rowSize + 2, 2) - &
                                 array(rowSize + 1, 2)) / 2
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a bottom right corner to specify
        array(rowSize + 2, colSize + 2) = (array(rowSize + 2, colSize + 1) + &
                                           array(rowSize + 1, colSize + 2) - &
                                           array(rowSize + 1, colSize + 1)) / 2
    end if
end subroutine calculateCorners

subroutine exchangeAll2DHalos3DArray(array, rowSize, colSize, depthSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow
    integer, dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    integer :: i, commWith, r, c, d, requests(8)
    integer, dimension(rowSize, depthSize) :: leftRecv, leftSend, rightSend, rightRecv
    integer, dimension(colSize, depthSize) :: topRecv, topSend, bottomSend, bottomRecv
    do i=1,8
        requests(i)= -1
    end do
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        do c=1, colSize
            do d=1, depthSize
                topSend(c, d) = array(2, c+1, d)
            end do
        end do
        call MPI_ISend(topSend, colSize*depthSize, MPI_INT, commWith, topTag, &
                      communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRecv, colSize*depthSize, MPI_INT, commWith, bottomTag, &
                      communicator, requests(2), ierror)
        call checkMPIError()
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        do c=1, colSize
            do d=1, depthSize
                bottomSend(c, d) = array(rowSize+1, c+1, d)
            end do
        end do
        call MPI_IRecv(topRecv, colSize*depthSize, MPI_INT, commWith, topTag, &
                      communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_ISend(bottomSend, colSize*depthSize, MPI_INT, commWith, bottomTag, &
                      communicator, requests(4), ierror)
        call checkMPIError()
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        do r=1, rowSize
            do d=1, depthSize
                leftSend(r, d) = array(r+1, 2, d)
            end do
        end do
        call MPI_ISend(leftSend, rowSize*depthSize, MPI_INT, commWith, leftTag, &
                      communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(rightRecv, rowSize*depthSize, MPI_INT, commWith, rightTag, &
                      communicator, requests(6), ierror)
        call checkMPIError()
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        do r=1, rowSize
            do d=1, depthSize
                rightSend(r, d) = array(r+1, colSize+1, d)
            end do
        end do
        call MPI_IRecv(leftRecv, rowSize*depthSize, MPI_INT, commWith, leftTag, &
                      communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_ISend(rightSend, rowSize*depthSize, MPI_INT, commWith, rightTag, &
                      communicator, requests(8), ierror)
        call checkMPIError()
    end if
    do i=1,8
        if (.not. requests(i) .eq. -1) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        do c=1, colSize
            do d=1, depthSize
                array(1, c+1, d) = bottomRecv(c, d)
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        do c=1, colSize
            do d=1, depthSize
                array(rowSize+2, c+1, d) = topRecv(c, d)
            end do
        end do
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        do r=1, rowSize
            do d=1, depthSize
                array(r+1, 1, d) = rightRecv(r, d)
            end do
        end do
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        do r=1, rowSize
            do d=1, depthSize
                array(r+1, colSize+2, d) = leftRecv(r, d)
            end do
        end do
    end if    
    do i=1, depthSize
        call calculateCorners(array(:,:,i), rowSize, colSize, procPerRow)
        call MPI_Barrier(communicator, ierror)
        call checkMPIError()
        call sleep(rank+1)
        call outputArray(array(:,:,i))
    end do
end subroutine exchangeAll2DHalos3DArray

subroutine calculateCornersReal(array, rowSize, colSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, procPerRow
    real(kind=4), dimension(rowSize + 2, colSize + 2), intent(inout) :: array
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a top left corner to specify
        array(1,1) = (array(2, 1) + array(1, 2) - array(2,2)) / 2
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a top right corner to specify
        array(1, colSize + 2) = (array(2, colSize + 2) + &
                                 array(1, colSize + 1) - &
                                 array(2, colSize + 1)) / 2
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a bottom left corner to specify
        array(rowSize + 2, 1) = (array(rowSize + 1, 1) + &
                                 array(rowSize + 2, 2) - &
                                 array(rowSize + 1, 2)) / 2
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a bottom right corner to specify
        array(rowSize + 2, colSize + 2) = (array(rowSize + 2, colSize + 1) + &
                                           array(rowSize + 1, colSize + 2) - &
                                           array(rowSize + 1, colSize + 1)) / 2
    end if
end subroutine calculateCornersReal

subroutine exchangeRealHalos(array, rowSize, colSize, depthSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow
    real(kind=4), dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    integer :: i, commWith, r, c, d, requests(8)
    real(kind=4), dimension(rowSize, depthSize) :: leftRecv, leftSend, rightSend, rightRecv
    real(kind=4), dimension(colSize, depthSize) :: topRecv, topSend, bottomSend, bottomRecv
    do i=1,8
        requests(i)= -1
    end do
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        do c=1, colSize
            do d=1, depthSize
                topSend(c, d) = array(2, c+1, d)
            end do
        end do
        call MPI_ISend(topSend, colSize*depthSize, MPI_REAL, commWith, topTag, &
                      communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRecv, colSize*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(2), ierror)
        call checkMPIError()
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        do c=1, colSize
            do d=1, depthSize
                bottomSend(c, d) = array(rowSize+1, c+1, d)
            end do
        end do
        call MPI_IRecv(topRecv, colSize*depthSize, MPI_REAL, commWith, topTag, &
                      communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_ISend(bottomSend, colSize*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(4), ierror)
        call checkMPIError()
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        do r=1, rowSize
            do d=1, depthSize
                leftSend(r, d) = array(r+1, 2, d)
            end do
        end do
        call MPI_ISend(leftSend, rowSize*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(rightRecv, rowSize*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(6), ierror)
        call checkMPIError()
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        do r=1, rowSize
            do d=1, depthSize
                rightSend(r, d) = array(r+1, colSize+1, d)
            end do
        end do
        call MPI_IRecv(leftRecv, rowSize*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_ISend(rightSend, rowSize*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(8), ierror)
        call checkMPIError()
    end if
    do i=1,8
        if (.not. requests(i) .eq. -1) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        do c=1, colSize
            do d=1, depthSize
                array(1, c+1, d) = bottomRecv(c, d)
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        do c=1, colSize
            do d=1, depthSize
                array(rowSize+2, c+1, d) = topRecv(c, d)
            end do
        end do
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        do r=1, rowSize
            do d=1, depthSize
                array(r+1, 1, d) = rightRecv(r, d)
            end do
        end do
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        do r=1, rowSize
            do d=1, depthSize
                array(r+1, colSize+2, d) = leftRecv(r, d)
            end do
        end do
    end if
    do i=1, depthSize
        call calculateCornersReal(array(:,:,i), rowSize, colSize, procPerRow)
    end do
end subroutine exchangeRealHalos

subroutine sideRightToLeftMPIAllExchange(array, rowSize, colSize, depthSize, procPerRow, columnToSendRecv)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow, columnToSendRecv
    real(kind=4), dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    real(kind=4), dimension(rowSize, depthSize) :: rightSend, leftRecv
    integer :: r, d, commWith
    if (isLeftmostColumn(procPerRow)) then
        commWith = rank + procPerRow - 1
        call MPI_Recv(leftRecv, rowSize*depthSize, MPI_Real, commWith, rightSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowSize
            do d=1, depthSize
                array(r+1, columnToSendRecv, d) = leftRecv(r, d)
            end do
        end do
    end if
    if (isRightmostColumn(procPerRow)) then
        commWith = rank - procPerRow + 1
        do r=1, rowSize
            do d=1, depthSize
                rightSend(r, d) = array(r+1, columnToSendRecv, d)
            end do
        end do
        call MPI_Send(rightSend, rowSize*depthSize, MPI_Real, commWith, rightSideTag, &
                      communicator, ierror)
        call checkMPIError()
    end if
end subroutine sideRightToLeftMPIAllExchange

subroutine sideLeftToRightMPIAllExchange(array, rowSize, colSize, depthSize, procPerRow, columnToSendRecv)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow, columnToSendRecv
    real(kind=4), dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    real(kind=4), dimension(rowSize, depthSize) :: leftSend, rightRecv
    integer :: r, d, commWith
    if (isLeftmostColumn(procPerRow)) then
        commWith = rank + procPerRow - 1
        do r=1, rowSize
            do d=1, depthSize
                leftSend(r, d) = array(r+1, columnToSendRecv, d)
            end do
        end do
        call MPI_Send(leftSend, rowSize*depthSize, MPI_Real, commWith, leftSideTag, &
                      communicator, ierror)
        call checkMPIError()
    end if
    if (isRightmostColumn(procPerRow)) then
        commWith = rank - procPerRow + 1
        call MPI_Recv(rightRecv, rowSize*depthSize, MPI_Real, commWith, leftSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowSize
            do d=1, depthSize
                array(r+1, columnToSendRecv, d) = rightRecv(r, d)
            end do
        end do
    end if
end subroutine sideLeftToRightMPIAllExchange

subroutine distributeZBM(zbm, ip, jp, ipmax, jpmax, procPerRow, procPerCol)
    implicit none
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, procPerCol
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: startRow, startCol, endRow, endCol, i, arraySize
    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            endRow = startRow + ip
            startCol = topLeftColValue(i, procPerCol, jp)
            endCol = startCol + jp
            arraySize = (endRow - startRow) * (endCol - startCol)
            call MPI_Send(zbm(startRow:endRow, startCol:endCol), arraySize, &
                          MPI_REAL, i, zbmTag, communicator, ierror)
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(zbm, (ip*jp), MPI_REAL, 0, zbmTag, communicator, status, ierror)
    end if
end subroutine distributeZBM

end module mpi_helper

