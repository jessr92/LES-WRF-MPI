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

integer function topLeftRowValue(process, procPerRow, rowCount)
    implicit none
    integer, intent(in) :: process, procPerRow, rowCount
    topLeftRowValue = process / procPerRow * rowCount
end function topLeftRowValue

integer function topLeftColValue(process, procPerRow, colCount)
    implicit none
    integer, intent(in) :: process, procPerRow, colCount
    topLeftColValue = modulo(process, procPerRow) * colCount
end function topLeftColValue

subroutine calculateCorners(array, procPerRow, leftThickness, rightThickness, &
                            topThickness, bottomThickness)
    implicit none
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, &
                           topThickness, bottomThickness
    integer, dimension(:,:), intent(inout) :: array
    integer :: r, c
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a top left corner to specify
        do r=topThickness,1,-1
            do c=leftThickness,1,-1
                array(r, c) = (array(r+1, c) + array(r, c+1) - array(r+1, c+1)) / 2
            end do
        end do
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a top right corner to specify
        do r=topThickness,1,-1
            do c=size(array,2)-rightThickness+1,size(array,2)
                array(r, c) = (array(r+1, c) + array(r, c-1) - array(r+1, c-1)) / 2
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a bottom left corner to specify
        do r=size(array,1)-bottomThickness+1,size(array,1)
            do c=leftThickness,1,-1
                array(r, c) = (array(r-1, c) + array(r, c+1) - array(r-1, c+1)) / 2
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a bottom right corner to specify
        do r=size(array,1)-bottomThickness+1,size(array,1)
            do c=size(array,2)-rightThickness+1,size(array,2)
                array(r, c) = (array(r, c-1) + array(r-1, c) - array(r-1, c-1)) / 2
            end do
       end do
    end if
end subroutine calculateCorners

subroutine exchangeIntegerHalos(array, procPerRow, leftThickness, &
                                rightThickness, topThickness, &
                                bottomThickness)
    implicit none
    integer, dimension(:,:,:), intent(inout) :: array
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    integer :: i, commWith, r, c, d, rowCount, colCount, depthSize, requests(8)
    integer, dimension(:,:,:), allocatable :: leftRecv, leftSend, rightSend, rightRecv
    integer, dimension(:,:,:), allocatable :: topRecv, topSend, bottomSend, bottomRecv
    rowCount = size(array, 1) - topThickness - bottomThickness
    colCount = size(array, 2) - leftThickness - rightThickness
    depthSize = size(array, 3)
    allocate(leftRecv(rowCount, rightThickness, depthSize))
    allocate(rightSend(rowCount, leftThickness, depthSize))
    allocate(rightRecv(rowCount, leftThickness, depthSize))
    allocate(leftSend(rowCount, rightThickness, depthSize))
    allocate(topRecv(bottomThickness, colCount, depthSize))
    allocate(bottomSend(topThickness, colCount, depthSize))
    allocate(bottomRecv(topThickness, colCount, depthSize))
    allocate(topSend(bottomThickness, colCount, depthSize))
    do i=1,8
        requests(i)= -1
    end do
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    topSend(r, c, d) = array(r + topThickness, c+leftThickness, d)
                end do
            end do
        end do
        call MPI_ISend(topSend, bottomThickness*colCount*depthSize, MPI_INT, commWith, topTag, &
                      communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRecv, topThickness*colCount*depthSize, MPI_INT, commWith, bottomTag, &
                      communicator, requests(2), ierror)
        call checkMPIError()
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        do r=1, topThickness
            do c=1, colCount
                do d=1, depthSize
                    bottomSend(r, c, d) = array(size(array, 1) - bottomThickness - topThickness + r, &
                                          c+leftThickness, &
                                          d)
                end do
            end do
        end do
        call MPI_IRecv(topRecv, bottomThickness*colCount*depthSize, MPI_INT, commWith, topTag, &
                      communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_ISend(bottomSend, topThickness*colCount*depthSize, MPI_INT, commWith, bottomTag, &
                      communicator, requests(4), ierror)
        call checkMPIError()
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    leftSend(r, c, d) = array(r+topThickness, c + leftThickness, d)
                end do
            end do
        end do
        call MPI_ISend(leftSend, rightThickness*rowCount*depthSize, MPI_INT, commWith, leftTag, &
                      communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(rightRecv, leftThickness*rowCount*depthSize, MPI_INT, commWith, rightTag, &
                      communicator, requests(6), ierror)
        call checkMPIError()
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    rightSend(r, c, d) = array(r+topThickness, &
                                               size(array, 2) - rightThickness - leftThickness + c,&
                                               d)
                end do
            end do
        end do
        call MPI_IRecv(leftRecv, rightThickness*rowCount*depthSize, MPI_INT, commWith, leftTag, &
                      communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_ISend(rightSend, leftThickness*rowCount*depthSize, MPI_INT, commWith, rightTag, &
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
        do r=1, topThickness
            do c=1, colCount
                do d=1, depthSize
                    array(r, c+leftThickness, d) = bottomRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    array(size(array, 1) - bottomThickness + r, c+leftThickness, d) = topRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    array(r+topThickness, c, d) = rightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    array(r+topThickness, size(array, 2) - rightThickness + c, d) = leftRecv(r, c, d)
                end do
            end do
        end do
    end if    
    do i=1, depthSize
        call calculateCorners(array(:,:,i), procPerRow, leftThickness, &
                              rightThickness, topThickness, bottomThickness)
        call MPI_Barrier(communicator, ierror)
        call checkMPIError()
        call sleep(rank+1)
        call outputArray(array(:,:,i))
    end do
    deallocate(leftRecv)
    deallocate(leftSend)
    deallocate(rightSend)
    deallocate(rightRecv)
    deallocate(topRecv)
    deallocate(topSend)
    deallocate(bottomSend)
    deallocate(bottomRecv)
end subroutine exchangeIntegerHalos

subroutine calculateCornersReal(array, procPerRow, leftThickness, rightThickness, &
                            topThickness, bottomThickness)
    implicit none
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, &
                           topThickness, bottomThickness
    real(kind=4), dimension(:,:), intent(inout) :: array
    integer :: r, c
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a top left corner to specify
        do r=topThickness,1,-1
            do c=leftThickness,1,-1
                array(r, c) = (array(r+1, c) + array(r, c+1) - array(r+1, c+1)) / 2.0
            end do
        end do
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a top right corner to specify
        do r=topThickness,1,-1
            do c=size(array,2)-rightThickness+1,size(array,2)
                array(r, c) = (array(r+1, c) + array(r, c-1) - array(r+1, c-1)) / 2.0
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a bottom left corner to specify
        do r=size(array,1)-bottomThickness+1,size(array,1)
            do c=leftThickness,1,-1
                array(r, c) = (array(r-1, c) + array(r, c+1) - array(r-1, c+1)) / 2.0
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a bottom right corner to specify
        do r=size(array,1)-bottomThickness+1,size(array,1)
            do c=size(array,2)-rightThickness+1,size(array,2)
                array(r, c) = (array(r, c-1) + array(r-1, c) - array(r-1, c-1)) / 2.0
            end do
       end do
    end if
end subroutine calculateCornersReal

subroutine exchangeRealHalos(array, procPerRow, leftThickness, &
                                rightThickness, topThickness, &
                                bottomThickness)
    implicit none
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    integer :: i, commWith, r, c, d, rowCount, colCount, depthSize, requests(8)
    real(kind=4), dimension(:,:,:), allocatable :: leftRecv, leftSend, rightSend, rightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topRecv, topSend, bottomSend, bottomRecv
    rowCount = size(array, 1) - topThickness - bottomThickness
    colCount = size(array, 2) - leftThickness - rightThickness
    depthSize = size(array, 3)
    allocate(leftRecv(rowCount, rightThickness, depthSize))
    allocate(rightSend(rowCount, leftThickness, depthSize))
    allocate(rightRecv(rowCount, leftThickness, depthSize))
    allocate(leftSend(rowCount, rightThickness, depthSize))
    allocate(topRecv(bottomThickness, colCount, depthSize))
    allocate(bottomSend(topThickness, colCount, depthSize))
    allocate(bottomRecv(topThickness, colCount, depthSize))
    allocate(topSend(bottomThickness, colCount, depthSize))
    do i=1,8
        requests(i)= -1
    end do
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    topSend(r, c, d) = array(r + topThickness, c+leftThickness, d)
                end do
            end do
        end do
        call MPI_ISend(topSend, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRecv, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(2), ierror)
        call checkMPIError()
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        do r=1, topThickness
            do c=1, colCount
                do d=1, depthSize
                    bottomSend(r, c, d) = array(size(array, 1) - bottomThickness - topThickness + r, &
                                          c+leftThickness, &
                                          d)
                end do
            end do
        end do
        call MPI_IRecv(topRecv, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_ISend(bottomSend, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(4), ierror)
        call checkMPIError()
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    leftSend(r, c, d) = array(r+topThickness, c + leftThickness, d)
                end do
            end do
        end do
        call MPI_ISend(leftSend, rightThickness*rowCount*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(rightRecv, leftThickness*rowCount*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(6), ierror)
        call checkMPIError()
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    rightSend(r, c, d) = array(r+topThickness, &
                                               size(array, 2) - rightThickness - leftThickness + c,&
                                               d)
                end do
            end do
        end do
        call MPI_IRecv(leftRecv, rightThickness*rowCount*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_ISend(rightSend, leftThickness*rowCount*depthSize, MPI_REAL, commWith, rightTag, &
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
        do r=1, topThickness
            do c=1, colCount
                do d=1, depthSize
                    array(r, c+leftThickness, d) = bottomRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    array(size(array, 1) - bottomThickness + r, c+leftThickness, d) = topRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    array(r+topThickness, c, d) = rightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    array(r+topThickness, size(array, 2) - rightThickness + c, d) = leftRecv(r, c, d)
                end do
            end do
        end do
    end if    
    do i=1, depthSize
        call calculateCornersReal(array(:,:,i), procPerRow, leftThickness, &
                              rightThickness, topThickness, bottomThickness)
    end do
    deallocate(leftRecv)
    deallocate(leftSend)
    deallocate(rightSend)
    deallocate(rightRecv)
    deallocate(topRecv)
    deallocate(topSend)
    deallocate(bottomSend)
    deallocate(bottomRecv)
end subroutine exchangeRealHalos

subroutine sideflowRightLeft(array, procPerRow, colToSend, colToRecv)
    implicit none
    integer, intent(in) :: procPerRow, colToSend, colToRecv
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftRecv, rightSend
    integer :: r, d, commWith, rowCount, depthSize
    rowCount = size(array, 1) - 2
    depthSize = size(array, 3)
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftRecv(rowCount, depthSize))
        commWith = rank + procPerRow - 1
        call MPI_Recv(leftRecv, rowCount*depthSize, MPI_Real, commWith, rightSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowCount
            do d=1, depthSize
                array(r+1, colToRecv, d) = leftRecv(r, d)
            end do
        end do
        deallocate(leftRecv)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightSend(rowCount, depthSize))
        commWith = rank - procPerRow + 1
        do r=1, rowCount
            do d=1, depthSize
                rightSend(r, d) = array(r+1, colToSend, d)
            end do
        end do
        call MPI_Send(rightSend, rowCount*depthSize, MPI_Real, commWith, rightSideTag, &
                      communicator, ierror)
        call checkMPIError()
        deallocate(rightSend)
    end if
end subroutine sideflowRightLeft

subroutine sideflowLeftRight(array, procPerRow, colToSend, colToRecv)
    implicit none
    integer, intent(in) :: procPerRow, colToSend, colToRecv
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftSend, rightRecv
    integer :: r, d, commWith, rowCount, depthSize
    rowCount = size(array, 1) - 2
    depthSize = size(array, 3)
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftSend(rowCount, depthSize))
        commWith = rank + procPerRow - 1
        do r=1, rowCount
            do d=1, depthSize
                leftSend(r, d) = array(r+1, colToSend, d)
            end do
        end do
        call MPI_Send(leftSend, rowCount*depthSize, MPI_Real, commWith, leftSideTag, &
                      communicator, ierror)
        call checkMPIError()
        deallocate(leftSend)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightRecv(rowCount, depthSize))
        commWith = rank - procPerRow + 1
        call MPI_Recv(rightRecv, rowCount*depthSize, MPI_Real, commWith, leftSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowCount
            do d=1, depthSize
                array(r+1, colToRecv, d) = rightRecv(r, d)
            end do
        end do
        deallocate(rightRecv)
    end if
end subroutine sideflowLeftRight

subroutine distributeZBM(zbm, ip, jp, ipmax, jpmax, procPerRow, procPerCol)
    implicit none
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow, procPerCol
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: startRow, startCol, i, r, c
    real(kind=4), dimension(ip, jp) :: sendBuffer, recvBuffer
    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerCol, jp)
            do r=1, ip
                do c=1, jp
                    sendBuffer(r, c) = zbm(startRow + r, startCol + c)
                end do
            end do
            call MPI_Send(sendBuffer, (ip*jp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(recvBuffer, (ip*jp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()
        do r=1, ip
            do c=1, jp
                zbm(r, c) = recvBuffer(r, c)
            end do
        end do
    end if
end subroutine distributeZBM

end module mpi_helper

