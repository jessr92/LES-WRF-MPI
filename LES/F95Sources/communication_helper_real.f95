module communication_helper_real
use communication_helper_base
implicit none

contains

subroutine getGlobalSumOf(value)
    implicit none
    real(kind=4), intent(inout) :: value
#ifdef GR_DEBUG
    !print*, 'Rank: ', rank, ' before sum: ', value
#endif
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_SUM, communicator, ierror)
#ifdef GR_DEBUG
    !print*, 'Rank: ', rank, ' after sum: ', value
#endif
    call checkMPIError()
end subroutine getGlobalSumOf

subroutine getGlobalMaxOf(value)
    implicit none
    real(kind=4), intent(inout) :: value
#ifdef GR_DEBUG
    !print*, 'Rank: ', rank, ' before max: ', value
#endif
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_MAX, communicator, ierror)
#ifdef GR_DEBUG
    !print*, 'Rank: ', rank, ' after max: ', value
#endif
    call checkMPIError()
end subroutine getGlobalMaxOf

subroutine getGlobalMinOf(value)
    implicit none
    real(kind=4), intent(inout) :: value
#ifdef GR_DEBUG
    !print*, 'Rank: ', rank, ' before min: ', value
#endif
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_MIN, communicator, ierror)
#ifdef GR_DEBUG
    !print*, 'Rank: ', rank, ' after min: ', value
#endif
    call checkMPIError()
end subroutine getGlobalMinOf

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
                array(r, c) = (array(r+1, c) + array(r, c+1) + array(r+1, c+1)) / 3.0
            end do
        end do
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        ! There is a top right corner to specify
        do r=topThickness,1,-1
            do c=size(array,2)-rightThickness+1,size(array,2)
                array(r, c) = (array(r+1, c) + array(r, c-1) + array(r+1, c-1)) / 3.0
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        ! There is a bottom left corner to specify
        do r=size(array,1)-bottomThickness+1,size(array,1)
            do c=leftThickness,1,-1
                array(r, c) = (array(r-1, c) + array(r, c+1) + array(r-1, c+1)) / 3.0
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

subroutine exchangeRealHalos(array, procPerRow, neighbours, leftThickness, &
                                rightThickness, topThickness, &
                                bottomThickness)
    implicit none
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    integer, dimension(:), intent(in) :: neighbours
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    integer :: i, commWith, r, c, d, rowCount, colCount, depthSize, requests(8)
    real(kind=4), dimension(:,:,:), allocatable :: leftRecv, leftSend, rightSend, rightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topRecv, topSend, bottomSend, bottomRecv
    call MPI_Barrier(communicator, ierror)
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' is starting exchangeRealHalos'
#endif
    if (size(neighbours, 1) .lt. 4) then
        print*, "Error: cannot have a 4-way halo exchange with less than 4 neighbours"
        call finalise_mpi()
        return
    end if
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
        requests(i)= MPI_REQUEST_NULL
    end do
    ! Top edge to send, bottom edge to receive
    commWith = neighbours(topNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with top neighbour ', commWith
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    topSend(r, c, d) = array(r + topThickness, c+leftThickness, d)
                end do
            end do
        end do
        call MPI_ISend(topSend, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      cartTopComm, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRecv, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(2), ierror)
        call checkMPIError()
    end if
    ! Bottom edge to send, top edge to receive
    commWith = neighbours(bottomNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with bottom neighbour ', commWith
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
                      cartTopComm, requests(3), ierror)
        call checkMPIError()
        call MPI_ISend(bottomSend, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(4), ierror)
        call checkMPIError()
    end if
    ! Left edge to send, right edge to receive
    commWith = neighbours(leftNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with left neighbour ', commWith
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
    ! Right edge to send, left edge to receive
    commWith = neighbours(rightNeighbour)
    if (commWith .ne. -1) then
        !print*, 'rank ', rank, ' communicating with right neighbour ', commWith
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
    if (neighbours(topNeighbour) .ne. -1) then
        call MPI_Wait(requests(1), status, ierror)
        call checkMPIError()
        call MPI_Wait(requests(2), status, ierror)
        call checkMPIError()
    end if
    if (neighbours(bottomNeighbour) .ne. -1) then
        call MPI_Wait(requests(3), status, ierror)
        call checkMPIError()
        call MPI_Wait(requests(4), status, ierror)
        call checkMPIError()
    end if
    if (neighbours(leftNeighbour) .ne. -1) then
        call MPI_Wait(requests(5), status, ierror)
        call checkMPIError()
        call MPI_Wait(requests(6), status, ierror)
        call checkMPIError()
    end if
    if (neighbours(rightNeighbour) .ne. -1) then
        call MPI_Wait(requests(7), status, ierror)
        call checkMPIError()
        call MPI_Wait(requests(8), status, ierror)
        call checkMPIError()
    end if
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
    call MPI_Barrier(communicator, ierror)
end subroutine exchangeRealHalos

subroutine sideflowRightLeft(array, procPerRow, colToSend, colToRecv, &
                             topThickness, bottomThickness, ignoreFirstK, ignoreLastK)
    implicit none
    integer, intent(in) :: procPerRow, colToSend, colToRecv, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftRecv, rightSend
    integer :: r, d, commWith, rowCount, depthSize, ignoreFirstK, ignoreLastK
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' is starting sideflowRightLeft'
#endif
    rowCount = size(array, 1) - topThickness - bottomThickness
    depthSize = size(array, 3) - ignoreFirstK - ignoreLastK
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftRecv(rowCount, depthSize))
        commWith = rank + procPerRow - 1
        call MPI_Recv(leftRecv, rowCount*depthSize, MPI_REAL, commWith, rightSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                array(r+topThickness, colToRecv, d) = leftRecv(r, d-ignoreFirstK)
            end do
        end do
        deallocate(leftRecv)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightSend(rowCount, depthSize))
        commWith = rank - procPerRow + 1
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                rightSend(r, d-ignoreFirstK) = array(r+topThickness, colToSend, d)
            end do
        end do
        call MPI_Send(rightSend, rowCount*depthSize, MPI_REAL, commWith, rightSideTag, &
                      communicator, ierror)
        call checkMPIError()
        deallocate(rightSend)
    end if
end subroutine sideflowRightLeft

subroutine sideflowLeftRight(array, procPerRow, colToSend, colToRecv, &
                             topThickness, bottomThickness, ignoreFirstK, ignoreLastK)
    implicit none
    integer, intent(in) :: procPerRow, colToSend, colToRecv, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftSend, rightRecv
    integer :: r, d, commWith, rowCount, depthSize, ignoreFirstK, ignoreLastK
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' is starting sideflowLeftRight'
#endif
    rowCount = size(array, 1) - topThickness - bottomThickness
    depthSize = size(array, 3) - ignoreFirstK - ignoreLastK
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftSend(rowCount, depthSize))
        commWith = rank + procPerRow - 1
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                leftSend(r, d-ignoreFirstK) = array(r+topThickness, colToSend, d)
            end do
        end do
        call MPI_Send(leftSend, rowCount*depthSize, MPI_REAL, commWith, leftSideTag, &
                      communicator, ierror)
        call checkMPIError()
        deallocate(leftSend)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightRecv(rowCount, depthSize))
        commWith = rank - procPerRow + 1
        call MPI_Recv(rightRecv, rowCount*depthSize, MPI_REAL, commWith, leftSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                array(r+topThickness, colToRecv, d) = rightRecv(r, d-ignoreFirstK)
            end do
        end do
        deallocate(rightRecv)
    end if
end subroutine sideflowLeftRight

subroutine distributeZBM(zbm, ip, jp, ipmax, jpmax, procPerRow)
    implicit none
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: startRow, startCol, i, r, c
    real(kind=4), dimension(ip, jp) :: sendBuffer, recvBuffer
    call MPI_Barrier(communicator, ierror)
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' is starting distributeZBM'
#endif
    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
        do i = 1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
#ifdef GR_DEBUG
            print*, 'GR: Sending (', (startRow+1), '-', (startRow+ip), ',', &
                     (startCol+1), '-', (startCol+jp), ') to rank ', i
#endif
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
    call MPI_Barrier(communicator, ierror)
#ifdef GR_DEBUG
    print*, 'GR: rank ', rank, ' has finished distributeZBM'
#endif
end subroutine distributeZBM

subroutine distribute1DRealRowWiseArray(arrayToBeSent, receivingArray, leftBoundary, rightBoundary, procPerRow)
    implicit none
    real(kind=4), dimension(:), intent(in) :: arrayToBeSent
    real(kind=4), dimension(:), intent(out) :: receivingArray
    real(kind=4), dimension(:), allocatable :: sendBuffer
    integer, intent(in) :: leftBoundary, rightBoundary, procPerRow
    integer :: totalSize, receivingSize, i, startI, endI, currentI
    totalSize = size(arrayToBeSent, 1)
    receivingSize = size(receivingArray, 1)
    if (isMaster()) then
        allocate(sendBuffer(receivingSize))
#ifdef VERBOSE
        print*, ' Rank ', rank, ' needs to row wise send ', receivingSize, &
                ' values out of ', totalSize, ' values to each process with a ', &
                ' left boundary of ', leftBoundary, ' and a right boundary of ', &
                rightBoundary
#endif
        ! Master needs to memory copy its required portion
        receivingArray = arrayToBeSent(1:receivingSize+1)
        do i=1, mpi_size-1
            ! MPI_Send
            startI = 1 + ((i / procPerRow) * (receivingSize - leftBoundary - rightBoundary))
            endI = startI + receivingSize - 1
#ifdef VERBOSE
            print*, ' Rank ', i, ' is getting values row wise, (', startI, ',', endI, ')'
#endif
            do currentI=startI,endI
                sendBuffer(currentI-startI+1) = arrayToBeSent(currentI)
            end do
            call MPI_Send(sendBuffer, receivingSize, MPI_Real, i, dxTag, communicator, &
                          ierror)
            call checkMPIError()
        end do
        deallocate(sendBuffer)
    else
        ! Receive receivingSize reals
        call MPI_Recv(receivingArray, receivingSize, MPI_REAL, 0, dxTag, communicator, &
                      status, ierror)
        call checkMPIError()
    end if
end subroutine distribute1DRealRowWiseArray

subroutine distribute1DRealColumnWiseArray(arrayToBeSent, receivingArray, leftBoundary, rightBoundary, procPerRow)
    implicit none
    real(kind=4), dimension(:), intent(in) :: arrayToBeSent
    real(kind=4), dimension(:), intent(out) :: receivingArray
    real(kind=4), dimension(:), allocatable :: sendBuffer
    integer, intent(in) :: leftBoundary, rightBoundary, procPerRow
    integer :: totalSize, receivingSize, i, startI, endI, currentI
    totalSize = size(arrayToBeSent, 1)
    receivingSize = size(receivingArray, 1)
    if (isMaster()) then
        allocate(sendBuffer(receivingSize))
#ifdef VERBOSE
        print*, ' Rank ', rank, ' needs to column wise send ', receivingSize, &
                ' values out of ', totalSize, ' values to each process with a ', &
                ' left boundary of ', leftBoundary, ' and a right boundary of ', &
                rightBoundary
#endif
        ! Master needs to memory copy its required portion
        receivingArray = arrayToBeSent(1:receivingSize+1)
        do i=1, mpi_size-1
            ! MPI_Send
            startI = 1 + (modulo(i, procPerRow) * (receivingSize - leftBoundary - rightBoundary))
            endI = startI + receivingSize - 1
#ifdef VERBOSE
            print*, ' Rank ', i, ' is getting values column wise, (', startI, ',', endI, ')'
#endif
            do currentI=startI,endI
                sendBuffer(currentI-startI+1) = arrayToBeSent(currentI)
            end do
            call MPI_Send(sendBuffer, receivingSize, MPI_Real, i, dyTag, communicator, &
                          ierror)
            call checkMPIError()
        end do
        deallocate(sendBuffer)
    else
        ! Receive receivingSize reals
        call MPI_Recv(receivingArray, receivingSize, MPI_REAL, 0, dyTag, communicator, &
                      status, ierror)
        call checkMPIError()
    end if
end subroutine distribute1DRealColumnWiseArray

subroutine collect3DReal4ArrayP(array, arrayTot, leftBoundary, rightBoundary, &
                               topBoundary, bottomBoundary, ip, jp, kp, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(in) :: array
    real(kind=4), dimension(:,:,:), intent(out) :: arrayTot
    integer, intent(in) :: leftBoundary, rightBoundary, topBoundary, bottomBoundary
    integer, intent(in) :: ip, jp, kp, procPerRow
    integer :: i, startRow, startCol, r, c, d, bufferSize
    real(kind=4), dimension(:,:,:), allocatable :: sendRecvBuffer
    allocate(sendRecvBuffer(size(array, 1) - topBoundary, size(array, 2) - leftBoundary, size(array, 3)))
    bufferSize = (size(array, 1) - topBoundary) * (size(array, 2) - leftBoundary) * (size(array, 3))
    if (isMaster()) then
        do r=1, size(array, 1)
            do c=1, size(array, 2)
                do d=1, size(array, 3)
                    arrayTot(r, c, d) = array(r, c, d)
                end do
            end do
        end do
        do i=1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
            call MPI_Recv(sendRecvBuffer, bufferSize, MPI_Real, i, collect3DReal4Tag, &
                          communicator, status, ierror)
            call checkMPIError()
            do r=startRow+1, startRow+1+ip+bottomBoundary
                do c=startCol+1, startCol+1+jp+rightBoundary
                    do d=1, size(array, 3)
                        arrayTot(r, c, d) = sendRecvBuffer(r-startRow, c-startCol, d)
                    end do
                end do
            end do
        end do
    else
        do r=topBoundary+1, size(array, 1)
            do c=leftBoundary+1, size(array, 2)
                do d=1, size(array, 3)
                    sendRecvBuffer(r - topBoundary, c - leftBoundary, d) = array(r, c, d)
                end do
            end do
        end do
        call MPI_Send(sendRecvBuffer, bufferSize, MPI_Real, 0, collect3DReal4Tag, &
                      communicator, ierror)
        call checkMPIError()
    end if
    deallocate(sendRecvBuffer)
end subroutine collect3DReal4ArrayP

subroutine collect3DReal4Array(array, arrayTot, leftBoundary, rightBoundary, &
                               topBoundary, bottomBoundary, ip, jp, kp, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(in) :: array
    real(kind=4), dimension(:,:,:), intent(out) :: arrayTot
    integer, intent(in) :: leftBoundary, rightBoundary, topBoundary, bottomBoundary
    integer, intent(in) :: ip, jp, kp, procPerRow
    integer :: i, startRow, startCol, r, c, d, bufferSize
    real(kind=4), dimension(:,:,:), allocatable :: recvBuffer
    bufferSize = size(array, 1) * size(array, 2) * size(array, 3)
    if (isMaster()) then
        allocate(recvBuffer(size(array, 1), size(array, 2), size(array, 3)))
        do r=1, size(array, 1)
            do c=1, size(array, 2)
                do d=1, size(array, 3)
                    arrayTot(r, c, d) = array(r, c, d)
                end do
            end do
        end do
        do i=1, mpi_size - 1
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
            call MPI_Recv(recvBuffer, bufferSize, MPI_Real, i, collect3DReal4Tag, &
                          communicator, status, ierror)
            call checkMPIError()
            do r=startRow+1, startRow+1+size(array, 1)
                do c=startCol+1, startCol+1+size(array, 2)
                    do d=1, size(array, 3)
                        arrayTot(r, c, d) = recvBuffer(r-startRow, c-startCol, d)
                    end do
                end do
            end do
        end do
        deallocate(recvBuffer)
    else
        call MPI_Send(array, bufferSize, MPI_Real, 0, collect3DReal4Tag, &
                      communicator, ierror)
        call checkMPIError()
    end if
end subroutine collect3DReal4Array

end module
