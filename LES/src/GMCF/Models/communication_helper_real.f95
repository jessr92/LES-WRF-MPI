module communication_helper_real
#ifdef MPI
use communication_helper_mpi
#endif
#ifdef GMCF
use communication_helper_gmcf
#endif

implicit none

contains

subroutine getGlobalSumOf(value)
    implicit none
    real(kind=4), intent(inout) :: value
#ifdef GMCF
    integer :: rank
    call gmcfGetModelId(rank)
#endif
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' before sum: ', value
#endif
#ifdef GMCF
    call getGlobalSumOfGMCF(value)
#else
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_SUM, communicator, ierror)
#endif
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' after sum: ', value
#endif
#ifdef MPI
    call checkMPIError()
#endif
end subroutine getGlobalSumOf

subroutine getGlobalMaxOf(value)
    implicit none
    real(kind=4), intent(inout) :: value
#ifdef GMCF
    integer :: rank
    call gmcfGetModelId(rank)
#endif
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' before max: ', value
#endif
#ifdef GMCF
    call getGlobalMaxOfGMCF(value)
#else
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_MAX, communicator, ierror)
#endif
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' after max: ', value
#endif
#ifdef MPI
    call checkMPIError()
#endif
end subroutine getGlobalMaxOf

subroutine getGlobalMinOf(value)
    implicit none
    real(kind=4), intent(inout) :: value
#ifdef GMCF
    integer :: rank
    call gmcfGetModelId(rank)
#endif
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' before min: ', value
#endif
#ifdef GMCF
    call getGlobalMinOfGMCF(value)
#else
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_MIN, communicator, ierror)
#endif
#ifdef GR_DEBUG
    print*, 'Rank: ', rank, ' after min: ', value
#endif
#ifdef MPI
    call checkMPIError()
#endif
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
                array(r, c) = (array(r, c-1) + array(r-1, c) + array(r-1, c-1)) / 3.0
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
    integer :: i, commWith, r, c, d, rowCount, colCount, depthSize
#ifdef GMCF
    integer :: rank
#endif
#ifdef MPI
    integer :: requests(8)
#endif
    real(kind=4), dimension(:,:,:), allocatable :: leftRecv, leftSend, rightSend, rightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topRecv, topSend, bottomSend, bottomRecv
#ifdef GMCF
    call gmcfGetModelId(rank)
#endif

#ifdef MPI
    if (size(neighbours, 1) .lt. 4) then
        print*, "Error: cannot have a 4-way halo exchange with less than 4 neighbours"
        call finalise_mpi()
        return
    end if
#endif
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
#ifdef MPI
    do i=1,8
        requests(i)= MPI_REQUEST_NULL
    end do
#endif
    ! Top edge to send, bottom edge to receive
#ifdef GMCF
    if (.not. isTopRow(procPerRow)) then
        commWith = rank - procPerRow
#else
    commWith = neighbours(topNeighbour)
    if (commWith .ne. -1) then
#endif
        !print*, 'rank ', rank, ' communicating with top neighbour ', commWith
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    topSend(r, c, d) = array(r + topThickness, c+leftThickness, d)
                end do
            end do
        end do
#ifdef GMCF
        call gmcfRequestData(rank, bottomTag, topThickness * colCount * depthSize, commWith, PRE, 1)
#else
        call MPI_ISend(topSend, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      cartTopComm, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRecv, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(2), ierror)
        call checkMPIError()
#endif
    end if
    ! Bottom edge to send, top edge to receive
#ifdef GMCF
    if (.not. isBottomRow(procPerRow)) then
        commWith = rank + procPerRow
#else
    commWith = neighbours(bottomNeighbour)
    if (commWith .ne. -1) then
#endif
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
#ifdef GMCF
        call gmcfRequestData(rank, topTag, bottomThickness * colCount * depthSize, commWith, PRE, 1)
#else
        call MPI_IRecv(topRecv, bottomThickness*colCount*depthSize, MPI_REAL, commWith, topTag, &
                      cartTopComm, requests(3), ierror)
        call checkMPIError()
        call MPI_ISend(bottomSend, topThickness*colCount*depthSize, MPI_REAL, commWith, bottomTag, &
                      communicator, requests(4), ierror)
        call checkMPIError()
#endif
    end if
    ! Left edge to send, right edge to receive
#ifdef GMCF
    if (.not. isLeftmostColumn(procPerRow)) then
        commWith = rank - 1
#else
    commWith = neighbours(leftNeighbour)
    if (commWith .ne. -1) then
#endif
        !print*, 'rank ', rank, ' communicating with left neighbour ', commWith
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    leftSend(r, c, d) = array(r+topThickness, c + leftThickness, d)
                end do
            end do
        end do
#ifdef GMCF
        call gmcfRequestData(rank, rightTag, rowCount * leftThickness * depthSize, commWith, PRE, 1)
#else
        call MPI_ISend(leftSend, rightThickness*rowCount*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(rightRecv, leftThickness*rowCount*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(6), ierror)
        call checkMPIError()
#endif
    end if
    ! Right edge to send, left edge to receive
#ifdef GMCF
    if (.not. isRightmostColumn(procPerRow)) then
        commWith = rank + 1
#else
    commWith = neighbours(rightNeighbour)
    if (commWith .ne. -1) then
#endif
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
#ifdef GMCF
        call gmcfRequestData(rank, leftTag, rowCount * rightThickness * depthSize, commWith, PRE, 1)
#else
        call MPI_IRecv(leftRecv, rightThickness*rowCount*depthSize, MPI_REAL, commWith, leftTag, &
                      communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_ISend(rightSend, leftThickness*rowCount*depthSize, MPI_REAL, commWith, rightTag, &
                      communicator, requests(8), ierror)
        call checkMPIError()
#endif
    end if
#ifdef GMCF
    call sendHaloBoundaries(leftSend, rightSend, topSend, bottomSend, procPerRow)
    call recvHaloBoundaries(leftRecv, rightRecv, topRecv, bottomRecv, procPerRow)
#else
    do i=1,8
        if (requests(i) .ne. MPI_REQUEST_NULL) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
#endif
    if (.not. isTopRow(procPerRow)) then
        do r=1, topThickness
            do c=1, colCount
                do d=1, depthSize
                    array(r, c+leftThickness, d) = bottomRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow)) then
        do r=1, bottomThickness
            do c=1, colCount
                do d=1, depthSize
                    array(size(array, 1) - bottomThickness + r, c+leftThickness, d) = topRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        do r=1, rowCount
            do c=1, leftThickness
                do d=1, depthSize
                    array(r+topThickness, c, d) = rightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        do r=1, rowCount
            do c=1, rightThickness
                do d=1, depthSize
                    array(r+topThickness, size(array, 2) - rightThickness + c, d) = leftRecv(r, c, d)
                end do
            end do
        end do
    end if
#ifdef EXACT_CORNERS
    call exchangeRealCorners(array, procPerRow, leftThickness, rightThickness, topThickness, bottomThickness)
#else
    do i=1, depthSize
        call calculateCornersReal(array(:,:,i), procPerRow, leftThickness, &
                              rightThickness, topThickness, bottomThickness)
    end do
#endif
#ifdef GMCF
    call waitForHaloAcks(procPerRow)
#endif
    deallocate(leftRecv)
    deallocate(leftSend)
    deallocate(rightSend)
    deallocate(rightRecv)
    deallocate(topRecv)
    deallocate(topSend)
    deallocate(bottomSend)
    deallocate(bottomRecv)
end subroutine exchangeRealHalos

subroutine exchangeRealCorners(array, procPerRow, leftThickness, rightThickness, topThickness, bottomThickness)
    implicit none
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:,:), allocatable :: topLeftRecv, topRightRecv, bottomLeftRecv, bottomRightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topLeftSend, topRightSend, bottomLeftSend, bottomRightSend
    integer :: depthSize, requests(8), i, commWith, r, c, d
#ifdef GMCF
    integer :: rank
    call gmcfGetModelId(rank)
#endif
    depthSize = size(array, 3)
    allocate(topLeftRecv(bottomThickness, rightThickness, depthSize))
    allocate(topLeftSend(bottomThickness, rightThickness, depthSize))
    allocate(topRightRecv(bottomThickness, leftThickness, depthSize))
    allocate(topRightSend(bottomThickness, leftThickness, depthSize))
    allocate(bottomLeftRecv(topThickness, rightThickness, depthSize))
    allocate(bottomLeftSend(topThickness, rightThickness, depthSize))
    allocate(bottomRightRecv(topThickness, leftThickness, depthSize))
    allocate(bottomRightSend(topThickness, leftThickness, depthSize))
#ifdef MPI
    do i=1,8
        requests(i) = MPI_REQUEST_NULL
    end do
#endif
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        commWith = rank - procPerRow - 1
        !print*, 'Rank ', rank, ' has a top left neighbour with rank ', commWith
        do r=1,bottomThickness
            do c=1,rightThickness
                do d=1,depthSize
                    topLeftSend(r, c, d) = array(r + topThickness, c + leftThickness, d)
                end do
            end do
        end do
#ifdef GMCF
        call gmcfRequestData(rank, bottomRightTag, topThickness*leftThickness*depthSize, commWith, PRE, 1)
#endif
#ifdef MPI
        call MPI_ISend(topLeftSend, bottomThickness*rightThickness*depthSize, MPI_REAL, &
                       commWith, topLeftTag, communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomRightRecv, topThickness*leftThickness*depthSize, &
                       MPI_Real, commWith, bottomRightTag, communicator, requests(2), ierror)
        call checkMPIError()
#endif
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        commWith = rank - procPerRow + 1
        !print*, 'Rank ', rank, ' has a top right neighbour with rank ', commWith
        do r=1, bottomThickness
            do c=1, leftThickness
                do d=1, depthSize
                    topRightSend(r, c, d) = array(r + topThickness, size(array, 2) - leftThickness - rightThickness + c, d)
                end do
            end do
        end do
#ifdef GMCF
        call gmcfRequestData(rank, bottomLeftTag, topThickness*rightThickness*depthSize, commWith, PRE, 1)
#endif
#ifdef MPI
        call MPI_ISend(topRightSend, bottomThickness*leftThickness*depthSize, MPI_REAL, &
                       commWith, topRightTag, communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_IRecv(bottomLeftRecv, topThickness*rightThickness*depthSize, MPI_REAL, &
                       commWith, bottomLeftTag, communicator, requests(4), ierror)
        call checkMPIError()
#endif
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        commWith = rank + procPerRow - 1
        !print*, 'Rank ', rank, ' has a bottom left neighbour with rank ', commWith
        do r=1, topThickness
            do c=1, rightThickness
                do d=1, depthSize
                    bottomLeftSend(r, c, d) = array(size(array, 1) - topThickness - bottomThickness + r, &
                                                    c + leftThickness, d)
                end do
            end do
        end do
#ifdef GMCF
        call gmcfRequestData(rank, topRightTag, bottomThickness*leftThickness*depthSize, commWith, PRE, 1)
#endif
#ifdef MPI
        call MPI_ISend(bottomLeftSend, topThickness*rightThickness*depthSize, MPI_REAL, &
                      commWith, bottomLeftTag, communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(topRightRecv, bottomThickness*leftThickness*depthSize, MPI_REAL, &
                       commWith, topRightTag, communicator, requests(6), ierror)
        call checkMPIError()
#endif
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        commWith = rank + procPerRow + 1
        !print*, 'Rank ', rank, ' has a bottom right neighbour with rank ', commWith
        do r=1,topThickness
            do c=1,leftThickness
                do d=1,depthSize
                    bottomRightSend(r, c, d) = array(size(array, 1) - topThickness - bottomThickness + r, &
                                                     size(array, 2) - leftThickness - rightThickness + c,d)
                end do
            end do
        end do
#ifdef GMCF
        call gmcfRequestData(rank, topLeftTag, bottomThickness*rightThickness*depthSize, commWith, PRE, 1)
#endif
#ifdef MPI
        call MPI_ISend(bottomRightSend, topThickness*leftThickness*depthSize, MPI_REAL, &
                       commWith, bottomRightTag, communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_IRecv(topLeftRecv, bottomThickness*rightThickness*depthSize, &
                       MPI_Real, commWith, topLeftTag, communicator, requests(8), ierror)
        call checkMPIError()
#endif
    end if
#ifdef GMCF
    call sendExactCorners(topLeftSend, topRightSend, bottomLeftSend, bottomRightSend, procPerRow)
    call recvExactCorners(topLeftRecv, topRightRecv, bottomLeftRecv, bottomRightRecv, procPerRow)
#endif
#ifdef MPI
    do i=1,8
        if (requests(i) .ne. MPI_REQUEST_NULL) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
#endif
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        do r=1,topThickness
            do c=1,leftThickness
                do d=1,depthSize
                    array(r, c, d) = bottomRightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        do r=1,topThickness
            do c=1,rightThickness
                do d=1,depthSize
                    array(r, size(array, 2) - rightThickness + c, d) = bottomLeftRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        do r=1,bottomThickness
            do c=1, leftThickness
                do d=1, rightThickness
                    array(size(array, 1) - bottomThickness + r, c, d) = topRightRecv(r, c, d)
                end do
            end do
        end do
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        do r=1, bottomThickness
            do c=1, rightThickness
                do d=1, depthSize
                    array(size(array, 1) - bottomThickness + r, size(array, 2) - rightThickness + c, d) = topLeftRecv(r, c, d)
                end do
            end do
        end do
    end if
#ifdef GMCF
    call waitForExactCornersAcks(procPerRow)
#endif
    deallocate(topLeftRecv)
    deallocate(topLeftSend)
    deallocate(topRightRecv)
    deallocate(topRightSend)
    deallocate(bottomLeftRecv)
    deallocate(bottomLeftSend)
    deallocate(bottomRightRecv)
    deallocate(bottomRightSend)
end subroutine exchangeRealCorners

subroutine sideflowRightLeft(array, procPerRow, colToSend, colToRecv, &
                             topThickness, bottomThickness, ignoreFirstK, ignoreLastK)
    implicit none
    integer, intent(in) :: procPerRow, colToSend, colToRecv, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftRecv, rightSend
    integer :: r, d, commWith, rowCount, depthSize, ignoreFirstK, ignoreLastK
#ifdef GMCF
    integer :: rank
    call gmcfGetModelId(rank)
#endif
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' is starting sideflowRightLeft'
#endif
    rowCount = size(array, 1) - topThickness - bottomThickness
    depthSize = size(array, 3) - ignoreFirstK - ignoreLastK
    if (isLeftmostColumn(procPerRow)) then
        allocate(leftRecv(rowCount, depthSize))
        commWith = rank + procPerRow - 1
#ifdef GMCF
        call gmcfRequestData(rank, rightSideTag, rowCount*depthSize, commWith, PRE, 1)
        call recvRightLeftSideflow(leftRecv, procPerRow)
#else
        call MPI_Recv(leftRecv, rowCount*depthSize, MPI_REAL, commWith, rightSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
#endif
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
#ifdef GMCF
        call sendRightLeftSideflow(rightSend, procPerRow)
        call waitForRightLeftSideflowAcks(procPerRow)
#else
        call MPI_Send(rightSend, rowCount*depthSize, MPI_REAL, commWith, rightSideTag, &
                      communicator, ierror)
        call checkMPIError()
#endif
        deallocate(rightSend)
    end if
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' has finished sideflowRightLeft'
#endif
end subroutine sideflowRightLeft

subroutine sideflowLeftRight(array, procPerRow, colToSend, colToRecv, &
                             topThickness, bottomThickness, ignoreFirstK, ignoreLastK)
    implicit none
    integer, intent(in) :: procPerRow, colToSend, colToRecv, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    real(kind=4), dimension(:,:), allocatable :: leftSend, rightRecv
    integer :: r, d, commWith, rowCount, depthSize, ignoreFirstK, ignoreLastK
#ifdef GMCF
    integer :: rank
    call gmcfGetModelId(rank)
#endif
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
#ifdef GMCF
        call sendLeftRightSideflow(leftSend, procPerRow)
        call waitForLeftRightSideflowAcks(procPerRow)
#else
        call MPI_Send(leftSend, rowCount*depthSize, MPI_REAL, commWith, leftSideTag, &
                      communicator, ierror)
        call checkMPIError()
#endif
        deallocate(leftSend)
    else if (isRightmostColumn(procPerRow)) then
        allocate(rightRecv(rowCount, depthSize))
        commWith = rank - procPerRow + 1
#ifdef GMCF
        call gmcfRequestData(rank, leftSideTag, rowCount*depthSize, commWith, PRE, 1)
        call recvLeftRightSideflow(rightRecv, procPerRow)
#else
        call MPI_Recv(rightRecv, rowCount*depthSize, MPI_REAL, commWith, leftSideTag, &
                      communicator, status, ierror)
        call checkMPIError()
#endif
        do r=1, rowCount
            do d=1+ignoreFirstK, size(array,3) - ignoreLastK
                array(r+topThickness, colToRecv, d) = rightRecv(r, d-ignoreFirstK)
            end do
        end do
        deallocate(rightRecv)
    end if
#ifdef GR_DEBUG
    !print*, 'GR: rank ', rank, ' has finished sideflowLeftRight'
#endif

end subroutine sideflowLeftRight

subroutine distributeZBM(zbm, ip, jp, ipmax, jpmax, procPerRow)
    implicit none
    integer, intent(in) :: ip, jp, ipmax, jpmax, procPerRow
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: startRow, startCol, i, r, c
    real(kind=4), dimension(ip, jp) :: sendBuffer, recvBuffer
#ifdef GMCF
    integer :: rank, fifo_empty, has_packets
    type(gmcfPacket) :: packet
    call gmcfGetModelId(rank)
#endif
#ifdef GR_DEBUG
    print*, 'GR: rank ', rank, ' is starting distributeZBM'
#endif
    if (isMaster()) then
        ! Send appropriate 2D section to the other ranks
#ifdef GMCF
        do i = 2, mpi_size
#else
        do i = 1, mpi_size - 1
#endif
            startRow = topLeftRowValue(i, procPerRow, ip)
            startCol = topLeftColValue(i, procPerRow, jp)
#ifdef GR_DEBUG
            print*, 'GR: zbm Sending (', (startRow+1), '-', (startRow+ip), ',', &
                     (startCol+1), '-', (startCol+jp), ') to rank ', i
#endif
            do r=1, ip
                do c=1, jp
                    sendBuffer(r, c) = zbm(startRow + r, startCol + c)
                end do
            end do
            print*, 'GR: sendBuffer zbm sum: ', sum(sendBuffer)
#ifdef GMCF
            call gmcfWaitFor(rank, REQDATA, i, 1)
            print*, 'Model_id ', rank, ' has received zbm request from ', i
            call gmcfHasPackets(rank, REQDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
                if (packet%source .eq. i) then
                    call gmcfSend2DFloatArray(rank, sendBuffer, shape(sendBuffer), zbmTag, i, PRE, 1)
                    exit
                else
                    call gmcfPushPending(rank, packet) ! Too early
                end if
                call gmcfHasPackets(rank, REQDATA, has_packets)
            end do
            print*, 'Model_id ', rank, ' has sent zbm response to ', i
            call gmcfWaitFor(rank, ACKDATA, i, 1)
            print*, 'Model_id ', rank, ' has received zbm ack from ', i
            call gmcfHasPackets(rank, ACKDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPending(rank, ACKDATA, packet, fifo_empty)
                if (packet%source .ne. i) then
                    print*, 'Model_id ', rank, ' received an unexpected ack in zbm distribution'
                end if
                call gmcfHasPackets(rank, ACKDATA, has_packets)
            end do
            print*, 'Model_id ', rank, ' has dealt with zbm ack from ', i
#else
            call MPI_Send(sendBuffer, (ip*jp), MPI_REAL, i, zbmTag, &
                          communicator, ierror)
            call checkMPIError()
#endif
        end do
    else
        ! Receive appropriate 2D section from master
#ifdef GMCF
        call gmcfRequestData(rank, zbmTag, ip*jp, 1, PRE, 1)
        print*, 'Model_id ', rank, ' has requested zbm from 1'
        call gmcfWaitFor(rank, RESPDATA, 1, 1)
        print*, 'Model_id ', rank, ' has received zbm response from 1'
        call gmcfHasPackets(rank, RESPDATA, has_packets)
        do while(has_packets == 1)
            call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
            if (packet%data_id .ne. zbmTag) then
                print*, 'Received unexpected packet'
            else
                call gmcfRead2DFloatArray(recvBuffer, shape(recvBuffer),packet)
            end if
            call gmcfHasPackets(rank, RESPDATA, has_packets)
        end do
        print*, 'Model_id ', rank, ' has read zbm response'
#else
        call MPI_Recv(recvBuffer, (ip*jp), MPI_REAL, 0, zbmTag, communicator, &
                      status, ierror)
        call checkMPIError()
#endif
        print*, 'GR: recvBuffer zbm sum: ', sum(recvBuffer)
        do r=1, ip
            do c=1, jp
                zbm(r, c) = recvBuffer(r, c)
            end do
        end do
    end if
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
#ifdef GMCF
    integer :: rank, fifo_empty, has_packets
    type(gmcfPacket) :: packet
    call gmcfGetModelId(rank)
#endif
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
        print*, 'GR: dx1 sum array slice: ', sum(receivingArray)
#ifdef GMCF
        do i=2, mpi_size
#else
        do i=1, mpi_size-1
#endif
            ! MPI_Send
#ifdef GMCF
            startI = 1 + (((i-1) / procPerRow) * (receivingSize - leftBoundary - rightBoundary))
#else
            startI = 1 + ((i / procPerRow) * (receivingSize - leftBoundary - rightBoundary))
#endif
            endI = startI + receivingSize - 1
#ifdef VERBOSE
            print*, ' Rank ', i, ' is getting values row wise, (', startI, ',', endI, ')'
#endif
            do currentI=startI,endI
                sendBuffer(currentI-startI+1) = arrayToBeSent(currentI)
            end do
#ifdef GMCF
            print*, 'GR: rank ', i, ' is getting a sum of: ', sum(sendBuffer)
            call gmcfWaitFor(rank, REQDATA, i, 1)
            call gmcfHasPackets(rank, REQDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
                if (packet%source .eq. i) then
                    call gmcfSend1DFloatArray(rank, sendBuffer, shape(sendBuffer), dxTag, i, PRE, 1)
                    exit
                else
                    call gmcfPushPending(rank, packet) ! Too early
                end if
                call gmcfHasPackets(rank, REQDATA, has_packets)
            end do
            call gmcfWaitFor(rank, ACKDATA, i, 1)
            call gmcfHasPackets(rank, ACKDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPending(rank, ACKDATA, packet, fifo_empty)
                if (packet%source .ne. i) then
                    print*, 'Model_id ', rank, ' received an unexpected ack in distribute real row wise'
                end if
                call gmcfHasPackets(rank, ACKDATA, has_packets)
            end do
#else
            call MPI_Send(sendBuffer, receivingSize, MPI_Real, i, dxTag, communicator, &
                          ierror)
            call checkMPIError()
#endif
        end do
        deallocate(sendBuffer)
    else
        ! Receive receivingSize reals
#ifdef GMCF
        call gmcfRequestData(rank, dxTag, receivingSize, 1, PRE, 1)
        call gmcfWaitFor(rank, RESPDATA, 1, 1)
        call gmcfHasPackets(rank, RESPDATA, has_packets)
        do while(has_packets == 1)
            call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
            if (packet%data_id .ne. dxTag) then
                print*, 'Received unexpected packet'
            else
                call gmcfRead1DFloatArray(receivingArray, shape(receivingArray),packet)
            end if
            call gmcfHasPackets(rank, RESPDATA, has_packets)
        end do
#else
        call MPI_Recv(receivingArray, receivingSize, MPI_REAL, 0, dxTag, communicator, &
                      status, ierror)
        call checkMPIError()
#endif
    end if
    print*, 'GR: rank ', rank, ' row wise sum ', sum(receivingArray)
end subroutine distribute1DRealRowWiseArray

subroutine distribute1DRealColumnWiseArray(arrayToBeSent, receivingArray, leftBoundary, rightBoundary, procPerRow)
    implicit none
    real(kind=4), dimension(:), intent(in) :: arrayToBeSent
    real(kind=4), dimension(:), intent(out) :: receivingArray
    real(kind=4), dimension(:), allocatable :: sendBuffer
    integer, intent(in) :: leftBoundary, rightBoundary, procPerRow
    integer :: totalSize, receivingSize, i, startI, endI, currentI
#ifdef GMCF
    integer :: rank, fifo_empty, has_packets
    type(gmcfPacket) :: packet
    call gmcfGetModelId(rank)
#endif
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
#ifdef GMCF
        do i=2, mpi_size
#else
        do i=1, mpi_size-1
#endif
            ! MPI_Send
#ifdef GMCF
            startI = 1 + (modulo(i - 1, procPerRow) * (receivingSize - leftBoundary - rightBoundary))
#else
            startI = 1 + (modulo(i, procPerRow) * (receivingSize - leftBoundary - rightBoundary))
#endif
            endI = startI + receivingSize - 1
#ifdef VERBOSE
            print*, ' Rank ', i, ' is getting values column wise, (', startI, ',', endI, ')'
#endif
            do currentI=startI,endI
                sendBuffer(currentI-startI+1) = arrayToBeSent(currentI)
            end do
#ifdef GMCF
            call gmcfWaitFor(rank, REQDATA, i, 1)
            call gmcfHasPackets(rank, REQDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
                if (packet%source .eq. i) then
                    call gmcfSend1DFloatArray(rank, sendBuffer, shape(sendBuffer), dyTag, i, PRE, 1)
                    exit
                else
                    call gmcfPushPending(rank, packet) ! Too early
                end if
                call gmcfHasPackets(rank, REQDATA, has_packets)
            end do
            call gmcfWaitFor(rank, ACKDATA, i, 1)
            call gmcfHasPackets(rank, ACKDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPEnding(rank, ACKDATA, packet, fifo_empty)
                if (packet%source .ne. i) then
                    print*, 'Model_id ', rank, ' received an unexpected ack in distribute real row wise'
                end if
                call gmcfHasPackets(rank, ACKDATA, has_packets)
            end do
#else
            call MPI_Send(sendBuffer, receivingSize, MPI_Real, i, dyTag, communicator, &
                          ierror)
            call checkMPIError()
#endif
        end do
        deallocate(sendBuffer)
    else
        ! Receive receivingSize reals
#ifdef GMCF
        call gmcfRequestData(rank, dyTag, receivingSize, 1, PRE, 1)
        call gmcfWaitFor(rank, RESPDATA, 1, 1)
        call gmcfHasPackets(rank, RESPDATA, has_packets)
        do while(has_packets == 1)
            call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
            if (packet%data_id .ne. dyTag) then
                print*, 'Received unexpected packet'
            else
                call gmcfRead1DFloatArray(receivingArray, shape(receivingArray),packet)
            end if
            call gmcfHasPackets(rank, RESPDATA, has_packets)
        end do
#else
        call MPI_Recv(receivingArray, receivingSize, MPI_REAL, 0, dyTag, communicator, &
                      status, ierror)
        call checkMPIError()
#endif
    end if
end subroutine distribute1DRealColumnWiseArray

subroutine collect3DReal4Array(array, arrayTot, leftBoundary, rightBoundary, &
                               topBoundary, bottomBoundary, ip, jp, kp, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(in) :: array
    real(kind=4), dimension(:,:,:), intent(out) :: arrayTot
    integer, intent(in) :: leftBoundary, rightBoundary, topBoundary, bottomBoundary
    integer, intent(in) :: ip, jp, kp, procPerRow
    integer :: i, startRow, startCol, r, c, d, bufferSize
    real(kind=4), dimension(:,:,:), allocatable :: recvBuffer
#ifdef GMCF
    integer :: rank, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(rank)
#endif
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
#ifdef GMCF
        do i=2, mpi_size
            startRow = (ip) * ((i-1) / procPerRow)
            startCol = (jp) * (modulo(i-1, procPerRow))
#else
        do i=1, mpi_size-1
            startRow = (ip) * (i / procPerRow)
            startCol = (jp) * (modulo(i, procPerRow))
#endif
#ifdef GMCF
            call gmcfRequestData(rank, collect3DReal4Tag, bufferSize, i, PRE, 1)
            call gmcfWaitFor(rank, RESPDATA, i, 1)
            call gmcfHasPackets(rank, RESPDATA, has_packets)
            do while(has_packets == 1)
                call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
                if (packet%source .ne. i) then
                    print*, 'Rank ', rank, ' received an unexpected RESPDATA in collect array'
                else
                    call gmcfRead3DFloatArray(recvBuffer, shape(recvBuffer), packet)
                end if
                call gmcfHasPackets(rank, RESPDATA, has_packets)
            end do
#endif
#ifdef MPI
            call MPI_Recv(recvBuffer, bufferSize, MPI_Real, i, collect3DReal4Tag, &
                          communicator, status, ierror)
            call checkMPIError()
#endif
            do r=1, size(recvBuffer, 1)
                do c=1, size(recvBuffer, 2)
                    do d=1, size(recvBuffer, 3)
                        arrayTot(r + startRow, c + startCol, d) = recvBuffer(r, c, d)
                    end do
                end do
            end do
        end do
        deallocate(recvBuffer)
    else
#ifdef GMCF
        call gmcfWaitFor(rank, REQDATA, 1, 1)
        call gmcfHasPackets(rank, REQDATA, has_packets)
        do while(has_packets == 1)
            call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
            if (packet%source .ne. 1) then
                print*, 'Rank ', rank, ' received an unexpected REQDATA in collect array'
            else
                call gmcfSend3DFloatArray(rank, array, shape(array), collect3DReal4Tag, packet%source, PRE, 1)
            end if
            call gmcfHasPackets(rank, REQDATA, has_packets)
       end do
       call gmcfWaitFor(rank, ACKDATA, 1, 1)
       call gmcfHasPackets(rank, ACKDATA, has_packets)
       do while(has_packets == 1)
           call gmcfShiftPending(rank, ACKDATA, packet, fifo_empty)
           if (packet%source .ne. 1) then
               print*, 'Rank ', rank, ' received an unexpected ACKDATA in collect array'
           end if
           call gmcfHasPackets(rank, ACKDATA, has_packets)
       end do
#endif
#ifdef MPI
        call MPI_Send(array, bufferSize, MPI_Real, 0, collect3DReal4Tag, &
                      communicator, ierror)
        call checkMPIError()
#endif
    end if
end subroutine collect3DReal4Array

end module
