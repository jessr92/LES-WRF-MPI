module mpi_helper_integer
use mpi_helper_base
implicit none

contains

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

end module
