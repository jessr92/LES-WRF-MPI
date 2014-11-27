module mpi_helper
use mpi
use fortran_helper
implicit none

integer(kind=4) :: rank, mpi_size, ierror, status(MPI_STATUS_SIZE)
integer :: communicator
integer, parameter :: topTag = 1, bottomTag = 2, leftTag = 3, rightTag = 4
integer, parameter :: zbmTag = 5

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
        !print*, ierror, " MPI error!"
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

subroutine exchange2DHalos(array, rowSize, colSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, procPerRow
    integer, dimension(rowSize + 2, colSize + 2), intent(inout) :: array
    integer :: commWith, colType, rowType, requests(8), i
    do i = 1, 8
        requests(i) = -1
    end do
    call MPI_TYPE_CONTIGUOUS(rowSize, MPI_INT, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    call MPI_TYPE_VECTOR(colSize, 1, rowSize+2, MPI_INT, rowType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(rowType, ierror)
    call checkMPIError()
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        !print*, 'Process ', rank, ' needs to send top edge to ', commWith
        call MPI_ISend(array(2, 2), 1, rowType, commWith, topTag, &
                       communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(array(1, 2), 1, rowType, commWith, bottomTag, &
                       communicator, requests(2), ierror)
        call checkMPIError()
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        !print*, 'Process ', rank, ' needs to send bottom edge to ', commWith
        call MPI_ISend(array(rowSize+1, 2), 1, rowType, commWith, bottomTag, &
                       communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_IRecv(array(rowSize+2, 2), 1, rowType, commWith, topTag, & 
                       communicator, requests(4), ierror)
        call checkMPIError()
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        !print*, 'Process ', rank, ' needs to send left edge to ', commWith
        call MPI_ISend(array(2, 2), 1, colType, commWith, leftTag, &
                       communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(array(2, 1), 1, colType, commWith, rightTag, &
                       communicator, requests(6), ierror)
        call checkMPIError()
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        !print*, 'Process ', rank, ' needs to send right edge to ', commWith
        call MPI_ISend(array(2, colSize+1), 1, colType, commWith, rightTag, &
                       communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_IRecv(array(2, colSize+2), 1, colType, commWith, leftTag, & 
                       communicator, requests(8), ierror)
        call checkMPIError()
    end if
    do i = 1, 8
        if (.not. requests(i) .eq. -1) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
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
    call MPI_Type_Free(rowType, ierror)
    call checkMPIError()
    call MPI_Type_Free(colType, ierror)
    call checkMPIError()
end subroutine exchange2DHalos

subroutine exchangeAll2DHalos3DArray(array, rowSize, colSize, depthSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow
    integer, dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    integer :: i
    do i=1, depthSize
        call exchange2DHalos(array(:,:,i), rowSize, colSize, procPerRow)
        call MPI_Barrier(communicator, ierror)
        call checkMPIError()
        call sleep(rank)
        call outputArray(array(:,:,i))
    end do
end subroutine exchangeAll2DHalos3DArray

subroutine exchange2DHalosReal(array, rowSize, colSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, procPerRow
    real(kind=4), dimension(rowSize + 2, colSize + 2), intent(inout) :: array
    integer :: commWith, colType, rowType, requests(8), i
    do i = 1, 8
        requests(i) = -1
    end do
    call MPI_TYPE_CONTIGUOUS(rowSize, MPI_REAL, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    call MPI_TYPE_VECTOR(colSize, 1, rowSize+2, MPI_REAL, rowType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(rowType, ierror)
    call checkMPIError()
    if (.not. isTopRow(procPerRow)) then
        ! Top edge to send, bottom edge to receive
        commWith = rank - procPerRow
        !print*, 'Process ', rank, ' needs to send top edge to ', commWith
        call MPI_ISend(array(2, 2), 1, rowType, commWith, topTag, &
                       communicator, requests(1), ierror)
        call checkMPIError()
        call MPI_IRecv(array(1, 2), 1, rowType, commWith, bottomTag, &
                       communicator, requests(2), ierror)
        call checkMPIError()
    end if
    if (.not. isBottomRow(procPerRow)) then
        ! Bottom edge to send, top edge to receive
        commWith = rank + procPerRow
        !print*, 'Process ', rank, ' needs to send bottom edge to ', commWith
        call MPI_ISend(array(rowSize+1, 2), 1, rowType, commWith, bottomTag, &
                       communicator, requests(3), ierror)
        call checkMPIError()
        call MPI_IRecv(array(rowSize+2, 2), 1, rowType, commWith, topTag, & 
                       communicator, requests(4), ierror)
        call checkMPIError()
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        ! Left edge to send, right edge to receive
        commWith = rank - 1
        !print*, 'Process ', rank, ' needs to send left edge to ', commWith
        call MPI_ISend(array(2, 2), 1, colType, commWith, leftTag, &
                       communicator, requests(5), ierror)
        call checkMPIError()
        call MPI_IRecv(array(2, 1), 1, colType, commWith, rightTag, &
                       communicator, requests(6), ierror)
        call checkMPIError()
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        ! Right edge to send, left edge to receive
        commWith = rank + 1
        !print*, 'Process ', rank, ' needs to send right edge to ', commWith
        call MPI_ISend(array(2, colSize+1), 1, colType, commWith, rightTag, &
                       communicator, requests(7), ierror)
        call checkMPIError()
        call MPI_IRecv(array(2, colSize+2), 1, colType, commWith, leftTag, & 
                       communicator, requests(8), ierror)
        call checkMPIError()
    end if
    do i = 1, 8
        if (.not. requests(i) .eq. -1) then
            call MPI_Wait(requests(i), status, ierror)
            call checkMPIError()
        end if
    end do
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
    !call sleep(rank + 1) ! to try and prevent process output being mangled by each other
    !call outputArrayReal(array)
    call MPI_Type_Free(rowType, ierror)
    call checkMPIError()
    call MPI_Type_Free(colType, ierror)
    call checkMPIError()
end subroutine exchange2DHalosReal

subroutine exchangeAll2DHalos3DRealArray(array, rowSize, colSize, depthSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow
    real(kind=4), dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    integer :: i
    !print *, 'rank ', rank, ' starting exchangeAll2DHalos3DRealArray'
    do i=1, depthSize
        call exchange2DHalosReal(array(:,:,i), rowSize, colSize, procPerRow)
    end do
    !print *, 'rank ', rank, ' finished exchangeAll2DHalos3DRealArray'
end subroutine exchangeAll2DHalos3DRealArray

subroutine sideRightToLeftMPIExchange(array, rowSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, procPerRow
    real(kind=4), dimension(rowSize + 2), intent(inout) :: array
    integer :: commWith, colType
    call MPI_TYPE_CONTIGUOUS(rowSize, MPI_REAL, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    if (isLeftmostColumn(procPerRow)) then
        commWith = rank + procPerRow - 1
        call MPI_Recv(array(2), 1, colType, commWith, leftTag, communicator, &
                      status, ierror)
        call checkMPIError()
    else if (isRightmostColumn(procPerRow)) then
        commWith = rank - procPerRow + 1
        call MPI_Send(array(2), 1, colType, commWith, leftTag, communicator, &
                      ierror)
    end if
    call MPI_Type_Free(colType, ierror)
    call checkMPIError()
end subroutine sideRightToLeftMPIExchange

subroutine sideRightToLeftMPIAllExchange(array, rowSize, colSize, depthSize, procPerRow, columnToSendRecv)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow, columnToSendRecv
    real(kind=4), dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    integer :: i
    do i=1, depthSize
        call sideRightToLeftMPIExchange(array(:,columnToSendRecv,i), rowSize, procPerRow)
    end do
end subroutine sideRightToLeftMPIAllExchange

subroutine sideLeftToRightMPIExchange(array, rowSize, procPerRow)
    implicit none
    integer, intent(in) :: rowSize, procPerRow
    real(kind=4), dimension(rowSize + 2), intent(inout) :: array
    integer :: commWith, colType
    call MPI_TYPE_CONTIGUOUS(rowSize, MPI_REAL, colType, ierror)
    call checkMPIError()
    call MPI_TYPE_COMMIT(colType, ierror)
    call checkMPIError()
    if (isLeftmostColumn(procPerRow)) then
        commWith = rank + procPerRow - 1
        call MPI_Send(array(2), 1, colType, commWith, rightTag, communicator, &
                      ierror)
        call checkMPIError()
    else if (isRightmostColumn(procPerRow)) then
        commWith = rank - procPerRow + 1
        call MPI_Recv(array(2), 1, colType, commWith, rightTag, communicator, &
                      status, ierror)
    end if
    call MPI_Type_Free(colType, ierror)
    call checkMPIError()
end subroutine sideLeftToRightMPIExchange

subroutine sideLeftToRightMPIAllExchange(array, rowSize, colSize, depthSize, procPerRow, columnToSendRecv)
    implicit none
    integer, intent(in) :: rowSize, colSize, depthSize, procPerRow, columnToSendRecv
    real(kind=4), dimension(rowSize + 2, colSize + 2, depthSize), intent(inout) :: array
    integer :: i
    do i=1, depthSize
        call sideLeftToRightMPIExchange(array(:,columnToSendRecv,i), rowSize, procPerRow)
    end do
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
            call MPI_Send(zbm(startRow:endRow, startCol:endCol), arraySize, MPI_REAL, i, zbmTag, communicator, ierror)
        end do
    else
        ! Receive appropriate 2D section from master
        call MPI_Recv(zbm, (ip*jp), MPI_REAL, 0, zbmTag, communicator, status, ierror)
    end if
end subroutine distributeZBM

end module mpi_helper

