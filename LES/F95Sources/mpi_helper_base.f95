module mpi_helper_base
use mpi
use fortran_helper
implicit none
integer(kind=4) :: rank, cartRank, mpi_size, ierror, status(MPI_STATUS_SIZE)
integer :: communicator, cartTopComm
integer, parameter :: topTag = 1, bottomTag = 2, leftTag = 3, rightTag = 4
integer, parameter :: zbmTag = 5
integer, parameter :: leftSideTag = 6, rightSideTag = 7
integer, parameter :: dxTag = 8, dyTag = 9, collect3DReal4Tag = 10
integer, parameter :: leftNeighbour = 1, rightNeighbour = 2, topNeighbour = 3, bottomNeighbour = 4
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

subroutine setupCartesianVirtualTopology(dimensions, dimensionSizes, periodicDimensions, coordinates, neighbours, reorder)
    implicit none
    integer, intent(in) :: dimensions
    integer, intent(in) :: dimensionSizes(dimensions)
    logical, intent(in) :: periodicDimensions(dimensions)
    integer, intent(out) :: coordinates(dimensions)
    integer, intent(out) :: neighbours(2*dimensions)
    logical, intent(in) :: reorder
    call MPI_Cart_Create(communicator, dimensions, dimensionSizes, &
                         periodicDimensions, reorder, cartTopComm, ierror)
    call checkMPIError()
    call MPI_Comm_Rank(cartTopComm, cartRank, ierror)
    call checkMPIError()
    call MPI_Cart_Coords(cartTopComm, cartRank, dimensions, coordinates, ierror)
    call checkMPIError()
    call MPI_Cart_Shift(cartTopComm, 0, 1, neighbours(topNeighbour), neighbours(bottomNeighbour), ierror)
    call checkMPIError()
    call MPI_Cart_Shift(cartTopComm, 1, 1, neighbours(leftNeighbour), neighbours(rightNeighbour), ierror)
end subroutine setupCartesianVirtualTopology

logical function isMaster()
    implicit none
    isMaster = rank .eq. 0
end function isMaster

logical function isTopRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isTopRow = rank .lt. procPerRow
end function isTopRow

logical function isTopRowNeighbours(neighbours)
    implicit none
    integer, dimension(:), intent(in) :: neighbours
    isTopRowNeighbours = neighbours(topNeighbour) .eq. -1
end function isTopRowNeighbours

logical function isBottomRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isBottomRow = rank .gt. (mpi_size - procPerRow - 1)
end function isBottomRow

logical function isBottomRowNeighbours(neighbours)
    implicit none
    integer, dimension(:), intent(in) :: neighbours
    isBottomRowNeighbours = neighbours(bottomNeighbour) .eq. -1
end function isBottomRowNeighbours

logical function isLeftmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isLeftmostColumn = modulo(rank, procPerRow) .eq. 0
end function isLeftmostColumn

logical function isLeftmostColumnNeighbours(neighbours)
    implicit none
    integer, dimension(:), intent(in) :: neighbours
    isLeftmostColumnNeighbours = neighbours(leftNeighbour) .eq. -1
end function isLeftmostColumnNeighbours

logical function isRightmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isRightmostColumn = modulo(rank, procPerRow) .eq. (procPerRow - 1)
end function isRightmostColumn

logical function isRightmostColumnNeighbours(neighbours)
    implicit none
    integer, dimension(:), intent(in) :: neighbours
    isRightmostColumnNeighbours = neighbours(rightNeighbour) .eq. -1
end function isRightmostColumnNeighbours

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

end module

