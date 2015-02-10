module communication_helper_gmcf
use communication_common
use gmcfAPI
implicit none

integer :: rank, mpi_size ! Names for compatability with existing MPI code

contains

subroutine initialise_gmcf(sys, tile, model_id, procPerRow, procPerCol)
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id, procPerRow, procPerCol
    rank = model_id
    mpi_size = procPerRow * procPerCol
    call gmcfInitCoupler(sys, tile, model_id)
end subroutine initialise_gmcf

subroutine finalise_gmcf()
    call gmcfFinished(rank)
end subroutine finalise_gmcf

logical function isMaster()
    implicit none
    isMaster = rank .eq. 1
end function isMaster

logical function isTopRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isTopRow = rank .le. procPerRow
end function isTopRow

logical function isBottomRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isBottomRow = rank .gt. (mpi_size - procPerRow)
end function isBottomRow

logical function isLeftmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isLeftmostColumn = modulo(rank - 1, procPerRow) .eq. 0
end function isLeftmostColumn

logical function isRightmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    isRightmostColumn = modulo(rank - 1, procPerRow) .eq. (procPerRow - 1)
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

end module communication_helper_gmcf

