subroutine program_haloExchange3DRealExample_(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    integer, parameter :: rows = 30, columns = 40, depthSize=2, dimensions = 2
    integer, parameter :: procPerCol = 3, procPerRow = 4
    integer, parameter :: rowCount = rows / procPerCol
    integer, parameter :: colCount = columns / procPerRow
    integer :: rank
    integer :: leftThickness, rightThickness, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), allocatable :: processArray
    integer :: i

    rank = model_id
    leftThickness = 3
    rightThickness = 2
    topThickness = 2
    bottomThickness = 3

    allocate(processArray(rowCount + topThickness + bottomThickness, &
                          colCount + leftThickness + rightThickness, &
                          depthSize))
    call initArray(processArray, leftThickness, rightThickness, topThickness, bottomThickness, rank)
    call exchangeRealHalos(processArray, procPerRow, leftThickness, rightThickness, topThickness, bottomThickness)
    deallocate(processArray)
end subroutine program_haloExchange3DRealExample_

subroutine initArray(processArray, leftThickness, rightThickness, topThickness, bottomThickness, rank)
    implicit none
    real(kind=4), dimension(:,:,:), intent(out) :: processArray
    integer, intent(in) :: leftThickness, rightThickness, topThickness, bottomThickness, rank
    integer :: col, row, depth
    do row = 1, size(processArray, 1)
        do col = 1, size(processArray, 2)
            do depth = 1, size(processArray, 3)
                processArray(row, col, depth) = -1.00
            end do
        end do
    end do
    do row = topThickness + 1, size(processArray, 1) - bottomThickness
        do col = leftThickness + 1, size(processArray, 2) - rightThickness
            do depth = 1, size(processArray, 3)
                processArray(row, col, depth) = rank
                if (col < rightThickness + leftThickness + 1 .or. &
                    col > size(processArray, 2) - rightThickness - leftThickness .or. &
                    row < topThickness + bottomThickness + 1 .or. &
                    row > size(processArray, 1) - bottomThickness - topThickness) then
                    processArray(row, col, depth) = processArray(row, col, depth) + 0.5
                end if
            end do
        end do
    end do
end subroutine initArray

subroutine exchangeRealHalos(array, procPerRow, leftThickness, &
                                rightThickness, topThickness, &
                                bottomThickness)
    implicit none
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    integer, intent(in) :: procPerRow, leftThickness, rightThickness, topThickness, bottomThickness
    integer :: i, commWith, r, c, d, rowCount, colCount, depthSize, requests(8)
    real(kind=4), dimension(:,:,:), allocatable :: leftRecv, leftSend, rightSend, rightRecv
    real(kind=4), dimension(:,:,:), allocatable :: topRecv, topSend, bottomSend, bottomRecv
end subroutine exchangeRealHalos

