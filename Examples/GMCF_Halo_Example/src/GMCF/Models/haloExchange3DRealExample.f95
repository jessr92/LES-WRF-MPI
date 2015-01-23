subroutine program_haloExchange3DRealExample(sys, tile, model_id) ! This replaces 'program main'
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
    print *, 'Hello from ', i
    deallocate(processArray)
end subroutine program_haloExchange3DRealExample

