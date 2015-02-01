subroutine program_haloexchange3drealexample(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    use gmcfCommunicationHelper
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    integer, parameter :: procPerCol = PROC_PER_COL, procPerRow = PROC_PER_ROW
    integer, parameter :: rows = 30, columns = 40, depthSize=2
    integer, parameter :: rowCount = rows / procPerCol
    integer, parameter :: colCount = columns / procPerRow
    integer :: leftThickness, rightThickness, topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), allocatable :: array
    integer :: i
    
    leftThickness = 3
    rightThickness = 2
    topThickness = 2
    bottomThickness = 3
    allocate(array(rowCount + topThickness + bottomThickness, &
                   colCount + leftThickness + rightThickness, &
                   depthSize))
    call gmcfInitCoupler(sys, tile, model_id)
    call initArray(array, model_id, topThickness, bottomThickness, leftThickness, rightThickness)
    call exchangeRealHalos(array, procPerRow, procPerCol, leftThickness, &
                                rightThickness, topThickness, &
                                bottomThickness, model_id)
    call gmcfFinished(model_id)
    do i=1, depthSize
        call sleep(model_id)
        call outputArrayReal(array(:,:,i))
        call sleep((procPerCol*procPerRow) - model_id)
    end do
    deallocate(array)
end subroutine program_haloexchange3drealexample

