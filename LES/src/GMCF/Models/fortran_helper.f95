module fortran_helper
implicit none

contains

subroutine zero3DReal4Array(array)
    implicit none
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    integer :: i, j, k
    do i=1, size(array,1)
        do j=1, size(array,2)
            do k=1, size(array,3)
                array(i,j,k) = 0.0
            end do
        end do
    end do
end subroutine zero3DReal4Array

subroutine zero2DReal4Array(array)
    implicit none
    real(kind=4), dimension(:,:), intent(inout) :: array
    integer :: i, j
    do i=1, size(array,1)
        do j=1, size(array,2)
            array(i,j) = 0.0
        end do
    end do
end subroutine zero2DReal4Array

subroutine zero1DReal4Array(array)
    implicit none
    real(kind=4), dimension(:), intent(inout) :: array
    integer :: i
    do i=1, size(array,1)
        array(i) = 0.0
    end do
end subroutine zero1DReal4Array

subroutine outputArray(array)
    implicit none
    integer, dimension(:,:), intent(in) :: array
    integer :: col, row
    do row = 1, size(array, 1)
        do col = 1, size(array,2)
            if (array(row, col) .ne. -1) then
                write(*,"(I4)",advance="no") array(row,col)
            else
                write(*,"(A4)",advance="no") '-'
            end if
        end do
        write (*,*)
    end do
    write (*,*)
end subroutine outputArray

subroutine outputArrayReal(array)
    implicit none
    real(kind=4), dimension(:,:), intent(in) :: array
    integer :: col, row
    do row = 1, size(array, 1)
        do col = 1, size(array,2)
            if (array(row, col) .ne. -1.0) then
                write(*,"(F6.2)",advance="no") array(row,col)
            else
                write(*,"(A6)", advance="no") '-'
            end if
        end do
        write (*,*)
    end do
    write (*,*)
end subroutine outputArrayReal

end module fortran_helper
