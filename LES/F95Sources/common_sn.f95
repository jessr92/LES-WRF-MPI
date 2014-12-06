module common_sn
      use params_common_sn ! context_free_refactorings() line 275
!      An IMPLICIT statement specifies a type and size for all user-defined names
!      that begin with any letter, either a single letter or in a range of letters,
!      appearing in the specification. 

      implicit real*4(a-h,o-z)
      implicit integer(i-n) 
!! Original line !!       real*4 nou1,nou2,nou3,nou4,nou5,nou6,nou7,nou8,nou9
! 
!       parameter(ip=150,jp=150,kp=90)
! 
!! Original line !!       character*70 data10,data11,data20,data21,data22,data23 ,data24,data25,data26,data27,data30 ,data31,data41 ,data40,data12,data13,data19,data29 ,data50,data51,data52,data53,data54 ,fname
! 
! 
! 
! 
! 
! 
! 
! --les
! --ifdata
! --press
! --vel2,velFG
! --stretch
! 
      real a1(1:ip,1:jp+1,1:kp+1),a2(1:ip,1:jp+1,1:kp+1) ,a3(1:ip,1:jp+1,1:kp+1)
contains

subroutine calculateCornersNonMPI(array, leftThickness, rightThickness, &
                            topThickness, bottomThickness)
    implicit none
    integer, intent(in) :: leftThickness, rightThickness, &
                           topThickness, bottomThickness
    real(kind=4), dimension(:,:,:), intent(inout) :: array
    integer :: i
    do i=1, size(array, 3)
        call calculateCornersNonMPIHelper(array(:,:,i), leftThickness, rightThickness, &
                            topThickness, bottomThickness)
    end do
end subroutine calculateCornersNonMPI

subroutine calculateCornersNonMPIHelper(array, leftThickness, rightThickness, &
                            topThickness, bottomThickness)
    implicit none
    integer, intent(in) :: leftThickness, rightThickness, &
                           topThickness, bottomThickness
    real(kind=4), dimension(:,:), intent(inout) :: array
    integer :: r, c
    ! There is a top left corner to specify
    do r=topThickness,1,-1
        do c=leftThickness,1,-1
            array(r, c) = (array(r+1, c) + array(r, c+1) - array(r+1, c+1)) / 2.0
        end do
    end do
    ! There is a top right corner to specify
    do r=topThickness,1,-1
        do c=size(array,2)-rightThickness+1,size(array,2)
            array(r, c) = (array(r+1, c) + array(r, c-1) - array(r+1, c-1)) / 2.0
        end do
    end do
    ! There is a bottom left corner to specify
    do r=size(array,1)-bottomThickness+1,size(array,1)
        do c=leftThickness,1,-1
            array(r, c) = (array(r-1, c) + array(r, c+1) - array(r-1, c+1)) / 2.0
        end do
    end do
    ! There is a bottom right corner to specify
    do r=size(array,1)-bottomThickness+1,size(array,1)
        do c=size(array,2)-rightThickness+1,size(array,2)
            array(r, c) = (array(r, c-1) + array(r-1, c) - array(r-1, c-1)) / 2.0
        end do
   end do
end subroutine calculateCornersNonMPIHelper

end module common_sn
