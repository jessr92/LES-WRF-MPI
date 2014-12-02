module module_boundp

! GR: Ignore boundp for halo exchange. This would kill performance!
! GR: Actually, we need this for correctness (UrbanFlow = NaN... is due to
! non-convergence).

contains

subroutine boundp2(jm,im,p,km)
    use common_sn ! create_new_include_statements() line 102
    implicit none
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j
! 
! --computational boundary(neumann condition)
    do j = 0,jm+1
        do i = 0,im+1
            p(i,j,   0) = p(i,j,1)
            p(i,j,km+1) = p(i,j,km)
        end do
    end do
#ifdef MPI
#ifndef FAST_MPI
! --halo exchanges
    call exchangeRealHalos(p, procPerRow)
#endif
#endif
end subroutine boundp2

subroutine boundp1(km,jm,p,im)
    use common_sn ! create_new_include_statements() line 102
    implicit none
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
#if (PROC_PER_ROW==1) || (defined(MPI) && defined(FAST_MPI)) || !defined(MPI)
    integer :: i, j, k
#else
    integer :: j, k
#endif
! 
! --computational boundary(neumann condition)
#ifdef MPI
#ifndef FAST_MPI
    if (isTopRow(procPerRow) .or. isBottomRow(procPerRow)) then
#endif
#endif
        do k = 0,km+1
            do j = 0,jm+1
#ifdef MPI
#ifndef FAST_MPI
                if (isTopRow(procPerRow)) then
#endif
#endif
                    p(   0,j,k) = p(1 ,j,k)
#ifdef MPI
#ifndef FAST_MPI
                else
#endif
#endif
                    p(im+1,j,k) = p(im,j,k)
#ifdef MPI
#ifndef FAST_MPI
                end if
#endif
#endif
            end do
        end do
#ifdef MPI
#ifndef FAST_MPI
    end if
#endif
#endif
! --side flow exchanges
#if (PROC_PER_ROW==1) || (defined(MPI) && defined(FAST_MPI)) || !defined(MPI)
    do k = 0,km+1
        do i = 0,im+1
            p(i,   0,k) = p(i,jm,k) ! right to left
            p(i,jm+1,k) = p(i, 1,k) ! left to right
        end do
    end do
#else
    call sideflowRightLeft(p, size(p, 1) - 2, size(p, 2) - 2, size(p, 3), procPerRow, jp+1, 1)
    call sideflowLeftRight(p, size(p, 1) - 2, size(p, 2) - 2, size(p, 3), procPerRow, 2, jp+2)
#endif
#ifdef MPI
! --halo exchanges
    call exchangeRealHalos(p, procPerRow)
#endif
end subroutine boundp1

end module module_boundp

