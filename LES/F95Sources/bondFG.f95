module module_bondFG

contains

subroutine bondfg(km,jm,f,im,g,h)
    use common_sn ! create_new_include_statements() line 102
    implicit none
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(Out) :: h
    integer, intent(In) :: im, jm, km
    integer :: i, j, k
! 
! --inflow condition
#ifdef MPI
    if (isTopRow(procPerRow)) then
#endif
        do k = 1,km
            do j = 1,jm
                f( 0,j,k) = f(1  ,j,k)
            end do
        end do
#ifdef MPI
    end if
#endif
! --sideflow condition
#if !defined(MPI) || (PROC_PER_ROW==1)
    do k = 1,km
        do i = 1,im
            g(i, 0,k) = g(i,jm  ,k) ! GR: Why only right->left? What about left->right?
        end do
    end do
#else
    call sideflowRightLeft(g, procPerRow, jp+1, 1)
#endif
! --ground and top condition
    do j = 1,jm
        do i = 1,im
            h(i,j, 0) = 0.0
            h(i,j,km) = 0.0
        end do
    end do
#ifdef MPI
! --halo exchanges
    call exchangeRealHalos(f, procPerRow, neighbours, 1, 1, 1, 1)
    call exchangeRealHalos(g, procPerRow, neighbours, 1, 1, 1, 1)
    call exchangeRealHalos(h, procPerRow, neighbours, 1, 1, 1, 1)
#else
#ifdef ESTIMATE_CORNERS
    call calculateCornersNonMPI(f, 1, 1, 1, 1)
    call calculateCornersNonMPI(g, 1, 1, 1, 1)
    call calculateCornersNonMPI(h, 1, 1, 1, 1)
#endif
#endif
end subroutine bondFG                                    

end module module_bondFG

