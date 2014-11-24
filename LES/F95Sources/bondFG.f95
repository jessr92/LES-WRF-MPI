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
#ifdef MPI
    if (isLeftMostColumn(procPerRow) .or. isRightMostColumn(procPerRow)) then
        call sideflowMPIAllExchange(g, ip, jp, kp, procPerRow)
    end if
#else
    do k = 1,km
        do i = 1,im
            g(i, 0,k) = g(i,jm  ,k)
        end do
    end do
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
    call exchangeAll2DHalos3DRealArray(f, ip, jp, kp, procPerRow)
    call exchangeAll2DHalos3DRealArray(g, ip, jp, kp, procPerRow)
    call exchangeAll2DHalos3DRealArray(h, ip, jp, kp, procPerRow)
#endif
end subroutine bondFG                                    

end module module_bondFG

