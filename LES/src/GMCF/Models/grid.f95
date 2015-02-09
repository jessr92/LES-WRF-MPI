module module_grid

contains

      subroutine grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)
      use common_sn ! create_new_include_statements() line 102
        real(kind=4), dimension(-1:ip+1) , intent(Out) :: dx1
        real(kind=4), dimension(0:ip) , intent(Out) :: dxl
        real(kind=4), dimension(0:ip) , intent(Out) :: dxs
        real(kind=4), dimension(0:jp+1) , intent(Out) :: dy1
        real(kind=4), dimension(0:jp) , intent(Out) :: dyl
        real(kind=4), dimension(0:jp) , intent(Out) :: dys
        real(kind=4), dimension(-1:kp+2) , intent(Out) :: dzn
        real(kind=4), dimension(-1:kp+2) , intent(Out) :: dzs
        real(kind=4), dimension(kp+2) , intent(Out) :: z2
#ifdef MPI
        real(kind=4), dimension(-1:(ip*procPerCol)+1) :: dx1Tot
        real(kind=4), dimension(0:(jp*procPerRow)+1) :: dy1Tot
        real(kind=4), dimension(0:ip*procPerCol) :: dxlTot
        real(kind=4), dimension(0:jp*procPerRow) :: dylTot
#endif
! 
! WV: I think the use of run-time im,jp,kp is dangerous, unless they are identical to ip,jp,kp
! WV: So I changed it to be that way
! --dx set; streamwise direction
! WV: so -1 and ip+1 are not set!!! I changed it analogous to dy1
!      do i = 0,ip
#ifdef MPI
    if (isMaster()) then
        do i=-1,(ip*procPerCol)+1
            dx1Tot(i) = 20.
        end do
    end if
    call distribute1DRealRowWiseArray(dx1Tot,dx1, 2, 1, procPerRow)
#else
      do i = -1,ip+1
       dx1(i) = 20.
      end do
#endif

#ifdef MPI
    if (isMaster()) then
        dxlTot(0) = 0.
        do i = 1, ip*procPerCol
            dxlTot(i) = dxlTot(i-1) + dx1Tot(i)
        end do
    end if
    call distribute1DRealRowWiseArray(dxlTot,dxl, 1, 0, procPerRow)
#else
      dxl(0) = 0.
      do i = 1,ip
       dxl(i) = dxl(i-1)+dx1(i)
      end do
#endif

! --dy set; spanwise direction
!WV: let's set the *whole* array to this value!
      !do j = 0,jp
#ifdef MPI
    if (isMaster()) then
        do j=0,(jp*procPerRow)+1
            dy1Tot(j) = 20.
        end do
    end if
    call distribute1DRealColumnWiseArray(dy1Tot, dy1, 1, 1, procPerRow)
#else
      do j = 0,jp+1
       dy1(j) = 20.
      end do
#endif

#ifdef MPI
    if (isMaster()) then
        dylTot(0) = 0.
        do j=1,(jp*procPerRow)
            dylTot(j) = dylTot(j-1) + dy1Tot(j)
        end do
    end if
    call distribute1DRealColumnWiseArray(dylTot, dyl, 1, 0, procPerRow)
#else
      dyl(0) = 0. 
      do j = 1,jp
       dyl(j) = dyl(j-1)+dy1(j)
      end do
#endif
! --dz set; vertical direction
!WV: also define the first and last point in the array!
      do k = 0,1
        z2(k) = 2.5
        dzn(k) = 2.5
      end do      
      do k = 2,15
        dzn(k) = dzn(k-1)*1.05
      end do
      do k = 16,44
        dzn(k) = 5.
      end do
      do k = 45,kp+1
        dzn(k) = dzn(k-1)*1.0459
      end do
      do k = 2,kp+2 ! WV: was kp+1
        z2(k) = z2(k-1)+dzn(k)
      end do      
      ! so z2(kp+2) is not set, why?
! --gaiten deno haba
      dzn(kp+1) = dzn(kp)
      !WV
      dzn(kp+2) = dzn(kp+1)
      dzn(0) = dzn(1)
      !WV
      dzn(-1)=dzn(0)
! -------------------------------------
      do k = 0,kp
        dzs(k) = dzn(k+1)/2.+dzn(k)/2.
      end do
! GR: dxs is defined from 0:ip but ip+1 is written to
! GR: dx1 is defined from -1 to ip+1 but ip+2 is read from
      !do i = 0,ip+1
      do i=0, ip
        dxs(i) = dx1(i)/2.+dx1(i+1)/2.
      end do
! WV: so the access to the undefine dy1(jp+2) seems to be what causes corruption of dy1(0)
! WV: In fact, dys is only defined (0 .. jp) so most likely they run into one another  
      !do j = 0,jp+1
      do j = 0,jp
        dys(j) = dy1(j)/2.+dy1(j+1)/2.
      end do 
! 
      dzs(kp+1) = dzs(kp)
      dzs(kp+2) = dzs(kp+1) !WV
      dzs(-1) = dzs(0) !WV
! 
#ifdef VERBOSE
      write(6,*) 'Computational Domain X,Y,Z=',dxl(ip),dyl(jp),z2(kp)
      do k = 1,kp
      write(6,*) 'Vertical grid size=',k,dzn(k)
      end do
#endif       
! 
      return
      end subroutine grid

end module module_grid

