#ifdef GMCF
    subroutine program_les(sys, tile, model_id)
#else
      program main
#endif
        use module_init 
        use module_grid 
        use module_set 
        use module_timdata 
#ifdef TIMSERIS_FIXED
        use module_timseris
#endif
        use module_aveflow
        use module_ifdata 
#if IANIME == 1      
        use module_anime 
#endif      
#ifdef _OPENCL_LES_WV
        use module_LES_combined_ocl
#else        
        use module_velnw 
        use module_bondv1 
        use module_velFG 
#if IFBF == 1      
        use module_feedbf 
#endif      
        use module_les 
        use module_press 
        use module_adam 
#endif        
        use common_sn 
#ifdef GMCF
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
#endif
        real(kind=4) :: alpha
        integer :: ianime
        integer :: ical
        integer :: ifbf
        integer :: im
        integer :: jm
        integer :: km
        integer :: n
        integer :: n0
        integer :: n1
        integer :: nmax
        real(kind=4) :: beta
        character(len=70) :: data10
        character(len=70) :: data11
        character(len=70) :: data20
        character(len=70) :: data21
        character(len=70) :: data22
        character(len=70) :: data23
        character(len=70) :: data24
        character(len=70) :: data25
        character(len=70) :: data26
        character(len=70) :: data27
        character(len=70) :: data30
        character(len=70) :: data31
        real(kind=4) :: dt
        real(kind=4) :: ro
        real(kind=4) :: time
        real(kind=4) :: vn
#ifdef TIMINGS
        integer :: clock_rate
#endif

#ifdef GMCF
    real(kind=4), dimension(:,:,:), allocatable :: amask1
    real(kind=4), dimension(:,:,:), allocatable :: avel
    real(kind=4), dimension(:,:,:), allocatable :: avep
    real(kind=4), dimension(:,:,:), allocatable :: avesm
    real(kind=4), dimension(:,:,:), allocatable :: avesmsm
    real(kind=4), dimension(:,:), allocatable :: avesu
    real(kind=4), dimension(:,:), allocatable :: avesuu
    real(kind=4), dimension(:,:), allocatable :: avesv
    real(kind=4), dimension(:,:), allocatable :: avesvv
    real(kind=4), dimension(:,:), allocatable :: avesw
    real(kind=4), dimension(:,:), allocatable :: avesww
    real(kind=4), dimension(:,:,:), allocatable :: aveu
    real(kind=4), dimension(:,:,:), allocatable :: aveuu
    real(kind=4), dimension(:,:,:), allocatable :: avev
    real(kind=4), dimension(:,:,:), allocatable :: avevv
    real(kind=4), dimension(:,:,:), allocatable :: avew
    real(kind=4), dimension(:,:,:), allocatable :: aveww
    real(kind=4), dimension(:,:,:), allocatable :: bmask1
    real(kind=4), dimension(:,:,:), allocatable :: cmask1
    real(kind=4), dimension(:,:,:), allocatable :: cn1
    real(kind=4), dimension(:), allocatable :: cn2l
    real(kind=4), dimension(:), allocatable :: cn2s
    real(kind=4), dimension(:), allocatable :: cn3l
    real(kind=4), dimension(:), allocatable :: cn3s
    real(kind=4), dimension(:), allocatable :: cn4l
    real(kind=4), dimension(:), allocatable :: cn4s
    real(kind=4), dimension(:,:,:), allocatable :: cov1
    real(kind=4), dimension(:,:,:), allocatable :: cov2
    real(kind=4), dimension(:,:,:), allocatable :: cov3
    real(kind=4), dimension(:,:,:), allocatable :: cov4
    real(kind=4), dimension(:,:,:), allocatable :: cov5
    real(kind=4), dimension(:,:,:), allocatable :: cov6
    real(kind=4), dimension(:,:,:), allocatable :: cov7
    real(kind=4), dimension(:,:,:), allocatable :: cov8
    real(kind=4), dimension(:,:,:), allocatable :: cov9
    real(kind=4), dimension(:), allocatable :: delx1
    real(kind=4), dimension(:,:,:), allocatable :: dfu1
    real(kind=4), dimension(:,:,:), allocatable :: dfv1
    real(kind=4), dimension(:,:,:), allocatable :: dfw1
    real(kind=4), dimension(:,:,:), allocatable :: diu1
    real(kind=4), dimension(:,:,:), allocatable :: diu2
    real(kind=4), dimension(:,:,:), allocatable :: diu3
    real(kind=4), dimension(:,:,:), allocatable :: diu4
    real(kind=4), dimension(:,:,:), allocatable :: diu5
    real(kind=4), dimension(:,:,:), allocatable :: diu6
    real(kind=4), dimension(:,:,:), allocatable :: diu7
    real(kind=4), dimension(:,:,:), allocatable :: diu8
    real(kind=4), dimension(:,:,:), allocatable :: diu9
    real(kind=4), dimension(:,:,:), allocatable :: dmask1
    real(kind=4), dimension(:), allocatable :: dx1
    real(kind=4), dimension(:), allocatable :: dxl
    real(kind=4), dimension(:), allocatable :: dxs
    real(kind=4), dimension(:), allocatable :: dy1
    real(kind=4), dimension(:), allocatable :: dyl
    real(kind=4), dimension(:), allocatable :: dys
    real(kind=4), dimension(:), allocatable :: dzn
    real(kind=4), dimension(:), allocatable :: dzs
    real(kind=4), dimension(:,:,:), allocatable :: f
#if ICAL == 1
    real(kind=4), dimension(:,:,:), allocatable :: fghold
#endif
    real(kind=4), dimension(:,:,:), allocatable :: fold
    real(kind=4), dimension(:,:,:), allocatable :: fx
    real(kind=4), dimension(:,:,:), allocatable :: fy
    real(kind=4), dimension(:,:,:), allocatable :: fz
    real(kind=4), dimension(:,:,:), allocatable :: g
    real(kind=4), dimension(:,:,:), allocatable :: gold
    real(kind=4), dimension(:,:,:), allocatable :: h
    real(kind=4), dimension(:,:,:), allocatable :: hold
#ifndef _OPENCL_LES_WV
    real(kind=4), dimension(:,:,:), allocatable :: fghold
#endif
    real(kind=4), dimension(:,:,:), allocatable :: nou1
    real(kind=4), dimension(:,:,:), allocatable :: nou2
    real(kind=4), dimension(:,:,:), allocatable :: nou3
    real(kind=4), dimension(:,:,:), allocatable :: nou4
    real(kind=4), dimension(:,:,:), allocatable :: nou5
    real(kind=4), dimension(:,:,:), allocatable :: nou6
    real(kind=4), dimension(:,:,:), allocatable :: nou7
    real(kind=4), dimension(:,:,:), allocatable :: nou8
    real(kind=4), dimension(:,:,:), allocatable :: nou9
    real(kind=4), dimension(:,:,:), allocatable :: p
    real(kind=4), dimension(:,:,:), allocatable :: rhs
    real(kind=4), dimension(:,:,:), allocatable :: sm
    real(kind=4), dimension(:,:,:), allocatable :: u
    real(kind=4), dimension(:,:,:), allocatable :: usum
    real(kind=4), dimension(:,:,:), allocatable :: uwfx
    real(kind=4), dimension(:,:), allocatable :: uwfxs
    real(kind=4), dimension(:,:,:), allocatable :: v
    real(kind=4), dimension(:,:,:), allocatable :: vsum
    real(kind=4), dimension(:,:,:), allocatable :: w
    real(kind=4), dimension(:,:,:), allocatable :: wsum
    real(kind=4), dimension(:), allocatable :: z2
    real(kind=4), dimension(:,:), allocatable :: zbm
#ifdef TIMINGS
    integer(kind=4), dimension(:), allocatable :: timestamp
#endif
#else
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: amask1
    real(kind=4), dimension(ip,jp,kp)  :: avel
    real(kind=4), dimension(ip,jp,kp)  :: avep
    real(kind=4), dimension(ip,jp,kp)  :: avesm
    real(kind=4), dimension(ip,jp,kp)  :: avesmsm
    real(kind=4), dimension(ip,kp)  :: avesu
    real(kind=4), dimension(ip,kp)  :: avesuu
    real(kind=4), dimension(ip,kp)  :: avesv
    real(kind=4), dimension(ip,kp)  :: avesvv
    real(kind=4), dimension(ip,kp)  :: avesw
    real(kind=4), dimension(ip,kp)  :: avesww
    real(kind=4), dimension(ip,jp,kp)  :: aveu
    real(kind=4), dimension(ip,jp,kp)  :: aveuu
    real(kind=4), dimension(ip,jp,kp)  :: avev
    real(kind=4), dimension(ip,jp,kp)  :: avevv
    real(kind=4), dimension(ip,jp,kp)  :: avew
    real(kind=4), dimension(ip,jp,kp)  :: aveww
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1)  :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: cmask1
    real(kind=4), dimension(ip,jp,kp)  :: cn1
    real(kind=4), dimension(ip)  :: cn2l
    real(kind=4), dimension(ip)  :: cn2s
    real(kind=4), dimension(jp)  :: cn3l
    real(kind=4), dimension(jp)  :: cn3s
    real(kind=4), dimension(kp)  :: cn4l
    real(kind=4), dimension(kp)  :: cn4s
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: cov5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: cov9
    real(kind=4), dimension(kp)  :: delx1
    real(kind=4), dimension(0:ip,jp,kp)  :: dfu1
    real(kind=4), dimension(ip,0:jp,kp)  :: dfv1
    real(kind=4), dimension(ip,jp,kp)  :: dfw1
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: diu1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: diu5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: diu9
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: dmask1
    real(kind=4), dimension(-1:ip+1)  :: dx1
    real(kind=4), dimension(0:ip)  :: dxl
    real(kind=4), dimension(0:ip)  :: dxs
    real(kind=4), dimension(0:jp+1)  :: dy1
    real(kind=4), dimension(0:jp)  :: dyl
    real(kind=4), dimension(0:jp)  :: dys
    real(kind=4), dimension(-1:kp+2)  :: dzn
    real(kind=4), dimension(-1:kp+2)  :: dzs
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: f
#if ICAL == 1
    real(kind=4), dimension(ip,jp,kp)  :: fghold
#endif
    real(kind=4), dimension(ip,jp,kp)  :: fold
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fx
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fy
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: fz
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: g
    real(kind=4), dimension(ip,jp,kp)  :: gold
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: h
    real(kind=4), dimension(ip,jp,kp)  :: hold
#ifndef _OPENCL_LES_WV
    real(kind=4), dimension(ip,jp,kp)  :: fghold
#endif
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: nou1
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou2
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou3
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou4
    real(kind=4), dimension(-1:ip+2,0:jp+2,0:kp+2)  :: nou5
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou6
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou7
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou8
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+2)  :: nou9
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1)  :: p
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1)  :: rhs
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1)  :: sm
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: u
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: usum
    real(kind=4), dimension(ip,jp,kp)  :: uwfx
    real(kind=4), dimension(ip,kp)  :: uwfxs
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: v
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: vsum
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1)  :: w
    real(kind=4), dimension(0:ip,0:jp,0:kp)  :: wsum
    real(kind=4), dimension(kp+2)  :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1)  :: zbm
#ifdef TIMINGS
    integer (kind=4), dimension(0:9) :: timestamp
#endif
#endif

#ifdef GMCF
    allocate(amask1((ip+1)-(0)+1,(jp+1)-(0)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(amask1)
    allocate(avel((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avel)
    allocate(avep((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avep)
    allocate(avesm((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avesm)
    allocate(avesmsm((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avesmsm)
    allocate(avesu((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(avesu)
    allocate(avesuu((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(avesuu)
    allocate(avesv((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(avesv)
    allocate(avesvv((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(avesvv)
    allocate(avesw((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(avesw)
    allocate(avesww((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(avesww)
    allocate(aveu((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(aveu)
    allocate(aveuu((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(aveuu)
    allocate(avev((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avev)
    allocate(avevv((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avevv)
    allocate(avew((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(avew)
    allocate(aveww((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(aveww)
    allocate(bmask1((ip+1)-(-1)+1,(jp+1)-(0)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(bmask1)
    allocate(cmask1((ip+1)-(0)+1,(jp+1)-(-1)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(cmask1)
    allocate(cn1((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(cn1)
    allocate(cn2l((ip)-(1)+1))
    call zero1DReal4Array(cn2l)
    allocate(cn2s((ip)-(1)+1))
    call zero1DReal4Array(cn2s)
    allocate(cn3l((jp)-(1)+1))
    call zero1DReal4Array(cn3l)
    allocate(cn3s((jp)-(1)+1))
    call zero1DReal4Array(cn3s)
    allocate(cn4l((kp)-(1)+1))
    call zero1DReal4Array(cn4l)
    allocate(cn4s((kp)-(1)+1))
    call zero1DReal4Array(cn4s)
    allocate(cov1((ip+2)-(-1)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov1)
    allocate(cov2((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov2)
    allocate(cov3((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov3)
    allocate(cov4((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov4)
    allocate(cov5((ip+2)-(-1)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov5)
    allocate(cov6((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov6)
    allocate(cov7((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov7)
    allocate(cov8((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov8)
    allocate(cov9((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(cov9)
    allocate(delx1((kp)-(1)+1))
    call zero1DReal4Array(delx1)
    allocate(dfu1((ip)-(0)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(dfu1)
    allocate(dfv1((ip)-(1)+1,(jp)-(0)+1,(kp)-(1)+1))
    call zero3DReal4Array(dfv1)
    allocate(dfw1((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(dfw1)
    allocate(diu1((ip+2)-(-1)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu1)
    allocate(diu2((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu2)
    allocate(diu3((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu3)
    allocate(diu4((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu4)
    allocate(diu5((ip+2)-(-1)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu5)
    allocate(diu6((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu6)
    allocate(diu7((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu7)
    allocate(diu8((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu8)
    allocate(diu9((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(diu9)
    allocate(dmask1((ip+1)-(0)+1,(jp+1)-(0)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(dmask1)
    allocate(dx1((ip+1)-(-1)+1))
    call zero1DReal4Array(dx1)
    allocate(dxl((ip)-(0)+1))
    call zero1DReal4Array(dxl)
    allocate(dxs((ip)-(0)+1))
    call zero1DReal4Array(dxs)
    allocate(dy1((jp+1)-(0)+1))
    call zero1DReal4Array(dy1)
    allocate(dyl((jp)-(0)+1))
    call zero1DReal4Array(dyl)
    allocate(dys((jp)-(0)+1))
    call zero1DReal4Array(dys)
    allocate(dzn((kp+2)-(-1)+1))
    call zero1DReal4Array(dzn)
    allocate(dzs((kp+2)-(-1)+1))
    call zero1DReal4Array(dzs)
    allocate(f((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(f)
#if ICAL==1
    allocate(fghold((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(fghold)
#endif
    allocate(fold((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(fold)
    allocate(fx((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(fx)
    allocate(fy((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(fy)
    allocate(fz((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(fz)
    allocate(g((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(g)
    allocate(gold((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(gold)
    allocate(h((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(h)
    allocate(hold((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(hold)
#ifndef _OPENCL_LES_WV
    allocate(fghold((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(fghold)
#endif
    allocate(nou1((ip+2)-(-1)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou1)
    allocate(nou2((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou2)
    allocate(nou3((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou3)
    allocate(nou4((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou4)
    allocate(nou5((ip+2)-(-1)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou5)
    allocate(nou6((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou6)
    allocate(nou7((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou7)
    allocate(nou8((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou8)
    allocate(nou9((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+2)-(0)+1))
    call zero3DReal4Array(nou9)
    allocate(p((ip+2)-(0)+1,(jp+2)-(0)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(p)
    allocate(rhs((ip+1)-(0)+1,(jp+1)-(0)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(rhs)
    allocate(sm((ip+1)-(-1)+1,(jp+1)-(-1)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(sm)
    allocate(u((ip+1)-(0)+1,(jp+1)-(-1)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(u)
    allocate(usum((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(usum)
    allocate(uwfx((ip)-(1)+1,(jp)-(1)+1,(kp)-(1)+1))
    call zero3DReal4Array(uwfx)
    allocate(uwfxs((ip)-(1)+1,(kp)-(1)+1))
    call zero2DReal4Array(uwfxs)
    allocate(v((ip+1)-(0)+1,(jp+1)-(-1)+1,(kp+1)-(0)+1))
    call zero3DReal4Array(v)
    allocate(vsum((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(vsum)
    allocate(w((ip+1)-(0)+1,(jp+1)-(-1)+1,(kp+1)-(-1)+1))
    call zero3DReal4Array(w)
    allocate(wsum((ip)-(0)+1,(jp)-(0)+1,(kp)-(0)+1))
    call zero3DReal4Array(wsum)
    allocate(z2((kp+2)-(1)+1))
    call zero1DReal4Array(z2)
    allocate(zbm((ipmax+1)-(-1)+1,(jpmax+1)-(-1)+1))
    call zero2DReal4Array(zbm)
#ifdef TIMINGS
    allocate(timestamp((9)-(0)+1))
    !call zero1DReal4Array(timestamp)
#endif
#endif

! -----------------------------------------------------------------------
!
#ifdef GMCF
        print*, 'Hello from model ', model_id
        call initialise_gmcf(sys, tile, model_id, procPerRow, procPerCol)
#endif
#ifdef MPI
      call initialise_mpi()
      if (mpi_size .ne. procPerRow * procPerCol) then
          print*, 'Needed ', (procPerRow * procPerCol), ' processes, got ', mpi_size
          call MPI_Abort(communicator, 1, ierror)
      end if
      call setupCartesianVirtualTopology(dimensions, dimensionSizes, & 
                                         periodicDimensions, coordinates, &
                                         neighbours, reorder)

#endif
#ifdef USE_NETCDF_OUTPUT
    call init_netcdf_file()
#endif
      call set(data10,data11,data20,data21,data22,data23,data24,data25,data26,data27,data30,data31, &
      im,jm,km,ifbf,ianime,ical,n0,n1,nmax,dt,ro,vn,alpha,beta)
      call grid(dx1,dxl,dy1,dyl,z2,dzn,dzs,dxs,dys)
      call timdata()
      call init(km,jm,im,u,v,w,p,cn2s,dxs,cn2l,cn3s,dys,cn3l,dzs,cn4s,cn4l,cn1,amask1,bmask1, &
      cmask1,dmask1,zbm,z2,dzn)
      n=n0
      call ifdata( &
#if ICAL == 1
      data30,data31, fold,gold,hold,fghold, time &
#endif
      n,u,im,jm,km,v,w,p,usum,vsum,wsum, &
      delx1,dx1,dy1,dzn,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g,h,z2,dt,dxs,cov1, &
      cov2,cov3,dfu1,vn,cov4,cov5,cov6,dfv1,cov7,cov8,cov9,dfw1,dzs,nou1,nou5,nou9,nou2,nou3,nou4, &
      nou6,nou7,nou8,bmask1,cmask1,dmask1,alpha,beta,fx,fy,fz,amask1,zbm)

#ifdef _OPENCL_LES_WV
      call initialise_LES_kernel( &
            p,u,v,w,usum,vsum,wsum,f,g,h,fold,gold,hold, &
            diu1, diu2, diu3, diu4, diu5, diu6, diu7, diu8, diu9, &
            amask1, bmask1, cmask1, dmask1, &
            cn1, cn2l, cn2s, cn3l, cn3s, cn4l, cn4s, &
            rhs, sm, dxs, dys, dzs, dx1, dy1, dzn, z2, &
            dt, im, jm, km &
              )
#endif


#ifdef VERBOSE
#ifdef _OPENCL_LES_WV
    print *,'MAIN: calling OpenCL run_LES_kernel for ', nmax-n0+1, ' time steps, domain = ',im,'x',jm,'x',km
#else
    print *,'MAIN: running reference LES code for ', nmax-n0+1, ' time steps, domain = ',im,'x',jm,'x',km
#endif
#endif
! --main loop
#ifdef TIMINGS
    nmax=201
    call system_clock(timestamp(8), clock_rate)
#endif
      do n = n0,nmax
        time = float(n-1)*dt
! -------calculate turbulent flow--------c
#ifdef _OPENCL_LES_WV
      call run_LES_kernel ( &
            n, nmax &
            )
#else
! -------calculate turbulent flow--------c
#ifdef TIMINGS
         print *, 'run_LES_reference: time step = ',n
#endif
        ! ========================================================================================================================================================
        ! ========================================================================================================================================================
#ifdef TIMINGS
        call system_clock(timestamp(0), clock_rate)
#endif
        call velnw(km,jm,im,p,ro,dxs,u,dt,f,dys,v,g,dzs,w,h)
#ifdef TIMINGS
        call system_clock(timestamp(1), clock_rate)
#endif
        call bondv1(jm,u,z2,dzn,v,w,km,n,im,dt,dxs)
#ifdef TIMINGS
        call system_clock(timestamp(2), clock_rate)
#endif
        call velfg(km,jm,im,dx1,cov1,cov2,cov3,dfu1,diu1,diu2,dy1,diu3,dzn,vn,f,cov4,cov5,cov6,dfv1, &
      diu4,diu5,diu6,g,cov7,cov8,cov9,dfw1,diu7,diu8,diu9,dzs,h,nou1,u,nou5,v,nou9,w,nou2,nou3, &
      nou4,nou6,nou7,nou8)
#ifdef TIMINGS
        call system_clock(timestamp(3), clock_rate)
#endif
#if IFBF == 1
        call feedbf(km,jm,im,usum,u,bmask1,vsum,v,cmask1,wsum,w,dmask1,alpha,dt,beta,fx,fy,fz,f,g, &
      h)
#endif
#ifdef TIMINGS
        call system_clock(timestamp(4), clock_rate)
#endif
        call les(km,delx1,dx1,dy1,dzn,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g,h)
#ifdef TIMINGS
        call system_clock(timestamp(5), clock_rate)
#endif
        call adam(n,nmax,data21,fold,im,jm,km,gold,hold,fghold,f,g,h)
#ifdef TIMINGS
        call system_clock(timestamp(6), clock_rate)
#endif
        call press(km,jm,im,rhs,u,dx1,v,dy1,w,dzn,f,g,h,dt,cn1,cn2l,p,cn2s,cn3l,cn3s,cn4l,cn4s,n, &
      nmax,data20,usum,vsum,wsum)
#ifdef TIMINGS
        call system_clock(timestamp(7), clock_rate)
        do i=1, 7
            print '("Time for state ",i2," = ",f6.3," s")',i,(timestamp(i)-timestamp(i-1))/ real(clock_rate)
        end do
#endif

#endif		
! -------data output ---------------------c
! WV: This is clearly broken, as the dimensions for u/v/w are 150x150x90
#ifdef TIMSERIS_FIXED
        call timseris(n,dt,u,v,w)
#endif
        call aveflow(n,n1,km,jm,im,aveu,avev,avew,avep,avel,aveuu,avevv,aveww,avesm,avesmsm,uwfx, &
      avesu,avesv,avesw,avesuu,avesvv,avesww,u,v,w,p,sm,nmax,uwfxs,data10,time,data11)
#if IANIME == 1
        call anime(n,n0,nmax,km,jm,im,dxl,dx1,dyl,dy1,z2,data22,data23,u,w,v,amask1)
#endif
!
      end do
#ifdef USE_NETCDF_OUTPUT
    call close_netcdf_file()
#endif
#ifdef TIMINGS
    call system_clock(timestamp(9))
    print *,"Total time:" ,(timestamp(9)-timestamp(8))/real(clock_rate),"s for ",nmax-n0,"iterations"
#endif

#ifdef GMCF_API
      call finalise_gmcf(model_id)
#else
#ifdef MPI
      call finalise_mpi()
#endif
#endif
#ifdef GMCF
    deallocate(amask1)
    deallocate(avel)
    deallocate(avep)
    deallocate(avesm)
    deallocate(avesmsm)
    deallocate(avesu)
    deallocate(avesuu)
    deallocate(avesv)
    deallocate(avesvv)
    deallocate(avesw)
    deallocate(avesww)
    deallocate(aveu)
    deallocate(aveuu)
    deallocate(avev)
    deallocate(avevv)
    deallocate(avew)
    deallocate(aveww)
    deallocate(bmask1)
    deallocate(cmask1)
    deallocate(cn1)
    deallocate(cn2l)
    deallocate(cn2s)
    deallocate(cn3l)
    deallocate(cn3s)
    deallocate(cn4l)
    deallocate(cn4s)
    deallocate(cov1)
    deallocate(cov2)
    deallocate(cov3)
    deallocate(cov4)
    deallocate(cov5)
    deallocate(cov6)
    deallocate(cov7)
    deallocate(cov8)
    deallocate(cov9)
    deallocate(delx1)
    deallocate(dfu1)
    deallocate(dfv1)
    deallocate(dfw1)
    deallocate(diu1)
    deallocate(diu2)
    deallocate(diu3)
    deallocate(diu4)
    deallocate(diu5)
    deallocate(diu6)
    deallocate(diu7)
    deallocate(diu8)
    deallocate(diu9)
    deallocate(dmask1)
    deallocate(dx1)
    deallocate(dxl)
    deallocate(dxs)
    deallocate(dy1)
    deallocate(dyl)
    deallocate(dys)
    deallocate(dzn)
    deallocate(dzs)
    deallocate(f)
#if ICAL==1
    deallocate(fghold)
#endif
    deallocate(fold)
    deallocate(fx)
    deallocate(fy)
    deallocate(fz)
    deallocate(g)
    deallocate(gold)
    deallocate(h)
    deallocate(hold)
#ifndef _OPENCL_LES_WV
    deallocate(fghold)
#endif
    deallocate(nou1)
    deallocate(nou2)
    deallocate(nou3)
    deallocate(nou4)
    deallocate(nou5)
    deallocate(nou6)
    deallocate(nou7)
    deallocate(nou8)
    deallocate(nou9)
    deallocate(p)
    deallocate(rhs)
    deallocate(sm)
    deallocate(u)
    deallocate(usum)
    deallocate(uwfx)
    deallocate(uwfxs)
    deallocate(v)
    deallocate(vsum)
    deallocate(w)
    deallocate(wsum)
    deallocate(z2)
    deallocate(zbm)
#ifdef TIMINGS
    deallocate(timestamp)
#endif
#endif
#ifdef GMCF
end subroutine program_les
#else
      end program
#endif


