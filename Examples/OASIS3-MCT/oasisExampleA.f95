program oasisExampleA
use netcdf
use mod_oasis
use oasisHelpers
implicit none

! Based off of https://github.com/ScottWales/oasis-pingpong
! Updated for OASIS3-MCT

character(len=6), parameter :: componentName = "A"
character(len=8), parameter :: componentInField = "ainxxxxx"
character(len=8), parameter :: componentOutField = "aoutxxxx"
character(len=4), parameter :: gridNamePrefix = "axxx"
integer, parameter :: nlon = 20, nlat = 10

call main()

contains

subroutine main()
    implicit none
    integer :: componentId, ierror, info, i, j, time
    real(kind=8), dimension(nlon, nlat) :: lat, lon
    integer, dimension(nlon, nlat) :: mask
    real(kind=8), dimension(nlon, nlat) :: outfield, infield
    integer :: outid, inid, partid
    integer, dimension(3) :: partition
    integer, dimension(2) :: ndims
    integer, dimension(4) :: dims
 
    do j=1, nlat
        do i=1, nlon
            lon(i,j) = nlon/20.0 * i
            lat(i,j) = nlat/10.0 * j
        end do
    end do

    call oasis_init_comp(componentId, componentName, ierror)
    call checkIError(ierror, componentId, componentName)
    
    call oasis_write_grid(gridNamePrefix, nlon, nlat, lon, lat)
    mask = 0
    call oasis_write_mask(gridNamePrefix, nlon, nlat, mask)
    
    partition = (/0,0,nlon*nlat/)
    call oasis_def_partition(partid, partition, ierror)
    call checkIError(ierror, componentId, componentName)
    
    ndims = (/2,1/)
    dims = (/1, nlon, 1, nlat/)
    call oasis_def_var(outid, componentOutField, partid, ndims, OASIS_Out, dims, OASIS_Real, ierror)
    call checkIError(ierror, componentId, componentName)
    call oasis_def_var(inid, componentInField, partid, ndims, OASIS_In, dims, OASIS_Real, ierror)
    call checkIError(ierror, componentId, componentName)
    call oasis_enddef(ierror)
    call checkIError(ierror, componentId, componentName)
    
    do time=0,140,10
        call oasis_get(inid, time, infield, info)
        write (*,*) componentName//" get status ", time, info
        call oasis_put(outid, time+10, outfield, info)
        write (*,*) componentName//" put status ", time+10, info
    end do

    call oasis_terminate(ierror)
    call checkIError(ierror, componentId, componentName)
end subroutine main

end program oasisExampleA

