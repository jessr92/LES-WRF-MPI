module gmcf_mpi_equivalent
use gmcfAPI
implicit none

! This is currently a skeleton with only communication between LES instances
! in mind.

contains

subroutine GMCF_MPI_AllReduceInPlaceRealSum(value)
    implicit none
    real(kind=4), intent(inout) :: value
    logical :: master
    ! master = <0th instance>
    ! Equivalent to
    ! MPI_Gather(xs)
    ! if (master) then
    !     do for all received values - value
    !         x = x + value
    !     end do
    ! end if
    ! MPI_Broadcast(x)
end subroutine GMCF_API_AllReduceInPlaceRealSum

subroutine GMCF_MPI_AllReduceInPlaceRealMax(value)
    implicit none
    real(kind=4), intent(inout) :: value
    logical :: master
    ! master = <0th instance>
    ! Equivalent to
    ! MPI_Gather(xs)
    ! if (master) then
    !     do for all recieved values - value
    !         if (value > x) then
    !             x = value
    !         end if
    !     end do
    ! end if
    ! MPI_Broadcast(x)          
end subroutine GMCF_API_AllReduceInPlaceRealMax

subroutine GMCF_MPI_AllReduceInPlaceRealMin(value)
    implicit none
    real(kind=4), intent(inout) :: value
    logical :: master
    ! master = <0th instance>
    ! Equivalent to
    ! MPI_Gather(xs)
    ! if (master) then
    !     do for all recieved values - value
    !         if (value < x) then
    !             x = value
    !         end if
    !     end do
    ! end if
    ! MPI_Broadcast(x) 
end subroutine GMCF_API_AllReduceInPlaceRealMin

subroutine GMCF_MPI_Gather()
    implicit none
    logical :: master
    ! master = <0th instance>
    ! Equivalent to
    ! if (master) then
    !     do for all other processes
    !         MPI_Recv()
    !     end do
    ! else
    !     MPI_Send()
    ! end if
end subroutine GMCF_MPI_Gather

subroutine GMCF_MPI_Broadcast(value)
    implicit none
    real(kind=4), intent(inout) :: value    
    logical :: master
    ! master = <0th instance>
    ! Equivalent to
    ! if (master) then
    !     do for all other processes
    !         MPI_Send()
    !     end do
    ! else
    !     MPI_Recv()
    ! end if
end subroutine GMCF_MPI_Broadcast

subroutine GMCF_MPI_ISend1DFloatArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:) :: array
    integer :: pre_post
    pre_post = PRE ! Arbitrary
    call gmcfSend1DFloatArray(rank, array, size(array), tag, destination, pre_post, time)
end subroutine GMCF_MPI_ISend1DFloatArray

subroutine GMCF_MPI_Send1DFloatArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:) :: array
    ! Equivalent to
    ! GMCF_MPI_ISend1DFloatArray
    ! No wait call required as the GMCF framework makes a copy of array so it
    ! is safe to reuse array straight away even though there is no blocking
    ! call.
    call GMCF_MPI_ISend1DFloatArray(rank, array, tag, destination, time)
end subroutine GMCF_MPI_Send1DFloatArray

subroutine GMCF_MPI_IRecv1DFloatArray()
    implicit none
    ! Equivalent to
    ! Nothing in this case? Within models doesn't need to request data from
    ! the sender unlike between models which does. Also MPI doesn't guarantee
    ! IRecv actually has any valid data until after MPI_Wait() returns. Since
    ! GMCF doesn't give unique labels to each request then there is nothing to
    ! do here...
end subroutine GMCF_MPI_IRecv1DFloatArray

subroutine GMCF_MPI_Recv1DFloatArray()
    implicit none
    ! Equivalent to
    call GMCF_MPI_IRecv1DFloatArray()
    ! GMCF_MPI_Wait()
end subroutine GMCF_MPI_Recv1DFloatArray

subroutine GMCF_MPI_ISend3DFloatArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:,:,:) :: array
    integer :: pre_post
    pre_post = PRE ! Arbitrary
    call gmcfSend3DFloatArray(rank, array, size(array), tag, destination, pre_post, time)
end subroutine GMCF_MPI_ISend3DFloatArray

subroutine GMCF_MPI_Send3DFloatArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:,:,:) :: array
    ! Equivalent to
    ! GMCF_MPI_ISend3DFloatArray
    ! No wait call required as the GMCF framework makes a copy of array so it
    ! is safe to reuse array straight away even though there is no blocking
    ! call.
    call GMCF_MPI_ISend3DFloatArray(rank, array, tag, destination, time)
end subroutine GMCF_MPI_Send3DFloatArray

subroutine GMCF_MPI_IRecv3DFloatArray()
    implicit none
    ! Equivalent to
    ! Nothing in this case? Within models doesn't need to request data from
    ! the sender unlike between models which does. Also MPI doesn't guarantee
    ! IRecv actually has any valid data until after MPI_Wait() returns. Since
    ! GMCF doesn't give unique labels to each request then there is nothing to
    ! do here...
end subroutine GMCF_MPI_IRecv3DFloatArray

subroutine GMCF_MPI_Recv3DFloatArray()
    implicit none
    ! Equivalent to
    call GMCF_MPI_IRecv3DFloatArray()
    ! GMCF_MPI_Wait()
end subroutine GMCF_MPI_Recv3DFloatArray

subroutine GMCF_MPI_Wait()
    implicit none
    ! Equivalent to
    ! gmcfHasPackets()
    ! do while (hasPackets)
    !     gmcfShiftPending()
    !     select case (...)
    !         gmcfRead(1D/3D)FloatArray()
    !     etc ...
    ! end do
end subroutine GMCF_MPI_Wait

end module
