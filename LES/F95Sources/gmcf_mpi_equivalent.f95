module gmcf_mpi_equivalent
use gmcfAPI
implicit none

! This is currently a skeleton with only communication between LES instances
! in mind.

contains

subroutine GMCF_MPI_AllReduceInPlaceRealSum(rank, value, instanceCount)
    implicit none
    integer, intent(in) :: rank
    real(kind=4), intent(inout) :: value
    real(kind=4), dimension(:), allocatable :: gatheredValues
    logical :: master
    integer :: i
    master = rank .eq. 0
    allocate(gatheredValues(instanceCount - 1))
    call GMCF_MPI_GatherSingleReal(gatheredValues, instanceCount, master)
    if (master) then
        do i=1, instanceCount
            value = value + gatheredValues(i)
        end do
    end if
    call MPI_Broadcast(value, instanceCount, master)
    deallocate(gatheredValues)
end subroutine GMCF_API_AllReduceInPlaceRealSum

subroutine GMCF_MPI_AllReduceInPlaceRealMax(value)
    implicit none
    integer, intent(in) :: rank
    real(kind=4), intent(inout) :: value
    real(kind=4), dimension(:), allocatable :: gatheredValues
    logical :: master
    integer :: i
    master = rank .eq. 0
    allocate(gatheredValues(instanceCount - 1))
    call GMCF_MPI_GatherSingleReal(gatheredValues, instanceCount, master)
    if (master) then
        do i=1, instanceCount
            if (gatheredValues(i) > value) then
                value = gatheredValues(i)
            end if
        end do
    end if
    call MPI_Broadcast(value, instanceCount, master)
    deallocate(gatheredValues)
end subroutine GMCF_API_AllReduceInPlaceRealMax

subroutine GMCF_MPI_AllReduceInPlaceRealMin(value)
    implicit none
    integer, intent(in) :: rank
    real(kind=4), intent(inout) :: value
    real(kind=4), dimension(:), allocatable :: gatheredValues
    logical :: master
    integer :: i
    master = rank .eq. 0
    allocate(gatheredValues(instanceCount - 1))
    call GMCF_MPI_GatherSingleReal(gatheredValues, instanceCount, master)
    if (master) then
        do i=1, instanceCount
            if (gatheredValues(i) < value) then
                value = gatheredValues(i)
            end if
        end do
    end if
    call MPI_Broadcast(value, instanceCount, master)
    deallocate(gatheredValues)
end subroutine GMCF_API_AllReduceInPlaceRealMin

subroutine GMCF_MPI_GatherSingleReal(gatheredValues, instanceCount, master)
    implicit none
    real(kind=4), dimension(:), intent(inout) :: gatheredValues
    integer, intent(in) :: instanceCount
    logical, intent(in) :: master
    integer :: i
    if (master) then
        do i=1, instanceCount
    !       GMCF_MPI_RecvSingleReal()
        end do
    else
    !    GMCF_MPI_SendSingleReal()
    end if
end subroutine GMCF_MPI_GatherSingleReal

subroutine GMCF_MPI_BroadcastSingleReal(value, instanceCount, master)
    implicit none
    real(kind=4), intent(inout) :: value  
    integer, intent(in) :: instanceCount  
    logical, intent(in) :: master
    integer :: i
    if (master) then
        do i=1, instanceCount
    !       GMCF_MPI_RecvSingleReal()
        end do
    else
    !   GMCF_MPI_RecvSingleReal()
    end if
end subroutine GMCF_MPI_BroadcastSingleReal

subroutine GMCF_MPI_ISend1DRealArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:) :: array
    integer :: pre_post
    pre_post = PRE ! Arbitrary
    call gmcfSend1DRealArray(rank, array, size(array), tag, destination, pre_post, time)
end subroutine GMCF_MPI_ISend1DRealArray

subroutine GMCF_MPI_Send1DRealArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:) :: array
    ! Equivalent to
    ! GMCF_MPI_ISend1DRealArray
    ! No wait call required as the GMCF framework makes a copy of array so it
    ! is safe to reuse array straight away even though there is no blocking
    ! call.
    call GMCF_MPI_ISend1DRealArray(rank, array, tag, destination, time)
end subroutine GMCF_MPI_Send1DRealArray

subroutine GMCF_MPI_IRecv1DRealArray()
    implicit none
    ! Equivalent to
    ! Nothing in this case? Within models doesn't need to request data from
    ! the sender unlike between models which does. Also MPI doesn't guarantee
    ! IRecv actually has any valid data until after MPI_Wait() returns. Since
    ! GMCF doesn't give unique labels to each request then there is nothing to
    ! do here...
end subroutine GMCF_MPI_IRecv1DRealArray

subroutine GMCF_MPI_Recv1DRealArray()
    implicit none
    ! Equivalent to
    call GMCF_MPI_IRecv1DRealArray()
    ! GMCF_MPI_Wait()
end subroutine GMCF_MPI_Recv1DRealArray

subroutine GMCF_MPI_ISend3DRealArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:,:,:) :: array
    integer :: pre_post
    pre_post = PRE ! Arbitrary
    call gmcfSend3DRealArray(rank, array, size(array), tag, destination, pre_post, time)
end subroutine GMCF_MPI_ISend3DRealArray

subroutine GMCF_MPI_Send3DRealArray(rank, array, tag, destination, time)
    implicit none
    integer, intent(in) :: rank, tag, destination, time
    real, dimension(:,:,:) :: array
    ! Equivalent to
    ! GMCF_MPI_ISend3DRealArray
    ! No wait call required as the GMCF framework makes a copy of array so it
    ! is safe to reuse array straight away even though there is no blocking
    ! call.
    call GMCF_MPI_ISend3DRealArray(rank, array, tag, destination, time)
end subroutine GMCF_MPI_Send3DRealArray

subroutine GMCF_MPI_IRecv3DRealArray()
    implicit none
    ! Equivalent to
    ! Nothing in this case? Within models doesn't need to request data from
    ! the sender unlike between models which does. Also MPI doesn't guarantee
    ! IRecv actually has any valid data until after MPI_Wait() returns. Since
    ! GMCF doesn't give unique labels to each request then there is nothing to
    ! do here...
end subroutine GMCF_MPI_IRecv3DRealArray

subroutine GMCF_MPI_Recv3DRealArray()
    implicit none
    ! Equivalent to
    call GMCF_MPI_IRecv3DRealArray()
    ! Perhaps not? GMCF seems to force you to go through all packets so if the
    ! instance is waiting for multiple boundaries (in almost every case) then
    ! we need to be ready to have all of them at once?
    ! GMCF_MPI_Wait()
end subroutine GMCF_MPI_Recv3DRealArray

subroutine GMCF_MPI_WaitRealHaloBoundaries(rank, topBoundary, bottomBoundary, leftBoundary, rightBoundary)
    implicit none
    integer, intent(in) :: rank, topNeighbour, bottomNeighbour, leftNeighbour, rightNeighbour
    real, dimension(:,:,:), intent(out) :: topBoundary, bottomBoundary, leftBoundary, rightBoundary)
    integer :: has_packets, fifo_empty
    type(gmcfPacket) :: packet
    if (topNeighbour .ne. -1) then
        call gmcfWaitFor(rank, RESPDATA, topNeighbour, 1)
    end if
    if (bottomNeighbour .ne. -1) then
        call gmcfWaitFor(rank, RESPDATA, bottomNeighbour, 1)
    end if
    if (leftNeighbour .ne. -1) then
        call gmcfWaitFor(rank, RESPDATA, leftNeighbour, 1)
    end if
    if (rightNeighbour .ne. -1) then
        call gmcfWaitFor(rank, RESPDATA, rightNeighbour, 1)
    end if
    call gmcfHasPackets(rank, RESPDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (topTag)
                call gmcfRead3DRealArray(topBoundary, shape(topBoundary), packet)
            case (bottomTag)
                call gmcfRead3DRealArray(bottomBoundary, shape(bottomBoundary), packet)
            case (leftTag)
                call gmcfRead3DRealArray(leftBoundary, shape(leftBoundary), packet)
            case (rightTag)
                call gmcfRead3DRealArray(rightBoundary, shape(rightBoundary), packet)
        end select
        call gmcfHasPackets(rank, RESPDATA, has_packets)
    end do
end subroutine GMCF_MPI_WaitRealHaloBoundaries

end module
