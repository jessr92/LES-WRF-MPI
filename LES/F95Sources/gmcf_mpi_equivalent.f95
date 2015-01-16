module gmcf_mpi_equivalent
use gmcfAPI
implicit none

integer, parameter :: topTag = 1, bottomTag = 2, leftTag = 3, rightTag = 4

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
    call GMCF_MPI_GatherReal(gatheredValues, instanceCount, master)
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
    call GMCF_MPI_GatherReal(gatheredValues, instanceCount, master)
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
    call GMCF_MPI_GatherReal(gatheredValues, instanceCount, master)
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

subroutine GMCF_MPI_GatherReal(gatheredValues, instanceCount, master)
    implicit none
    real(kind=4), dimension(:), intent(inout) :: gatherdValues
    integer, intent(in) :: instanceCount
    logical, intent(in) :: master
    integer :: i
    if (master) then
        do i=1, instanceCount
    !       MPI_Recv()
        end do
    else
    !    MPI_Send()
    end if
end subroutine GMCF_MPI_Gather

subroutine GMCF_MPI_Broadcast(value, instanceCount, master)
    implicit none
    real(kind=4), intent(inout) :: value  
    integer, intent(in) :: instanceCount  
    logical, intent(in) :: master
    integer :: i
    if (master) then
        do i=1, instanceCount
    !       MPI_Send()
        end do
    else
    !   MPI_Recv()
    end if
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
    ! Perhaps not? GMCF seems to force you to go through all packets so if the
    ! instance is waiting for multiple boundaries (in almost every case) then
    ! we need to be ready to have all of them at once?
    ! GMCF_MPI_Wait()
end subroutine GMCF_MPI_Recv3DFloatArray

subroutine GMCF_MPI_WaitAll(rank, topBoundary, bottomBoundary, leftBoundary, rightBoundary) ! also any side flows and broadcasts?
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
                call gmcfRead3DFloatArray(topBoundary, shape(topBoundary), packet)
            case (bottomTag)
                call gmcfRead3DFloatArray(bottomBoundary, shape(bottomBoundary), packet)
            case (leftTag)
                call gmcfRead3DFloatArray(leftBoundary, shape(leftBoundary), packet)
            case (rightTag)
                call gmcfRead3DFloatArray(rightBoundary, shape(rightBoundary), packet)
        end select
        call gmcfHasPackets(rank, RESPDATA, has_packets)
    end do
end subroutine GMCF_MPI_WaitAll

end module
