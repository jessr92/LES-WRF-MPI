module communication_helper_gmcf
use communication_common
use gmcfAPI
implicit none

integer :: mpi_size ! Names for compatibility with existing MPI code

contains

subroutine initialise_gmcf(sys, tile, model_id, procPerRow, procPerCol)
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id, procPerRow, procPerCol
    mpi_size = procPerRow * procPerCol
    call gmcfInitCoupler(sys, tile, model_id)
end subroutine initialise_gmcf

subroutine finalise_gmcf(model_id)
    implicit none
    integer, intent(in) :: model_id
    call gmcfFinished(model_id)
end subroutine finalise_gmcf

subroutine sendHaloBoundaries(leftSend, rightSend, topSend, bottomSend, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(in) :: leftSend, rightSend, topSend, bottomSend
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    !print*, 'Model_id ', model_id, ' is waiting for halo boundary requests'
    if (.not. isTopRow(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a request from ', model_id - procPerRow
        call gmcfWaitFor(model_id, REQDATA, model_id - procPerRow, 1)
    end if
    if (.not. isBottomRow(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a request from ', model_id + procPerRow
        call gmcfWaitFor(model_id, REQDATA, model_id + procPerRow, 1)
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a request from ', model_id - 1
        call gmcfWaitFor(model_id, REQDATA, model_id - 1, 1)
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a request from ', model_id + 1
        call gmcfWaitFor(model_id, REQDATA, model_id + 1, 1)
    end if
    !print*, 'Model_id ', model_id, ' has received halo boundary requests'
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (topTag)
                call gmcfSend3DFloatArray(model_id, topSend, shape(topSend), topTag, packet%source, PRE, 1)
            case (bottomTag)
                call gmcfSend3DFloatArray(model_id, bottomSend, shape(bottomSend), bottomTag, packet%source, PRE, 1)
            case (leftTag)
                call gmcfSend3DFloatArray(model_id, leftSend, shape(leftSend), leftTag, packet%source, PRE, 1)
            case (rightTag)
                call gmcfSend3DFloatArray(model_id, rightSend, shape(rightSend), rightTag, packet%source, PRE, 1)
            case default
                print*, 'Model_id  ', model_id, ' received an unexpected REQDATA.'
        end select
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
    !print*, 'Model_id ', model_id, ' has responded to halo boundary requests'
end subroutine sendHaloBoundaries

subroutine recvHaloBoundaries(leftRecv, rightRecv, topRecv, bottomRecv, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(out) :: leftRecv, rightRecv, topRecv, bottomRecv
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    !print*, 'Model_id ', model_id, ' is waiting for halo boundaries'
    if (.not. isTopRow(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a response from ', model_id - procPerRow
        call gmcfWaitFor(model_id, RESPDATA, model_id - procPerRow, 1)
    end if
    if (.not. isBottomRow(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a response from ', model_id + procPerRow
        call gmcfWaitFor(model_id, RESPDATA, model_id + procPerRow, 1)
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a response from ', model_id - 1
        call gmcfWaitFor(model_id, RESPDATA, model_id - 1, 1)
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for a response from ', model_id + 1
        call gmcfWaitFor(model_id, RESPDATA, model_id + 1, 1)
    end if
    !print*, 'Model_id ', model_id, ' has finished waiting for halo boundaries'
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (topTag)
                call gmcfRead3DFloatArray(topRecv, shape(topRecv), packet)
            case (bottomTag)
                call gmcfRead3DFloatArray(bottomRecv, shape(bottomRecv), packet)
            case (leftTag)
                call gmcfRead3DFloatArray(leftRecv, shape(leftRecv), packet)
            case (rightTag)
                call gmcfRead3DFloatArray(rightRecv, shape(rightRecv), packet)
            case default
                print*, 'Model_id  ', model_id, ' received an unexpected RESPDATA for recvhalo, got one from ', packet%source
        end select
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
    !print*, 'Model_id ', model_id, ' has received halo boundaries'
end subroutine recvHaloBoundaries

subroutine waitForHaloAcks(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    if (.not. isTopRow(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for an ack from ', model_id - procPerRow
        call gmcfWaitFor(model_id, ACKDATA, model_id - procPerRow, 1)
    end if
    if (.not. isBottomRow(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for an ack from ', model_id + procPerRow
        call gmcfWaitFor(model_id, ACKDATA, model_id + procPerRow, 1)
    end if
    if (.not. isLeftmostColumn(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for an ack from ', model_id - 1
        call gmcfWaitFor(model_id, ACKDATA, model_id - 1, 1)
    end if
    if (.not. isRightmostColumn(procPerRow)) then
        !print*, 'Model_id ', model_id, ' is waiting for an ack from ', model_id + 1
        call gmcfWaitFor(model_id, ACKDATA, model_id + 1, 1)
    end if
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        if (packet%source .ne. model_id - procPerRow .and. &
              packet%source .ne. model_id + procPerRow .and. &
              packet%source .ne. model_id - 1 .and. &
              packet%source .ne. model_id + 1) then
            print*, 'Model_id  ', model_id, ' received an unexpected ACKDATA for halo acks, got one from ', packet%source
        end if
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do
end subroutine waitForHaloAcks

subroutine sendLeftRightSideflow(leftRightSend, procPerRow)
    implicit none
    real(kind=4), dimension(:,:), intent(in) :: leftRightSend
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    call gmcfWaitFor(model_id, REQDATA, model_id + procPerRow - 1, 1)
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (leftSideTag)
                call gmcfSend2DFloatArray(model_id, leftRightSend, shape(leftRightSend), leftSideTag, packet%source, PRE, 1)
                exit
            case default
                !print*, 'Model_id  ', model_id, ' received an unexpected REQDATA for lr sideflow, got one from ', &
                 !packet%source
                call gmcfPushPending(model_id, packet)
        end select
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
end subroutine sendLeftRightSideflow

subroutine recvLeftRightSideflow(leftRightRecv, procPerRow)
    implicit none
    real(kind=4), dimension(:,:), intent(out) :: leftRightRecv
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    call gmcfWaitFor(model_id, RESPDATA, model_id - procPerRow + 1, 1)
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (leftSideTag)
                call gmcfRead2DFloatArray(leftRightRecv, shape(leftRightRecv), packet)
            case default
                print*, 'Model_id  ', model_id, ' received an unexpected RESPDATA for lr sideflow, got one from ', &
                packet%source
        end select
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
end subroutine recvLeftRightSideflow

subroutine waitForLeftRightSideflowAcks(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    call gmcfWaitFor(model_id, ACKDATA, model_id + procPerRow - 1, 1)
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        if (packet%source .ne. model_id + procPerRow - 1) then
            print*, 'Model_id  ', model_id, ' received an unexpected ACKDATA for left sideflow acks, got one from ', packet%source
        end if
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do
end subroutine waitForLeftRightSideflowAcks

subroutine sendRightLeftSideflow(rightLeftSend, procPerRow)
    implicit none
    real(kind=4), dimension(:,:), intent(in) :: rightLeftSend
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    call gmcfWaitFor(model_id, REQDATA, model_id - procPerRow + 1, 1)
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (rightSideTag)
                call gmcfSend2DFloatArray(model_id, rightLeftSend, shape(rightLeftSend), rightSideTag, packet%source, PRE, 1)
                exit
            case default
                !print*, 'Model_id  ', model_id, ' received an unexpected REQDATA for rl sideflow, got one from ', &
                !packet%source
                call gmcfPushPending(model_id, packet)
        end select
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
end subroutine sendRightLeftSideflow

subroutine recvRightLeftSideflow(rightLeftRecv, procPerRow)
    implicit none
    real(kind=4), dimension(:,:), intent(out) :: rightLeftRecv
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    call gmcfWaitFor(model_id, RESPDATA, model_id + procPerRow - 1, 1)
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (rightSideTag)
                call gmcfRead2DFloatArray(rightLeftRecv, shape(rightLeftRecv), packet)
            case default
                print*, 'Model_id  ', model_id, ' received an unexpected RESPDATA for rl sideflow, got one from ', &
                packet%source
        end select
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
end subroutine recvRightLeftSideflow

subroutine waitForRightLeftSideflowAcks(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    call gmcfWaitFor(model_id, ACKDATA, model_id - procPerRow + 1, 1)
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        if (packet%source .ne. model_id - procPerRow + 1) then
            print*, 'Model_id  ', model_id, ' received an unexpected ACKDATA for right sideflow acks, got one from ', packet%source
        end if
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do
end subroutine waitForRightLeftSideflowAcks

subroutine sendExactCorners(topLeftSend, topRightSend, bottomLeftSend, bottomRightSend, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(in) :: topLeftSend, topRightSend, bottomLeftSend, bottomRightSend
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, REQDATA, model_id - procPerRow - 1, 1)
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, REQDATA, model_id - procPerRow + 1, 1)
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, REQDATA, model_id + procPerRow - 1, 1)
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, REQDATA, model_id + procPerRow + 1, 1)
    end if
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (topLeftTag)
                call gmcfSend3DFloatArray(model_id, topLeftSend, shape(topLeftSend), topLeftTag, packet%source, PRE, 1)
            case (topRightTag)
                call gmcfSend3DFloatArray(model_id, topRightSend, shape(topRightSend), topRightTag, packet%source, PRE, 1)
            case (bottomLeftTag)
                call gmcfSend3DFloatArray(model_id, bottomLeftSend, shape(bottomLeftSend), bottomLeftTag, packet%source, PRE, 1)
            case (bottomRightTag)
                call gmcfSend3DFloatArray(model_id, bottomRightSend, shape(bottomRightSend), bottomRightTag, packet%source, PRE, 1)
            case default
                print*, 'Model_id  ', model_id, ' received an unexpected REQDATA.'
        end select
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
end subroutine sendExactCorners

subroutine recvExactCorners(topLeftRecv, topRightRecv, bottomLeftRecv, bottomRightRecv, procPerRow)
    implicit none
    real(kind=4), dimension(:,:,:), intent(in) :: topLeftRecv, topRightRecv, bottomLeftRecv, bottomRightRecv
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, RESPDATA, model_id - procPerRow - 1, 1)
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, RESPDATA, model_id - procPerRow + 1, 1)
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, RESPDATA, model_id + procPerRow - 1, 1)
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, RESPDATA, model_id + procPerRow + 1, 1)
    end if
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        select case (packet%data_id)
            case (topLeftTag)
                call gmcfRead3DFloatArray(topLeftRecv, shape(topLeftRecv), packet)
            case (topRightTag)
                call gmcfRead3DFloatArray(topRightRecv, shape(topRightRecv), packet)
            case (bottomLeftTag)
                call gmcfRead3DFloatArray(bottomLeftRecv, shape(bottomLeftRecv), packet)
            case (bottomRightTag)
                call gmcfRead3DFloatArray(bottomRightRecv, shape(bottomRightRecv), packet)
            case default
                print*, 'Model_id  ', model_id, ' received an unexpected REQDATA.'
        end select
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
end subroutine recvExactCorners

subroutine waitForExactCornersAcks(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfGetModelId(model_id)
    if (.not. isTopRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, ACKDATA, model_id - procPerRow - 1, 1)
    end if
    if (.not. isTopRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, ACKDATA, model_id - procPerRow + 1, 1)
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isLeftmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, ACKDATA, model_id + procPerRow - 1, 1)
    end if
    if (.not. isBottomRow(procPerRow) .and. .not. isRightmostColumn(procPerRow)) then
        call gmcfWaitFor(model_id, ACKDATA, model_id + procPerRow + 1, 1)
    end if
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while (has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        if (packet%source .ne. model_id - procPerRow - 1 .and. &
              packet%source .ne. model_id - procPerRow + 1 .and. &
              packet%source .ne. model_id + procPerRow - 1 .and. &
              packet%source .ne. model_id + procPerRow + 1) then
            print*, 'Model_id  ', model_id, ' received an unexpected ACKDATA for exact corner acks.'
        end if
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do
end subroutine waitForExactCornersAcks

subroutine getGlobalSumOfGMCF(value)
    implicit none
    real(kind=4), intent(inout) :: value
    integer :: model_id
    call gmcfGetModelId(model_id)
    call getGlobalOp(model_id, value, globalSumTag)
end subroutine getGlobalSumOfGMCF

subroutine getGlobalMaxOfGMCF(value)
    implicit none
    real(kind=4), intent(inout) :: value
    integer :: model_id
    call gmcfGetModelId(model_id)
    call getGlobalOp(model_id, value, globalMaxTag)
end subroutine getGlobalMaxOfGMCF

subroutine getGlobalMinOfGMCF(value)
    implicit none
    real(kind=4), intent(inout) :: value
    integer :: model_id
    call gmcfGetModelId(model_id)
    call getGlobalOp(model_id, value, globalMinTag)
end subroutine getGlobalMinOfGMCF

subroutine getGlobalOp(model_id, value, tag)
    implicit none
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    !print*, 'Model_id ', model_id, ' beginning getGlobalOp with tag ', tag
    if (isMaster()) then
        call getGlobalOpMaster(model_id, value, tag)
    else
        call getGlobalOpNotMaster(model_id, value, tag)
    end if
    !print*, 'Model_id ', model_id, ' finished getGlobalOp'
end subroutine getGlobalOp

subroutine getGlobalOpMaster(model_id, value, tag)
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    real(kind=4), dimension(1) :: receiveBuffer, sendBuffer
    integer :: i, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    !print*, 'Model_id ', model_id, ' is master'
    ! Request everybody's value
    do i=2, mpi_size
        call gmcfRequestData(model_id, tag, 1, i, PRE, 1)
    end do
    !print*, 'Model_id ', model_id, ' has requested every value'
    ! Wait for responses
    do i=2, mpi_size
        call gmcfWaitFor(model_id, RESPDATA, i, 1)
    end do
    !print*, 'Model_id ', model_id, ' has received responses for every value'
    ! Read in responses and find the min value
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfRead1DFloatArray(receiveBuffer, shape(receiveBuffer), packet)
            if (tag .eq. globalMinTag) then
                if (receiveBuffer(1) .lt. value) then
                    value = receiveBuffer(1)
                end if
            else if (tag .eq. globalMaxTag) then
                if (receiveBuffer(1) .gt. value) then
                    value = receiveBuffer(1)
                end if
            else if (tag .eq. globalSumTag) then
                value = value + receiveBuffer(1)
            else
                print*, 'Unexpected global op'
            end if
        end if
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
    !print*, 'Model_id ', model_id, ' has every value and has done the op'
    ! Wait for everybody to request the global sum
    sendBuffer(1) = value
    do i=2, mpi_size
        call gmcfWaitFor(model_id, REQDATA, i, 1)
    end do
    !print*, 'Model_id ', model_id, ' has received all requests for new value'
    ! Send the global op result
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfSend1DFloatArray(model_id, sendBuffer, shape(sendBuffer), tag, packet%source, PRE, 1)
            !print*, 'Model_id ', model_id, ' sent result value to ', packet%source
        end if
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
    !print*, 'Model_id ', model_id, ' has sent everyone the new value'
    ! Wait for acks
    do i=2,mpi_size
        call gmcfWaitFor(model_id, ACKDATA, i, 1)
        !print*, 'Model_id ', model_id, ' received final ack from ', i
    end do
    !print*, 'Model_id ', model_id, ' has receive all acks for new value'
    ! Deal with the acks
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do
end subroutine getGlobalOpMaster

subroutine getGlobalOpNotMaster(model_id, value, tag)
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    real(kind=4), dimension(1) :: receiveBuffer, sendBuffer
    integer :: has_packets, fifo_empty
    type(gmcfPacket) :: packet
    !print*, 'Model_id ', model_id, ' is an else thread'
    sendBuffer(1) = value
    ! Wait for master to ask for my value
    call gmcfWaitFor(model_id, REQDATA, 1, 1)
    !print*, 'Model_id ', model_id, ' has got masters REQDATA '
    ! Send my value
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfSend1DFloatArray(model_id, sendBuffer, shape(sendBuffer), tag, packet%source, PRE, 1)
            !print*, 'value sent to ', packet%source
        end if
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
    !print*, 'Model_id ', model_id, ' has sent their value'
    call gmcfWaitFor(model_id, ACKDATA, 1, 1)
    ! Deal with the acks
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do        
    ! Request global value
    call gmcfRequestData(model_id, tag, 1, 1, PRE, 1)
    ! Wait for response
    call gmcfWaitFor(model_id, RESPDATA, 1, 1)
    ! Read in response
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfRead1DFloatArray(receiveBuffer, shape(receiveBuffer),packet)
        end if
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
    ! Copy received value into appropriate place
    value = receiveBuffer(1)
end subroutine getGlobalOpNotMaster

subroutine recv3DReal4Array(rank, i, recvBuffer, bufferSize)
    real(kind=4), dimension(:,:,:), intent(out) :: recvBuffer
    integer :: rank, has_packets, fifo_empty, i, bufferSize
    type(gmcfPacket) :: packet
    call gmcfRequestData(rank, collect3DReal4Tag, bufferSize, i, PRE, 1)
    call gmcfWaitFor(rank, RESPDATA, i, 1)
    call gmcfHasPackets(rank, RESPDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
        if (packet%source .ne. i) then
            print*, 'Rank ', rank, ' received an unexpected RESPDATA in recv 3d real 4 array'
        else
            call gmcfRead3DFloatArray(recvBuffer, shape(recvBuffer), packet)
        end if
        call gmcfHasPackets(rank, RESPDATA, has_packets)
    end do
end subroutine recv3DReal4Array

subroutine send3DReal4Array(array, rank)
    real(kind=4), dimension(:,:,:), intent(in) :: array
    integer :: rank, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    call gmcfWaitFor(rank, REQDATA, 1, 1)
    call gmcfHasPackets(rank, REQDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
        if (packet%source .ne. 1) then
            print*, 'Rank ', rank, ' received an unexpected REQDATA in send 3d real 4 array'
        else
            call gmcfSend3DFloatArray(rank, array, shape(array), collect3DReal4Tag, packet%source, PRE, 1)
        end if
        call gmcfHasPackets(rank, REQDATA, has_packets)
   end do
   call gmcfWaitFor(rank, ACKDATA, 1, 1)
   call gmcfHasPackets(rank, ACKDATA, has_packets)
   do while(has_packets == 1)
       call gmcfShiftPending(rank, ACKDATA, packet, fifo_empty)
       if (packet%source .ne. 1) then
           print*, 'Rank ', rank, ' received an unexpected ACKDATA in send 3d real 4 array'
       end if
       call gmcfHasPackets(rank, ACKDATA, has_packets)
   end do
end subroutine send3DReal4Array

subroutine gmcfSend1DArray(sendBuffer, rank, i, tag)
    real(kind=4), dimension(:) :: sendBuffer
    integer :: rank, i, tag, fifo_empty, has_packets
    type(gmcfPacket) :: packet
    call gmcfWaitFor(rank, REQDATA, i, 1)
    call gmcfHasPackets(rank, REQDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
        if (packet%source .eq. i) then
            call gmcfSend1DFloatArray(rank, sendBuffer, shape(sendBuffer), tag, i, PRE, 1)
            exit
        else
            call gmcfPushPending(rank, packet) ! Too early
        end if
        call gmcfHasPackets(rank, REQDATA, has_packets)
    end do
    call gmcfWaitFor(rank, ACKDATA, i, 1)
    call gmcfHasPackets(rank, ACKDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, ACKDATA, packet, fifo_empty)
        if (packet%source .ne. i) then
            print*, 'Model_id ', rank, ' received an unexpected ack in send 1d array'
        end if
        call gmcfHasPackets(rank, ACKDATA, has_packets)
    end do
end subroutine gmcfSend1DArray

subroutine gmcfRecv1DArray(receivingArray, receivingSize, rank, tag)
    real(kind=4), dimension(:) :: receivingArray
    integer :: rank, tag, fifo_empty, has_packets, receivingSize
    type(gmcfPacket) :: packet
    call gmcfRequestData(rank, tag, receivingSize, 1, PRE, 1)
    call gmcfWaitFor(rank, RESPDATA, 1, 1)
    call gmcfHasPackets(rank, RESPDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfRead1DFloatArray(receivingArray, shape(receivingArray),packet)
        end if
        call gmcfHasPackets(rank, RESPDATA, has_packets)
    end do
end subroutine gmcfRecv1DArray

subroutine gmcfSend2DArray(sendBuffer, rank, i, tag)
    real(kind=4), dimension(:,:) :: sendBuffer
    integer :: rank, i, tag, fifo_empty, has_packets
    type(gmcfPacket) :: packet
    call gmcfWaitFor(rank, REQDATA, i, 1)
    call gmcfHasPackets(rank, REQDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, REQDATA, packet, fifo_empty)
        if (packet%source .eq. i) then
            call gmcfSend2DFloatArray(rank, sendBuffer, shape(sendBuffer), tag, i, PRE, 1)
            exit
        else
            call gmcfPushPending(rank, packet) ! Too early
        end if
        call gmcfHasPackets(rank, REQDATA, has_packets)
    end do
    call gmcfWaitFor(rank, ACKDATA, i, 1)
    call gmcfHasPackets(rank, ACKDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, ACKDATA, packet, fifo_empty)
        if (packet%source .ne. i) then
            print*, 'Model_id ', rank, ' received an unexpected ack in send 1d array'
        end if
        call gmcfHasPackets(rank, ACKDATA, has_packets)
    end do
end subroutine gmcfSend2DArray

subroutine gmcfRecv2DArray(recvBuffer, receivingSize, rank, tag)
    real(kind=4), dimension(:,:) :: recvBuffer
    integer :: rank, tag, fifo_empty, has_packets, receivingSize
    type(gmcfPacket) :: packet
    call gmcfRequestData(rank, tag, receivingSize, 1, PRE, 1)
    print*, 'Model_id ', rank, ' has requested zbm from 1'
    call gmcfWaitFor(rank, RESPDATA, 1, 1)
    print*, 'Model_id ', rank, ' has received zbm response from 1'
    call gmcfHasPackets(rank, RESPDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(rank, RESPDATA, packet, fifo_empty)
        if (packet%data_id .ne. zbmTag) then
            print*, 'Received unexpected packet'
        else
            call gmcfRead2DFloatArray(recvBuffer, shape(recvBuffer),packet)
        end if
        call gmcfHasPackets(rank, RESPDATA, has_packets)
    end do
    print*, 'Model_id ', rank, ' has read zbm response'
end subroutine gmcfRecv2DArray

logical function isMaster()
    implicit none
    integer :: model_id
    call gmcfGetModelId(model_id)
    isMaster = model_id .eq. 1
end function isMaster

logical function isTopRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id
    call gmcfGetModelId(model_id)
    isTopRow = model_id .le. procPerRow
end function isTopRow

logical function isBottomRow(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id
    call gmcfGetModelId(model_id)
    isBottomRow = model_id .gt. (mpi_size - procPerRow)
end function isBottomRow

logical function isLeftmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id
    call gmcfGetModelId(model_id)
    isLeftmostColumn = modulo(model_id - 1, procPerRow) .eq. 0
end function isLeftmostColumn

logical function isRightmostColumn(procPerRow)
    implicit none
    integer, intent(in) :: procPerRow
    integer :: model_id
    call gmcfGetModelId(model_id)
    isRightmostColumn = modulo(model_id - 1, procPerRow) .eq. (procPerRow - 1)
end function isRightmostColumn

integer function topLeftRowValue(process, procPerRow, rowCount)
    implicit none
    integer, intent(in) :: process, procPerRow, rowCount
    topLeftRowValue = (process - 1) / procPerRow * rowCount
end function topLeftRowValue

integer function topLeftColValue(process, procPerRow, colCount)
    implicit none
    integer, intent(in) :: process, procPerRow, colCount
    topLeftColValue = modulo(process - 1, procPerRow) * colCount
end function topLeftColValue

end module communication_helper_gmcf

