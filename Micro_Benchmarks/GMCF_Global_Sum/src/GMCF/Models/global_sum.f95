subroutine program_global_sum(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    integer :: iterations, i
    integer :: clock_start, clock_end, clock_rate
    real(kind=4) :: total_time, messages_per_second
    real(kind=4) :: value
    iterations = 10000
    call gmcfInitCoupler(sys, tile, model_id)
    call system_clock(clock_start, clock_rate)
    do i=1, iterations
        value = model_id
        call getGlobalSumOfGMCF(model_id, value)
    end do
    print*, value
    call system_clock(clock_end, clock_rate)
    call gmcfFinished(model_id)
    if (model_id .eq. 1) then
        total_time = (clock_end - clock_start)/real(clock_rate)
        messages_per_second = iterations / total_time
        print*, total_time, "s for ", iterations, " iterations, a rate of ", messages_per_second, " iterations per second."
    end if
end subroutine program_global_sum

subroutine getGlobalSumOfGMCF(model_id, value)
    implicit none
    integer, intent(in) :: model_id
    real(kind=4), intent(inout) :: value
    call getGlobalOp(model_id, value, 1)
end subroutine getGlobalSumOfGMCF

subroutine getGlobalOp(model_id, value, tag)
    implicit none
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    if (model_id .eq. 1) then
        call getGlobalOpMaster(model_id, value, tag)
    else
        call getGlobalOpNotMaster(model_id, value, tag)
    end if
end subroutine getGlobalOp

subroutine getGlobalOpMaster(model_id, value, tag)
    use gmcfAPI
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    real(kind=4), dimension(1) :: receiveBuffer, sendBuffer
    integer :: i, has_packets, fifo_empty
    type(gmcfPacket) :: packet
    do i=2, (PROC_PER_ROW * PROC_PER_COL)
        call gmcfRequestData(model_id, tag, 1, i, PRE, 1)
    end do
    do i=2, (PROC_PER_ROW * PROC_PER_COL)
        call gmcfWaitFor(model_id, RESPDATA, i, 1)
    end do
    call gmcfHasPackets(model_id, RESPDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, RESPDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfRead1DFloatArray(receiveBuffer, shape(receiveBuffer), packet)
            if (tag .eq. 1) then
                value = value + receiveBuffer(1)
            else
                print*, 'Unexpected global op'
            end if
        end if
        call gmcfHasPackets(model_id, RESPDATA, has_packets)
    end do
    sendBuffer(1) = value
    do i=2, (PROC_PER_ROW * PROC_PER_COL)
        call gmcfWaitFor(model_id, REQDATA, i, 1)
    end do
    call gmcfHasPackets(model_id, REQDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, REQDATA, packet, fifo_empty)
        if (packet%data_id .ne. tag) then
            print*, 'Received unexpected packet'
        else
            call gmcfSend1DFloatArray(model_id, sendBuffer, shape(sendBuffer), tag, packet%source, PRE, 1)
        end if
        call gmcfHasPackets(model_id, REQDATA, has_packets)
    end do
    do i=2,(PROC_PER_ROW * PROC_PER_COL)
        call gmcfWaitFor(model_id, ACKDATA, i, 1)
    end do
    call gmcfHasPackets(model_id, ACKDATA, has_packets)
    do while(has_packets == 1)
        call gmcfShiftPending(model_id, ACKDATA, packet, fifo_empty)
        call gmcfHasPackets(model_id, ACKDATA, has_packets)
    end do
end subroutine getGlobalOpMaster

subroutine getGlobalOpNotMaster(model_id, value, tag)
    use gmcfAPI
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

