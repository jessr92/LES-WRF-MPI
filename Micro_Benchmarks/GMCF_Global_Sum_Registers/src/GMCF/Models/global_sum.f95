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
    iterations = 30000
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
    real(kind=4) :: received
    integer :: i, fifo_empty
    type(gmcfPacket) :: packet
    do i=2,INSTANCES
        call gmcfAddOneToSet(model_id, REGREADY, i)
    end do
    do i=2,INSTANCES
        call gmcfWaitFor(model_id, REGREADY, i, 1)
    end do
    do i=2,INSTANCES
        call gmcfShiftPending(model_id, i, REGREADY, packet, fifo_empty)
        call gmcfReadRegC(sba_sys, i, tag, received)
        call gmcfSendAck(model_id, tag, i)
        if (tag .eq. 1) then
            value = value + received
        else
            print*, tag, ' is an invalid tag.'
        end if
    end do
    ! Write global op result to register
    call gmcfWriteRegC(sba_sys, model_id, tag, value)
    do i=2,INSTANCES
        call gmcfSendRegReady(model_id, tag, i)
    end do
    do i=2,INSTANCES
        call gmcfWaitFor(model_id, ACKDATA, i, 1)
    end do
    do i=2,INSTANCES
        call gmcfShiftPending(model_id, i, ACKDATA, packet, fifo_empty)
    end do
end subroutine getGlobalOpMaster

subroutine getGlobalOpNotMaster(model_id, value, tag)
    use gmcfAPI
    integer, intent(in) :: model_id, tag
    real(kind=4), intent(inout) :: value
    integer :: fifo_empty
    type(gmcfPacket) :: packet
    call gmcfWriteRegC(sba_sys, model_id, tag, value)
    call gmcfSendRegReady(model_id, tag, 1)
    call gmcfWaitFor(model_id, ACKDATA, 1, 1)
    call gmcfShiftPending(model_id, 1, ACKDATA, packet, fifo_empty)
    ! Get global op result
    call gmcfAddOneToSet(model_id, REGREADY, 1)
    call gmcfWaitFor(model_id, REGREADY, 1, 1)
    call gmcfShiftPending(model_id, 1, REGREADY, packet, fifo_empty)
    call gmcfReadRegC(sba_sys, 1, tag, value)
    call gmcfSendAck(model_id, tag, 1)
end subroutine getGlobalOpNotMaster

