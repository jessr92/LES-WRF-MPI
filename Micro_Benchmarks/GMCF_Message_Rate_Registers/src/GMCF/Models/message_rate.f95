subroutine program_message_rate(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    integer :: iterations, i, has_packets, fifo_empty
    integer :: clock_start, clock_end, clock_rate, elements
    real(kind=4) :: total_time, messages_per_second, throughput
    type(gmcfPacket) :: packet
    real(kind=4), dimension(1000) :: buffer
    iterations = 100
    elements = size(buffer)
    call gmcfInitCoupler(sys, tile, model_id)
    call system_clock(clock_start, clock_rate)
    if (mod(model_id, 2) .eq. 0) then
        buffer = 1
        do i=1,iterations

        end do
    else
        do i=1,iterations

        end do
    end if
    call system_clock(clock_end, clock_rate)
    call gmcfFinished(model_id)
    if (mod(model_id, 2) .eq. 0) then
        total_time = (clock_end - clock_start)/real(clock_rate)
        messages_per_second = iterations / total_time
        throughput = (iterations * elements * 4) / total_time
        print*, total_time, "s for ", iterations, " messages, a rate of ", messages_per_second, " messages per second."
        print*, "Message size is ", elements, " reals so there is a throughput of ", throughput/1024, "KB/s"
    end if
end subroutine program_message_rate

