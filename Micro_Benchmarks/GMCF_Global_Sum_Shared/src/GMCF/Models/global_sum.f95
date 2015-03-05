subroutine program_global_sum(sys, tile, model_id) ! This replaces 'program main'
    use gmcfAPI
    use module_global_sum
    implicit none
    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    integer :: iterations, i
    integer :: clock_start, clock_end, clock_rate
    real(kind=4) :: total_time, messages_per_second
    real(kind=4) :: value
    iterations = 1000000
    call gmcfInitCoupler(sys, tile, model_id)
    call gmcfInitGlobalOpSpinLock()
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

