program message_rate
use mpi
integer :: rank, ierror, mpi_size, status(MPI_STATUS_SIZE)
integer :: iterations, i
integer :: clock_start, clock_end, clock_rate, elements
real(kind=4) :: total_time, messages_per_second, throughput
real(kind=4) :: value
iterations = 1000000
call MPI_Init(ierror)
call MPI_Comm_Rank(MPI_COMM_WORLD, rank, ierror)
call MPI_Comm_Size(MPI_COMM_WORLD, mpi_size, ierror)
call system_clock(clock_start, clock_rate)
do i=1, iterations
    value = rank + 1
    call MPI_AllReduce(MPI_IN_PLACE, value, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierror)
end do
print*, value
call system_clock(clock_end, clock_rate)
call MPI_Finalize(ierror)
if (rank .eq. 0) then
    total_time = (clock_end - clock_start)/real(clock_rate)
    messages_per_second = iterations / total_time
    print*, total_time, "s for ", iterations, " messages, a rate of ", messages_per_second, " messages per second."
end if
end program message_rate

