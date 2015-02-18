program message_rate
use mpi
integer :: rank, ierror, mpi_size, status(MPI_STATUS_SIZE)
integer :: iterations, i
integer :: clock_start, clock_end, clock_rate
real(kind=4) :: total_time, messages_per_second
real(kind=4), dimension(1) :: buffer
iterations = 10000000

call MPI_Init(ierror)
call MPI_Comm_Rank(MPI_COMM_WORLD, rank, ierror)
call MPI_Comm_Size(MPI_COMM_WORLD, mpi_size, ierror)
if (mod(mpi_size, 2) .ne. 0) then
    print*, 'Please run this with an even number of processes.'
    call MPI_Abort(MPI_COMM_WORLD, 0, ierror)
end if

call system_clock(clock_start, clock_rate)
if (mod(rank, 2) .eq. 0) then
    buffer = 1
    do i=1, iterations
        call MPI_Send(buffer, size(buffer), MPI_REAL, rank + 1, 1, MPI_COMM_WORLD, ierror)
    end do
else
    do i=1, iterations
        call MPI_Recv(buffer, size(buffer), MPI_REAL, rank - 1, 1, MPI_COMM_WORLD, status, ierror)
    end do
endif
call system_clock(clock_end, clock_rate)

call MPI_Finalize(ierror)
    if (mod(rank, 2) .eq. 0) then
        total_time = (clock_end - clock_start)/real(clock_rate)
        messages_per_second = iterations / total_time
        print*, total_time, "s for ", iterations, " messages, a rate of ", messages_per_second, " messages per second."
    end if
end program message_rate

