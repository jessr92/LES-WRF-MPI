program mpi_pi
use mpi_helper
implicit none
integer, parameter :: darts = 50000, rounds = 5000, master = 0
call main()

contains

subroutine main()
    implicit none
    real(kind=4) :: seednum
    real(kind=8) :: homepi, pi, avepi, pisum
    integer :: i
    external  d_vadd
    call initialise_mpi()
    call checkMPIError()
    avepi = 0.0
    do i = 1, rounds
        homepi = dboard()
        call MPI_Reduce(homepi, pisum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                        master, MPI_COMM_WORLD, ierror)
        if (rank .eq. master) then
            pi = pisum / mpi_size
            avepi = ((avepi*(i-1)) + pi) / i
            print*,'After ', i,' rounds, average value of pi = ', pi 
        endif
    end do
    if (rank .eq. master) then
        print *, ' '
        print *,'Real value of PI: 3.1415926535897'
        print *, ' '
    endif
    call finalise_mpi()
    call checkMPIError()
end subroutine main

real(kind=8) function dboard()
    implicit none
    integer :: score, n
    real(kind=4) :: r
    real(kind=8) :: x_coord, x_sqr, y_coord, y_sqr, pi
    do n = 1, darts
        call random_number(r)
        x_coord = (2.0 * r) - 1.0
        x_sqr = x_coord * x_coord
        call random_number(r)
        y_coord = (2.0 * r) - 1.0
        y_sqr = y_coord * y_coord
        if ((x_sqr + y_sqr) .le. 1.0) then
            score = score + 1
        endif
    end do
    pi = 4.0 * score / darts
    dboard = pi
end function dboard

end program mpi_pi

