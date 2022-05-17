! Program to test the performance of the initialisation of (non-)separable quantum states

program performance
    use quantum_system
    implicit none
    external open_file

    complex(8), dimension(:), allocatable :: state    ! Vector of coefficients
    real(8)                               :: ti, tf   ! Time instants
    integer(4)                            :: dd, nn   ! Counters for D, N
    character(len=1024)                   :: filename ! Output file name

    print*, "Not-separable state"
    do dd=2,6
        write(filename, "(ai1)") "notsep_D=", dd
        call open_file(filename, 10+dd)
        do nn=2,10
            print*, "Writing N =", nn, ", D =", dd
            call CPU_TIME(ti)
            state = rand_state_init(nn, dd, .FALSE.)
            call CPU_TIME(tf)
            write(10+dd,"(g0ag0ag0)") nn, achar(9), tf - ti, achar(9), SIZEOF(state)
        end do
        close(10+dd)
    end do

    print*, "Separable state"
    do dd=2,6
        write(filename, "(ai1)") "sep_D=", dd
        call open_file(filename, 10+dd)
        do nn=10**4,5*10**6,5*10**4
            print*, "Writing N =", nn, ", D =", dd
            call CPU_TIME(ti)
            state = rand_state_init(nn, dd, .TRUE.)
            call CPU_TIME(tf)
            write(10+dd,"(g0ag0ag0)") nn, achar(9), tf - ti, achar(9), SIZEOF(state)
        end do
        close(10+dd)
    end do

end program performance


subroutine open_file(filename, unit)

    character(*), intent(in)  :: filename
    integer(4)                :: unit

    character(:), allocatable :: name
    logical                   :: exist

    name = TRIM(ADJUSTL(filename))

    inquire(file=name//".dat", exist=exist)
    if (exist) then
        open(unit=unit, file=name//".dat", status="old", position="append", action="write")
    else
        open(unit=unit, file=name//".dat", status="new", action="write")
    end if

end subroutine open_file