! Demo program for the module ising1d: no control on input variables is set

program demo
    use ising1d
    implicit none
    external open_file
    
    integer(4)                              :: N                          ! Number of spins
    real(8)                                 :: lambda                     ! Interaction strength parameter
    integer(4)                              :: K                          ! Number of eigenvalues to save
    character(1)                            :: P                          ! Performance flag

    complex(8), dimension(:,:), allocatable :: ham                        ! TFI Hamiltonian
    real(8), dimension(:), allocatable      :: eigs                       ! Vector of eigenvalues

    logical                                 :: info                       ! Logical variable used during debug
    logical                                 :: perf                       ! If .TRUE. performance is monitored
    real(8)                                 :: ti, tf                     ! Time instants
    integer(4)                              :: ii                         ! Auxiliary variables for counters
    character(:), allocatable               :: arg_N, arg_L, arg_K, arg_P ! Retrieve argument passed on the command line
    integer(4)                              :: arglen                     ! Command line argument length

    info = .FALSE.

    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: arg_N)
    call GET_COMMAND_ARGUMENT(1, value=arg_N)
    read(arg_N(:),'(i2)') N

    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: arg_L)
    call GET_COMMAND_ARGUMENT(2, value=arg_L)
    read(arg_L(:),'(f3.2)') lambda

    call GET_COMMAND_ARGUMENT(3, length=arglen)
    allocate(character(arglen) :: arg_K)
    call GET_COMMAND_ARGUMENT(3, value=arg_K)
    read(arg_K(:),'(i2)') K

    call GET_COMMAND_ARGUMENT(4, length=arglen)
    allocate(character(arglen) :: arg_P)
    call GET_COMMAND_ARGUMENT(4, value=arg_P)
    read(arg_P(:),'(a1)') P

    if (P.EQ.'y') then
        perf = .TRUE.
    else
        perf = .FALSE.
    end if

    call CPU_TIME(ti)

    call ising_ham(N, lambda, ham)
    if (info) call print_mat_std(ham, .TRUE.)
    call eigs_ham(ham, eigs)

    call CPU_TIME(tf)

    if (perf) then
        call open_file('perf', 10)
        write(10,"(g0ag0)") N, achar(9), tf - ti
        close(10)
    else
        call open_file('eigv', 11)
        do ii=1,K
            if (ii<=2**N) write(11,"(g0ag0ag0ag0)") N, achar(9), lambda, achar(9), eigs(ii), achar(9), ii
        end do
        close(11)
    end if

    if (info) then
        write(*, "(a)") ''
        call print_eigs_std(eigs, .TRUE.)
    end if

end program demo

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