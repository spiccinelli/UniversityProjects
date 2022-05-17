! Demo program for the module ising1d: no control on input variables is set

program demo
    use ising1d
    implicit none
    external open_file
    
    integer(4)                              :: N              ! Number of spins
    real(8)                                 :: lambda         ! Interaction strength parameter

    complex(8), dimension(:,:), allocatable :: ham            ! TFI Hamiltonian
    complex(8), dimension(:,:), allocatable :: ham_L, ham_R   ! Hamiltonian interaction terms
    complex(8), dimension(:,:), allocatable :: eigvec         ! Matrix of eigenvectors
    real(8), dimension(:), allocatable      :: eigval         ! Vector of eigenvalues
    complex(8), dimension(2,2)              :: s_x            ! Pauli sigma_x matrix
    real(8)                                 :: e_prec, e_next ! Ground state values for consecutive iterations
    real(8)                                 :: eps            ! Precision threshold
    integer(4)                              :: Niter          ! Number of iterations

    character(:), allocatable               :: arg_N, arg_L   ! Retrieve argument passed on the command line
    integer(4)                              :: arglen         ! Command line argument length

    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: arg_N)
    call GET_COMMAND_ARGUMENT(1, value=arg_N)
    read(arg_N(:),'(i1)') N

    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: arg_L)
    call GET_COMMAND_ARGUMENT(2, value=arg_L)
    read(arg_L(:),'(f3.2)') lambda

    allocate(ham_L(2**N,2**N), ham_R(2**N,2**N))
    allocate(eigvec(2**N,2**N))
    allocate(eigval(2**N))

    ! Pauli matrices initialisation
    s_x      = COMPLEX(0.0d0, 0.0d0)
    s_x(1,2) = COMPLEX(1.0d0, 0.0d0)
    s_x(2,1) = COMPLEX(1.0d0, 0.0d0)

    eps    = 1.0d-10
    eigval = 0.0d0
    Niter  = 0

    ! Initialisations of Hamiltonians
    call ising_ham(N, lambda, ham)

    ham_L = kronecker_product_c(identity_c(N-1), s_x)
    ham_R = kronecker_product_c(s_x, identity_c(N-1))

    ! Main loop
    e_prec = 0.1
    e_next = -1

    do while(abs(e_next - e_prec) > eps)
        e_prec = e_next

        ham   = ham   / 2.0d0
        ham_L = ham_L / SQRT(2.0d0)
        ham_R = ham_R / SQRT(2.0d0)

        call rsrg_step(N, ham, ham_L, ham_R, e_next)
        Niter  = Niter + 1
    end do

    ! Final diagonalization
    call diag_mat(ham, eigvec, eigval)

    ! Write on file
    call open_file('GS_ising', 10)
    write(10,"(g0ag0ag0ag0)") N, achar(9), lambda, achar(9), Niter, achar(9), eigval(1) / N
    close(10)

    deallocate(ham_L, ham_R)
    deallocate(eigvec)
    deallocate(eigval)

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