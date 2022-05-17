! Module of utilities to initialise and manipulate a quantum state

module quantum_system
    use checkpoint
    implicit none
    
contains
!-----------------------------------------------------------------------------------------------------------------------------------
function rand_state_init(N,D,isSep) result(state) ! Initialises random (non-)separable state

    integer(4)                            :: N      ! Number of subsystems
    integer(4)                            :: D      ! Local dimension
    logical                               :: isSep  ! .TRUE. if separable
    
    complex(8), dimension(:), allocatable :: state  ! Initialised state

    integer(4)                            :: dim    ! Dimension of the Hilbert space
    real(8)                               :: norm   ! Norm of the state
    real(8), dimension(:), allocatable    :: re, im ! Real and imaginary part of the coefficients

    if (isSep) then
        dim = N * D
    else
        dim = D**N
    end if

    allocate(state(dim))
    allocate(re(dim))
    allocate(im(dim))

    call RANDOM_NUMBER(re)
    call RANDOM_NUMBER(im)
    re = re*2.0d0 - 1.0d0
    im = im*2.0d0 - 1.0d0

    ! Initialise state vector and normalize
    norm  = SUM(re**2 + im**2)
    state = DCMPLX(re, im) / DCMPLX(SQRT(norm),0.0d0)

end function rand_state_init
!-----------------------------------------------------------------------------------------------------------------------------------
function compute_trace(M) result(trace)
    complex(8), dimension(:,:) :: M

    complex(8)                 :: trace

    integer(4)                 :: ii

    trace = COMPLEX(0d0, 0d0)

    if (SIZE(M,1).EQ.SIZE(M,2)) then
        do ii=1,SIZE(M,1)
            trace = trace + M(ii,ii)
        end do
    else
        call check(debug=.TRUE., msg="Cannot compute trace: input is not a square matrix", msg_type="Error", stop_exec=.TRUE.)
    end if

end function compute_trace
!-----------------------------------------------------------------------------------------------------------------------------------
function dmat_pure_state(state) result(rho) ! Compute the density matrix for a pure state

    complex(8), dimension(:)                :: state    ! Vector of coefficients

    complex(8), dimension(:,:), allocatable :: rho      ! Density matrix

    complex(8), dimension(:,:), allocatable :: bra, ket ! ρ = | ψ >< ψ |
    integer(4)                              :: dim      ! Dimension of the Hilbert space

    dim = size(state,1)

    allocate(rho(dim,dim))
    allocate(bra(1,dim))
    allocate(ket(dim,1))

    ket(:,1) = state 
    bra(1,:) = CONJG(state)

    rho = MATMUL(ket,bra)
    
end function dmat_pure_state
!-----------------------------------------------------------------------------------------------------------------------------------
function partial_trace_left(rho, D) result(rho_b) ! Compute the left partial trace for a bipartite system

    complex(8), dimension(:,:) :: rho        ! Bipartite matrix
    integer(4)                 :: D          ! Local dimension (N = D^2)

    complex(8), dimension(D,D) :: rho_b      ! Reduced matrix

    integer(4)                 :: dim        ! Dimension of the Hilbert space
    integer(4)                 :: ii, jj, kk ! Auxiliary variable for counters

    dim = SIZE(rho, 1)

    if (IAND(dim, dim-1).EQ.0) then ! Check if dimension is a power of 2
        rho_b = (0.d0,0.d0)
        do ii = 1,D
            do jj = 1,D
                do kk = 1,D
                    rho_b(ii,jj) = rho_b(ii,jj) + rho( (kk-1) * D + ii, (kk-1) * D + jj)
                end do
            end do
        end do
    else
        call check(debug=.TRUE., msg="System is not bipartite, stopping.", var=dim, msg_type="Error", stop_exec=.TRUE.)
    end if
    
end function partial_trace_left
!-----------------------------------------------------------------------------------------------------------------------------------
function partial_trace_right(rho, D) result(rho_a) ! Compute the right partial trace for a bipartite system

    complex(8), dimension(:,:) :: rho        ! Bipartite matrix
    integer(4)                 :: D          ! Local dimension (N = D^2)

    complex(8), dimension(D,D) :: rho_a      ! Reduced matrix

    integer(4)                 :: dim        ! Dimension of the Hilbert space
    integer(4)                 :: ii, jj, kk ! Auxiliary variable for counters

    dim = SIZE(rho, 1)

    if (IAND(dim, dim-1).EQ.0) then ! Check if dimension is a power of 2
        rho_a = (0.d0,0.d0)
        do ii = 1,D
            do jj = 1,D
                do kk = 1,D
                    rho_a(ii,jj) = rho_a(ii,jj) + rho( (ii-1) * D + kk, (jj-1) * D + kk)
                end do
            end do
        end do
    else
        call check(debug=.TRUE., msg="System is not bipartite, aborting.", msg_type="Error", stop_exec=.TRUE.)
    end if
    
end function partial_trace_right
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine print_state(state, formatted) ! Print random initialised state on standard output

    complex(8), dimension(:) :: state     ! Vector of coefficients
    logical                  :: formatted ! If .TRUE. elements are printed in formatted way

    integer(4)               :: ii

    do ii=1,size(state,1)
        if (formatted) then
            write(*, "('('sf7.4xspf7.4'i)')") state(ii)
        else
            write(*, *) state(ii)
        end if
    end do

end subroutine print_state
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine print_dmat(rho, formatted) ! Print random state density matrix on standard output

    complex(8), dimension(:,:) :: rho       ! Density matrix
    logical                    :: formatted ! If .TRUE. elements are printed in formatted way

    integer(4)                 :: ii, jj

    do ii=1,size(rho,1)
        if (formatted) then
            write(*, "(*('('sf7.4xspf7.4'i)':x))") (rho(ii,jj), jj=1,size(rho,2))
        else
            write(*, *) (rho(ii,jj), jj=1,size(rho,2))
        end if
    end do

end subroutine print_dmat

end module quantum_system