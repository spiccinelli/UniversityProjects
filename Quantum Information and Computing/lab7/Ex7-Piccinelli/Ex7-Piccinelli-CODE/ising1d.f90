! Module to initialise and diagonalize the Hamiltonian for the transverse Ising model of N spin-1/2 particles on a 1D lattice

module ising1d
    implicit none
    
contains

    function identity_c(N) result(identity) ! Returns the complex (2^Nx2^N) identity matrix

        integer(4) :: N                                     ! System size
        complex(8), dimension(:,:), allocatable :: identity ! Identity matrix

        integer(4) :: ii                                    ! Auxiliary variable for counters

        allocate(identity(2**N,2**N))

        identity = COMPLEX(0.0d0, 0.0d0)
        forall ( ii = 1:size(identity,1) ) identity(ii,ii) = COMPLEX(1.0d0,0.0d0)
    end function identity_c
!------------------------------------------------------------------------------------------------------------------------------------
    function kronecker_product_c(M1, M2) result(M1_kp_M2) ! Returns the tensor product of two generic complex matrices

        complex(8), dimension(:,:)              :: M1, M2             ! Matrices to take the tensor product of
        complex(8), dimension(:,:), allocatable :: M1_kp_M2           ! Matrix containing the tensor product of M1 and M2

        integer(4)                              :: nr1, nc1, nr2, nc2 ! Number of rows and columns of the two matrices
        integer(4)                              :: ii, jj             ! Auxiliary variables for counters

        nr1 = size(M1,1)
        nc1 = size(M1,2)
        nr2 = size(M2,1)
        nc2 = size(M2,2)

        allocate(M1_kp_M2(nr1*nr2,nc1*nc2))

        ! Initialise tensor product to zero
        M1_kp_M2 = 0.0d0
        forall ( ii = 1:nr1, jj = 1:nc1 ) M1_kp_M2(nr2*(ii-1)+1 : nr2*ii, nc2*(jj-1)+1 : nc2*jj)  =  M1(ii,jj)*M2
        
    end function kronecker_product_c
!------------------------------------------------------------------------------------------------------------------------------------
    subroutine ising_ham(N, lambda, ising) ! Returns the initialised Hamiltonian
        integer(4) :: N                                  ! Number of spins
        real(8)    :: lambda                             ! Interaction strength parameter
        complex(8), dimension(:,:), allocatable :: ising ! Initialised Hamiltonian

        complex(8), dimension(2,2) :: s_x, s_z           ! Pauli matrices
        integer(4) :: ii                                 ! Auxiliary variables for counters

        ! Pauli matrices initialisation
        s_x      = COMPLEX( 0.0d0, 0.0d0)
        s_x(1,2) = COMPLEX( 1.0d0, 0.0d0)
        s_x(2,1) = COMPLEX( 1.0d0, 0.0d0)
        s_z      = COMPLEX( 0.0d0, 0.0d0)
        s_z(1,1) = COMPLEX( 1.0d0, 0.0d0)
        s_z(2,2) = COMPLEX(-1.0d0, 0.0d0)

        allocate(ising(2**N,2**N))

        ! Initialise Hamiltonian to zero
        ising = COMPLEX(0.0d0, 0.0d0)

        ! First term of the Ising Hamiltonian
        do ii=1,N
            ising = ising + kronecker_product_c(                        &
                            kronecker_product_c(identity_c(ii-1), s_z), &
                            identity_c(N-ii)                            &
            )
        end do

        ! Multiply by lambda
        ising = COMPLEX(lambda,0.0d0) * ising

        ! Second term of the Ising Hamiltonian
        do ii=1,N-1
            ising = ising - kronecker_product_c(                        &
                            kronecker_product_c(                        &
                            kronecker_product_c(identity_c(ii-1), s_x), &
                            s_x), identity_c(N-ii-1)                    &
            )

        end do

    end subroutine ising_ham
!------------------------------------------------------------------------------------------------------------------------------------
    subroutine eigs_ham(mat, eigs) ! Diagonalize a generic complex matrix
        complex(8), dimension(:,:)         :: mat     ! Complex matrix of generic dimensions
        real(8), dimension(:), allocatable :: eigs    ! Vector of eigenvalues

        integer(4)                         :: dim     ! Matrix dimension
        integer(4), parameter              :: nb = 64 ! ZHEEV subroutine parameters
        integer(4)                         :: lwork, info
        real(8), allocatable               :: rwork(:)
        complex(8), allocatable            :: work(:)
        complex(8)                         :: dummy(1)

        dim = size(mat, 1)

        allocate(eigs(dim))
        allocate(rwork(max(1, 3*dim-2)))
        lwork = -1

        call ZHEEV('V', 'U', dim, mat, dim, eigs, dummy, lwork, rwork, info)

        lwork = max((nb+1)*dim, nint(real(dummy(1))))
        allocate(work(lwork))

        call ZHEEV('N', 'U', dim, mat, dim, eigs, work, lwork, rwork, info)

        deallocate(work)
        deallocate(rwork)

    end subroutine eigs_ham
!------------------------------------------------------------------------------------------------------------------------------------
    subroutine print_mat_std(mat, formatted) ! Print matrix on standard output

        complex(8), dimension(:,:) :: mat       ! Density matrix
        logical                    :: formatted ! If .TRUE. elements are printed in formatted way
    
        integer(4)                 :: ii, jj    ! Auxiliary variable for counters
    
        do ii=1,size(mat,1)
            if (formatted) then
                write(*, "(*('('sf7.4xspf7.4'i )':x))") (mat(ii,jj), jj=1,size(mat,2))
            else
                write(*, *) (mat(ii,jj), jj=1,size(mat,2))
            end if
        end do
    
    end subroutine print_mat_std
!------------------------------------------------------------------------------------------------------------------------------------
    subroutine print_eigs_std(eigs, formatted) ! Print eigenvalues on standard output

        real(8), dimension(:) :: eigs      ! Vector of eigenvalues
        logical               :: formatted ! If .TRUE. elements are printed in formatted way
    
        integer(4)            :: ii        ! Auxiliary variable for counters
    
        do ii=1,size(eigs,1)
            if (formatted) then
                write(*, "(*(sf9.6:x))") eigs(ii)
            else
                write(*, *) eigs(ii)
            end if
        end do
    
    end subroutine print_eigs_std

end module ising1d