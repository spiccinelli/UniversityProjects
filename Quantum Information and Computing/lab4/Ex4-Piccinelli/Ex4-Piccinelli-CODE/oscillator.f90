! ***The program finds the eigenvalues and eigenfunctions of the one-dimensional armonic oscillator***
!
! [    d^2       2m        ]         2m*E
! [ - -----  + ------ V(x) ] Ψ(x) = ------  Ψ(x)
! [   d x^2    hbar^2      ]        hbar^2
!
! To solve Schroedinger equation, a fourth-order approximation for the discretized derivative
! (Numerov method) is used, while for the eigenvalue problem LAPACK's DSTEV is employed.
! Adimensional units: we set hbar=1, m=1.

module oscillator
    implicit none
    contains

    function hamiltonian(x_max, N, omega) result(H)
        integer(4) :: N
        real(8) :: x_max, omega
        real(8), dimension(N,N) :: H

        integer(4) :: ii
        real(8) :: delta_x

        delta_x = (2 * ABS(x_max)) / (N-1)

        H = 0.d0

        do ii=1,N
            H(ii,ii) = (1.0d0/(delta_x**2)) + (0.5d0*omega**2) * (-ABS(x_max) + delta_x*(ii-1))**2
        end do

        do ii=1,N-1
            H(ii,ii+1) = - (1.0d0/(2.0d0*delta_x**2))
            H(ii+1,ii) = H(ii,ii+1)
        end do

    end function hamiltonian

    subroutine diagonalize(H, eigs)
        real(8), dimension(:,:), intent(in) :: H
        real(8), dimension(:), intent(out) :: eigs
        
        real(8), dimension(size(H,1)) :: D
        real(8), dimension(size(H,1)-1) :: E
        real(8), allocatable :: work(:)
        integer(4) :: N, info, lwork
        integer(4) :: ii

        N = size(H,1)

        do ii=1,N
            D(ii) = H(ii,ii)
        end do

        do ii=1,N-1
            E(ii) = H(ii,ii+1)
        end do

        lwork = max(1, 2*N-2)
        allocate(work(lwork))
        call DSTEV('V', N, D, E, H, N, work, info)

        deallocate(work)
        eigs = D

    end subroutine diagonalize

end module