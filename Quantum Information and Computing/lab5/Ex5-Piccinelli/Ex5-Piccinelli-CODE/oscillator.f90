module oscillator
    use, intrinsic :: iso_c_binding
    implicit none
    
    contains

    function hamiltonian(N, x_max, m, omega, t0, tau) result(H)
        integer(4) :: N
        real(8) :: x_max
        real(8) :: m
        real(8) :: omega
        real(8) :: t0
        real(8) :: tau

        real(8), dimension(N+1,N+1) :: H
        real(8) :: dx
        integer(4) :: ii

        x_max = ABS(x_max)
        dx = (2 * x_max) / N

        H = 0.0d0

        do ii=1,N+1
            H(ii,ii) = (1.0d0/(m*dx**2)) + (0.5d0*m*omega**2) * (-x_max + dx*(ii-1) - t0/tau)**2
        end do

        do ii=1,N
            H(ii,ii+1) = - (1.0d0/(2.0d0*m*dx**2))
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

    function kinetic(N, x_max, m) result(H_T)
        integer(4) :: N
        real(8) :: x_max
        real(8) :: m

        real(8), dimension(N+1) :: H_T
        real(8) :: PI = 4.0d0*DATAN(1.0d0)
        real(8) :: dk
        integer(4) :: ii

        x_max = ABS(x_max)
        dk = PI / x_max

        do ii=1,N+1
            if(ii.LE.INT(N/2)) then
                   H_T(ii) = (1.0d0/(2.0d0*m)) * (dk*ii)**2
            else if (ii.GT.INT(N/2)) then
                H_T(ii) = (1.0d0/(2.0d0*m)) * (dk*(ii-N-1))**2
            end if
        end do

    end function kinetic

    function potential(N, x_max, m, omega, time, tau) result(H_V)
        integer(4) :: N
        real(8) :: x_max
        real(8) :: m
        real(8) :: omega
        real(8) :: time
        real(8) :: tau

        real(8), dimension(N+1) :: H_V
        real(8) :: dx
        integer(4) :: ii

        x_max = ABS(x_max)
        dx = (2 * x_max) / N

        do ii=1,N+1
            H_V(ii) = (1.0d0/2.0d0) * m * omega**2 * (-x_max + (ii-1)*dx - time/tau)**2
        end do

    end function potential

    function FFT(psi) result(FFT_psi)
        complex(8), dimension(:) :: psi

        complex(8), dimension(size(psi,1)) :: FFT_psi
        integer(4) :: N
        integer(8) :: plan

        N = size(psi,1)

        call dfftw_plan_dft_1d(plan, N, psi, FFT_psi, -1, 64)
        call dfftw_execute_dft(plan, psi, FFT_psi)

        FFT_psi = FFT_psi / SQRT(DBLE(N)+1)
        
        call dfftw_destroy_plan(plan)

    end function FFT

    function AFFT(psi) result(AFFT_psi)
        complex(8), dimension(:) :: psi

        complex(8), dimension(size(psi,1)) :: AFFT_psi
        integer(4) :: N
        integer(8) :: plan

        N = size(psi,1)

        call dfftw_plan_dft_1d(plan, N, psi, AFFT_psi, +1, 64)
        call dfftw_execute_dft(plan, psi, AFFT_psi)

        AFFT_psi = AFFT_psi / SQRT(DBLE(N)+1)

        call dfftw_destroy_plan(plan)

    end function AFFT

    subroutine time_step(psi, dt, x_max, m, omega, time, tau)
        complex(8), dimension(:), intent(inout) :: psi
        real(8), intent(in) :: dt
        real(8), intent(in) :: x_max
        real(8), intent(in) :: m
        real(8), intent(in) :: omega
        real(8), intent(in) :: time
        real(8), intent(in) :: tau

        real(8), dimension(size(psi,1)) :: V
        real(8), dimension(size(psi,1)) :: T
        integer(4) :: N
        integer(4) :: ii

        N = size(psi,1) - 1

        V = potential(N, x_max, m, omega, time, tau)
        T = kinetic(N, x_max, m)

        do ii=1,N+1
            psi(ii) = ZEXP(COMPLEX(0.0d0,-0.5d0*dt*V(ii))) * psi(ii)
        end do
        
        psi = FFT(psi)
        
        do ii=1,N+1
            psi(ii) = ZEXP(COMPLEX(0.0d0,-1.0d0*dt*T(ii))) * psi(ii)
        end do
        
        psi = AFFT(psi)

        do ii=1,N+1
            psi(ii) = ZEXP(COMPLEX(0.0d0,-0.5d0*dt*V(ii))) * psi(ii)
        end do

    end subroutine time_step

    subroutine save_psi(xs, psi, filename)
        real(8), dimension(:) :: xs
        complex(8), dimension(:) :: psi
        character(*) :: filename

        integer(4) :: ii

        open(10, file=filename, status='replace')
        do ii=1,size(xs,1)
            write(10,'(g0ag0ag0)') xs(ii), char(9), REAL(psi(ii)), char(9), AIMAG(psi(ii))
        end do
        close(10)
    end subroutine save_psi

end module oscillator