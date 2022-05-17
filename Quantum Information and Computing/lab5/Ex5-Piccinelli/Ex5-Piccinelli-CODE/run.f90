program run
    use oscillator
    implicit none

    real(8), dimension(:,:), allocatable :: H
    real(8), dimension(:), allocatable :: eigs
    real(8), dimension(:), allocatable :: xs, ts
    complex(8), dimension(:), allocatable :: psi
    complex(8), dimension(:,:), allocatable :: psi_register

    real(8) :: dx, dt
    real(8) :: t0, time, tau, t_max
    real(8) :: m, omega
    real(8) :: x_max
    integer(4) :: Nx, Nt
    integer(4) :: ii, jj
    character(4) :: psi_nr
    character(len=1024) :: filename

    print *, "****************************************************************************************"
    print *, "Time evolution of the ground state for a ime-dependen one-dimensional armonic oscillator"
    print *, "Nx    ->  number of spatial discretization intervals"
    print *, "x_max ->  right/left extreme of the simulation interval"
    print *, "Nt    ->  number of time discretization intervals"
    print *, "t_max ->  final time of simulation"
    print *, "tau   ->  characteristic time of the potential"
    print *, "omega ->  angular frequency of the oscillator"
    print *, "m     ->  mass of the oscillator"
    print *, "****************************************************************************************"
    print *, "Insert Nx (> 1000):"
    read *, Nx
    print *, "Insert x_max (e.g. 5):"
    read *, x_max
    print *, "Insert Nt (> 1000):"
    read *, Nt
    print *, "Insert t_max (e.g. 8):"
    read *, t_max
    print *, "Insert tau (e.g. 1):"
    read *, tau
    print *, "Insert omega (e.g. 1):"
    read *, omega
    print *, "Insert m (e.g. 1):"
    read *, m

    ! change filename during execution
    write(filename, "(ai4af4.1ai4af4.1af4.1af3.1af3.1af3.1)") &
        "Nx", Nx, "xmax", x_max,"Nt", Nt, "tmax", t_max, "tau", tau, "O", omega, "m", m

    x_max = ABS(x_max)
    dx = (2 * x_max) / Nx

    t0 = 0.0d0
    dt = (t_max - t0) / Nt

    allocate(H(Nx+1,Nx+1))
    allocate(eigs(Nx+1))
    allocate(xs(Nx+1))
    allocate(ts(Nt+1))
    allocate(psi(Nx+1))
    allocate(psi_register(Nx+1,Nt+1))

    print *, "Computing the Hamiltonian and diagonalizing ..."
    H = hamiltonian(Nx, x_max, m, omega, t0, tau)
    call diagonalize(H, eigs)

    ! reconstruct time and space grid values
    do ii=1,size(xs,1)
        xs(ii) = -x_max + (ii-1)*dx
    end do
    do ii=1,size(ts,1)
        ts(ii) = (ii-1)*dt
    end do

    ! normalize eigenfunctions
    do ii=1,size(H,1)
        do jj=1,size(H,2)
            H(ii,jj) = H(ii,jj) * SQRT(1.0d0/dx)
        end do
    end do

    print *, "... extracting ground state at t0 and saving ..."
    time = 0.0d0
    do ii=1,size(H,1)
        psi(ii) = COMPLEX(H(ii,1), 0.0d0)
    end do
    psi_register(:,1) = psi
    write(psi_nr, "(i4)") 1
    call save_psi(xs, psi,"psi/"//TRIM(psi_nr)//"_psi_"//TRIM(filename)//".dat")

    print *, "... iterating for every time step and saving ..."
    do ii=1,Nt
        time = time + dt
        call time_step(psi, dt, x_max, m, omega, time, tau)
        psi_register(:,ii+1) = psi
        write(psi_nr, "(i4)") ii
        call save_psi(xs, psi, "psi/"//TRIM(psi_nr)//"_psi_"//TRIM(filename)//".dat")
    end do

    print *, "... saving time evolution of the wave function on file ..."
    open(10, file="cmap_"//TRIM(filename)//".dat", status="replace")
    do ii=1,size(ts,1)
        do jj=1, size(xs,1)
            write(10,'(g0ag0ag0)') xs(jj), char(9), ts(ii), char(9), REAL(psi_register(jj,ii)*CONJG(psi_register(jj,ii)))
        end do
    end do
    close(10)

    print *, "... done!"

end program run