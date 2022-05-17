program run
    use oscillator
    implicit none

    real(8), dimension(:,:), allocatable :: H
    real(8), dimension(:), allocatable :: eigs_num, eigs_th, eps
    real(8), dimension(:), allocatable :: xs, ns
    real(8) :: delta_x
    integer(4) :: ii, jj
    character(len=1024) :: filename

    integer(4) :: N
    real(8) :: x_max, omega

    print *, "*************************************************************************"
    print *, "Eigenvalues and eigenfunctions for the one-dimensional armonic oscillator"
    print *, "  N   ->  number of points in the discretization grid"
    print *, "x_max ->  right/left extreme of the mesh"
    print *, "omega ->  angular frequency of the oscillator"
    print *, "*************************************************************************"
    print *, "Insert N > 1000:"
    read *, N
    print *, "Insert x_max (e.g. 5):"
    read *, x_max
    print *, "Insert omega (e.g. 1):"
    read *, omega

    delta_x = (2 * ABS(x_max)) / (N-1)

    allocate(H(N,N))
    allocate(eigs_num(N))
    allocate(eigs_th(N))
    allocate(eps(N))
    allocate(xs(N))
    allocate(ns(N))

    print *, "Computing ..."

    H  = hamiltonian(x_max, N, omega)
    call diagonalize(H, eigs_num)

    ! reconstruct grid values, compute theoretical expectation values
    do ii=1,N
        xs(ii) = (-ABS(x_max)) + delta_x * (ii-1)
        ns(ii) =  (ii - 1.0d0)

        eigs_th(ii) = ((ii - 1.0d0) + 0.5d0) * omega
        eps(ii) = ABS(eigs_num(ii) - eigs_th(ii)) / eigs_th(ii)
    end do

    ! normalize eigenfunctions
    do ii=1,N
        do jj=1,N
            H(ii,jj) = H(ii,jj) * SQRT(1/delta_x)
        end do
    end do

    ! change filename during execution
    write(filename, "(ai4af4.1af4.1)") "_N=", N, "_xmax=", x_max, "_O=", omega

    print *, "... writing results on file ..."
    ! write eigenvalues
    open(10, file="eigv"//trim(filename)//".dat", status="replace")
    do ii=1,N
        write(10,"(g0ag0ag0ag0)") ns(ii), char(9), eigs_th(ii), char(9), eigs_num(ii), char(9), eps(ii)
    end do
    close(10)

    ! write eigenfunctions
    open(12, file="eigf"//trim(filename)//".dat", status="replace")
    do ii=1,N
        do jj=1,N
            write(12,'(g0ag0)') xs(jj), char(9), H(jj,ii)
        end do
    end do
    close(12)

    print *, "... done!"
    
end program run