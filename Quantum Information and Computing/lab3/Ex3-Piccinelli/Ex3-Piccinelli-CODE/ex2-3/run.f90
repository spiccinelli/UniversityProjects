program run
    use hmat_ops
    use complex_matrix
    implicit none

    integer(4) :: dim, ii
    real(8), dimension(:), allocatable :: eigs_random(:), eigs_diagonal(:)

    dim = 1000
    print *, "Starting..."

    call write_eigs("eigs_random", 10)
    call write_eigs("spacing_random", 11)
    call write_eigs("eigs_diagonal", 12)
    call write_eigs("spacing_diagonal", 13)

    do ii=1,50

        print *, "Iteration: ", ii

        eigs_random = hmat_eigs(dim=dim, diagonal=.FALSE.)
        write(10,'(*(g0.6,:,","))') eigs_random
        write(11,'(*(g0.6,:,","))') hmat_spacing(eigs_random)

        eigs_diagonal = hmat_eigs(dim=dim, diagonal=.TRUE.)
        write(12,'(*(g0.6,:,","))') eigs_diagonal
        write(13,'(*(g0.6,:,","))') hmat_spacing(eigs_diagonal)

    end do

    close(10)
    close(11)
    close(12)
    close(13)
   
end program run