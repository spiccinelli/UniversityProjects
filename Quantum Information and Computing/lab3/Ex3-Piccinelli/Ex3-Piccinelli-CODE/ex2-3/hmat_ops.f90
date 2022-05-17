module hmat_ops
    use complex_matrix
    implicit none

    contains

    function hmat_eigs(dim, diagonal) result(eigs)
        integer(4), intent(in) :: dim
        logical :: diagonal
        real(8), allocatable :: eigs(:)

        type(cmat) :: A
        integer(4), parameter :: nb = 64
        integer(4) :: lwork, info
        real(8), allocatable :: rwork(:)
        real(8) :: sum_eigs, tr, thld = 1d-06
        complex(8), allocatable :: work(:)
        complex(8) :: dummy(1)

        if (dim.LE.0) then
            call check(debug=.TRUE., &
                    msg="Invalid dimension, cannot compute eigenvalues: stopping.", &
                    msg_type="Error", stop_exec=.TRUE.)
        else
            A%dim(1) = dim
            A%dim(2) = dim
                    
            allocate(A%elem(A%dim(1), A%dim(2)))

            allocate(eigs(dim))
            allocate(rwork(max(1, 3*dim-2)))

            lwork = -1
            call ZHEEV('N', 'U', dim, A%elem, dim, eigs, dummy, lwork, rwork, info)

            lwork = max((nb+1)*dim, nint(real(dummy(1))))
            allocate(work(lwork))

            if (.NOT.diagonal) then
                ! init hermitian matrix
                A = .RH.dim
            else
                A = .DH.dim
            end if

            call ZHEEV('N', 'U', dim, A%elem, dim, eigs, work, lwork, rwork, info)

            if (info.EQ.0) then
                call check(debug=.TRUE., &
                        msg="The algorithm converged.", &
                        msg_type="OK", stop_exec=.FALSE.)
            else
                call check(debug=.TRUE., &
                        msg="The algorithm failed to converge!", &
                        msg_type="Warning", stop_exec=.FALSE.)

            end if

            sum_eigs = SUM(eigs)
            tr = REALPART(.TR.A)

            if (ABS(sum_eigs - tr) < 0.5*thld*ABS(sum_eigs + tr)) then
                call check(debug=.TRUE., &
                        msg="Sum of eigv matches trace of initial matrix.", &
                        msg_type="OK", stop_exec=.FALSE.)
            else
                call check(debug=.TRUE., &
                        msg="Sum of eigv does not match trace of initial matrix.", &
                        msg_type="Warning", stop_exec=.FALSE.)
            end if

            deallocate(work)
            deallocate(rwork)

        end if

    end function hmat_eigs

    function hmat_spacing(eigs) result(spacing)
        real(8), dimension(:)  :: eigs

        integer(4) :: ii
        real(8), dimension(size(eigs,1)-1) :: spacing
        real(8) :: avg_spacing

        do ii=1,size(eigs,1)-1
            spacing(ii) = eigs(ii+1) - eigs(ii)
        end do

        avg_spacing = (eigs(size(eigs,1)) - eigs(1)) / size(spacing,1)

        spacing = spacing / avg_spacing

    end function hmat_spacing

    subroutine write_eigs(name, unit)

        character(*), intent(in), optional :: name
        integer(4) :: unit

        character(:), allocatable :: file_name
        logical :: exist

        if (present(name)) then
            file_name = trim(adjustl(name))
        else
            file_name = trim(fdate())
        end if

        inquire(file=file_name // ".dat", exist=exist)
        if (exist) then
            open(unit=unit, file=file_name // ".dat", status="old", position="append", action="write")
        else
            open(unit=unit, file=file_name // ".dat", status="new", action="write")
        end if

    end subroutine

end module hmat_ops