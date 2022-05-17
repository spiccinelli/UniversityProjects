! *** Module with double complex matrix type elements, dimensions, trace and determinant ***

module complex_matrix
    use checkpoint
    implicit none

    ! define new type: double complex matrix
    type cmat
        complex(8), dimension(:,:), allocatable :: elem
        integer(4), dimension(2) :: dim
        complex(8) :: tr
        complex(8) :: det
    end type cmat

    ! specify interface operators
    interface operator(.ZINIT.)
        module procedure cmat_init_zero
    end interface

    interface operator(.RINIT.)
        module procedure cmat_init_rand
    end interface

    interface operator(.RH.)
        module procedure hmat_init_rand
    end interface

    interface operator(.DH.)
        module procedure hmat_init_diag
    end interface

    interface operator(.TR.)
        module procedure cmat_tr
    end interface

    interface operator(.ADJ.)
        module procedure cmat_adj
    end interface

    contains

    ! initialise matrix with zeros elements
    function cmat_init_zero(dim) result(M)
        integer(4), dimension(2), intent(in) :: dim
        integer(4) :: ii, jj
        type(cmat) :: M

        if ((dim(1).LE.0).AND.(dim(2).LE.0)) then
            call check(debug=.TRUE., &
                    msg="Invalid dimensions in initialization of complex matrix type.", &
                    msg_type="Error", stop_exec=.TRUE.)
        else
            M%dim(1) = dim(1)
            M%dim(2) = dim(2)

            allocate(M%elem(M%dim(1), M%dim(2)))

            do jj=1,dim(2)
                do ii=1,dim(1)
                    M%elem(ii,jj) = COMPLEX(0d0, 0d0)
                end do
            end do

            M%tr = cmat_tr(M)

        end if

    end function cmat_init_zero

    ! initialise matrix with random elements
    function cmat_init_rand(dim) result(M)
        integer(4), dimension(2), intent(in) :: dim
        integer(4) :: ii, jj
        type(cmat) :: M

        if ((dim(1).LE.0).AND.(dim(2).LE.0)) then
            call check(debug=.TRUE., &
                    msg="Invalid dimensions in initialization of complex matrix type.", &
                    msg_type="Error", stop_exec=.TRUE.)
        else
            M%dim(1) = dim(1)
            M%dim(2) = dim(2)

            allocate(M%elem(M%dim(1), M%dim(2)))

            do jj=1,dim(2)
                do ii=1,dim(1)
                    M%elem(ii,jj) = COMPLEX(rand(), rand())
                end do
            end do

            M%tr = cmat_tr(M)

        end if

    end function cmat_init_rand

    function random_stdnormal() result(x)
        real(8) :: x
        real(8), parameter :: pi = 4.d0*atan(1.d0)
        real(8) :: u1,u2

        call RANDOM_NUMBER(u1)
        call RANDOM_NUMBER(u2)
        x = sqrt(-2*log(u1))*cos(2*pi*(u2))
     end function random_stdnormal

    ! initialise hermitian matrix with random elements
    function hmat_init_rand(dim) result(M)
        integer(4), intent(in) :: dim
        integer(4) :: ii, jj
        type(cmat) :: M

        ! pre-condition: check invalid dimensions for hmat
        if (dim.LE.0) then
            call check(debug=.TRUE., &
                    msg="Invalid dimensions in initialization of hermitian matrix.", &
                    msg_type="Error", stop_exec=.TRUE.)
        else
            M%dim(1) = dim
            M%dim(2) = dim

            allocate(M%elem(M%dim(1), M%dim(2)))

            do jj=1,dim
                do ii=1,dim
                    if (jj.GT.ii) then
                        M%elem(ii,jj) = COMPLEX(random_stdnormal(), random_stdnormal())
                        M%elem(jj,ii) = CONJG(M%elem(ii,jj))
                    else if (ii.EQ.jj) then
                        M%elem(ii,jj) = COMPLEX(random_stdnormal(), 0d0)
                    else
                        continue
                    end if
                end do
            end do

            M%tr = cmat_tr(M)

        end if

    end function hmat_init_rand

    ! initialise diagonal hermitian matrix with random elements
    function hmat_init_diag(dim) result(M)
        integer(4), intent(in) :: dim
        integer(4) :: ii
        type(cmat) :: M

        ! pre-condition: check invalid dimensions for hmat
        if (dim.LE.0) then
            call check(debug=.TRUE., &
                    msg="Invalid dimensions in initialization of diagonal hermitian matrix.", &
                    msg_type="Error", stop_exec=.TRUE.)
        else
            M%dim(1) = dim
            M%dim(2) = dim

            allocate(M%elem(M%dim(1), M%dim(2)))
            M%elem = COMPLEX(0d0,0d0)

            do ii=1,dim
                M%elem(ii,ii) = COMPLEX(random_stdnormal(), 0d0)
            end do

            M%tr = cmat_tr(M)

        end if

    end function hmat_init_diag

    ! define trace operation
    function cmat_tr(M) result(trace)
        type(cmat), intent(in) :: M
        complex(8) :: trace
        integer(4) :: ii

        trace = COMPLEX(0d0, 0d0)

        if (M%dim(1).NE.M%dim(2)) then
            call check(debug=.TRUE., &
                    msg="Cannot compute trace: input is not a square matrix.", &
                    msg_type="Error", stop_exec=.TRUE.)
        else

            do ii=1,M%dim(1)
                trace = trace + M%elem(ii,ii)
            end do

        end if

    end function cmat_tr

    ! define adjoint operation
    function cmat_adj(M) result(adj_mat)
        type(cmat), intent(in) :: M
        type(cmat) :: adj_mat

        adj_mat%dim(1) = M%dim(2)
        adj_mat%dim(2) = M%dim(1)

        allocate(adj_mat%elem(adj_mat%dim(1), adj_mat%dim(2)))

        adj_mat%elem = CONJG(TRANSPOSE(M%elem))

        if (M%dim(1).NE.M%dim(2)) then
            call check(debug=.TRUE., &
                    msg="Cannot compute trace: input is not a square matrix.", &
                    msg_type="Error", stop_exec=.TRUE.)

        else
            adj_mat%tr = CONJG(M%tr)

        end if
    end function cmat_adj

    ! write on file
    subroutine cmat_write(M,format,name)
        type(cmat)  :: M
        logical :: format
        character(*), intent(in), optional :: name
        integer(4) :: ii
        character(:), allocatable :: file_name

        ! if name is not present, file_name is set to the current time and date
        if (present(name)) then
            file_name = trim(adjustl(name))
        else
            file_name = trim(fdate())
        end if

        ! open file and write matrix dimensions
        open(unit=10, file="mat_" // file_name // ".dat", action="write", status="replace")
        write(unit=10, fmt="(A,i10.1,A,i10.1,/)") "Matrix dimensions: ", M%dim(1), "x", M%dim(2)

        ! write matrix elements
        write(unit=10, fmt="(A,/)") "Matrix elements:"
        do ii=1,M%dim(1)
            ! if logical var format is true, output is formatted (https://bit.ly/3ChxboO)
            if (format) then
                write(10, fmt="(*('('sf9.6xspf9.6'i)':x))") M%elem(ii,:)
            else
                write(10, *) M%elem(ii,:)
            end if
        end do

        ! write matrix trace and determinant
        if (M%dim(1).EQ.M%dim(2)) then
            write(10, fmt="(/,A)", advance="no") "Trace: "
            if (format) then
                write(10, fmt="(*('('sf9.6xspf9.6'i)':x))") M%tr
            else
                write(10,*) M%tr
            end if
        else
            write(10,fmt="(/,A)") "Trace: not a square matrix."
        end if

        write(unit=10,fmt="(/,A)", advance="no") "Determinant: "
        write(10,*) "To be implemented"

        close(10)
    end subroutine cmat_write

    ! delete matrix if currently allocated
    subroutine cmat_del(M)
        type(cmat) :: M

        if (allocated(M%elem)) then
           M%dim = 0
           deallocate(M%elem)
        else
            call check(debug=.TRUE., &
            msg="Cannot delete: trying to deallocate a non-existent matrix.", &
            msg_type="Warning", stop_exec=.TRUE.)

        end if
        return
    end subroutine cmat_del

end module complex_matrix