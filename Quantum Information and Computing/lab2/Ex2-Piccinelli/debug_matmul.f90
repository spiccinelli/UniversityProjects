! *** Module with pre- and post-conditions for matrix-matrix multiplication ***

module debug_matmul
    implicit none
    
contains

    ! PRE-CONDITIONS

    ! verifies that the matrix is of correct type: for each
    ! element it returns false if it is character or logical
    function input_control(mat) result(valid_input)
        class(*), dimension(:,:) :: mat
        logical :: valid_input
        integer(4) :: ii, jj
        integer(4) :: N, M
        class(*), allocatable :: mat_el

        N = size(mat, 1)
        M = size(mat, 2)

        valid_input = .TRUE.

        do jj=1,M
            do ii=1,N
                mat_el = mat(ii,jj)
                select type(mat_el)
                    type is (character(*))
                        valid_input = .FALSE.
                        exit
                    type is (logical)
                        valid_input = .FALSE.
                        exit
                end select
            end do
        end do

    end function input_control

    ! verifies correct matrices dimensions for matrix product
    function can_multiply(mat1, mat2) result(are_multipliable)
        real(8), dimension(:,:) :: mat1, mat2
        logical :: are_multipliable
        integer(4) :: M1, N2

        M1 = size(mat1, 2)
        N2 = size(mat2, 1)

        if (M1.EQ.N2) then
            are_multipliable = .TRUE.
        else
            are_multipliable = .FALSE.
        end if

    end function can_multiply

    ! POST-CONDITIONS

    ! verifies correct dimension after matrix multiplication
    function dim_control(mat1, mat2) result(equal_dim)
        real(8), dimension(:,:) :: mat1, mat2
        logical :: equal_dim
        integer(4) :: N1, M1, N2, M2

        N1 = size(mat1, 1)
        M1 = size(mat1, 2)
        N2 = size(mat2, 1)
        M2 = size(mat2, 2)

        if ((N1.EQ.N2).AND.(M1.EQ.M2)) then
            equal_dim = .TRUE.
        else
            equal_dim = .FALSE.
        end if

    end function dim_control

    ! verifies that matrices are equal up to a given precision
    function precision_control(mat1, mat2) result(equal)
        real(8), dimension(:,:) :: mat1, mat2
        logical :: equal
        integer(4) :: ii, jj
        integer(4) :: N1, M1
        real(8) :: diff, norm

        N1 = size(mat1, 1)
        M1 = size(mat1, 2)

        equal = .TRUE.

        if (dim_control(mat1,mat2)) then
            do jj=1,M1
                do ii=1,N1
                    diff = ABS(mat1(ii,jj) - mat2(ii,jj))
                    norm = MIN(ABS(mat1(ii,jj)) + ABS(mat2(ii,jj)), HUGE(1.d0))
                    ! for a complete explaination see https://bit.ly/3ooSKPp
                    if ((mat1(ii,jj).EQ.mat2(ii,jj)).OR.(diff.LT.MAX(TINY(1.d0), 128*EPSILON(1.d0)*norm))) then
                        continue
                    else
                        equal = .FALSE.
                        exit
                    end if
                end do
            end do
        else
            equal = .FALSE.
        end if
    end function precision_control
    
end module debug_matmul