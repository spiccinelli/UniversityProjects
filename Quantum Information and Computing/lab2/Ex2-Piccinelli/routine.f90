! *** Module with error handling for different matrix-matrix multiplication methods ***

module routine
    use checkpoint
    use debug_matmul
    implicit none

    logical :: DEBUG_OPTION
    logical :: STOP_EXECUTION

        contains
            ! matrix-matrix multiplication by row
            function by_row(mat1,mat2) result(res)
                real(8), dimension(:,:) :: mat1, mat2
                real(8), dimension(size(mat1, 1),size(mat2, 2)) :: res
                integer(4) :: ii, jj, kk
                integer(4) :: N, M, K, L
    
                N = size(mat1, 1)
                M = size(mat1, 2)
                K = size(mat2, 1)
                L = size(mat2, 2)

                if (.NOT.input_control(mat1).AND..NOT.input_control(mat2)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Matrices contain non-numeric variables, stopping.", &
                                msg_type="Error")
                else if ((N<=0).OR.(M<=0)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Invalid dimension for matrix 1, stopping.", &
                                msg_type="Error")
                else if ((K<=0).OR.(L<=0)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Invalid dimension for matrix 2, stopping.", &
                                msg_type="Error")
                else if (.NOT.can_multiply(mat1, mat2)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Cannot multiply matrices since dimensions do not match, stopping.", &
                                msg_type="Error")
                else
                    call check(debug=DEBUG_OPTION, &
                    msg="Proceed to multiply by row...", &
                    msg_type="OK")
                end if
        
                do ii=1,N
                    do kk=1,M
                        do jj=1,L
                            res(ii,jj) = res(ii,jj) + mat1(ii,kk) * mat2(kk,jj)
                        end do
                    end do
                end do

                print *, "... done."
            end function by_row
    
            ! matrix-matrix multiplication by column
            function by_col(mat1,mat2) result(res)
                real(8), dimension(:,:) :: mat1, mat2
                real(8), dimension(size(mat1, 1),size(mat2, 2)) :: res
                integer(4) :: ii, jj, kk
                integer(4) :: N, M, K, L
    
                N = size(mat1, 1)
                M = size(mat1, 2)
                K = size(mat2, 1)
                L = size(mat2, 2)

                if (.NOT.input_control(mat1).AND..NOT.input_control(mat2)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Matrices contain non-numeric variables, stopping.", &
                                msg_type="Error")
                else if ((N<=0).OR.(M<=0)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Invalid dimension for matrix 1, stopping.", &
                                msg_type="Error")
                else if ((K<=0).OR.(L<=0)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Invalid dimension for matrix 2, stopping.", &
                                msg_type="Error")
                else if (.NOT.can_multiply(mat1, mat2)) then
                    call check(debug=DEBUG_OPTION, &
                                msg="Cannot multiply matrices since dimensions do not match, stopping.", &
                                msg_type="Error")
                else
                    call check(debug=DEBUG_OPTION, &
                    msg="Proceed to multiply by column...", &
                    msg_type="OK")
                end if
        
                do jj=1,L
                    do kk=1,M
                        do ii=1,N
                            res(ii,jj) = res(ii,jj) + mat1(ii,kk)*mat2(kk,jj)
                        end do      
                    end do
                end do

                print *, "... done."
            end function by_col

end module routine