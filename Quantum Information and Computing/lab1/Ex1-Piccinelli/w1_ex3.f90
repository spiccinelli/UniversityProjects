! w1_ex3.f90
! The program measures the performance of matrix-matrix multiplication through the Fortran Function CPUTIME

module matmul_loop
!   Matrix dimensions are N×M and M×L
    contains

    !   Matrix multiplication by row
        subroutine by_row(mat1,mat2,mat3)
            integer*4 :: ii, jj, kk
            integer*4 :: N, M, L
            real*4, dimension(:,:), intent(in) :: mat1, mat2
            real*4, dimension(size(mat1, 1),size(mat2, 2)), intent(out) :: mat3

            N = size(mat1, 1)
            M = size(mat1, 2)
            L = size(mat2, 2)
    
            do ii=1,N
                do kk=1,L
                    do jj=1,M
                        mat3(ii,jj) = mat3(ii,jj) + mat1(ii,kk) * mat2(kk,jj)
                    end do      
                end do
            end do
        end subroutine

    !   Matrix multiplication by column
        subroutine by_col(mat1,mat2,mat3)
            integer*4 :: ii, jj, kk
            integer*4 :: N, M, L
            real*4, dimension(:,:), intent(in) :: mat1, mat2
            real*4, dimension(size(mat1, 1),size(mat2, 2)), intent(out) :: mat3

            N = size(mat1, 1)
            M = size(mat1, 2)
            L = size(mat2, 2)
    
            do jj=1,M
                do kk=1,L
                    do ii=1,N
                        mat3(ii,jj) = mat3(ii,jj) + mat1(ii,kk) * mat2(kk,jj)
                    end do      
                end do
            end do
        end subroutine

end module

program performance
    use matmul_loop
    implicit none
    
    integer*2 :: dim
    real*4, dimension (:,:), allocatable :: mat1, mat2, res1, res2, res3
    real*4 :: start, finish

    open (10, file='by_row.txt')
    open (11, file='by_col.txt')
    open (12, file='matmul.txt')

    do dim=50,950,10

        allocate(mat1(dim,dim))
        allocate(mat2(dim,dim))
        allocate(res1(dim,dim))
        allocate(res2(dim,dim))
        allocate(res3(dim,dim))

        call RANDOM_NUMBER(mat1)
        call RANDOM_NUMBER(mat2)

        call CPU_TIME(start)
        call by_row(mat1, mat2, res1)
        call CPU_TIME(finish)
        write (10,*) dim, finish-start

        call CPU_TIME(start)
        call by_col(mat1, mat2, res2)
        call CPU_TIME(finish)
        write (11,*) dim, finish-start

        call CPU_TIME(start)
        res3 = MATMUL(mat1, mat2)
        call CPU_TIME(finish)
        write (12,*) dim, finish-start

        deallocate(mat1)
        deallocate(mat2)
        deallocate(res1)
        deallocate(res2)
        deallocate(res3)
    
    end do

    close (10)
    close (11)
    close (12)

end program performance