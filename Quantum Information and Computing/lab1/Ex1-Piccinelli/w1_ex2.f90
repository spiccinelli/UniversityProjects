! w1_ex2.f90
! The program explores the limits of INTEGER and REAL in Fortran

program precison

    implicit none
!   Declare integer variables
    integer*2 :: int_16_1, int_16_2, int_16_sum
    integer*4 :: int_32_1, int_32_2, int_32_sum

!   Declare real variables
    real*4 :: real_32_1, real_32_2, real_32_sum
    real*8 :: real_64_1, real_64_2, real_64_sum

!   Initialize integer variables
    int_16_1 = 200000
    int_16_2 = 1
    int_32_1 = 200000
    int_32_2 = 1

!   Integer sums
    int_16_sum = int_16_1 + int_16_2
    int_32_sum = int_32_1 + int_32_2

    print*, "INTEGER*2 sum:", int_16_sum
    print*, "INTEGER*4 sum:", int_32_sum

!   Initialize real variables
    real_32_1 = 4.0e0*atan(1.0e0) * 1.0e32
    real_32_2 = sqrt(2.0e0) * 1.0e21
    real_64_1 = 4.0d0*atan(1.0d0) * 1.0d32
    real_64_2 = sqrt(2.0d0) * 1.0d21

!   Real sums:
    real_32_sum = real_32_1 + real_32_2
    real_64_sum = real_64_1 + real_64_2

    print*, "REAL*4 sum:", real_32_sum
    print*, "REAL*8 sum:", real_64_sum

end program precison