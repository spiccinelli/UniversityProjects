! *** Program: performance of different matrix-matrix multiplication methods ***

program performance
    use routine
    implicit none
    
    real(8), dimension (:,:), allocatable :: mat1, mat2, res1, res2, res3
    integer(4) :: N, M, L, K
    integer(2) :: mall_m1, mall_m2, mall_r1, mall_r2, mall_r3
    real(8) :: start, finish

    ! get command line arguments: https://bit.ly/3DfCruG
    character(:), allocatable :: arg
    integer(4) :: arglen
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: arg)
    call GET_COMMAND_ARGUMENT(1, value=arg)

    ! switch on debug flag if specified during execution
    if (arg == "-debug") then
        DEBUG_OPTION = .TRUE.
    else
        DEBUG_OPTION = .FALSE.
    end if

    print *, "*******************************************************"
    print *, "Performance of matrix-matrix multiplication methods"
    print *, "Matrices are multiplied by ROW, COL and using MATMUL"
    print *, "To enable debug, run the [./*.out -debug] flag"
    print *, "*******************************************************"
    print *, NEW_LINE('A')
    print *, "Insert the dimensions of the matrices to multiply:"
    print *, "Insert matrix 1 dimensions:"
    read(*,*) N, M

    if ((N>1000).OR.(M>1000)) then
        call check(debug=DEBUG_OPTION, &
                    msg="Matrix 1 size too big, things might get slow.", &
                    msg_type="Warning")
    end if

    print *, "Insert matrix 2 dimensions:"
    read(*,*) L, K

    if ((L>1000).OR.(K>1000)) then
        call check(debug=DEBUG_OPTION, &
                    msg="Matrix 2 size too big, things might get slow.", &
                    msg_type="Warning")
    end if

    allocate(mat1(N,M), stat=mall_m1)
    allocate(mat2(L,K), stat=mall_m2)
    allocate(res1(N,K), stat=mall_r1)
    allocate(res2(N,K), stat=mall_r2)
    allocate(res3(N,K), stat=mall_r3)

    ! verifies correct matrix allocation
    if (mall_m1.NE.0.OR.mall_m2.NE.0.OR.mall_r1.NE.0.OR.&
        mall_r2.NE.0.OR.mall_r3.NE.0) then
        call check(debug=DEBUG_OPTION, &
            msg="Matrices not correcly allocated, stopping.", &
            msg_type="Error")
    end if

    ! initialize matrices
    call RANDOM_NUMBER(mat1)
    call RANDOM_NUMBER(mat2)
    res1 = 0.0
    res2 = 0.0
    res3 = 0.0

    ! results are written on file with columns | dim 1 | dim 2 | time |
    print *, "*******************************************************"
    call CPU_TIME(start)
    res1 = by_row(mat1, mat2)
    call CPU_TIME(finish)
    open(unit=10, file="mat_by_row.dat", action="write", position="append")
    write(10, *) N, K, finish-start
    close(10)
    print *, "By ROW: ", finish-start

    print *, "*******************************************************"
    call CPU_TIME(start)
    res2 = by_col(mat1, mat2)
    call CPU_TIME(finish)
    open(unit=10, file="mat_by_col.dat", action="write", position="append")
    write(10, *) N, K, finish-start
    close(10)
    print *, "By COL: ", finish-start

    print *, "*******************************************************"
    call CPU_TIME(start)
    res3 = MATMUL(mat1, mat2)
    call CPU_TIME(finish)
    open(unit=10, file="mat_MATMUL.dat", action="write", position="append")
    write(10, *) N, K, finish-start
    close(10)
    print *, "MATMUL: ", finish-start
    print *, "*******************************************************"

    ! post-conditions: check if results from ROW/COL/MATMUL ops are equal
    if (.NOT.precision_control(res1, res2)) then
        call check(debug=DEBUG_OPTION, &
                    msg="ROW and COL produce different results!", &
                    msg_type="Warning")
    end if

    if (.NOT.precision_control(res2, res3)) then
        call check(debug=DEBUG_OPTION, &
                    msg="COL and MATMUL produce different results!", &
                    msg_type="Warning")
    end if

    if (.NOT.precision_control(res1, res3)) then
        call check(debug=DEBUG_OPTION, &
                    msg="ROW and MATMUL produce different results!", &
                    msg_type="Warning")
    end if

    deallocate(mat1)
    deallocate(mat2)
    deallocate(res1)
    deallocate(res2)
    deallocate(res3)

end program performance