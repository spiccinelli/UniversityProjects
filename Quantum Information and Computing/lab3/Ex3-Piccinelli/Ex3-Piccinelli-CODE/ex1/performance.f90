! *** Program: performance of different matrix-matrix multiplication methods ***

program performance
    use routine
    implicit none
    
    real(8), dimension (:,:), allocatable :: mat1, mat2, res1, res2, res3
    integer(4) :: N
    integer(2) :: mall_m1, mall_m2, mall_r1, mall_r2, mall_r3
    real(8) :: start, finish

    ! get command line arguments: https://bit.ly/3DfCruG
    character(:), allocatable :: argument
    integer(4) :: arglen
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: argument)
    call GET_COMMAND_ARGUMENT(1, value=argument)

    ! convert command line argument to integer: matrix size (N)
    read(argument(:),'(i5)') N

    if (N>5000) then
        call check(debug=DEBUG_OPTION, &
                    msg="Matrix 1 size too big, things might get slow.", &
                    msg_type="Warning")
    end if

    ! enable debug
    DEBUG_OPTION = .TRUE.

    allocate(mat1(N,N), stat=mall_m1)
    allocate(mat2(N,N), stat=mall_m2)
    allocate(res1(N,N), stat=mall_r1)
    allocate(res2(N,N), stat=mall_r2)
    allocate(res3(N,N), stat=mall_r3)

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

    ! results are printed on screen
    call CPU_TIME(start)
    res1 = by_row(mat1, mat2)
    call CPU_TIME(finish)
    print *, finish-start

    call CPU_TIME(start)
    res2 = by_col(mat1, mat2)
    call CPU_TIME(finish)
    print *, finish-start

    call CPU_TIME(start)
    res3 = MATMUL(mat1, mat2)
    call CPU_TIME(finish)
    print *, finish-start

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