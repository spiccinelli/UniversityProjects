! *** Demo program to test complex matrix module and cmat type ***

program demo
    use complex_matrix
    implicit none

    type(cmat) :: A, adjA
    integer(2), dimension(2) :: dim
    integer(2) :: row, column

    print *, "Enter the number of rows and columns:"
    read (*,*) row, column

    dim = (/row,column/)

    print *, "Starting..."

    A = .RINIT.dim
    adjA = .ADJ.A

    print *, NEW_LINE('A'), "Writing matrix on file..."
    call cmat_write(A,.TRUE.)
    print *, "... writing successful."

    print *, NEW_LINE('A'), "Checking operations..."
    print *, "Trace: ", .TR.A
    print *, "Trace of adj: ", .TR.adjA

    print *, "Freeing memory..."
    call cmat_del(A)
    call cmat_del(adjA)

    print *, "... ending the program."
    
end program demo