! Program to test the two-spin one-half system, compute its density matrix and partial trace

program test_bipartite_state
   use quantum_system
   implicit none
   
   integer(4)       :: N         ! Number of subsystems
   integer(4)       :: D         ! Local dimension
   character(len=1) :: separable ! Input variable: is the system separable?
   logical          :: isSep     ! .TRUE. if separable
   
   complex(8), dimension(:), allocatable   :: state
   complex(8), dimension(:,:), allocatable :: rho
   complex(8), dimension(:,:), allocatable :: rho_a, rho_b

   print *, "******************************************************************************************"
   print *, "Program to test the two-spin one-half system, compute its density matrix and partial trace"
   print *, "separable -> yes (no) to initialise a (non-)separable state"
   print *, "******************************************************************************************"
   print *, "Separable state? [y/n]:"
   read *, separable

   N = 2
   D = 2

   if (separable.EQ."y") then
      isSep = .TRUE.
   else
      isSep = .FALSE.
   end if

   write(*, "(a)") ''
   write(*, "(a)") "Running ..."
   write(*, "(a)") ''

   state = rand_state_init(N, D, isSep)

   write(*, "(a)") "- State coefficients:"
   call print_state(state, .TRUE.)
   write(*, "(a)") ''
   
   rho = dmat_pure_state(state)
   rho_b = partial_trace_left(rho, D)
   rho_a = partial_trace_right(rho, D)

   write(*, "(a)") "- Density matrix (rho):"
   call print_dmat(rho, .TRUE.)
   write(*, "(a)") "Tr(rho):"
   write(*, "('('sf7.4xspf7.4'i)')") compute_trace(rho)
   write(*, "(a)") "Tr(rho^2):"
   write(*, "('('sf7.4xspf7.4'i)')") compute_trace(MATMUL(rho,rho))
   write(*, "(a)") ''
   write(*, "(ai2a)") "- Reduced density matrix, right system (rho_a):"
   call print_dmat(rho_a, .TRUE.)
   write(*, "(a)") "Tr(rho_a):"
   write(*, "('('sf7.4xspf7.4'i)')") compute_trace(rho_a)
   write(*, "(a)") ''
   write(*, "(ai2a)") "- Reduced density matrix, left system (rho_b):"
   call print_dmat(rho_b, .TRUE.)
   write(*, "(a)") "Tr(rho_b):"
   write(*, "('('sf7.4xspf7.4'i)')") compute_trace(rho_b)

end program test_bipartite_state