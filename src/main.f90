! Program: numerical_derivatives
! By: Louis Andre
!-----------------------------------------------------------------------------
!
! Calculates the second derivative of f(x) = x sin(x) using Euler's method.
! ... expand your documentation of your code. How does the program work?
!
!-----------------------------------------------------------------------------
program numerical_derivatives
use types 
use read_write

implicit none
real(dp) :: x_zero

call read_input(x_zero)
call write_derivatives(x_zero)
end program numerical_derivatives