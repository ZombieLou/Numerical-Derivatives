!-----------------------------------------------------------------------
!Module: analytic_functions
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! This module contains two subroutines; one which gives an exact answer 
!! to the function x*sinx for a given x_zero input by user (called analytic_f).
!! Another which gives an exact answer to the second derivative of x*sinx 
!! at the x_zero given by user
!! 
!!----------------------------------------------------------------------
!! Included functions:
!!
!! analytic_f(x)
!! second_derivative_f(x)
!-----------------------------------------------------------------------
module analytic_functions
use types
implicit none

! The private statement restricts every function, parameter and variable
! defined in this module to be visible only by this module
private
! Then we use the public statement to only make visible to other modules 
! the few functions or subroutines that will be used by them
public analytic_f, second_derivative_f

contains

!-----------------------------------------------------------------------
!Function: analytic_f
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! Subroutine that gives exact answer to x*sinx given user input x.
!! 
!!----------------------------------------------------------------------
!! Arguments:
!!
!! x_zero	real	point x_0 at which to evaluate f(x_0)
!-----------------------------------------------------------------------
!! Result:
!!
!! y_zero	real	x_0 sin(x_0)
!-----------------------------------------------------------------------
function analytic_f(x_zero) result(y_zero)
    implicit none
    real(dp), intent(in) :: x_zero
    real(dp) :: y_zero
    ! This one is pretty easy. The function should return
    ! x*sin(x)
    y_zero = x_zero*sin(x_zero)
    !y_zero = 0._dp
end function analytic_f

!-----------------------------------------------------------------------
!Function: second_derivative_f
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! Subroutine that computes exact answer to second derivative of x*sinx
!! given user input x. For use in euler_formulas module.
!!----------------------------------------------------------------------
!! Arguments:
!!
!! x_zero	real	point x_0 at which to evaluate f''(x_0)
!-----------------------------------------------------------------------
!! Result:
!!
!! y_zero	real	2*cos(x) - x*sinx
!-----------------------------------------------------------------------
function second_derivative_f(x_zero) result(y_zero)
    implicit none
    real(dp), intent(in) :: x_zero
    real(dp) :: y_zero
    ! You need to code the analytic expression for 
    ! the second derivative of x*sin(x) and store it in y_zero
    y_zero = 2*cos(x_zero) - x_zero*sin(x_zero)
    
end function second_derivative_f
    
end module analytic_functions
