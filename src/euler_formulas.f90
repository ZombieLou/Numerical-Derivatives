!-----------------------------------------------------------------------
!Module: euler_formulas
!-----------------------------------------------------------------------
!! By: Louis Andre
!!This module contans two subroutines; each of them take in an x value 
!!around which we would like a second derivative of xsinx calculated, and
!!and h_step value indicating the separation between points used to estimate
!!second derivative. The euler_3points subroutine takes a three point approximation
!! to estimate the second derivative, whereas the euler_5points takes a five point
!! approximation to estimate second derivative (taking two points on each side of
!! x_zero rather than one on each side).
!!----------------------------------------------------------------------
!! Included functions:
!!
!! euler_3points(x_zero,h_step)
!! euler_5point(x_zero,h_step)
!-----------------------------------------------------------------------
module euler_formulas
use types
use analytic_functions, only : analytic_f
implicit none

! The private statement restricts every function, parameter and variable
! defined in this module to be visible only by this module
private
! Then we use the public statement to only make visible to other modules 
! the few functions or subroutines that will be used by them
public euler_3points, euler_5points

contains

!-----------------------------------------------------------------------
!Function: euler_3points
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! This is the approximation to the second derivative of x*sinx, this uses 
!! one point on either side of (x_zero, f(x_zero)) in order to calculate slope
!! around x_zero, all done with decreasing distance between points from 
!! 0.20 down to 0.01. 
!!----------------------------------------------------------------------
!! Arguments:
!!
!! x_zero   real    point x_0 at which to evaluate f''(x_0)
!! h_step   real    step size in the numerical expression
!-----------------------------------------------------------------------
!! Result:
!!
!! y_zero   real    (f(x+h)-2f(x)+f(x-h))/(h^2)
!-----------------------------------------------------------------------
function euler_3points(x_zero,h_step) result(y_zero)
    implicit none
    real(dp), intent(in) :: x_zero, h_step
    real(dp) :: y_zero
    real(dp) :: f_plus, f_zero, f_minus
    ! This evaluates the analytic function defined in the analytic_functions
    ! module at x+h, x, and x-h. Modify as you see necessary 
    f_plus = analytic_f(x_zero + h_step)
    f_zero = analytic_f(x_zero)
    f_minus = analytic_f(x_zero - h_step)

    y_zero = (f_plus - 2*f_zero + f_minus)/(h_step*h_step)

    ! Here you can use the evaluated values to calculate the numerical
    ! approximation to the second derivative
    
end function euler_3points

!-----------------------------------------------------------------------
!Function: euler_5points
!-----------------------------------------------------------------------
!! By Louis Andre
!!
!! This is the approximation to the second derivative of x*sinx, this uses
!! two points on either side of (x_zero, f(x_zero)) in order to calculate slope
!! around x_zero, all done with decreasing distance between points from 
!! 0.20 down to 0.01.
!!----------------------------------------------------------------------
!! Arguments:
!!
!! x_zero   real    point x_0 at which to evaluate f''(x_0)
!! h_step   real    step size in the numerical expression
!-----------------------------------------------------------------------
!! Result:
!!
!! y_zero   real    (-f(x+2h)+16f(x+h)-30f(x)+16f(x-h)-f(x-2h))/(12 h^2)
function euler_5points(x_zero,h_step) result(y_zero)
    implicit none
    real(dp), intent(in) :: x_zero, h_step
    real(dp) :: y_zero
    real(dp) :: f_plus, f_zero, f_minus, f_minustwo, f_plustwo

    f_plus = analytic_f(x_zero + h_step)
    f_plustwo = analytic_f(x_zero + 2*h_step)
    f_zero = analytic_f(x_zero)
    f_minus = analytic_f(x_zero - h_step)
    f_minustwo = analytic_f(x_zero - 2*h_step)

    y_zero = (-f_plustwo + 16*f_plus - 30*f_zero + 16*f_minus - f_minustwo)/(12*h_step*h_step)
    
    
end function euler_5points
    
end module euler_formulas