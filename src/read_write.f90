!-----------------------------------------------------------------------
!Module: read_write
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! THis module contains the read_input and write_derivatives subroutines. 
!! This module is meant to prompt the user for an input, check that it 
!! is the correct type of input, then send the x_zero input from user
!! into euler_formulas module to calculate derivative approximations, 
!! then prints the results of these calculations in the results.dat file. 
!!----------------------------------------------------------------------
!! Included subroutines:
!!
!! read_input(x_zero)
!! write_derivatives(x_zero)
!-----------------------------------------------------------------------
module read_write
use types
use analytic_functions, only : second_derivative_f
use euler_formulas, only : euler_3points, euler_5points
implicit none

! The private statement restricts every function, parameter and variable
! defined in this module to be visible only by this module
private
! Then we use the public statement to only make visible to other modules 
! the few functions or subroutines that will be used by them
public read_input, write_derivatives

contains

!-----------------------------------------------------------------------
!Subroutine: read_input
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! This is the subroutine responsibe for prompting the user for input,
!! and then verifying that this inout is the correct type. 
!! 
!!----------------------------------------------------------------------
!! Arguments:
!! x_zero  real  value at which the derivatives will be calculated
!-----------------------------------------------------------------------
subroutine read_input(x_zero)
    implicit none
    real(dp), intent(out) :: x_zero
    character(len=120) :: string
    integer :: ierror
    ! Use the 'print *, ' command to show in the screen a brief message
    ! to the user explaining the program and what is expected from the 
    ! user's input (in this case a real number)
    ! You can use 'print *, ' several times to write different
    ! lines
    print *, 'This program calculates the second derivative of x*sin(x)'
    print *, 'Please enter the value for x around which you will want the derivative.'
    print *, ''

    ! The next part I'm giving to you for free! 
    ! When reading input from a user, checks have to be made to make sure that
    ! the user provided the correct type of input. 
    ! 
    ! We enclose the input reading inside an infinite loop that can only
    ! be exited when a correct input is given.
    !
    ! Instead of trying to read a real number we read a string containing
    ! the user's input and then make checks on that string by converting 
    ! it into a real number.
    ! 
    ! The first check is to make sure that the string is not empty 
    ! (i.e. the user simply pressed the enter key)
    ! 
    ! The second check is made by using the 'read' statement to convert
    ! the string into a number, if that is not possible iostat gives an
    ! error code different from zero.
    do
        read(*,'(a)',iostat=ierror) string
        if(string.ne.'') then
            read(string,*,iostat=ierror) x_zero
            if (ierror == 0 ) exit
            print *, "'"//trim(string)//"'"//' is not a number, please provide a number'
        else
            print *, 'that was an empty input, please provide a number'
        endif
    enddo
end subroutine read_input

!-----------------------------------------------------------------------
!Subroutine: write_derivatives
!-----------------------------------------------------------------------
!! By: Louis Andre
!!
!! This module sends the x value input into the euler_formulas module for
!! varying h_step (point separations) in order to obtain second derivative 
!! approximations. This is done with a "do" loop until the h_step reached
!! its max value of 0.20. These results are printed in columns in the file
!! results.dat.
!! 
!!----------------------------------------------------------------------
!! Arguments:
!! x_zero  real  value at which the derivatives will be calculated
!-----------------------------------------------------------------------
subroutine write_derivatives(x_zero)
    implicit none
    real(dp), intent(in) :: x_zero
    ! part of your assignment is finding appropriate values
    ! for h_step, h_increment and h_max
    ! the current values will go over the loop but they're 
    ! probably not the appropriate ones to see the 
    ! accuracy of the numerical formulas as a function of h_step
    real(dp) :: h_step = 0.01_dp
    real(dp), parameter :: h_increment = 1.25_dp, h_max = 0.20_dp
    real(dp) :: d2_analytic, d2_num3, d2_num5
    character(len=*), parameter :: file_name = 'results.dat'
    integer :: unit

    d2_analytic = second_derivative_f(x_zero)

    open(newunit=unit,file=file_name)
    write(unit,'(4a28)') 'h', 'analytic', '3 point', '5 point'
    do 
        d2_num3 = euler_3points(x_zero,h_step)
        d2_num5 = euler_5points(x_zero,h_step)
        write(unit,'(4e28.16)') h_step, d2_analytic, d2_num3, d2_num5
        if(h_step.gt.h_max) exit
        ! NEVER USE GOTO TO EXIT A LOOP!!!!!
        ! use 'exit' or 'cycle' depending on what you need your 
        ! program to do
        h_step = h_step*h_increment
    enddo
    close(unit)

    print *, 'The derivatives were written in the '//file_name//' file'
end subroutine write_derivatives

end module read_write
