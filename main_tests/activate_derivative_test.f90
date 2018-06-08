program activate_derivative_test
! Use statement
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: fill_matrix_rand
use mod_utilities, only: matrix_print
! Implicit Statements
implicit none
! Declaration Statements
external activate_derivative
real(kind=real_kind) :: x(3,3), y(3,3)
! Implementation
  call fill_matrix_rand(x, real(1))
  write(*,*) "ACTIVATE_DERIVATIVE_TEST"
  write(*,*) " x = "
  call matrix_print(x)
  write(*,*) " activate_derivative(a) = "
  call activate_derivative(x, y, (/3, 3/))
  call matrix_print(y)
end program activate_derivative_test
