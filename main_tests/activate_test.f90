program activate_test
! Use Statements
use mod_numeric, only: real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: fill_matrix_rand
use mod_utilities, only: matrix_print
! Implicit Statements
implicit none
! Declaration Statements
external activate
real(kind=real_kind) :: x(3,3), y(3,3)
! Implementation
  call fill_matrix_rand(x, real(1))
  write(*,*) "ACTIVATE_TEST"
  write(*,*) " x = "
  call matrix_print(x)
  write(*,*) " activate(a) = "
  call activate(x, y, (/3, 3/))
  call matrix_print(y)
end program activate_test
