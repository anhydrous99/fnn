program update_backpropagation_test
!
! Use Statements
!
use mod_numeric, only: int_kind, & ! kind for int vars
                       real_kind   ! kind for real vars
use mod_matrix_arithmetic, only: fill_matrix_rand
use mod_utilities, only: matrix_print
!
! Implicit Statement
!
implicit none
!
! Var Decleretion
!
external update_backpropagation
real(kind=real_kind) :: input(3,3), bias(3,3), weights(6,3)
!
! Implementation
!
  call fill_matrix_rand(input, real(1))
  call fill_matrix_rand(bias, real(1))
  call fill_matrix_rand(weights, real(1))
  write(*,*) "UPDATE_BACKPROPAGATION_TEST"
  write(*,*) " input = "
  call matrix_print(input)
  write(*,*) " bias = "
  call matrix_print(bias)
  write(*,*)
  write(*,*) " weights = "
  call matrix_print(weights)
  write(*,*)
  write(*,*) " updated weights = "
  call update_backpropagation(input, weights, 0.1, bias, (/3,3/), (/6,3/), (/3,3/))
  call matrix_print(weights)
  write(*,*)
end program update_backpropagation_test
