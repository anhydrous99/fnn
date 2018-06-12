program eval_network_test
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
! Var Decleration
!
external eval_network
real(kind=real_kind) :: regression_error, classification_error
real(kind=real_kind) :: input(3,3), weight(6,3), bias(3,3), &
                        target_output(3,3) = reshape((/1, 0, 0, &
                                                       1, 0, 0, &
                                                       1, 0, 0/), (/3,3/), order=(/2,1/)), &
                        target_class(3) = (/1, 1, 1/)
!
! Implementation
!
  write(*,*) "EVAL_NETWORK_TEST"
  call fill_matrix_rand(input, real(1))
  call fill_matrix_rand(weight, real(1))
  call fill_matrix_rand(bias, real(1))
  write(*,*) " input = "
  call matrix_print(input)
  write(*,*) " weights = "
  call matrix_print(weight)
  write(*,*) " bias = "
  call matrix_print(bias)
  write(*,*) " target_output = "
  call matrix_print(target_output)
  write(*,*) " target_class = "
  write(*,*) target_class
  call eval_network(input, weight, bias, target_output, target_class, regression_error, classification_error, &
          (/3,3/), (/6,3/), (/3,3/))
  write(*,*)
  write(*,*) " Regression_error = ", regression_error, " Classification_error = ", classification_error
  write(*,*)
end program eval_network_test
