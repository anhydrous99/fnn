program feedforward_test
! Use Statements
use mod_numeric, only: real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: fill_matrix_rand
use mod_utilities, only: matrix_print
! Implicit Statements
implicit none
! Declaration Statements
external feedforward
real(kind=real_kind) :: inputs(3, 3), weights(3, 3), bias(3, 3)
real(kind=real_kind) :: net(3, 6), output(3, 6)
! Implementation
  write(*,*) "FEEDFORWARD_TEST"
  write(*,*)
  call fill_matrix_rand(inputs, real(1))
  call fill_matrix_rand(weights, real(1))
  call fill_matrix_rand(bias, real(1))
  write(*,*) " inputs = "
  call matrix_print(inputs)
  write(*,*) " weights = "
  call matrix_print(weights)
  write(*,*) " bias = "
  call matrix_print(bias)
  write(*,*) " Feedforward outputs: "
  call feedforward(inputs, weights, bias, net, output, (/3, 3/), (/3, 3/), (/3, 3/))
  write(*,*) " net = "
  call matrix_print(net)
  write(*,*) " output = "
  call matrix_print(output)
end program feedforward_test
