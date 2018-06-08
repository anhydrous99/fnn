subroutine feedforward(inputs, weights, bias, net, output, &
                       inputs_shape, weights_shape, bias_shape)
! Using statement
use mod_numeric, only: int_kind, & ! kind for Interger vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: matrix_product, &
                                 horizontal_concatenation
! Implicit Statement
implicit none
! External Subroutine
external activate
! Var Decleration
integer(kind=int_kind), intent(in) :: inputs_shape(2), weights_shape(2), bias_shape(2)
real(kind=real_kind), intent(in)   :: inputs(inputs_shape(1), inputs_shape(2)),    &
                                      weights(weights_shape(1), weights_shape(2)), &
                                      bias(bias_shape(1), bias_shape(2))
real(kind=real_kind), intent(out)  :: net(weights_shape(1), inputs_shape(2) + bias_shape(2)), &
                                      output(weights_shape(1), inputs_shape(2) + bias_shape(2))
real(kind=real_kind) :: tmp(weights_shape(1), inputs_shape(2) + bias_shape(2))
! Implementation
  call horizontal_concatenation(inputs, bias, tmp)
  call matrix_product(weights, tmp, net)
  call activate(net, output, (/ weights_shape(1), inputs_shape(2) + bias_shape(2) /))
end subroutine feedforward
