subroutine update_backpropagation(input, weights, target_output, learning_rate, bias, input_shape, weights_shape, bias_shape)
!
! Use Statement
!
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: hadamard_product, &
                                 kronecker_product, &
                                 horizontal_concatenation
use mod_utilities, only: matrix_print
!
! Implicit Statement
!
implicit none
!
! Var Decleration
!
intrinsic transpose
external feedforward, activate_derivative
integer(kind=int_kind), intent(in) :: input_shape(2), weights_shape(2), bias_shape(2)
real(kind=real_kind), intent(in)   :: input(input_shape(1), input_shape(2)),    &
                                      bias(bias_shape(1), bias_shape(2)),       &
                                      target_output(input_shape(1), weights_shape(2)), &
                                      learning_rate
real(kind=real_kind), intent(inout) :: weights(weights_shape(1), weights_shape(2))
! tmp
real(kind=real_kind) :: net(1, weights_shape(2)),      &
                        net_diff(1, weights_shape(2)), &
                        output(1, weights_shape(2)),   &
                        error(1, weights_shape(2)),    &
                        tmp_input(1, input_shape(2)),  &
                        inp1(1, input_shape(2) + bias_shape(2)),   &
                        tinput(input_shape(2) + bias_shape(2), 1), &
                        tmp_bias(1, bias_shape(2)),    &
                        delta(1, weights_shape(2)),    &
                        krondel(input_shape(2) + bias_shape(2), weights_shape(2))
integer(kind=int_kind) :: i
!
! ImplementationS
!
  do i = 1, input_shape(1)
    tmp_input = input(i:i, :)
    tmp_bias = bias(i:i, :)
    call feedforward(tmp_input, weights, tmp_bias, net, output, &
            (/1, input_shape(2)/), weights_shape, (/1, bias_shape(2)/))
    error = target_output(i:i,:) - output
    call activate_derivative(net, net_diff, (/1, weights_shape(2)/))
    call hadamard_product(error, net_diff, delta)
    call horizontal_concatenation(tmp_input, tmp_bias, inp1)
    tinput = transpose(inp1)
    call kronecker_product(tinput, delta, krondel)
    krondel = learning_rate * krondel
    weights = weights + krondel
  end do
end subroutine update_backpropagation
