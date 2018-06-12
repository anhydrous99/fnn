subroutine eval_network(input, weight, bias, target_output, target_class, regression_error, classification_error, &
                        input_shape, weight_shape, bias_shape)
!
! Use statements
!
use mod_numeric,           only: int_kind, & ! kind for int vars
                                 real_kind   ! kind for real vars
use mod_matrix_arithmetic, only: output_to_class
!
! Implicit Statement
!
implicit none
!
! Var Decleration
!
external feedforward
integer(kind=int_kind), intent(in), dimension(2) :: input_shape, weight_shape, bias_shape
real(kind=real_kind), intent(in)   :: input(input_shape(1), input_shape(2)),                        &
                                     weight(weight_shape(1), weight_shape(2)),                      &
                                     bias(bias_shape(1), bias_shape(2)),                            &
                                     target_output(input_shape(1), weight_shape(2)),                &
                                     target_class(input_shape(1))
real(kind=real_kind), intent(out) :: regression_error, classification_error
! Tmp Vars
real(kind=real_kind), dimension(input_shape(1), weight_shape(2)) :: net, outputs
real(kind=real_kind)   :: classi(input_shape(1))
integer(kind=int_kind) :: i
real(kind=real_kind)   :: s
!
! Implementation
!
  call feedforward(input, weight, bias, net, outputs, input_shape, weight_shape, bias_shape)
  regression_error = sum((outputs - target_output) ** 2) / (input_shape(1) * weight_shape(2))
  call output_to_class(outputs, classi)
  s = 0
  do i = 1, input_shape(1)
    if (classi(i) /= target_class(i)) s = s + 1
  end do
  classification_error = s / input_shape(1)
end subroutine eval_network
