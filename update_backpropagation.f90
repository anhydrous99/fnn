subroutine update_backpropagation(input, weights, learning_rate, bias, input_shape, weights_shape, bias_shape)
!
! Use Statement
!
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
!
! Implicit Statement
!
implicit none
!
! Var Decleration
!
integer(kind=int_kind), intent(in) :: learning_rate, input_shape(2), weights_shape(2), bias_shape(2)
real(kind=real_kind), intent(in)   :: input(input_shape(1), input_shape(2)),    &
                                      bias(bias_shape(1), bias_shape(2))
real(kind=real_kind), intent(inout) :: weights(weights_shape(1), weights_shape(2))
!
! ImplementationS
!
  ! TODO
end subroutine update_backpropagation
