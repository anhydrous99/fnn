subroutine activate_derivative(x, y, dimen)
! Use Statement
use mod_numeric, only: int_kind, & ! kind for Int vars
                       real_kind   ! kind for Real vars
! Implicit Statement
implicit none
intrinsic tanh
! Var Declaration
integer(kind=int_kind), intent(in) :: dimen(2)
real(kind=real_kind), intent(in) :: x(dimen(1), dimen(2))
real(kind=real_kind) :: y(dimen(1), dimen(2))
  y = (1 - tanh(x) ** 2) / 2
end subroutine
