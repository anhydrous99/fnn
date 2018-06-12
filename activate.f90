subroutine activate(x, y, dimen)
!
! Use Statement
!
use mod_numeric, only: int_kind, & ! kind for int vars
                       real_kind   ! kind for Real vars
!
! Implicit Statement
!
implicit none
!
! Instrisic statements
!
intrinsic tanh
! Var Decleration
integer(kind=int_kind), intent(in) :: dimen(2)
real(kind=real_kind), intent(in) :: x(dimen(1), dimen(2))
real(kind=real_kind), intent(out) :: y(dimen(1), dimen(2))
! Implementation
  y = (tanh(x) + 1) / 2
end subroutine activate
