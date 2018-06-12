module mod_utilities
!
! Using statement
!
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
!
! Implicit Statement
!
implicit none
!
! Public Statement
!
public
contains
  subroutine matrix_print(a)
  real(kind=real_kind), intent(in)   :: a(:, :)
  integer(kind=int_kind) :: dimen(2), i, j, x, y
  ! Implementation
    dimen = shape(a) 
    do i = 1, dimen(1)
      write(*, *) (a(i, j), j = 1, dimen(2))
    end do
  end subroutine matrix_print
end module mod_utilities
