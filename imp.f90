subroutine imp(path, dataset, dataset_shape)
!
! Use Statement
!
use mod_numeric, only: int_kind, & ! kind for int vars
                       real_kind   ! kind for real vars
use mod_utilities, only: matrix_print
!
! Implicit Statement
!
implicit none
!
! Var Decleration
!
character(len=*), intent(in)       :: path
integer(kind=int_kind), intent(in) :: dataset_shape(2)
real(kind=real_kind), intent(out)  :: dataset(dataset_shape(1), dataset_shape(2))
! Tmp vars
integer(kind=int_kind) :: i, j
!
! Implementation
!
  open(10, file=path)
  do i = 1, dataset_shape(1)
    read(10,*) (dataset(i, j), j = 1, dataset_shape(2))
  end do
  close(10)
end subroutine
