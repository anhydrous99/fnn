program fill_matrix_rand_test
!
! Use Statements
!
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: fill_matrix_rand
use mod_utilities, only: matrix_print
!
! Implicit Statements
!
implicit none
!
! Declaration Statements
!
real(kind=real_kind) :: a(2,3), b(3,3), c(3,2), d(4,4), e(5,5), f(6,6)
!
! Impementation
!
  write(*,*) 'FILL_MATRIX_RAND TEST'
  write(*,*) ' R^(2x3) a = '
  call fill_matrix_rand(a, 100E0)
  call matrix_print(a)
  write(*,*) ' R^(3x3) b = '
  call fill_matrix_rand(b, 200E0)
  call matrix_print(b)
  write(*,*) ' R^(3x2) c = '
  call fill_matrix_rand(c, 15E0)
  call matrix_print(c)
  write(*,*) ' R^(4x4) d = '
  call fill_matrix_rand(d, 15E0)
  call matrix_print(d)
  write(*,*) ' R^(5x5) e = '
  call fill_matrix_rand(e, 15E0)
  call matrix_print(e)
  write(*,*) ' R^(6x6) f = '
  call fill_matrix_rand(f, 15E0)
  call matrix_print(f)
end program fill_matrix_rand_test
