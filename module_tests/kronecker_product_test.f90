program kronecker_product_test
! Using Statement
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: kronecker_product
use mod_utilities, only: matrix_print
! Implicit Statement
implicit none
! Declaration Statement
real(kind=real_kind), parameter :: a(2, 2) = reshape( (/1, 2, 3, 4/), (/2, 2/), order=(/2, 1/)) 
real(kind=real_kind), parameter :: b(2, 2) = reshape( (/0, 5, 6, 7/), (/2, 2/), order=(/2, 1/)) 
real(kind=real_kind), parameter :: c(2, 2) = reshape( (/5, 7, 2, 6/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind) :: skron(8, 8), kron(4, 4), kron2(4, 4), tmp(2, 2), k = 1.25
real(kind=real_kind) :: mtmp1(4, 4), mtmp2(4, 4), mtmp3(4, 4), smtmp(8, 8)
integer(kind=int_kind) :: i
  call kronecker_product(a, b, kron)
  write(*,*) 'KRONECKER PRODUCT TEST'
  write(*,*)
  write(*,*) 'a = '
  call matrix_print(a)
  write(*,*)
  write(*,*) 'b = '
  call matrix_print(b)
  write(*,*)
  write(*,*) 'c = '
  call matrix_print(c)
  write(*,*)
  write(*,*) 'k = ', k
  write(*,*)
  write(*,*) 'a kronecker_product b = '
  call matrix_print(kron)
  write(*,*)
  write(*,*) 'a kron (a + b) = a kron b + a kron c'
  write(*,*) 'a kron (b + c) = '
  tmp = b + c
  call kronecker_product(a, tmp, kron)
  call matrix_print(kron)
  mtmp1 = kron
  write(*,*) 'a kron b + a kron c = '
  call kronecker_product(a, b, kron)
  call kronecker_product(a, c, kron2)
  kron = kron + kron2
  mtmp2 = kron
  call matrix_print(kron)
  write(*,*) mtmp1 == mtmp2
  write(*,*)
  write(*,*) '(a + b) kron c = a kron c + b kron c'
  write(*,*) '(a + b) kron c = '
  tmp = a + b
  call kronecker_product(tmp, c, kron)
  call matrix_print(kron)
  mtmp1 = kron
  write(*, *) 'a kron c + b kron c = '
  call kronecker_product(a, c, kron)
  call kronecker_product(b, c, kron2)
  kron = kron + kron2
  mtmp2 = kron
  call matrix_print(kron)
  write(*,*) mtmp1 == mtmp2
  write(*,*)
  write(*,*) '(k*a) kron b = a kron (k*b) = k(a kron b)'
  write(*,*) '(k*a) kron b = '
  tmp = k * a
  call kronecker_product(tmp, b, kron)
  mtmp1 = kron
  call matrix_print(kron)
  write(*,*) 'a kron (k*b) = '
  tmp = k * b
  call kronecker_product(a, tmp, kron)
  mtmp2 = kron
  call matrix_print(kron)
  write(*,*) 'k(a kron b) = '
  call kronecker_product(a, b, kron)
  kron = k * kron
  mtmp3 = kron
  call matrix_print(kron)
  write(*,*) mtmp1 == mtmp2
  write(*,*) mtmp1 == mtmp3
  write(*,*) mtmp2 == mtmp3
  write(*,*)
  write(*,*) '(a kron b) kron c = a kron ( b kron c)'
  write(*,*) '(a kron b) kron c = '
  call kronecker_product(a, b, kron)
  call kronecker_product(kron, c, skron)
  call matrix_print(skron)
  write(*,*) 'a kron (b kron c) = '
  call kronecker_product(b, c, kron)
  call kronecker_product(a, kron, smtmp)
  call matrix_print(smtmp)
  write(*,*) skron == smtmp
end program kronecker_product_test
