program matrix_product_test
! Using Statements
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: matrix_product
use mod_utilities, only: matrix_print
! Implicit statement
implicit none
! Declaration Statement
real(kind=real_kind), parameter :: a(2, 2) = reshape( (/1, 2, 3, 4/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind), parameter :: b(2, 2) = reshape( (/0, 5, 6, 7/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind), parameter :: c(2, 2) = reshape( (/5, 7, 2, 6/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind) :: tmp(2, 2), tmp1(2, 2), tmp2(2, 2), k = 1.38

  write(*,*) 'MATRIX PRODUCT TEST'
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
  write(*,*) 'ab != ba'
  write(*,*) 'ab = '
  call matrix_product(a, b, tmp)
  call matrix_print(tmp)
  write(*,*) 'ba = '
  call matrix_product(b, a, tmp1)
  call matrix_print(tmp1)
  write(*,*) tmp == tmp1
  write(*,*) 
  write(*,*) 'a(b + c) = ab + ac'
  write(*,*) 'a(b + c) = '
  tmp = b + c
  call matrix_product(a, tmp, tmp1)
  call matrix_print(tmp1)
  write(*,*) 'ab + ac = '
  call matrix_product(a, b, tmp1)
  call matrix_product(a, c, tmp2)
  tmp1 = tmp1 + tmp2
  call matrix_print(tmp1)
  write(*,*) tmp == tmp1
  write(*,*) 
  write(*,*) '(b + c)a = ba + ca'
  write(*,*) '(b + c)a = '
  tmp = b + c
  call matrix_product(tmp, a, tmp1)
  call matrix_print(tmp1)
  write(*,*) 'ba + ca = '
  call matrix_product(b, a, tmp1)
  call matrix_product(c, a, tmp2)
  tmp1 = tmp1 + tmp2
  call matrix_print(tmp1)
  write(*,*) tmp == tmp1
  write(*,*)
  write(*,*) 'k(AB) = (kA)B'
  write(*,*) 'k(AB) = '
  call matrix_product(a, b, tmp)
  tmp = k * tmp
  call matrix_print(tmp)
  write(*,*) '(kA)B = '
  tmp = k * a
  call matrix_product(tmp, b, tmp1)
  call matrix_print(tmp1)
end program matrix_product_test
