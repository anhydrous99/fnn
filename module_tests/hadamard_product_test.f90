program hadamard_product_test
!
! Use Statements
!
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: hadamard_product
use mod_utilities, only: matrix_print
!
! Implicit Statement
!
implicit none
!
! Declaration Statements
!
real(kind=real_kind), parameter :: a(2, 2) = reshape( (/1, 2, 3, 4/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind), parameter :: b(2, 2) = reshape( (/0, 5, 6, 7/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind), parameter :: c(2, 2) = reshape( (/5, 7, 2, 6/), (/2, 2/), order=(/2, 1/))
real(kind=real_kind) :: tmp(2, 2), tmp1(2, 2), tmp2(2, 2)
!
! Implementation
!
  write(*,*) 'HADAMARD PRODUCT TEST'
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
  write(*,*) 'a had b = b had a'
  write(*,*) 'a had b = '
  call hadamard_product(a, b, tmp)
  call matrix_print(tmp)
  write(*,*) 'b had a = '
  call hadamard_product(b, a, tmp1)
  call matrix_print(tmp1)
  write(*,*) tmp == tmp1
  write(*,*)
  write(*,*) 'a had (b had c) = (a had b) had c'
  write(*,*) 'a had (b had c) = '
  call hadamard_product(b, c, tmp)
  call hadamard_product(a, tmp, tmp)
  call matrix_print(tmp)
  write(*,*)  '(a had b) had c = '
  call hadamard_product(a, b, tmp1)
  call hadamard_product(tmp1, c, tmp1)
  call matrix_print(tmp1)
  write(*,*) tmp == tmp1
  write(*,*)
  write(*,*) 'a had (b + c) = a had b + a had c'
  write(*,*) 'a had (b + c) = '
  tmp = b + c
  call hadamard_product(a, tmp, tmp)
  call matrix_print(tmp)
  write(*,*) 'a had b + a had c = '
  call hadamard_product(a, b, tmp1)
  call hadamard_product(a, c, tmp2)
  tmp1 = tmp1 + tmp2
  call matrix_print(tmp1)
  write(*,*) tmp == tmp1
end program hadamard_product_test
