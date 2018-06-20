program imp_test
!
! Use Statements
!
use mod_numeric, only: real_kind ! kind for real vars
use mod_utilities, only: matrix_print
!
! Implicit Statements
!
implicit none
!
! Declaration Statements
!
external imp
real(kind=real_kind) :: mat(38, 7)
!
! Implementation
!
  write(*,*)
  write(*,*) "IMP_TEST"
  call imp("../iris_data_files/iris_test.dat", mat, (/38, 7/))
  call matrix_print(mat)
  write(*,*)
end program imp_test
