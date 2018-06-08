program output_to_class_test
! Using Statements
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: output_to_class, &
                                 class_to_output
use mod_utilities, only: matrix_print
! Implicit Statement
implicit none
! Declaraction Statement
real(kind=real_kind), parameter :: a(10, 3) = reshape((/ 0, 0, 1, &
                                                         1, 0, 0, &
                                                         0, 1, 0, &
                                                         0, 1, 0, &
                                                         1, 0, 0, &
                                                         0, 0, 1, &
                                                         1, 0, 0, &
                                                         0, 1, 0, &
                                                         1, 0, 0, &
                                                         0, 1, 0 /), (/10, 3/), order=(/2, 1/))
real(kind=real_kind) :: b(10)
real(kind=real_kind) :: c(10, 3)
  write(*,*) "-- Output To Class Test --"
  call matrix_print(a)
  call output_to_class(a, b)
  write(*,*)
  write(*,*) b
  write(*,*)
  write(*,*) "-- Class to Output Test --"
  write(*,*)
  call class_to_output(b, c)
  call matrix_print(c)
  write (*,*) a == c
end program output_to_class_test
