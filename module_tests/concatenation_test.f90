program concatenation_test
! Use Statement
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
use mod_matrix_arithmetic, only: horizontal_concatenation, &
                                 vertical_concatenation
use mod_utilities, only: matrix_print
! Implicit Statement
implicit none
! Declaration Statement
real(kind=real_kind), parameter :: a(3, 5) = reshape((/ 17, 24,  1,  8, 15, &
                                                        23,  5,  7, 14, 16, &
                                                         4,  6, 13, 20, 22 /), (/3, 5/), order=(/2, 1/))
real(kind=real_kind), parameter :: b(3, 3) = reshape((/ 800, 100, 600, &
                                                        300, 500, 700, &
                                                        400, 900, 200 /), (/3, 3/), order=(/2, 1/))
real(kind=real_kind), parameter :: c(2, 3) = reshape((/  10,  20,  10, &
                                                         30,  50,  80 /), (/2, 3/), order=(/2, 1/))
real(kind=real_kind) :: d(3, 8), e(5, 3)
write(*,*)
write(*,*) "Concatenation Test"
write(*,*) " Horizontal Concatenation"
write(*,*) " a = "
call matrix_print(a)
write(*,*) " b = "
call matrix_print(b)
write(*,*) " c = "
call matrix_print(c)
write(*,*)
write(*,*) " a horizontal_concatenation b = "
call horizontal_concatenation(a, b, d)
call matrix_print(d)
write(*,*)
write(*,*) " b vertical_concatenation c = "
call vertical_concatenation(b, c, e)
call matrix_print(e)
end program
