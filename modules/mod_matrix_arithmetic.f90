module mod_matrix_arithmetic
! Using statement
use mod_numeric, only: int_kind, & ! kind for Integer vars
                       real_kind   ! kind for Real vars
! Implicit Statement
implicit none
! Public Statement
public
contains
  subroutine matrix_product(a, b, c)
  real(kind=real_kind), intent(in)   :: a(:, :), b(:, :)
  real(kind=real_kind), intent(out)  :: c(:, :)
  integer(kind=real_kind) :: n(2), m(2)
  integer(kind=int_kind) :: i, j, k
    n = shape(a)
    m = shape(b)
    do i = 1, n(1)
      do j = 1, m(2)
        c(i, j) = 0
        do k = 1, n(2)
          c(i, j) = c(i, j) + a(i, k) * b(k, j)
        end do
      end do
    end do
  end subroutine matrix_product

  subroutine kronecker_product(a, b, c)
  real(kind=real_kind),   intent(in)  :: a(:, :), b(:, :)
  real(kind=real_kind),   intent(out) :: c(:, :)
  integer(kind=int_kind) :: n(2), m(2)
  integer(kind=int_kind) :: i, j, x1, y1, x2, y2
    n = shape(a)
    m = shape(b)
    do i = 1, n(1) * m(1)
      do j = 1, n(2) * m(2)
        x1 = floor(real(i - 1) / real(m(1))) + 1
        y1 = floor(real(j - 1) / real(m(2))) + 1
        x2 = mod(real(i - 1), real(m(1))) + 1
        y2 = mod(real(j - 1), real(m(2))) + 1
        c(i, j) = a(x1, y1) * b(x2, y2)
      end do
    end do
  end subroutine kronecker_product

  subroutine hadamard_product(a, b, c)
  real(kind=real_kind), intent(in)    :: a(:, :), b(:, :)
  real(kind=real_kind), intent(out)   :: c(:, :)
  integer(kind=int_kind) :: i, j, n(2)
    n = shape(a)
    do i = 1, n(1)
      do j = 1, n(2)
        c(i, j) = a(i, j) * b(i, j)
      end do
    end do
  end subroutine hadamard_product

  subroutine horizontal_concatenation(a, b, c)
  real(kind=real_kind), intent(in)   :: a(:, :), b(:, :)
  real(kind=real_kind), intent(out)  :: c(:, :)
  integer(kind=int_kind) :: i, j, n(2), m(2)
    n = shape(a)
    m = shape(b)
    do i = 1, n(1)
      do j = 1, n(2) + m(2)
        if (j .lt. n(2) + 1) then
          c(i, j) = a(i, j)
        else
          c(i, j) = b(i, j - n(2))
        end if
      end do
    end do
  end subroutine horizontal_concatenation

  subroutine vertical_concatenation(a, b, c)
  real(kind=real_kind), intent(in)   :: a(:, :), b(:, :)
  real(kind=real_kind), intent(out)  :: c(:, :)
  integer(kind=int_kind) :: i, j, n(2), m(2)
    n = shape(a)
    m = shape(b)
    do i = 1, n(2)
      do j = 1, n(1) + m(1)
        if (j .lt. n(1) + 1) then
          c(j, i) = a(j, i)
        else
          c(j, i) = b(j - n(1), i)
        end if
      end do
    end do
  end subroutine vertical_concatenation

  subroutine output_to_class(a, b)
  real(kind=real_kind), intent(in)   :: a(:, :)
  real(kind=real_kind), intent(out)  :: b(:)
  integer(kind=int_kind) :: i, j, s, n(2)
  ! Implementation
    n = shape(a)
    do i = 1, n(1)
      s = 1
      do j = 1, n(2)
       if (a(i,j) .eq. 1) then
         exit
       end if
       s = s + 1
      end do
      b(i) = s
    end do
  end subroutine output_to_class
  
  subroutine class_to_output(a, b)
  real(kind=real_kind), intent(in)   :: a(:)
  real(kind=real_kind), intent(out)  :: b(:, :)
  integer(kind=int_kind) :: i, j, n(2)
  ! Implementation
    n = shape(b)
    do i = 1, n(1)
      do j = 1, n(2)
        if (j .eq. a(i)) then
          b(i, j) = 1
        else
          b(i, j) = 0
        end if
      end do
    end do
  end subroutine class_to_output

  subroutine fill_matrix_rand(a, mx)
  real(kind=real_kind), intent(inout) :: a(:,:)
  real(kind=real_kind), intent(in) :: mx
  integer(kind=int_kind) :: dimen(2), i, j
  ! Implementation
    dimen = shape(a)
    do i = 1, dimen(1)
      do j = 1, dimen(2)
        a(i,j) = (2 * rand() - 1) * mx
      end do
    end do
  end subroutine fill_matrix_rand
end module mod_matrix_arithmetic
