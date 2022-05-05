! Test optional dummy arguments in HAS_DEVICE_ADDR.

program main
  use omp_lib
  use iso_c_binding
  implicit none

  integer, target :: x
  integer, pointer :: ptr
  integer, parameter :: N=7
  real :: y1(N), y2(N)
  integer, target :: y3(N:2*N-1)
  integer :: i

  x = 24
  ptr => x
  y1 = 42.24
  y2 = 42.24
  y3 = 42

  call optional_scalar (is_present=.false.)
  if (x /= 24) stop 1

  call optional_scalar (x, is_present=.true.)
  if (x /= 42) stop 2

  call optional_ptr (is_present=.false.)
  if (x /= 42) stop 3
  if (ptr /= 42) stop 4

  call optional_ptr (ptr, is_present=.true.)
  if (x /= 84) stop 5
  if (ptr /= 84) stop 6

  call optional_array (is_present=.false.)
  if (any (y1 /= [(42.24, i=1, N)])) stop 7
  if (any (y2 /= [(42.24, i=1, N)])) stop 8
  if (any (y3 /= [(42, i=1, N)])) stop 9

  call optional_array (y1, y2, y3, is_present=.true.)
  if (any (y1 /= [(42.24+i, i=1, N)])) stop 10
  if (any (y2 /= [(42.24+2*i, i=1, N)])) stop 11
  if (any (y3 /= [(42+3*i, i=1, N)])) stop 12

contains
  subroutine optional_scalar (a, is_present)
    integer, optional :: a
    logical, value :: is_present

    !$omp target data map(a) use_device_addr(a)
      !$omp target has_device_addr(a)
        if (is_present) a = 42
      !$omp end target
    !$omp end target data
  end subroutine optional_scalar

  subroutine optional_ptr (a, is_present)
    integer, pointer, optional :: a
    logical, value :: is_present
    !$omp target data map(a) use_device_addr(a)
      !$omp target has_device_addr(a)
        if (is_present) a = 84
      !$omp end target
    !$omp end target data
  end subroutine optional_ptr

  subroutine optional_array (a, b, c, is_present)
    real, optional :: a(:), b(*)
    integer, optional, pointer, intent(in) :: c(:)
    logical, value :: is_present
    integer :: i

    !$omp target data map(a, b(:N), c) use_device_addr(a, b, c)
      !$omp target has_device_addr(a, b, c)
        if (is_present) then
          if (lbound(a,dim=1) /= 1 .or. ubound(a,dim=1) /= N) stop 21
          if (lbound(b,dim=1) /= 1) stop 22
          if (lbound(c,dim=1) /= N .or. ubound(c,dim=1) /= 2*N-1) stop 23
          if (any (a /= [(42.24, i = 1, N)])) stop 24
          if (any (b(:N) /= [(42.24, i = 1, N)])) stop 25
          if (any (c /= [(42, i = 1, N)])) stop 26
          a = [(42.24+i, i=1, N)]
          b(:N) = [(42.24+2*i, i=1, N)]
          c = [(42+3*i, i=1, N)]
        end if
      !$omp end target
    !$omp end target data
  end subroutine optional_array

end program main
