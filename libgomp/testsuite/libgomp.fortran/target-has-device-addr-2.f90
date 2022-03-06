program main
  use omp_lib
  use iso_c_binding
  implicit none

  integer, parameter :: N = 5
  integer :: i, x(N), y(N), z(N:2*N-1)
  target :: z

  x = 42
  y = 43
  z = 44

  call foo (x, y, z)
  if (any (x /= [(i, i = 1, N)])) stop 1
  if (any (y /= [(2*i, i = 1, N)])) stop 2
  if (any (z /= [(3*i, i = 1, N)])) stop 3

  contains
  subroutine foo(a, b, c)
    integer :: a(:)
    integer :: b(*)
    integer, pointer, intent(in) :: c(:)

    !$omp target data map(a,b(:N),c) use_device_addr(a,b(:N),c)
      !$omp target has_device_addr(A,B(:N),C)
        if (lbound(a,dim=1) /= 1 .or. ubound(a,dim=1) /= N) stop 10
        if (lbound(b,dim=1) /= 1) stop 11
        if (lbound(c,dim=1) /= N .or. ubound(c,dim=1) /= 2*N-1) stop 12
        if (any (a /= 42)) stop 13
        if (any (b(:N) /= 43)) stop 14
        if (any (c /= 44)) stop 15
        a = [(i, i=1, N)]
        b(:N) = [(2*i, i = 1, N)]
        c = [(3*i, i = 1, N)]
      !$omp end target
    !$omp end target data
  end subroutine foo

end program main
