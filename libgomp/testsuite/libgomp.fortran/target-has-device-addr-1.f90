program main
  use omp_lib
  use iso_c_binding
  implicit none

  integer, parameter :: N = 40
  integer :: x, i
  integer :: y (N)
  integer :: u (0)

  x = 24
  !$omp target data map(x) use_device_addr(x)
    !$omp target has_device_addr(x)
      x = 42;
    !$omp end target
  !$omp end target data
  if (x /= 42) stop 1

  y = 42
  !$omp target data map(y) use_device_addr(y)
    !$omp target has_device_addr(y)
      y = [(i, i=1, N)]
    !$omp end target
  !$omp end target data
  if (any (y /= [(i, i = 1, N)])) stop 2

  !$omp target data map(y(:N)) use_device_addr(y)
    !$omp target has_device_addr(y(:N))
      y = [(i+2, i=1, N)]
    !$omp end target
  !$omp end target data
  if (any (y /= [(i+2, i = 1, N)])) stop 3

  !$omp target data map(y) use_device_addr(y)
    !$omp target has_device_addr(y(24:))
      do i = 24, N
        y(i) = i + 3
      end do
    !$omp end target
  !$omp end target data
  do i = 24, N
    if (y(i) /= i + 3) stop 5
  end do

  !$omp target data map(u) use_device_addr(u)
    !$omp target has_device_addr(u)
    !$omp end target
  !$omp end target data

end program main
