! Test allocatables in HAS_DEVICE_ADDR.

program main
  use omp_lib
  use iso_c_binding
  implicit none

  integer, parameter :: N = 5
  integer, allocatable :: x
  integer, allocatable :: y(:)
  call scalar_dummy (x)
  call array_dummy (y)
  call array_dummy_optional (y)
  call array_dummy_optional ()

contains
  subroutine scalar_dummy (a)
    integer, allocatable :: a

    allocate (a)
    a = 24

    !$omp target data map(a) use_device_addr(a)
      !$omp target has_device_addr(a)
        a = 42
      !$omp end target
    !$omp end target data
    if (a /= 42) stop 1

    deallocate (a)
  end subroutine scalar_dummy

  subroutine array_dummy (a)
    integer, allocatable :: a(:)
    integer :: i

    allocate (a(N))
    a = 42

    !$omp target data map(a) use_device_addr(a)
      !$omp target has_device_addr(a)
        a = [(i, i=1, N)]
      !$omp end target
    !$omp end target data
    if (any (a /= [(i, i=1, N)])) stop 2

    deallocate (a)
  end subroutine array_dummy

  subroutine array_dummy_optional (a)
    integer, optional, allocatable :: a(:)
    integer :: i

    if (present (a)) then
      allocate (a(N))
      a = 42
    end if

    !$omp target data map(a) use_device_addr(a)
      !$omp target has_device_addr(a)
        if (present (a)) a = [(i, i=1, N)]
      !$omp end target
    !$omp end target data

    if (present (a)) then
      if (any (a /= [(i, i=1, N)])) stop 2
      deallocate (a)
    end if
  end subroutine array_dummy_optional

end program main
