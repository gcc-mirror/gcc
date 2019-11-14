! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as } */

module target_test
  implicit none (type, external)
  integer, parameter :: N = 40
  integer :: sum
  integer :: var1 = 1
  integer :: var2 = 2

  !$omp declare target to(D)
  integer :: D(N) = 0
contains
  subroutine enter_data (X)
    integer :: X(:)
    !$omp target enter data map(to: var1, var2, X) map(alloc: sum)
  end subroutine enter_data

  subroutine exit_data_0 (D)
    integer :: D(N)
    !$omp target exit data map(delete: D)
  end subroutine exit_data_0

  subroutine exit_data_1 ()
    !$omp target exit data map(from: var1)
  end subroutine exit_data_1

  subroutine exit_data_2 (X)
    integer :: X(N)
    !$omp target exit data map(from: var2) map(release: X, sum)
  end subroutine exit_data_2

  subroutine exit_data_3 (p, idx)
    integer :: p(:)
    integer, value :: idx
    !$omp target exit data map(from: p(idx))
  end subroutine exit_data_3

  subroutine test_nested ()
    integer :: X, Y, Z
    X = 0
    Y = 0
    Z = 0

    !$omp target data map(from: X, Y, Z)
      !$omp target data map(from: X, Y, Z)
        !$omp target map(from: X, Y, Z)
          X = 1337
          Y = 1337
          Z = 1337
        !$omp end target
        if (X /= 0) stop 11
        if (Y /= 0) stop 12
        if (Z /= 0) stop 13

        !$omp target exit data map(from: X) map(release: Y)
        if (X /= 0) stop 14
        if (Y /= 0) stop 15

        !$omp target exit data map(release: Y) map(delete: Z)
        if (Y /= 0) stop 16
        if (Z /= 0) stop 17
      !$omp end target data
      if (X /= 1337) stop 18
      if (Y /= 0) stop 19
      if (Z /= 0) stop 20

      !$omp target map(from: X)
        X = 2448
      !$omp end target
      if (X /= 2448) stop 21
      if (Y /= 0) stop 22
      if (Z /= 0) stop 23

      X = 4896
    !$omp end target data
    if (X /= 4896) stop 24
    if (Y /= 0) stop 25
    if (Z /= 0) stop 26
  end subroutine test_nested
end module target_test

program main
  use target_test
  implicit none (type, external)

  integer, allocatable :: X(:)
  integer, pointer, contiguous :: Y(:)


  allocate(X(N), Y(N))
  X(10) = 10
  Y(20) = 20
  call enter_data (X)

  call exit_data_0 (D)  ! This should have no effect on D.

  !$omp target map(alloc: var1, var2, X) map(to: Y) map(always, from: sum)
    var1 = var1 + X(10)
    var2 = var2 + Y(20)
    sum = var1 + var2
    D(sum) = D(sum) + 1
  !$omp end target

  if (var1 /= 1) stop 1
  if (var2 /= 2) stop 2
  if (sum /= 33) stop 3

  call exit_data_1 ()
  if (var1 /= 11) stop 4
  if (var2 /= 2) stop 5

  ! Increase refcount of already mapped X(1:N).
  !$omp target enter data map(alloc: X(16:17))

  call exit_data_2 (X)
  if (var2 /= 22) stop 6

  call exit_data_3 (X, 5) ! Unmap X(1:N).

  deallocate (X, Y)

  call test_nested ()
end program main
