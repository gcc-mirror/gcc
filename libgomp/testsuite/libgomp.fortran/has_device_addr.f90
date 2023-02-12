! { dg-additional-options "-fdump-tree-original" }

!
! PR fortran/108558
!

! { dg-final { scan-tree-dump-times "#pragma omp target has_device_addr\\(x\\) has_device_addr\\(y\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:x\\) map\\(tofrom:y\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data use_device_addr\\(x\\) use_device_addr\\(y\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target update from\\(y\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:x\\) map\\(tofrom:y\\) use_device_addr\\(x\\) use_device_addr\\(y\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp distribute" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 2 "original" } }

module m
contains
subroutine vectorAdd(x, y, N)
  implicit none
  integer :: N
  integer(4) :: x(N), y(N)
  integer :: i

  !$omp target teams distribute parallel do has_device_addr(x, y)
  do i = 1, N
    y(i) = x(i) + y(i)
  end do
end subroutine vectorAdd
end module m

program main
  use m
  implicit none
  integer, parameter :: N = 9876
  integer(4) :: x(N), y(N)
  integer :: i

  x(:) = 1
  y(:) = 2

  !$omp target data map(x, y)
    !$omp target data use_device_addr(x, y)
      call vectorAdd(x, y, N)
    !$omp end target data
    !$omp target update from(y)
    if (any (y /= 3)) error stop
  !$omp end target data

  x = 1
  y = 2
  !$omp target data map(x, y) use_device_addr(x, y)
    !$omp target teams distribute parallel do has_device_addr(x, y)
    do i = 1, N
      y(i) = x(i) + y(i)
    end do
 !$omp end target data
 if (any (y /= 3)) error stop
end program
