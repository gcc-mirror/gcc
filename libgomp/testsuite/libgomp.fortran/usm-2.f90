! { dg-do run }
! { dg-require-effective-target omp_usm }

! Ensure that USM doesn't break the use_device_ptr clause (host pointers and
! target pointers being "unified").

program usm
  use iso_fortran_env
  use omp_lib
  implicit none

  !$omp requires unified_shared_memory

  integer, parameter :: N = 1024
  real(real64), allocatable :: x(:), y(:)
  integer :: i

  allocate(x(N),y(N))

  !$omp target data map(x)
  ! The "i" variable is not explictly mapped yet, so ensures that both
  ! mapping scan passes are tested.
  !$omp target data map(i) use_device_ptr(x)
  !$omp target teams distribute parallel do simd
  do i=1,N
    y(i) = x(i)
  enddo
  !$omp end target data 
  !$omp end target data 

  deallocate(x,y)

end program usm
