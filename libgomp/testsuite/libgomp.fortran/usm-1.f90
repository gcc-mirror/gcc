! { dg-do run }
! { dg-require-effective-target omp_usm }

! Ensure that USM works for implicit mappings.
! This needs to cover both the initial mapping scan and the rescan that
! happens when some of the mappings aren't no-ops (in this cases there are
! some hidden pointers).

program usm
  use iso_fortran_env
  use omp_lib
  implicit none

  !$omp requires unified_shared_memory

  integer, parameter :: N = 1024
  real(real64), allocatable :: x(:), y(:)
  integer :: i

  allocate(x(N), y(N))
  !$omp target teams distribute parallel do simd
  do i=1,N
    y(i) = x(i)
  enddo

  deallocate(x,y)

end program usm
