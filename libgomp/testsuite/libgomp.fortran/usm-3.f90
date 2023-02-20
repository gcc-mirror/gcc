! { dg-do run }
! { dg-require-effective-target omp_usm }

! Ensure that derived types containing allocated values work
! with Unified Shared Memory.

program usm
!$omp requires unified_shared_memory
  use iso_fortran_env
  implicit none

  type :: struct
    real(real64), allocatable :: v(:)
  end type struct

  integer :: index
  type(struct) :: s

  real(real64), allocatable :: expected(:)

  allocate(s%v(100))
  do index = 1, size(s%v)
    s%v(index) = index
  end do
  allocate(expected, mold=s%v)
  expected = s%v - 1._real64

  !$omp target
  s%v = s%v - 1._real64
  !$omp end target

  if (any(s%v /= expected)) STOP 1
end program usm
