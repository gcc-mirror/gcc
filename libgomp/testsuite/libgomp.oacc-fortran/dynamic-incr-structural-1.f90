! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

program map_multi
  use openacc
  implicit none
  integer, parameter :: n = 512
  integer, allocatable :: a(:), b(:), c(:)

  allocate(a(1:n))
  allocate(b(1:n))
  allocate(c(1:n))

  !$acc data copy(a, b, c)

  ! These arrays have descriptors, so use multiple mappings.  Make sure those
  ! are matched up properly with the mappings in the enclosing data region.
  !$acc enter data copyin(a)
  !$acc enter data copyin(b)
  !$acc enter data copyin(c)

  !$acc end data

  if (.not.acc_is_present (a)) stop 1
  if (.not.acc_is_present (b)) stop 2
  if (.not.acc_is_present (c)) stop 3

  !$acc exit data delete(a)

  if (acc_is_present (a)) stop 4
  if (.not.acc_is_present (b)) stop 5
  if (.not.acc_is_present (c)) stop 6

  !$acc exit data delete(b)

  if (acc_is_present (a)) stop 7
  if (acc_is_present (b)) stop 8
  if (.not.acc_is_present (c)) stop 9

  !$acc exit data delete(c)

  if (acc_is_present (a)) stop 10
  if (acc_is_present (b)) stop 11
  if (acc_is_present (c)) stop 12

  deallocate(a)
  deallocate(b)
  deallocate(c)
end program map_multi
