! Test 'allocatable' with OpenACC data clauses.

! See also '../libgomp.fortran/target-allocatable-1-1.f90'.

! { dg-do run }
! { dg-additional-options "-cpp" }

program main
  implicit none
  integer, allocatable :: a, b, c, d, e

  allocate (a)
  a = 11

  b = 25 ! Implicit allocation.

  c = 52 ! Implicit allocation.

  !No 'allocate (d)' here.

  !No 'allocate (e)' here.

  !$acc parallel copyin(a) copy(b, c, d) copyout(e)

  if (.not. allocated (a)) stop 1
  if (a .ne. 11) stop 2
  a = 33

  if (.not. allocated (b)) stop 3
  if (b .ne. 25) stop 4

  if (.not. allocated (c)) stop 5
  if (c .ne. 52) stop 6
  c = 10

  if (allocated (d)) stop 7
  d = 42 ! Implicit allocation, but on device only.
  if (.not. allocated (d)) stop 8
  deallocate (d) ! OpenMP requires must be "unallocated upon exit from the region".

  if (allocated (e)) stop 9
  e = 24 ! Implicit allocation, but on device only.
  if (.not. allocated (e)) stop 10
  deallocate (e) ! OpenMP requires must be "unallocated upon exit from the region".

  !$acc end parallel

  if (.not. allocated (a)) stop 20
#if ACC_MEM_SHARED
  if (a .ne. 33) stop 21
#else
  if (a .ne. 11) stop 22
#endif
  deallocate (a)

  if (.not. allocated (b)) stop 23
  if (b .ne. 25) stop 24
  deallocate (b)

  if (.not. allocated (c)) stop 25
  if (c .ne. 10) stop 26
  deallocate (c)

  if (allocated (d)) stop 27

  if (allocated (e)) stop 28

end program main
