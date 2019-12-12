! Test 'allocatable' with OpenMP 'target' 'map' clauses, subroutine in module,
! pass by reference.

! See also '../libgomp.oacc-fortran/allocatable-1-2.f90'.

! { dg-do run }
! { dg-additional-options "-cpp" }
! { dg-additional-options "-DMEM_SHARED" { target offload_device_shared_as } }

module m
contains
  subroutine r (a, b, c, d, e)
    implicit none
    integer, allocatable :: a, b, c, d, e

    !$omp target map(to: a) map(tofrom: b, c, d) map(from: e)

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

    !$omp end target

  end subroutine r
end module m

program main
  use m
  implicit none
  integer, allocatable :: a, b, c, d, e

  allocate (a)
  a = 11

  b = 25 ! Implicit allocation.

  c = 52 ! Implicit allocation.

  !No 'allocate (d)' here.

  !No 'allocate (e)' here.

  call r(a, b, c, d, e)

  if (.not. allocated (a)) stop 20
#ifdef MEM_SHARED
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
