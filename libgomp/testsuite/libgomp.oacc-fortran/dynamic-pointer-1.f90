! Verify that a 'enter data'ed 'pointer' object creates a persistent, visible device copy

! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

module m
  implicit none
contains

  subroutine verify_a (a_ref, a)
    implicit none
    integer, dimension (:, :, :), allocatable :: a_ref
    integer, dimension (:, :, :), pointer :: a

    !$acc routine seq

    if (any (lbound (a) /= lbound (a_ref))) stop 101
    if (any (ubound (a) /= ubound (a_ref))) stop 102
    if (size (a) /= size (a_ref)) stop 103
  end subroutine verify_a

end module m

program main
  use m
  use openacc
  implicit none
  integer, parameter :: n = 30
  integer, dimension (:, :, :), allocatable, target :: a1, a2
  integer, dimension (:, :, :), pointer :: p

  allocate (a1(1:n, 0:n-1, 10:n/2))
  !$acc enter data create(a1)
  allocate (a2(3:n/3, 10:n, n-10:n+10))
  !$acc enter data create(a2)

  p => a1
  call verify_a(a1, p)

  ! 'p' object isn't present on the device.
  !$acc parallel ! Implicit 'copy(p)'; creates 'p' object...
  call verify_a(a1, p)
  !$acc end parallel ! ..., and deletes it again.

  p => a2
  call verify_a(a2, p)

  ! 'p' object isn't present on the device.
  !$acc parallel ! Implicit 'copy(p)'; creates 'p' object...
  call verify_a(a2, p)
  !$acc end parallel ! ..., and deletes it again.

  p => a1

  !$acc enter data create(p)
  ! 'p' object is now present on the device (visible device copy).
  !TODO PR96080 if (.not. acc_is_present (p)) stop 1

  !$acc parallel
  ! On the device, got created as 'p => a1'.
  call verify_a(a1, p)
  !$acc end parallel
  call verify_a(a1, p)

  !$acc parallel
  p => a2
  ! On the device, 'p => a2' is now set.
  call verify_a(a2, p)
  !$acc end parallel
  ! On the host, 'p => a1' persists.
  call verify_a(a1, p)

  !$acc parallel
  ! On the device, 'p => a2' persists.
  call verify_a(a2, p)
  !$acc end parallel
  ! On the host, 'p => a1' still persists.
  call verify_a(a1, p)

  p => a2

  !$acc parallel
  p => a1
  ! On the device, 'p => a1' is now set.
  call verify_a(a1, p)
  !$acc end parallel
  ! On the host, 'p => a2' persists.
  call verify_a(a2, p)

  !$acc parallel
  ! On the device, 'p => a1' persists.
  call verify_a(a1, p)
  !$acc end parallel
  ! On the host, 'p => a2' still persists.
  call verify_a(a2, p)

end program main
