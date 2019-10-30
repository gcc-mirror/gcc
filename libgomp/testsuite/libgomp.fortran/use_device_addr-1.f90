! { dg-do run }
! Comprehensive run-time test for use_device_addr
!
! Differs from use_device_addr-2.f90 by using a 8-byte variable (c_double)
!
! This test case assumes that a 'var' appearing in 'use_device_addr' is
! only used as 'c_loc(var)' - such that only the actual data is used/usable
! on the device - and not meta data ((dynamic) type information, 'present()'
! status, array shape).
!
! Untested in this test case are:
! - arrays with array descriptor
! - polymorphic variables
! - absent optional arguments
!
module target_procs
  use iso_c_binding
  implicit none (type, external)
  private
  public :: copy3_array, copy3_scalar
contains
  subroutine copy3_array_int(from_ptr, to_ptr, N)
    !$omp declare target
    real(c_double) :: from_ptr(:)
    real(c_double) :: to_ptr(:)
    integer, value :: N
    integer :: i

    !$omp parallel do
    do i = 1, N
      to_ptr(i) = 3 * from_ptr(i)
    end do
    !$omp end parallel do
  end subroutine copy3_array_int

  subroutine copy3_scalar_int(from, to)
    !$omp declare target
    real(c_double) :: from, to

    to = 3 * from
  end subroutine copy3_scalar_int


  subroutine copy3_array(from, to, N)
    type(c_ptr), value :: from, to
    integer, value :: N
    real(c_double), pointer :: from_ptr(:), to_ptr(:)

    call c_f_pointer(from, from_ptr, shape=[N])
    call c_f_pointer(to, to_ptr, shape=[N])

    call do_offload_scalar(from_ptr,to_ptr)
  contains
    subroutine do_offload_scalar(from_r, to_r)
      real(c_double), target :: from_r(:), to_r(:)
      ! The extra function is needed as is_device_ptr
      ! requires non-value, non-pointer dummy arguments

      !$omp target is_device_ptr(from_r, to_r)
      call copy3_array_int(from_r, to_r, N)
      !$omp end target
    end subroutine do_offload_scalar
  end subroutine copy3_array

  subroutine copy3_scalar(from, to)
    type(c_ptr), value, target :: from, to
    real(c_double), pointer :: from_ptr(:), to_ptr(:)

    ! Standard-conform detour of using an array as at time of writing
    ! is_device_ptr below does not handle scalars
    call c_f_pointer(from, from_ptr, shape=[1])
    call c_f_pointer(to, to_ptr, shape=[1])

    call do_offload_scalar(from_ptr,to_ptr)
  contains
    subroutine do_offload_scalar(from_r, to_r)
      real(c_double), target :: from_r(:), to_r(:)
      ! The extra function is needed as is_device_ptr
      ! requires non-value, non-pointer dummy arguments

      !$omp target is_device_ptr(from_r, to_r)
      call copy3_scalar_int(from_r(1), to_r(1))
      !$omp end target
    end subroutine do_offload_scalar
  end subroutine copy3_scalar
end module target_procs



! Test local dummy arguments (w/o optional)
module test_dummies
  use iso_c_binding
  use target_procs
  implicit none (type, external)
  private
  public :: test_dummy_call_1, test_dummy_call_2
contains
  subroutine test_dummy_call_1()
     integer, parameter :: N = 1000

     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     allocate(cc, dd, ee, ff)

     aa = 11.0_c_double
     bb = 22.0_c_double
     cc = 33.0_c_double
     dd = 44.0_c_double
     ee = 55.0_c_double
     ff = 66.0_c_double
     gg = 77.0_c_double
     hh = 88.0_c_double

     call test_dummy_callee_1(aa, bb, cc, dd, ee, ff, gg, hh, N)
     deallocate(ee, ff) ! pointers, only
  end subroutine test_dummy_call_1

  subroutine test_dummy_callee_1(aa, bb, cc, dd, ee, ff, gg, hh, N)
     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)
     integer, value :: N

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     call copy3_scalar(c_loc(aa), c_loc(bb))
     !$omp end target data
     if (abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     call copy3_scalar(c_loc(cc), c_loc(dd))
     !$omp end target data
     if (abs(cc - 33.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     call copy3_scalar(c_loc(ee), c_loc(ff))
     !$omp end target data
     if (abs(ee - 55.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1


     !$omp target data map(to:gg) map(from:hh) use_device_addr(gg,hh)
     call copy3_array(c_loc(gg), c_loc(hh), N)
     !$omp end target data
     if (any(abs(gg - 77.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
  end subroutine test_dummy_callee_1

  ! Save device ptr - and recall pointer
  subroutine test_dummy_call_2()
     integer, parameter :: N = 1000

     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr
     real(c_double), pointer :: aptr, bptr, cptr, dptr, eptr, fptr
     real(c_double), pointer :: gptr(:), hptr(:)

     allocate(cc, dd, ee, ff)
     call test_dummy_callee_2(aa, bb, cc, dd, ee, ff, gg, hh, &
                               c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr, &
                               aptr, bptr, cptr, dptr, eptr, fptr, gptr, hptr, &
                               N)
     deallocate(ee, ff)
  end subroutine test_dummy_call_2

  subroutine test_dummy_callee_2(aa, bb, cc, dd, ee, ff, gg, hh, &
                                  c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr, &
                                  aptr, bptr, cptr, dptr, eptr, fptr, gptr, hptr, &
                                  N)
     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr
     real(c_double), pointer :: aptr, bptr, cptr, dptr, eptr, fptr
     real(c_double), pointer :: gptr(:), hptr(:)

     integer, value :: N

     real(c_double) :: dummy

     aa = 111.0_c_double
     bb = 222.0_c_double
     cc = 333.0_c_double
     dd = 444.0_c_double
     ee = 555.0_c_double
     ff = 666.0_c_double
     gg = 777.0_c_double
     hh = 888.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_loc(aptr), c_loc(bptr))
     !$omp target update from(bb)
     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
     !$omp end target data

     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1


     !$omp target data map(to:cc) map(from:dd)
     !$omp target data map(alloc:dummy) use_device_addr(cc,dd)
     c_cptr = c_loc(cc)
     c_dptr = c_loc(dd)
     cptr => cc
     dptr => dd
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_cptr, c_dptr)
     !$omp target update from(dd)
     if (abs(cc - 333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     ! check c_loc ptr again after target-value modification
     cc = 3333.0_c_double
     !$omp target update to(cc)
     call copy3_scalar(c_cptr, c_dptr)
     !$omp target update from(dd)
     if (abs(cc - 3333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     ! check Fortran pointer after target-value modification
     cc = 33333.0_c_double
     !$omp target update to(cc)
     call copy3_scalar(c_loc(cptr), c_loc(dptr))
     !$omp target update from(dd)
     if (abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1
     !$omp end target data

     if (abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(dd)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(dd)) stop 1


     !$omp target data map(to:ee) map(from:ff)
     !$omp target data map(alloc:dummy) use_device_addr(ee,ff)
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_eptr, c_fptr)
     !$omp target update from(ff)
     if (abs(ee - 555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     ! check c_loc ptr again after target-value modification
     ee = 5555.0_c_double
     !$omp target update to(ee)
     call copy3_scalar(c_eptr, c_fptr)
     !$omp target update from(ff)
     if (abs(ee - 5555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     ! check Fortran pointer after target-value modification
     ee = 55555.0_c_double
     !$omp target update to(ee)
     call copy3_scalar(c_loc(eptr), c_loc(fptr))
     !$omp target update from(ff)
     if (abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ff)) stop 1
     !$omp end target data

     if (abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1


     !$omp target data map(to:gg) map(from:hh)
     !$omp target data map(alloc:dummy) use_device_addr(gg,hh)
     c_gptr = c_loc(gg)
     c_hptr = c_loc(hh)
     gptr => gg
     hptr => hh
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_gptr, c_hptr, N)
     !$omp target update from(hh)
     if (any(abs(gg - 777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(hh))) stop 1

     ! check c_loc ptr again after target-value modification
     gg = 7777.0_c_double
     !$omp target update to(gg)
     call copy3_array(c_gptr, c_hptr, N)
     !$omp target update from(hh)
     if (any(abs(gg - 7777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1

     ! check Fortran pointer after target-value modification
     gg = 77777.0_c_double
     !$omp target update to(gg)
     call copy3_array(c_loc(gptr), c_loc(hptr), N)
     !$omp target update from(hh)
     if (any(abs(gg - 77777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
     !$omp end target data

     if (any(abs(gg - 77777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
  end subroutine test_dummy_callee_2
end module test_dummies



! Test local dummy arguments + VALUE (w/o optional)
module test_dummies_value
  use iso_c_binding
  use target_procs
  implicit none (type, external)
  private
  public :: test_dummy_val_call_1, test_dummy_val_call_2
contains
  subroutine test_dummy_val_call_1()
     ! scalars - with value, neither allocatable nor pointer no dimension permitted
     real(c_double), target :: aa, bb

     aa = 11.0_c_double
     bb = 22.0_c_double

     call test_dummy_val_callee_1(aa, bb)
  end subroutine test_dummy_val_call_1

  subroutine test_dummy_val_callee_1(aa, bb)
     ! scalars
     real(c_double), value, target :: aa, bb

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     call copy3_scalar(c_loc(aa), c_loc(bb))
     !$omp end target data
     if (abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
  end subroutine test_dummy_val_callee_1

  ! Save device ptr - and recall pointer
  subroutine test_dummy_val_call_2()
     ! scalars - with value, neither allocatable nor pointer no dimension permitted
     real(c_double), target :: aa, bb
     type(c_ptr) :: c_aptr, c_bptr
     real(c_double), pointer :: aptr, bptr

     call test_dummy_val_callee_2(aa, bb, c_aptr, c_bptr, aptr, bptr)
  end subroutine test_dummy_val_call_2

  subroutine test_dummy_val_callee_2(aa, bb, c_aptr, c_bptr, aptr, bptr)
     real(c_double), value, target :: aa, bb
     type(c_ptr), value :: c_aptr, c_bptr
     real(c_double), pointer :: aptr, bptr

     real(c_double) :: dummy

     aa = 111.0_c_double
     bb = 222.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_loc(aptr), c_loc(bptr))
     !$omp target update from(bb)
     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
     !$omp end target data

     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
  end subroutine test_dummy_val_callee_2
end module test_dummies_value



! Test local dummy arguments + OPTIONAL
! Values present and ptr associated to nonzero
module test_dummies_opt
  use iso_c_binding
  use target_procs
  implicit none (type, external)
  private
  public :: test_dummy_opt_call_1, test_dummy_opt_call_2
contains
  subroutine test_dummy_opt_call_1()
     integer, parameter :: N = 1000

     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     allocate(cc, dd, ee, ff)

     aa = 11.0_c_double
     bb = 22.0_c_double
     cc = 33.0_c_double
     dd = 44.0_c_double
     ee = 55.0_c_double
     ff = 66.0_c_double
     gg = 77.0_c_double
     hh = 88.0_c_double

     call test_dummy_opt_callee_1(aa, bb, cc, dd, ee, ff, gg, hh, N)
     deallocate(ee, ff) ! pointers, only
  end subroutine test_dummy_opt_call_1

  subroutine test_dummy_opt_callee_1(aa, bb, cc, dd, ee, ff, gg, hh, N)
     ! scalars
     real(c_double), optional, target :: aa, bb
     real(c_double), optional, target, allocatable :: cc, dd
     real(c_double), optional, pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), optional, target :: gg(N), hh(N)
     integer, value :: N

     ! All shall be present - and pointing to non-NULL
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.present(cc) .or. .not.present(dd)) stop 1
     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (.not.present(gg) .or. .not.present(hh)) stop 1

     if (.not.associated(ee) .or. .not.associated(ff)) stop 1

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 1
     call copy3_scalar(c_loc(aa), c_loc(bb))
     !$omp end target data
     if (abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     if (.not.present(cc) .or. .not.present(dd)) stop 1
     if (.not.c_associated(c_loc(cc)) .or. .not.c_associated(c_loc(dd))) stop 1
     call copy3_scalar(c_loc(cc), c_loc(dd))
     !$omp end target data
     if (abs(cc - 33.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (.not.associated(ee) .or. .not.associated(ff)) stop 1
     if (.not.c_associated(c_loc(ee)) .or. .not.c_associated(c_loc(ff))) stop 1
     call copy3_scalar(c_loc(ee), c_loc(ff))
     !$omp end target data
     if (abs(ee - 55.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     !$omp target data map(to:gg) map(from:hh) use_device_addr(gg,hh)
     if (.not.present(gg) .or. .not.present(hh)) stop 1
     if (.not.c_associated(c_loc(gg)) .or. .not.c_associated(c_loc(hh))) stop 1
     call copy3_array(c_loc(gg), c_loc(hh), N)
     !$omp end target data
     if (any(abs(gg - 77.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
  end subroutine test_dummy_opt_callee_1

  ! Save device ptr - and recall pointer
  subroutine test_dummy_opt_call_2()
     integer, parameter :: N = 1000

     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr
     real(c_double), pointer :: aptr, bptr, cptr, dptr, eptr, fptr
     real(c_double), pointer :: gptr(:), hptr(:)

     allocate(cc, dd, ee, ff)
     call test_dummy_opt_callee_2(aa, bb, cc, dd, ee, ff, gg, hh, &
                                   c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr, &
                                   aptr, bptr, cptr, dptr, eptr, fptr, gptr, hptr, &
                                   N)
     deallocate(ee, ff)
  end subroutine test_dummy_opt_call_2

  subroutine test_dummy_opt_callee_2(aa, bb, cc, dd, ee, ff, gg, hh, &
                                      c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr, &
                                      aptr, bptr, cptr, dptr, eptr, fptr, gptr, hptr, &
                                      N)
     ! scalars
     real(c_double), optional, target :: aa, bb
     real(c_double), optional, target, allocatable :: cc, dd
     real(c_double), optional, pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), optional, target :: gg(N), hh(N)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr
     real(c_double), optional, pointer :: aptr, bptr, cptr, dptr, eptr, fptr
     real(c_double), optional, pointer :: gptr(:), hptr(:)

     integer, value :: N

     real(c_double) :: dummy

     ! All shall be present - and pointing to non-NULL
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.present(cc) .or. .not.present(dd)) stop 1
     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (.not.present(gg) .or. .not.present(hh)) stop 1

     if (.not.associated(ee) .or. .not.associated(ff)) stop 1

     aa = 111.0_c_double
     bb = 222.0_c_double
     cc = 333.0_c_double
     dd = 444.0_c_double
     ee = 555.0_c_double
     ff = 666.0_c_double
     gg = 777.0_c_double
     hh = 888.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 1
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     if (.not.c_associated(c_aptr) .or. .not.c_associated(c_bptr)) stop 1
     if (.not.associated(aptr) .or. .not.associated(bptr)) stop 1
     !$omp end target data

     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 1
     if (.not.c_associated(c_aptr) .or. .not.c_associated(c_bptr)) stop 1
     if (.not.associated(aptr) .or. .not.associated(bptr)) stop 1

     ! check c_loc ptr once
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_loc(aptr), c_loc(bptr))
     !$omp target update from(bb)
     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
     !$omp end target data

     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1


     !$omp target data map(to:cc) map(from:dd)
     !$omp target data map(alloc:dummy) use_device_addr(cc,dd)
     if (.not.present(cc) .or. .not.present(dd)) stop 1
     if (.not.c_associated(c_loc(cc)) .or. .not.c_associated(c_loc(dd))) stop 1
     c_cptr = c_loc(cc)
     c_dptr = c_loc(dd)
     cptr => cc
     dptr => dd
     if (.not.c_associated(c_cptr) .or. .not.c_associated(c_dptr)) stop 1
     if (.not.associated(cptr) .or. .not.associated(dptr)) stop 1
     !$omp end target data
     if (.not.present(cc) .or. .not.present(dd)) stop 1
     if (.not.c_associated(c_loc(cc)) .or. .not.c_associated(c_loc(dd))) stop 1
     if (.not.c_associated(c_cptr) .or. .not.c_associated(c_dptr)) stop 1
     if (.not.associated(cptr) .or. .not.associated(dptr)) stop 1

     ! check c_loc ptr once
     call copy3_scalar(c_cptr, c_dptr)
     !$omp target update from(dd)
     if (abs(cc - 333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     ! check c_loc ptr again after target-value modification
     cc = 3333.0_c_double
     !$omp target update to(cc)
     call copy3_scalar(c_cptr, c_dptr)
     !$omp target update from(dd)
     if (abs(cc - 3333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     ! check Fortran pointer after target-value modification
     cc = 33333.0_c_double
     !$omp target update to(cc)
     call copy3_scalar(c_loc(cptr), c_loc(dptr))
     !$omp target update from(dd)
     if (abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1
     !$omp end target data

     if (abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(dd)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(dd)) stop 1


     !$omp target data map(to:ee) map(from:ff)
     !$omp target data map(alloc:dummy) use_device_addr(ee,ff)
     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (.not.associated(ee) .or. .not.associated(ff)) stop 1
     if (.not.c_associated(c_loc(ee)) .or. .not.c_associated(c_loc(ff))) stop 1
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     if (.not.c_associated(c_eptr) .or. .not.c_associated(c_fptr)) stop 1
     if (.not.associated(eptr) .or. .not.associated(fptr)) stop 1
     !$omp end target data
     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (.not.associated(ee) .or. .not.associated(ff)) stop 1
     if (.not.c_associated(c_loc(ee)) .or. .not.c_associated(c_loc(ff))) stop 1
     if (.not.c_associated(c_eptr) .or. .not.c_associated(c_fptr)) stop 1
     if (.not.associated(eptr) .or. .not.associated(fptr)) stop 1

     ! check c_loc ptr once
     call copy3_scalar(c_eptr, c_fptr)
     !$omp target update from(ff)
     if (abs(ee - 555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     ! check c_loc ptr again after target-value modification
     ee = 5555.0_c_double
     !$omp target update to(ee)
     call copy3_scalar(c_eptr, c_fptr)
     !$omp target update from(ff)
     if (abs(ee - 5555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     ! check Fortran pointer after target-value modification
     ee = 55555.0_c_double
     !$omp target update to(ee)
     call copy3_scalar(c_loc(eptr), c_loc(fptr))
     !$omp target update from(ff)
     if (abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ff)) stop 1
     !$omp end target data

     if (abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1


     !$omp target data map(to:gg) map(from:hh)
     !$omp target data map(alloc:dummy) use_device_addr(gg,hh)
     if (.not.present(gg) .or. .not.present(hh)) stop 1
     if (.not.c_associated(c_loc(gg)) .or. .not.c_associated(c_loc(hh))) stop 1
     c_gptr = c_loc(gg)
     c_hptr = c_loc(hh)
     gptr => gg
     hptr => hh
     if (.not.c_associated(c_gptr) .or. .not.c_associated(c_hptr)) stop 1
     if (.not.associated(gptr) .or. .not.associated(hptr)) stop 1
     !$omp end target data
     if (.not.present(gg) .or. .not.present(hh)) stop 1
     if (.not.c_associated(c_loc(gg)) .or. .not.c_associated(c_loc(hh))) stop 1
     if (.not.c_associated(c_gptr) .or. .not.c_associated(c_hptr)) stop 1
     if (.not.associated(gptr) .or. .not.associated(hptr)) stop 1

     ! check c_loc ptr once
     call copy3_array(c_gptr, c_hptr, N)
     !$omp target update from(hh)
     if (any(abs(gg - 777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(hh))) stop 1

     ! check c_loc ptr again after target-value modification
     gg = 7777.0_c_double
     !$omp target update to(gg)
     call copy3_array(c_gptr, c_hptr, N)
     !$omp target update from(hh)
     if (any(abs(gg - 7777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1

     ! check Fortran pointer after target-value modification
     gg = 77777.0_c_double
     !$omp target update to(gg)
     call copy3_array(c_loc(gptr), c_loc(hptr), N)
     !$omp target update from(hh)
     if (any(abs(gg - 77777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
     !$omp end target data

     if (any(abs(gg - 77777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
  end subroutine test_dummy_opt_callee_2
end module test_dummies_opt



! Test local dummy arguments + OPTIONAL + VALUE
! Values present
module test_dummies_opt_value
  use iso_c_binding
  use target_procs
  implicit none (type, external)
  private
  public :: test_dummy_opt_val_call_1, test_dummy_opt_val_call_2
contains
  subroutine test_dummy_opt_val_call_1()
     ! scalars - with value, neither allocatable nor pointer no dimension permitted
     real(c_double), target :: aa, bb

     aa = 11.0_c_double
     bb = 22.0_c_double

     call test_dummy_opt_val_callee_1(aa, bb)
  end subroutine test_dummy_opt_val_call_1

  subroutine test_dummy_opt_val_callee_1(aa, bb)
     ! scalars
     real(c_double), optional, value, target :: aa, bb

     if (.not.present(aa) .or. .not.present(bb)) stop 1

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 1
     call copy3_scalar(c_loc(aa), c_loc(bb))
     !$omp end target data
     if (abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
  end subroutine test_dummy_opt_val_callee_1

  ! Save device ptr - and recall pointer
  subroutine test_dummy_opt_val_call_2()
     ! scalars - with value, neither allocatable nor pointer no dimension permitted
     real(c_double), target :: aa, bb
     type(c_ptr) :: c_aptr, c_bptr
     real(c_double), pointer :: aptr, bptr

     call test_dummy_opt_val_callee_2(aa, bb, c_aptr, c_bptr, aptr, bptr)
  end subroutine test_dummy_opt_val_call_2

  subroutine test_dummy_opt_val_callee_2(aa, bb, c_aptr, c_bptr, aptr, bptr)
     real(c_double), optional, value, target :: aa, bb
     type(c_ptr), optional, value :: c_aptr, c_bptr
     real(c_double), optional, pointer :: aptr, bptr

     real(c_double) :: dummy

     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.present(c_aptr) .or. .not.present(c_bptr)) stop 1
     if (.not.present(aptr) .or. .not.present(bptr)) stop 1

     aa = 111.0_c_double
     bb = 222.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.present(c_aptr) .or. .not.present(c_bptr)) stop 1
     if (.not.present(aptr) .or. .not.present(bptr)) stop 1

     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 1
     if (.not.present(c_aptr) .or. .not.present(c_bptr)) stop 1
     if (.not.present(aptr) .or. .not.present(bptr)) stop 1

     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     if (.not.c_associated(c_aptr) .or. .not.c_associated(c_bptr)) stop 1
     if (.not.associated(aptr) .or. .not.associated(bptr)) stop 1
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_loc(aptr), c_loc(bptr))
     !$omp target update from(bb)
     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
     !$omp end target data

     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
  end subroutine test_dummy_opt_val_callee_2
end module test_dummies_opt_value



! Test nullptr
module test_nullptr
  use iso_c_binding
  implicit none (type, external)
  private
  public :: test_nullptr_1
contains
  subroutine test_nullptr_1()
     ! scalars
     real(c_double), pointer :: aa, bb
     real(c_double), pointer :: ee, ff

     type(c_ptr) :: c_aptr, c_bptr, c_eptr, c_fptr
     real(c_double), pointer :: aptr, bptr, eptr, fptr

     aa => null()
     bb => null()
     ee => null()
     ff => null()

     if (associated(aa) .or. associated(bb)) stop 1
     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     if (c_associated(c_loc(aa)) .or. c_associated(c_loc(bb))) stop 1
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     if (c_associated(c_aptr) .or. c_associated(c_bptr)) stop 1
     if (associated(aptr) .or. associated(bptr, bb)) stop 1
     !$omp end target data
     if (c_associated(c_aptr) .or. c_associated(c_bptr)) stop 1
     if (associated(aptr) .or. associated(bptr, bb)) stop 1

     call test_dummy_opt_nullptr_callee_1(ee, ff, c_eptr, c_fptr, eptr, fptr)
  end subroutine test_nullptr_1

  subroutine test_dummy_opt_nullptr_callee_1(ee, ff, c_eptr, c_fptr, eptr, fptr)
     ! scalars
     real(c_double), optional, pointer :: ee, ff

     type(c_ptr), optional :: c_eptr, c_fptr
     real(c_double), optional, pointer :: eptr, fptr

     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (associated(ee) .or. associated(ff)) stop 1

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     if (.not.present(ee) .or. .not.present(ff)) stop 1
     if (associated(ee) .or. associated(ff)) stop 1
     if (c_associated(c_loc(ee)) .or. c_associated(c_loc(ff))) stop 1
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     if (c_associated(c_eptr) .or. c_associated(c_fptr)) stop 1
     if (associated(eptr) .or. associated(fptr)) stop 1
     !$omp end target data

     if (c_associated(c_eptr) .or. c_associated(c_fptr)) stop 1
     if (associated(eptr) .or. associated(fptr)) stop 1
  end subroutine test_dummy_opt_nullptr_callee_1
end module test_nullptr



! Test local variables
module tests
  use iso_c_binding
  use target_procs
  implicit none (type, external)
  private
  public :: test_main_1, test_main_2
contains
   ! map + use_device_addr + c_loc
   subroutine test_main_1()
     integer, parameter :: N = 1000

     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     allocate(cc, dd, ee, ff)


     aa = 11.0_c_double
     bb = 22.0_c_double
     cc = 33.0_c_double
     dd = 44.0_c_double
     ee = 55.0_c_double
     ff = 66.0_c_double
     gg = 77.0_c_double
     hh = 88.0_c_double

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     call copy3_scalar(c_loc(aa), c_loc(bb))
     !$omp end target data
     if (abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     call copy3_scalar(c_loc(cc), c_loc(dd))
     !$omp end target data
     if (abs(cc - 33.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     call copy3_scalar(c_loc(ee), c_loc(ff))
     !$omp end target data
     if (abs(ee - 55.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1


     !$omp target data map(to:gg) map(from:hh) use_device_addr(gg,hh)
     call copy3_array(c_loc(gg), c_loc(hh), N)
     !$omp end target data
     if (any(abs(gg - 77.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1

     deallocate(ee, ff) ! pointers, only
   end subroutine test_main_1

   ! Save device ptr - and recall pointer
   subroutine test_main_2
     integer, parameter :: N = 1000

     ! scalars
     real(c_double), target :: aa, bb
     real(c_double), target, allocatable :: cc, dd
     real(c_double), pointer :: ee, ff

     ! non-descriptor arrays
     real(c_double), target :: gg(N), hh(N)

     real(c_double) :: dummy
     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, c_gptr, c_hptr
     real(c_double), pointer :: aptr, bptr, cptr, dptr, eptr, fptr
     real(c_double), pointer :: gptr(:), hptr(:)

     allocate(cc, dd, ee, ff)

     aa = 111.0_c_double
     bb = 222.0_c_double
     cc = 333.0_c_double
     dd = 444.0_c_double
     ee = 555.0_c_double
     ff = 666.0_c_double
     gg = 777.0_c_double
     hh = 888.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_aptr, c_bptr)
     !$omp target update from(bb)
     if (abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_scalar(c_loc(aptr), c_loc(bptr))
     !$omp target update from(bb)
     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1
     !$omp end target data

     if (abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa)) stop 1
     if (abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa)) stop 1


     !$omp target data map(to:cc) map(from:dd)
     !$omp target data map(alloc:dummy) use_device_addr(cc,dd)
     c_cptr = c_loc(cc)
     c_dptr = c_loc(dd)
     cptr => cc
     dptr => dd
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_cptr, c_dptr)
     !$omp target update from(dd)
     if (abs(cc - 333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     ! check c_loc ptr again after target-value modification
     cc = 3333.0_c_double
     !$omp target update to(cc)
     call copy3_scalar(c_cptr, c_dptr)
     !$omp target update from(dd)
     if (abs(cc - 3333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1

     ! check Fortran pointer after target-value modification
     cc = 33333.0_c_double
     !$omp target update to(cc)
     call copy3_scalar(c_loc(cptr), c_loc(dptr))
     !$omp target update from(dd)
     if (abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(cc)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc)) stop 1
     !$omp end target data

     if (abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(dd)) stop 1
     if (abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(dd)) stop 1


     !$omp target data map(to:ee) map(from:ff)
     !$omp target data map(alloc:dummy) use_device_addr(ee,ff)
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     !$omp end target data

     ! check c_loc ptr once
     call copy3_scalar(c_eptr, c_fptr)
     !$omp target update from(ff)
     if (abs(ee - 555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     ! check c_loc ptr again after target-value modification
     ee = 5555.0_c_double
     !$omp target update to(ee)
     call copy3_scalar(c_eptr, c_fptr)
     !$omp target update from(ff)
     if (abs(ee - 5555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1

     ! check Fortran pointer after target-value modification
     ee = 55555.0_c_double
     !$omp target update to(ee)
     call copy3_scalar(c_loc(eptr), c_loc(fptr))
     !$omp target update from(ff)
     if (abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ff)) stop 1
     !$omp end target data

     if (abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee)) stop 1
     if (abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee)) stop 1


     !$omp target data map(to:gg) map(from:hh)
     !$omp target data map(alloc:dummy) use_device_addr(gg,hh)
     c_gptr = c_loc(gg)
     c_hptr = c_loc(hh)
     gptr => gg
     hptr => hh
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_gptr, c_hptr, N)
     !$omp target update from(hh)
     if (any(abs(gg - 777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(hh))) stop 1

     ! check c_loc ptr again after target-value modification
     gg = 7777.0_c_double
     !$omp target update to(gg)
     call copy3_array(c_gptr, c_hptr, N)
     !$omp target update from(hh)
     if (any(abs(gg - 7777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1

     ! check Fortran pointer after target-value modification
     gg = 77777.0_c_double
     !$omp target update to(gg)
     call copy3_array(c_loc(gptr), c_loc(hptr), N)
     !$omp target update from(hh)
     if (any(abs(gg - 77777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1
     !$omp end target data

     if (any(abs(gg - 77777.0_c_double) > 10.0_c_double * epsilon(gg))) stop 1
     if (any(abs(3.0_c_double * gg - hh) > 10.0_c_double * epsilon(gg))) stop 1

     deallocate(ee, ff)
   end subroutine test_main_2
end module tests


program omp_device_addr
  use tests
  use test_dummies
  use test_dummies_value
  use test_dummies_opt
  use test_dummies_opt_value
  use test_nullptr
  implicit none (type, external)

  call test_main_1()
  call test_main_2()

  call test_dummy_call_1()
  call test_dummy_call_2()

  call test_dummy_val_call_1()
  call test_dummy_val_call_2()

  call test_dummy_opt_call_1()
  call test_dummy_opt_call_2()

  call test_dummy_opt_val_call_1()
  call test_dummy_opt_val_call_2()

  call test_nullptr_1()
end program omp_device_addr
