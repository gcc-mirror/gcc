! { dg-do run }

! Comprehensive run-time test for use_device_addr
!
! Tests array with array descriptor
!
! Differs from use_device_addr-4.f90 by using a 8-byte variable (c_double)
!
! This test case assumes that a 'var' appearing in 'use_device_addr' is
! only used as 'c_loc(var)' - such that only the actual data is used/usable
! on the device - and not meta data ((dynamic) type information, 'present()'
! status, array shape).
!
! Untested in this test case are:
! - scalars
! - polymorphic variables
! - absent optional arguments
!
module target_procs
  use iso_c_binding
  implicit none (type, external)
  private
  public :: copy3_array
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

     real(c_double), target :: aa(N), bb(N)
     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     allocate(cc(N), dd(N), ee(N), ff(N))

     aa = 11.0_c_double
     bb = 22.0_c_double
     cc = 33.0_c_double
     dd = 44.0_c_double
     ee = 55.0_c_double
     ff = 66.0_c_double

     call test_dummy_callee_1(aa, bb, cc, dd, ee, ff, N)
     deallocate(ee, ff) ! pointers, only
  end subroutine test_dummy_call_1

  subroutine test_dummy_callee_1(aa, bb, cc, dd, ee, ff, N)
     real(c_double), target :: aa(:), bb(:)
     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     integer, value :: N

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     call copy3_array(c_loc(aa), c_loc(bb), N)
     !$omp end target data
     if (any(abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa))) stop 2
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 3

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     call copy3_array(c_loc(cc), c_loc(dd), N)
     !$omp end target data
     if (any(abs(cc - 33.0_c_double) > 10.0_c_double * epsilon(cc))) stop 4
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 5

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     call copy3_array(c_loc(ee), c_loc(ff), N)
     !$omp end target data
     if (any(abs(ee - 55.0_c_double) > 10.0_c_double * epsilon(ee))) stop 6
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 7
  end subroutine test_dummy_callee_1

  ! Save device ptr - and recall pointer
  subroutine test_dummy_call_2()
     integer, parameter :: N = 1000

     real(c_double), target :: aa(N), bb(N)
     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr
     real(c_double), pointer :: aptr(:), bptr(:), cptr(:), dptr(:), eptr(:), fptr(:)

     allocate(cc(N), dd(N), ee(N), ff(N))

     call test_dummy_callee_2(aa, bb, cc, dd, ee, ff, &
                               c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, &
                               aptr, bptr, cptr, dptr, eptr, fptr, &
                               N)
     deallocate(ee, ff)
  end subroutine test_dummy_call_2

  subroutine test_dummy_callee_2(aa, bb, cc, dd, ee, ff, &
                                  c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, &
                                  aptr, bptr, cptr, dptr, eptr, fptr, &
                                  N)
     real(c_double), target :: aa(:), bb(:)
     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr
     real(c_double), pointer :: aptr(:), bptr(:), cptr(:), dptr(:), eptr(:), fptr(:)

     integer, value :: N

     real(c_double) :: dummy

     aa = 111.0_c_double
     bb = 222.0_c_double
     cc = 333.0_c_double
     dd = 444.0_c_double
     ee = 555.0_c_double
     ff = 666.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_aptr, c_bptr, N)
     !$omp target update from(bb)
     if (any(abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 8
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 9

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_array(c_aptr, c_bptr, N)
     !$omp target update from(bb)
     if (any(abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 10
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 11

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_array(c_loc(aptr), c_loc(bptr), N)
     !$omp target update from(bb)
     if (any(abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 12
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 13
     !$omp end target data

     if (any(abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 14
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 15


     !$omp target data map(to:cc) map(from:dd)
     !$omp target data map(alloc:dummy) use_device_addr(cc,dd)
     c_cptr = c_loc(cc)
     c_dptr = c_loc(dd)
     cptr => cc
     dptr => dd
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_cptr, c_dptr, N)
     !$omp target update from(dd)
     if (any(abs(cc - 333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 16
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 17

     ! check c_loc ptr again after target-value modification
     cc = 3333.0_c_double
     !$omp target update to(cc)
     call copy3_array(c_cptr, c_dptr, N)
     !$omp target update from(dd)
     if (any(abs(cc - 3333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 18
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 19

     ! check Fortran pointer after target-value modification
     cc = 33333.0_c_double
     !$omp target update to(cc)
     call copy3_array(c_loc(cptr), c_loc(dptr), N)
     !$omp target update from(dd)
     if (any(abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 20
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 21
     !$omp end target data

     if (any(abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(dd))) stop 22
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(dd))) stop 23


     !$omp target data map(to:ee) map(from:ff)
     !$omp target data map(alloc:dummy) use_device_addr(ee,ff)
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_eptr, c_fptr, N)
     !$omp target update from(ff)
     if (any(abs(ee - 555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 24
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 25

     ! check c_loc ptr again after target-value modification
     ee = 5555.0_c_double
     !$omp target update to(ee)
     call copy3_array(c_eptr, c_fptr, N)
     !$omp target update from(ff)
     if (any(abs(ee - 5555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 26
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 27

     ! check Fortran pointer after target-value modification
     ee = 55555.0_c_double
     !$omp target update to(ee)
     call copy3_array(c_loc(eptr), c_loc(fptr), N)
     !$omp target update from(ff)
     if (any(abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 28
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ff))) stop 29
     !$omp end target data

     if (any(abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 30
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 31
  end subroutine test_dummy_callee_2
end module test_dummies



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

     real(c_double), target :: aa(N), bb(N)
     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     allocate(cc(N), dd(N), ee(N), ff(N))

     aa = 11.0_c_double
     bb = 22.0_c_double
     cc = 33.0_c_double
     dd = 44.0_c_double
     ee = 55.0_c_double
     ff = 66.0_c_double

     call test_dummy_opt_callee_1(aa, bb, cc, dd, ee, ff, N)
     call test_dummy_opt_callee_1_absent(N=N)
     deallocate(ee, ff) ! pointers, only
  end subroutine test_dummy_opt_call_1

  subroutine test_dummy_opt_callee_1(aa, bb, cc, dd, ee, ff, N)
     ! scalars
     real(c_double), optional, target :: aa(:), bb(:)
     real(c_double), optional, target, allocatable :: cc(:), dd(:)
     real(c_double), optional, pointer :: ee(:), ff(:)

     integer, value :: N

     ! All shall be present - and pointing to non-NULL
     if (.not.present(aa) .or. .not.present(bb)) stop 32
     if (.not.present(cc) .or. .not.present(dd)) stop 33
     if (.not.present(ee) .or. .not.present(ff)) stop 34

     if (.not.allocated(cc) .or. .not.allocated(dd)) stop 35
     if (.not.associated(ee) .or. .not.associated(ff)) stop 36

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 37
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 38
     call copy3_array(c_loc(aa), c_loc(bb), N)
     !$omp end target data
     if (any(abs(aa - 11.0_c_double) > 10.0_c_double * epsilon(aa))) stop 39
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 40

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     if (.not.present(cc) .or. .not.present(dd)) stop 41
     if (.not.allocated(cc) .or. .not.allocated(dd)) stop 42
     if (.not.c_associated(c_loc(cc)) .or. .not.c_associated(c_loc(dd))) stop 43
     call copy3_array(c_loc(cc), c_loc(dd), N)
     !$omp end target data
     if (any(abs(cc - 33.0_c_double) > 10.0_c_double * epsilon(cc))) stop 44
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 45

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     if (.not.present(ee) .or. .not.present(ff)) stop 46
     if (.not.associated(ee) .or. .not.associated(ff)) stop 47
     if (.not.c_associated(c_loc(ee)) .or. .not.c_associated(c_loc(ff))) stop 48
     call copy3_array(c_loc(ee), c_loc(ff), N)
     !$omp end target data
     if (any(abs(ee - 55.0_c_double) > 10.0_c_double * epsilon(ee))) stop 49
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 50
  end subroutine test_dummy_opt_callee_1

  subroutine test_dummy_opt_callee_1_absent(aa, bb, cc, dd, ee, ff, N)
     ! scalars
     real(c_double), optional, target :: aa(:), bb(:)
     real(c_double), optional, target, allocatable :: cc(:), dd(:)
     real(c_double), optional, pointer :: ee(:), ff(:)

     integer, value :: N

     ! All shall be absent
     if (present(aa) .or. present(bb)) stop 51
     if (present(cc) .or. present(dd)) stop 52
     if (present(ee) .or. present(ff)) stop 53

     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     if (present(aa) .or. present(bb)) stop 54
     !$omp end target data

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     if (present(cc) .or. present(dd)) stop 55
     !$omp end target data

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     if (present(ee) .or. present(ff)) stop 56
     !$omp end target data
  end subroutine test_dummy_opt_callee_1_absent

  ! Save device ptr - and recall pointer
  subroutine test_dummy_opt_call_2()
     integer, parameter :: N = 1000

     real(c_double), target :: aa(N), bb(N)
     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr
     real(c_double), pointer :: aptr(:), bptr(:), cptr(:), dptr(:), eptr(:), fptr(:)

     allocate(cc(N), dd(N), ee(N), ff(N))
     call test_dummy_opt_callee_2(aa, bb, cc, dd, ee, ff, &
                                   c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, &
                                   aptr, bptr, cptr, dptr, eptr, fptr, &
                                   N)
     deallocate(ee, ff)
  end subroutine test_dummy_opt_call_2

  subroutine test_dummy_opt_callee_2(aa, bb, cc, dd, ee, ff, &
                                      c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr, &
                                      aptr, bptr, cptr, dptr, eptr, fptr,  &
                                      N)
     ! scalars
     real(c_double), optional, target :: aa(:), bb(:)
     real(c_double), optional, target, allocatable :: cc(:), dd(:)
     real(c_double), optional, pointer :: ee(:), ff(:)

     type(c_ptr) :: c_aptr, c_bptr, c_cptr, c_dptr, c_eptr, c_fptr
     real(c_double), optional, pointer :: aptr(:), bptr(:), cptr(:), dptr(:), eptr(:), fptr(:)

     integer, value :: N

     real(c_double) :: dummy

     ! All shall be present - and pointing to non-NULL
     if (.not.present(aa) .or. .not.present(bb)) stop 57
     if (.not.present(cc) .or. .not.present(dd)) stop 58
     if (.not.present(ee) .or. .not.present(ff)) stop 59

     if (.not.allocated(cc) .or. .not.allocated(dd)) stop 60
     if (.not.associated(ee) .or. .not.associated(ff)) stop 61

     aa = 111.0_c_double
     bb = 222.0_c_double
     cc = 333.0_c_double
     dd = 444.0_c_double
     ee = 555.0_c_double
     ff = 666.0_c_double

     !$omp target data map(to:aa) map(from:bb)
     !$omp target data map(alloc:dummy) use_device_addr(aa,bb)
     if (.not.present(aa) .or. .not.present(bb)) stop 62
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 63
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     if (.not.c_associated(c_aptr) .or. .not.c_associated(c_bptr)) stop 64
     if (.not.associated(aptr) .or. .not.associated(bptr)) stop 65
     !$omp end target data

     if (.not.present(aa) .or. .not.present(bb)) stop 66
     if (.not.c_associated(c_loc(aa)) .or. .not.c_associated(c_loc(bb))) stop 67
     if (.not.c_associated(c_aptr) .or. .not.c_associated(c_bptr)) stop 68
     if (.not.associated(aptr) .or. .not.associated(bptr)) stop 69

     ! check c_loc ptr once
     call copy3_array(c_aptr, c_bptr, N)
     !$omp target update from(bb)
     if (any(abs(aa - 111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 70
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 71

     ! check c_loc ptr again after target-value modification
     aa = 1111.0_c_double
     !$omp target update to(aa)
     call copy3_array(c_aptr, c_bptr, N)
     !$omp target update from(bb)
     if (any(abs(aa - 1111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 72
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 73

     ! check Fortran pointer after target-value modification
     aa = 11111.0_c_double
     !$omp target update to(aa)
     call copy3_array(c_loc(aptr), c_loc(bptr), N)
     !$omp target update from(bb)
     if (any(abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 74
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 75
     !$omp end target data

     if (any(abs(aa - 11111.0_c_double) > 10.0_c_double * epsilon(aa))) stop 76
     if (any(abs(3.0_c_double * aa - bb) > 10.0_c_double * epsilon(aa))) stop 77

     !$omp target data map(to:cc) map(from:dd)
     !$omp target data map(alloc:dummy) use_device_addr(cc,dd)
     if (.not.present(cc) .or. .not.present(dd)) stop 78
     if (.not.allocated(cc) .or. .not.allocated(dd)) stop 79
     if (.not.c_associated(c_loc(cc)) .or. .not.c_associated(c_loc(dd))) stop 80
     c_cptr = c_loc(cc)
     c_dptr = c_loc(dd)
     cptr => cc
     dptr => dd
     if (.not.c_associated(c_cptr) .or. .not.c_associated(c_dptr)) stop 81
     if (.not.associated(cptr) .or. .not.associated(dptr)) stop 82
     !$omp end target data
     if (.not.present(cc) .or. .not.present(dd)) stop 83
     if (.not.c_associated(c_loc(cc)) .or. .not.c_associated(c_loc(dd))) stop 84
     if (.not.c_associated(c_cptr) .or. .not.c_associated(c_dptr)) stop 85
     if (.not.associated(cptr) .or. .not.associated(dptr)) stop 86

     ! check c_loc ptr once
     call copy3_array(c_cptr, c_dptr, N)
     !$omp target update from(dd)
     if (any(abs(cc - 333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 87
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 88

     ! check c_loc ptr again after target-value modification
     cc = 3333.0_c_double
     !$omp target update to(cc)
     call copy3_array(c_cptr, c_dptr, N)
     !$omp target update from(dd)
     if (any(abs(cc - 3333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 89
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 90

     ! check Fortran pointer after target-value modification
     cc = 33333.0_c_double
     !$omp target update to(cc)
     call copy3_array(c_loc(cptr), c_loc(dptr), N)
     !$omp target update from(dd)
     if (any(abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 91
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 92
     !$omp end target data

     if (any(abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(dd))) stop 93
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(dd))) stop 94


     !$omp target data map(to:ee) map(from:ff)
     !$omp target data map(alloc:dummy) use_device_addr(ee,ff)
     if (.not.present(ee) .or. .not.present(ff)) stop 95
     if (.not.associated(ee) .or. .not.associated(ff)) stop 96
     if (.not.c_associated(c_loc(ee)) .or. .not.c_associated(c_loc(ff))) stop 97
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     if (.not.c_associated(c_eptr) .or. .not.c_associated(c_fptr)) stop 98
     if (.not.associated(eptr) .or. .not.associated(fptr)) stop 99
     !$omp end target data
     if (.not.present(ee) .or. .not.present(ff)) stop 100
     if (.not.associated(ee) .or. .not.associated(ff)) stop 101
     if (.not.c_associated(c_loc(ee)) .or. .not.c_associated(c_loc(ff))) stop 102
     if (.not.c_associated(c_eptr) .or. .not.c_associated(c_fptr)) stop 103
     if (.not.associated(eptr) .or. .not.associated(fptr)) stop 104

     ! check c_loc ptr once
     call copy3_array(c_eptr, c_fptr, N)
     !$omp target update from(ff)
     if (any(abs(ee - 555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 105
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 106

     ! check c_loc ptr again after target-value modification
     ee = 5555.0_c_double
     !$omp target update to(ee)
     call copy3_array(c_eptr, c_fptr, N)
     !$omp target update from(ff)
     if (any(abs(ee - 5555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 107
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 108

     ! check Fortran pointer after target-value modification
     ee = 55555.0_c_double
     !$omp target update to(ee)
     call copy3_array(c_loc(eptr), c_loc(fptr), N)
     !$omp target update from(ff)
     if (any(abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 109
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ff))) stop 110
     !$omp end target data

     if (any(abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 111
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 112
  end subroutine test_dummy_opt_callee_2
end module test_dummies_opt



! Test nullptr
module test_nullptr
  use iso_c_binding
  implicit none (type, external)
  private
  public :: test_nullptr_1
contains
  subroutine test_nullptr_1()
     real(c_double), pointer :: aa(:), bb(:)
     real(c_double), pointer :: ee(:), ff(:)

     real(c_double), allocatable, target :: gg(:), hh(:)

     type(c_ptr) :: c_aptr, c_bptr, c_eptr, c_fptr, c_gptr, c_hptr
     real(c_double), pointer :: aptr(:), bptr(:), eptr(:), fptr(:), gptr(:), hptr(:)

     aa => null()
     bb => null()
     ee => null()
     ff => null()

     if (associated(aa) .or. associated(bb)) stop 113
     !$omp target data map(to:aa) map(from:bb) use_device_addr(aa,bb)
     if (c_associated(c_loc(aa)) .or. c_associated(c_loc(bb))) stop 114
     c_aptr = c_loc(aa)
     c_bptr = c_loc(bb)
     aptr => aa
     bptr => bb
     if (c_associated(c_aptr) .or. c_associated(c_bptr)) stop 115
     if (associated(aptr) .or. associated(bptr, bb)) stop 116
     if (associated(aa) .or. associated(bb)) stop 117
     !$omp end target data
     if (c_associated(c_aptr) .or. c_associated(c_bptr)) stop 118
     if (associated(aptr) .or. associated(bptr, bb)) stop 119
     if (associated(aa) .or. associated(bb)) stop 120

     if (allocated(gg)) stop 121
     !$omp target data map(tofrom:gg) use_device_addr(gg)
     if (c_associated(c_loc(gg))) stop 122
     c_gptr = c_loc(gg)
     gptr => gg
     if (c_associated(c_gptr)) stop 123
     if (associated(gptr)) stop 124
     if (allocated(gg)) stop 125
     !$omp end target data
     if (c_associated(c_gptr)) stop 126
     if (associated(gptr)) stop 127
     if (allocated(gg)) stop 128

     call test_dummy_opt_nullptr_callee_1(ee, ff, hh, c_eptr, c_fptr, c_hptr, eptr, fptr, hptr)
  end subroutine test_nullptr_1

  subroutine test_dummy_opt_nullptr_callee_1(ee, ff, hh, c_eptr, c_fptr, c_hptr, eptr, fptr, hptr)
     ! scalars
     real(c_double), optional, pointer :: ee(:), ff(:)
     real(c_double), optional, allocatable, target :: hh(:)

     type(c_ptr), optional :: c_eptr, c_fptr, c_hptr
     real(c_double), optional, pointer :: eptr(:), fptr(:), hptr(:)

     if (.not.present(ee) .or. .not.present(ff)) stop 129
     if (associated(ee) .or. associated(ff)) stop 130

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     if (.not.present(ee) .or. .not.present(ff)) stop 131
     if (associated(ee) .or. associated(ff)) stop 132
     if (c_associated(c_loc(ee)) .or. c_associated(c_loc(ff))) stop 133
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     if (c_associated(c_eptr) .or. c_associated(c_fptr)) stop 134
     if (associated(eptr) .or. associated(fptr)) stop 135
     !$omp end target data

     if (c_associated(c_eptr) .or. c_associated(c_fptr)) stop 136
     if (associated(eptr) .or. associated(fptr)) stop 137

     if (allocated(hh)) stop 138
     !$omp target data map(tofrom:hh) use_device_addr(hh)
     if (c_associated(c_loc(hh))) stop 139
     c_hptr = c_loc(hh)
     hptr => hh
     if (c_associated(c_hptr)) stop 140
     if (associated(hptr)) stop 141
     if (allocated(hh)) stop 142
     !$omp end target data
     if (c_associated(c_hptr)) stop 143
     if (associated(hptr)) stop 144
     if (allocated(hh)) stop 145
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

     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     allocate(cc(N), dd(N), ee(N), ff(N))

     cc = 33.0_c_double
     dd = 44.0_c_double
     ee = 55.0_c_double
     ff = 66.0_c_double

     !$omp target data map(to:cc) map(from:dd) use_device_addr(cc,dd)
     call copy3_array(c_loc(cc), c_loc(dd), N)
     !$omp end target data
     if (any(abs(cc - 33.0_c_double) > 10.0_c_double * epsilon(cc))) stop 146
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 147

     !$omp target data map(to:ee) map(from:ff) use_device_addr(ee,ff)
     call copy3_array(c_loc(ee), c_loc(ff), N)
     !$omp end target data
     if (any(abs(ee - 55.0_c_double) > 10.0_c_double * epsilon(ee))) stop 148
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 149

     deallocate(ee, ff) ! pointers, only
   end subroutine test_main_1

   ! Save device ptr - and recall pointer
   subroutine test_main_2
     integer, parameter :: N = 1000

     real(c_double), target, allocatable :: cc(:), dd(:)
     real(c_double), pointer :: ee(:), ff(:)

     real(c_double) :: dummy
     type(c_ptr) :: c_cptr, c_dptr, c_eptr, c_fptr
     real(c_double), pointer :: cptr(:), dptr(:), eptr(:), fptr(:)

     allocate(cc(N), dd(N), ee(N), ff(N))

     cc = 333.0_c_double
     dd = 444.0_c_double
     ee = 555.0_c_double
     ff = 666.0_c_double

     !$omp target data map(to:cc) map(from:dd)
     !$omp target data map(alloc:dummy) use_device_addr(cc,dd)
     c_cptr = c_loc(cc)
     c_dptr = c_loc(dd)
     cptr => cc
     dptr => dd
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_cptr, c_dptr, N)
     !$omp target update from(dd)
     if (any(abs(cc - 333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 150
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 151

     ! check c_loc ptr again after target-value modification
     cc = 3333.0_c_double
     !$omp target update to(cc)
     call copy3_array(c_cptr, c_dptr, N)
     !$omp target update from(dd)
     if (any(abs(cc - 3333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 152
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 153

     ! check Fortran pointer after target-value modification
     cc = 33333.0_c_double
     !$omp target update to(cc)
     call copy3_array(c_loc(cptr), c_loc(dptr), N)
     !$omp target update from(dd)
     if (any(abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(cc))) stop 154
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(cc))) stop 155
     !$omp end target data

     if (any(abs(cc - 33333.0_c_double) > 10.0_c_double * epsilon(dd))) stop 156
     if (any(abs(3.0_c_double * cc - dd) > 10.0_c_double * epsilon(dd))) stop 157


     !$omp target data map(to:ee) map(from:ff)
     !$omp target data map(alloc:dummy) use_device_addr(ee,ff)
     c_eptr = c_loc(ee)
     c_fptr = c_loc(ff)
     eptr => ee
     fptr => ff
     !$omp end target data

     ! check c_loc ptr once
     call copy3_array(c_eptr, c_fptr, N)
     !$omp target update from(ff)
     if (any(abs(ee - 555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 158
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 159

     ! check c_loc ptr again after target-value modification
     ee = 5555.0_c_double
     !$omp target update to(ee)
     call copy3_array(c_eptr, c_fptr, N)
     !$omp target update from(ff)
     if (any(abs(ee - 5555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 160
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 161

     ! check Fortran pointer after target-value modification
     ee = 55555.0_c_double
     !$omp target update to(ee)
     call copy3_array(c_loc(eptr), c_loc(fptr), N)
     !$omp target update from(ff)
     if (any(abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 162
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ff))) stop 163
     !$omp end target data

     if (any(abs(ee - 55555.0_c_double) > 10.0_c_double * epsilon(ee))) stop 164
     if (any(abs(3.0_c_double * ee - ff) > 10.0_c_double * epsilon(ee))) stop 165

     deallocate(ee, ff)
   end subroutine test_main_2
end module tests


program omp_device_addr
  use tests
  use test_dummies
  use test_dummies_opt
  use test_nullptr
  implicit none (type, external)

  call test_main_1()
  call test_main_2()

  call test_dummy_call_1()
  call test_dummy_call_2()

  call test_dummy_opt_call_1()
  call test_dummy_opt_call_2()

  call test_nullptr_1()
end program omp_device_addr
