! Check that global variables with allocatable components
! are properly handled.
!
! PR fortran/128969
!
!
! Test case from https://github.com/sbryngelson/offtarget
!
! MIT License
! Copyright (c) 2026 Spencer Bryngelson
!

module dt_m
  implicit none
  type :: holder
    integer, allocatable :: v1(:)
  end type holder
  type(holder) :: gq
  !$omp declare target(gq)
end module dt_m

program declared_alloc_component
  use omp_lib, only: omp_is_initial_device
  use dt_m
  implicit none
  integer :: out(64), i, host_flag
  allocate(gq%v1(64))
  gq%v1 = [(3*i, i = 1, 64)]

  out = 0
  host_flag = 1
  !$omp target teams distribute parallel do map(tofrom: out, host_flag) map(always, tofrom: gq)
  do i = 1, 64
    if (i == 1) host_flag = merge(1, 0, omp_is_initial_device())
    out(i) = gq%v1(i) + 1
  end do

  ! print '(a,*(i0,","))', 'out= ', out
  ! print '(a,i0)', 'sum= ', sum(out)
   print '(a,i0,a,a)', 'RESULT ran_on_host=', host_flag, ' check=', merge('pass', 'fail', sum(out) == 3*64*65/2 + 64)

  if (sum(out) /= 3*64*65/2 + 64) stop 1
end program declared_alloc_component
