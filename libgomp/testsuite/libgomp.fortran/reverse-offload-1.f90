! { dg-do run }
! { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } }
! { dg-additional-sources reverse-offload-1-aux.f90 }

! Check that reverse offload works in particular:
! - no code is generated on the device side (i.e. no
!   implicit declare target of called functions and no
!   code gen for the target-region body)
!   -> would otherwise fail due to 'add_3' symbol
! - Plus the usual (compiles, runs, produces correct result)

! Note: Running also the non-reverse-offload target regions
! on the host (host fallback) is valid and will pass.

module m
  interface
    integer function add_3 (x)
      implicit none
      integer, value :: x
    end function
  end interface
  integer :: global_var = 5
end module m

module m2
  use m
  !$omp requires reverse_offload
  implicit none (type, external)
contains
  subroutine check_offload (x, y)
    integer :: x, y
    x = add_3(x)
    y = add_3(y)
  end subroutine check_offload
  subroutine m2_tg_fn(x, y)
    integer :: x, y
    !$omp declare target
    if (x /= 2 .or. y /= 3) stop 1
    x = x + 2
    y = y + 7
    !$omp target device(ancestor : 1) map(tofrom: x)
      call check_offload(x, y)
    !$omp end target
    if (x /= 2+2+3 .or. y /= 3 + 7) stop 2
  end subroutine
end module m2

program main
  use m
  !$omp requires reverse_offload
  implicit none (type, external)

  integer :: prog_var = 99

  !$omp target
  block
    use m2
    integer :: x, y
    x = 2; y = 3
    call m2_tg_fn (x, y)
  end block

  !$omp target
  block
    use m2
    integer :: x, y
    x = -2; y = -1
    !$omp target device ( ancestor:1 ) firstprivate(y) map(tofrom:x)
      if (x /= -2 .or. y /= -1) stop 3
      call my_func (x, y)
      if (x /= 2*(3-2) .or. y /= 3*(3-1)) stop 5
    !$omp end target
    if (x /= 2*(3-2) .or. y /= -1) stop 6
  end block

  if (prog_var /= 41 .or. global_var /= 242) stop 7

contains

  subroutine my_func(x, y)
    integer :: x, y
    if (prog_var /= 99) stop 8
    if (global_var /= 5) stop 9
    prog_var = 41
    global_var = 242
    x = 2*add_3(x)
    y = 3*add_3(y)
  end subroutine my_func
end
