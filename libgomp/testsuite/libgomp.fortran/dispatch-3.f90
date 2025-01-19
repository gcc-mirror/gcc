! { dg-do run }
! { dg-additional-options "-fdump-tree-gimple" }

! Check that nested function calls in a dispatch region are handled correctly,
! i.e. that the adjust_args clause is applied only to the outer call.

module m
  use iso_c_binding
  use omp_lib
  implicit none(type,external)
contains
  integer function f(x, y1, y2, z1, z2)
    allocatable :: f
    integer, value :: x
    type(c_ptr), value :: y1, y2
    type(c_ptr) :: z1, z2

    if (x == 1) then  ! HOST
      block
        integer, pointer :: iy1, iy2, iz1, iz2
        call c_f_pointer (y1, iy1)
        call c_f_pointer (y2, iy2)
        call c_f_pointer (z1, iz1)
        call c_f_pointer (z2, iz2)
        f = (iy1 + iy2) + 10 * (iz1+iz2)
      end block
    else
      allocate(f)
      !$omp target is_device_ptr(y1, y2, z1, z2) map(tofrom: f)
      block
        integer, pointer :: iy1, iy2, iz1, iz2
        call c_f_pointer (y1, iy1)
        call c_f_pointer (y2, iy2)
        call c_f_pointer (z1, iz1)
        call c_f_pointer (z2, iz2)
        f = -(iy1+iy2)*23  -127 * (iz1+iz2) - x * 3
      end block
    end if
  end

  integer function g(x, y1, y2, z1, z2)
    !$omp declare variant(f) match(construct={dispatch}) adjust_args(need_device_ptr : y1, y2, z1, z2)
    allocatable :: g
    integer, value :: x
    type(c_ptr), value :: y1, y2
    type(c_ptr) :: z1, z2
    g = x
    stop 2  ! should not get called
  end
end 

program main
  use m
  implicit none (type, external)
  integer, target :: v1, v2
  integer :: res, ref
  v1 = 5
  v2 = 11

  ref = 5*2 + 10 * 11*2
  ref = -(5*2)*23 -127 * (11*2) - ref * 3

  !$omp target data map(v1,v2)
    res = func (c_loc(v1), c_loc(v1), c_loc(v2), c_loc(v2))
  !$omp end target data

  if (res /= ref) stop 1
contains
integer function func(x1, x2, x3, x4)
  use m
  implicit none(type,external)
  type(c_ptr) :: x1, x2, x3, x4
  value :: x1, x3

  !$omp dispatch
    func = g(g(1,x1,x2,x3,x4), x1,x2,x3,x4)
end
end

! { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 4 "gimple" } }
