! { dg-do link }
! { dg-additional-options "-fdump-tree-gimple" }
! { dg-additional-sources "declare-variant-mod-1-use.f90" }

! Note: We have to use 'link' as otherwise '-o' is specified,
! which does not work with multiple files.

! Check that module-file handling works for declare_variant
! and its match/adjust_args/append_args clauses
!
! PR fortran/115271

! Define to make linker happy
integer function m1_f (x, y, z)
  use iso_c_binding
  type(c_ptr) :: x, y, z
  value :: x
end

integer function m1_g (x, y, z)
  use iso_c_binding
  type(c_ptr) :: x, y, z
  value :: x
end

module m1
  implicit none (type, external)

  interface
    integer function m1_f (x, y, z)
      use iso_c_binding
      type(c_ptr) :: x, y, z
      value :: x
    end
    integer function m1_g (x, y, z)
      !$omp declare variant(m1_f) match(construct={dispatch}) adjust_args(need_device_ptr: x, 3) adjust_args(nothing: y)
      use iso_c_binding
      type(c_ptr) :: x, y, z
      value :: x
    end
  end interface
end module m1

module m2
  implicit none (type, external)
contains
  integer function m2_f (x, y, z)
    use iso_c_binding
    type(c_ptr) :: x, y, z
    value :: x
    m2_f = 1
  end
  integer function m2_g (x, y, z)
    !$omp declare variant(m2_f) match(construct={dispatch}) adjust_args(need_device_ptr: x, 3) adjust_args(nothing: y)
    use iso_c_binding
    type(c_ptr) :: x, y, z
    value :: x
    m2_g = 2
  end
end module m2

module m3_pre
  implicit none (type, external)
contains
  integer function m3_f (x, y, z)
    use iso_c_binding
    type(c_ptr) :: x, y, z
    value :: x
    m3_f = 1
  end
  integer function m3_g (x, y, z)
    use iso_c_binding
    type(c_ptr) :: x, y, z
    value :: x
    m3_g = 2
  end
end module m3_pre

module m3
  use m3_pre, only: my_m3_f => m3_f, my_m3_g => m3_g
  implicit none (type, external)
  !$omp declare variant(my_m3_g : my_m3_f) match(construct={dispatch}) adjust_args(need_device_ptr: 1, 3) adjust_args(nothing: 2)
end module m3
