! { dg-do compile { target skip-all-targets } }

! Declare the following function in a separare translation unit
! to ensure it won't have a device version.


integer function add_3 (x)
  implicit none
  integer, value :: x

  add_3 = x + 3
end function
