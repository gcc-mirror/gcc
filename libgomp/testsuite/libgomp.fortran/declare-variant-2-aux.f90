! { dg-do compile { target skip-all-targets } } 

! Test XFAILed due to https://gcc.gnu.org/PR115271


subroutine base_proc (a)
   use iso_c_binding, only: c_ptr
   type(c_ptr), intent(inout) :: a
end subroutine

program main
   use iso_c_binding, only: c_ptr
   use my_mod
   implicit none

   type(c_ptr) :: a


   call base_proc(a)
   !call variant_proc(a)

   !$omp dispatch
   call base_proc(a)

end program main
