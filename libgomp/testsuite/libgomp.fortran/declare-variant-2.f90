! { dg-do run }
! { dg-additional-sources declare-variant-2-aux.f90 }
! { dg-additional-options "-fdump-tree-gimple" }

module my_mod
   use iso_c_binding, only: c_ptr
   implicit none
   interface
      subroutine base_proc (a)
         use iso_c_binding, only: c_ptr
         type(c_ptr), intent(inout) :: a
      end subroutine
   end interface

contains
   subroutine variant_proc (a)
      type(c_ptr), intent(inout) :: a
      !$omp declare variant (base_proc) match (construct={dispatch}) adjust_args(need_device_ptr: a)
   end subroutine
end module

! { dg-final { scan-tree-dump "variant_proc \\(&a\\)" "gimple" { xfail *-*-* } } }
