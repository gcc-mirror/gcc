! { dg-do run }
! { dg-additional-sources allocate-1.c }
! { dg-prune-output "command-line option '-fintrinsic-modules-path=.*' is valid for Fortran but not for C" }

module m
  use omp_lib
  use iso_c_binding
  implicit none
  interface
    integer(c_int) function is_64bit_aligned (a) bind(C)
      import :: c_int
      integer  :: a
    end
  end interface

contains

subroutine foo (x, y, h)
  use omp_lib
  integer  :: x
  integer  :: y
  integer (kind=omp_allocator_handle_kind) :: h
  integer, allocatable :: var1

  !$omp allocate (var1)  allocator(h)
  allocate (var1)

  if (is_64bit_aligned(var1) == 0) then
    stop 19
  end if

  deallocate(var1)
end subroutine
end module m

program main
  use omp_lib
  use m
  type (omp_alloctrait) :: traits(2)
  integer (omp_allocator_handle_kind) :: a

  traits = [omp_alloctrait (omp_atk_alignment, 64), &
            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb)]
  a = omp_init_allocator (omp_default_mem_space, 2, traits)
  if (a == omp_null_allocator) stop 1
  call foo (42, 12, a);
  call omp_destroy_allocator (a);
end
