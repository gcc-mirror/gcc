! { dg-additional-options "-Wall -Wextra" }
program main
  use omp_lib
  use ISO_C_Binding
  implicit none (external, type)
  type(c_ptr) :: p
  integer, pointer, contiguous :: ip(:)
  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a
  integer (c_ptrdiff_t) :: iptr

  traits = [omp_alloctrait (omp_atk_alignment, 64), &
            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
            omp_alloctrait (omp_atk_sync_hint, omp_atv_serialized)]
  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) stop 1

  p = omp_alloc (3 * c_sizeof (0), a)
  if (.not. c_associated (p)) stop 2
  call c_f_pointer (p, ip, [3])
  if (mod (TRANSFER (p, iptr), 64) /= 0) &
    stop 3
  ip(1) = 1
  ip(2) = 2
  ip(3) = 3
  call omp_free (p, a)
  call omp_destroy_allocator (a)
end program main
