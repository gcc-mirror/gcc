module m
  use omp_lib
  implicit none

  type (omp_alloctrait), parameter :: traits(*) &
    = [ omp_alloctrait (omp_atk_alignment, 16), &
        omp_alloctrait (omp_atk_sync_hint, omp_atv_default), &
        omp_alloctrait (omp_atk_access, omp_atv_default), &
        omp_alloctrait (omp_atk_fallback, omp_atv_default_mem_fb), &
        omp_alloctrait (omp_atk_partition, omp_atv_environment)]
end module m

program main
  use m
  use iso_c_binding
  implicit none (external, type)
  integer (omp_allocator_handle_kind) :: a
  type (c_ptr) :: p, q
  integer (c_size_t), volatile :: large_sz
  integer (c_ptrdiff_t) :: iptr

  a = omp_init_allocator (omp_default_mem_space, size (traits), traits)
  if (a == omp_null_allocator) &
    stop 1
  p = omp_alloc (2048_c_size_t, a)
  if (mod (TRANSFER (p, iptr), 16) /= 0) &
    stop 2
  large_sz = NOT (1023_c_size_t)
  q = omp_alloc (large_sz, a)
  if (c_associated (q)) &
    stop 3
  q = omp_aligned_alloc (32_c_size_t, large_sz, a)
  if (c_associated (q)) &
    stop 4
  q = omp_calloc (large_sz / 4_c_size_t, 4_c_size_t, a)
  if (c_associated (q)) &
    stop 5
  q = omp_aligned_calloc (1_c_size_t, 2_c_size_t, large_sz / 2, a)
  if (c_associated (q)) &
    stop 6
  call omp_free (p, a)
  large_sz = NOT (0_c_size_t)
  large_sz = ISHFT (large_sz, -1)
  large_sz = large_sz + 1  ! signed integer overflow
  if (c_associated (omp_calloc (2_c_size_t, large_sz, a))) &
    stop 7
  if (c_associated (omp_calloc (large_sz, 1024_c_size_t, a))) &
    stop 8
  if (c_associated (omp_calloc (large_sz, large_sz, a))) &
    stop 9
  if (c_associated (omp_aligned_calloc (16_c_size_t, 2_c_size_t, large_sz, a))) &
    stop 10
  if (c_associated (omp_aligned_calloc (32_c_size_t, large_sz, 1024_c_size_t, a))) &
    stop 11
  if (c_associated (omp_aligned_calloc (64_c_size_t, large_sz, large_sz, a))) &
    stop 12
  call omp_destroy_allocator (a)
end program main
