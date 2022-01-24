module m
  use omp_lib
  implicit none

  type (omp_alloctrait), parameter :: traits(*) &
    = [ omp_alloctrait (omp_atk_pool_size, 1), &
        omp_alloctrait (omp_atk_fallback, omp_atv_abort_fb) ]
end module m

program main
  use m
  use iso_c_binding
  implicit none (external, type)
  integer (omp_allocator_handle_kind) :: a
  integer(c_size_t), parameter :: zero = 0_c_size_t

  if (c_associated (omp_alloc (zero, omp_null_allocator))) &
    stop 1
  if (c_associated (omp_aligned_alloc (64_c_size_t, zero, omp_null_allocator))) &
    stop 2
  if (c_associated (omp_calloc (zero, zero, omp_null_allocator)) &
      .or. c_associated (omp_calloc (32_c_size_t, zero, omp_null_allocator)) &
      .or. c_associated (omp_calloc (zero, 64_c_size_t, omp_null_allocator))) &
    stop 3
  if (c_associated (omp_aligned_calloc (32_c_size_t, zero, zero, omp_null_allocator)) &
      .or. c_associated (omp_aligned_calloc (64_c_size_t, 32_c_size_t, zero, omp_null_allocator)) &
      .or. c_associated (omp_aligned_calloc (16_c_size_t, zero, 64_c_size_t, omp_null_allocator))) &
    stop 4
  a = omp_init_allocator (omp_default_mem_space, 2, traits)
  if (a /= omp_null_allocator) then
    if (c_associated (omp_alloc (zero, a)) &
        .or. c_associated (omp_alloc (zero, a)) &
        .or. c_associated (omp_alloc (zero, a)) &
        .or. c_associated (omp_aligned_alloc (16_c_size_t, zero, a)) &
        .or. c_associated (omp_aligned_alloc (128_c_size_t, zero, a)) &
        .or. c_associated (omp_calloc (zero, zero, a)) &
        .or. c_associated (omp_calloc (32_c_size_t, zero, a)) &
        .or. c_associated (omp_calloc (zero, 64_c_size_t, a)) &
        .or. c_associated (omp_aligned_calloc (32_c_size_t, zero, zero, a)) &
        .or. c_associated (omp_aligned_calloc (64_c_size_t, 32_c_size_t, zero, a)) &
        .or. c_associated (omp_aligned_calloc (16_c_size_t, zero, 64_c_size_t, a))) &
      stop 5
    call omp_destroy_allocator (a)
  end if
end program main
