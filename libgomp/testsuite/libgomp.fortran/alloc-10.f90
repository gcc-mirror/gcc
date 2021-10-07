! { dg-additional-sources alloc-7.c }
! { dg-prune-output "command-line option '-fintrinsic-modules-path=.*' is valid for Fortran but not for C" }
module m
  use omp_lib
  use iso_c_binding
  implicit none

  type (omp_alloctrait), parameter :: traits2(*) &
    = [ omp_alloctrait (omp_atk_alignment, 16), &
        omp_alloctrait (omp_atk_sync_hint, omp_atv_default), &
        omp_alloctrait (omp_atk_access, omp_atv_default), &
        omp_alloctrait (omp_atk_pool_size, 1024), &
        omp_alloctrait (omp_atk_fallback, omp_atv_default_mem_fb), &
        omp_alloctrait (omp_atk_partition, omp_atv_environment)]
  type (omp_alloctrait) :: traits3(7) &
    = [ omp_alloctrait (omp_atk_sync_hint, omp_atv_uncontended), &
        omp_alloctrait (omp_atk_alignment, 32), &
        omp_alloctrait (omp_atk_access, omp_atv_all), &
        omp_alloctrait (omp_atk_pool_size, 512), &
        omp_alloctrait (omp_atk_fallback, omp_atv_allocator_fb), &
        omp_alloctrait (omp_atk_fb_data, 0), &
        omp_alloctrait (omp_atk_partition, omp_atv_default)]
  type (omp_alloctrait), parameter :: traits4(*) &
    = [ omp_alloctrait (omp_atk_alignment, 128), &
        omp_alloctrait (omp_atk_pool_size, 1024), &
        omp_alloctrait (omp_atk_fallback, omp_atv_null_fb)]

  interface
    integer(c_int) function get__alignof_int () bind(C)
      import :: c_int
    end
  end interface
end module m

program main
  use m
  implicit none (external, type)
  type(c_ptr) :: p, q, r
  integer, pointer, contiguous :: ip(:), iq(:), ir(:)
  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a, a2
  integer (c_ptrdiff_t) :: iptr
  integer :: i

  traits  = [ omp_alloctrait (omp_atk_alignment, 64), &
              omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
              omp_alloctrait (omp_atk_pool_size, 4096)]

  p = omp_aligned_calloc (c_sizeof (0), 3_c_size_t, c_sizeof (0), omp_default_mem_alloc)
  call c_f_pointer (p, ip, [3])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0 &
      .or. ip(1) /= 0 .or. ip(2) /= 0 .or. ip(3) /= 0) &
    stop 1
  ip(1) = 1
  ip(2) = 2
  ip(3) = 3
  call omp_free (p, omp_default_mem_alloc)
  p = omp_aligned_calloc (2 * c_sizeof (0), 1_c_size_t, 2 * c_sizeof (0), omp_default_mem_alloc)
  call c_f_pointer (p, ip, [2])
  if (mod (TRANSFER (p, iptr), 2 * c_sizeof (0)) /= 0 &
      .or. ip(1) /= 0 .or. ip(2) /= 0) &
    stop 2
  ip(1) = 1
  ip(2) = 2
  call omp_free (p, omp_null_allocator)
  call omp_set_default_allocator (omp_default_mem_alloc)
  p = omp_aligned_calloc (1_c_size_t, 1_c_size_t, c_sizeof (0), omp_null_allocator)
  call c_f_pointer (p, ip, [1])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0 &
      .or. ip(1) /= 0) &
    stop 3
  ip(1) = 3
  call omp_free (p, omp_get_default_allocator ())

  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) &
    stop 4
  p = omp_aligned_calloc (32_c_size_t, 3_c_size_t, 1024_c_size_t, a)
  call c_f_pointer (p, ip, [3072 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 64) /= 0) &
    stop 5
  do i = 1, 3072 / c_sizeof (0)
    if (ip(i) /= 0) &
      stop 6
  end do
  ip(1) = 1
  ip(3072 / c_sizeof (0)) = 2
  if (c_associated (omp_aligned_calloc (8_c_size_t, 192_c_size_t, 16_c_size_t, a))) &
    stop 7
  call omp_free (p, a)
  p = omp_aligned_calloc (128_c_size_t, 6_c_size_t, 512_c_size_t, a)
  call c_f_pointer (p, ip, [3072 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 128) /= 0) &
    stop 8
  do i = 1, 3072 / c_sizeof (0)
    if (ip(i) /= 0) &
      stop 9
  end do
  ip(1) = 3
  ip(3072 / c_sizeof (0)) = 4
  call omp_free (p, omp_null_allocator)
  call omp_set_default_allocator (a)
  if (omp_get_default_allocator () /= a) &
    stop 10
  p = omp_aligned_calloc (64_c_size_t, 12_c_size_t, 256_c_size_t, omp_null_allocator)
  call c_f_pointer (p, ip, [3072 / c_sizeof (0)])
  do i = 1, 3072 / c_sizeof (0)
    if (ip(i) /= 0) &
      stop 11
  end do
  if (c_associated (omp_aligned_calloc (8_c_size_t, 128_c_size_t, 24_c_size_t, omp_null_allocator))) &
    stop 12
  call omp_free (p, a)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, size (traits2), traits2)
  if (a == omp_null_allocator) &
    stop 13
  if (traits3(6)%key /= omp_atk_fb_data) &
    stop 14
  traits3(6)%value = a
  a2 = omp_init_allocator (omp_default_mem_space, size (traits3), traits3)
  if (a2 == omp_null_allocator) &
    stop 15
  p = omp_aligned_calloc (4_c_size_t, 5_c_size_t, 84_c_size_t, a2)
  call c_f_pointer (p, ip, [420 / c_sizeof (0)])
  do i = 1, 420 / c_sizeof (0)
    if (ip(i) /= 0) &
      stop 16
  end do
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 17
  ip(1) = 5
  ip(420 / c_sizeof (0)) = 6
  q = omp_aligned_calloc (8_c_size_t, 24_c_size_t, 32_c_size_t, a2)
  call c_f_pointer (q, iq, [768 / c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 16) /= 0) &
    stop 18
  do i = 1, 768 / c_sizeof (0)
    if (iq(i) /= 0) &
      stop 19
  end do
  iq(1) = 7
  iq(768 / c_sizeof (0)) = 8
  r = omp_aligned_calloc (8_c_size_t, 64_c_size_t, 8_c_size_t, a2)
  call c_f_pointer (r, ir, [512 / c_sizeof (0)])
  if (mod (TRANSFER (r, iptr), 8) /= 0) &
    stop 20
  do i = 1, 512 / c_sizeof (0)
    if (ir(i) /= 0) &
      stop 21
  end do
  ir(1) = 9
  ir(512 / c_sizeof (0)) = 10
  call omp_free (p, omp_null_allocator)
  call omp_free (q, a2)
  call omp_free (r, omp_null_allocator)
  call omp_destroy_allocator (a2)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, size (traits4), traits4)
  if (a == omp_null_allocator) &
    stop 22
  if (traits3(6)%key /= omp_atk_fb_data) &
    stop 23
  traits3(6)%value = a
  a2 = omp_init_allocator (omp_default_mem_space, size (traits3), traits3)
  if (a2 == omp_null_allocator) &
    stop 24
  call omp_set_default_allocator (a2)
  p = omp_aligned_calloc (4_c_size_t, 21_c_size_t, 20_c_size_t, omp_null_allocator)
  call c_f_pointer (p, ip, [420 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 25
  do i = 1, 420 / c_sizeof (0)
    if (ip(i) /= 0)  &
      stop 26
  end do
  ip(1) = 5
  ip(420 / c_sizeof (0)) = 6
  q = omp_aligned_calloc (64_c_size_t, 12_c_size_t, 64_c_size_t, omp_null_allocator)
  call c_f_pointer (q, iq, [768 / c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 128) /= 0) &
    stop 27
  do i = 1, 768 / c_sizeof (0)
    if (iq(i) /= 0) &
      stop 28
  end do
  iq(1) = 7
  iq(768 / c_sizeof (0)) = 8
  if (c_associated (omp_aligned_calloc (8_c_size_t, 24_c_size_t, 32_c_size_t, omp_null_allocator))) &
    stop 29
  call omp_free (p, omp_null_allocator)
  call omp_free (q, omp_null_allocator)
  call omp_free (c_null_ptr, omp_null_allocator)
  call omp_free (c_null_ptr, omp_null_allocator)
  call omp_destroy_allocator (a2)
  call omp_destroy_allocator (a)
end program main
