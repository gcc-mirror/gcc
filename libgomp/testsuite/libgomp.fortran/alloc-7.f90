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
  integer(c_ptrdiff_t) :: iptr
  type (c_ptr), volatile :: p, q, r
  integer, pointer, volatile, contiguous :: ip(:), iq(:), ir(:)
  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a, a2
  traits  = [ omp_alloctrait (omp_atk_alignment, 64), &
              omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
              omp_alloctrait (omp_atk_pool_size, 4096)]

  p = omp_aligned_alloc (c_sizeof (0), 3 * c_sizeof (0), omp_default_mem_alloc)
  call c_f_pointer (p, ip, [3])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0) &
    stop 1
  ip(1) = 1
  ip(2) = 2
  ip(3) = 3
  call omp_free (p, omp_default_mem_alloc)

  p = omp_aligned_alloc (2 * c_sizeof (0), 2 * c_sizeof (0), omp_default_mem_alloc)
  call c_f_pointer (p, ip, [2])
  if (mod (TRANSFER (p, iptr), 2 * c_sizeof (0)) /= 0) &
    stop 2
  ip(1) = 1
  ip(2) = 2
  call omp_free (p, omp_null_allocator)

  call omp_set_default_allocator (omp_default_mem_alloc)
  p = omp_aligned_alloc (1_c_size_t, 2 * c_sizeof (0), omp_null_allocator)
  call c_f_pointer (p, ip, [2])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0) &
    stop 3
  ip(1) = 3
  call omp_free (p, omp_get_default_allocator ())

  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) &
    stop 4
  p = omp_aligned_alloc (32_c_size_t, 3072_c_size_t, a)
  call c_f_pointer (p, ip, [3072/c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 64) /= 0) &
    stop 5
  ip(1) = 1
  ip(3072 / c_sizeof (0)) = 2

  if (c_associated (omp_aligned_alloc (8_c_size_t, 3072_c_size_t, a))) &
    stop 6

  call omp_free (p, a)

  p = omp_aligned_alloc (128_c_size_t, 3072_c_size_t, a)
  call c_f_pointer (p, ip, [3072/c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 128) /= 0) &
    stop 7
  ip(1) = 3
  ip(3072 / c_sizeof (0)) = 4
  call omp_free (p, omp_null_allocator)

  call omp_set_default_allocator (a)
  if (omp_get_default_allocator () /= a) &
    stop 8
  p = omp_aligned_alloc (64_c_size_t, 3072_c_size_t, omp_null_allocator)
  call c_f_pointer (p, ip, [3072/c_sizeof (0)])
  if (c_associated (omp_aligned_alloc (8_c_size_t, 3072_c_size_t, omp_null_allocator))) &
    stop 9
  call omp_free (p, a)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, size (traits2), traits2)
  if (a == omp_null_allocator) &
    stop 9
  if (traits3(6)%key /= omp_atk_fb_data) &
    stop 10
  traits3(6)%value = a
  a2 = omp_init_allocator (omp_default_mem_space, size (traits3), traits3)
  if (a2 == omp_null_allocator) &
    stop 11

  p = omp_aligned_alloc (4_c_size_t, 420_c_size_t, a2)
  call c_f_pointer (p, ip, [420/c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 12
  ip(1) = 5
  ip(420 / c_sizeof (0)) = 6

  q = omp_aligned_alloc (8_c_size_t, 768_c_size_t, a2)
  call c_f_pointer (q, iq, [768/c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 16) /= 0) &
    stop 13
  iq(1) = 7
  iq(768 / c_sizeof (0)) = 8

  r = omp_aligned_alloc (8_c_size_t, 512_c_size_t, a2)
  call c_f_pointer (r, ir, [512/c_sizeof (0)])
  if (mod (TRANSFER (r, iptr), 8) /= 0) &
    stop 14
  ir(1) = 9
  ir(512 / c_sizeof (0)) = 10
  call omp_free (p, omp_null_allocator)
  call omp_free (q, a2)
  call omp_free (r, omp_null_allocator)
  call omp_destroy_allocator (a2)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, size (traits4), traits4)
  if (a == omp_null_allocator) &
    stop 15
  if (traits3(6)%key /= omp_atk_fb_data) &
    stop 16
  traits3(6)%value = a
  a2 = omp_init_allocator (omp_default_mem_space, size (traits3), traits3)
  if (a2 == omp_null_allocator) &
    stop 17
  call omp_set_default_allocator (a2)

  p = omp_aligned_alloc (4_c_size_t, 420_c_size_t, omp_null_allocator)
  call c_f_pointer (p, ip, [420/c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 18
  ip(1) = 5
  ip(420 / c_sizeof (0)) = 6

  q = omp_aligned_alloc (64_c_size_t, 768_c_size_t, omp_null_allocator)
  call c_f_pointer (q, iq, [768/c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 128) /= 0) &
    stop 19
  iq(1) = 7
  iq(768 / c_sizeof (0)) = 8
  if (c_associated (omp_aligned_alloc (8_c_size_t, 768_c_size_t, omp_null_allocator))) &
    stop 20
  call omp_free (p, omp_null_allocator)
  call omp_free (q, omp_null_allocator)
  call omp_free (c_null_ptr, omp_null_allocator)
  call omp_free (c_null_ptr, omp_null_allocator)
  call omp_destroy_allocator (a2)
  call omp_destroy_allocator (a)
end program main
