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
  type (omp_alloctrait) :: traits5(2)
  integer (omp_allocator_handle_kind) :: a, a2
  integer (c_ptrdiff_t) :: iptr

  traits = [ omp_alloctrait (omp_atk_alignment, 64), &
             omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
             omp_alloctrait (omp_atk_pool_size, 4096)]
  traits5 = [ omp_alloctrait (omp_atk_fallback, omp_atv_null_fb), &
              omp_alloctrait (omp_atk_pool_size, 4096)]

  p = omp_alloc (3 * c_sizeof (0), omp_default_mem_alloc)
  call c_f_pointer (p, ip, [3])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0) &
    stop 1
  ip(1) = 1
  ip(2) = 2
  ip(3) = 3
  p = omp_realloc (p, 4 * c_sizeof (0), omp_default_mem_alloc, omp_default_mem_alloc)
  call c_f_pointer (p, ip, [4])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0 &
      .or. ip(1) /= 1 .or. ip(2) /= 2 .or. ip(3) /= 3) &
    stop 2
  ip(1) = 4
  ip(2) = 5
  ip(3) = 6
  ip(4) = 7
  p = omp_realloc (p, 2 * c_sizeof (0), omp_default_mem_alloc, omp_default_mem_alloc)
  call c_f_pointer (p, ip, [2])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0 &
      .or. ip(1) /= 4 .or. ip(2) /= 5) &
    stop 3
  ip(1) = 8
  ip(2) = 9
  if (c_associated (omp_realloc (p, 0_c_size_t, omp_null_allocator, omp_default_mem_alloc))) &
    stop 4
  p = omp_realloc (c_null_ptr, 2 * c_sizeof (0), omp_default_mem_alloc, omp_null_allocator)
  call c_f_pointer (p, ip, [2])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0) &
    stop 5
  ip(1) = 1
  ip(2) = 2
  p = omp_realloc (p, 5 * c_sizeof (0), omp_default_mem_alloc, omp_default_mem_alloc)
  call c_f_pointer (p, ip, [5])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0 &
      .or. ip(1) /= 1 .or. ip(2) /= 2) &
    stop 6
  ip(1) = 3
  ip(2) = 4
  ip(3) = 5
  ip(4) = 6
  ip(5) = 7
  call omp_free (p, omp_null_allocator)
  call omp_set_default_allocator (omp_default_mem_alloc)
  if (c_associated (omp_realloc (c_null_ptr, 0_c_size_t, omp_null_allocator, omp_null_allocator))) &
    stop 7
  p = omp_alloc (c_sizeof (0), omp_null_allocator)
  call c_f_pointer (p, ip, [1])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0) &
    stop 8
  ip(1) = 3
  p = omp_realloc (p, 3 * c_sizeof (0), omp_null_allocator, omp_null_allocator)
  call c_f_pointer (p, ip, [3])
  if (mod (TRANSFER (p, iptr), get__alignof_int ()) /= 0 &
      .or. ip(1) /= 3) &
    stop 9
  ip(1) = 4
  ip(2) = 5
  ip(3) = 6
  if (c_associated (omp_realloc (p, 0_c_size_t, omp_null_allocator, omp_get_default_allocator ()))) &
    stop 10
  a = omp_init_allocator (omp_default_mem_space, 3, traits)
  if (a == omp_null_allocator) &
    stop 11
  p = omp_alloc (c_sizeof (0), a)
  call c_f_pointer (p, ip, [1])
  if (mod (TRANSFER (p, iptr), 64) /= 0) &
    stop 12
  ip(1) = 7
  p = omp_realloc (p, 3072_c_size_t, a, a)
  call c_f_pointer (p, ip, [3072 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 64) /= 0 &
      .or. ip(1) /= 7) &
    stop 13
  ip(1) = 1
  ip(3072 / c_sizeof (0)) = 2
  q = omp_alloc (c_sizeof (0), a)
  call c_f_pointer (q, iq, [1])
  if (mod (TRANSFER (q, iptr), 64) /= 0) &
    stop 14
  iq(1) = 8
  if (c_associated (omp_realloc (q, 3072_c_size_t, a, a))) &
    stop 15
  call omp_free (p, a)
  call omp_free (q, a)
  p = omp_alloc (c_sizeof (0), a)
  call c_f_pointer (p, ip, [1])
  ip(1) = 42
  p = omp_realloc (p, 3072_c_size_t, a, a)
  call c_f_pointer (p, ip, [3072 / c_sizeof (0)])
  if (ip(1) /= 42) &
    stop 16
  ip(1) = 3
  ip(3072 / c_sizeof (0)) = 4
  ! ignore return value
  r = omp_realloc (p, 0_c_size_t, omp_null_allocator, omp_null_allocator)
  call omp_set_default_allocator (a)
  if (omp_get_default_allocator () /= a) &
    stop 17
  p = omp_alloc (31_c_size_t, omp_null_allocator)
  if (.not. c_associated (p)) &
    stop 18
  p = omp_realloc (p, 3072_c_size_t, omp_null_allocator, omp_null_allocator)
  if (.not. c_associated (p)) &
    stop 19
  q = omp_alloc (c_sizeof (0), omp_null_allocator)
  if (.not. c_associated (q)) &
    stop 20
  if (c_associated (omp_realloc (q, 3072_c_size_t, omp_null_allocator, omp_null_allocator))) &
    stop 21
  call omp_free (p, a)
  call omp_free (q, a)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, 2, traits5)
  if (a == omp_null_allocator) &
    stop 22
  call omp_set_default_allocator (a)
  if (omp_get_default_allocator () /= a) &
    stop 23
  p = omp_alloc (3071_c_size_t, omp_null_allocator)
  if (.not. c_associated (p)) &
    stop 24
  p = omp_realloc (p, 3072_c_size_t, omp_null_allocator, omp_null_allocator)
  if (.not. c_associated (p)) &
    stop 25
  q = omp_alloc (c_sizeof (0), omp_null_allocator)
  if (.not. c_associated (q)) &
    stop 26
  if (c_associated (omp_realloc (q, 3072_c_size_t, omp_null_allocator, omp_null_allocator))) &
    stop 27
  call omp_free (p, a)
  call omp_free (q, a)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, size (traits2), traits2)
  if (a == omp_null_allocator) &
    stop 28
  if (traits3(6)%key /= omp_atk_fb_data) &
    stop 29
  traits3(6)%value = a
  a2 = omp_init_allocator (omp_default_mem_space, size (traits3), traits3)
  if (a2 == omp_null_allocator) &
    stop 30
  p = omp_alloc (c_sizeof (0), a2)
  call c_f_pointer (p, ip, [1])
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 31
  ip(1) = 84
  p = omp_realloc (p, 380_c_size_t, a2, a2)
  call c_f_pointer (p, ip, [380 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 32) /= 0 &
      .or. ip(1) /= 84) &
    stop 32
  ip(1) = 5
  ip(380 / c_sizeof (0)) = 6
  q = omp_alloc (c_sizeof (0), a2)
  call c_f_pointer (q, iq, [1])
  if (mod (TRANSFER (q, iptr), 32) /= 0) &
    stop 33
  iq(1) = 42
  q = omp_realloc (q, 768_c_size_t, a2, a2)
  call c_f_pointer (q, iq, [768 / c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 16) /= 0 &
      .or. iq(1) /= 42) &
    stop 34
  iq(1) = 7
  iq(768 / c_sizeof (0)) = 8
  r = omp_realloc (c_null_ptr, 512_c_size_t, a2, omp_null_allocator)
  call c_f_pointer (r, ir, [512 / c_sizeof (0)])
  if (mod (TRANSFER (r, iptr), get__alignof_int ()) /= 0) &
    stop 35
  ir(1) = 9
  ir(512 / c_sizeof (0)) = 10
  call omp_free (p, omp_null_allocator)
  call omp_free (q, a2)
  call omp_free (r, omp_null_allocator)
  p = omp_alloc (c_sizeof (0), a2)
  call c_f_pointer (p, ip, [1])
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 36
  ip(1) = 85
  p = omp_realloc (p, 320_c_size_t, a, a2)
  call c_f_pointer (p, ip, [320 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 16) /= 0 &
      .or. ip(1) /= 85) &
    stop 37
  ip(1) = 5
  ip(320 / c_sizeof (0)) = 6
  q = omp_alloc (c_sizeof (0), a)
  call c_f_pointer (q, iq, [1])
  if (mod (TRANSFER (q, iptr), 16) /= 0) &
    stop 38
  iq(1) = 43
  q = omp_realloc (q, 320_c_size_t, a2, a)
  call c_f_pointer (q, iq, [320 / c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 32) /= 0 &
      .or. iq(1) /= 43) &
    stop 39
  iq(1) = 44
  iq(320 / c_sizeof (0)) = 8
  q = omp_realloc (q, 568_c_size_t, a2, a2)
  call c_f_pointer (q, iq, [568 / c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 16) /= 0 &
      .or. iq(1) /= 44) &
    stop 40
  iq(1) = 7
  iq(568 / c_sizeof (0)) = 8
  call omp_free (p, omp_null_allocator)
  call omp_free (q, a2)
  call omp_destroy_allocator (a2)
  call omp_destroy_allocator (a)

  a = omp_init_allocator (omp_default_mem_space, size (traits4), traits4)
  if (a == omp_null_allocator) &
    stop 41
  if (traits3(6)%key /= omp_atk_fb_data) &
    stop 1
  traits3(6)%value = a
  a2 = omp_init_allocator (omp_default_mem_space, size (traits3), traits3)
  if (a2 == omp_null_allocator) &
    stop 42
  call omp_set_default_allocator (a2)
  p = omp_realloc (c_null_ptr, 420_c_size_t, omp_null_allocator, omp_null_allocator)
  call c_f_pointer (p, ip, [420 / c_sizeof (0)])
  if (mod (TRANSFER (p, iptr), 32) /= 0) &
    stop 43
  ip(1) = 5
  ip(420 / c_sizeof (0)) = 6
  q = omp_realloc (c_null_ptr, c_sizeof (0), omp_null_allocator, omp_null_allocator)
  call c_f_pointer (q, iq, [1])
  if (mod (TRANSFER (q, iptr), 32) /= 0) &
    stop 44
  iq(1) = 99
  q = omp_realloc (q, 700_c_size_t, omp_null_allocator, omp_null_allocator)
  call c_f_pointer (q, iq, [700 / c_sizeof (0)])
  if (mod (TRANSFER (q, iptr), 128) /= 0 &
      .or. iq(1) /= 99) &
    stop 45
  iq(1) = 7
  iq(700 / c_sizeof (0)) = 8
  if (c_associated (omp_realloc (c_null_ptr, 768_c_size_t, omp_null_allocator, omp_null_allocator))) &
    stop 46
  call omp_free (p, omp_null_allocator)
  if (c_associated (omp_realloc (q, 0_c_size_t, omp_null_allocator, omp_null_allocator))) &
    stop 47
  call omp_free (c_null_ptr, omp_null_allocator)
  call omp_free (c_null_ptr, omp_null_allocator)
  call omp_destroy_allocator (a2)
  call omp_destroy_allocator (a)
end program main
