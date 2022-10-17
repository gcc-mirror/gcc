! { dg-additional-options "-Wall -Wextra -Wno-maybe-uninitialized" }
#ifdef DEFAULT_INTEGER_8
#define ONEoFIVE 105_c_size_t*8_c_size_t
#else
#define ONEoFIVE 105_c_size_t*4_c_size_t
#endif
      program main
        use iso_c_binding
#ifdef USE_F77_INCLUDE
        implicit none
#include "omp_lib.h"
#else
        use omp_lib
        implicit none (external, type)
#endif

        type (omp_alloctrait), parameter :: traits2(*)                  &
     &    = [omp_alloctrait (omp_atk_alignment, 16),                    &
     &       omp_alloctrait (omp_atk_sync_hint, omp_atv_default),       &
     &       omp_alloctrait (omp_atk_access, omp_atv_default),          &
     &       omp_alloctrait (omp_atk_pool_size, 1024),                  &
     &       omp_alloctrait (omp_atk_fallback, omp_atv_default_mem_fb), &
     &       omp_alloctrait (omp_atk_partition, omp_atv_environment)]
        type (omp_alloctrait), parameter :: traits3(*)                  &
     &    = [omp_alloctrait (omp_atk_sync_hint, omp_atv_uncontended),   &
     &       omp_alloctrait (omp_atk_alignment, 32),                    &
     &       omp_alloctrait (omp_atk_access, omp_atv_all),              &
     &       omp_alloctrait (omp_atk_pool_size, 512),                   &
     &       omp_alloctrait (omp_atk_fallback, omp_atv_allocator_fb),   &
     &       omp_alloctrait (omp_atk_fb_data, 0),                       &
     &       omp_alloctrait (omp_atk_partition, omp_atv_default)]
        type (omp_alloctrait), parameter :: traits4(*)                  &
     &    = [omp_alloctrait (omp_atk_alignment, 128),                   &
     &       omp_alloctrait (omp_atk_pool_size, 1024),                  &
     &       omp_alloctrait (omp_atk_fallback, omp_atv_null_fb)]

        type (omp_alloctrait), allocatable :: traits(:), traits5(:)

        type(c_ptr), volatile :: cp, cq, cr
        integer :: i
        integer(c_intptr_t) :: intptr
        integer, pointer, volatile :: p(:), p0, q(:), r(:)
        integer (omp_allocator_handle_kind) :: a, a2

        cp = omp_alloc (3_c_size_t * c_sizeof (i),                      &
     &                  omp_default_mem_alloc)
        if (mod (transfer (cp, intptr), 4_c_intptr_t) /= 0) stop 1
        call c_f_pointer (cp, p, [3])
        p(1) = 1
        p(2) = 2
        p(3) = 3
        call omp_free (cp, omp_default_mem_alloc)

        cp = omp_alloc (2_c_size_t * c_sizeof (i),                      &
     &                  omp_default_mem_alloc)
        if (mod (transfer (cp, intptr), 4_c_intptr_t) /= 0) stop 2
        call c_f_pointer (cp, p, [2])
        p(1) = 1
        p(2) = 2
        call omp_free (cp, omp_null_allocator)

        call omp_set_default_allocator (omp_default_mem_alloc)
        cp = omp_alloc (c_sizeof (i), omp_null_allocator)
        if (mod (transfer (cp, intptr), 4_c_intptr_t) /= 0) stop 3
        call c_f_pointer (cp, p0)
        p0 = 3
        call omp_free (cp, omp_get_default_allocator ())

        traits = [omp_alloctrait (omp_atk_alignment, 64),               &
     &            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb),   &
     &            omp_alloctrait (omp_atk_pool_size, 4096)]
        a = omp_init_allocator (omp_default_mem_space, 3, traits)
        if (a == omp_null_allocator) stop 4
        cp = omp_alloc (3072_c_size_t, a)
        if (mod (transfer (cp, intptr), 64_c_intptr_t) /= 0) stop 4
        call c_f_pointer (cp, p, [3072 / c_sizeof (i)])
        p(1) = 1
        p(3072 / c_sizeof (i)) = 2
        if (c_associated (omp_alloc (3072_c_size_t, a))) stop 5
        call omp_free (cp, a)
        cp = omp_alloc (3072_c_size_t, a)
        call c_f_pointer (cp, p, [3072 / c_sizeof (i)])
        p(1) = 3
        p(3072 / c_sizeof (i)) = 4
        call omp_free (cp, omp_null_allocator)
        call omp_set_default_allocator (a)
        if (omp_get_default_allocator () /= a) stop 6
        cp = omp_alloc (3072_c_size_t, omp_null_allocator)
        if (c_associated (omp_alloc (3072_c_size_t,                     &
     &                    omp_null_allocator)))                         &
     &     stop 7
        call omp_free (cp, a)
        call omp_destroy_allocator (a)

        traits5 = traits3
        a = omp_init_allocator (omp_default_mem_space, size (traits2),  &
     &                          traits2)
        if (a == omp_null_allocator) stop 8
        if (traits5(6)%key /= omp_atk_fb_data) stop 9
        traits5(6)%value = a
        if (traits5(4)%key /= omp_atk_pool_size) stop 20
#if DEFAULT_INTEGER_8
        traits5(4)%value = 1024
#endif
        a2 = omp_init_allocator (omp_default_mem_space,                 &
     &                           size (traits5), traits5)
        if (a2 == omp_null_allocator) stop 10
        cp = omp_alloc (ONEoFIVE, a2)
        if (mod (transfer (cp, intptr), 32_c_intptr_t) /= 0) stop 11
        call c_f_pointer (cp, p, [ONEoFIVE                              &
     &                            / c_sizeof (i)])
        p(1) = 5
        p(ONEoFIVE / c_sizeof (i)) = 6
        cq = omp_alloc (768_c_size_t, a2)
        if (mod (transfer (cq, intptr), 16_c_intptr_t) /= 0) stop 12
        call c_f_pointer (cq, q, [768 / c_sizeof (i)])
        q(1) = 7
        q(768 / c_sizeof (i)) = 8
        cr = omp_alloc (512_c_size_t, a2)
        if (mod (transfer (cr, intptr), 4_c_intptr_t) /= 0) stop 13
        call c_f_pointer (cr, r, [512 / c_sizeof (i)])
        r(1) = 9
        r(512 / c_sizeof (i)) = 10
        call omp_free (cp, omp_null_allocator)
        call omp_free (cq, a2)
        call omp_free (cr, omp_null_allocator)
        call omp_destroy_allocator (a2)
        call omp_destroy_allocator (a)

        a = omp_init_allocator (omp_default_mem_space, size (traits4),  &
     &                          traits4)
        if (a == omp_null_allocator) stop 14
        if (traits5(6)%key /= omp_atk_fb_data) stop 15
        traits5(6)%value = a
        a2 = omp_init_allocator (omp_default_mem_space,                 &
     &                           size (traits5), traits5)
        if (a2 == omp_null_allocator) stop 16
        call omp_set_default_allocator (a2)
        cp = omp_alloc (ONEoFIVE,                                       &
     &                  omp_null_allocator)
        if (mod (transfer (cp, intptr), 32_c_intptr_t) /= 0) stop 17
        call c_f_pointer (cp, p, [ONEoFIVE                              &
     &                            / c_sizeof (i)])
        p(1) = 5
        p(ONEoFIVE / c_sizeof (i)) = 6
        cq = omp_alloc (768_c_size_t, omp_null_allocator)
        if (mod (transfer (cq, intptr), 128_c_intptr_t) /= 0) stop 18
        call c_f_pointer (cq, q, [768 / c_sizeof (i)])
        q(1) = 7
        q(768 / c_sizeof (i)) = 8
        if (c_associated (omp_alloc (768_c_size_t, omp_null_allocator))) &
     &    stop 19
        call omp_free (cp, omp_null_allocator)
        call omp_free (cq, omp_null_allocator)
        call omp_free (c_null_ptr, omp_null_allocator)
        call omp_free (c_null_ptr, omp_null_allocator)
        call omp_destroy_allocator (a2)
        call omp_destroy_allocator (a)
      end program
