/* { dg-do run } */

/* { dg-require-effective-target offload_device } */
/* { dg-xfail-if "not implemented" { ! { offload_target_nvptx || offload_target_amdgcn } } } */

/* Test that GPU low-latency allocation is limited to team access.  */

#include <stddef.h>
#include <omp.h>

#pragma omp requires dynamic_allocators

int
main ()
{
  #pragma omp target
  {
    /* Ensure that the memory we get *is* low-latency with a null-fallback.  */
    omp_alloctrait_t traits[2]
      = { { omp_atk_fallback, omp_atv_null_fb },
	  { omp_atk_access, omp_atv_pteam } };
    omp_allocator_handle_t lowlat = omp_init_allocator (omp_low_lat_mem_space,
							2, traits);

    omp_alloctrait_t traits_all[2]
      = { { omp_atk_fallback, omp_atv_null_fb },
	  { omp_atk_access, omp_atv_all } };
    omp_allocator_handle_t lowlat_all
      = omp_init_allocator (omp_low_lat_mem_space, 2, traits_all);

    omp_alloctrait_t traits_default[1]
      = { { omp_atk_fallback, omp_atv_null_fb } };
    omp_allocator_handle_t lowlat_default
      = omp_init_allocator (omp_low_lat_mem_space, 1, traits_default);

    void *a = omp_alloc(1, lowlat);	    // good
    void *b = omp_alloc(1, lowlat_all);     // bad
    void *c = omp_alloc(1, lowlat_default); // bad

    if (!a || b || c)
      __builtin_abort ();

    omp_free (a, lowlat);


    a = omp_calloc(1, 1, lowlat);	  // good
    b = omp_calloc(1, 1, lowlat_all);     // bad
    c = omp_calloc(1, 1, lowlat_default); // bad

    if (!a || b || c)
      __builtin_abort ();

    omp_free (a, lowlat);


    a = omp_realloc(NULL, 1, lowlat, lowlat);		      // good
    b = omp_realloc(NULL, 1, lowlat_all, lowlat_all);	      // bad
    c = omp_realloc(NULL, 1, lowlat_default, lowlat_default); // bad

    if (!a || b || c)
      __builtin_abort ();

    omp_free (a, lowlat);
  }

return 0;
}

