/* { dg-do run } */

/* Test that low-latency realloc and free chains are sound.  */

#include <stddef.h>
#include <omp.h>

#pragma omp requires dynamic_allocators

void
check (int cond, const char *msg)
{
  if (!cond)
    {
      __builtin_printf ("%s\n", msg);
      __builtin_abort ();
    }
}

int
main ()
{
  if (omp_get_initial_device () == omp_get_default_device ())
    return 0;  /* This test isn't interesting with host-fallback.  */

  #pragma omp target
  {
    /* Ensure that the memory we get *is* low-latency with a null-fallback.  */
    omp_alloctrait_t traits[2]
      = { { omp_atk_fallback, omp_atv_null_fb },
          { omp_atk_access, omp_atv_cgroup } };
    omp_allocator_handle_t lowlat = omp_init_allocator (omp_low_lat_mem_space,
							2, traits);

    int size = 16;

    char *a = (char *) omp_alloc (size, lowlat);
    char *b = (char *) omp_alloc (size, lowlat);
    char *c = (char *) omp_alloc (size, lowlat);
    char *d = (char *) omp_alloc (size, lowlat);

    /* There are headers and padding to account for.  */
    int size2 = size + (b-a);
    int size3 = size + (c-a);
    int size4 = size + (d-a) + 100; /* Random larger amount.  */

    check (a != NULL && b != NULL && c != NULL && d != NULL,
	   "omp_alloc returned NULL\n");

    char *p = omp_realloc (b, size, lowlat, lowlat);
    check (p == b, "realloc did not reuse same size chunk, no space after");

    p = omp_realloc (b, size-8, lowlat, lowlat);
    check (p == b, "realloc did not reuse smaller chunk, no space after");

    p = omp_realloc (b, size, lowlat, lowlat);
    check (p == b, "realloc did not reuse original size chunk, no space after");

    /* Make space after b.  */
    omp_free (c, lowlat);

    p = omp_realloc (b, size, lowlat, lowlat);
    check (p == b, "realloc did not reuse same size chunk");

    p = omp_realloc (b, size-8, lowlat, lowlat);
    check (p == b, "realloc did not reuse smaller chunk");

    p = omp_realloc (b, size, lowlat, lowlat);
    check (p == b, "realloc did not reuse original size chunk");

    p = omp_realloc (b, size+8, lowlat, lowlat);
    check (p == b, "realloc did not extend in place by a little");

    p = omp_realloc (b, size2, lowlat, lowlat);
    check (p == b, "realloc did not extend into whole next chunk");

    p = omp_realloc (b, size3, lowlat, lowlat);
    check (p != b, "realloc did not move b elsewhere");
    omp_free (p, lowlat);


    p = omp_realloc (a, size, lowlat, lowlat);
    check (p == a, "realloc did not reuse same size chunk, first position");

    p = omp_realloc (a, size-8, lowlat, lowlat);
    check (p == a, "realloc did not reuse smaller chunk, first position");

    p = omp_realloc (a, size, lowlat, lowlat);
    check (p == a, "realloc did not reuse original size chunk, first position");

    p = omp_realloc (a, size+8, lowlat, lowlat);
    check (p == a, "realloc did not extend in place by a little, first position");

    p = omp_realloc (a, size3, lowlat, lowlat);
    check (p == a, "realloc did not extend into whole next chunk, first position");

    p = omp_realloc (a, size4, lowlat, lowlat);
    check (p != a, "realloc did not move a elsewhere, first position");
    omp_free (p, lowlat);


    p = omp_realloc (d, size, lowlat, lowlat);
    check (p == d, "realloc did not reuse same size chunk, last position");

    p = omp_realloc (d, size-8, lowlat, lowlat);
    check (p == d, "realloc did not reuse smaller chunk, last position");

    p = omp_realloc (d, size, lowlat, lowlat);
    check (p == d, "realloc did not reuse original size chunk, last position");

    p = omp_realloc (d, size+8, lowlat, lowlat);
    check (p == d, "realloc did not extend in place by d little, last position");

    /* Larger than low latency memory.  */
    p = omp_realloc (d, 100000000, lowlat, lowlat);
    check (p == NULL, "realloc did not fail on OOM");
  }

  return 0;
}

