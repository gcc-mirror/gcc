/* { dg-do run } */

/* Test that low-latency free chains are sound.  */

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

    int size = 4;

    char *a = omp_alloc (size, lowlat);
    char *b = omp_alloc (size, lowlat);
    char *c = omp_alloc (size, lowlat);
    char *d = omp_alloc (size, lowlat);

    /* There are headers and padding to account for.  */
    int size2 = size + (b-a);
    int size3 = size + (c-a);
    int size4 = size + (d-a) + 100; /* Random larger amount.  */

    check (a != NULL && b != NULL && c != NULL && d != NULL,
	   "omp_alloc returned NULL\n");

    omp_free (a, lowlat);
    char *p = omp_alloc (size, lowlat);
    check (p == a, "allocate did not reuse first chunk");

    omp_free (b, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not reuse second chunk");

    omp_free (c, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not reuse third chunk");

    omp_free (a, lowlat);
    omp_free (b, lowlat);
    p = omp_alloc (size2, lowlat);
    check (p == a, "allocate did not coalesce first two chunks");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == a, "allocate did not split first chunk (1)");
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split first chunk (2)");

    omp_free (b, lowlat);
    omp_free (c, lowlat);
    p = omp_alloc (size2, lowlat);
    check (p == b, "allocate did not coalesce middle two chunks");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split second chunk (1)");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split second chunk (2)");

    omp_free (b, lowlat);
    omp_free (a, lowlat);
    p = omp_alloc (size2, lowlat);
    check (p == a, "allocate did not coalesce first two chunks, reverse free");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == a, "allocate did not split first chunk (1), reverse free");
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split first chunk (2), reverse free");

    omp_free (c, lowlat);
    omp_free (b, lowlat);
    p = omp_alloc (size2, lowlat);
    check (p == b, "allocate did not coalesce second two chunks, reverse free");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split second chunk (1), reverse free");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split second chunk (2), reverse free");

    omp_free (a, lowlat);
    omp_free (b, lowlat);
    omp_free (c, lowlat);
    p = omp_alloc (size3, lowlat);
    check (p == a, "allocate did not coalesce first three chunks");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == a, "allocate did not split first chunk (1)");
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split first chunk (2)");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split first chunk (3)");

    omp_free (b, lowlat);
    omp_free (c, lowlat);
    omp_free (d, lowlat);
    p = omp_alloc (size3, lowlat);
    check (p == b, "allocate did not coalesce last three chunks");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split second chunk (1)");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split second chunk (2)");
    p = omp_alloc (size, lowlat);
    check (p == d, "allocate did not split second chunk (3)");

    omp_free (c, lowlat);
    omp_free (b, lowlat);
    omp_free (a, lowlat);
    p = omp_alloc (size3, lowlat);
    check (p == a, "allocate did not coalesce first three chunks, reverse free");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == a, "allocate did not split first chunk (1), reverse free");
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split first chunk (2), reverse free");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split first chunk (3), reverse free");

    omp_free (d, lowlat);
    omp_free (c, lowlat);
    omp_free (b, lowlat);
    p = omp_alloc (size3, lowlat);
    check (p == b, "allocate did not coalesce second three chunks, reverse free");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split second chunk (1), reverse free");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split second chunk (2), reverse free");
    p = omp_alloc (size, lowlat);
    check (p == d, "allocate did not split second chunk (3), reverse free");

    omp_free (c, lowlat);
    omp_free (a, lowlat);
    omp_free (b, lowlat);
    p = omp_alloc (size3, lowlat);
    check (p == a, "allocate did not coalesce first three chunks, mixed free");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == a, "allocate did not split first chunk (1), mixed free");
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split first chunk (2), mixed free");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split first chunk (3), mixed free");

    omp_free (d, lowlat);
    omp_free (b, lowlat);
    omp_free (c, lowlat);
    p = omp_alloc (size3, lowlat);
    check (p == b, "allocate did not coalesce second three chunks, mixed free");

    omp_free (p, lowlat);
    p = omp_alloc (size, lowlat);
    check (p == b, "allocate did not split second chunk (1), mixed free");
    p = omp_alloc (size, lowlat);
    check (p == c, "allocate did not split second chunk (2), mixed free");
    p = omp_alloc (size, lowlat);
    check (p == d, "allocate did not split second chunk (3), mixed free");

    omp_free (a, lowlat);
    omp_free (b, lowlat);
    omp_free (c, lowlat);
    omp_free (d, lowlat);
    p = omp_alloc (size4, lowlat);
    check (p == a, "allocate did not coalesce all memory");
  }

  return 0;
}

