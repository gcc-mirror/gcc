#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

const omp_alloctrait_t traits[]
= { { omp_atk_alignment, 16 },
    { omp_atk_sync_hint, omp_atv_default },
    { omp_atk_access, omp_atv_default },
    { omp_atk_fallback, omp_atv_default_mem_fb },
    { omp_atk_partition, omp_atv_environment } };

int
main ()
{
  omp_allocator_handle_t a;
  void *p, *q;
  volatile size_t large_sz;

  a = omp_init_allocator (omp_default_mem_space,
			  sizeof (traits) / sizeof (traits[0]),
			  traits);
  if (a == omp_null_allocator)
    abort ();
  p = omp_alloc (2048, a);
  if ((((uintptr_t) p) % 16) != 0)
    abort ();
  large_sz = ~(size_t) 1023;
  q = omp_alloc (large_sz, a);
  if (q != NULL)
    abort ();
  q = omp_aligned_alloc (32, large_sz, a);
  if (q != NULL)
    abort ();
  q = omp_calloc (large_sz / 4, 4, a);
  if (q != NULL)
    abort ();
  q = omp_aligned_calloc (1, 2, large_sz / 2, a);
  if (q != NULL)
    abort ();
  omp_free (p, a);
  large_sz = ~(size_t) 0;
  large_sz >>= 1;
  large_sz += 1;
  if (omp_calloc (2, large_sz, a) != NULL)
    abort ();
  if (omp_calloc (large_sz, 1024, a) != NULL)
    abort ();
  if (omp_calloc (large_sz, large_sz, a) != NULL)
    abort ();
  if (omp_aligned_calloc (16, 2, large_sz, a) != NULL)
    abort ();
  if (omp_aligned_calloc (32, large_sz, 1024, a) != NULL)
    abort ();
  if (omp_aligned_calloc (64, large_sz, large_sz, a) != NULL)
    abort ();
  omp_destroy_allocator (a);
  return 0;
}
