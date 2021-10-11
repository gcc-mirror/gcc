#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

const omp_alloctrait_t traits[]
= { { omp_atk_alignment, 64 },
    { omp_atk_sync_hint, omp_atv_serialized },
    { omp_atk_fallback, omp_atv_null_fb } };

int
main ()
{
  omp_allocator_handle_t a;
  int *volatile p;
  a = omp_init_allocator (omp_default_mem_space, 3, traits);
  if (a == omp_null_allocator)
    abort ();
  p = (int *) omp_alloc (3072, a);
  if ((((uintptr_t) p) % 64) != 0)
    abort ();
  p[0] = 1;
  p[3071 / sizeof (int)] = 2;
  omp_free (p, a);
  omp_destroy_allocator (a);
}
