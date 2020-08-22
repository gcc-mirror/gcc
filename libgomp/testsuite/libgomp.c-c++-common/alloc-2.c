#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

int
main ()
{
  omp_alloctrait_t traits[3]
    = { { omp_atk_alignment, 64 },
	{ omp_atk_fallback, omp_atv_null_fb },
	{ omp_atk_pool_size, 4096 } };
  omp_allocator_handle_t a
    = omp_init_allocator (omp_default_mem_space, 3, traits);
  if (a == omp_null_allocator)
    abort ();

  #pragma omp parallel num_threads(4)
  {
    int n = omp_get_thread_num ();
    double *volatile p, *volatile q;
    omp_set_default_allocator ((n & 1) ? a : omp_default_mem_alloc);
    p = (double *) omp_alloc (1696, omp_null_allocator);
    if (p == NULL)
      abort ();
    p[0] = 1.0;
    p[1695 / sizeof (double)] = 2.0;
    #pragma omp barrier
    omp_set_default_allocator ((n & 1) ? omp_default_mem_alloc : a);
    q = (double *) omp_alloc (1696, omp_null_allocator);
    if (n & 1)
      {
	if (q == NULL)
	  abort ();
	q[0] = 3.0;
	q[1695 / sizeof (double)] = 4.0;
      }
    else if (q != NULL)
      abort ();
    #pragma omp barrier
    omp_free (p, omp_null_allocator);
    omp_free (q, omp_null_allocator);
    omp_set_default_allocator (omp_default_mem_alloc);
  }
  omp_destroy_allocator (a);
  return 0;
}
