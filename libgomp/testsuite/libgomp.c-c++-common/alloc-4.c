#include <omp.h>
#include <stdlib.h>

const omp_alloctrait_t traits[]
= { { omp_atk_pool_size, 1 },
    { omp_atk_fallback, omp_atv_abort_fb } };

int
main ()
{
  omp_allocator_handle_t a;

  if (omp_alloc (0, omp_null_allocator) != NULL)
    abort ();
  a = omp_init_allocator (omp_default_mem_space, 2, traits);
  if (a != omp_null_allocator)
    {
      if (omp_alloc (0, a) != NULL
	  || omp_alloc (0, a) != NULL
	  || omp_alloc (0, a) != NULL)
	abort ();
      omp_destroy_allocator (a);
    }
  return 0;
}
