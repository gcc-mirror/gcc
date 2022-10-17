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
  if (omp_aligned_alloc (64, 0, omp_null_allocator) != NULL)
    abort ();
  if (omp_calloc (0, 0, omp_null_allocator) != NULL
      || omp_calloc (32, 0, omp_null_allocator) != NULL
      || omp_calloc (0, 64, omp_null_allocator) != NULL)
    abort ();
  if (omp_aligned_calloc (32, 0, 0, omp_null_allocator) != NULL
      || omp_aligned_calloc (64, 32, 0, omp_null_allocator) != NULL
      || omp_aligned_calloc (16, 0, 64, omp_null_allocator) != NULL)
    abort ();
  a = omp_init_allocator (omp_default_mem_space, 2, traits);
  if (a != omp_null_allocator)
    {
      if (omp_alloc (0, a) != NULL
	  || omp_alloc (0, a) != NULL
	  || omp_alloc (0, a) != NULL
	  || omp_aligned_alloc (16, 0, a) != NULL
	  || omp_aligned_alloc (128, 0, a) != NULL
	  || omp_calloc (0, 0, a) != NULL
	  || omp_calloc (32, 0, a) != NULL
	  || omp_calloc (0, 64, a) != NULL
	  || omp_aligned_calloc (32, 0, 0, a) != NULL
	  || omp_aligned_calloc (64, 32, 0, a) != NULL
	  || omp_aligned_calloc (16, 0, 64, a) != NULL)
	abort ();
      omp_destroy_allocator (a);
    }
  return 0;
}
