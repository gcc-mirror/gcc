/* { dg-do run } */
/* { dg-require-effective-target omp_usm } */

#include <omp.h>

#pragma omp requires unified_shared_memory

int
main ()
{
  int *a = (int *) omp_alloc (sizeof (int), ompx_unified_shared_mem_alloc);
  if (!a)
    __builtin_abort ();

  for (int d = 0; d <= omp_get_num_devices (); d++)
    if (!omp_target_is_accessible (a, sizeof (int), d))
      __builtin_abort ();

  omp_free(a, ompx_unified_shared_mem_alloc);
  return 0;
}
