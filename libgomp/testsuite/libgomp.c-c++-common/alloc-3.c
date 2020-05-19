/* { dg-set-target-env-var OMP_ALLOCATOR "omp_cgroup_mem_alloc" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

#include <string.h>
#include <stdlib.h>
#include <omp.h>

int
main ()
{
  const char *p = getenv ("OMP_ALLOCATOR");
  if (p && strcmp (p, "omp_cgroup_mem_alloc") == 0)
    {
      if (omp_get_default_allocator () != omp_cgroup_mem_alloc)
	abort ();
      #pragma omp parallel num_threads (2)
      {
	if (omp_get_default_allocator () != omp_cgroup_mem_alloc)
	  abort ();
	#pragma omp parallel num_threads (2)
	{
	  if (omp_get_default_allocator () != omp_cgroup_mem_alloc)
	    abort ();
	}
      }
    }
  return 0;
}
