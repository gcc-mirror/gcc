/* { dg-set-target-env-var OMP_ALLOCATOR "omp_default_mem_space:alignment=3" } */

/* { dg-output ".*libgomp: Allocator of environment variable OMP_ALLOCATOR cannot be created, using omp_default_mem_alloc instead.*" } */
/* OMP_ALLOCATOR's alignment is not power of 2 -> use omp_default_mem_alloc.  */

#include <omp.h>

int
main ()
{
  omp_allocator_handle_t m = omp_get_default_allocator ();
  if (m != omp_default_mem_alloc)
    __builtin_abort ();
  return 0;
}
