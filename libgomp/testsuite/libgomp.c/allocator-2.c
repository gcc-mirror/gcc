/* { dg-set-target-env-var OMP_ALLOCATOR "omp_large_cap_mem_space" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* Expect omp_large_cap_mem_alloc as allocator for omp_large_cap_mem_space. */
/* { dg-output ".\\\[host\\\] OMP_ALLOCATOR = 'omp_large_cap_mem_alloc'.*" } */
#include <omp.h>

int
main ()
{
  omp_allocator_handle_t m = omp_get_default_allocator ();
  /* Without traits, omp_large_cap_mem_space implies
     omp_large_cap_mem_alloc.  */
  if (m != omp_large_cap_mem_alloc)
    __builtin_abort ();
  return 0;
}
