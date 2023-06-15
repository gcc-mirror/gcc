/* { dg-set-target-env-var OMP_ALLOCATOR "omp_large_cap_mem_alloc" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* { dg-output ".\\\[host\\\] OMP_ALLOCATOR = 'omp_large_cap_mem_alloc'.*" } */

#include <omp.h>

int
main ()
{
  omp_allocator_handle_t m = omp_get_default_allocator ();
  if (m != omp_large_cap_mem_alloc)
    __builtin_abort ();
  return 0;
}
