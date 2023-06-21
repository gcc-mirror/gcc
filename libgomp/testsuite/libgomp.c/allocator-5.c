/* { dg-set-target-env-var OMP_ALLOCATOR "omp_const_mem_space:access=none,pinned=false" } */

/* { dg-output ".*libgomp: Invalid value for environment variable OMP_ALLOCATOR when parsing: none,pinned=false.*" } */
/* OMP_ALLOCATOR syntax error -> use omp_default_mem_alloc.  */

#include <omp.h>

int
main ()
{
  omp_allocator_handle_t m = omp_get_default_allocator ();
  if (m != omp_default_mem_alloc)
    __builtin_abort ();
  return 0;
}
