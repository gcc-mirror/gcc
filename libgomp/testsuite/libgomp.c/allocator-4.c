/* { dg-set-target-env-var OMP_ALLOCATOR "omp_const_mem_space:alignment=3,pinned=" } */

/* { dg-output ".*libgomp: Missing value at the end of environment variable OMP_ALLOCATOR=omp_const_mem_space:alignment=3,pinned=.*" } */
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
