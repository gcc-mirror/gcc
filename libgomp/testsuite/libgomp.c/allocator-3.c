/* { dg-set-target-env-var OMP_ALLOCATOR "  omp_default_mem_space:alignment=512,pinned=false,access=all  " } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* We copied the environment string; hence, it may contain white space.  */
/* { dg-output ".\\\[host\\\] OMP_ALLOCATOR = '  omp_default_mem_space:alignment=512,pinned=false,access=all  '.*" } */

#include <stdint.h>
#include <omp.h>

int
main ()
{
  int *a, *b;
  a = omp_alloc (sizeof (int) * 1024, omp_null_allocator);

  omp_allocator_handle_t m = omp_get_default_allocator ();
  b = omp_alloc (sizeof (int) * 1024, m);

  if ((uintptr_t) a % 512 != 0)
    __builtin_abort ();

  if ((uintptr_t) b % 512 != 0)
    __builtin_abort ();
  omp_free (a, omp_null_allocator);
  omp_free (b, m);
  return 0;
}
