/* { dg-do run } */
/* { dg-require-effective-target omp_usm } */
/* { dg-additional-options -foffload-options=amdgcn-amdhsa=-mxnack=on { target offload_target_amdgcn } } */

#include <omp.h>
#include <stdint.h>

int
main ()
{
  int *a = (int *) omp_alloc(sizeof(int)*2, ompx_unified_shared_mem_alloc);
  if (!a)
    __builtin_abort ();

  a[0] = 42;
  a[1] = 43;

  uintptr_t a_p = (uintptr_t)a;

  #pragma omp target map(a[0])
    {
      if (a[0] != 42 || a_p != (uintptr_t)a)
	__builtin_abort ();
    }

  #pragma omp target map(a[1])
    {
      if (a[1] != 43 || a_p != (uintptr_t)a)
	__builtin_abort ();
    }

  omp_free(a, ompx_unified_shared_mem_alloc);
  return 0;
}
