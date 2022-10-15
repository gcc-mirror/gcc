/* { dg-do run } */
/* { dg-require-effective-target omp_usm } */
/* { dg-options "-foffload=amdgcn-amdhsa=-mxnack=on" { target offload_target_amdgcn } } */

#include <omp.h>
#include <stdint.h>

int
main ()
{
  int *a = (int *) omp_alloc(sizeof(int), ompx_unified_shared_mem_alloc);
  if (!a)
    __builtin_abort ();

  *a = 42;
  uintptr_t a_p = (uintptr_t)a;

  #pragma omp target is_device_ptr(a)
    {
      if (*a != 42 || a_p != (uintptr_t)a)
	__builtin_abort ();
    }

  omp_free(a, ompx_unified_shared_mem_alloc);
  return 0;
}
