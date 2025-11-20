/* { dg-do run } */
/* { dg-require-effective-target omp_managedmem } */
/* { dg-additional-options -foffload-options=amdgcn-amdhsa=-mxnack=on { target offload_target_amdgcn_with_xnack } } */

/* Check that omp_calloc can allocate Managed Memory, and that host and target
   can see the data, at the same address, without a mapping.  */

#include <omp.h>
#include <stdint.h>

int
main ()
{
  int *a = (int *) omp_calloc(5, sizeof(int), ompx_gnu_managed_mem_alloc);
  if (!a)
    __builtin_abort ();

  /* Check that memory is zero-initialized */
  for (int i = 0; i < 5; i++)
    if (a[i] != 0)
      __builtin_abort ();

  a[0] = 42;
  a[4] = 99;
  uintptr_t a_p = (uintptr_t)a;

  #pragma omp target is_device_ptr(a)
    {
      if (a[0] != 42 || a[4] != 99 || a_p != (uintptr_t)a)
	__builtin_abort ();
      /* Check zero-initialization on device side */
      for (int i = 1; i < 4; i++)
	if (a[i] != 0)
	  __builtin_abort ();
    }

  omp_free(a, ompx_gnu_managed_mem_alloc);
  return 0;
}
