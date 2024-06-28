/* { dg-do run } */
/* { dg-require-effective-target omp_managedmem } */

/* Check that omp_realloc can allocate Managed Memory, and that host and target
   can see the data, at the same address, without a mapping.  */

#include <omp.h>
#include <stdint.h>

int
main ()
{
  int *a = (int *) omp_alloc(2 * sizeof(int), ompx_gnu_managed_mem_alloc);
  if (!a)
    __builtin_abort ();

  a[0] = 42;
  a[1] = 43;

  /* Reallocate to larger size */
  int *b = (int *) omp_realloc(a, 5 * sizeof(int), ompx_gnu_managed_mem_alloc,
			       ompx_gnu_managed_mem_alloc);
  if (!b)
    __builtin_abort ();

  /* Check that original data is preserved */
  if (b[0] != 42 || b[1] != 43)
    __builtin_abort ();

  b[2] = 44;
  b[3] = 45;
  b[4] = 46;
  uintptr_t b_p = (uintptr_t)b;

  #pragma omp target is_device_ptr(b)
    {
      if (b[0] != 42 || b[1] != 43 || b[2] != 44 || b[3] != 45 || b[4] != 46
	  || b_p != (uintptr_t)b)
	__builtin_abort ();
    }

  omp_free(b, ompx_gnu_managed_mem_alloc);
  return 0;
}
