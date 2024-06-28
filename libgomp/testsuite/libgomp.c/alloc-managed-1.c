/* { dg-do run } */
/* { dg-require-effective-target omp_managedmem } */

/* Check that omp_alloc can allocate Managed Memory, and that host and target
   can see the data, at the same address, without a mapping.  */

#include <omp.h>
#include <stdint.h>

int
main ()
{
  int *a = (int *) omp_alloc(sizeof(int), ompx_gnu_managed_mem_alloc);
  if (!a)
    __builtin_abort ();

  *a = 42;
  uintptr_t a_p = (uintptr_t)a;

  #pragma omp target is_device_ptr(a)
    {
      if (*a != 42 || a_p != (uintptr_t)a)
	__builtin_abort ();
    }

  omp_free(a, ompx_gnu_managed_mem_alloc);
  return 0;
}
