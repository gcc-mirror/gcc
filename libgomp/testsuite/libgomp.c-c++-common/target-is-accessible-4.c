/* { dg-require-effective-target omp_managedmem } */

#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

//#define __builtin_abort() __builtin_printf ("fail at line %d\n", __LINE__)

int
main ()
{
  int *p = (int*)omp_alloc (sizeof (int), ompx_gnu_managed_mem_alloc);

  *p = 42;
  uintptr_t a_p = (uintptr_t)p;

  #pragma omp target is_device_ptr(p)
    {
      if (*p != 42 || a_p != (uintptr_t)p)
	__builtin_abort ();
    }
  if (!p
      || !omp_target_is_accessible (p, sizeof (int),
				    omp_get_default_device ()))
    __builtin_abort ();

  return 0;
}
