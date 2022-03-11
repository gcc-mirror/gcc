/* { dg-do run } */

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

#pragma omp target enter data map(to:a[0:2])

#pragma omp target
    {
      if (a[0] != 42 || a_p != (uintptr_t)a)
	__builtin_abort ();
    }

#pragma omp target
    {
      if (a[1] != 43 || a_p != (uintptr_t)a)
	__builtin_abort ();
    }

#pragma omp target exit data map(delete:a[0:2])

  omp_free(a, ompx_unified_shared_mem_alloc);
  return 0;
}
