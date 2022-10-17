#include <omp.h>
#include <stdlib.h>
#include <stdint.h>

int zero;

omp_allocator_handle_t
allocator (omp_allocator_handle_t h)
{
  if (zero)
    return h;
  else
    abort ();
}

omp_allocator_handle_t
align (int a)
{
  if (zero)
    return omp_default_mem_alloc;
  else
    abort ();
}

int
main ()
{
  int x = 1, y = 2;
  #pragma omp parallel num_threads(2) firstprivate (x, y) allocate (allocator (omp_default_mem_alloc) : x) allocate (align (16) : y)
  {
    if (x != 1 || y != 2)
      abort ();
    if ((((uintptr_t) &y) & 15) != 0)
      abort ();
  }
  return 0;
}
