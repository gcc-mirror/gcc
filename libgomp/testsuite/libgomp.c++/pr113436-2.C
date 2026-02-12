/* PR middle-end/113436 */
/* { dg-do run } */

#include <omp.h>
#include <stdint.h>

void
test_int_by_ref ()
{
  int a = 5;
  int &b = a;

  #pragma omp target private(b) \
		     allocate(allocator(omp_high_bw_mem_alloc), align(64): b)
    {
      if (((uintptr_t) &b) % 64  != 0)
	__builtin_abort ();
      b = 7;
    }
}

int main ()
{
  test_int_by_ref ();
}
