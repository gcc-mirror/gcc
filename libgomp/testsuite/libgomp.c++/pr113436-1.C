/* PR middle-end/113436 */
/* { dg-do run } */

#include <omp.h>
#include <stdint.h>

void
test_int_by_ref ()
{
  int a = 5;
  int &b = a;

  #pragma omp target firstprivate(b) \
		     allocate(allocator(omp_high_bw_mem_alloc), align(64): b)
    {
      if (((uintptr_t) &b) % 64  != 0)
	__builtin_abort ();
      b *= 7;
      if (b != 35)
	__builtin_abort ();
    }
}

void
test_vla_by_ref (int n)
{
  int x[n];
  for (int i = 0; i < n; i++)
    x[i] = i;
  int (&y)[n] = x;

  #pragma omp target firstprivate(y) \
		     allocate(allocator(omp_low_lat_mem_alloc), align(128): y)
    {
      if (((uintptr_t) &y) % 128  != 0)
	__builtin_abort ();
      for (int i = 0; i < n; i++)
	y[i]++;
      for (int i = 0; i < n; i++)
	if (y[i] != i + 1)
	  __builtin_abort ();
    }
}

int main ()
{
  test_int_by_ref ();
  test_vla_by_ref (17);
}
