/* PR middle-end/113436 */
/* { dg-do run } */

#include <omp.h>
#include <stdint.h>

void
test_int_by_val ()
{
  int x;

  #pragma omp target private(x) \
		     allocate(allocator(omp_high_bw_mem_alloc), align(16): x)
    {
      if (((uintptr_t) &x) % 16  != 0)
	__builtin_abort ();
      x = 2;
    }
}

void
test_struct_by_val ()
{
  struct S {
    int a[4];
    float b[4];
  } s = { { 1, 2, 3, 4 }, { 5.0f, 6.0f, 7.0f, 8.0f } };

  #pragma omp target private(s) \
		     allocate(allocator(omp_low_lat_mem_alloc), align(32): s)
    {
      if (((uintptr_t) &s) % 32  != 0)
	__builtin_abort ();
      for (int i = 0; i < 4; i++)
	{
	  s.a[i] = i + 1;
	  s.b[i] = 2.0f * i;
	}
    }
}

void
test_ptr ()
{
  int x = 42;
  int *p = &x;

  #pragma omp target firstprivate(p) \
		     allocate(allocator(omp_default_mem_alloc), align(16): p)
    {
      if (((uintptr_t) &p) % 16  != 0)
	__builtin_abort ();
      p++;
    }
}

void
test_vla (int n)
{
  int x[n];
  for (int i = 0; i < n; i++)
    x[i] = i;

  #pragma omp target private(x) \
		     allocate(allocator(omp_high_bw_mem_alloc), align(128): x)
    {
      if (((uintptr_t) &x) % 128  != 0)
	__builtin_abort ();
      for (int i = 0; i < n; i++)
	x[i] = i * 2;
    }
}

int main ()
{
  test_int_by_val ();
  test_struct_by_val ();
  test_ptr ();
  test_vla (32);
}
