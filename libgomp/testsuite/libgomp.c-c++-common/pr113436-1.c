/* PR middle-end/113436 */
/* { dg-do run } */

#include <omp.h>
#include <stdint.h>

void
test_int_by_val ()
{
  int x = 64;

  #pragma omp target firstprivate(x) \
		     allocate(allocator(omp_high_bw_mem_alloc), align(16): x)
    {
      if (((uintptr_t) &x) % 16  != 0)
	__builtin_abort ();
      x *= 2;
      if (x != 128)
	__builtin_abort ();
    }
}

void
test_struct_by_val ()
{
  struct S {
    int a[4];
    float b[4];
  } s = { { 1, 2, 3, 4 }, { 5.0f, 6.0f, 7.0f, 8.0f } };

  #pragma omp target firstprivate(s) \
		     allocate(allocator(omp_low_lat_mem_alloc), align(32): s)
    {
      if (((uintptr_t) &s) % 32  != 0)
	__builtin_abort ();
      for (int i = 0; i < 4; i++)
	{
	  s.a[i] *= 2;
	  s.b[i] *= 2.0f;
	}
      for (int i = 0; i < 4; i++)
	if (s.a[i] != (i + 1) * 2 || s.b[i] != (i + 5) * 2.0f)
	  __builtin_abort ();
    }
}

void
test_ptr ()
{
  int x = 42;
  int *p = &x;
  uintptr_t p_orig = (uintptr_t) p;
  uintptr_t p_new;

  #pragma omp target firstprivate(p) \
		     allocate(allocator(omp_default_mem_alloc), align(16): p) \
		     map(from: p_new)
    {
      if (((uintptr_t) &p) % 16  != 0)
	__builtin_abort ();
      p_new = (uintptr_t) p;
    }

  if (p_new != p_orig)
      __builtin_abort ();
}

void
test_vla (int n)
{
  int x[n];
  for (int i = 0; i < n; i++)
    x[i] = i;

  #pragma omp target firstprivate(x) \
		     allocate(allocator(omp_high_bw_mem_alloc), align(128): x)
    {
      if (((uintptr_t) &x) % 128  != 0)
	__builtin_abort ();
      for (int i = 0; i < n; i++)
	x[i]++;
      for (int i = 0; i < n; i++)
	if (x[i] != i + 1)
	  __builtin_abort ();
    }
}

int main ()
{
  test_int_by_val ();
  test_struct_by_val ();
  test_ptr ();
  test_vla (16);
}
