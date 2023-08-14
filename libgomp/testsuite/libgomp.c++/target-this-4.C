// We use 'auto' without a function return type, so specify dialect here
// { dg-additional-options "-std=c++14" }
// { dg-do run { target offload_device_nonshared_as } }

#include <cstdlib>
#include <cstring>
#include <cstdint>

struct T
{
  int *ptr;
  int ptr_len;

  int *&refptr;
  int refptr_len;

  auto set_ptr_func (int n)
  {
    auto fn = [=](void) -> bool
      {
	bool mapped;
	uintptr_t hostptr = (uintptr_t) ptr;
	#pragma omp target map(from:mapped)
	{
	  if (ptr != (int *) hostptr)
	    for (int i = 0; i < ptr_len; i++)
	      ptr[i] = n;
	  mapped = (ptr != (int *) hostptr);
	}
	return mapped;
      };
    return fn;
  }

  auto set_refptr_func (int n)
  {
    auto fn = [=](void) -> bool
      {
	bool mapped;
	uintptr_t hostrefptr = (uintptr_t) refptr;
	#pragma omp target map(from:mapped)
	{
	  if (refptr != (int *) hostrefptr)
	    for (int i = 0; i < refptr_len; i++)
	      refptr[i] = n;
	  mapped = (refptr != (int *) hostrefptr);
	}
	return mapped;
      };
    return fn;
  }
};

int main (void)
{
  #define N 10
  int *ptr1 = new int[N];
  int *ptr2 = new int[N];

  memset (ptr1, 0, sizeof (int) * N);
  memset (ptr2, 0, sizeof (int) * N);

  T a = { ptr1, N, ptr2, N };

  auto p1 = a.set_ptr_func (1);
  auto r2 = a.set_refptr_func (2);

  if (p1 ())
    abort ();
  if (r2 ())
    abort ();

  if (a.ptr != ptr1)
    abort ();
  if (a.refptr != ptr2)
    abort ();

  for (int i = 0; i < N; i++)
    if (ptr1[i] != 0)
      abort ();

  for (int i = 0; i < N; i++)
    if (ptr2[i] != 0)
      abort ();

  #pragma omp target data map(ptr1[:N], ptr2[:N])
  {
    if (!p1 ())
      abort ();
    if (!r2 ())
      abort ();
  }

  if (a.ptr != ptr1)
    abort ();
  if (a.refptr != ptr2)
    abort ();

  for (int i = 0; i < N; i++)
    if (ptr1[i] != 1)
      abort ();

  for (int i = 0; i < N; i++)
    if (ptr2[i] != 2)
      abort ();

  return 0;
}
