// { dg-do run { target offload_device_nonshared_as } }

#include <stdio.h>
#include <string.h>
#include <stdint.h>
extern "C" void abort ();

struct S
{
  int * ptr;
  int ptr_len;

  int *&refptr;
  int refptr_len;

  bool set_ptr (int n)
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
  }

  bool set_refptr (int n)
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
  }
};

int main (void)
{
  #define N 10
  int *ptr1 = new int[N];
  int *ptr2 = new int[N];

  memset (ptr1, 0, sizeof (int) * N);
  memset (ptr2, 0, sizeof (int) * N);

  S s = { ptr1, N, ptr2, N };

  bool mapped;
  int val = 123;

  mapped = s.set_ptr (val);
  if (mapped)
    abort ();
  if (s.ptr != ptr1)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr1[i] != 0)
      abort ();

  mapped = s.set_refptr (val);
  if (mapped)
    abort ();
  if (s.refptr != ptr2)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr2[i] != 0)
      abort ();

  #pragma omp target data map(ptr1[:N])
  mapped = s.set_ptr (val);

  if (!mapped)
    abort ();
  if (s.set_refptr (0))
    abort ();
  if (s.ptr != ptr1 || s.refptr != ptr2)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr1[i] != val)
      abort ();

  #pragma omp target data map(ptr2[:N])
  mapped = s.set_refptr (val);

  if (!mapped)
    abort ();
  if (s.set_ptr (0))
    abort ();
  if (s.ptr != ptr1 || s.refptr != ptr2)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr2[i] != val)
      abort ();

  return 0;
}
