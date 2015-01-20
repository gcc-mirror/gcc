/* { dg-do run } */

#include <stdlib.h>

int main (void)
{
  void *a, *a_1, *a_2;

#define A (void *) 0x123
  a = A;

#pragma acc data copyout (a_1, a_2)
#pragma acc kernels deviceptr (a)
  {
    a_1 = a;
    a_2 = &a;
  }

  if (a != A)
    abort ();
  if (a_1 != a)
    abort ();
#if ACC_MEM_SHARED
  if (a_2 != &a)
    abort ();
#else
  if (a_2 == &a)
    abort ();
#endif

  return 0;
}
