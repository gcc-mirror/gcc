#include <stdlib.h>

#define N 32

unsigned int
foo (int n, unsigned int *a)
{
  int r;
#pragma acc kernels copyout(r) copy (a[0:N])
  {
    r = a[0];

    for (int i = 0; i < n; i++)
      a[i] = 1;
  }

  return r;
}

int
main (void)
{
  unsigned int a[N];
  unsigned res, i;

  for (i = 0; i < N; ++i)
    a[i] = i % 4;

  res = foo (N, a);
  if (res != 0)
    abort ();

  return 0;
}
