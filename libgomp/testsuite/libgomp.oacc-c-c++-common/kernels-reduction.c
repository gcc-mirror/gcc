#include <stdlib.h>

#define n 10000

unsigned int a[n];

void  __attribute__((noinline,noclone))
foo (void)
{
  int i;
  unsigned int sum = 1;

#pragma acc kernels copyin (a[0:n]) copy (sum)
  {
    for (i = 0; i < n; ++i)
      sum += a[i];
  }

  if (sum != 5001)
    abort ();
}

int
main ()
{
  int i;

  for (i = 0; i < n; ++i)
    a[i] = i % 2;

  foo ();

  return 0;
}
