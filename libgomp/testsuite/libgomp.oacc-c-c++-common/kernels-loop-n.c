#include <stdlib.h>

#define N ((1024 * 512) + 1)
#define COUNTERTYPE unsigned int

static int __attribute__((noinline,noclone))
foo (COUNTERTYPE n)
{
  unsigned int *__restrict a;
  unsigned int *__restrict b;
  unsigned int *__restrict c;

  a = (unsigned int *__restrict)malloc (n * sizeof (unsigned int));
  b = (unsigned int *__restrict)malloc (n * sizeof (unsigned int));
  c = (unsigned int *__restrict)malloc (n * sizeof (unsigned int));

  for (COUNTERTYPE i = 0; i < n; i++)
    a[i] = i * 2;

  for (COUNTERTYPE i = 0; i < n; i++)
    b[i] = i * 4;

#pragma acc kernels copyin (a[0:n], b[0:n]) copyout (c[0:n])
  {
    for (COUNTERTYPE ii = 0; ii < n; ii++)
      c[ii] = a[ii] + b[ii];
  }

  for (COUNTERTYPE i = 0; i < n; i++)
    if (c[i] != a[i] + b[i])
      abort ();

  free (a);
  free (b);
  free (c);

  return 0;
}

int
main (void)
{
  return foo (N);
}
