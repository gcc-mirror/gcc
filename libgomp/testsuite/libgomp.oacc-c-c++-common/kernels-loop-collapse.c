#include <stdlib.h>

#define N 100

int a[N][N];

void __attribute__((noinline, noclone))
foo (int m, int n)
{
  int i, j;
  #pragma acc kernels
  {
#pragma acc loop collapse(2)
    for (i = 0; i < m; i++)
      for (j = 0; j < n; j++)
	a[i][j] = 1;
  }
}

int
main (void)
{
  int i, j;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[i][j] = 0;

  foo (N, N);

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (a[i][j] != 1)
	abort ();

  return 0;
}
