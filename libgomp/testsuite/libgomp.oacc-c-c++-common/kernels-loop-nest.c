#include <stdlib.h>

#define N 1000

int
main (void)
{
  int x[N][N];

#pragma acc kernels copyout (x)
  {
    for (int ii = 0; ii < N; ii++)
      for (int jj = 0; jj < N; jj++)
	x[ii][jj] = ii + jj + 3;
  }

  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      if (x[i][j] != i + j + 3)
	abort ();

  return 0;
}
