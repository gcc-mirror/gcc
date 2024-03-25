/* { dg-do run } */

#include <stdlib.h>
#include <assert.h>

#define N 1024
#define M 64

int main (void)
{
  int *a_orig[N];
  int *(&a)[N] = a_orig;

  for (int i = 0; i < N; i++)
    a[i] = (int *) calloc (M, sizeof (int));

  /* 'target enter data'/'target exit data' with array of pointers.  */
#pragma omp target enter data map(alloc: a[0:N])

  for (int i = 0; i < N; i++)
  {
#pragma omp target enter data map(to: a[i][0:M])
  }

#pragma omp target map(alloc: a)
  {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < M; j++)
	a[i][j] = i + j;
  }

for (int i = 0; i < N; i++)
  {
#pragma omp target exit data map(release: a[i]) map(from: a[i][0:M])
  }

#pragma omp target exit data map(release: a, a[0:N])

  /* 'target data' with array of pointers.  */
#pragma omp target data map(alloc: a[0:N])
  {
#pragma omp target data map(tofrom: a[5][0:M])
    {
#pragma omp target map(alloc: a)
      {
	for (int i = 0; i < M; i++)
	  a[5][i]++;
      }
    }
  }

  /* 'target' with array of pointers.  */
#pragma omp target data map(alloc: a[0:N])
  {
#pragma omp target map(tofrom: a[7][0:M])
    {
      for (int i = 0; i < M; i++)
	a[7][i] += 2;
    }
  }

  for (int i = 0; i < N; i++)
    for (int j = 0; j < M; j++)
      assert (a[i][j] == i + j + (i == 5) + 2 * (i == 7));

  for (int i = 0; i < N; i++)
    free (a[i]);

  return 0;
}
