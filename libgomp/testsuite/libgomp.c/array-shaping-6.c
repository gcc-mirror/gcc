// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define N 10

int main ()
{
  int *iptr = calloc (N * N * N, sizeof (int));

#pragma omp target enter data map(to: iptr[0:N*N*N])

#pragma omp target
  {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
	iptr[i * N * N + 4 * N + j] = i + j;
  }

  /* An array ref between two array sections.  */
#pragma omp target update from(([N][N][N]) iptr[2:3][4][6:3])

  for (int i = 2; i < 5; i++)
    for (int j = 6; j < 9; j++)
      assert (iptr[i * N * N + 4 * N + j] == i + j);

  memset (iptr, 0, N * N * N * sizeof (int));

  for (int i = 0; i < N; i++)
    iptr[2 * N * N + i * N + 4] = 3 * i;

  /* Array section between two array refs.  */
#pragma omp target update to(([N][N][N]) iptr[2][3:6][4])

#pragma omp target exit data map(from: iptr[0:N*N*N])

  for (int i = 3; i < 9; i++)
    assert (iptr[2 * N * N + i * N + 4] == 3 * i);

  free (iptr);

  return 0;
}
