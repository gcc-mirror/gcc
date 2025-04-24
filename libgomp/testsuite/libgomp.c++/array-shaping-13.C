// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

#define N 10

template<typename T>
void foo ()
{
  T *tptr = new T[N * N * N];

  memset (tptr, 0, N * N * N * sizeof (T));

#pragma omp target enter data map(to: tptr[0:N*N*N])

#pragma omp target
  {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
	tptr[i * N * N + 4 * N + j] = 2 * (i + j);
  }

  /* An array ref between two array sections.  */
#pragma omp target update from(([N][N][N]) tptr[4:3][4][5:3])

  for (int i = 4; i < 7; i++)
    for (int j = 5; j < 8; j++)
      assert (tptr[i * N * N + 4 * N + j] == 2 * (i + j));

  memset (tptr, 0, N * N * N * sizeof (T));

  for (int i = 0; i < N; i++)
    tptr[2 * N * N + i * N + 4] = 4 * i;

  /* Array section between two array refs.  */
#pragma omp target update to(([N][N][N]) tptr[2][3:6][4])

#pragma omp target exit data map(from: tptr[0:N*N*N])

  for (int i = 3; i < 9; i++)
    assert (tptr[2 * N * N + i * N + 4] == 4 * i);

#pragma omp target exit data map(delete: tptr[0:N*N*N])

  delete[] tptr;
}

int main ()
{
  int *iptr = new int[N * N * N];

  memset (iptr, 0, N * N * N * sizeof (int));

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

  delete[] iptr;

  foo<unsigned long> ();

  return 0;
}
