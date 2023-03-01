// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

#define N 10

template<typename T>
void foo ()
{
  T tarr_real[N * N];
  T (&tarr)[N * N] = tarr_real;

  memset (tarr, 0, N * N * sizeof (T));

#pragma omp target enter data map(to: tarr)

#pragma omp target
  {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
	tarr[i * N + j] = 2 * (i + j);
  }

  /* A ref to an array, but cast to a pointer, then reshaped.  */
#pragma omp target update from(([N][N]) ((T *) &tarr[0])[4:3][5:3])

  for (int i = 4; i < 7; i++)
    for (int j = 5; j < 8; j++)
      assert (tarr[i * N + j] == 2 * (i + j));

#pragma omp target exit data map(delete: tarr)
}

int main ()
{
  int iarr_real[N * N];
  int (&iarr)[N * N] = iarr_real;

  memset (iarr, 0, N * N * sizeof (int));

#pragma omp target enter data map(to: iarr)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	iarr[i * 10 + j] = i + j;
  }

  /* A ref to an array, but cast to a pointer, then reshaped.  */
#pragma omp target update from(([10][10]) ((int *) &iarr[0])[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (iarr[i * 10 + j] == i + j);

#pragma omp target exit data map(delete: iarr)

  foo<unsigned short> ();

  return 0;
}
