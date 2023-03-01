// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

#define N 10

template<typename T>
void foo ()
{
  T tarr_real[N * N];
  T *tarrp = &tarr_real[0];
  T **tarrpp = &tarrp;

  memset (tarrp, 0, N * N * sizeof (T));

#pragma omp target enter data map(to: tarr_real)

#pragma omp target
  {
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
	tarrp[i * N + j] = 2 * (i + j);
  }

  /* A pointer with an extra indirection.  */
#pragma omp target update from(([N][N]) (*tarrpp)[4:3][5:3])

  for (int i = 4; i < 7; i++)
    for (int j = 5; j < 8; j++)
      assert (tarrp[i * N + j] == 2 * (i + j));

#pragma omp target exit data map(delete: tarr_real)
}

int main ()
{
  int iarr_real[N * N];
  int *iarrp = &iarr_real[0];
  int **iarrpp = &iarrp;

  memset (iarrp, 0, N * N * sizeof (int));

#pragma omp target enter data map(to: iarr_real)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	iarrp[i * 10 + j] = i + j;
  }

  /* A pointer with an extra indirection.  */
#pragma omp target update from(([10][10]) (*iarrpp)[4:3][4:3])

  for (int i = 4; i < 7; i++)
    for (int j = 4; j < 7; j++)
      assert (iarrp[i * 10 + j] == i + j);

#pragma omp target exit data map(delete: iarr_real)

  foo<unsigned short> ();

  return 0;
}
