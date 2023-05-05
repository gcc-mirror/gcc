// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

#define N 10

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

  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      if (i >= 4 && i < 7 && j >= 4 && j < 7)
	assert (iarrp[i * 10 + j] == i + j);
      else
	assert (iarrp[i * 10 + j] == 0);

#pragma omp target exit data map(delete: iarr_real)

  return 0;
}
