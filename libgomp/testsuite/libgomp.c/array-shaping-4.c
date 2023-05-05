// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <string.h>

#define N 10

int main ()
{
  int iarr[N * N];

  memset (iarr, 0, N * N * sizeof (int));

#pragma omp target enter data map(to: iarr)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	iarr[i * 10 + j] = i + j;
  }

  /* An array, but cast to a pointer, then reshaped.  */
#pragma omp target update from(([10][10]) ((int *) &iarr[0])[4:3][4:3])

  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      if (i >= 4 && i < 7 && j >= 4 && j < 7)
	assert (iarr[i * 10 + j] == i + j);
      else
	assert (iarr[i * 10 + j] == 0);

#pragma omp target exit data map(delete: iarr)

  return 0;
}
