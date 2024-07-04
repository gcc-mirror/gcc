/* { dg-do run } */

#include <stdlib.h>
#include <assert.h>

int main (void)
{
  int **a,i,j,n;

  j = 3;
  n = 12;

  a = (int **) calloc (32, sizeof (int *));
  for (int x = 0; x < 32; x++)
    a[x] = (int *) calloc (32, sizeof (int));

  for (int i = 2; i < 32; i++)
    {
      #pragma omp target enter data map(a, a[2:30])
      #pragma omp target enter data map(a[i][j:n])
      #pragma omp target map(alloc: a)
      {
	for (int x = j; x < j + n; x++)
	  a[i][x]++;
      }
      #pragma omp target exit data map(a[i][j:n])

      #pragma omp target data map(a, a[i][j:n])
      {
	#pragma omp target map(alloc: a)
	{
	  for (int x = j; x < j + n; x++)
	    a[i][x]++;
	}
      }
      #pragma omp target exit data map(a, a[2:30])

      #pragma omp target data map(a, a[0:32])
      {
	#pragma omp target map(a[i][j:n])
	{
	  for (int x = j; x < j + n; x++)
	    a[i][x]++;
	}
      }
    }

  for (int i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      if (i >= 2 && j >= 3 && j < 15)
	assert (a[i][j] == 3);
      else
	assert (a[i][j] == 0);

  return 0;
}
