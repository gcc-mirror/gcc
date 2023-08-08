#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef struct {
  int *ptr;
} S;

int main(void)
{
#pragma omp declare mapper(grid: S x) map(([9][11]) x.ptr[3:3:2][1:4:3])
  S q;
  q.ptr = (int *) calloc (9 * 11, sizeof (int));

#pragma omp target enter data map(to: q.ptr, q.ptr[0:9*11])

#pragma omp target
  for (int i = 0; i < 9*11; i++)
    q.ptr[i] = i;

#pragma omp target update from(mapper(grid): q)

  for (int j = 0; j < 9; j++)
    for (int i = 0; i < 11; i++)
      if (j >= 3 && j <= 7 && ((j - 3) % 2) == 0
	  && i >= 1 && i <= 10 && ((i - 1) % 3) == 0)
	assert (q.ptr[j * 11 + i] == j * 11 + i);
      else
	assert (q.ptr[j * 11 + i] == 0);

#pragma omp target exit data map(release: q.ptr, q.ptr[0:9*11])
  return 0;
}
