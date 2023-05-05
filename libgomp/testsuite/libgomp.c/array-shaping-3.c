// { dg-do run { target offload_device_nonshared_as } }

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define N 10

typedef struct {
  int arr[N][N];
} B;

int main()
{
  B *bvar = malloc (sizeof (B));

  memset (bvar, 0, sizeof (B));

#pragma omp target enter data map(to: bvar->arr)

#pragma omp target
  {
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
	bvar->arr[i][j] = i + j;
  }

#pragma omp target update from(bvar->arr[4:3][4:3])

  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      if (i >= 4 && i < 7 && j >= 4 && j < 7)
	assert (bvar->arr[i][j] == i + j);
      else
	assert (bvar->arr[i][j] == 0);

#pragma omp target exit data map(delete: bvar->arr)

  free (bvar);

  return 0;
}
