/* { dg-do run } */

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define N 16

/* NOTE: This test is the same as array-of-struct-2.c, except the fields of
   this struct are in a different order.  */

struct Z {
  int arr[N];
  int *ptr;
  int c;
};

void
foo (struct Z *zarr, int len)
{
#pragma omp target map(to:zarr, zarr[5].ptr) map(tofrom:zarr[5].ptr[0:len])
  {
    for (int i = 0; i < len; i++)
      zarr[5].ptr[i]++;
  }

#pragma omp target map(to:zarr) map(tofrom:zarr[4].arr[0:len])
  {
    for (int i = 0; i < len; i++)
      zarr[4].arr[i]++;
  }

#pragma omp target map (to:zarr[3].ptr) map(tofrom:zarr[3].ptr[0:len])
  {
    for (int i = 0; i < len; i++)
      zarr[3].ptr[i]++;
  }

#pragma omp target map(tofrom:zarr[2].arr[0:len])
  {
    for (int i = 0; i < len; i++)
      zarr[2].arr[i]++;
  }
}

int main (int argc, char *argv[])
{
  struct Z zs[10];
  memset (zs, 0, sizeof zs);

  for (int i = 0; i < 10; i++)
    zs[i].ptr = (int *) calloc (N, sizeof (int));

  foo (zs, N);

  for (int i = 0; i < N; i++)
    {
      assert (zs[2].arr[i] == 1);
      assert (zs[4].arr[i] == 1);
      assert (zs[3].ptr[i] == 1);
      assert (zs[5].ptr[i] == 1);
    }

  return 0;
}
