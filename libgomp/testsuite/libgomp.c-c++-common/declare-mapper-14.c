/* { dg-do run } */

#include <stdlib.h>
#include <assert.h>

struct Z {
  int *arr;
};

void baz (struct Z *zarr, int len)
{
#pragma omp declare mapper (struct Z myvar) map(to: myvar.arr) \
					    map(tofrom: myvar.arr[0:len])
  zarr[0].arr = (int *) calloc (len, sizeof (int));
  zarr[5].arr = (int *) calloc (len, sizeof (int));

#pragma omp target map(zarr, *zarr)
  {
    for (int i = 0; i < len; i++)
      zarr[0].arr[i]++;
  }

#pragma omp target map(zarr, zarr[5])
  {
    for (int i = 0; i < len; i++)
      zarr[5].arr[i]++;
  }

#pragma omp target map(zarr[5])
  {
    for (int i = 0; i < len; i++)
      zarr[5].arr[i]++;
  }

#pragma omp target map(zarr, zarr[5:1])
  {
    for (int i = 0; i < len; i++)
      zarr[5].arr[i]++;
  }

  for (int i = 0; i < len; i++)
    assert (zarr[0].arr[i] == 1);

  for (int i = 0; i < len; i++)
    assert (zarr[5].arr[i] == 3);

  free (zarr[5].arr);
  free (zarr[0].arr);
}

int
main (int argc, char *argv[])
{
  struct Z myzarr[10];
  baz (myzarr, 256);
  return 0;
}
