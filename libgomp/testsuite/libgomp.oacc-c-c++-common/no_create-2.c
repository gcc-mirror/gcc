/* Test 'no_create' clause on compute construct, with data not present on the
   device.  */

#include <stdlib.h>
#include <stdio.h>

#define N 128

int
main (int argc, char *argv[])
{
  int var;
  int *arr = (int *) malloc (N * sizeof (*arr));
  int *devptr[2];

#pragma acc parallel no_create(var, arr[0:N]) copyout(devptr)
  {
    devptr[0] = &var;
    devptr[1] = &arr[2];
  }

  if (devptr[0] != &var)
    __builtin_abort ();
  if (devptr[1] != &arr[2])
    __builtin_abort ();

  free (arr);

  return 0;
}
