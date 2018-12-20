/* Test no_create clause when data is not present on the device.  */

#include <stdlib.h>
#include <stdio.h>

#define N 128

int
main (int argc, char *argv[])
{
  int *arr = (int *) malloc (N * sizeof (*arr));
  int *devptr;

  #pragma acc data no_create(arr[0:N])
  {
    #pragma acc parallel copyout(devptr)
    {
      devptr = &arr[2];
    }
  }

  if (devptr != &arr[2])
    __builtin_abort ();

  free (arr);

  return 0;
}
