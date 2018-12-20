/* Test no_create clause when data is present on the device.  */

#include <stdlib.h>
#include <stdio.h>
#include <openacc.h>

#define N 128

int
main (int argc, char *argv[])
{
  int *arr = (int *) malloc (N * sizeof (*arr));
  int *devptr;

  acc_copyin (arr, N * sizeof (*arr));

  #pragma acc parallel no_create(arr[0:N]) copyout(devptr)
  {
    devptr = &arr[2];
  }

#if !ACC_MEM_SHARED
  if (acc_hostptr (devptr) != (void *) &arr[2])
    __builtin_abort ();
#endif

  acc_delete (arr, N * sizeof (*arr));

#if ACC_MEM_SHARED
  if (&arr[2] != devptr)
    __builtin_abort ();
#else
  if (&arr[2] == devptr)
    __builtin_abort ();
#endif

  free (arr);

  return 0;
}
