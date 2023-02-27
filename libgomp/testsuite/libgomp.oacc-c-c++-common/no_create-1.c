/* Test 'no_create' clause on compute construct, with data present on the
   device.  */

#include <stdlib.h>
#include <stdio.h>
#include <openacc.h>

#define N 128

int
main (int argc, char *argv[])
{
  int var;
  int *arr = (int *) malloc (N * sizeof (*arr));
  int *devptr[2];

  acc_copyin (&var, sizeof (var));
  acc_copyin (arr, N * sizeof (*arr));

#pragma acc parallel no_create(var, arr[0:N]) copyout(devptr)
  {
    devptr[0] = &var;
    devptr[1] = &arr[2];
  }
  if (acc_hostptr (devptr[0]) != (void *) &var)
    __builtin_abort ();
  if (acc_hostptr (devptr[1]) != (void *) &arr[2])
    __builtin_abort ();
#if ACC_MEM_SHARED
  if (devptr[0] != &var)
    __builtin_abort ();
  if (devptr[1] != &arr[2])
    __builtin_abort ();
#else
  if (devptr[0] == &var)
    __builtin_abort ();
  if (devptr[1] == &arr[2])
    __builtin_abort ();
#endif

#pragma acc parallel no_create(var, arr[0:N]) copyout(devptr) async
  {
    devptr[0] = &arr[N - 2];
    devptr[1] = &var;
  }
#pragma acc wait
  if (acc_hostptr (devptr[0]) != (void *) &arr[N - 2])
    __builtin_abort ();
  if (acc_hostptr (devptr[1]) != (void *) &var)
    __builtin_abort ();
#if ACC_MEM_SHARED
  if (devptr[0] != &arr[N - 2])
    __builtin_abort ();
  if (devptr[1] != &var)
    __builtin_abort ();
#else
  if (devptr[0] == &arr[N - 2])
    __builtin_abort ();
  if (devptr[1] == &var)
    __builtin_abort ();
#endif

  acc_delete (&var, sizeof (var));
  acc_delete (arr, N * sizeof (*arr));

  free (arr);

  return 0;
}
