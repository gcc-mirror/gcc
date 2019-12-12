/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <openacc.h>
#include <stdlib.h>
#include "cuda.h"

#include <stdio.h>

#define n 128

int
main (void)
{
  CUresult r;
  CUstream stream1;
  int N = n;
  int a[n];
  int c[n];

  acc_init (acc_device_nvidia);

  r = cuStreamCreate (&stream1, CU_STREAM_NON_BLOCKING);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuStreamCreate failed: %d\n", r);
      abort ();
    }

  acc_set_cuda_stream (1, stream1);

  for (int i = 0; i < n; i++)
    {
      a[i] = 3;
      c[i] = 0;
    }

#pragma acc data copy (a, c) copyin (N)
  {
#pragma acc parallel async (1)
    ;

#pragma acc parallel async (1) num_gangs (320)
    #pragma acc loop gang
    for (int ii = 0; ii < N; ii++)
      c[ii] = (a[ii] + a[N - ii - 1]);

#pragma acc parallel async (1)
    #pragma acc loop seq
    for (int ii = 0; ii < n; ii++)
      a[ii] = 6;

#pragma acc wait (1)
  }

  for (int i = 0; i < n; i++)
    if (c[i] != 6)
      abort ();

  return 0;
}
