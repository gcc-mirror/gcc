/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */
/* { dg-require-effective-target openacc_cuda } */

#include <sys/time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <openacc.h>
#include <cuda.h>

int
main (int argc, char **argv)
{
  CUfunction delay;
  CUmodule module;
  CUresult r;
  const int N = 6;
  int i;
  CUstream streams[N];

  acc_init (acc_device_nvidia);

  r = cuModuleLoad (&module, "subr.ptx");
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuModuleLoad failed: %d\n", r);
      abort ();
    }

  r = cuModuleGetFunction (&delay, module, "delay");
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuModuleGetFunction failed: %d\n", r);
      abort ();
    }

  for (i = 0; i < N; i++)
    {
      streams[i] = (CUstream) acc_get_cuda_stream (i);
      if (streams[i] != NULL)
	abort ();

      r = cuStreamCreate (&streams[i], CU_STREAM_DEFAULT);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuStreamCreate failed: %d\n", r);
	  abort ();
	}

        if (!acc_set_cuda_stream (i, streams[i]))
	  abort ();
    }

  for (i = 0; i < N; i++)
    {
      r = cuLaunchKernel (delay, 1, 1, 1, 1, 1, 1, 0, streams[i], NULL, 0);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuLaunchKernel failed: %d\n", r);
	  abort ();
	}
    }

  if (acc_async_test_all () != 0)
    {
      fprintf (stderr, "asynchronous operation not running\n");
      abort ();
    }

  r = cuCtxSynchronize ();
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuCtxSynchronize failed: %d\n", r);
      abort ();
    }

  if (acc_async_test_all () != 1)
    {
      fprintf (stderr, "asynchronous operation not running\n");
      abort ();
    }

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
