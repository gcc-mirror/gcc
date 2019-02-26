/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */
/* { dg-require-effective-target openacc_cuda } */

#include <stdio.h>
#include <unistd.h>
#include <openacc.h>
#include <cuda.h>

int
main (int argc, char **argv)
{
  CUfunction delay;
  CUmodule module;
  CUresult r;
  CUstream stream;

  acc_init (acc_device_nvidia);

  r = cuModuleLoad (&module, "./subr.ptx");
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

  stream = (CUstream) acc_get_cuda_stream (0);
  if (stream != NULL)
    abort ();

  r = cuStreamCreate (&stream, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuStreamCreate failed: %d\n", r);
      abort ();
    }

  if (!acc_set_cuda_stream (0, stream))
    abort ();

  r = cuLaunchKernel (delay, 1, 1, 1, 1, 1, 1, 0, stream, NULL, 0);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuLaunchKernel failed: %d\n", r);
      abort ();
    }

  if (acc_async_test (0) != 0)
    {
      fprintf (stderr, "asynchronous operation not running\n");
      abort ();
    }

  /* Test unseen async-argument.  */
  if (acc_async_test (1) != 1)
    {
      fprintf (stderr, "acc_async_test failed on unseen async-argument\n");
      abort ();
    }

  sleep (1);

  if (acc_async_test (0) != 1)
    {
      fprintf (stderr, "found asynchronous operation still running\n");
      abort ();
    }

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
