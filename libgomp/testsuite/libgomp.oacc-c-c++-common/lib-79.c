/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */
/* { dg-require-effective-target openacc_cuda } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include <cuda.h>
#include "timer.h"

int
main (int argc, char **argv)
{
  CUdevice dev;
  CUfunction delay;
  CUmodule module;
  CUresult r;
  int N;
  int i;
  CUstream stream;
  unsigned long *a, *d_a, dticks;
  int nbytes;
  float atime, dtime, hitime, lotime;
  void *kargs[2];
  int clkrate;
  int devnum, nprocs;

  devnum = 2;

  acc_init (acc_device_nvidia);

  devnum = acc_get_device_num (acc_device_nvidia);

  r = cuDeviceGet (&dev, devnum);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuDeviceGet failed: %d\n", r);
      abort ();
    }

  r =
    cuDeviceGetAttribute (&nprocs, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT,
			  dev);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuDeviceGetAttribute failed: %d\n", r);
      abort ();
    }

  r = cuDeviceGetAttribute (&clkrate, CU_DEVICE_ATTRIBUTE_CLOCK_RATE, dev);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuDeviceGetAttribute failed: %d\n", r);
      abort ();
    }

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

  nbytes = nprocs * sizeof (unsigned long);

  dtime = 200.0;

  dticks = (unsigned long) (dtime * clkrate);

  N = nprocs;

  a = (unsigned long *) malloc (nbytes);
  d_a = (unsigned long *) acc_malloc (nbytes);

  acc_map_data (a, d_a, nbytes);

  r = cuStreamCreate (&stream, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuStreamCreate failed: %d\n", r);
      abort ();
    }

  if (!acc_set_cuda_stream (1, stream))
    abort ();

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

  init_timers (1);

  kargs[0] = (void *) &d_a;
  kargs[1] = (void *) &dticks;

  start_timer (0);

  for (i = 0; i < N; i++)
    {
      r = cuLaunchKernel (delay, 1, 1, 1, 1, 1, 1, 0, stream, kargs, 0);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuLaunchKernel failed: %d\n", r);
	  abort ();
	}
    }

  if (acc_async_test (0) != 0)
    abort ();

  /* Test unseen async-argument.  */
  if (acc_async_test (1) != 1)
    abort ();

  acc_wait_async (0, 1);

  if (acc_async_test (0) != 0)
    abort ();

  if (acc_async_test (1) != 0)
    abort ();

  /* Test unseen async-argument.  */
  {
    if (acc_async_test (2) != 1)
      abort ();

    acc_wait_async (2, 1);

    if (acc_async_test (0) != 0)
      abort ();

    if (acc_async_test (1) != 0)
      abort ();

    if (acc_async_test (2) != 1)
      abort ();
  }

  acc_wait (1);

  atime = stop_timer (0);

  if (acc_async_test (0) != 1)
    abort ();

  if (acc_async_test (1) != 1)
    abort ();

  hitime = dtime * N;
  hitime += hitime * 0.02;

  lotime = dtime * N;
  lotime -= lotime * 0.02;

  if (atime > hitime || atime < lotime)
    {
      fprintf (stderr, "actual time < delay time\n");
      abort ();
    }

  acc_unmap_data (a);

  fini_timers ();

  free (a);
  acc_free (d_a);

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
