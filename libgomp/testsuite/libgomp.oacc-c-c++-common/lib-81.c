/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */
/* { dg-require-effective-target openacc_cuda } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include <cuda.h>
#include <sys/time.h>

int
main (int argc, char **argv)
{
  CUfunction delay;
  CUmodule module;
  CUresult r;
  const int N = 2;
  int i;
  CUstream *streams, stream;
  struct timeval tv1, tv2;
  time_t t1, t2;

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

  gettimeofday (&tv1, NULL);

  r = cuLaunchKernel (delay, 1, 1, 1, 1, 1, 1, 0, NULL, NULL, 0);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuLaunchKernel failed: %d\n", r);
	abort ();
    }

  r = cuCtxSynchronize ();
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuCtxSynchronize failed: %d\n", r);
	abort ();
    }

  gettimeofday (&tv2, NULL);

  t1 = ((tv2.tv_sec - tv1.tv_sec) * 1000000) + (tv2.tv_usec - tv1.tv_usec);

  streams = (CUstream *) malloc (N * sizeof (void *));

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

  stream = (CUstream) acc_get_cuda_stream (N);
  if (stream != NULL)
    abort ();

  r = cuStreamCreate (&stream, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuStreamCreate failed: %d\n", r);
      abort ();
    }

  if (!acc_set_cuda_stream (N, stream))
    abort ();

  gettimeofday (&tv1, NULL);

  for (i = 0; i < N; i++)
    {
      r = cuLaunchKernel (delay, 1, 1, 1, 1, 1, 1, 0, streams[i], NULL, 0);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuLaunchKernel failed: %d\n", r);
	  abort ();
	}
    }

  gettimeofday (&tv2, NULL);

  t2 = ((tv2.tv_sec - tv1.tv_sec) * 1000000) + (tv2.tv_usec - tv1.tv_usec);

  acc_wait_all_async (N);

  for (i = 0; i <= N; i++)
    {
      if (acc_async_test (i) != 0)
	abort ();
    }

  acc_wait (N);

  for (i = 0; i <= N; i++)
    {
      if (acc_async_test (i) != 1)
	abort ();
    }

  if ((t1 * N) < t2)
    {
      fprintf (stderr, "too long 1\n");
      abort ();
    }

  gettimeofday (&tv1, NULL);

  stream = (CUstream) acc_get_cuda_stream (N + 1);
  if (stream != NULL)
    abort ();

  r = cuStreamCreate (&stream, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuStreamCreate failed: %d\n", r);
      abort ();
    }

  if (!acc_set_cuda_stream (N + 1, stream))
    abort ();

  acc_wait_all_async (N + 1);

  acc_wait (N + 1);

  gettimeofday (&tv2, NULL);

  t1 = ((tv2.tv_sec - tv1.tv_sec) * 1000000) + (tv2.tv_usec - tv1.tv_usec);

  if (t1 > 1000)
    {
      fprintf (stderr, "too long 2\n");
      abort ();
    }

  gettimeofday (&tv1, NULL);

  acc_wait_all_async (N);

  acc_wait (N);

  gettimeofday (&tv2, NULL);

  t1 = ((tv2.tv_sec - tv1.tv_sec) * 1000000) + (tv2.tv_usec - tv1.tv_usec);

  if (t1 > 1000)
    {
      fprintf (stderr, "too long 3\n");
      abort ();
    }

  free (streams);

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
