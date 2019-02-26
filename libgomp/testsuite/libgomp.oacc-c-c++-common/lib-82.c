/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */
/* { dg-require-effective-target openacc_cuda } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include <cuda.h>

int
main (int argc, char **argv)
{
  CUfunction delay2;
  CUmodule module;
  CUresult r;
  const int N = 32;
  int i;
  CUstream *streams;
  unsigned long **a, **d_a, *tid;
  int nbytes;
  void *kargs[2];

  acc_init (acc_device_nvidia);

  r = cuModuleLoad (&module, "subr.ptx");
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuModuleLoad failed: %d\n", r);
      abort ();
    }

  r = cuModuleGetFunction (&delay2, module, "delay2");
  if (r != CUDA_SUCCESS)
    {
      fprintf (stderr, "cuModuleGetFunction failed: %d\n", r);
      abort ();
    }

  nbytes = sizeof (int);

  streams = (CUstream *) malloc (N * sizeof (void *));

  a = (unsigned long **) malloc (N * sizeof (unsigned long *));
  d_a = (unsigned long **) malloc (N * sizeof (unsigned long *));
  tid = (unsigned long *) malloc (N * sizeof (unsigned long));

  for (i = 0; i < N; i++)
    {
      a[i] = (unsigned long *) malloc (sizeof (unsigned long));
      *a[i] = N;
      d_a[i] = (unsigned long *) acc_malloc (nbytes);
      tid[i] = i;

      acc_map_data (a[i], d_a[i], nbytes);

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
      kargs[0] = (void *) &d_a[i];
      kargs[1] = (void *) &tid[i];

      r = cuLaunchKernel (delay2, 1, 1, 1, 1, 1, 1, 0, streams[i], kargs, 0);
      if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuLaunchKernel failed: %d\n", r);
	  abort ();
	}
    }

  acc_wait_all_async (0);

  for (i = 0; i < N; i++)
    {
      acc_memcpy_from_device (a[i], d_a[i], nbytes);
      if (*a[i] != i)
	abort ();

      acc_unmap_data (a[i]);

      acc_free (d_a[i]);
    }

  free (streams);

  for (i = 0; i < N; i++)
    {
      free (a[i]);
    }

  free (a);
  free (d_a);
  free (tid);

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
