/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <openacc.h>
#include <cuda.h>

int
main (int argc, char **argv)
{
  const int N = 100;
  int i;
  CUstream *streams;
  CUstream s;
  CUresult r;

  acc_init (acc_device_nvidia);

  (void) acc_get_device_num (acc_device_nvidia);

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

  for (i = 0; i < N; i++)
    {
      int j;
      int cnt;

      cnt = 0;

      s = streams[i];

      for (j = 0; j < N; j++)
	{
	  if (s == streams[j])
	    cnt++;
	}

      if (cnt != 1)
	abort ();
    }

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
