/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include <stdio.h>
#include <cuda.h>

#if !defined __cplusplus
# undef static_assert
# define static_assert _Static_assert
#endif

static_assert (acc_async_sync == -2, "acc_async_sync?");
static_assert (acc_async_noval == -1, "acc_async_noval?");

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

  streams = (CUstream *) malloc ((2 + N) * sizeof (void *));
  streams += 2;
  /* "streams[i]" is valid for i in [acc_async_sync..N).  */

  for (i = acc_async_sync; i < N; i++)
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

      int ret = acc_set_cuda_stream (i, streams[i]);
      if (i == acc_async_sync)
	{
	  if (ret == 1)
	    abort ();
	}
      else
	{
	  if (ret != 1)
	    abort ();
	}
    }

  s = NULL;

  if (acc_set_cuda_stream (N + 1, s) != 0)
    abort ();

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
