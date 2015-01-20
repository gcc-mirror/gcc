/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include <sys/time.h>
#include <stdio.h>
#include <cuda.h>

int
main (int argc, char **argv)
{
  const int N = 1024 * 1024;
  int i;
  unsigned char *h;
  void *d;
  float async, sync;
  struct timeval start, stop;
  CUresult r;
  CUstream s;

  acc_init (acc_device_nvidia);

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  d = acc_malloc (N);

  acc_map_data (h, d, N);

  gettimeofday (&start, NULL);

  for (i = 0; i < 100; i++)
    {
#pragma acc update device(h[0:N])
    }

  gettimeofday (&stop, NULL);

  sync = (float) (stop.tv_sec - start.tv_sec);
  sync += (float) ((stop.tv_usec - start.tv_usec) / 1000000.0);

  gettimeofday (&start, NULL);

  r = cuStreamCreate (&s, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuStreamCreate failed: %d\n", r);
	  abort ();
	}

  if (!acc_set_cuda_stream (0, s))
	  abort ();

  for (i = 0; i < 100; i++)
    {
#pragma acc update device(h[0:N]) async(0)
    }

  acc_wait_all ();

  gettimeofday (&stop, NULL);

  async = (float) (stop.tv_sec - start.tv_sec);
  async += (float) ((stop.tv_usec - start.tv_usec) / 1000000.0);

  if (async > (sync * 1.5))
    abort ();

  acc_free (d);

  free (h);

  acc_shutdown (acc_device_nvidia);

  return 0;
}

/* { dg-output "" } */
