/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include <cuda.h>
#include <sys/time.h>

int
main (int argc, char **argv)
{
  CUstream stream;
  CUresult r;
  struct timeval tv1, tv2;
  time_t t1;

  acc_init (acc_device_nvidia);

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

  gettimeofday (&tv1, NULL);

  acc_wait_all_async (0);

  acc_wait (0);

  gettimeofday (&tv2, NULL);

  t1 = ((tv2.tv_sec - tv1.tv_sec) * 1000000) + (tv2.tv_usec - tv1.tv_usec);

  if (t1 > 1000)
    {
      fprintf (stderr, "too long\n");
      abort ();
    }

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
