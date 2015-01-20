/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>
#include "timer.h"

int
main (int argc, char **argv)
{
  float atime;
  CUstream stream;
  CUresult r;

  acc_init (acc_device_nvidia);

  (void) acc_get_device_num (acc_device_nvidia);

  init_timers (1);

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

  start_timer (0);

  acc_wait_all_async (0);

  acc_wait (0);

  atime = stop_timer (0);

  if (0.010 < atime)
    {
      fprintf (stderr, "actual time too long\n");
      abort ();
    }

  fini_timers ();

  acc_shutdown (acc_device_nvidia);

  exit (0);
}

/* { dg-output "" } */
