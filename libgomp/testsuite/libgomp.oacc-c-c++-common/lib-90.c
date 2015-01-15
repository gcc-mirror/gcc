/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda" } */

#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <ctype.h>
#include <openacc.h>
#include <cuda.h>

unsigned char **x;
void **d_x;
const int N = 16;
const int NTHREADS = 32;

static void *
test (void *arg)
{
  int i;
  int tid;
  unsigned char *p;
  int devnum;

  tid = (int) (long) arg;

  devnum = acc_get_device_num (acc_device_nvidia);
  acc_set_device_num (devnum, acc_device_nvidia);

  if (acc_get_current_cuda_context () == NULL)
    abort ();

  p = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      p[i] = tid;
    }

  x[tid] = p;

  d_x[tid] = acc_copyin (p, N);

  acc_wait_all ();

  return 0;
}

int
main (int argc, char **argv)
{
  int i;
  pthread_attr_t attr;
  pthread_t *tid;
  CUresult r;
  CUstream s;

  acc_init (acc_device_nvidia);

  x = (unsigned char **) malloc (NTHREADS * N);
  d_x = (void **) malloc (NTHREADS * N);

  if (pthread_attr_init (&attr) != 0)
    perror ("pthread_attr_init failed");

  tid = (pthread_t *) malloc (NTHREADS * sizeof (pthread_t));

  r = cuStreamCreate (&s, CU_STREAM_DEFAULT);
  if (r != CUDA_SUCCESS)
	{
	  fprintf (stderr, "cuStreamCreate failed: %d\n", r);
	  abort ();
	}

  if (!acc_set_cuda_stream (0, s))
	  abort ();

  for (i = 0; i < NTHREADS; i++)
    {
      if (pthread_create (&tid[i], &attr, &test, (void *) (unsigned long) (i))
	  != 0)
	perror ("pthread_create failed");
    }

  if (pthread_attr_destroy (&attr) != 0)
    perror ("pthread_attr_destroy failed");

  for (i = 0; i < NTHREADS; i++)
    {
      void *res;

      if (pthread_join (tid[i], &res) != 0)
	perror ("pthread join failed");
    }


  for (i = 0; i < NTHREADS; i++)
    {
      if (acc_is_present (x[i], N) != 1)
	abort ();
    }

  acc_get_cuda_stream (1);

  for (i = 0; i < NTHREADS; i++)
    {
      memset (x[i], 0, N);
      acc_copyout (x[i], N);
    }

  acc_wait_all ();

  for (i = 0; i < NTHREADS; i++)
    {
      unsigned char *p;
      int j;

      p = x[i];

      for (j = 0; j < N; j++)
	{
	  if (p[j] != i)
	    abort ();
	}

      if (acc_is_present (x[i], N) != 0)
	abort ();
    }

  acc_shutdown (acc_device_nvidia);

  return 0;
}

/* { dg-output "" } */
