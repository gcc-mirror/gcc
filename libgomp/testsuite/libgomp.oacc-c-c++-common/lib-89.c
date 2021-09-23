/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <openacc.h>

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

  devnum = acc_get_device_num (acc_device_default);
  acc_set_device_num (devnum, acc_device_default);

#if ACC_DEVICE_TYPE_nvidia
  if (acc_get_current_cuda_context () == NULL)
    abort ();
#else
  if (acc_get_current_cuda_context () != NULL)
    abort ();
#endif

  p = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      p[i] = tid;
    }

  x[tid] = p;

  d_x[tid] = acc_copyin (p, N);

  return 0;
}

int
main (int argc, char **argv)
{
  int i;
  pthread_attr_t attr;
  pthread_t *tid;

  acc_init (acc_device_default);

  x = (unsigned char **) malloc (NTHREADS * N);
  d_x = (void **) malloc (NTHREADS * N);

  if (pthread_attr_init (&attr) != 0)
    perror ("pthread_attr_init failed");

  tid = (pthread_t *) malloc (NTHREADS * sizeof (pthread_t));

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

  for (i = 0; i < NTHREADS; i++)
    {
      memset (x[i], 0, N);
      acc_copyout (x[i], N);
    }

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

  return 0;
}

/* { dg-output "" } */
