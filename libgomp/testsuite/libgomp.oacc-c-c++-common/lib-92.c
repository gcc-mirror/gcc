/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <openacc.h>

unsigned char **x;
void **d_x;
const int N = 32;
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

  acc_copyout (x[tid], N);

  p = x[tid];

  for (i = 0; i < N; i++)
    {
      if (p[i] != i)
	abort ();
    }

  return 0;
}

int
main (int argc, char **argv)
{
  int i;
  pthread_attr_t attr;
  pthread_t *tid;
  unsigned char *p;

  acc_init (acc_device_default);

  x = (unsigned char **) malloc (NTHREADS * N);
  d_x = (void **) malloc (NTHREADS * N);

  for (i = 0; i < N; i++)
    {
      int j;

      p = (unsigned char *) malloc (N);

      x[i] = p;

      for (j = 0; j < N; j++)
	{
	  p[j] = j;
	}

      d_x[i] = acc_copyin (p, N);
    }

  if (pthread_attr_init (&attr) != 0)
    perror ("pthread_attr_init failed");

  tid = (pthread_t *) malloc (NTHREADS * sizeof (pthread_t));

  acc_get_cuda_stream (1);

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
      if (acc_is_present (x[i], N) != 0)
	abort ();
    }

  return 0;
}

/* { dg-output "" } */
