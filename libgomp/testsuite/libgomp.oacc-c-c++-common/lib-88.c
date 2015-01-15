/* { dg-do run } */

#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <openacc.h>

unsigned char *x;
void *d_x;
const int N = 256;

static void *
test (void *arg)
{
  int i;

  if (acc_get_current_cuda_context () != NULL)
    abort ();

  if (acc_is_present (x, N) != 1)
    abort ();

  memset (x, 0, N);

  acc_copyout (x, N);

  for (i = 0; i < N; i++)
    {
      if (x[i] != i)
	abort ();

      x[i] = N - i - 1;
    }

  d_x = acc_copyin (x, N);

  return 0;
}

int
main (int argc, char **argv)
{
  const int nthreads = 1;
  int i;
  pthread_attr_t attr;
  pthread_t *tid;

  if (acc_get_num_devices (acc_device_nvidia) == 0)
    return 0;

  acc_init (acc_device_nvidia);

  x = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      x[i] = i;
    }

  d_x = acc_copyin (x, N);

  if (acc_is_present (x, N) != 1)
    abort ();

  if (pthread_attr_init (&attr) != 0)
    perror ("pthread_attr_init failed");

  tid = (pthread_t *) malloc (nthreads * sizeof (pthread_t));

  for (i = 0; i < nthreads; i++)
    {
      if (pthread_create (&tid[i], &attr, &test, (void *) (unsigned long) (i))
	  != 0)
	perror ("pthread_create failed");
    }

  if (pthread_attr_destroy (&attr) != 0)
    perror ("pthread_attr_destroy failed");

  for (i = 0; i < nthreads; i++)
    {
      void *res;

      if (pthread_join (tid[i], &res) != 0)
	perror ("pthread join failed");
    }

  if (acc_is_present (x, N) != 1)
    abort ();

  memset (x, 0, N);

  acc_copyout (x, N);

  for (i = 0; i < N; i++)
    {
      if (x[i] != N - i - 1)
	abort ();
    }

  if (acc_is_present (x, N) != 0)
    abort ();

  acc_shutdown (acc_device_nvidia);

  return 0;
}

/* { dg-output "" } */
