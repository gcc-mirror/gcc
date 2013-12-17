/* { dg-do run { target *-*-linux* *-*-gnu* *-*-freebsd* } } */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include <pthread.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

pthread_barrier_t bar;
omp_nest_lock_t lock;

void *tf (void *p)
{
  int l;
  if (p)
    {
      if (omp_test_nest_lock (&lock) != 1)
	abort ();
      if (omp_test_nest_lock (&lock) != 2)
	abort ();
    }
  pthread_barrier_wait (&bar);
  if (!p && omp_test_nest_lock (&lock) != 0)
    abort ();
  pthread_barrier_wait (&bar);
  if (p)
    {
      if (omp_test_nest_lock (&lock) != 3)
	abort ();
      omp_unset_nest_lock (&lock);
      omp_unset_nest_lock (&lock);
      omp_unset_nest_lock (&lock);
    }
  pthread_barrier_wait (&bar);
  if (!p)
    {
      if (omp_test_nest_lock (&lock) != 1)
	abort ();
      if (omp_test_nest_lock (&lock) != 2)
	abort ();
      omp_unset_nest_lock (&lock);
      omp_unset_nest_lock (&lock);
    }
  return NULL;
}

int
main (void)
{
  pthread_t th;
  omp_init_nest_lock (&lock);
  pthread_barrier_init (&bar, NULL, 2);
  pthread_create (&th, NULL, tf, NULL);
  tf ("");
  pthread_join (th, NULL);
  omp_destroy_nest_lock (&lock);
  return 0;
}
