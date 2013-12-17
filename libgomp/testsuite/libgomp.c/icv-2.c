/* { dg-do run { target *-*-linux* *-*-gnu* *-*-freebsd* } } */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include <pthread.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

pthread_barrier_t bar;

void *tf (void *p)
{
  int l;
  if (p)
    omp_set_num_threads (3);
  pthread_barrier_wait (&bar);
  if (!p)
    omp_set_num_threads (6);
  pthread_barrier_wait (&bar);
  omp_set_dynamic (0);
  if (omp_get_max_threads () != (p ? 3 : 6))
    abort ();
  l = 0;
  #pragma omp parallel num_threads (6) reduction (|:l)
    {
      l |= omp_get_max_threads () != (p ? 3 : 6);
      omp_set_num_threads ((p ? 3 : 6) + omp_get_thread_num ());
      l |= omp_get_max_threads () != ((p ? 3 : 6) + omp_get_thread_num ());
    }
  if (l)
    abort ();
  return NULL;
}

int
main (void)
{
  pthread_t th;
  pthread_barrier_init (&bar, NULL, 2);
  pthread_create (&th, NULL, tf, NULL);
  tf ("");
  pthread_join (th, NULL);
  return 0;
}
