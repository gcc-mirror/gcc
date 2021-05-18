/* { dg-do run { target *-*-linux* *-*-gnu* *-*-freebsd* } } */
/* { dg-timeout 10 } */

/* Test that omp_fulfill_event works when called from an external
   non-OpenMP thread.  */

#include <omp.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>

int finished = 0;
int event_pending = 0;
omp_event_handle_t detach_event;

void *
fulfill_thread (void *)
{
  while (!__atomic_load_n (&finished, __ATOMIC_RELAXED))
    {
      if (__atomic_load_n (&event_pending, __ATOMIC_ACQUIRE))
	{
	  omp_fulfill_event (detach_event);
	  __atomic_store_n (&event_pending, 0, __ATOMIC_RELEASE);
	}

      sleep(1);
    }

  return 0;
}

int
main (void)
{
  pthread_t thr;
  int dep;
  pthread_create (&thr, NULL, fulfill_thread, 0);

  #pragma omp parallel
    #pragma omp single
    {
      omp_event_handle_t ev;

      #pragma omp task depend (out: dep) detach (ev)
      {
	detach_event = ev;
	__atomic_store_n (&event_pending, 1, __ATOMIC_RELEASE);
      }

      #pragma omp task depend (in: dep)
      {
	__atomic_store_n (&finished, 1, __ATOMIC_RELAXED);
      }
    }

  pthread_join (thr, 0);
  return 0;
}
