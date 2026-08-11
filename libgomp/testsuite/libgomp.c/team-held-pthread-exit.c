/* Verify cleanup of a retained team when a foreign initial thread exits.
   The pthread first leaves workers held at a multi-threaded final barrier and
   then runs a one-thread parallel region, which leaves that older barrier in
   the thread pool.  The pthread TLS destructor of `gomp_free_thread` must
   release the retained tree barrier, join all workers, and free the pool
   before pthread_join returns.  Repetition also checks that cleanup does not
   leak state into a later foreign initial thread.  */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include <omp.h>
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>

enum { threads = 4, rounds = 12 };

static void *
run_regions (void *argument)
{
  int participants = 0;

  omp_set_dynamic (0);
#pragma omp parallel num_threads(threads) shared(participants)
  {
#pragma omp atomic update
    participants++;
  }
  if (participants != threads)
    abort ();

#pragma omp parallel num_threads(1)
  {
    if (omp_get_num_threads () != 1 || omp_get_thread_num () != 0)
      abort ();
  }

  return argument;
}

int
main (void)
{
  for (intptr_t round = 1; round <= rounds; ++round)
    {
      pthread_t driver;
      void *argument = (void *) round;
      void *result;

      if (pthread_create (&driver, NULL, run_regions, argument) != 0)
	abort ();
      if (pthread_join (driver, &result) != 0 || result != argument)
	abort ();
    }

  return 0;
}
