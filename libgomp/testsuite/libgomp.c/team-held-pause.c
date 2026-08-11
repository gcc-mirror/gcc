/* Verify that a hard pause releases workers retained at a previous final
   barrier even when a one-thread parallel region has since replaced the
   current team.  The serial region does not need the retained workers, so
   omp_pause_resource_all must find the older prev_barrier, release it, and
   join the complete worker pool without hanging or using freed team state.
   This variant exercises a retained flat barrier on the futex_waitv target and
   the default barrier elsewhere.  */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "flat" } */

#include <omp.h>
#include <stdlib.h>

enum { threads = 4, rounds = 16 };

int
main (void)
{
  omp_set_dynamic (0);

  for (int round = 0; round < rounds; ++round)
    {
      int participants = 0;

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

      if (omp_pause_resource_all (omp_pause_hard) != 0)
	abort ();
    }

  return 0;
}
