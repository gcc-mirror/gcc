/* Test functionality of cancelling a region with no cancellable barrier.
   On the `futex_waitv` target in this case the implicit barrier at the end of
   the parallel region sees the BAR_CANCELLED flag and needs to be able to
   handle it.

   While it needs to be able to handle it, it still needs to wait for all
   threads, because this is not a cancellable barrier it is the implicit
   non-cancellable barrier at the end of the parallel region.

   Still run the testcase on all targets for breadth of testing.  */
/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */
#include <omp.h>
#include <stdlib.h>

int
main (void)
{
  omp_set_dynamic (0);

  for (int round = 0; round < 200; ++round)
    {
      int phase = 0;

#pragma omp parallel num_threads(8)
      {
	int tid = omp_get_thread_num ();

	if (tid == 0)
	  {
	    /* Primary goes straight through to barrier, and waits for other
	       threads to arrive.  Publish that we are about to fall into the
	       final barrier so the non-primary coordination can start.  */
#pragma omp atomic write
	    phase = 1;
	  }
	else if (tid == 1)
	  {
	    /* We need one non-primary thread to issue the cancel while the
	       primary is still waiting for secondaries to arrive.  */
	    while (1)
	      {
		int seen;
#pragma omp atomic read
		seen = phase;
		if (seen >= 1)
		  break;
	      }
#pragma omp atomic write
	    phase = 2;
#pragma omp cancel parallel if (1)
	  }
	else if (tid == 2)
	  {
	    /* Second non-primary thread holds the primary thread until the
	       region has been cancelled.  */
	    while (1)
	      {
		int seen;
#pragma omp atomic read
		seen = phase;
		if (seen >= 2)
		  break;
	      }
	  }
      }
    }

  return 0;
}
