/* Check that cancellation of the "next" barrier is not misattributed to
   cancellation of the "current" barrier.  After 255 successful uses, secondary
   threads arrive at use 256 before the primary.  The primary completes that
   barrier, wrapping the eight-bit cancellable generation to zero, and
   immediately cancels the next epoch.

   A secondary still observing completion of use 256 must not treat the next
   epoch's BAR_CANCELLED flag as cancellation of its successful barrier or
   roll back its local generation.  Every secondary must pass the first
   barrier and leave only at the following cancellation point.  */
/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include <omp.h>
#include <sched.h>
#include <stdlib.h>

enum { threads = 2, prewrap_rounds = 255 };

static int target_arrived;
static int after_first;
static int escaped_second;
static int inactive_cancel;

int
main (void)
{
  if (!omp_get_cancellation ())
    abort ();
  omp_set_dynamic (0);

#pragma omp parallel num_threads(threads) \
  shared(target_arrived, after_first, escaped_second)
  {
    int tid = omp_get_thread_num ();

    for (int round = 0; round < prewrap_rounds; ++round)
      {
#pragma omp cancel parallel if(inactive_cancel)
#pragma omp barrier
      }

    if (tid != 0)
      {
#pragma omp atomic update
	target_arrived++;
      }
    else
      {
	int seen;
	do
	  {
#pragma omp atomic read
	    seen = target_arrived;
	    if (seen != threads - 1)
	      sched_yield ();
	  }
	while (seen != threads - 1);
      }

    /* This successful use wraps the cancellable generation to zero.  */
#pragma omp barrier

    if (tid == 0)
      {
	/* This cancellation belongs to the following barrier epoch.  */
#pragma omp cancel parallel
	abort ();
      }
    else
      {
#pragma omp atomic update
	after_first++;

#pragma omp barrier
#pragma omp atomic update
	escaped_second++;
      }
  }

  if (target_arrived != threads - 1 || after_first != threads - 1
      || escaped_second != 0)
    abort ();

  return 0;
}
