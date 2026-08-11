/* Force late arrival at an already-cancelled flat barrier.  Thread 0 is
   released toward the barrier before cancellation is requested, while thread 2
   waits until thread 1 has committed cancellation before starting the same
   barrier.  The late waiter must return cancelled without recording an arrival
   for the cancelled generation.  A following plain barrier makes a runtime
   with assertions enabled scan each thread-local generation and also verifies
   that team reuse remains live.

   When ran on non-flat barrier still forces barrier entry after cancellation.
   That is a code path not forced elsewhere in the testsuite, hence running it
   on all targets.  */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "flat" } */

#include <omp.h>
#include <sched.h>
#include <stdlib.h>

enum { rounds = 64 };

static int early_ready;
static int late_ready;
static int cancel_committed;
static int escaped;

static int
read_atomic (int *value)
{
  int result;
#pragma omp atomic read
  result = *value;
  return result;
}

static void
wait_for (int *value)
{
  while (!read_atomic (value))
    sched_yield ();
}

static void
mark_committed_cancel (int *active)
{
  if (*active)
    {
#pragma omp atomic write
      cancel_committed = 1;
    }
}

static void
run_cancelled_region (void)
{
  early_ready = 0;
  late_ready = 0;
  cancel_committed = 0;
  escaped = 0;

#pragma omp parallel num_threads(3) \
  shared(early_ready, late_ready, cancel_committed, escaped)
  {
    int tid = omp_get_thread_num ();

    if (tid == 1)
      {
	int mark __attribute__ ((cleanup (mark_committed_cancel))) = 1;
	wait_for (&early_ready);
	wait_for (&late_ready);

	/* Cleanup runs only after GOMP_cancel has committed BAR_CANCELLED
	   and transferred control out of this branch.  */
#pragma omp cancel parallel
	abort ();
      }
    else
      {
	if (tid == 0)
	  {
#pragma omp atomic write
	    early_ready = 1;
	  }
	else
	  {
#pragma omp atomic write
	    late_ready = 1;
	    wait_for (&cancel_committed);
	  }

#pragma omp barrier
#pragma omp atomic update
	escaped++;
      }
  }

  if (!cancel_committed || escaped != 0)
    abort ();
}

static void
run_plain_control (void)
{
  int arrived = 0;

#pragma omp parallel num_threads(3) shared(arrived)
  {
#pragma omp atomic update
    arrived++;
#pragma omp barrier
    if (read_atomic (&arrived) != 3)
      abort ();
  }

  if (arrived != 3)
    abort ();
}

int
main (void)
{
  if (!omp_get_cancellation ())
    abort ();
  omp_set_dynamic (0);

  for (int round = 0; round < rounds; ++round)
    {
      run_cancelled_region ();
      run_plain_control ();
    }

  return 0;
}
