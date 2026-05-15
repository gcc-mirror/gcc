/*
   Test functionality of cancelling a region with no cancellation point.
   I.e. this tests the `_final` barrier being able to handle cancellation
   mid-way.

   N.b. this testcase was triggering an assertion during development of the
   linux/futex_waitv target.  It's here as a regression test, but the failure
   mode is only really visible when testing a libgomp built with assertions.
   The code should still work either way so it's not problematic to run each
   time, but catching the performance problem that would be a real issue is not
   very feasible.
   While the problem was only seen in on that target I run the testcase on all
   targets just for the sake of exercising code paths.

   For information's sake:  the performance problem was that threads could
   busy-wait when we judge they should have entering the kernel.  They would do
   this because the code assumed the `BAR_CANCELLED` flag would not change and
   they checked the originally read generation against their most recently read
   generation to see whether to continue spinning or sleep.
 */
/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <omp.h>
#include <stdlib.h>

#define NUM_THREADS 4
#define NUM_TASKS 500

/* Each task does busy work to keep tasks pending at the barrier. */
#define TASK_WORK 500000

static volatile int tasks_executed;
static volatile int tasks_total;

/* Goal is to trigger `BAR_CANCELLED` getting set on the barrier generation
   flag while many threads are already waiting on the `*_final` barrier at the
   end of this parallel region.

   We have tasks running in order to ensure that the setting of `BAR_CANCELLED`
   happens while secondary threads are in the tasking loop of
   `gomp_team_barrier_wait_for_tasks`.  */
int
main (void)
{
  if (!omp_get_cancellation ())
    abort ();
  omp_set_dynamic (0);
  omp_set_num_threads (NUM_THREADS);

  for (int trial = 0; trial < 20; trial++)
    {
#pragma omp atomic write
      tasks_executed = 0;
#pragma omp atomic write
      tasks_total = 0;

#pragma omp parallel num_threads(NUM_THREADS)
      {
	int tid = omp_get_thread_num ();

#pragma omp single nowait
	{
	  for (int i = 0; i < NUM_TASKS; i++)
	    {
#pragma omp task
	      {
		/* Busy work to keep the task running for ~1ms.
		   Gives the thread that hopes to cancel the barrier enough
		   time to observe that some tasks have executed and cancel the
		   barrier all tasks have been completed. */
		volatile int y = 0;
		for (int j = 0; j < TASK_WORK; j++)
		  y++;
#pragma omp atomic update
		tasks_executed += 1;
	      }
	    }
	}

	if (tid == 0)
	  {
	    /* Wait for some tasks to get executed.  As soon as there is at
	       least one task getting executed cancel the barrier.  This should
	       hopefully cancel the barrier before all tasks have been
	       completed (and observation shows this to be the case).  I.e.
	       ensures that other threads are in the relevant task handling loop
	       when the barrier is cancelled.  */
	    int executed;
#pragma omp atomic read
	    executed = tasks_executed;
	    while (executed == 0)
	      {
#pragma omp atomic read
		executed = tasks_executed;
	      };
#pragma omp cancel parallel
	  }
      }

      int n_exec;
#pragma omp atomic read
      n_exec = tasks_executed;
      /* If you're seeing an abort here then it is not a sign that libgomp is
	 failing.  Rather it is a sign that the timings in this testsuite do
	 not match your machine.  I.e. your machine completed the tasks too
	 fast for this testcase to be stressing anything.

	 That means this testcase may need to be updated, and without this
	 abort we would not notice.  */
      if (n_exec == NUM_TASKS)
	abort ();
    }
  return 0;
}
