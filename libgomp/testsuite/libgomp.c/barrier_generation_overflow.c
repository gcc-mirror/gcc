/* We only run this for linux/futex_waitv targets because they have their
   generation number wrap at a much lower value than other targets.  The much
   higher value of other targets takes a very long time to reach -- so much
   that the default DejaGNU timeout treats it as a timeout anyway.  */
/* { dg-do run { target linux_futex_waitv } } */
/* Including the numbers for both futex_waitv and non-futex_waitv targets even
   though we only run on futex_waitv targets.  Just for anyone reading this
   file -- does not make any difference to how the testcase is ran.  */
/* { dg-additional-options "-DPREWRAP_BARRIERS=1048575" { target linux_futex_waitv } } */
/* { dg-additional-options "-DPREWRAP_BARRIERS=536870911" { target { ! linux_futex_waitv } } } */
#include <omp.h>
#include <unistd.h>

#define NTHREADS 2
#define TASK_USLEEP 500000

/* This is the plain-barrier analogue of cancel_incr_wraparound.c.
   The structure deliberately mirrors the 2-thread cancellable-generation
   wraparound testcase:

   1. A single long-lived 2-thread team executes 1,048,575 plain barriers
      without any tasks.  That advances the non-cancellable generation to the
      highest value value it can hold before wraparound (0xfffff000).
   2. Thread 1 creates one deferred task and then moves on to the final
      barrier.
   3. Thread 0 waits outside that final barrier until the deferred task has
      actually started.  In this testcase there are no other points where
      libgomp will schedule tasks between task creation and the final barrier,
      so knowing the task has started implies that thread 1 has entered the
      plain barrier wait path.
   4. Thread 0 then enters the 1,048,576th plain barrier use while the task is
      still running, so the wrap-sensitive raw-BAR_WAS_LAST completion check is
      exercised with task_count != 0.

   On the futex_waitv backend BAR_INCR = 4096, so the plain generation wraps
   after 2^32 / 4096 = 1,048,576 uses of the same plain barrier generation.  */

static int secondary_task_started;

static void
hold_barrier_task (void)
{
#pragma omp atomic write
  secondary_task_started = 1;
  usleep (TASK_USLEEP);
}

int
main (void)
{
  omp_set_dynamic (0);
  secondary_task_started = 0;

#pragma omp parallel num_threads(NTHREADS) shared(secondary_task_started)
  {
    int tid = omp_get_thread_num ();

    for (int round = 0; round < PREWRAP_BARRIERS; ++round)
      {
#pragma omp barrier
      }

    if (tid == 1)
      {
#pragma omp task
	hold_barrier_task ();
      }

    if (tid == 0)
      {
	int seen = 0;
	while (!seen)
	  {
#pragma omp atomic read
	    seen = secondary_task_started;
	  }
      }

      /* Thread 1 has already entered the barrier and started the deferred task
	 above before thread 0 enters this wrap-sensitive 1,048,576th plain
	 barrier use.  This means that we can force the check in
	 `gomp_barrier_has_completed` to be ran using a `state` which has
	 `BAR_WAS_LAST` set on it.   */
#pragma omp barrier
  }

  return 0;
}
