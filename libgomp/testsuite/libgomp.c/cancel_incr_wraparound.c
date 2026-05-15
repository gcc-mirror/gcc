/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */
/* Regression test for bug seen during development.

   This exercises the case where `gomp_barrier_handle_tasks` is called with a
   `state` that has non-zero flags while the cancel generation bitfield is
   about to wrap around.  By doing this it exercises an edge case for
   `gomp_barrier_incremented` where `state > gen` but `next < gen`.

   On the futex_waitv backend the cancellable generation uses BAR_CANCEL_INCR =
   16 and 8 dedicated bits, so the wrap-sensitive state can only be exhibited
   after 256 uses of the same cancel-capable barrier generation. This test
   has some pre-wrap rounds to get to the wrap-sensitive state before running
   the edge case.  */

#include <omp.h>
#include <unistd.h>

#define NTHREADS 2
#define PREWRAP_ROUNDS 255
#define TASK_USLEEP 200000

static volatile int inactive_cancel;
static int secondary_task_started;

static inline void
cheap_work (int i)
{
  asm volatile ("" : : "r"(i) : "memory");
}

static void
hold_barrier_task (void)
{
#pragma omp atomic write
  secondary_task_started = 1;
  usleep (TASK_USLEEP);
}

static void
run_outer_round (void)
{
  inactive_cancel = 0;
  secondary_task_started = 0;

#pragma omp parallel num_threads(NTHREADS) shared(secondary_task_started)
  {
    int tid = omp_get_thread_num ();

    /* These 255 loop-end barriers, plus the explicit barrier below, give the
       same cancel-capable barrier 256 uses on this 2-thread team.  */
    for (int round = 0; round < PREWRAP_ROUNDS; ++round)
      {
#pragma omp cancel parallel if (tid == 0 && inactive_cancel)
#pragma omp for schedule(static, 1)
	for (int i = 0; i < NTHREADS; ++i)
	  cheap_work (i + round);
      }

    if (tid == 1)
      {
#pragma omp task
	hold_barrier_task ();
      }

#pragma omp cancel parallel if (tid == 0 && inactive_cancel)
    if (tid == 0)
      {
	int seen = 0;
	while (!seen)
	  {
#pragma omp atomic read
	    seen = secondary_task_started;
	  }
      }

      /* If we did everything right, thread 1 has already entered the
	 cancellable barrier wait path and started the deferred task above
	 before thread 0 enters this wrap-sensitive barrier use.  That deferred
	 task should be long-running enough that the primary thread can go
	 through the initial part of the barrier and start attempting to perform
	 tasks before all tasks have been completed.  */
#pragma omp barrier
  }
}

int
main (void)
{
  omp_set_dynamic (0);
  omp_set_schedule (omp_sched_static, 1);
  run_outer_round ();
  return 0;
}
