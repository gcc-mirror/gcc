/* Verify that a secondary tree coordinator drains tasks while gathering its
   children at both ordinary and final barriers.  In libgomp's radix-four
   topology logical thread 4 gathers threads 5 through 7.  Those children and
   thread 0 remain in user code until the queued task reports that thread 4
   ran it, making thread 4's child-gather wait the only runtime path that can
   execute the task.

   The ordinary case checks completion and release of every participant.  The
   final case additionally exercises setting BAR_HOLDING_SECONDARIES after
   child gathering and task draining have both completed.  This is an
   implementation regression test and is deliberately restricted to the new
   tree backend.

   When run on targets without a tree barrier this still forces a non-last
   waiter to execute a task before other threads can enter the barrier.
   Forces exercising of a code path not forced elsewhere in the testsuite.  */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include <omp.h>
#include <sched.h>
#include <stdlib.h>

enum { rounds = 32 };

static void
wait_for (int *value)
{
  int observed;
  do
    {
#pragma omp atomic read
      observed = *value;
      if (!observed)
	sched_yield ();
    }
  while (!observed);
}

static void
run_ordinary (void)
{
  int release_coordinator = 0;
  int release_others = 0;
  int task_ran = 0;
  int after_barrier = 0;

#pragma omp parallel num_threads(8) \
  shared(release_coordinator, release_others, task_ran, after_barrier)
  {
    int tid = omp_get_thread_num ();

    if (tid == 0)
      {
#pragma omp task shared(task_ran)
	{
	  if (omp_get_thread_num () != 4)
	    abort ();
#pragma omp atomic write
	  task_ran = 1;
	}

#pragma omp atomic write
	release_coordinator = 1;
	wait_for (&task_ran);
#pragma omp atomic write
	release_others = 1;
      }
    else if (tid == 4)
      wait_for (&release_coordinator);
    else
      wait_for (&release_others);

#pragma omp barrier

    int observed;
#pragma omp atomic read
    observed = task_ran;
    if (!observed)
      abort ();
#pragma omp atomic update
    after_barrier++;
  }

  if (task_ran != 1 || after_barrier != 8)
    abort ();
}

static void
run_final (void)
{
  int release_coordinator = 0;
  int release_others = 0;
  int task_ran = 0;

#pragma omp parallel num_threads(8) \
  shared(release_coordinator, release_others, task_ran)
  {
    int tid = omp_get_thread_num ();

    if (tid == 0)
      {
#pragma omp task shared(task_ran)
	{
	  if (omp_get_thread_num () != 4)
	    abort ();
#pragma omp atomic write
	  task_ran = 1;
	}

#pragma omp atomic write
	release_coordinator = 1;
	wait_for (&task_ran);
#pragma omp atomic write
	release_others = 1;
      }
    else if (tid == 4)
      wait_for (&release_coordinator);
    else
      wait_for (&release_others);
  }

  if (task_ran != 1)
    abort ();
}

int
main (void)
{
  omp_set_dynamic (0);
  for (int round = 0; round < rounds; ++round)
    {
      run_ordinary ();
      run_final ();
    }
  return 0;
}
