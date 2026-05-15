/* Exercise changing between flat and centralized barriers for a live team.
   N.b. there should be no observable difference between switching and not, but
   having a testcase at least exercises the code path.

   The switching happens after the spin count is dynamically throttled due to
   the number of threads exceeding the number of available CPU's.  It is then
   switched back after a helper thread completes and the number of threads
   drops back down.  */
/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "auto" } */
/* { dg-set-target-env-var GOMP_SPINCOUNT "100000000" } */
/* { dg-set-target-env-var OMP_DYNAMIC "false" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS "2" } */

#define _GNU_SOURCE 1
#include <omp.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

static _Atomic int start_helper;
static _Atomic int helper_active;
static _Atomic int stop_helper;
static _Atomic int helper_finished;

static void
wait_for (const _Atomic int *flag)
{
  while (!atomic_load_explicit (flag, memory_order_acquire))
    sched_yield ();
}

static void
barrier (void)
{
#pragma omp barrier
}

/* Restrict the process before libgomp is reinitialised so that
   gomp_available_cpus is three.  With the main nested team active there are
   three managed threads.  Starting the helper nested team increases that to
   four, making spin_count switch to its throttled value.  */
static bool
restrict_to_three_cpus_and_reexec (void)
{
  if (getenv ("GOMP_BARRIER_SWITCH_REEXEC"))
    return true;

  cpu_set_t available;
  cpu_set_t selected;
  if (sched_getaffinity (0, sizeof (available), &available) != 0)
    return false;

  CPU_ZERO (&selected);
  int count = 0;
  for (int cpu = 0; cpu < CPU_SETSIZE && count != 3; ++cpu)
    if (CPU_ISSET (cpu, &available))
      {
	CPU_SET (cpu, &selected);
	++count;
      }
  if (count != 3
      || sched_setaffinity (0, sizeof (selected), &selected) != 0
      || setenv ("GOMP_BARRIER_SWITCH_REEXEC", "1", 1) != 0)
    return false;

  execl ("/proc/self/exe", "barrier-switch-1", NULL);
  abort ();
}

int
main (void)
{
  if (!restrict_to_three_cpus_and_reexec ())
    return 0;

  omp_set_dynamic (0);
  omp_set_max_active_levels (2);

#pragma omp parallel num_threads(2)
  {
    if (omp_get_thread_num () == 0)
      {
	/* Keep this nested team alive while its sibling team changes the global
	   number of managed threads.  */
#pragma omp parallel num_threads(2)
	{
	  /* Initially three managed threads run on three available CPUs.  This
	     barrier is flat and selects flat for the next generation.  */
	  barrier ();

#pragma omp masked
	  {
	    atomic_store_explicit (&start_helper, 1, memory_order_release);
	    wait_for (&helper_active);
	  }

	  /* The current barrier is flat.  Four managed threads are now running
	     on three available CPUs, so completion selects centralized.  */
	  barrier ();
	  /* Exercise the selected centralized barrier.  */
	  barrier ();

#pragma omp masked
	  {
	    atomic_store_explicit (&stop_helper, 1, memory_order_release);
	    wait_for (&helper_finished);
	  }

	  /* The current barrier is centralized.  The helper nested team has
	     finished, returning to three managed threads, so completion
	     selects flat.  */
	  barrier ();
	  /* Exercise the selected flat barrier.  */
	  barrier ();
	}
      }
    else
      {
	wait_for (&start_helper);

	/* This sibling nested team adds one managed thread until both members
	   leave the region.  */
#pragma omp parallel num_threads(2)
	{
#pragma omp masked
	  atomic_store_explicit (&helper_active, 1, memory_order_release);

	  wait_for (&stop_helper);
	}

	/* Nested-team workers are removed from gomp_managed_threads before the
	   nested parallel construct returns.  */
	atomic_store_explicit (&helper_finished, 1, memory_order_release);
      }
  }

  return 0;
}
