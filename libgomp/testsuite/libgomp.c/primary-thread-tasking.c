/* Test to check our primary thread can execute some tasks while waiting for
   other threads.  This to check an edge-case in a recent implementation of the
   barrier tasking mechanism.
   I don't believe there's any way to guarantee that a task will be run on a
   given thread.  Hence I don't know anywhere I can put an `abort` and say we
   failed.  However I can set things up so that we'll timeout if the primary
   thread is not executing any tasks.  That timeout will at least count as a
   fail.

   Idea here being that we keep spawning tasks until one is handled by the
   primary thread.  Meanwhile we give the secondary threads lots of
   opportunities to sleep and let the primary thread take a task.  */
/* { dg-do run { target *-*-linux* } } */

#define _GNU_SOURCE
#include <omp.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <linux/futex.h>
#include <assert.h>
#include <stdatomic.h>
#include <limits.h>

int wake_flag = 0;

void
continue_until_on_thread0 ()
{
  if (omp_get_thread_num () == 0)
    {
      __atomic_fetch_add (&wake_flag, 1, memory_order_relaxed);
      syscall (SYS_futex, &wake_flag, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, INT_MAX);
    }
  else
    {
      /* If the flag has been set try again.  Otherwise put another few tasks
       * on the task queue.  */
      if (__atomic_load_n (&wake_flag, memory_order_relaxed))
	{
	  return;
	}
#pragma omp task
      continue_until_on_thread0 ();
#pragma omp task
      continue_until_on_thread0 ();
#pragma omp task
      continue_until_on_thread0 ();
      syscall (SYS_futex, &wake_flag, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, 0, NULL);
    }
}

void
foo ()
{
#pragma omp parallel
  {
    if (omp_get_thread_num () != 0)
      {
#pragma omp task
	continue_until_on_thread0 ();
#pragma omp task
	continue_until_on_thread0 ();
	/* Wait on the master thread to have executed one of the tasks.  */
	int val = __atomic_load_n (&wake_flag, memory_order_acquire);
	while (val == 0)
	  {
	    syscall (SYS_futex, &wake_flag, FUTEX_WAIT | FUTEX_PRIVATE_FLAG,
		     val, NULL);
	    val = __atomic_load_n (&wake_flag, memory_order_acquire);
	  }
      }
  }
}

int
main ()
{
  foo ();
  return 0;
}
