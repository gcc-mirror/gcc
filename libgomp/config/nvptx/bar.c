/* Copyright (C) 2015-2023 Free Software Foundation, Inc.
   Contributed by Alexander Monakov <amonakov@ispras.ru>

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This is an NVPTX specific implementation of a barrier synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and bar.sync instruction.  */

#include <limits.h>
#include "libgomp.h"

void
gomp_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* Next time we'll be awaiting TOTAL threads again.  */
      bar->awaited = bar->total;
      __atomic_store_n (&bar->generation, bar->generation + BAR_INCR,
			MEMMODEL_RELEASE);
    }
  if (bar->total > 1)
    asm ("bar.sync 1, %0;" : : "r" (32 * bar->total));
}

void
gomp_barrier_wait (gomp_barrier_t *bar)
{
  gomp_barrier_wait_end (bar, gomp_barrier_wait_start (bar));
}

/* Like gomp_barrier_wait, except that if the encountering thread
   is not the last one to hit the barrier, it returns immediately.
   The intended usage is that a thread which intends to gomp_barrier_destroy
   this barrier calls gomp_barrier_wait, while all other threads
   call gomp_barrier_wait_last.  When gomp_barrier_wait returns,
   the barrier can be safely destroyed.  */

void
gomp_barrier_wait_last (gomp_barrier_t *bar)
{
  /* The above described behavior matches 'bar.arrive' perfectly.  */
  if (bar->total > 1)
    asm ("bar.arrive 1, %0;" : : "r" (32 * bar->total));
}

/* Barriers are implemented mainly using 'bar.red.or', which combines a bar.sync
   operation with a OR-reduction of "team->task_count != 0" across all threads.
   Task processing is done only after synchronization and verifying that
   task_count was non-zero in at least one of the team threads.

   This use of simple-barriers, and queueing of tasks till the end, is deemed
   more efficient performance-wise for GPUs in the common offloading case, as
   opposed to implementing futex-wait/wake operations to simultaneously process
   tasks in a CPU-thread manner (which is not easy to implement efficiently
   on GPUs).  */

void
gomp_team_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  bool run_tasks = (team->task_count != 0);
  if (bar->total > 1)
    run_tasks = __builtin_nvptx_bar_red_or (1, 32 * bar->total, true,
					    (team->task_count != 0));

  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* Next time we'll be awaiting TOTAL threads again.  */
      bar->awaited = bar->total;
      team->work_share_cancelled = 0;
    }

  if (__builtin_expect (run_tasks == true, 0))
    {
      while (__atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE)
	     & BAR_TASK_PENDING)
	gomp_barrier_handle_tasks (state);

      if (bar->total > 1)
	asm volatile ("bar.sync 1, %0;" : : "r" (32 * bar->total));
    }
}

void
gomp_team_barrier_wait (gomp_barrier_t *bar)
{
  gomp_team_barrier_wait_end (bar, gomp_barrier_wait_start (bar));
}

void
gomp_team_barrier_wait_final (gomp_barrier_t *bar)
{
  gomp_barrier_state_t state = gomp_barrier_wait_final_start (bar);
  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    bar->awaited_final = bar->total;
  gomp_team_barrier_wait_end (bar, state);
}

/* See also comments for gomp_team_barrier_wait_end.  */

bool
gomp_team_barrier_wait_cancel_end (gomp_barrier_t *bar,
				   gomp_barrier_state_t state)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  bool run_tasks = (team->task_count != 0);
  if (bar->total > 1)
    run_tasks = __builtin_nvptx_bar_red_or (1, 32 * bar->total, true,
					    (team->task_count != 0));
  if (state & BAR_CANCELLED)
    return true;

  if (__builtin_expect (state & BAR_WAS_LAST, 0))
    {
      /* Note: BAR_CANCELLED should never be set in state here, because
	 cancellation means that at least one of the threads has been
	 cancelled, thus on a cancellable barrier we should never see
	 all threads to arrive.  */

      /* Next time we'll be awaiting TOTAL threads again.  */
      bar->awaited = bar->total;
      team->work_share_cancelled = 0;
    }

  if (__builtin_expect (run_tasks == true, 0))
    {
      while (__atomic_load_n (&bar->generation, MEMMODEL_ACQUIRE)
	     & BAR_TASK_PENDING)
	gomp_barrier_handle_tasks (state);

      if (bar->total > 1)
	asm volatile ("bar.sync 1, %0;" : : "r" (32 * bar->total));
    }

  return false;
}

bool
gomp_team_barrier_wait_cancel (gomp_barrier_t *bar)
{
  return gomp_team_barrier_wait_cancel_end (bar, gomp_barrier_wait_start (bar));
}

void
gomp_team_barrier_cancel (struct gomp_team *team)
{
  gomp_mutex_lock (&team->task_lock);
  if (team->barrier.generation & BAR_CANCELLED)
    {
      gomp_mutex_unlock (&team->task_lock);
      return;
    }
  team->barrier.generation |= BAR_CANCELLED;
  gomp_mutex_unlock (&team->task_lock);

  /* The 'exit' instruction cancels this thread and also fullfills any other
     CTA threads waiting on barriers.  */
  asm volatile ("exit;");
}
