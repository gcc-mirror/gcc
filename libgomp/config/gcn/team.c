/* Copyright (C) 2017-2021 Free Software Foundation, Inc.
   Contributed by Mentor Embedded.

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

/* This file handles maintenance of threads on AMD GCN.  */

#include "libgomp.h"
#include <stdlib.h>
#include <string.h>

static void gomp_thread_start (struct gomp_thread_pool *);

/* This externally visible function handles target region entry.  It
   sets up a per-team thread pool and transfers control by returning to
   the kernel in the master thread or gomp_thread_start in other threads.

   The name of this function is part of the interface with the compiler: for
   each OpenMP kernel the compiler configures the stack, then calls here.

   Likewise, gomp_gcn_exit_kernel is called during the kernel epilogue.  */

void
gomp_gcn_enter_kernel (void)
{
  int threadid = __builtin_gcn_dim_pos (1);

  if (threadid == 0)
    {
      int numthreads = __builtin_gcn_dim_size (1);
      int teamid = __builtin_gcn_dim_pos(0);

      /* Set up the global state.
	 Every team will do this, but that should be harmless.  */
      gomp_global_icv.nthreads_var = 16;
      gomp_global_icv.thread_limit_var = numthreads;
      /* Starting additional threads is not supported.  */
      gomp_global_icv.dyn_var = true;

      /* Initialize the team arena for optimized memory allocation.
         The arena has been allocated on the host side, and the address
         passed in via the kernargs.  Each team takes a small slice of it.  */
      register void **kernargs asm("s8");
      void *team_arena = (kernargs[4] + TEAM_ARENA_SIZE*teamid);
      void * __lds *arena_start = (void * __lds *)TEAM_ARENA_START;
      void * __lds *arena_free = (void * __lds *)TEAM_ARENA_FREE;
      void * __lds *arena_end = (void * __lds *)TEAM_ARENA_END;
      *arena_start = team_arena;
      *arena_free = team_arena;
      *arena_end = team_arena + TEAM_ARENA_SIZE;

      /* Allocate and initialize the team-local-storage data.  */
      struct gomp_thread *thrs = team_malloc_cleared (sizeof (*thrs)
						      * numthreads);
      set_gcn_thrs (thrs);

      /* Allocate and initialize a pool of threads in the team.
         The threads are already running, of course, we just need to manage
         the communication between them.  */
      struct gomp_thread_pool *pool = team_malloc (sizeof (*pool));
      pool->threads = team_malloc (sizeof (void *) * numthreads);
      for (int tid = 0; tid < numthreads; tid++)
	pool->threads[tid] = &thrs[tid];
      pool->threads_size = numthreads;
      pool->threads_used = numthreads;
      pool->threads_busy = 1;
      pool->last_team = NULL;
      gomp_simple_barrier_init (&pool->threads_dock, numthreads);
      thrs->thread_pool = pool;

      asm ("s_barrier" ::: "memory");
      return;  /* Return to kernel.  */
    }
  else
    {
      asm ("s_barrier" ::: "memory");
      gomp_thread_start (gcn_thrs ()[0].thread_pool);
      /* gomp_thread_start does not return.  */
    }
}

void
gomp_gcn_exit_kernel (void)
{
  gomp_free_thread (gcn_thrs ());
  team_free (gcn_thrs ());
}

/* This function contains the idle loop in which a thread waits
   to be called up to become part of a team.  */

static void
gomp_thread_start (struct gomp_thread_pool *pool)
{
  struct gomp_thread *thr = gomp_thread ();

  gomp_sem_init (&thr->release, 0);
  thr->thread_pool = pool;

  /* The loop exits only when "fn" is assigned "gomp_free_pool_helper",
     which contains "s_endpgm", or an infinite no-op loop is
     suspected (this happens when the thread master crashes).  */
  int nul_limit = 99;
  do
    {
      gomp_simple_barrier_wait (&pool->threads_dock);
      if (!thr->fn)
	{
	  if (nul_limit-- > 0)
	    continue;
	  else
	    {
	      const char msg[] = ("team master not responding;"
				  " slave thread aborting");
	      write (2, msg, sizeof (msg)-1);
	      abort();
	    }
	}
      thr->fn (thr->data);
      thr->fn = NULL;

      struct gomp_task *task = thr->task;
      gomp_team_barrier_wait_final (&thr->ts.team->barrier);
      gomp_finish_task (task);
    }
  while (1);
}

/* Launch a team.  */

void
gomp_team_start (void (*fn) (void *), void *data, unsigned nthreads,
		 unsigned flags, struct gomp_team *team,
		 struct gomp_taskgroup *taskgroup)
{
  struct gomp_thread *thr, *nthr;
  struct gomp_task *task;
  struct gomp_task_icv *icv;
  struct gomp_thread_pool *pool;
  unsigned long nthreads_var;

  thr = gomp_thread ();
  pool = thr->thread_pool;
  task = thr->task;
  icv = task ? &task->icv : &gomp_global_icv;

  /* Always save the previous state, even if this isn't a nested team.
     In particular, we should save any work share state from an outer
     orphaned work share construct.  */
  team->prev_ts = thr->ts;

  thr->ts.team = team;
  thr->ts.team_id = 0;
  ++thr->ts.level;
  if (nthreads > 1)
    ++thr->ts.active_level;
  thr->ts.work_share = &team->work_shares[0];
  thr->ts.last_work_share = NULL;
  thr->ts.single_count = 0;
  thr->ts.static_trip = 0;
  thr->task = &team->implicit_task[0];
  nthreads_var = icv->nthreads_var;
  gomp_init_task (thr->task, task, icv);
  team->implicit_task[0].icv.nthreads_var = nthreads_var;
  team->implicit_task[0].taskgroup = taskgroup;

  if (nthreads == 1)
    return;

  /* Release existing idle threads.  */
  for (unsigned i = 1; i < nthreads; ++i)
    {
      nthr = pool->threads[i];
      nthr->ts.team = team;
      nthr->ts.work_share = &team->work_shares[0];
      nthr->ts.last_work_share = NULL;
      nthr->ts.team_id = i;
      nthr->ts.level = team->prev_ts.level + 1;
      nthr->ts.active_level = thr->ts.active_level;
      nthr->ts.single_count = 0;
      nthr->ts.static_trip = 0;
      nthr->task = &team->implicit_task[i];
      gomp_init_task (nthr->task, task, icv);
      team->implicit_task[i].icv.nthreads_var = nthreads_var;
      team->implicit_task[i].taskgroup = taskgroup;
      nthr->fn = fn;
      nthr->data = data;
      team->ordered_release[i] = &nthr->release;
    }

  gomp_simple_barrier_wait (&pool->threads_dock);
}

#include "../../team.c"
