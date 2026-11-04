/* Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

/* This file handles maintenance of wavefronts and threads on AMD GCN.

   In SIMD mode, and in SIMT-DIRECTED mode, and OpenMP "thread" corresponds
   1-to-1 with a GPU "wavefront".

   In SIMT mode (no "directed"), an OpenMP "thread" corresponds to a GPU
   "work-item" (vector lane), conceptually.  However, the internal
   implementation (i.e. here) still maps gomp_thread_pool to wavefronts,
   since this is the smallest unit that can be assigned to a task.

   The thread numbers are translated from wavefront/work-item to a flat
   thread number only when presented to the user code.  */
#include <assert.h>

#include "libgomp.h"
#include "target-indirect.h"
#include <stdlib.h>
#include <string.h>

#define LITTLEENDIAN_CPU
#include "hsa.h"

#define UNLIKELY(x) (__builtin_expect ((x), 0))

/* Defined in basic-allocator.c via config/amdgcn/allocator.c.  */
void __gcn_lowlat_init (void *heap, size_t size);

static void gomp_wavefront_start (struct gomp_thread_pool *);

/* This externally visible function handles target region entry.  It
   sets up a per-team thread pool and transfers control by returning to
   the kernel in the master thread (wavefront) or gomp_wavefront_start in
   other threads (wavefronts).

   The name of this function is part of the interface with the compiler: for
   each OpenMP kernel the compiler configures the stack, then calls here.

   Likewise, gomp_gcn_exit_kernel is called during the kernel epilogue.  */

#ifdef __SIMT__
/* TODO: implement SIMT-DIRECTED vs. SIMT mode switching per kernel.
   The global-variable implementation is simpler but requires all kernels
   make the same choice.  There's not actually a way to compile per kernel
   anyway, yet.

   0 - SIMT-DIRECTED (vector-level)
   1 - SIMT (thread-level)  */
int __gcn_simt_mode = 0;
#endif

void
gomp_gcn_enter_kernel (
#ifdef __SIMT__
		       int simt_mode
#else
		       void
#endif
		       )
{
#ifdef __SIMT__
  __gcn_simt_mode = simt_mode;
#endif

  int wavefrontid = __builtin_gcn_dim_pos (1);

  if (wavefrontid == 0)
    {
      int numwavefronts = __builtin_gcn_dim_size (1);
      int teamid = __builtin_gcn_dim_pos(0);

      /* Initialize indirect function support for older libgomp.  */
      if (UNLIKELY (GOMP_INDIRECT_ADDR_MAP != NULL && teamid == 0))
	build_indirect_map ();

      /* Set up the global state.
	 Every team will do this, but that should be harmless.  */
#ifdef __SIMT__
      gomp_global_icv.nthreads_var = (simt_mode ? 1024 : 16);
      gomp_global_icv.thread_limit_var = (numwavefronts
					  * (simt_mode ? 64 : 1));
#else
      gomp_global_icv.nthreads_var = 16;
      gomp_global_icv.thread_limit_var = numwavefronts;
#endif
      /* Starting additional threads is not supported.  */
      gomp_global_icv.dyn_var = true;

      int __lds *gomp_team_num = (int __lds *) GOMP_TEAM_NUM;
      *gomp_team_num = 0;

      /* Initialize the team arena for optimized memory allocation.
         The arena has been allocated on the host side, and the address
         passed in via the kernargs.  Each team takes a small slice of it.  */
      struct kernargs_abi *kernargs =
	(struct kernargs_abi*) __builtin_gcn_kernarg_ptr ();
      void *team_arena = ((void*)kernargs->arena_ptr
			  + kernargs->arena_size_per_team * teamid);
      void * __lds *arena_start = (void * __lds *)TEAM_ARENA_START;
      void * __lds *arena_free = (void * __lds *)TEAM_ARENA_FREE;
      void * __lds *arena_end = (void * __lds *)TEAM_ARENA_END;
      *arena_start = team_arena;
      *arena_free = team_arena;
      *arena_end = team_arena + kernargs->arena_size_per_team;

      /* Initialize the low-latency heap.  The header is the size.  */
      void __lds *lowlat = (void __lds *)GCN_LOWLAT_HEAP;
      hsa_kernel_dispatch_packet_t *queue_ptr = __builtin_gcn_dispatch_ptr ();
      __gcn_lowlat_init ((void*)(uintptr_t)(void __flat*)lowlat,
			 queue_ptr->group_segment_size - GCN_LOWLAT_HEAP);

      /* Allocate and initialize the team-local-storage data.  */
      struct gomp_thread *thrs = team_malloc_cleared (sizeof (*thrs)
						      * numwavefronts);
      set_gcn_thrs (thrs);

      /* Allocate and initialize a pool of threads (wavefronts) in the team.
         The wavefronts are already running, of course, we just need to manage
         the communication between them.  */
      struct gomp_thread_pool *pool = team_malloc (sizeof (*pool));
      pool->threads = team_malloc (sizeof (void *) * numwavefronts);
      for (int tid = 0; tid < numwavefronts; tid++)
	pool->threads[tid] = &thrs[tid];
      pool->threads_size = numwavefronts;
      pool->threads_used = numwavefronts;
      pool->threads_busy = 1;
      pool->last_team = NULL;
      gomp_simple_barrier_init (&pool->threads_dock, numwavefronts);
      thrs->thread_pool = pool;

      asm ("s_barrier" ::: "memory");
      return;  /* Return to kernel.  */
    }
  else
    {
      asm ("s_barrier" ::: "memory");
      gomp_wavefront_start (gcn_thrs ()[0].thread_pool);
      /* gomp_wavefront_start does not return.  */
    }
}

void
gomp_gcn_exit_kernel (void)
{
  gomp_free_thread (gcn_thrs ());
  team_free (gcn_thrs ());
}

/* Populate THR from START_DATA.  Assumes THR is current thread.  Argument is
   broken out to avoid repeated calls to gomp_thread, which may be
   expensive.  */

static inline void
gomp_prep_our_thread (struct gomp_thread *thr,
		      struct gomp_thread_start_data *start_data,
		      int threadid)
{
  thr->ts.team = start_data->team;
  thr->ts.work_share = &start_data->team->work_shares[0];
  thr->ts.last_work_share = NULL;
  thr->ts.team_id = threadid;
  thr->ts.level = start_data->level;
  thr->ts.active_level = start_data->active_level;
  thr->ts.single_count = 0;
  thr->ts.static_trip = 0;
  thr->task = &start_data->team->implicit_task[threadid];
  gomp_init_task (thr->task, start_data->parent_task, &start_data->prev_icvs);
  /* TODO(arsen): This should be part of a mechanism that allows us to override
     nthreads-var with OMP_NUM_THREADS.  But, we currently don't have access to
     that list on the device.

     thr->task->icv.nthreads_var = ...;  */
  thr->task->taskgroup = start_data->taskgroup;
}

#ifdef __SIMT__
/* True if this lane of wave TEAM_ID is one of the OpenMP threads of TEAM,
   an outermost team in SIMT mode.  The team's last wave may be partial.  */

static inline bool
gomp_simt_lane_in_team_p (struct gomp_team *team, unsigned team_id)
{
  return team_id * SIMT_LANES + SIMT_LANE_ID < team->omp_nthreads;
}
#endif

/* Return a value that changes whenever the master wavefront passes a barrier
   of its current team, or starts or ends a team.  */

static inline uintptr_t
gomp_master_progress (void)
{
  struct gomp_team *team = __atomic_load_n (&gcn_thrs ()[0].ts.team,
					    MEMMODEL_RELAXED);
  if (!team)
    return 0;
  return ((uintptr_t) team
	  + __atomic_load_n (&team->barrier.generation, MEMMODEL_RELAXED));
}

/* This function contains the idle loop in which a wavefront (thread) waits
   to be called up to become part of a team.  */

static void
gomp_wavefront_start (struct gomp_thread_pool *pool)
{
  struct gomp_thread *thr = gomp_thread ();

  gomp_sem_init (&thr->release, 0);
  thr->thread_pool = pool;

  /* The loop exits only when "fn" is assigned "gomp_free_pool_helper",
     which contains "s_endpgm", or an infinite no-op loop is
     suspected (this happens when the thread master crashes).  */
  int nul_limit = 99;
  uintptr_t progress = 0;
  do
    {
      gomp_simple_barrier_wait (&pool->threads_dock);
      if (!thr->fn)
	{
	  /* GCN barriers stop every wavefront, so the barriers of a team that
	     does not use every wavefront also release the idle ones.  Count
	     only the wake-ups during which the master made no progress.  */
	  uintptr_t now = gomp_master_progress ();
	  if (now != progress)
	    {
	      progress = now;
	      nul_limit = 99;
	      continue;
	    }
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

#if __SIMT__
      /* Cleanup helpers have no team.  */
      struct gomp_team *team = thr->start_data ? thr->start_data->team : NULL;
#endif

      /* Perform rest of task initialization.  Populated from
	 gomp_team_start.  */
      if (thr->start_data)
	/* If !start_data, we're probably executing cleanup helpers, so we
	   don't really care about initializing these fields.  */
	{
	  /* On threads other than the main thread, the thread ID within a
	     team is always equal to dim_pos(1).  */
	  gomp_prep_our_thread (thr, thr->start_data, __builtin_gcn_dim_pos (1));
	  thr->start_data = NULL;
	}

#if __SIMT__
      if (__gcn_simt_mode)
	{
	  int _ = __builtin_gcn_simt_enter (1, 2);
	  if (!team || gomp_simt_lane_in_team_p (team, thr->ts.team_id))
	    thr->fn (thr->data);
	  __builtin_gcn_simt_exit (_);
	}
      else
#endif
	thr->fn (thr->data);
      thr->fn = NULL;

      struct gomp_task *task = thr->task;
      gomp_team_barrier_wait_final (&thr->ts.team->barrier);
      gomp_finish_task (task);
    }
  while (1);
}

#ifdef __SIMT__
/* See LAUNCH_MASTER_THREAD.  */

void __attribute__((noinline))
gomp_simt_launch_master (void (*fn) (void *), void *data)
{
  struct gomp_team *team = gomp_thread ()->ts.team;
  int _ = __builtin_gcn_simt_enter (1, 2);
  if (gomp_simt_lane_in_team_p (team, 0))
    fn (data);
  __builtin_gcn_simt_exit (_);
}
#endif

/* Launch a team.  */

void
gomp_team_start (void (*fn) (void *), void *data, unsigned nthreads,
		 unsigned flags, struct gomp_team *team,
		 struct gomp_taskgroup *taskgroup)
{
  struct gomp_thread *thr, *nthr;
  struct gomp_task *prev_task;
  struct gomp_task_icv *icv;
  struct gomp_thread_pool *pool;

  /* A nested team is started from SIMT code, but THR is per wave.  */
  if (!FIRST_LANE_P)
    return;

  thr = gomp_thread ();
  pool = thr->thread_pool;
  prev_task = thr->task;
  icv = prev_task ? &prev_task->icv : &gomp_global_icv;

  /* Always save the previous state, even if this isn't a nested team.
     In particular, we should save any work share state from an outer
     orphaned work share construct.  */
  team->prev_ts = thr->ts;

  /* Populate start data.  */
  team->thr_start_data = (struct gomp_thread_start_data) {
    .team = team,
    .level = thr->ts.level + 1,
    .active_level = thr->ts.active_level + (nthreads > 1),
    .parent_task = thr->task,
    .prev_icvs = *icv,
    .taskgroup = taskgroup
  };

#ifdef __SIMT__
  /* NTHREADS counts OpenMP threads; from here on it counts waves.  */
  nthreads = team->nthreads;
#endif

  unsigned actual_thread_count = __builtin_gcn_dim_size (1);
  if (nthreads > actual_thread_count)
    nthreads = actual_thread_count;

  if (nthreads != 1)
    {
      /* When there's more than one thread, we expect that we're operating on
	 thread w/ dim_pos(1) == 0, and that each of the other initialized
	 threads will operate with team_id == dim_pos(1).  */
      assert (__builtin_gcn_dim_pos (1) == 0);
      /* We only expect one team to have more than one active thread.  See
	 accelerator-specific logic in gomp_resolve_num_threads.  */
      assert (!thr->ts.active_level);

      /* Prepare other threads waiting on our barrier.  Besides fn, data,
	 taskgroup, all the fields of those threads are initialized based on
	 the values initialized in our thread above (which is always the master
	 thread).  */
      for (unsigned i = 1; i < nthreads; ++i)
	{
	  nthr = pool->threads[i];

	  nthr->start_data = &team->thr_start_data;
	  nthr->fn = fn;
	  nthr->data = data;
	  team->ordered_release[i] = &nthr->release;
	}

      /* Release the other threads.  */
      gomp_simple_barrier_wait (&pool->threads_dock);
    }

  /* Finish initializing our thread.  The thread ID in the team of the caller
     is always zero, even if __builtin_gcn_dim_pos (1) != 0.  */
  gomp_prep_our_thread (thr, &team->thr_start_data, 0);
}

#include "../../team.c"
