/* Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This file handles the maintenance of threads in response to team
   creation and termination.  */

#include "libgomp.h"
#include "pool.h"
#include <stdlib.h>
#include <string.h>

static void
gomp_release_held_threads (struct gomp_thread_pool *pool,
			   struct gomp_team *team, unsigned team_id)
{
  gomp_assert (team_id == 0, "releasing threads from non-primary thread %u",
	       team_id);
  if (pool->prev_barrier)
    {
      struct gomp_team *saved_team __attribute__ ((unused))
	= pool->last_team ? pool->last_team : team;
      gomp_assert (pool->prev_barrier == &saved_team->barrier,
		   "prev_barrier not within cached team: %p != %p",
		   pool->prev_barrier, &saved_team->barrier);
      gomp_team_barrier_done_final (pool->prev_barrier, team_id);
      pool->prev_barrier = NULL;
    }
}

#ifdef LIBGOMP_USE_PTHREADS
pthread_attr_t gomp_thread_attr;

/* This key is for the thread destructor.  */
pthread_key_t gomp_thread_destructor;


/* This is the libgomp per-thread data structure.  */
#if defined HAVE_TLS || defined USE_EMUTLS
__thread struct gomp_thread gomp_tls_data;
#else
pthread_key_t gomp_tls_key;
#endif


/* This structure is used to communicate across pthread_create.  */

struct gomp_thread_start_data
{
  struct gomp_team *team;
  struct gomp_thread_pool *thread_pool;
  unsigned int place;
  unsigned team_id;
  unsigned active_level;
  unsigned place_partition_off;
  unsigned place_partition_len;
  bool nested;
  pthread_t handle;
};

static inline void
gomp_init_team_state (struct gomp_team_state *ts, struct gomp_team *team,
		      unsigned team_id, unsigned active_level,
		      unsigned place_partition_off,
		      unsigned place_partition_len)
{
  ts->team = team;
  ts->work_share = &team->work_shares[0];
  ts->last_work_share = NULL;
  ts->team_id = team_id;
  ts->level = team->prev_ts.level + 1;
  ts->active_level = active_level;
  ts->place_partition_off = place_partition_off;
  ts->place_partition_len = place_partition_len;
  ts->def_allocator = team->ti.def_allocator;
#ifdef HAVE_SYNC_BUILTINS
  ts->single_count = 0;
#endif
  ts->static_trip = 0;
}

/* This function is a pthread_create entry point.  This contains the idle
   loop in which a thread waits to be called up to become part of a team.  */

static void *
gomp_thread_start (void *xdata)
{
  struct gomp_thread_start_data *data = xdata;
  struct gomp_thread *thr;
  struct gomp_thread_pool *pool;
  void (*local_fn) (void *);
  void *local_data;

#if defined HAVE_TLS || defined USE_EMUTLS
  thr = &gomp_tls_data;
#else
  struct gomp_thread local_thr;
  thr = &local_thr;
#endif
  gomp_sem_init (&thr->release, 0);

  /* Extract what we need from data.  */
  struct gomp_team *team = data->team;
  gomp_init_team_state (&thr->ts, team, data->team_id, data->active_level,
			data->place_partition_off, data->place_partition_len);

  local_fn = team->ti.fn;
  local_data = team->ti.data;
  thr->num_teams = team->ti.num_teams;
  thr->team_num = team->ti.team_num;
  thr->thread_pool = data->thread_pool;
  thr->place = data->place;
  thr->exit = false;
#ifdef GOMP_NEEDS_THREAD_HANDLE
  thr->handle = data->handle;
#endif
#if !(defined HAVE_TLS || defined USE_EMUTLS)
  pthread_setspecific (gomp_tls_key, thr);
#endif

  team->ordered_release[thr->ts.team_id] = &thr->release;

  /* Make thread pool local. */
  pool = thr->thread_pool;

  if (data->nested)
    {
      struct gomp_team *team = thr->ts.team;
      thr->task = &team->implicit_task[thr->ts.team_id];
      gomp_init_task (thr->task, team->ti.task_parent_init,
		      &team->ti.task_icv_init);
      thr->task->taskgroup = team->ti.task_taskgroup;
      struct gomp_task *task = thr->task;

      gomp_barrier_wait (&team->barrier, thr->ts.team_id);

      local_fn (local_data);
      gomp_team_barrier_wait_final (&team->barrier, thr->ts.team_id);
      gomp_finish_task (task);
      gomp_barrier_wait_last (&team->barrier, thr->ts.team_id);
    }
  else
    {
      pool->threads[thr->ts.team_id] = thr;

      gomp_simple_barrier_wait (&pool->threads_dock);

      /* Ensure we initialise the task *after* the wait on threads_dock just in
	 case affinity changes have meant that this new thread has the same
	 team_id as a thread used to have.  In this case we want to ensure the
	 `gomp_finish_task` below is certain to happen before the
	 `gomp_init_task` here.  */
      thr->task = &team->implicit_task[thr->ts.team_id];
      gomp_init_task (thr->task, team->ti.task_parent_init,
		      &team->ti.task_icv_init);
      thr->task->taskgroup = team->ti.task_taskgroup;

      do
	{
	  struct gomp_team *team = thr->ts.team;
	  unsigned team_id = thr->ts.team_id;
	  struct gomp_task *task = thr->task;

	  thr->fn = NULL;
	  local_fn (local_data);
	  gomp_team_barrier_wait_final (&team->barrier, thr->ts.team_id);
	  gomp_finish_task (task);
	  /* Hold at the simple barrier if we're exiting.
	     Do this in order to synchronise with the primary thread that could
	     (after the next piece of work has been performed) free our team.
	     Need to ensure that the barrier waited on above is not freed until
	     we've completed it.  With any threads that will be used in the
	     next iteration we know that it will synchronise with the primary
	     thread before it frees this team because it will synchronise with
	     the primary thread in the next iteration of this loop (in
	     `gomp_team_barrier_wait_final`).
	     Also have to hold here if we adjusted our team_id since the above
	     `task` location is possibly going to be used by a newly started
	     thread so we have to synchronise with those.  Use boolean
	     short-circuit to avoid data-race on thr->ts.team in case where the
	     final barrier can't hold threads.  */
	  if (!gomp_barrier_can_hold (&team->barrier)
	      || thr->exit
	      || (thr->ts.team == team && thr->ts.team_id != team_id))
	    gomp_simple_barrier_wait (&pool->threads_dock);

	  /* Early exit because if `thr->fn == NULL` then we can't guarantee
	     the `thr->ts.team` hasn't been freed by the primary thread racing
	     ahead.  Hence we don't want to write to it.  */
	  if (thr->exit)
	    break;

	  /* Usually `thr->fn` is not used or set.  It is used when secondary
	     threads are "set going" without being assigned to any team.
	     See `gomp_pause_host` and `gomp_free_thread`.
	     In this case we don't want to bother reading the team
	     initialisation things because they've not been set up for us.  */
	  if (__builtin_expect (thr->fn != NULL, 0))
	    {
	      local_fn = thr->fn;
	      local_data = thr->data;
	      continue;
	    }

	  team = thr->ts.team;
	  thr->ts.work_share = &team->work_shares[0];
	  thr->ts.last_work_share = NULL;
	  thr->ts.def_allocator = team->ti.def_allocator;
#ifdef HAVE_SYNC_BUILTINS
	  thr->ts.single_count = 0;
#endif
	  thr->ts.static_trip = 0;
	  local_fn = team->ti.fn;
	  local_data = team->ti.data;
	  thr->num_teams = team->ti.num_teams;
	  thr->team_num = team->ti.team_num;
	  thr->task = &team->implicit_task[thr->ts.team_id];
	  gomp_init_task (thr->task, team->ti.task_parent_init,
			  &team->ti.task_icv_init);
	  thr->task->taskgroup = team->ti.task_taskgroup;
	}
      while (true);
    }

  gomp_sem_destroy (&thr->release);
  pthread_detach (pthread_self ());
  thr->thread_pool = NULL;
  thr->task = NULL;
  return NULL;
}
#endif

static inline struct gomp_team *
get_last_team (unsigned nthreads)
{
  struct gomp_thread *thr = gomp_thread ();
  if (thr->ts.team == NULL)
    {
      gomp_assert (thr->ts.level == 0,
		   "looking for cached team in nested region %u thr->ts.level",
		   thr->ts.level);
      struct gomp_thread_pool *pool = gomp_get_thread_pool (thr, nthreads);
      struct gomp_team *last_team = pool->last_team;
      if (last_team != NULL && last_team->nthreads == nthreads)
        {
	  gomp_assert (!pool->prev_barrier
			 || pool->prev_barrier == &pool->last_team->barrier,
		       "prev_barrier not within cached team: %p != %p",
		       pool->prev_barrier, &pool->last_team->barrier);
	  pool->last_team = NULL;
	  last_team->reused = true;
	  return last_team;
	}
    }
  return NULL;
}

/* Create a new team data structure.  */

struct gomp_team *
gomp_new_team (unsigned nthreads)
{
  struct gomp_team *team;
  int i;

  team = get_last_team (nthreads);
  if (team == NULL)
    {
      void *barrier_extra;
      struct gomp_barrier_extra_alloc_needs barrier_extra_alloc
	= gomp_barrier_extra_alloc (nthreads);
      size_t extra = sizeof (team->ordered_release[0])
		     + sizeof (team->implicit_task[0]);
      size_t team_extra = nthreads * extra;
      size_t barrier_padding = 0;
      if (barrier_extra_alloc.size)
	{
	  /* `offsetof` in case the structure is padded and `implicit_task` is
	     not at the end of `sizeof (*team)`.  */
	  size_t barrier_offset
	    = offsetof (struct gomp_team, implicit_task) + team_extra;
	  gomp_assert (barrier_extra_alloc.align
			 <= __alignof (struct gomp_team),
		       "barrier extra alignment too large: %lu > %lu",
		       (unsigned long) barrier_extra_alloc.align,
		       (unsigned long) __alignof (struct gomp_team));
	  barrier_padding = ((barrier_offset + barrier_extra_alloc.align - 1)
			     & ~(barrier_extra_alloc.align - 1))
			    - barrier_offset;
	}
      size_t alloc_extra
	= team_extra + barrier_padding + barrier_extra_alloc.size;
#ifdef GOMP_USE_ALIGNED_WORK_SHARES
      team = gomp_aligned_alloc (__alignof (struct gomp_team),
				 sizeof (*team) + alloc_extra);
#else
      team = team_malloc (sizeof (*team) + alloc_extra);
#endif

#ifndef HAVE_SYNC_BUILTINS
      gomp_mutex_init (&team->work_share_list_free_lock);
#endif
      barrier_extra = (char *) &team->implicit_task[nthreads]
		      + nthreads * sizeof (team->ordered_release[0])
		      + barrier_padding;
      gomp_barrier_init (&team->barrier, nthreads, barrier_extra);
      gomp_mutex_init (&team->task_lock);

      team->nthreads = nthreads;
      team->reused = false;
    }

  team->work_share_chunk = 8;
#ifdef HAVE_SYNC_BUILTINS
  team->single_count = 0;
#endif
  team->work_shares_to_free = &team->work_shares[0];
  gomp_init_work_share (&team->work_shares[0], 0, nthreads);
  team->work_shares[0].next_alloc = NULL;
  team->work_share_list_free = NULL;
  team->work_share_list_alloc = &team->work_shares[1];
  for (i = 1; i < 7; i++)
    team->work_shares[i].next_free = &team->work_shares[i + 1];
  team->work_shares[i].next_free = NULL;

  gomp_sem_init (&team->master_release, 0);
  team->ordered_release = (void *) &team->implicit_task[nthreads];
  team->ordered_release[0] = &team->master_release;

  priority_queue_init (&team->task_queue);
  team->task_count = 0;
  team->task_queued_count = 0;
  team->task_running_count = 0;
  team->work_share_cancelled = 0;
  team->team_cancelled = 0;

  team->task_detach_count = 0;

  return team;
}


/* Free a team data structure.  */

static void
free_team (struct gomp_team *team)
{
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_destroy (&team->work_share_list_free_lock);
#endif
  gomp_barrier_destroy (&team->barrier);
  gomp_mutex_destroy (&team->task_lock);
  priority_queue_free (&team->task_queue);
  team_free (team);
}

static void
gomp_free_pool_helper (void *thread_pool)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_thread_pool *pool
    = (struct gomp_thread_pool *) thread_pool;
  gomp_simple_barrier_wait_last (&pool->threads_dock);
  gomp_sem_destroy (&thr->release);
  thr->thread_pool = NULL;
  thr->task = NULL;
#ifdef LIBGOMP_USE_PTHREADS
  pthread_detach (pthread_self ());
  pthread_exit (NULL);
#elif defined(__nvptx__)
  asm ("exit;");
#elif defined(__AMDGCN__)
  asm ("s_endpgm");
#else
#error gomp_free_pool_helper must terminate the thread
#endif
}

/* Free a thread pool and release its threads. */

void
gomp_free_thread (void *arg __attribute__((unused)))
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_thread_pool *pool = thr->thread_pool;
  if (pool)
    {
      if (pool->threads_used > 0)
	{
	  int i;
	  for (i = 1; i < pool->threads_used; i++)
	    {
	      struct gomp_thread *nthr = pool->threads[i];
	      nthr->fn = gomp_free_pool_helper;
	      nthr->data = pool;
	    }
	  /* This barrier undocks threads docked on pool->threads_dock.  */
	  gomp_release_held_threads (pool, NULL, thr->ts.team_id);
	  if (!gomp_barrier_can_hold (pool->prev_barrier))
	    gomp_simple_barrier_wait (&pool->threads_dock);
	  /* And this waits till all threads have called
	     gomp_simple_barrier_wait_last in gomp_free_pool_helper.  */
	  gomp_simple_barrier_wait (&pool->threads_dock);
	  /* Now it is safe to destroy the barrier and free the pool.  */
	  gomp_simple_barrier_destroy (&pool->threads_dock);

#ifdef HAVE_SYNC_BUILTINS
	  __sync_fetch_and_add (&gomp_managed_threads,
				1L - pool->threads_used);
#else
	  gomp_mutex_lock (&gomp_managed_threads_lock);
	  gomp_managed_threads -= pool->threads_used - 1L;
	  gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
	}
      if (pool->last_team)
	free_team (pool->last_team);
#ifndef __nvptx__
      team_free (pool->threads);
      team_free (pool);
#endif
      thr->thread_pool = NULL;
    }
  if (thr->ts.level == 0 && __builtin_expect (thr->ts.team != NULL, 0))
    gomp_team_end ();
  if (thr->task != NULL)
    {
      struct gomp_task *task = thr->task;
      gomp_end_task ();
      free (task);
    }
}

/* Launch a team.  */

#ifdef LIBGOMP_USE_PTHREADS
static unsigned
gomp_barrier_wait_n (unsigned num_new_threads, unsigned num_exiting_threads,
		     unsigned num_remapped_threads, unsigned old_threads_used,
		     unsigned nthreads, unsigned affinity_count, bool can_hold)
{
  if (can_hold)
    {
      unsigned tmp
	= num_new_threads + num_exiting_threads + num_remapped_threads;
      return tmp ? tmp + 1 : 0;
    }
  if (!affinity_count)
    return old_threads_used > nthreads ? old_threads_used : nthreads;
  return nthreads + affinity_count;
}

void
gomp_team_start (void (*fn) (void *), void *data, unsigned nthreads,
		 unsigned flags, struct gomp_team *team,
		 struct gomp_taskgroup *taskgroup)
{
  struct gomp_thread_start_data *start_data = NULL;
  struct gomp_thread *thr, *nthr;
  struct gomp_task *task;
  struct gomp_task_icv *icv;
  bool nested;
  struct gomp_thread_pool *pool;
  unsigned i, n, old_threads_used = 0;
  unsigned simple_barrier_n = 0;
  unsigned num_new_threads = 0;
  unsigned num_remapped_threads = 0;
  unsigned spawned_new_threads __attribute__ ((unused)) = 0;
  unsigned num_exiting_threads = 0;
  pthread_attr_t thread_attr, *attr;
  unsigned long nthreads_var;
  char bind, bind_var;
  unsigned int s = 0, rest = 0, p = 0, k = 0;
  unsigned int affinity_count = 0;
  struct gomp_thread **affinity_thr = NULL;
  bool force_display = false;

  thr = gomp_thread ();
  nested = thr->ts.level;
  pool = thr->thread_pool;
  task = thr->task;
  icv = task ? &task->icv : &gomp_global_icv;
  if (__builtin_expect (gomp_places_list != NULL, 0) && thr->place == 0)
    {
      gomp_init_affinity ();
      if (__builtin_expect (gomp_display_affinity_var, 0) && nthreads == 1)
	gomp_display_affinity_thread (gomp_thread_self (), &thr->ts,
				      thr->place);
    }

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
#ifdef HAVE_SYNC_BUILTINS
  thr->ts.single_count = 0;
#endif
  thr->ts.static_trip = 0;
  thr->task = &team->implicit_task[0];
#ifdef GOMP_NEEDS_THREAD_HANDLE
  thr->handle = pthread_self ();
#endif
  nthreads_var = icv->nthreads_var;
  if (__builtin_expect (gomp_nthreads_var_list != NULL, 0)
      && thr->ts.level < gomp_nthreads_var_list_len)
    nthreads_var = gomp_nthreads_var_list[thr->ts.level];
  bind_var = icv->bind_var;
  if (bind_var != omp_proc_bind_false && (flags & 7) != omp_proc_bind_false)
    bind_var = flags & 7;
  bind = bind_var;
  if (__builtin_expect (gomp_bind_var_list != NULL, 0)
      && thr->ts.level < gomp_bind_var_list_len)
    bind_var = gomp_bind_var_list[thr->ts.level];
  gomp_init_task (thr->task, task, icv);
  thr->task->taskgroup = taskgroup;
  team->implicit_task[0].icv.nthreads_var = nthreads_var;
  team->implicit_task[0].icv.bind_var = bind_var;

  if (nthreads == 1)
    return;

  i = 1;

  if (__builtin_expect (gomp_places_list != NULL, 0))
    {
      /* Depending on chosen proc_bind model, set subpartition
	 for the master thread and initialize helper variables
	 P and optionally S, K and/or REST used by later place
	 computation for each additional thread.  */
      p = thr->place - 1;
      switch (bind)
	{
	case omp_proc_bind_true:
	case omp_proc_bind_close:
	  if (nthreads > thr->ts.place_partition_len)
	    {
	      /* T > P.  S threads will be placed in each place,
		 and the final REM threads placed one by one
		 into the already occupied places.  */
	      s = nthreads / thr->ts.place_partition_len;
	      rest = nthreads % thr->ts.place_partition_len;
	    }
	  else
	    s = 1;
	  k = 1;
	  break;
	case omp_proc_bind_master:
	  /* Each thread will be bound to master's place.  */
	  break;
	case omp_proc_bind_spread:
	  if (nthreads <= thr->ts.place_partition_len)
	    {
	      /* T <= P.  Each subpartition will have in between s
		 and s+1 places (subpartitions starting at or
		 after rest will have s places, earlier s+1 places),
		 each thread will be bound to the first place in
		 its subpartition (except for the master thread
		 that can be bound to another place in its
		 subpartition).  */
	      s = thr->ts.place_partition_len / nthreads;
	      rest = thr->ts.place_partition_len % nthreads;
	      rest = (s + 1) * rest + thr->ts.place_partition_off;
	      if (p < rest)
		{
		  p -= (p - thr->ts.place_partition_off) % (s + 1);
		  thr->ts.place_partition_len = s + 1;
		}
	      else
		{
		  p -= (p - rest) % s;
		  thr->ts.place_partition_len = s;
		}
	      thr->ts.place_partition_off = p;
	    }
	  else
	    {
	      /* T > P.  Each subpartition will have just a single
		 place and we'll place between s and s+1
		 threads into each subpartition.  */
	      s = nthreads / thr->ts.place_partition_len;
	      rest = nthreads % thr->ts.place_partition_len;
	      thr->ts.place_partition_off = p;
	      thr->ts.place_partition_len = 1;
	      k = 1;
	    }
	  break;
	}
    }
  else
    bind = omp_proc_bind_false;

  /* Copy entirely rather than copy pointer so that we have an immutable
     copy for the secondary threads to take from for the time-frame that
     said secondary threads are executing on.  Can't copy from
     `team->implicit_task[0].icv` because primary thread could have adjusted
     its per-task ICV by the time the secondary thread gets around to
     initialising things.  */
  team->ti.task_icv_init = team->implicit_task[0].icv;
  team->ti.task_parent_init = task;
  team->ti.task_taskgroup = taskgroup;
  team->ti.num_teams = thr->num_teams;
  team->ti.team_num = thr->team_num;
  team->ti.fn = fn;
  team->ti.data = data;
  team->ti.def_allocator = thr->ts.def_allocator;

  /* We only allow the reuse of idle threads for non-nested PARALLEL
     regions.  This appears to be implied by the semantics of
     threadprivate variables, but perhaps that's reading too much into
     things.  Certainly it does prevent any locking problems, since
     only the initial program thread will modify gomp_threads.  */
  if (!nested)
    {
      gomp_assert (team->prev_ts.team_id == 0,
		   "starting a team from thread with id %u in previous team\n",
		   team->prev_ts.team_id);
      old_threads_used = pool->threads_used;

      if (team->reused
	  && team->ppv.place_partition_off == thr->ts.place_partition_off
	  && team->ppv.place_partition_len == thr->ts.place_partition_len
	  && team->ppv.bind == bind)
	{
	  gomp_assert (nthreads == old_threads_used,
		       "odd number of threads when re-using team: "
		       "nthreads = %u old_threads_used = %u\n",
		       nthreads, old_threads_used);
	  goto do_release;
	}
      else
	{
	  team->ppv.place_partition_off = thr->ts.place_partition_off;
	  team->ppv.place_partition_len = thr->ts.place_partition_len;
	  team->ppv.bind = bind;
	}


      if (nthreads == old_threads_used)
	{
	  n = nthreads;
	  num_new_threads = 0;
	  num_exiting_threads = 0;
	  simple_barrier_n
	    = gomp_barrier_wait_n (num_new_threads, num_exiting_threads,
				   num_remapped_threads, old_threads_used,
				   nthreads, affinity_count,
				   gomp_barrier_can_hold (pool->prev_barrier));
	  if (gomp_barrier_can_hold (pool->prev_barrier))
	    gomp_simple_barrier_reinit (&pool->threads_dock, simple_barrier_n);
	}
      else if (nthreads < old_threads_used)
	{
	  n = nthreads;
	  num_new_threads = 0;
	  num_exiting_threads = old_threads_used - nthreads;
	  simple_barrier_n
	    = gomp_barrier_wait_n (num_new_threads, num_exiting_threads,
				   num_remapped_threads, old_threads_used,
				   nthreads, affinity_count,
				   gomp_barrier_can_hold (pool->prev_barrier));
	  if (gomp_barrier_can_hold (pool->prev_barrier))
	    gomp_simple_barrier_reinit (&pool->threads_dock, simple_barrier_n);
	}
      else if (old_threads_used == 0)
	{
	  n = 0;
	  num_new_threads = nthreads - 1;
	  num_exiting_threads = 0;
	  simple_barrier_n = nthreads;
	  gomp_simple_barrier_init (&pool->threads_dock, nthreads);
	}
      else
	{
	  n = old_threads_used;
	  num_new_threads = nthreads - old_threads_used;
	  num_exiting_threads = 0;
	  simple_barrier_n
	    = gomp_barrier_wait_n (num_new_threads, num_exiting_threads,
				   num_remapped_threads, old_threads_used,
				   nthreads, affinity_count,
				   gomp_barrier_can_hold (pool->prev_barrier));
	  /* Increase the barrier threshold to make sure all new
	     threads arrive before the team is released.  */
	  gomp_simple_barrier_reinit (&pool->threads_dock, simple_barrier_n);
	}

      /* Not true yet, but soon will be.  We're going to release all
	 threads from the dock, and those that aren't part of the
	 team will exit.  */
      pool->threads_used = nthreads;

      /* If necessary, expand the size of the gomp_threads array.  It is
	 expected that changes in the number of threads are rare, thus we
	 make no effort to expand gomp_threads_size geometrically.  */
      if (nthreads >= pool->threads_size)
	{
	  pool->threads_size = nthreads + 1;
	  pool->threads
	    = gomp_realloc (pool->threads,
			    pool->threads_size
			    * sizeof (struct gomp_thread *));
	  /* Add current (master) thread to threads[].  */
	  pool->threads[0] = thr;
	}

      /* Release existing idle threads.  */
      for (; i < n; ++i)
	{
	  unsigned int place_partition_off = thr->ts.place_partition_off;
	  unsigned int place_partition_len = thr->ts.place_partition_len;
	  unsigned int place = 0;
	  if (__builtin_expect (gomp_places_list != NULL, 0))
	    {
	      switch (bind)
		{
		case omp_proc_bind_true:
		case omp_proc_bind_close:
		  if (k == s)
		    {
		      ++p;
		      if (p == (team->prev_ts.place_partition_off
				+ team->prev_ts.place_partition_len))
			p = team->prev_ts.place_partition_off;
		      k = 1;
		      if (i == nthreads - rest)
			s = 1;
		    }
		  else
		    ++k;
		  break;
		case omp_proc_bind_master:
		  break;
		case omp_proc_bind_spread:
		  if (k == 0)
		    {
		      /* T <= P.  */
		      if (p < rest)
			p += s + 1;
		      else
			p += s;
		      if (p == (team->prev_ts.place_partition_off
				+ team->prev_ts.place_partition_len))
			p = team->prev_ts.place_partition_off;
		      place_partition_off = p;
		      if (p < rest)
			place_partition_len = s + 1;
		      else
			place_partition_len = s;
		    }
		  else
		    {
		      /* T > P.  */
		      if (k == s)
			{
			  ++p;
			  if (p == (team->prev_ts.place_partition_off
				    + team->prev_ts.place_partition_len))
			    p = team->prev_ts.place_partition_off;
			  k = 1;
			  if (i == nthreads - rest)
			    s = 1;
			}
		      else
			++k;
		      place_partition_off = p;
		      place_partition_len = 1;
		    }
		  break;
		}
	      if (affinity_thr != NULL
		  || (bind != omp_proc_bind_true
		      && pool->threads[i]->place != p + 1)
		  || pool->threads[i]->place <= place_partition_off
		  || pool->threads[i]->place > (place_partition_off
						+ place_partition_len))
		{
		  unsigned int l;
		  force_display = true;
		  if (affinity_thr == NULL)
		    {
		      unsigned int j;

		      if (team->prev_ts.place_partition_len > 64)
			affinity_thr
			  = gomp_malloc (team->prev_ts.place_partition_len
					 * sizeof (struct gomp_thread *));
		      else
			affinity_thr
			  = gomp_alloca (team->prev_ts.place_partition_len
					 * sizeof (struct gomp_thread *));
		      memset (affinity_thr, '\0',
			      team->prev_ts.place_partition_len
			      * sizeof (struct gomp_thread *));
		      for (j = i; j < old_threads_used; j++)
			{
			  if (pool->threads[j]->place
			      > team->prev_ts.place_partition_off
			      && (pool->threads[j]->place
				  <= (team->prev_ts.place_partition_off
				      + team->prev_ts.place_partition_len)))
			    {
			      l = pool->threads[j]->place - 1
				  - team->prev_ts.place_partition_off;
			      pool->threads[j]->data = affinity_thr[l];
			      affinity_thr[l] = pool->threads[j];
			    }
			  else
			    pool->threads[j]->exit = true;
			  pool->threads[j] = NULL;
			}
		      if (nthreads > old_threads_used)
			memset (&pool->threads[old_threads_used],
				'\0', ((nthreads - old_threads_used)
				       * sizeof (struct gomp_thread *)));
		      n = nthreads;
		      affinity_count = old_threads_used - i;
		    }
		  if (affinity_count == 0)
		    break;
		  l = p;
		  if (affinity_thr[l - team->prev_ts.place_partition_off]
		      == NULL)
		    {
		      if (bind != omp_proc_bind_true)
			continue;
		      for (l = place_partition_off;
			   l < place_partition_off + place_partition_len;
			   l++)
			if (affinity_thr[l - team->prev_ts.place_partition_off]
			    != NULL)
			  break;
		      if (l == place_partition_off + place_partition_len)
			continue;
		    }
		  nthr = affinity_thr[l - team->prev_ts.place_partition_off];
		  affinity_thr[l - team->prev_ts.place_partition_off]
		    = (struct gomp_thread *) nthr->data;
		  affinity_count--;
		  pool->threads[i] = nthr;
		}
	      else
		nthr = pool->threads[i];
	      place = p + 1;
	    }
	  else
	    nthr = pool->threads[i];
	  __atomic_store_n (&nthr->ts.team, team, MEMMODEL_RELEASE);
	  if (team->reused && nthr->ts.team_id != i)
	    num_remapped_threads++;
	  nthr->ts.team_id = i;
	  gomp_assert (nthr->ts.level == 1 && team->prev_ts.level == 0,
		       "level indicators surprising: nthr: %u, team->prev_ts: "
		       "%u\n", nthr->ts.level, team->prev_ts.level);
	  gomp_assert (nthr->ts.active_level == 1 && thr->ts.active_level == 1,
		       "active_level indicators surprising: nthr: %u, thr: "
		       "%u\n", nthr->ts.active_level, thr->ts.active_level);
	  nthr->ts.place_partition_off = place_partition_off;
	  nthr->ts.place_partition_len = place_partition_len;
	  nthr->place = place;
	  team->ordered_release[i] = &nthr->release;
	}

      if (__builtin_expect (affinity_thr != NULL, 0))
	{
	  /* Mark each of the threads that are exiting as such.  Hence in
	     `gomp_thread_start` they exit their main loop.  */
	  for (unsigned l = 0; l < team->prev_ts.place_partition_len; l++)
	    {
	      struct gomp_thread *t = affinity_thr[l];
	      while (t)
		{
		  struct gomp_thread *next = (struct gomp_thread *) t->data;
		  t->exit = true;
		  t = next;
		}
	    }
	  /* If AFFINITY_THR is non-NULL just because we had to
	     permute some threads in the pool, but we've managed
	     to find exactly as many old threads as we'd find
	     without affinity, we don't need to handle this
	     specially anymore.  */
	  if (nthreads <= old_threads_used
	      ? (affinity_count == old_threads_used - nthreads)
	      : (i == old_threads_used))
	    {
	      if (team->prev_ts.place_partition_len > 64)
		free (affinity_thr);
	      affinity_thr = NULL;
	      affinity_count = 0;
	    }
	  else
	    {
	      i = 1;
	      /* We are going to compute the places/subpartitions
		 again from the beginning.  So, we need to reinitialize
		 vars modified by the switch (bind) above inside
		 of the loop, to the state they had after the initial
		 switch (bind).  */
	      switch (bind)
		{
		case omp_proc_bind_true:
		case omp_proc_bind_close:
		  if (nthreads > thr->ts.place_partition_len)
		    /* T > P.  S has been changed, so needs
		       to be recomputed.  */
		    s = nthreads / thr->ts.place_partition_len;
		  k = 1;
		  p = thr->place - 1;
		  break;
		case omp_proc_bind_master:
		  /* No vars have been changed.  */
		  break;
		case omp_proc_bind_spread:
		  p = thr->ts.place_partition_off;
		  if (k != 0)
		    {
		      /* T > P.  */
		      s = nthreads / team->prev_ts.place_partition_len;
		      k = 1;
		    }
		  break;
		}

	      /* Increase the barrier threshold to make sure all new
		 threads and all the threads we're going to let die
		 arrive before the team is released.  */
	      if (affinity_count)
		{
		  /* Total number of threads running is:
		     nthreads + affinity_count
		     Equation to be satisfied is:
		      (nthreads + affinity_count ==
		       old_threads_used + num_new_threads).
		     So num_new_threads
			 == (nthreads + affinity_count - old_threads_used)
		     Originally
		      num_new_threads == nthreads > old_threads_used
			  ? (nthreads - old_threads_used)
			  : 0;

		     Now:
		       nthreads + num_exiting_threads
			 == old_threads_used + num_new_threads
		     So:
		       num_exiting_threads
			 == old_threads_used + num_new_threads - nthreads
			 == affinity_count
		     */
		  gomp_assert (nthreads + affinity_count > old_threads_used,
			       "not spawning more threads but "
			       "reinitialising barrier "
			       "nthreads = %u"
			       ", affinity_count = %u"
			       ", old_threads_used = %u",
			       nthreads, affinity_count, old_threads_used);
		  num_new_threads
		    = (nthreads + affinity_count) - old_threads_used;
		  num_exiting_threads = affinity_count;
		}
	    }
	}
      else
	for (unsigned j = n; j < old_threads_used; j++)
	  pool->threads[j]->exit = true;

      /* Account for any affinity changes & remapped threads.  */
      simple_barrier_n
	= gomp_barrier_wait_n (num_new_threads, num_exiting_threads,
			       num_remapped_threads, old_threads_used, nthreads,
			       affinity_count,
			       gomp_barrier_can_hold (pool->prev_barrier));
      gomp_simple_barrier_reinit (&pool->threads_dock, simple_barrier_n);

      if (i == nthreads)
	{
	  gomp_assert (num_new_threads == 0,
		       "calculated need %u new threads but spawning none",
		       num_new_threads);
	  gomp_assert (gomp_barrier_can_hold (pool->prev_barrier)
			 ? (simple_barrier_n
			    == ((num_exiting_threads || num_remapped_threads)
				? (num_exiting_threads + num_remapped_threads
				   + 1)
				: 0))
			 : simple_barrier_n == old_threads_used,
		       "have calculated need simple_barrier_n == %u"
		       " starting 0 threads, affinity_count == %u, "
		       "old_threads_used == %u, calculated "
		       "num_exiting_threads == %u, nthreads == %u, "
		       "num_remapped_threads == %u",
		       simple_barrier_n, affinity_count, old_threads_used,
		       num_exiting_threads, nthreads, num_remapped_threads);
	  goto do_release;
	}
    }

  if (__builtin_expect (nthreads + affinity_count > old_threads_used, 0))
    {
      long diff = (long) (nthreads + affinity_count) - (long) old_threads_used;

      if (old_threads_used == 0)
	--diff;

#ifdef HAVE_SYNC_BUILTINS
      __sync_fetch_and_add (&gomp_managed_threads, diff);
#else
      gomp_mutex_lock (&gomp_managed_threads_lock);
      gomp_managed_threads += diff;
      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
    }

  attr = &gomp_thread_attr;
  if (__builtin_expect (gomp_places_list != NULL, 0))
    {
      size_t stacksize;
      pthread_attr_init (&thread_attr);
      if (! pthread_attr_getstacksize (&gomp_thread_attr, &stacksize))
	pthread_attr_setstacksize (&thread_attr, stacksize);
      attr = &thread_attr;
    }

  if (i >= nthreads)
    __builtin_unreachable ();
  start_data = gomp_alloca (sizeof (struct gomp_thread_start_data)
			    * (nthreads - i));

  /* Launch new threads.  */
  for (; i < nthreads; ++i)
    {
      int err;

      start_data->place_partition_off = thr->ts.place_partition_off;
      start_data->place_partition_len = thr->ts.place_partition_len;
      start_data->place = 0;
      if (__builtin_expect (gomp_places_list != NULL, 0))
	{
	  switch (bind)
	    {
	    case omp_proc_bind_true:
	    case omp_proc_bind_close:
	      if (k == s)
		{
		  ++p;
		  if (p == (team->prev_ts.place_partition_off
			    + team->prev_ts.place_partition_len))
		    p = team->prev_ts.place_partition_off;
		  k = 1;
		  if (i == nthreads - rest)
		    s = 1;
		}
	      else
		++k;
	      break;
	    case omp_proc_bind_master:
	      break;
	    case omp_proc_bind_spread:
	      if (k == 0)
		{
		  /* T <= P.  */
		  if (p < rest)
		    p += s + 1;
		  else
		    p += s;
		  if (p == (team->prev_ts.place_partition_off
			    + team->prev_ts.place_partition_len))
		    p = team->prev_ts.place_partition_off;
		  start_data->place_partition_off = p;
		  if (p < rest)
		    start_data->place_partition_len = s + 1;
		  else
		    start_data->place_partition_len = s;
		}
	      else
		{
		  /* T > P.  */
		  if (k == s)
		    {
		      ++p;
		      if (p == (team->prev_ts.place_partition_off
				+ team->prev_ts.place_partition_len))
			p = team->prev_ts.place_partition_off;
		      k = 1;
		      if (i == nthreads - rest)
			s = 1;
		    }
		  else
		    ++k;
		  start_data->place_partition_off = p;
		  start_data->place_partition_len = 1;
		}
	      break;
	    }
	  start_data->place = p + 1;
	  if (affinity_thr != NULL && pool->threads[i] != NULL)
	    continue;
	  gomp_init_thread_affinity (attr, p);
	}

      start_data->team = team;
      start_data->team_id = i;

      start_data->active_level = thr->ts.active_level;
      start_data->thread_pool = pool;
      start_data->nested = nested;
      attr = gomp_adjust_thread_attr (attr, &thread_attr);
      err = pthread_create (&start_data->handle, attr, gomp_thread_start,
			    start_data);
      spawned_new_threads += 1;
      start_data++;
      if (err != 0)
	gomp_fatal ("Thread creation failed: %s", strerror (err));
    }
  if (!nested)
    {
      gomp_assert (spawned_new_threads == num_new_threads,
		   "calculated num_new_threads != spawned_new_threads: "
		   " %u != %u",
		   num_new_threads, spawned_new_threads);
      bool calc_check __attribute__ ((unused)) = false;
      if (old_threads_used == 0)
	calc_check = (num_new_threads == (simple_barrier_n - 1));
      else if (gomp_barrier_can_hold (pool->prev_barrier))
	calc_check
	  = ((num_new_threads + num_exiting_threads + num_remapped_threads)
	     == (simple_barrier_n - 1));
      else
	calc_check = (num_new_threads == (simple_barrier_n - old_threads_used));
      gomp_assert (calc_check,
		   "simple barrier calculation incorrect for %s team: "
		   "simple_barrier_n: %u"
		   ", nthreads: %u"
		   ", num_new_threads: %u"
		   ", num_exiting_threads: %u"
		   ", num_remapped_threads: %u"
		   ", old_threads_used: %u",
		   nested ? "nested" : "top-level", simple_barrier_n, nthreads,
		   num_new_threads, num_exiting_threads, num_remapped_threads,
		   old_threads_used);
    }

  if (__builtin_expect (attr == &thread_attr, 0))
    pthread_attr_destroy (&thread_attr);

 do_release:
  if (nested)
    gomp_barrier_wait (&team->barrier, thr->ts.team_id);
  else
    {
      gomp_release_held_threads (pool, team, thr->ts.team_id);
      if (num_new_threads
	  || num_exiting_threads
	  || num_remapped_threads
	  || !gomp_barrier_can_hold (pool->prev_barrier))
	gomp_simple_barrier_wait (&pool->threads_dock);
    }

  /* Decrease the barrier threshold to match the number of threads
     that should arrive back at the end of this team.  The extra
     threads should be exiting.  Note that we arrange for this test
     to never be true for nested teams.  If AFFINITY_COUNT is non-zero,
     the barrier as well as gomp_managed_threads was temporarily
     set to NTHREADS + AFFINITY_COUNT.  For NTHREADS < OLD_THREADS_COUNT,
     AFFINITY_COUNT if non-zero will be always at least
     OLD_THREADS_COUNT - NTHREADS.  */
  if (__builtin_expect (nthreads < old_threads_used, 0)
      || __builtin_expect (affinity_count, 0))
    {
      long diff = (long) nthreads - (long) old_threads_used;

      if (affinity_count)
	diff = -affinity_count;

      gomp_simple_barrier_reinit (&pool->threads_dock, nthreads);

#ifdef HAVE_SYNC_BUILTINS
      __sync_fetch_and_add (&gomp_managed_threads, diff);
#else
      gomp_mutex_lock (&gomp_managed_threads_lock);
      gomp_managed_threads += diff;
      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
    }
  else if (!nested && gomp_barrier_can_hold (pool->prev_barrier))
    {
      /* Need to adjust the simple barrier threshold once it's been used in
	 order to account for all threads that would need to be waited on in
	 something like `gomp_free_thread` or `gomp_pause_thread`.  */
      gomp_simple_barrier_reinit (&pool->threads_dock, nthreads);
    }

  if (__builtin_expect (gomp_display_affinity_var, 0))
    {
      if (nested
	  || nthreads != old_threads_used
	  || force_display)
	{
	  gomp_display_affinity_thread (gomp_thread_self (), &thr->ts,
					thr->place);
	  if (nested)
	    {
	      start_data -= nthreads - 1;
	      struct gomp_team_state ts;
	      for (i = 1; i < nthreads; ++i)
		{
		  gomp_init_team_state (&ts, team, start_data->team_id,
					start_data->active_level,
					start_data->place_partition_off,
					start_data->place_partition_len);
		  gomp_display_affinity_thread (
#ifdef LIBGOMP_USE_PTHREADS
		    start_data->handle,
#else
		    gomp_thread_self (),
#endif
		    &ts, start_data->place);
		  start_data++;
		}
	    }
	  else
	    {
	      for (i = 1; i < nthreads; ++i)
		{
		  gomp_thread_handle handle
		    = gomp_thread_to_pthread_t (pool->threads[i]);
		  gomp_display_affinity_thread (handle, &pool->threads[i]->ts,
						pool->threads[i]->place);
		}
	    }
	}
    }
  if (__builtin_expect (affinity_thr != NULL, 0)
      && team->prev_ts.place_partition_len > 64)
    free (affinity_thr);
}
#endif


/* Terminate the current team.  This is only to be called by the master
   thread.  We assume that we must wait for the other threads.  */

void
gomp_team_end (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  unsigned team_id = thr->ts.team_id;

  /* This barrier handles all pending explicit threads.
     As #pragma omp cancel parallel might get awaited count in
     team->barrier in a inconsistent state, we need to use a different
     counter here.  */
  gomp_team_barrier_wait_final (&team->barrier, team_id);
  /* After this point all tasking has finished.  Just haven't told all
     secondary threads that they're good to continue.  */
  if (__builtin_expect (team->team_cancelled, 0))
    {
      struct gomp_work_share *ws = team->work_shares_to_free;
      do
	{
	  struct gomp_work_share *next_ws = gomp_ptrlock_get (&ws->next_ws);
	  if (next_ws == NULL)
	    gomp_ptrlock_set (&ws->next_ws, ws);
	  gomp_fini_work_share (ws);
	  ws = next_ws;
	}
      while (ws != NULL);
    }
  else
    gomp_fini_work_share (thr->ts.work_share);

  gomp_end_task ();
  thr->ts = team->prev_ts;

  bool finished_threads __attribute__ ((unused)) = false;
  if (__builtin_expect (thr->ts.level != 0, 0))
    {
      gomp_team_barrier_done_final (&team->barrier, team_id);
      finished_threads = true;
#ifdef HAVE_SYNC_BUILTINS
      __sync_fetch_and_add (&gomp_managed_threads, 1L - team->nthreads);
#else
      gomp_mutex_lock (&gomp_managed_threads_lock);
      gomp_managed_threads -= team->nthreads - 1L;
      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
      /* This barrier has gomp_barrier_wait_last counterparts
	 and ensures the team can be safely destroyed.  */
      gomp_barrier_wait (&team->barrier, team_id);
    }

  if (__builtin_expect (team->work_shares[0].next_alloc != NULL, 0))
    {
      struct gomp_work_share *ws = team->work_shares[0].next_alloc;
      do
	{
	  struct gomp_work_share *next_ws = ws->next_alloc;
	  free (ws);
	  ws = next_ws;
	}
      while (ws != NULL);
    }
  gomp_sem_destroy (&team->master_release);

  if (__builtin_expect (thr->ts.team != NULL
			  && (thr->ts.team->nthreads != 1
			      || thr->ts.level != 0),
			0)
      || __builtin_expect (team->nthreads == 1, 0))
    {
      gomp_assert (finished_threads || team->nthreads == 1,
		   "freeing team while threads may be waiting on it."
		   " team = %p, level = %u",
		   team, thr->ts.level + 1);
      free_team (team);
    }
  else
    {
      struct gomp_thread_pool *pool = thr->thread_pool;
      if (pool->last_team)
	free_team (pool->last_team);
      pool->last_team = team;
      gomp_assert (!finished_threads,
		   "have let threads go while still recording prev_barrier"
		   " level = %u, team = %p, nthreads = %u",
		   thr->ts.level + 1, team, team->nthreads);
      gomp_assert (!pool->prev_barrier,
		   "previous barrier not released before it gets overridden"
		   " level = %u, team = %p, nthreads = %u",
		   thr->ts.level + 1, team, team->nthreads);
      /* Only record `prev_barrier` when `gomp_barrier_can_hold`:
	 - It's only used when gomp_barrier_can_hold.
	 - Some targets have custom versions of `gomp_team_start` that doesn't
	   call `gomp_release_held_threads` and hence don't clear this value.
	 */
      if (gomp_barrier_can_hold (&team->barrier))
	pool->prev_barrier = &team->barrier;
      /* Releasing pool should only happen if we're going to then
	 `gomp_get_thread_pool` later.  When `thr->ts.team != NULL` we don't
	 fetch the last team and call `gomp_get_thread_pool`.  Hence we don't
	 want to release the pool here.  */
      if (thr->ts.team == NULL)
	gomp_release_thread_pool (pool);
    }
}

#ifdef LIBGOMP_USE_PTHREADS

/* Constructors for this file.  */

static void __attribute__((constructor))
initialize_team (void)
{
#if !defined HAVE_TLS && !defined USE_EMUTLS
  static struct gomp_thread initial_thread_tls_data;

  pthread_key_create (&gomp_tls_key, NULL);
  pthread_setspecific (gomp_tls_key, &initial_thread_tls_data);
#endif

  if (pthread_key_create (&gomp_thread_destructor, gomp_free_thread) != 0)
    gomp_fatal ("could not create thread pool destructor.");
}

static void __attribute__((destructor))
team_destructor (void)
{
  /* Without this dlclose on libgomp could lead to subsequent
     crashes.  */
  pthread_key_delete (gomp_thread_destructor);
}

/* Similar to gomp_free_pool_helper, but don't detach itself,
   gomp_pause_host will pthread_join those threads.  */

static void
gomp_pause_pool_helper (void *thread_pool)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_thread_pool *pool
    = (struct gomp_thread_pool *) thread_pool;
  gomp_simple_barrier_wait_last (&pool->threads_dock);
  gomp_sem_destroy (&thr->release);
  thr->thread_pool = NULL;
  thr->task = NULL;
  pthread_exit (NULL);
}

/* Free a thread pool and release its threads.  Return non-zero on
   failure.  */

int
gomp_pause_host (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_thread_pool *pool = thr->thread_pool;
  if (thr->ts.level)
    return -1;
  if (pool)
    {
      if (pool->threads_used > 0)
	{
	  int i;
	  pthread_t *thrs
	    = gomp_alloca (sizeof (pthread_t) * pool->threads_used);
	  for (i = 1; i < pool->threads_used; i++)
	    {
	      struct gomp_thread *nthr = pool->threads[i];
	      nthr->fn = gomp_pause_pool_helper;
	      nthr->data = pool;
	      thrs[i] = gomp_thread_to_pthread_t (nthr);
	    }
	  /* This barrier undocks threads docked on pool->threads_dock.  */
	  gomp_release_held_threads (pool, NULL, thr->ts.team_id);
	  if (!gomp_barrier_can_hold (pool->prev_barrier))
	    gomp_simple_barrier_wait (&pool->threads_dock);
	  /* And this waits till all threads have called
	     gomp_simple_barrier_wait_last in gomp_pause_pool_helper.  */
	  gomp_simple_barrier_wait (&pool->threads_dock);
	  /* Now it is safe to destroy the barrier and free the pool.  */
	  gomp_simple_barrier_destroy (&pool->threads_dock);

#ifdef HAVE_SYNC_BUILTINS
	  __sync_fetch_and_add (&gomp_managed_threads,
				1L - pool->threads_used);
#else
	  gomp_mutex_lock (&gomp_managed_threads_lock);
	  gomp_managed_threads -= pool->threads_used - 1L;
	  gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
	  for (i = 1; i < pool->threads_used; i++)
	    pthread_join (thrs[i], NULL);
	}
      if (pool->last_team)
	free_team (pool->last_team);
#ifndef __nvptx__
      team_free (pool->threads);
      team_free (pool);
#endif
      thr->thread_pool = NULL;
    }
  return 0;
}
#endif

struct gomp_task_icv *
gomp_new_icv (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_task *task = gomp_malloc (sizeof (struct gomp_task));
  gomp_init_task (task, NULL, &gomp_global_icv);
  thr->task = task;
#ifdef LIBGOMP_USE_PTHREADS
  pthread_setspecific (gomp_thread_destructor, thr);
#endif
  return &task->icv;
}
