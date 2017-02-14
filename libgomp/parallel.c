/* Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

/* This file handles the (bare) PARALLEL construct.  */

#include "libgomp.h"
#include <limits.h>


/* Determine the number of threads to be launched for a PARALLEL construct.
   This algorithm is explicitly described in OpenMP 3.0 section 2.4.1.
   SPECIFIED is a combination of the NUM_THREADS clause and the IF clause.
   If the IF clause is false, SPECIFIED is forced to 1.  When NUM_THREADS
   is not present, SPECIFIED is 0.  */

unsigned
gomp_resolve_num_threads (unsigned specified, unsigned count)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_task_icv *icv;
  unsigned threads_requested, max_num_threads, num_threads;
  unsigned long busy;
  struct gomp_thread_pool *pool;

  icv = gomp_icv (false);

  if (specified == 1)
    return 1;
  else if (thr->ts.active_level >= 1 && !icv->nest_var)
    return 1;
  else if (thr->ts.active_level >= gomp_max_active_levels_var)
    return 1;

  /* If NUM_THREADS not specified, use nthreads_var.  */
  if (specified == 0)
    threads_requested = icv->nthreads_var;
  else
    threads_requested = specified;

  max_num_threads = threads_requested;

  /* If dynamic threads are enabled, bound the number of threads
     that we launch.  */
  if (icv->dyn_var)
    {
      unsigned dyn = gomp_dynamic_max_threads ();
      if (dyn < max_num_threads)
	max_num_threads = dyn;

      /* Optimization for parallel sections.  */
      if (count && count < max_num_threads)
	max_num_threads = count;
    }

  /* UINT_MAX stands for infinity.  */
  if (__builtin_expect (icv->thread_limit_var == UINT_MAX, 1)
      || max_num_threads == 1)
    return max_num_threads;

  /* The threads_busy counter lives in thread_pool, if there
     isn't a thread_pool yet, there must be just one thread
     in the contention group.  If thr->team is NULL, this isn't
     nested parallel, so there is just one thread in the
     contention group as well, no need to handle it atomically.  */
  pool = thr->thread_pool;
  if (thr->ts.team == NULL || pool == NULL)
    {
      num_threads = max_num_threads;
      if (num_threads > icv->thread_limit_var)
	num_threads = icv->thread_limit_var;
      if (pool)
	pool->threads_busy = num_threads;
      return num_threads;
    }

#ifdef HAVE_SYNC_BUILTINS
  do
    {
      busy = pool->threads_busy;
      num_threads = max_num_threads;
      if (icv->thread_limit_var - busy + 1 < num_threads)
	num_threads = icv->thread_limit_var - busy + 1;
    }
  while (__sync_val_compare_and_swap (&pool->threads_busy,
				      busy, busy + num_threads - 1)
	 != busy);
#else
  gomp_mutex_lock (&gomp_managed_threads_lock);
  num_threads = max_num_threads;
  busy = pool->threads_busy;
  if (icv->thread_limit_var - busy + 1 < num_threads)
    num_threads = icv->thread_limit_var - busy + 1;
  pool->threads_busy += num_threads - 1;
  gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif

  return num_threads;
}

void
GOMP_parallel_start (void (*fn) (void *), void *data, unsigned num_threads)
{
  num_threads = gomp_resolve_num_threads (num_threads, 0);
  gomp_team_start (fn, data, num_threads, 0, gomp_new_team (num_threads));
}

void
GOMP_parallel_end (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  if (__builtin_expect (icv->thread_limit_var != UINT_MAX, 0))
    {
      struct gomp_thread *thr = gomp_thread ();
      struct gomp_team *team = thr->ts.team;
      unsigned int nthreads = team ? team->nthreads : 1;
      gomp_team_end ();
      if (nthreads > 1)
	{
	  /* If not nested, there is just one thread in the
	     contention group left, no need for atomicity.  */
	  if (thr->ts.team == NULL)
	    thr->thread_pool->threads_busy = 1;
	  else
	    {
#ifdef HAVE_SYNC_BUILTINS
	      __sync_fetch_and_add (&thr->thread_pool->threads_busy,
				    1UL - nthreads);
#else
	      gomp_mutex_lock (&gomp_managed_threads_lock);
	      thr->thread_pool->threads_busy -= nthreads - 1;
	      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
	    }
	}
    }
  else
    gomp_team_end ();
}
ialias (GOMP_parallel_end)

void
GOMP_parallel (void (*fn) (void *), void *data, unsigned num_threads, unsigned int flags)
{
  num_threads = gomp_resolve_num_threads (num_threads, 0);
  gomp_team_start (fn, data, num_threads, flags, gomp_new_team (num_threads));
  fn (data);
  ialias_call (GOMP_parallel_end) ();
}

bool
GOMP_cancellation_point (int which)
{
  if (!gomp_cancel_var)
    return false;

  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  if (which & (GOMP_CANCEL_LOOP | GOMP_CANCEL_SECTIONS))
    {
      if (team == NULL)
	return false;
      return team->work_share_cancelled != 0;
    }
  else if (which & GOMP_CANCEL_TASKGROUP)
    {
      if (thr->task->taskgroup && thr->task->taskgroup->cancelled)
	return true;
      /* FALLTHRU into the GOMP_CANCEL_PARALLEL case,
	 as #pragma omp cancel parallel also cancels all explicit
	 tasks.  */
    }
  if (team)
    return gomp_team_barrier_cancelled (&team->barrier);
  return false;
}
ialias (GOMP_cancellation_point)

bool
GOMP_cancel (int which, bool do_cancel)
{
  if (!gomp_cancel_var)
    return false;

  if (!do_cancel)
    return ialias_call (GOMP_cancellation_point) (which);

  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  if (which & (GOMP_CANCEL_LOOP | GOMP_CANCEL_SECTIONS))
    {
      /* In orphaned worksharing region, all we want to cancel
	 is current thread.  */
      if (team != NULL)
	team->work_share_cancelled = 1;
      return true;
    }
  else if (which & GOMP_CANCEL_TASKGROUP)
    {
      if (thr->task->taskgroup && !thr->task->taskgroup->cancelled)
	{
	  gomp_mutex_lock (&team->task_lock);
	  thr->task->taskgroup->cancelled = true;
	  gomp_mutex_unlock (&team->task_lock);
	}
      return true;
    }
  team->team_cancelled = 1;
  gomp_team_barrier_cancel (team);
  return true;
}

/* The public OpenMP API for thread and team related inquiries.  */

int
omp_get_num_threads (void)
{
  struct gomp_team *team = gomp_thread ()->ts.team;
  return team ? team->nthreads : 1;
}

int
omp_get_thread_num (void)
{
  return gomp_thread ()->ts.team_id;
}

/* This wasn't right for OpenMP 2.5.  Active region used to be non-zero
   when the IF clause doesn't evaluate to false, starting with OpenMP 3.0
   it is non-zero with more than one thread in the team.  */

int
omp_in_parallel (void)
{
  return gomp_thread ()->ts.active_level > 0;
}

int
omp_get_level (void)
{
  return gomp_thread ()->ts.level;
}

int
omp_get_ancestor_thread_num (int level)
{
  struct gomp_team_state *ts = &gomp_thread ()->ts;
  if (level < 0 || level > ts->level)
    return -1;
  for (level = ts->level - level; level > 0; --level)
    ts = &ts->team->prev_ts;
  return ts->team_id;
}

int
omp_get_team_size (int level)
{
  struct gomp_team_state *ts = &gomp_thread ()->ts;
  if (level < 0 || level > ts->level)
    return -1;
  for (level = ts->level - level; level > 0; --level)
    ts = &ts->team->prev_ts;
  if (ts->team == NULL)
    return 1;
  else
    return ts->team->nthreads;
}

int
omp_get_active_level (void)
{
  return gomp_thread ()->ts.active_level;
}

ialias (omp_get_num_threads)
ialias (omp_get_thread_num)
ialias (omp_in_parallel)
ialias (omp_get_level)
ialias (omp_get_ancestor_thread_num)
ialias (omp_get_team_size)
ialias (omp_get_active_level)
