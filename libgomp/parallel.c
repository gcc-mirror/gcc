/* Copyright (C) 2005, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

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
  struct gomp_thread *thread = gomp_thread();
  struct gomp_task_icv *icv;
  unsigned threads_requested, max_num_threads, num_threads;
  unsigned long remaining;

  icv = gomp_icv (false);

  if (specified == 1)
    return 1;
  else if (thread->ts.active_level >= 1 && !icv->nest_var)
    return 1;
  else if (thread->ts.active_level >= gomp_max_active_levels_var)
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

  /* ULONG_MAX stands for infinity.  */
  if (__builtin_expect (gomp_thread_limit_var == ULONG_MAX, 1)
      || max_num_threads == 1)
    return max_num_threads;

#ifdef HAVE_SYNC_BUILTINS
  do
    {
      remaining = gomp_remaining_threads_count;
      num_threads = max_num_threads;
      if (num_threads > remaining)
	num_threads = remaining + 1;
    }
  while (__sync_val_compare_and_swap (&gomp_remaining_threads_count,
				      remaining, remaining - num_threads + 1)
	 != remaining);
#else
  gomp_mutex_lock (&gomp_remaining_threads_lock);
  num_threads = max_num_threads;
  remaining = gomp_remaining_threads_count;
  if (num_threads > remaining)
    num_threads = remaining + 1;
  gomp_remaining_threads_count -= num_threads - 1;
  gomp_mutex_unlock (&gomp_remaining_threads_lock);
#endif

  return num_threads;
}

void
GOMP_parallel_start (void (*fn) (void *), void *data, unsigned num_threads)
{
  num_threads = gomp_resolve_num_threads (num_threads, 0);
  gomp_team_start (fn, data, num_threads, gomp_new_team (num_threads));
}

void
GOMP_parallel_end (void)
{
  if (__builtin_expect (gomp_thread_limit_var != ULONG_MAX, 0))
    {
      struct gomp_thread *thr = gomp_thread ();
      struct gomp_team *team = thr->ts.team;
      if (team && team->nthreads > 1)
	{
#ifdef HAVE_SYNC_BUILTINS
	  __sync_fetch_and_add (&gomp_remaining_threads_count,
				1UL - team->nthreads);
#else
	  gomp_mutex_lock (&gomp_remaining_threads_lock);
	  gomp_remaining_threads_count -= team->nthreads - 1;
	  gomp_mutex_unlock (&gomp_remaining_threads_lock);
#endif
	}
    }
  gomp_team_end ();
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
