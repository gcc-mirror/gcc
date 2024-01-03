/* Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

/* This file handles the SECTIONS construct.  */

#include "libgomp.h"
#include <string.h>


ialias_redirect (GOMP_taskgroup_reduction_register)

/* Initialize the given work share construct from the given arguments.  */

static inline void
gomp_sections_init (struct gomp_work_share *ws, unsigned count)
{
  ws->sched = GFS_DYNAMIC;
  ws->chunk_size = 1;
  ws->end = count + 1L;
  ws->incr = 1;
  ws->next = 1;
#ifdef HAVE_SYNC_BUILTINS
  /* Prepare things to make each iteration faster.  */
  if (sizeof (long) > sizeof (unsigned))
    ws->mode = 1;
  else
    {
      struct gomp_thread *thr = gomp_thread ();
      struct gomp_team *team = thr->ts.team;
      long nthreads = team ? team->nthreads : 1;

      ws->mode = ((nthreads | ws->end)
		  < 1UL << (sizeof (long) * __CHAR_BIT__ / 2 - 1));
    }
#else
  ws->mode = 0;
#endif
}

/* This routine is called when first encountering a sections construct
   that is not bound directly to a parallel construct.  The first thread 
   that arrives will create the work-share construct; subsequent threads
   will see the construct exists and allocate work from it.

   COUNT is the number of sections in this construct.

   Returns the 1-based section number for this thread to perform, or 0 if
   all work was assigned to other threads prior to this thread's arrival.  */

unsigned
GOMP_sections_start (unsigned count)
{
  struct gomp_thread *thr = gomp_thread ();
  long s, e, ret;

  if (gomp_work_share_start (0))
    {
      gomp_sections_init (thr->ts.work_share, count);
      gomp_work_share_init_done ();
    }

#ifdef HAVE_SYNC_BUILTINS
  if (gomp_iter_dynamic_next (&s, &e))
    ret = s;
  else
    ret = 0;
#else
  gomp_mutex_lock (&thr->ts.work_share->lock);
  if (gomp_iter_dynamic_next_locked (&s, &e))
    ret = s;
  else
    ret = 0;
  gomp_mutex_unlock (&thr->ts.work_share->lock);
#endif

  return ret;
}

unsigned
GOMP_sections2_start (unsigned count, uintptr_t *reductions, void **mem)
{
  struct gomp_thread *thr = gomp_thread ();
  long s, e, ret;

  if (reductions)
    gomp_workshare_taskgroup_start ();
  if (gomp_work_share_start (0))
    {
      gomp_sections_init (thr->ts.work_share, count);
      if (reductions)
	{
	  GOMP_taskgroup_reduction_register (reductions);
	  thr->task->taskgroup->workshare = true;
	  thr->ts.work_share->task_reductions = reductions;
	}
      if (mem)
	{
	  uintptr_t size = (uintptr_t) *mem;
#define INLINE_ORDERED_TEAM_IDS_OFF \
  ((offsetof (struct gomp_work_share, inline_ordered_team_ids)		\
    + __alignof__ (long long) - 1) & ~(__alignof__ (long long) - 1))
	  if (sizeof (struct gomp_work_share)
	      <= INLINE_ORDERED_TEAM_IDS_OFF
	      || __alignof__ (struct gomp_work_share) < __alignof__ (long long)
	      || size > (sizeof (struct gomp_work_share)
			- INLINE_ORDERED_TEAM_IDS_OFF))
	    *mem
	      = (void *) (thr->ts.work_share->ordered_team_ids
			  = gomp_malloc_cleared (size));
	  else
	    *mem = memset (((char *) thr->ts.work_share)
			   + INLINE_ORDERED_TEAM_IDS_OFF, '\0', size);
	}
      gomp_work_share_init_done ();
    }
  else
    {
      if (reductions)
	{
	  uintptr_t *first_reductions = thr->ts.work_share->task_reductions;
	  gomp_workshare_task_reduction_register (reductions,
						  first_reductions);
	}
      if (mem)
	{
	  if ((offsetof (struct gomp_work_share, inline_ordered_team_ids)
	       & (__alignof__ (long long) - 1)) == 0)
	    *mem = (void *) thr->ts.work_share->ordered_team_ids;
	  else
	    {
	      uintptr_t p = (uintptr_t) thr->ts.work_share->ordered_team_ids;
	      p += __alignof__ (long long) - 1;
	      p &= ~(__alignof__ (long long) - 1);
	      *mem = (void *) p;
	    }
	}
    }

#ifdef HAVE_SYNC_BUILTINS
  if (gomp_iter_dynamic_next (&s, &e))
    ret = s;
  else
    ret = 0;
#else
  gomp_mutex_lock (&thr->ts.work_share->lock);
  if (gomp_iter_dynamic_next_locked (&s, &e))
    ret = s;
  else
    ret = 0;
  gomp_mutex_unlock (&thr->ts.work_share->lock);
#endif

  return ret;
}

/* This routine is called when the thread completes processing of the
   section currently assigned to it.  If the work-share construct is
   bound directly to a parallel construct, then the construct may have
   been set up before the parallel.  In which case, this may be the
   first iteration for the thread.

   Returns the 1-based section number for this thread to perform, or 0 if
   all work was assigned to other threads prior to this thread's arrival.  */

unsigned
GOMP_sections_next (void)
{
  long s, e, ret;

#ifdef HAVE_SYNC_BUILTINS
  if (gomp_iter_dynamic_next (&s, &e))
    ret = s;
  else
    ret = 0;
#else
  struct gomp_thread *thr = gomp_thread ();

  gomp_mutex_lock (&thr->ts.work_share->lock);
  if (gomp_iter_dynamic_next_locked (&s, &e))
    ret = s;
  else
    ret = 0;
  gomp_mutex_unlock (&thr->ts.work_share->lock);
#endif

  return ret;
}

/* This routine pre-initializes a work-share construct to avoid one
   synchronization once we get into the loop.  */

void
GOMP_parallel_sections_start (void (*fn) (void *), void *data,
			      unsigned num_threads, unsigned count)
{
  struct gomp_team *team;

  num_threads = gomp_resolve_num_threads (num_threads, count);
  team = gomp_new_team (num_threads);
  gomp_sections_init (&team->work_shares[0], count);
  gomp_team_start (fn, data, num_threads, 0, team, NULL);
}

ialias_redirect (GOMP_parallel_end)

void
GOMP_parallel_sections (void (*fn) (void *), void *data,
			unsigned num_threads, unsigned count, unsigned flags)
{
  struct gomp_team *team;

  num_threads = gomp_resolve_num_threads (num_threads, count);
  team = gomp_new_team (num_threads);
  gomp_sections_init (&team->work_shares[0], count);
  gomp_team_start (fn, data, num_threads, flags, team, NULL);
  fn (data);
  GOMP_parallel_end ();
}

/* The GOMP_section_end* routines are called after the thread is told
   that all sections are complete.  The first two versions synchronize
   all threads; the nowait version does not.  */

void
GOMP_sections_end (void)
{
  gomp_work_share_end ();
}

bool
GOMP_sections_end_cancel (void)
{
  return gomp_work_share_end_cancel ();
}

void
GOMP_sections_end_nowait (void)
{
  gomp_work_share_end_nowait ();
}
