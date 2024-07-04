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

/* This file contains routines to manage the work-share queue for a team
   of threads.  */

#include "libgomp.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>


/* Allocate a new work share structure, preferably from current team's
   free gomp_work_share cache.  */

static struct gomp_work_share *
alloc_work_share (struct gomp_team *team)
{
  struct gomp_work_share *ws;
  unsigned int i;

  /* This is called in a critical section.  */
  if (team->work_share_list_alloc != NULL)
    {
      ws = team->work_share_list_alloc;
      team->work_share_list_alloc = ws->next_free;
      return ws;
    }

#ifdef HAVE_SYNC_BUILTINS
  ws = team->work_share_list_free;
  /* We need atomic read from work_share_list_free,
     as free_work_share can be called concurrently.  */
  __asm ("" : "+r" (ws));

  if (ws && ws->next_free)
    {
      struct gomp_work_share *next = ws->next_free;
      ws->next_free = NULL;
      team->work_share_list_alloc = next->next_free;
      return next;
    }
#else
  gomp_mutex_lock (&team->work_share_list_free_lock);
  ws = team->work_share_list_free;
  if (ws)
    {
      team->work_share_list_alloc = ws->next_free;
      team->work_share_list_free = NULL;
      gomp_mutex_unlock (&team->work_share_list_free_lock);
      return ws;
    }
  gomp_mutex_unlock (&team->work_share_list_free_lock);
#endif

  team->work_share_chunk *= 2;
  /* Allocating gomp_work_share structures aligned is just an
     optimization, don't do it when using the fallback method.  */
#ifdef GOMP_USE_ALIGNED_WORK_SHARES
  ws = gomp_aligned_alloc (__alignof (struct gomp_work_share),
			   team->work_share_chunk
			   * sizeof (struct gomp_work_share));
#else
  ws = gomp_malloc (team->work_share_chunk * sizeof (struct gomp_work_share));
#endif
  ws->next_alloc = team->work_shares[0].next_alloc;
  team->work_shares[0].next_alloc = ws;
  team->work_share_list_alloc = &ws[1];
  for (i = 1; i < team->work_share_chunk - 1; i++)
    ws[i].next_free = &ws[i + 1];
  ws[i].next_free = NULL;
  return ws;
}

/* Initialize an already allocated struct gomp_work_share.
   This shouldn't touch the next_alloc field.  */

void
gomp_init_work_share (struct gomp_work_share *ws, size_t ordered,
		      unsigned nthreads)
{
  gomp_mutex_init (&ws->lock);
  if (__builtin_expect (ordered, 0))
    {
#define INLINE_ORDERED_TEAM_IDS_SIZE \
  (sizeof (struct gomp_work_share) \
   - offsetof (struct gomp_work_share, inline_ordered_team_ids))

      if (__builtin_expect (ordered != 1, 0))
	{
	  size_t o = nthreads * sizeof (*ws->ordered_team_ids);
	  o += __alignof__ (long long) - 1;
	  if ((offsetof (struct gomp_work_share, inline_ordered_team_ids)
	       & (__alignof__ (long long) - 1)) == 0
	      && __alignof__ (struct gomp_work_share)
		 >= __alignof__ (long long))
	    o &= ~(__alignof__ (long long) - 1);
	  ordered += o - 1;
	}
      else
	ordered = nthreads * sizeof (*ws->ordered_team_ids);
      if (ordered > INLINE_ORDERED_TEAM_IDS_SIZE)
	ws->ordered_team_ids = team_malloc (ordered);
      else
	ws->ordered_team_ids = ws->inline_ordered_team_ids;
      memset (ws->ordered_team_ids, '\0', ordered);
      ws->ordered_num_used = 0;
      ws->ordered_owner = -1;
      ws->ordered_cur = 0;
    }
  else
    ws->ordered_team_ids = ws->inline_ordered_team_ids;
  gomp_ptrlock_init (&ws->next_ws, NULL);
  ws->threads_completed = 0;
}

/* Do any needed destruction of gomp_work_share fields before it
   is put back into free gomp_work_share cache or freed.  */

void
gomp_fini_work_share (struct gomp_work_share *ws)
{
  gomp_mutex_destroy (&ws->lock);
  if (ws->ordered_team_ids != ws->inline_ordered_team_ids)
    team_free (ws->ordered_team_ids);
  gomp_ptrlock_destroy (&ws->next_ws);
}

/* Free a work share struct, if not orphaned, put it into current
   team's free gomp_work_share cache.  */

static inline void
free_work_share (struct gomp_team *team, struct gomp_work_share *ws)
{
  gomp_fini_work_share (ws);
  if (__builtin_expect (team == NULL, 0))
    free (ws);
  else
    {
      struct gomp_work_share *next_ws;
#ifdef HAVE_SYNC_BUILTINS
      do
	{
	  next_ws = team->work_share_list_free;
	  ws->next_free = next_ws;
	}
      while (!__sync_bool_compare_and_swap (&team->work_share_list_free,
					    next_ws, ws));
#else
      gomp_mutex_lock (&team->work_share_list_free_lock);
      next_ws = team->work_share_list_free;
      ws->next_free = next_ws;
      team->work_share_list_free = ws;
      gomp_mutex_unlock (&team->work_share_list_free_lock);
#endif
    }
}

/* The current thread is ready to begin the next work sharing construct.
   In all cases, thr->ts.work_share is updated to point to the new
   structure.  In all cases the work_share lock is locked.  Return true
   if this was the first thread to reach this point.  */

bool
gomp_work_share_start (size_t ordered)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws;

  /* Work sharing constructs can be orphaned.  */
  if (team == NULL)
    {
#ifdef GOMP_USE_ALIGNED_WORK_SHARES
      ws = gomp_aligned_alloc (__alignof (struct gomp_work_share),
			       sizeof (*ws));
#else
      ws = gomp_malloc (sizeof (*ws));
#endif
      gomp_init_work_share (ws, ordered, 1);
      thr->ts.work_share = ws;
      return true;
    }

  ws = thr->ts.work_share;
  thr->ts.last_work_share = ws;
  ws = gomp_ptrlock_get (&ws->next_ws);
  if (ws == NULL)
    {
      /* This thread encountered a new ws first.  */
      struct gomp_work_share *ws = alloc_work_share (team);
      gomp_init_work_share (ws, ordered, team->nthreads);
      thr->ts.work_share = ws;
      return true;
    }
  else
    {
      thr->ts.work_share = ws;
      return false;
    }
}

/* The current thread is done with its current work sharing construct.
   This version does imply a barrier at the end of the work-share.  */

void
gomp_work_share_end (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  gomp_barrier_state_t bstate;

  /* Work sharing constructs can be orphaned.  */
  if (team == NULL)
    {
      free_work_share (NULL, thr->ts.work_share);
      thr->ts.work_share = NULL;
      return;
    }

  bstate = gomp_barrier_wait_start (&team->barrier);

  if (gomp_barrier_last_thread (bstate))
    {
      if (__builtin_expect (thr->ts.last_work_share != NULL, 1))
	{
	  team->work_shares_to_free = thr->ts.work_share;
	  free_work_share (team, thr->ts.last_work_share);
	}
    }

  gomp_team_barrier_wait_end (&team->barrier, bstate);
  thr->ts.last_work_share = NULL;
}

/* The current thread is done with its current work sharing construct.
   This version implies a cancellable barrier at the end of the work-share.  */

bool
gomp_work_share_end_cancel (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  gomp_barrier_state_t bstate;

  /* Cancellable work sharing constructs cannot be orphaned.  */
  bstate = gomp_barrier_wait_cancel_start (&team->barrier);

  if (gomp_barrier_last_thread (bstate))
    {
      if (__builtin_expect (thr->ts.last_work_share != NULL, 1))
	{
	  team->work_shares_to_free = thr->ts.work_share;
	  free_work_share (team, thr->ts.last_work_share);
	}
    }
  thr->ts.last_work_share = NULL;

  return gomp_team_barrier_wait_cancel_end (&team->barrier, bstate);
}

/* The current thread is done with its current work sharing construct.
   This version does NOT imply a barrier at the end of the work-share.  */

void
gomp_work_share_end_nowait (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned completed;

  /* Work sharing constructs can be orphaned.  */
  if (team == NULL)
    {
      free_work_share (NULL, ws);
      thr->ts.work_share = NULL;
      return;
    }

  if (__builtin_expect (thr->ts.last_work_share == NULL, 0))
    return;

#ifdef HAVE_SYNC_BUILTINS
  completed = __sync_add_and_fetch (&ws->threads_completed, 1);
#else
  gomp_mutex_lock (&ws->lock);
  completed = ++ws->threads_completed;
  gomp_mutex_unlock (&ws->lock);
#endif

  if (completed == team->nthreads)
    {
      team->work_shares_to_free = thr->ts.work_share;
      free_work_share (team, thr->ts.last_work_share);
    }
  thr->ts.last_work_share = NULL;
}
