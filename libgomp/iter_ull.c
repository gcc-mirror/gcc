/* Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

/* This file contains routines for managing work-share iteration, both
   for loops and sections.  */

#include "libgomp.h"
#include <stdlib.h>

typedef unsigned long long gomp_ull;

/* This function implements the STATIC scheduling method.  The caller should
   iterate *pstart <= x < *pend.  Return zero if there are more iterations
   to perform; nonzero if not.  Return less than 0 if this thread had
   received the absolutely last iteration.  */

int
gomp_iter_ull_static_next (gomp_ull *pstart, gomp_ull *pend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_work_share *ws = thr->ts.work_share;
  unsigned long nthreads = team ? team->nthreads : 1;

  if (thr->ts.static_trip == -1)
    return -1;

  /* Quick test for degenerate teams and orphaned constructs.  */
  if (nthreads == 1)
    {
      *pstart = ws->next_ull;
      *pend = ws->end_ull;
      thr->ts.static_trip = -1;
      return ws->next_ull == ws->end_ull;
    }

  /* We interpret chunk_size zero as "unspecified", which means that we
     should break up the iterations such that each thread makes only one
     trip through the outer loop.  */
  if (ws->chunk_size_ull == 0)
    {
      gomp_ull n, q, i, t, s0, e0, s, e;

      if (thr->ts.static_trip > 0)
	return 1;

      /* Compute the total number of iterations.  */
      if (__builtin_expect (ws->mode, 0) == 0)
	n = (ws->end_ull - ws->next_ull + ws->incr_ull - 1) / ws->incr_ull;
      else
	n = (ws->next_ull - ws->end_ull - ws->incr_ull - 1) / -ws->incr_ull;
      i = thr->ts.team_id;

      /* Compute the "zero-based" start and end points.  That is, as
	 if the loop began at zero and incremented by one.  */
      q = n / nthreads;
      t = n % nthreads;
      if (i < t)
	{
	  t = 0;
	  q++;
	}
      s0 = q * i + t;
      e0 = s0 + q;

      /* Notice when no iterations allocated for this thread.  */
      if (s0 >= e0)
	{
	  thr->ts.static_trip = 1;
	  return 1;
	}

      /* Transform these to the actual start and end numbers.  */
      s = s0 * ws->incr_ull + ws->next_ull;
      e = e0 * ws->incr_ull + ws->next_ull;

      *pstart = s;
      *pend = e;
      thr->ts.static_trip = (e0 == n ? -1 : 1);
      return 0;
    }
  else
    {
      gomp_ull n, s0, e0, i, c, s, e;

      /* Otherwise, each thread gets exactly chunk_size iterations
	 (if available) each time through the loop.  */

      if (__builtin_expect (ws->mode, 0) == 0)
	n = (ws->end_ull - ws->next_ull + ws->incr_ull - 1) / ws->incr_ull;
      else
	n = (ws->next_ull - ws->end_ull - ws->incr_ull - 1) / -ws->incr_ull;
      i = thr->ts.team_id;
      c = ws->chunk_size_ull;

      /* Initial guess is a C sized chunk positioned nthreads iterations
	 in, offset by our thread number.  */
      s0 = (thr->ts.static_trip * (gomp_ull) nthreads + i) * c;
      e0 = s0 + c;

      /* Detect overflow.  */
      if (s0 >= n)
	return 1;
      if (e0 > n)
	e0 = n;

      /* Transform these to the actual start and end numbers.  */
      s = s0 * ws->incr_ull + ws->next_ull;
      e = e0 * ws->incr_ull + ws->next_ull;

      *pstart = s;
      *pend = e;

      if (e0 == n)
	thr->ts.static_trip = -1;
      else
	thr->ts.static_trip++;
      return 0;
    }
}


/* This function implements the DYNAMIC scheduling method.  Arguments are
   as for gomp_iter_ull_static_next.  This function must be called with
   ws->lock held.  */

bool
gomp_iter_ull_dynamic_next_locked (gomp_ull *pstart, gomp_ull *pend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  gomp_ull start, end, chunk, left;

  start = ws->next_ull;
  if (start == ws->end_ull)
    return false;

  chunk = ws->chunk_size_ull;
  left = ws->end_ull - start;
  if (__builtin_expect (ws->mode & 2, 0))
    {
      if (chunk < left)
	chunk = left;
    }
  else
    {
      if (chunk > left)
	chunk = left;
    }
  end = start + chunk;

  ws->next_ull = end;
  *pstart = start;
  *pend = end;
  return true;
}


#if defined HAVE_SYNC_BUILTINS && defined __LP64__
/* Similar, but doesn't require the lock held, and uses compare-and-swap
   instead.  Note that the only memory value that changes is ws->next_ull.  */

bool
gomp_iter_ull_dynamic_next (gomp_ull *pstart, gomp_ull *pend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  gomp_ull start, end, nend, chunk;

  end = ws->end_ull;
  chunk = ws->chunk_size_ull;

  if (__builtin_expect (ws->mode & 1, 1))
    {
      gomp_ull tmp = __sync_fetch_and_add (&ws->next_ull, chunk);
      if (__builtin_expect (ws->mode & 2, 0) == 0)
	{
	  if (tmp >= end)
	    return false;
	  nend = tmp + chunk;
	  if (nend > end)
	    nend = end;
	  *pstart = tmp;
	  *pend = nend;
	  return true;
	}
      else
	{
	  if (tmp <= end)
	    return false;
	  nend = tmp + chunk;
	  if (nend < end)
	    nend = end;
	  *pstart = tmp;
	  *pend = nend;
	  return true;
	}
    }

  start = ws->next_ull;
  while (1)
    {
      gomp_ull left = end - start;
      gomp_ull tmp;

      if (start == end)
	return false;

      if (__builtin_expect (ws->mode & 2, 0))
	{
	  if (chunk < left)
	    chunk = left;
	}
      else
	{
	  if (chunk > left)
	    chunk = left;
	}
      nend = start + chunk;

      tmp = __sync_val_compare_and_swap (&ws->next_ull, start, nend);
      if (__builtin_expect (tmp == start, 1))
	break;

      start = tmp;
    }

  *pstart = start;
  *pend = nend;
  return true;
}
#endif /* HAVE_SYNC_BUILTINS */


/* This function implements the GUIDED scheduling method.  Arguments are
   as for gomp_iter_ull_static_next.  This function must be called with the
   work share lock held.  */

bool
gomp_iter_ull_guided_next_locked (gomp_ull *pstart, gomp_ull *pend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  struct gomp_team *team = thr->ts.team;
  gomp_ull nthreads = team ? team->nthreads : 1;
  gomp_ull n, q;
  gomp_ull start, end;

  if (ws->next_ull == ws->end_ull)
    return false;

  start = ws->next_ull;
  if (__builtin_expect (ws->mode, 0) == 0)
    n = (ws->end_ull - start) / ws->incr_ull;
  else
    n = (start - ws->end_ull) / -ws->incr_ull;
  q = (n + nthreads - 1) / nthreads;

  if (q < ws->chunk_size_ull)
    q = ws->chunk_size_ull;
  if (q <= n)
    end = start + q * ws->incr_ull;
  else
    end = ws->end_ull;

  ws->next_ull = end;
  *pstart = start;
  *pend = end;
  return true;
}

#if defined HAVE_SYNC_BUILTINS && defined __LP64__
/* Similar, but doesn't require the lock held, and uses compare-and-swap
   instead.  Note that the only memory value that changes is ws->next_ull.  */

bool
gomp_iter_ull_guided_next (gomp_ull *pstart, gomp_ull *pend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_work_share *ws = thr->ts.work_share;
  struct gomp_team *team = thr->ts.team;
  gomp_ull nthreads = team ? team->nthreads : 1;
  gomp_ull start, end, nend, incr;
  gomp_ull chunk_size;

  start = ws->next_ull;
  end = ws->end_ull;
  incr = ws->incr_ull;
  chunk_size = ws->chunk_size_ull;

  while (1)
    {
      gomp_ull n, q;
      gomp_ull tmp;

      if (start == end)
	return false;

      if (__builtin_expect (ws->mode, 0) == 0)
	n = (end - start) / incr;
      else
	n = (start - end) / -incr;
      q = (n + nthreads - 1) / nthreads;

      if (q < chunk_size)
	q = chunk_size;
      if (__builtin_expect (q <= n, 1))
	nend = start + q * incr;
      else
	nend = end;

      tmp = __sync_val_compare_and_swap (&ws->next_ull, start, nend);
      if (__builtin_expect (tmp == start, 1))
	break;

      start = tmp;
    }

  *pstart = start;
  *pend = nend;
  return true;
}
#endif /* HAVE_SYNC_BUILTINS */
