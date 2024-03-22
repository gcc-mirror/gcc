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

/* This file handles the SINGLE construct.  */

#include "libgomp.h"


/* This routine is called when first encountering a SINGLE construct that
   doesn't have a COPYPRIVATE clause.  Returns true if this is the thread
   that should execute the clause.  */

bool
GOMP_single_start (void)
{
#ifdef HAVE_SYNC_BUILTINS
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  unsigned long single_count;

  if (__builtin_expect (team == NULL, 0))
    return true;

  single_count = thr->ts.single_count++;
  return __sync_bool_compare_and_swap (&team->single_count, single_count,
				       single_count + 1L);
#else
  bool ret = gomp_work_share_start (0);
  if (ret)
    gomp_work_share_init_done ();
  gomp_work_share_end_nowait ();
  return ret;
#endif
}

/* This routine is called when first encountering a SINGLE construct that
   does have a COPYPRIVATE clause.  Returns NULL if this is the thread
   that should execute the clause; otherwise the return value is pointer
   given to GOMP_single_copy_end by the thread that did execute the clause.  */

void *
GOMP_single_copy_start (void)
{
  struct gomp_thread *thr = gomp_thread ();

  bool first;
  void *ret;

  first = gomp_work_share_start (0);
  
  if (first)
    {
      gomp_work_share_init_done ();
      ret = NULL;
    }
  else
    {
      gomp_team_barrier_wait (&thr->ts.team->barrier);

      ret = thr->ts.work_share->copyprivate;
      gomp_work_share_end_nowait ();
    }

  return ret;
}

/* This routine is called when the thread that entered a SINGLE construct
   with a COPYPRIVATE clause gets to the end of the construct.  */

void
GOMP_single_copy_end (void *data)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  if (team != NULL)
    {
      thr->ts.work_share->copyprivate = data;
      gomp_team_barrier_wait (&team->barrier);
    }

  gomp_work_share_end_nowait ();
}
