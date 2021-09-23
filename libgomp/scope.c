/* Copyright (C) 2021 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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

/* This file handles the SCOPE construct with task reductions.  */

#include "libgomp.h"
#include <string.h>


ialias_redirect (GOMP_taskgroup_reduction_register)

/* This routine is called when first encountering a scope construct
   with task reductions.  While scope is not a work-sharing construct,
   if it has task reductions on it, we treat it as one, but as if it is
   nowait, so the work-sharing behavior is done solely to choose which
   thread does the initial initialization of task reductions and which
   threads follow.  scope with task reductions must not be nowait,
   but the barrier and GOMP_workshare_task_reduction_unregister are emitted
   by the lowered code later.  */

void
GOMP_scope_start (uintptr_t *reductions)
{
  struct gomp_thread *thr = gomp_thread ();

  gomp_workshare_taskgroup_start ();
  if (gomp_work_share_start (0))
    {
      GOMP_taskgroup_reduction_register (reductions);
      thr->task->taskgroup->workshare = true;
      thr->ts.work_share->task_reductions = reductions;
      gomp_work_share_init_done ();
    }
  else
    {
      uintptr_t *first_reductions = thr->ts.work_share->task_reductions;
      gomp_workshare_task_reduction_register (reductions,
					      first_reductions);
    }
}
