/* Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

/* This is an AMD GCN specific implementation of a barrier synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and s_barrier instruction.  It
   uses MEMMODEL_RELAXED here because barriers are within workgroups and
   therefore don't need to flush caches.  */

#ifndef GOMP_BARRIER_H
#define GOMP_BARRIER_H 1

#include "mutex.h"

typedef struct
{
  unsigned total;
  unsigned generation;
  unsigned awaited;
  unsigned awaited_final;
} gomp_barrier_t;

typedef unsigned int gomp_barrier_state_t;

/* The generation field contains a counter in the high bits, with a few
   low bits dedicated to flags.  Note that TASK_PENDING and WAS_LAST can
   share space because WAS_LAST is never stored back to generation.  */
#define BAR_TASK_PENDING	1
#define BAR_WAS_LAST		1
#define BAR_WAITING_FOR_TASK	2
#define BAR_CANCELLED		4
#define BAR_INCR		8

static inline void gomp_barrier_init (gomp_barrier_t *bar, unsigned count)
{
  bar->total = count;
  bar->awaited = count;
  bar->awaited_final = count;
  bar->generation = 0;
}

static inline void gomp_barrier_reinit (gomp_barrier_t *bar, unsigned count)
{
  __atomic_add_fetch (&bar->awaited, count - bar->total, MEMMODEL_RELAXED);
  bar->total = count;
}

static inline void gomp_barrier_destroy (gomp_barrier_t *bar)
{
}

extern void gomp_barrier_wait (gomp_barrier_t *);
extern void gomp_barrier_wait_last (gomp_barrier_t *);
extern void gomp_barrier_wait_end (gomp_barrier_t *, gomp_barrier_state_t);
extern void gomp_team_barrier_wait (gomp_barrier_t *);
extern void gomp_team_barrier_wait_final (gomp_barrier_t *);
extern void gomp_team_barrier_wait_end (gomp_barrier_t *,
					gomp_barrier_state_t);
extern bool gomp_team_barrier_wait_cancel (gomp_barrier_t *);
extern bool gomp_team_barrier_wait_cancel_end (gomp_barrier_t *,
					       gomp_barrier_state_t);
extern void gomp_team_barrier_wake (gomp_barrier_t *, int);
struct gomp_team;
extern void gomp_team_barrier_cancel (struct gomp_team *);

static inline gomp_barrier_state_t
gomp_barrier_wait_start (gomp_barrier_t *bar)
{
  unsigned int ret = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  ret &= -BAR_INCR | BAR_CANCELLED;
  /* A memory barrier is needed before exiting from the various forms
     of gomp_barrier_wait, to satisfy OpenMP API version 3.1 section
     2.8.6 flush Construct, which says there is an implicit flush during
     a barrier region.  This is a convenient place to add the barrier,
     so we use MEMMODEL_ACQ_REL here rather than MEMMODEL_ACQUIRE.  */
  if (__atomic_add_fetch (&bar->awaited, -1, MEMMODEL_RELAXED) == 0)
    ret |= BAR_WAS_LAST;
  return ret;
}

static inline gomp_barrier_state_t
gomp_barrier_wait_cancel_start (gomp_barrier_t *bar)
{
  return gomp_barrier_wait_start (bar);
}

/* This is like gomp_barrier_wait_start, except it decrements
   bar->awaited_final rather than bar->awaited and should be used
   for the gomp_team_end barrier only.  */
static inline gomp_barrier_state_t
gomp_barrier_wait_final_start (gomp_barrier_t *bar)
{
  unsigned int ret = __atomic_load_n (&bar->generation, MEMMODEL_RELAXED);
  ret &= -BAR_INCR | BAR_CANCELLED;
  /* See above gomp_barrier_wait_start comment.  */
  if (__atomic_add_fetch (&bar->awaited_final, -1, MEMMODEL_RELAXED) == 0)
    ret |= BAR_WAS_LAST;
  return ret;
}

static inline bool
gomp_barrier_last_thread (gomp_barrier_state_t state)
{
  return state & BAR_WAS_LAST;
}

/* All the inlines below must be called with team->task_lock
   held.  */

static inline void
gomp_team_barrier_set_task_pending (gomp_barrier_t *bar)
{
  bar->generation |= BAR_TASK_PENDING;
}

static inline void
gomp_team_barrier_clear_task_pending (gomp_barrier_t *bar)
{
  bar->generation &= ~BAR_TASK_PENDING;
}

static inline void
gomp_team_barrier_set_waiting_for_tasks (gomp_barrier_t *bar)
{
  bar->generation |= BAR_WAITING_FOR_TASK;
}

static inline bool
gomp_team_barrier_waiting_for_tasks (gomp_barrier_t *bar)
{
  return (bar->generation & BAR_WAITING_FOR_TASK) != 0;
}

static inline bool
gomp_team_barrier_cancelled (gomp_barrier_t *bar)
{
  return __builtin_expect ((bar->generation & BAR_CANCELLED) != 0, 0);
}

static inline void
gomp_team_barrier_done (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  bar->generation = (state & -BAR_INCR) + BAR_INCR;
}

#endif /* GOMP_BARRIER_H */
