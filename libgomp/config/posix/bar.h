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

/* This is the default implementation of a barrier synchronization mechanism
   for libgomp.  This type is private to the library.  Note that we rely on
   being able to adjust the barrier count while threads are blocked, so the
   POSIX pthread_barrier_t won't work.  */

#ifndef GOMP_BARRIER_H
#define GOMP_BARRIER_H 1

#include <pthread.h>

typedef struct
{
  gomp_mutex_t mutex1;
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_t mutex2;
#endif
  gomp_sem_t sem1;
  gomp_sem_t sem2;
  unsigned total;
  unsigned arrived;
  unsigned generation;
  bool cancellable;
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

extern void gomp_barrier_init (gomp_barrier_t *, unsigned);
extern void gomp_barrier_reinit (gomp_barrier_t *, unsigned);
extern void gomp_barrier_destroy (gomp_barrier_t *);

extern void gomp_barrier_wait (gomp_barrier_t *);
extern void gomp_barrier_wait_end (gomp_barrier_t *, gomp_barrier_state_t);
extern void gomp_team_barrier_wait (gomp_barrier_t *);
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
  unsigned int ret;
  gomp_mutex_lock (&bar->mutex1);
  ret = bar->generation & (-BAR_INCR | BAR_CANCELLED);
  if (++bar->arrived == bar->total)
    ret |= BAR_WAS_LAST;
  return ret;
}

static inline gomp_barrier_state_t
gomp_barrier_wait_cancel_start (gomp_barrier_t *bar)
{
  unsigned int ret;
  gomp_mutex_lock (&bar->mutex1);
  ret = bar->generation & (-BAR_INCR | BAR_CANCELLED);
  if (ret & BAR_CANCELLED)
    return ret;
  if (++bar->arrived == bar->total)
    ret |= BAR_WAS_LAST;
  return ret;
}

static inline void
gomp_team_barrier_wait_final (gomp_barrier_t *bar)
{
  gomp_team_barrier_wait (bar);
}

static inline bool
gomp_barrier_last_thread (gomp_barrier_state_t state)
{
  return state & BAR_WAS_LAST;
}

static inline void
gomp_barrier_wait_last (gomp_barrier_t *bar)
{
  gomp_barrier_wait (bar);
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
