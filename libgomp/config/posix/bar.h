/* Copyright (C) 2005-2013 Free Software Foundation, Inc.
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
} gomp_barrier_t;
typedef unsigned int gomp_barrier_state_t;

extern void gomp_barrier_init (gomp_barrier_t *, unsigned);
extern void gomp_barrier_reinit (gomp_barrier_t *, unsigned);
extern void gomp_barrier_destroy (gomp_barrier_t *);

extern void gomp_barrier_wait (gomp_barrier_t *);
extern void gomp_barrier_wait_end (gomp_barrier_t *, gomp_barrier_state_t);
extern void gomp_team_barrier_wait (gomp_barrier_t *);
extern void gomp_team_barrier_wait_end (gomp_barrier_t *,
					gomp_barrier_state_t);
extern void gomp_team_barrier_wake (gomp_barrier_t *, int);

static inline gomp_barrier_state_t
gomp_barrier_wait_start (gomp_barrier_t *bar)
{
  unsigned int ret;
  gomp_mutex_lock (&bar->mutex1);
  ret = bar->generation & ~3;
  ret += ++bar->arrived == bar->total;
  return ret;
}

static inline bool
gomp_barrier_last_thread (gomp_barrier_state_t state)
{
  return state & 1;
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
  bar->generation |= 1;
}

static inline void
gomp_team_barrier_clear_task_pending (gomp_barrier_t *bar)
{
  bar->generation &= ~1;
}

static inline void
gomp_team_barrier_set_waiting_for_tasks (gomp_barrier_t *bar)
{
  bar->generation |= 2;
}

static inline bool
gomp_team_barrier_waiting_for_tasks (gomp_barrier_t *bar)
{
  return (bar->generation & 2) != 0;
}

static inline void
gomp_team_barrier_done (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  bar->generation = (state & ~3) + 4;
}

#endif /* GOMP_BARRIER_H */
