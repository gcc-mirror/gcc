/* Copyright (C) 2005, 2008 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License 
   along with libgomp; see the file COPYING.LIB.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files, some
   of which are compiled with GCC, to produce an executable, this library
   does not by itself cause the resulting executable to be covered by the
   GNU General Public License.  This exception does not however invalidate
   any other reasons why the executable file might be covered by the GNU
   General Public License.  */

/* This is a Linux specific implementation of a barrier synchronization
   mechanism for libgomp.  This type is private to the library.  This 
   implementation uses atomic instructions and the futex syscall.  */

#ifndef GOMP_BARRIER_H
#define GOMP_BARRIER_H 1

#include "mutex.h"

typedef struct
{
  /* Make sure total/generation is in a mostly read cacheline, while
     awaited in a separate cacheline.  */
  unsigned total __attribute__((aligned (64)));
  unsigned generation;
  unsigned awaited __attribute__((aligned (64)));
} gomp_barrier_t;
typedef unsigned int gomp_barrier_state_t;

static inline void gomp_barrier_init (gomp_barrier_t *bar, unsigned count)
{
  bar->total = count;
  bar->awaited = count;
  bar->generation = 0;
}

static inline void gomp_barrier_reinit (gomp_barrier_t *bar, unsigned count)
{
  __sync_fetch_and_add (&bar->awaited, count - bar->total);
  bar->total = count;
}

static inline void gomp_barrier_destroy (gomp_barrier_t *bar)
{
}

extern void gomp_barrier_wait (gomp_barrier_t *);
extern void gomp_barrier_wait_last (gomp_barrier_t *);
extern void gomp_barrier_wait_end (gomp_barrier_t *, gomp_barrier_state_t);
extern void gomp_team_barrier_wait (gomp_barrier_t *);
extern void gomp_team_barrier_wait_end (gomp_barrier_t *,
					gomp_barrier_state_t);
extern void gomp_team_barrier_wake (gomp_barrier_t *, int);

static inline gomp_barrier_state_t
gomp_barrier_wait_start (gomp_barrier_t *bar)
{
  unsigned int ret = bar->generation & ~3;
  /* Do we need any barrier here or is __sync_add_and_fetch acting
     as the needed LoadLoad barrier already?  */
  ret += __sync_add_and_fetch (&bar->awaited, -1) == 0;
  return ret;
}

static inline bool
gomp_barrier_last_thread (gomp_barrier_state_t state)
{
  return state & 1;
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
