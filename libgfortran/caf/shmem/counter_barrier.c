/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"
#include "counter_barrier.h"
#include "supervisor.h"
#include "thread_support.h"

#include <assert.h>

/* Lock the associated counter of this barrier.  */

static inline void
lock_counter_barrier (counter_barrier *b)
{
  pthread_mutex_lock (&b->mutex);
}

/* Unlock the associated counter of this barrier.  */

static inline void
unlock_counter_barrier (counter_barrier *b)
{
  pthread_mutex_unlock (&b->mutex);
}

void
counter_barrier_init (counter_barrier *b, int val)
{
  *b = (counter_barrier) {PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER,
			  val, 0, val};
  initialize_shared_condition (&b->cond);
  initialize_shared_mutex (&b->mutex);
}

void
counter_barrier_wait (counter_barrier *b)
{
  int wait_group_beginning;

  lock_counter_barrier (b);

  wait_group_beginning = b->curr_wait_group;

  if ((--b->wait_count) <= 0)
    pthread_cond_broadcast (&b->cond);
  else
    {
      while (b->wait_count > 0 && b->curr_wait_group == wait_group_beginning)
	  pthread_cond_wait (&b->cond, &b->mutex);
    }

  if (b->wait_count <= 0)
    {
      b->curr_wait_group = !wait_group_beginning;
      b->wait_count = b->count;
    }

  unlock_counter_barrier (b);
}


static inline void
change_internal_barrier_count (counter_barrier *b, int val)
{
  b->wait_count += val;
  if (b->wait_count <= 0)
    pthread_cond_broadcast (&b->cond);
}

int
counter_barrier_add_locked (counter_barrier *c, int val)
{
  int ret;
  ret = (c->count += val);
  change_internal_barrier_count (c, val);

  return ret;
}

int
counter_barrier_add (counter_barrier *c, int val)
{
  int ret;
  pthread_mutex_lock (&c->mutex);
  ret = counter_barrier_add_locked (c, val);

  pthread_mutex_unlock (&c->mutex);
  return ret;
}

int
counter_barrier_get_count (counter_barrier *c)
{
  int ret;
  pthread_mutex_lock (&c->mutex);
  ret = c->count;
  pthread_mutex_unlock (&c->mutex);
  return ret;
}
