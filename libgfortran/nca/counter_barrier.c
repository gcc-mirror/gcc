/* Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

   This file is part of the GNU Fortran Native Coarray Library (libnca).

   Libnca is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libnca is distributed in the hope that it will be useful,
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
#include "util.h"
#include "counter_barrier.h"

#include <assert.h>

/* Lock the associated counter of this barrier.  */

static inline void
lock_counter_barrier (counter_barrier *b)
{
  pthread_mutex_lock (&b->count->m);
}

/* Unlock the associated counter of this barrier.  */

static inline void
unlock_counter_barrier (counter_barrier *b)
{
  pthread_mutex_unlock (&b->count->m);
}

/* Wait on the barrier.  */

void
counter_barrier_wait (counter_barrier *b)
{
  int initial_count;
  int wait_group_beginning;

  assert (b->count);

  lock_counter_barrier (b);

  wait_group_beginning = b->curr_wait_group;

  if (--b->wait_count <= 0)
    pthread_cond_broadcast (&b->cond);
  else
    {
      while (b->wait_count > 0 && b->curr_wait_group == wait_group_beginning)
	pthread_cond_wait (&b->cond, &b->count->m);
    }

  if (b->wait_count <= 0)
    {
      b->curr_wait_group = !wait_group_beginning;
      b->wait_count = b->count->val;
    }

  unlock_counter_barrier (b);
}

/* Adjust the counter of a barrier by val (which can be positive or
   negative), signalling if necessary.  */

static inline void
change_internal_barrier_count (counter_barrier *b, int val)
{
  b->wait_count += val;
  if (b->wait_count <= 0)
    pthread_cond_broadcast (&b->cond);
}

/* Adjust all associated barriers of a counter.  */
int
waitable_counter_add (waitable_counter *c, int val)
{
  counter_barrier *curr;
  int ret;
  pthread_mutex_lock (&c->m);
  ret = (c->val += val);
  for (curr = c->b; curr; curr = curr->next)
    change_internal_barrier_count (curr, val);

  pthread_mutex_unlock (&c->m);
  return ret;
}

/* Get the value of a counter.  */

int
waitable_counter_get_val (waitable_counter *c)
{
  int ret;
  pthread_mutex_lock (&c->m);
  ret = c->val;
  pthread_mutex_unlock (&c->m);
  return ret;
}

/* Initialize waitable counter.  */

void
waitable_counter_init (waitable_counter *c, int val)
{
  initialize_shared_mutex (&c->m);
  c->val = val;
  c->b = NULL;
}

/* Bind a barrier to a counter.  */

void
bind_counter_barrier (counter_barrier *b, waitable_counter *c)
{
  initialize_shared_condition (&b->cond);

  pthread_mutex_lock (&c->m);
  b->next = c->b;
  b->curr_wait_group = 0;
  b->count = c;
  b->wait_count = c->val;
  c->b = b;
  pthread_mutex_unlock (&c->m);
}
