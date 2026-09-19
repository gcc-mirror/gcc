/* Copyright (C) 2025-2026 Free Software Foundation, Inc.
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
  caf_shmem_mutex_lock (&b->mutex);
}

/* Unlock the associated counter of this barrier.  */

static inline void
unlock_counter_barrier (counter_barrier *b)
{
  caf_shmem_mutex_unlock (&b->mutex);
}

void
counter_barrier_init (counter_barrier *b, int val)
{
  *b = (counter_barrier) {.mutex = CAF_SHMEM_MUTEX_INITIALIZER,
			  .cond = CAF_SHMEM_COND_INITIALIZER,
			  .wait_count = val,
			  .curr_wait_group = 1,
			  .aborted_round = 0,
			  .abortable_arrivals = 0,
			  .aborting = false,
			  .count = val};
  initialize_shared_condition (&b->cond, val);
  initialize_shared_mutex (&b->mutex);
}

/* Start the next round of the barrier and wake the images waiting in the
   current one.  */

static void
next_round (counter_barrier *b, bool abort)
{
  if (abort)
    b->aborted_round = b->curr_wait_group;
  ++b->curr_wait_group;
  b->wait_count = b->count;
  b->abortable_arrivals = 0;
  caf_shmem_cond_broadcast (&b->cond);
}

/* Take part in the current round of the barrier, with its lock held.  Returns
   false, when the round was aborted.  */

static bool
wait_round (counter_barrier *b, bool abortable)
{
  const uint64_t round = b->curr_wait_group;

  if (abortable)
    ++b->abortable_arrivals;
  --b->wait_count;
  while (b->wait_count > 0 && b->curr_wait_group == round)
    caf_shmem_cond_wait (&b->cond, &b->mutex);

  /* The last image to arrive, or to be woken after the count dropped, ends
     the round.  */
  if (b->curr_wait_group == round)
    next_round (b, false);

  return b->aborted_round != round;
}

void
counter_barrier_wait (counter_barrier *b)
{
  lock_counter_barrier (b);
  while (!wait_round (b, false))
    ;
  unlock_counter_barrier (b);
}

bool
counter_barrier_wait_abortable (counter_barrier *b)
{
  bool completed;

  lock_counter_barrier (b);
  completed = !b->aborting && wait_round (b, true);
  unlock_counter_barrier (b);
  return completed;
}

void
counter_barrier_abort_locked (counter_barrier *b)
{
  b->aborting = true;
  if (b->abortable_arrivals)
    next_round (b, true);
}

static inline void
change_internal_barrier_count (counter_barrier *b, int val)
{
  b->wait_count += val;
  if (b->wait_count <= 0)
    caf_shmem_cond_broadcast (&b->cond);
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
  caf_shmem_mutex_lock (&c->mutex);
  ret = counter_barrier_add_locked (c, val);

  caf_shmem_mutex_unlock (&c->mutex);
  return ret;
}

void
counter_barrier_init_add (counter_barrier *b, int val)
{
  b->count += val;
  b->wait_count += val;
  caf_shmem_cond_update_count (&b->cond, val);
}

int
counter_barrier_get_count (counter_barrier *c)
{
  int ret;
  caf_shmem_mutex_lock (&c->mutex);
  ret = c->count;
  caf_shmem_mutex_unlock (&c->mutex);
  return ret;
}
