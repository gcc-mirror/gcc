/* Copyright (C) 2015-2022 Free Software Foundation, Inc.
   Contributed by Alexander Monakov <amonakov@ispras.ru>

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

/* This is an NVPTX specific implementation of a barrier synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and bar.sync instruction.  */

#include <limits.h>
#include "libgomp.h"

/* For cpu_relax.  */
#include "doacross.h"

/* Assuming ADDR is &bar->generation, return bar.  Copied from
   rtems/bar.c.  */

static gomp_barrier_t *
generation_to_barrier (int *addr)
{
  char *bar
    = (char *) addr - __builtin_offsetof (gomp_barrier_t, generation);
  return (gomp_barrier_t *)bar;
}

/* Implement futex_wait-like behaviour to plug into the linux/bar.c
   implementation.  Assumes ADDR is &bar->generation.   */

static inline void
futex_wait (int *addr, int val)
{
  gomp_barrier_t *bar = generation_to_barrier (addr);

  if (bar->total < 2)
    /* A barrier with less than two threads, nop.  */
    return;

  gomp_mutex_lock (&bar->lock);

  /* Futex semantics: only go to sleep if *addr == val.  */
  if (__builtin_expect (__atomic_load_n (addr, MEMMODEL_ACQUIRE) != val, 0))
    {
      gomp_mutex_unlock (&bar->lock);
      return;
    }

  /* Register as waiter.  */
  unsigned int waiters
    = __atomic_add_fetch (&bar->waiters, 1, MEMMODEL_ACQ_REL);
  if (waiters == 0)
    __builtin_abort ();
  unsigned int waiter_id = waiters;

  if (waiters > 1)
    {
      /* Wake other threads in bar.sync.  */
      asm volatile ("bar.sync 1, %0;" : : "r" (32 * waiters));

      /* Ensure that they have updated waiters.  */
      asm volatile ("bar.sync 1, %0;" : : "r" (32 * waiters));
    }

  gomp_mutex_unlock (&bar->lock);

  while (1)
    {
      /* Wait for next thread in barrier.  */
      asm volatile ("bar.sync 1, %0;" : : "r" (32 * (waiters + 1)));

      /* Get updated waiters.  */
      unsigned int updated_waiters
	= __atomic_load_n (&bar->waiters, MEMMODEL_ACQUIRE);

      /* Notify that we have updated waiters.  */
      asm volatile ("bar.sync 1, %0;" : : "r" (32 * (waiters + 1)));

      waiters = updated_waiters;

      if (waiter_id > waiters)
	/* A wake happened, and we're in the group of woken threads.  */
	break;

      /* Continue waiting.  */
    }
}

/* Implement futex_wake-like behaviour to plug into the linux/bar.c
   implementation.  Assumes ADDR is &bar->generation.  */

static inline void
futex_wake (int *addr, int count)
{
  gomp_barrier_t *bar = generation_to_barrier (addr);

  if (bar->total < 2)
    /* A barrier with less than two threads, nop.  */
    return;

  gomp_mutex_lock (&bar->lock);
  unsigned int waiters = __atomic_load_n (&bar->waiters, MEMMODEL_ACQUIRE);
  if (waiters == 0)
    {
      /* No threads to wake.  */
      gomp_mutex_unlock (&bar->lock);
      return;
    }

  if (count == INT_MAX)
    /* Release all threads.  */
    __atomic_store_n (&bar->waiters, 0, MEMMODEL_RELEASE);
  else if (count < bar->total)
    /* Release count threads.  */
    __atomic_add_fetch (&bar->waiters, -count, MEMMODEL_ACQ_REL);
  else
    /* Count has an illegal value.  */
    __builtin_abort ();

  /* Wake other threads in bar.sync.  */
  asm volatile ("bar.sync 1, %0;" : : "r" (32 * (waiters + 1)));

  /* Let them get the updated waiters.  */
  asm volatile ("bar.sync 1, %0;" : : "r" (32 * (waiters + 1)));

  gomp_mutex_unlock (&bar->lock);
}

/* Copied from linux/wait.h.  */

static inline int do_spin (int *addr, int val)
{
  /* The current implementation doesn't spin.  */
  return 1;
}

/* Copied from linux/wait.h.  */

static inline void do_wait (int *addr, int val)
{
  if (do_spin (addr, val))
    futex_wait (addr, val);
}

/* Reuse the linux implementation.  */
#define GOMP_WAIT_H 1
#include "../linux/bar.c"
