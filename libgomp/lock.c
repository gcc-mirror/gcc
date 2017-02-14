/* Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

/* This is a generic implementation of the public OpenMP locking primitives in
   terms of internal gomp_mutex_t.  It is not meant to be compiled on its own.
   It is #include'd from config/{linux,nvptx}/lock.c.  */

#include <string.h>
#include "libgomp.h"

/* The internal gomp_mutex_t and the external non-recursive omp_lock_t
   have the same form.  Re-use it.  */

void
gomp_init_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_init (lock);
}

void
gomp_destroy_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_destroy (lock);
}

void
gomp_set_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_lock (lock);
}

void
gomp_unset_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_unlock (lock);
}

int
gomp_test_lock_30 (omp_lock_t *lock)
{
  int oldval = 0;

  return __atomic_compare_exchange_n (lock, &oldval, 1, false,
				      MEMMODEL_ACQUIRE, MEMMODEL_RELAXED);
}

void
gomp_init_nest_lock_30 (omp_nest_lock_t *lock)
{
  memset (lock, '\0', sizeof (*lock));
}

void
gomp_destroy_nest_lock_30 (omp_nest_lock_t *lock)
{
}

void
gomp_set_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);

  if (lock->owner != me)
    {
      gomp_mutex_lock (&lock->lock);
      lock->owner = me;
    }

  lock->count++;
}

void
gomp_unset_nest_lock_30 (omp_nest_lock_t *lock)
{
  if (--lock->count == 0)
    {
      lock->owner = NULL;
      gomp_mutex_unlock (&lock->lock);
    }
}

int
gomp_test_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);
  int oldval;

  if (lock->owner == me)
    return ++lock->count;

  oldval = 0;
  if (__atomic_compare_exchange_n (&lock->lock, &oldval, 1, false,
				   MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
    {
      lock->owner = me;
      lock->count = 1;
      return 1;
    }

  return 0;
}
