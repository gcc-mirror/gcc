/* Copyright (C) 2015-2025 Free Software Foundation, Inc.
   Contributed by Sebastian Huber <sebastian.huber@embedded-brains.de>.

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

/* This is the RTEMS implementation of the thread pool management
   for libgomp.  This type is private to the library.  */

#ifndef GOMP_POOL_H
#define GOMP_POOL_H 1

#include "libgomp.h"
#include <sys/lock.h>
#include <string.h>

/* For each scheduler instance there may be a thread pool reservoir
   to limit the number of thread pools used by the OpenMP master threads of this
   scheduler instance.  The reservoirs are configured via the
   GOMP_RTEMS_THREAD_POOLS environment variable.  */
struct gomp_thread_pool_reservoir {
  gomp_sem_t available;
  pthread_spinlock_t lock;
  size_t index;
  int priority;
  struct gomp_thread_pool *pools[];
};

struct gomp_tls_rtems_data {
  struct gomp_thread_pool_reservoir *thread_pool_reservoir;
};

extern struct gomp_thread_pool_reservoir **gomp_thread_pool_reservoirs;

extern __thread struct gomp_tls_rtems_data gomp_tls_rtems_data;

static inline struct gomp_thread_pool_reservoir *
gomp_get_thread_pool_reservoir (void)
{
  struct gomp_thread_pool_reservoir *res =
    gomp_tls_rtems_data.thread_pool_reservoir;

  if (res == NULL && gomp_thread_pool_reservoirs != NULL)
    {
      struct gomp_thread *thr = gomp_thread ();
      thr->thread_pool = gomp_malloc_cleared (sizeof (*thr->thread_pool));
      res = gomp_thread_pool_reservoirs[_Sched_Index ()];
      gomp_tls_rtems_data.thread_pool_reservoir = res;
    }

  return res;
}

static inline struct gomp_thread_pool *
gomp_get_own_thread_pool (struct gomp_thread *thr, unsigned nthreads)
{
  struct gomp_thread_pool *pool = thr->thread_pool;
  if (__builtin_expect (pool == NULL, 0))
    {
      pool = gomp_malloc_cleared (sizeof (*pool));
      pool->threads_busy = nthreads;
      thr->thread_pool = pool;
    }
  return pool;
}

static inline struct gomp_thread_pool *
gomp_get_thread_pool (struct gomp_thread *thr, unsigned nthreads)
{
  struct gomp_thread_pool *pool;
  struct gomp_thread_pool_reservoir *res;

  if (__builtin_expect (thr->thread_pool == NULL, 0))
    pthread_setspecific (gomp_thread_destructor, thr);

  res = gomp_get_thread_pool_reservoir ();
  if (res != NULL)
    {
      gomp_sem_wait (&res->available);
      pthread_spin_lock (&res->lock);
      pool = res->pools[--res->index];
      pthread_spin_unlock (&res->lock);
      pool->threads_busy = nthreads;
      thr->thread_pool = pool;
    }
  else
    pool = gomp_get_own_thread_pool (thr, nthreads);

  return pool;
}

static inline void
gomp_release_thread_pool (struct gomp_thread_pool *pool)
{
  struct gomp_thread_pool_reservoir *res =
    gomp_tls_rtems_data.thread_pool_reservoir;
  if (res != NULL)
    {
      pthread_spin_lock (&res->lock);
      res->pools[res->index++] = pool;
      pthread_spin_unlock (&res->lock);
      gomp_sem_post (&res->available);
    }
}

static inline pthread_attr_t *
gomp_adjust_thread_attr (pthread_attr_t *attr, pthread_attr_t *mutable_attr)
{
  struct gomp_thread_pool_reservoir *res = gomp_get_thread_pool_reservoir ();
  if (res != NULL && res->priority > 0)
    {
      struct sched_param param;
      int err;
      if (attr != mutable_attr)
	{
	  attr = mutable_attr;
	  pthread_attr_init (attr);
	}
      memset (&param, 0, sizeof (param));
      param.sched_priority = res->priority;
      err = pthread_attr_setschedparam (attr, &param);
      if (err != 0)
	gomp_fatal ("Thread attribute set scheduler parameters failed: %s", strerror (err));
      err = pthread_attr_setschedpolicy (attr, SCHED_FIFO);
      if (err != 0)
	gomp_fatal ("Thread attribute set scheduler policy failed: %s", strerror (err));
      err = pthread_attr_setinheritsched (attr, PTHREAD_EXPLICIT_SCHED);
      if (err != 0)
	gomp_fatal ("Thread attribute set explicit scheduler failed: %s", strerror (err));
    }
  return attr;
}

#endif /* GOMP_POOL_H */
