/* Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and the futex syscall.  */

#ifndef GOMP_MUTEX_H
#define GOMP_MUTEX_H 1

typedef int gomp_mutex_t;

#define GOMP_MUTEX_INIT_0 1

extern void gomp_mutex_lock_slow (gomp_mutex_t *mutex, int);
extern void gomp_mutex_unlock_slow (gomp_mutex_t *mutex);

static inline void
gomp_mutex_init (gomp_mutex_t *mutex)
{
  *mutex = 0;
}

static inline void
gomp_mutex_destroy (gomp_mutex_t *mutex)
{
}

static inline void
gomp_mutex_lock (gomp_mutex_t *mutex)
{
  int oldval = 0;
  if (!__atomic_compare_exchange_n (mutex, &oldval, 1, false,
				    MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
    gomp_mutex_lock_slow (mutex, oldval);
}

static inline void
gomp_mutex_unlock (gomp_mutex_t *mutex)
{
  int wait = __atomic_exchange_n (mutex, 0, MEMMODEL_RELEASE);
  if (__builtin_expect (wait < 0, 0))
    gomp_mutex_unlock_slow (mutex);
}
#endif /* GOMP_MUTEX_H */
