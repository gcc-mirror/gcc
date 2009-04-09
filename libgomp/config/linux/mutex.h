/* Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and the futex syscall.  */

#ifndef GOMP_MUTEX_H
#define GOMP_MUTEX_H 1

typedef int gomp_mutex_t;

#define GOMP_MUTEX_INIT_0 1

static inline void gomp_mutex_init (gomp_mutex_t *mutex)
{
  *mutex = 0;
}

extern void gomp_mutex_lock_slow (gomp_mutex_t *mutex);
static inline void gomp_mutex_lock (gomp_mutex_t *mutex)
{
  if (!__sync_bool_compare_and_swap (mutex, 0, 1))
    gomp_mutex_lock_slow (mutex);
}

extern void gomp_mutex_unlock_slow (gomp_mutex_t *mutex);
static inline void gomp_mutex_unlock (gomp_mutex_t *mutex)
{
  int val = __sync_lock_test_and_set (mutex, 0);
  if (__builtin_expect (val > 1, 0))
    gomp_mutex_unlock_slow (mutex);
}

static inline void gomp_mutex_destroy (gomp_mutex_t *mutex)
{
}

#endif /* GOMP_MUTEX_H */
