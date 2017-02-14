/* Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

/* This is an NVPTX specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and busy waiting.  */

#ifndef GOMP_MUTEX_H
#define GOMP_MUTEX_H 1

typedef int gomp_mutex_t;

#define GOMP_MUTEX_INIT_0 1

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
  while (__sync_lock_test_and_set (mutex, 1))
    /* spin */ ;
}

static inline void
gomp_mutex_unlock (gomp_mutex_t *mutex)
{
  __sync_lock_release (mutex);
}
#endif /* GOMP_MUTEX_H */
