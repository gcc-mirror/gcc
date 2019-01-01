/* Copyright (C) 2015-2019 Free Software Foundation, Inc.
   Contributed by Sebastian Huber <sebastian.huber@embedded-brains.de>.

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

/* This is the RTEMS implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  */

#ifndef GOMP_MUTEX_H
#define GOMP_MUTEX_H 1

#include <sys/lock.h>

typedef struct _Mutex_Control gomp_mutex_t;

#define GOMP_MUTEX_INIT_0 1

static inline void gomp_mutex_init (gomp_mutex_t *mutex)
{
  _Mutex_Initialize (mutex);
}

static inline void gomp_mutex_lock (gomp_mutex_t *mutex)
{
  _Mutex_Acquire (mutex);
}

static inline void gomp_mutex_unlock (gomp_mutex_t *mutex)
{
  _Mutex_Release (mutex);
}

static inline void gomp_mutex_destroy (gomp_mutex_t *mutex)
{
  _Mutex_Destroy (mutex);
}

#endif /* GOMP_MUTEX_H */
