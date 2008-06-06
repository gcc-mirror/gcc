/* Copyright (C) 2008 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License 
   along with libgomp; see the file COPYING.LIB.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files, some
   of which are compiled with GCC, to produce an executable, this library
   does not by itself cause the resulting executable to be covered by the
   GNU General Public License.  This exception does not however invalidate
   any other reasons why the executable file might be covered by the GNU
   General Public License.  */

/* This is a Linux specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and the futex syscall.  */

#ifndef GOMP_PTRLOCK_H
#define GOMP_PTRLOCK_H 1

typedef struct { void *ptr; gomp_mutex_t lock; } gomp_ptrlock_t;

static inline void gomp_ptrlock_init (gomp_ptrlock_t *ptrlock, void *ptr)
{
  ptrlock->ptr = ptr;
  gomp_mutex_init (&ptrlock->lock);
}

static inline void *gomp_ptrlock_get (gomp_ptrlock_t *ptrlock)
{
  if (ptrlock->ptr != NULL)
    return ptrlock->ptr;

  gomp_mutex_lock (&ptrlock->lock);
  if (ptrlock->ptr != NULL)
    {
      gomp_mutex_unlock (&ptrlock->lock);
      return ptrlock->ptr;
    }

  return NULL;
}

static inline void gomp_ptrlock_set (gomp_ptrlock_t *ptrlock, void *ptr)
{
  ptrlock->ptr = ptr;
  gomp_mutex_unlock (&ptrlock->lock);
}

static inline void gomp_ptrlock_destroy (gomp_ptrlock_t *ptrlock)
{
  gomp_mutex_destroy (&ptrlock->lock);
}

#endif /* GOMP_PTRLOCK_H */
