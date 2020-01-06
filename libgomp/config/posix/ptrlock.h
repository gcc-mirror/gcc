/* Copyright (C) 2008-2020 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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

/* This is a generic POSIX implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  */

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
