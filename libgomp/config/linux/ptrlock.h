/* Copyright (C) 2008, 2009 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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

#ifndef GOMP_PTRLOCK_H
#define GOMP_PTRLOCK_H 1

typedef void *gomp_ptrlock_t;

static inline void gomp_ptrlock_init (gomp_ptrlock_t *ptrlock, void *ptr)
{
  *ptrlock = ptr;
}

extern void *gomp_ptrlock_get_slow (gomp_ptrlock_t *ptrlock);
static inline void *gomp_ptrlock_get (gomp_ptrlock_t *ptrlock)
{
  if ((uintptr_t) *ptrlock > 2)
    return *ptrlock;

  if (__sync_bool_compare_and_swap (ptrlock, NULL, (uintptr_t) 1))
    return NULL;

  return gomp_ptrlock_get_slow (ptrlock);
}

extern void gomp_ptrlock_set_slow (gomp_ptrlock_t *ptrlock, void *ptr);
static inline void gomp_ptrlock_set (gomp_ptrlock_t *ptrlock, void *ptr)
{
  if (!__sync_bool_compare_and_swap (ptrlock, (uintptr_t) 1, ptr))
    gomp_ptrlock_set_slow (ptrlock, ptr);
}

static inline void gomp_ptrlock_destroy (gomp_ptrlock_t *ptrlock)
{
}

#endif /* GOMP_PTRLOCK_H */
