/* Copyright (C) 2015-2021 Free Software Foundation, Inc.
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
   implementation uses atomic instructions and busy waiting.

   A ptrlock has four states:
   0/NULL Initial
   1      Owned by me, I get to write a pointer to ptrlock.
   2      Some thread is waiting on the ptrlock.
   >2     Ptrlock contains a valid pointer.
   It is not valid to gain the ptrlock and then write a NULL to it.  */

#ifndef GOMP_PTRLOCK_H
#define GOMP_PTRLOCK_H 1

typedef void *gomp_ptrlock_t;

static inline void gomp_ptrlock_init (gomp_ptrlock_t *ptrlock, void *ptr)
{
  *ptrlock = ptr;
}

static inline void *gomp_ptrlock_get (gomp_ptrlock_t *ptrlock)
{
  uintptr_t v = (uintptr_t) __atomic_load_n (ptrlock, MEMMODEL_ACQUIRE);
  if (v > 2)
    return (void *) v;

  if (v == 0
      && __atomic_compare_exchange_n (ptrlock, &v, 1, false,
				      MEMMODEL_ACQUIRE, MEMMODEL_ACQUIRE))
    return NULL;

  while (v == 1)
    v = (uintptr_t) __atomic_load_n (ptrlock, MEMMODEL_ACQUIRE);

  return (void *) v;
}

static inline void gomp_ptrlock_set (gomp_ptrlock_t *ptrlock, void *ptr)
{
  __atomic_store_n (ptrlock, ptr, MEMMODEL_RELEASE);
}

static inline void gomp_ptrlock_destroy (gomp_ptrlock_t *ptrlock)
{
}

#endif /* GOMP_PTRLOCK_H */
