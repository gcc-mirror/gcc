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

/* This is an NVPTX specific implementation of a semaphore synchronization
   mechanism for libgomp.  This type is private to the library.  This
   semaphore implementation uses atomic instructions and busy waiting.  */

#ifndef GOMP_SEM_H
#define GOMP_SEM_H 1

typedef int gomp_sem_t;

static inline void
gomp_sem_init (gomp_sem_t *sem, int value)
{
  *sem = value;
}

static inline void
gomp_sem_destroy (gomp_sem_t *sem)
{
}

static inline void
gomp_sem_wait (gomp_sem_t *sem)
{
  int count = __atomic_load_n (sem, MEMMODEL_ACQUIRE);
  for (;;)
    {
      while (count == 0)
	count = __atomic_load_n (sem, MEMMODEL_ACQUIRE);
      if (__atomic_compare_exchange_n (sem, &count, count - 1, false,
				       MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
	return;
    }
}

static inline void
gomp_sem_post (gomp_sem_t *sem)
{
  (void) __atomic_add_fetch (sem, 1, MEMMODEL_RELEASE);
}
#endif /* GOMP_SEM_H */
