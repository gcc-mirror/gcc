/* Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of a semaphore synchronization
   mechanism for libgomp.  This type is private to the library.  This 
   counting semaphore implementation uses atomic instructions and the
   futex syscall, and a single 32-bit int to store semaphore state.
   The low 31 bits are the count, the top bit is a flag set when some
   threads may be waiting.  */

#ifndef GOMP_SEM_H
#define GOMP_SEM_H 1

typedef int gomp_sem_t;
#define SEM_WAIT (-__INT_MAX__ - 1)
#define SEM_INC 1

extern void gomp_sem_wait_slow (gomp_sem_t *, int);
extern void gomp_sem_post_slow (gomp_sem_t *);

static inline void
gomp_sem_init (gomp_sem_t *sem, int value)
{
  *sem = value * SEM_INC;
}

static inline void
gomp_sem_destroy (gomp_sem_t *sem)
{
}

static inline void
gomp_sem_wait (gomp_sem_t *sem)
{
  int count = *sem;

  while ((count & ~SEM_WAIT) != 0)
    if (__atomic_compare_exchange_n (sem, &count, count - SEM_INC, true,
				     MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
      return;
  gomp_sem_wait_slow (sem, count);
}

static inline void
gomp_sem_post (gomp_sem_t *sem)
{
  int count = *sem;

  /* Clear SEM_WAIT here so that if there are no more waiting threads
     we transition back to the uncontended state that does not make
     futex syscalls.  If there are waiting threads then when one is
     awoken it will set SEM_WAIT again, so other waiting threads are
     woken on a future gomp_sem_post.  Furthermore, the awoken thread
     will wake other threads in case gomp_sem_post was called again
     before it had time to set SEM_WAIT.  */
  while (!__atomic_compare_exchange_n (sem, &count,
				       (count + SEM_INC) & ~SEM_WAIT, true,
				       MEMMODEL_RELEASE, MEMMODEL_RELAXED))
    continue;

  if (__builtin_expect (count & SEM_WAIT, 0))
    gomp_sem_post_slow (sem);
}

static inline int
gomp_sem_getcount (gomp_sem_t *sem)
{
  int count = __atomic_load_n (sem, MEMMODEL_RELAXED);
  if ((count & SEM_WAIT) != 0)
    return -1;
  return count / SEM_INC;
}
#endif /* GOMP_SEM_H */
