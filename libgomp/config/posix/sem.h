/* Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

/* This is the default POSIX 1003.1b implementation of a semaphore
   synchronization mechanism for libgomp.  This type is private to
   the library.

   This is a bit heavy weight for what we need, in that we're not
   interested in sem_wait as a cancelation point, but it's not too
   bad for a default.  */

#ifndef GOMP_SEM_H
#define GOMP_SEM_H 1

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility push(default)
#endif

#include <semaphore.h>

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility pop
#endif

#ifdef HAVE_BROKEN_POSIX_SEMAPHORES
#include <pthread.h>

struct gomp_sem
{
  pthread_mutex_t	mutex;
  pthread_cond_t	cond;
  int			value;
};

typedef struct gomp_sem gomp_sem_t;

extern void gomp_sem_init (gomp_sem_t *sem, int value);

extern void gomp_sem_wait (gomp_sem_t *sem);

extern void gomp_sem_post (gomp_sem_t *sem);

extern void gomp_sem_destroy (gomp_sem_t *sem);

#else /* HAVE_BROKEN_POSIX_SEMAPHORES  */

typedef sem_t gomp_sem_t;

static inline void gomp_sem_init (gomp_sem_t *sem, int value)
{
  sem_init (sem, 0, value);
}

extern void gomp_sem_wait (gomp_sem_t *sem);

static inline void gomp_sem_post (gomp_sem_t *sem)
{
  sem_post (sem);
}

static inline void gomp_sem_destroy (gomp_sem_t *sem)
{
  sem_destroy (sem);
}
#endif /* doesn't HAVE_BROKEN_POSIX_SEMAPHORES  */
#endif /* GOMP_SEM_H  */
