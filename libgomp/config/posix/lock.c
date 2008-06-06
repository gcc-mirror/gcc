/* Copyright (C) 2005, 2008 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This is the default PTHREADS implementation of the public OpenMP
   locking primitives.

   Because OpenMP uses different entry points for normal and recursive
   locks, and pthreads uses only one entry point, a system may be able
   to do better and streamline the locking as well as reduce the size
   of the types exported.  */

/* We need Unix98 extensions to get recursive locks.  On Tru64 UNIX V4.0F,
   the declarations are available without _XOPEN_SOURCE, which actually
   breaks compilation.  */
#ifndef __osf__
#define _XOPEN_SOURCE 500
#endif

#include "libgomp.h"

#ifdef HAVE_BROKEN_POSIX_SEMAPHORES
void
gomp_init_lock_30 (omp_lock_t *lock)
{
  pthread_mutex_init (lock, NULL);
}

void
gomp_destroy_lock_30 (omp_lock_t *lock)
{
  pthread_mutex_destroy (lock);
}

void
gomp_set_lock_30 (omp_lock_t *lock)
{
  pthread_mutex_lock (lock);
}

void
gomp_unset_lock_30 (omp_lock_t *lock)
{
  pthread_mutex_unlock (lock);
}

int
gomp_test_lock_30 (omp_lock_t *lock)
{
  return pthread_mutex_trylock (lock) == 0;
}

void
gomp_init_nest_lock_30 (omp_nest_lock_t *lock)
{
  pthread_mutex_init (&lock->lock, NULL);
  lock->count = 0;
  lock->owner = NULL;
}

void
gomp_destroy_nest_lock_30 (omp_nest_lock_t *lock)
{
  pthread_mutex_destroy (&lock->lock);
}

void
gomp_set_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);

  if (lock->owner != me)
    {
      pthread_mutex_lock (&lock->lock);
      lock->owner = me;
    }
  lock->count++;
}

void
gomp_unset_nest_lock_30 (omp_nest_lock_t *lock)
{
  if (--lock->count == 0)
    {
      lock->owner = NULL;
      pthread_mutex_unlock (&lock->lock);
    }
}

int
gomp_test_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);

  if (lock->owner != me)
    {
      if (pthread_mutex_trylock (&lock->lock) != 0)
	return 0;
      lock->owner = me;
    }

  return ++lock->count;
}

#else

void
gomp_init_lock_30 (omp_lock_t *lock)
{
  sem_init (lock, 0, 1);
}

void
gomp_destroy_lock_30 (omp_lock_t *lock)
{
  sem_destroy (lock);
}

void
gomp_set_lock_30 (omp_lock_t *lock)
{
  while (sem_wait (lock) != 0)
    ;
}

void
gomp_unset_lock_30 (omp_lock_t *lock)
{
  sem_post (lock);
}

int
gomp_test_lock_30 (omp_lock_t *lock)
{
  return sem_trywait (lock) == 0;
}

void
gomp_init_nest_lock_30 (omp_nest_lock_t *lock)
{
  sem_init (&lock->lock, 0, 1);
  lock->count = 0;
  lock->owner = NULL;
}

void
gomp_destroy_nest_lock_30 (omp_nest_lock_t *lock)
{
  sem_destroy (&lock->lock);
}

void
gomp_set_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);

  if (lock->owner != me)
    {
      while (sem_wait (&lock->lock) != 0)
	;
      lock->owner = me;
    }
  lock->count++;
}

void
gomp_unset_nest_lock_30 (omp_nest_lock_t *lock)
{
  if (--lock->count == 0)
    {
      lock->owner = NULL;
      sem_post (&lock->lock);
    }
}

int
gomp_test_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);

  if (lock->owner != me)
    {
      if (sem_trywait (&lock->lock) != 0)
	return 0;
      lock->owner = me;
    }

  return ++lock->count;
}
#endif

#ifdef LIBGOMP_GNU_SYMBOL_VERSIONING
void
gomp_init_lock_25 (omp_lock_25_t *lock)
{
  pthread_mutex_init (lock, NULL);
}

void
gomp_destroy_lock_25 (omp_lock_25_t *lock)
{
  pthread_mutex_destroy (lock);
}

void
gomp_set_lock_25 (omp_lock_25_t *lock)
{
  pthread_mutex_lock (lock);
}

void
gomp_unset_lock_25 (omp_lock_25_t *lock)
{
  pthread_mutex_unlock (lock);
}

int
gomp_test_lock_25 (omp_lock_25_t *lock)
{
  return pthread_mutex_trylock (lock) == 0;
}

void
gomp_init_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  pthread_mutexattr_t attr;

  pthread_mutexattr_init (&attr);
  pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init (&lock->lock, &attr);
  lock->count = 0;
  pthread_mutexattr_destroy (&attr);
}

void
gomp_destroy_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  pthread_mutex_destroy (&lock->lock);
}

void
gomp_set_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  pthread_mutex_lock (&lock->lock);
  lock->count++;
}

void
gomp_unset_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  lock->count--;
  pthread_mutex_unlock (&lock->lock);
}

int
gomp_test_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  if (pthread_mutex_trylock (&lock->lock) == 0)
    return ++lock->count;
  return 0;
}

omp_lock_symver (omp_init_lock)
omp_lock_symver (omp_destroy_lock)
omp_lock_symver (omp_set_lock)
omp_lock_symver (omp_unset_lock)
omp_lock_symver (omp_test_lock)
omp_lock_symver (omp_init_nest_lock)
omp_lock_symver (omp_destroy_nest_lock)
omp_lock_symver (omp_set_nest_lock)
omp_lock_symver (omp_unset_nest_lock)
omp_lock_symver (omp_test_nest_lock)

#else

ialias (omp_init_lock)
ialias (omp_init_nest_lock)
ialias (omp_destroy_lock)
ialias (omp_destroy_nest_lock)
ialias (omp_set_lock)
ialias (omp_set_nest_lock)
ialias (omp_unset_lock)
ialias (omp_unset_nest_lock)
ialias (omp_test_lock)
ialias (omp_test_nest_lock)

#endif
