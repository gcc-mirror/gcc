/* Copyright (C) 2005, 2008, 2009, 2011 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of the public OpenMP locking
   primitives.  This implementation uses atomic instructions and the futex
   syscall.  */

#include <string.h>
#include <unistd.h>
#include <sys/syscall.h>
#include "wait.h"


/* The internal gomp_mutex_t and the external non-recursive omp_lock_t
   have the same form.  Re-use it.  */

void
gomp_init_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_init (lock);
}

void
gomp_destroy_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_destroy (lock);
}

void
gomp_set_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_lock (lock);
}

void
gomp_unset_lock_30 (omp_lock_t *lock)
{
  gomp_mutex_unlock (lock);
}

int
gomp_test_lock_30 (omp_lock_t *lock)
{
  int oldval = 0;

  return __atomic_compare_exchange_n (lock, &oldval, 1, false,
				      MEMMODEL_ACQUIRE, MEMMODEL_RELAXED);
}

void
gomp_init_nest_lock_30 (omp_nest_lock_t *lock)
{
  memset (lock, '\0', sizeof (*lock));
}

void
gomp_destroy_nest_lock_30 (omp_nest_lock_t *lock)
{
}

void
gomp_set_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);

  if (lock->owner != me)
    {
      gomp_mutex_lock (&lock->lock);
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
      gomp_mutex_unlock (&lock->lock);
    }
}

int
gomp_test_nest_lock_30 (omp_nest_lock_t *lock)
{
  void *me = gomp_icv (true);
  int oldval;

  if (lock->owner == me)
    return ++lock->count;

  oldval = 0;
  if (__atomic_compare_exchange_n (&lock->lock, &oldval, 1, false,
				   MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
    {
      lock->owner = me;
      lock->count = 1;
      return 1;
    }

  return 0;
}

#ifdef LIBGOMP_GNU_SYMBOL_VERSIONING
/* gomp_mutex_* can be safely locked in one thread and
   unlocked in another thread, so the OpenMP 2.5 and OpenMP 3.0
   non-nested locks can be the same.  */
strong_alias (gomp_init_lock_30, gomp_init_lock_25)
strong_alias (gomp_destroy_lock_30, gomp_destroy_lock_25)
strong_alias (gomp_set_lock_30, gomp_set_lock_25)
strong_alias (gomp_unset_lock_30, gomp_unset_lock_25)
strong_alias (gomp_test_lock_30, gomp_test_lock_25)

/* The external recursive omp_nest_lock_25_t form requires additional work.  */

/* We need an integer to uniquely identify this thread.  Most generally
   this is the thread's TID, which ideally we'd get this straight from
   the TLS block where glibc keeps it.  Unfortunately, we can't get at
   that directly.

   If we don't support (or have disabled) TLS, one function call is as
   good (or bad) as any other.  Use the syscall all the time.

   On an ILP32 system (defined here as not LP64), we can make do with
   any thread-local pointer.  Ideally we'd use the TLS base address,
   since that requires the least amount of arithmetic, but that's not
   always available directly.  Make do with the gomp_thread pointer
   since it's handy.  */

# if !defined (HAVE_TLS)
static inline int gomp_tid (void)
{
  return syscall (SYS_gettid);
}
# elif !defined(__LP64__)
static inline int gomp_tid (void)
{
  return (int) gomp_thread ();
}
# else
static __thread int tid_cache;
static inline int gomp_tid (void)
{
  int tid = tid_cache;
  if (__builtin_expect (tid == 0, 0))
    tid_cache = tid = syscall (SYS_gettid);
  return tid;
}
# endif


void
gomp_init_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  memset (lock, 0, sizeof (lock));
}

void
gomp_destroy_nest_lock_25 (omp_nest_lock_25_t *lock)
{
}

void
gomp_set_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  int otid, tid = gomp_tid ();

  while (1)
    {
      otid = 0;
      if (__atomic_compare_exchange_n (&lock->owner, &otid, tid, false,
				       MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
	{
	  lock->count = 1;
	  return;
	}
      if (otid == tid)
	{
	  lock->count++;
	  return;
	}

      do_wait (&lock->owner, otid);
    }
}

void
gomp_unset_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  /* ??? Validate that we own the lock here.  */

  if (--lock->count == 0)
    {
      __atomic_store_n (&lock->owner, 0, MEMMODEL_RELEASE);
      futex_wake (&lock->owner, 1);
    }
}

int
gomp_test_nest_lock_25 (omp_nest_lock_25_t *lock)
{
  int otid, tid = gomp_tid ();

  otid = 0;
  if (__atomic_compare_exchange_n (&lock->owner, &otid, tid, false,
				   MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
    {
      lock->count = 1;
      return 1;
    }
  if (otid == tid)
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
