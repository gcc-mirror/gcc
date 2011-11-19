/* go-semacquire.c -- implement runtime.Semacquire and runtime.Semrelease.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include <pthread.h>

#include "go-assert.h"
#include "runtime.h"

/* We use a single global lock and condition variable.  This is
   painful, since it will cause unnecessary contention, but is hard to
   avoid in a portable manner.  On GNU/Linux we can use futexes, but
   they are unfortunately not exposed by libc and are thus also hard
   to use portably.  */

static pthread_mutex_t sem_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t sem_cond = PTHREAD_COND_INITIALIZER;

/* If the value in *ADDR is positive, and we are able to atomically
   decrement it, return true.  Otherwise do nothing and return
   false.  */

static _Bool
acquire (uint32 *addr)
{
  while (1)
    {
      uint32 val;

      val = *addr;
      if (val == 0)
	return 0;
      if (__sync_bool_compare_and_swap (addr, val, val - 1))
	return 1;
    }
}

/* Implement runtime.Semacquire.  ADDR points to a semaphore count.
   We have acquired the semaphore when we have decremented the count
   and it remains nonnegative.  */

void
runtime_semacquire (uint32 *addr)
{
  while (1)
    {
      int i;

      /* If the current count is positive, and we are able to atomically
	 decrement it, then we have acquired the semaphore.  */
      if (acquire (addr))
	return;

      /* Lock the mutex.  */
      i = pthread_mutex_lock (&sem_lock);
      __go_assert (i == 0);

      /* Check the count again with the mutex locked.  */
      if (acquire (addr))
	{
	  i = pthread_mutex_unlock (&sem_lock);
	  __go_assert (i == 0);
	  return;
	}

      /* The count is zero.  Even if a call to runtime.Semrelease
	 increments it to become positive, that call will try to
	 acquire the mutex and block, so we are sure to see the signal
	 of the condition variable.  */
      i = pthread_cond_wait (&sem_cond, &sem_lock);
      __go_assert (i == 0);

      /* Unlock the mutex and try again.  */
      i = pthread_mutex_unlock (&sem_lock);
      __go_assert (i == 0);
    }
}

/* Implement runtime.Semrelease.  ADDR points to a semaphore count.  We
   must atomically increment the count.  If the count becomes
   positive, we signal the condition variable to wake up another
   process.  */

void
runtime_semrelease (uint32 *addr)
{
  int32_t val;

  val = __sync_fetch_and_add (addr, 1);

  /* VAL is the old value.  It should never be negative.  If it is
     negative, that implies that Semacquire somehow decremented a zero
     value, or that the count has overflowed.  */
  __go_assert (val >= 0);

  /* If the old value was zero, then we have now released a count, and
     we signal the condition variable.  If the old value was positive,
     then nobody can be waiting.  We have to use
     pthread_cond_broadcast, not pthread_cond_signal, because
     otherwise there would be a race condition when the count is
     incremented twice before any locker manages to decrement it.  */
  if (val == 0)
    {
      int i;

      i = pthread_mutex_lock (&sem_lock);
      __go_assert (i == 0);

      i = pthread_cond_broadcast (&sem_cond);
      __go_assert (i == 0);

      i = pthread_mutex_unlock (&sem_lock);
      __go_assert (i == 0);
    }
}


#ifndef HAVE_SYNC_FETCH_AND_ADD_4

/* For targets which don't have the required sync support.  Really
   this should be provided by gcc itself.  FIXME.  */

static pthread_mutex_t sync_lock = PTHREAD_MUTEX_INITIALIZER;

uint32
__sync_fetch_and_add_4(uint32*, uint32)
  __attribute__((visibility("hidden")));

uint32
__sync_fetch_and_add_4(uint32* ptr, uint32 add)
{
  int i;
  uint32 ret;

  i = pthread_mutex_lock(&sync_lock);
  __go_assert(i == 0);

  ret = *ptr;
  *ptr += add;

  i = pthread_mutex_unlock(&sync_lock);
  __go_assert(i == 0);

  return ret;
}

#endif
