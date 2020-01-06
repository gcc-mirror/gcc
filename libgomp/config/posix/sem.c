/* Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

/* This is the default POSIX 1003.1b implementation of a semaphore
   synchronization mechanism for libgomp.  This type is private to
   the library.

   This is a bit heavy weight for what we need, in that we're not
   interested in sem_wait as a cancelation point, but it's not too
   bad for a default.  */

#include "libgomp.h"

#ifdef HAVE_BROKEN_POSIX_SEMAPHORES
#include <stdlib.h>

void gomp_sem_init (gomp_sem_t *sem, int value)
{
  int ret;

  ret = pthread_mutex_init (&sem->mutex, NULL);
  if (ret)
    return;

  ret = pthread_cond_init (&sem->cond, NULL);
  if (ret)
    return;

  sem->value = value;
}

void gomp_sem_wait (gomp_sem_t *sem)
{
  int ret;

  ret = pthread_mutex_lock (&sem->mutex);
  if (ret)
    return;

  if (sem->value > 0)
    {
      sem->value--;
      ret = pthread_mutex_unlock (&sem->mutex);
      return;
    }

  while (sem->value <= 0)
    {
      ret = pthread_cond_wait (&sem->cond, &sem->mutex);
      if (ret)
	{
	  pthread_mutex_unlock (&sem->mutex);
	  return;
	}
    }

  sem->value--;
  ret = pthread_mutex_unlock (&sem->mutex);
  return;
}

void gomp_sem_post (gomp_sem_t *sem)
{
  int ret;

  ret = pthread_mutex_lock (&sem->mutex);
  if (ret)
    return;

  sem->value++;

  ret = pthread_mutex_unlock (&sem->mutex);
  if (ret)
    return;

  ret = pthread_cond_signal (&sem->cond);

  return;
}

void gomp_sem_destroy (gomp_sem_t *sem)
{
  int ret;

  ret = pthread_mutex_destroy (&sem->mutex);
  if (ret)
    return;

  ret = pthread_cond_destroy (&sem->cond);

  return;
}
#else /* HAVE_BROKEN_POSIX_SEMAPHORES  */
void
gomp_sem_wait (gomp_sem_t *sem)
{
  /* With POSIX, the wait can be canceled by signals.  We don't want that.
     It is expected that the return value here is -1 and errno is EINTR.  */
  while (sem_wait (sem) != 0)
    continue;
}
#endif
