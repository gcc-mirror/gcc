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

/* This is a Linux specific implementation of a semaphore synchronization
   mechanism for libgomp.  This type is private to the library.  This 
   implementation uses atomic instructions and the futex syscall.  */

#include "wait.h"

void
gomp_sem_wait_slow (gomp_sem_t *sem, int count)
{
  /* First loop spins a while.  */
  while (count == 0)
    if (do_spin (sem, 0)
	/* Spin timeout, nothing changed.  Set waiting flag.  */
	&& __atomic_compare_exchange_n (sem, &count, SEM_WAIT, false,
					MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
      {
	futex_wait (sem, SEM_WAIT);
	count = *sem;
	break;
      }
  /* Something changed.  If it wasn't the wait flag, we're good to go.  */
    else if (__builtin_expect (((count = *sem) & SEM_WAIT) == 0 && count != 0,
			       1))
      {
	if (__atomic_compare_exchange_n (sem, &count, count - SEM_INC, false,
					 MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
	  return;
      }

  /* Second loop waits until semaphore is posted.  We always exit this
     loop with wait flag set, so next post will awaken a thread.  */
  while (1)
    {
      unsigned int wake = count & ~SEM_WAIT;
      int newval = SEM_WAIT;

      if (wake != 0)
	newval |= wake - SEM_INC;
      if (__atomic_compare_exchange_n (sem, &count, newval, false,
				       MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
	{
	  if (wake != 0)
	    {
	      /* If we can wake more threads, do so now.  */
	      if (wake > SEM_INC)
		gomp_sem_post_slow (sem);
	      break;
	    }
	  do_wait (sem, SEM_WAIT);
	  count = *sem;
	}
    }
}

void
gomp_sem_post_slow (gomp_sem_t *sem)
{
  futex_wake (sem, 1);
}
