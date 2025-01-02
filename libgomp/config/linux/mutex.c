/* Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and the futex syscall.  */

#include "wait.h"

int gomp_futex_wake = FUTEX_WAKE | FUTEX_PRIVATE_FLAG;
int gomp_futex_wait = FUTEX_WAIT | FUTEX_PRIVATE_FLAG;

void
gomp_mutex_lock_slow (gomp_mutex_t *mutex, int oldval)
{
  /* First loop spins a while.  */
  while (oldval == 1)
    {
      if (do_spin (mutex, 1))
	{
	  /* Spin timeout, nothing changed.  Set waiting flag.  */
	  oldval = __atomic_exchange_n (mutex, -1, MEMMODEL_ACQUIRE);
	  if (oldval == 0)
	    return;
	  futex_wait (mutex, -1);
	  break;
	}
      else
	{
	  /* Something changed.  If now unlocked, we're good to go.  */
	  oldval = 0;
	  if (__atomic_compare_exchange_n (mutex, &oldval, 1, false,
					   MEMMODEL_ACQUIRE, MEMMODEL_RELAXED))
	    return;
	}
    }

  /* Second loop waits until mutex is unlocked.  We always exit this
     loop with wait flag set, so next unlock will awaken a thread.  */
  while ((oldval = __atomic_exchange_n (mutex, -1, MEMMODEL_ACQUIRE)))
    do_wait (mutex, -1);
}

void
gomp_mutex_unlock_slow (gomp_mutex_t *mutex)
{
  futex_wake (mutex, 1);
}
