/* Copyright (C) 2005, 2008, 2009 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and the futex syscall.  */

#include "wait.h"

long int gomp_futex_wake = FUTEX_WAKE | FUTEX_PRIVATE_FLAG;
long int gomp_futex_wait = FUTEX_WAIT | FUTEX_PRIVATE_FLAG;

void
gomp_mutex_lock_slow (gomp_mutex_t *mutex)
{
  do
    {
      int oldval = __sync_val_compare_and_swap (mutex, 1, 2);
      if (oldval != 0)
	do_wait (mutex, 2);
    }
  while (!__sync_bool_compare_and_swap (mutex, 0, 2));
}

void
gomp_mutex_unlock_slow (gomp_mutex_t *mutex)
{
  futex_wake (mutex, 1);
}
