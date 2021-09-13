/* Copyright (C) 2020-2021 Free Software Foundation, Inc.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
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

/* Functions libat_lock_n/libat_unlock_n based on GOMP_atomic_start/end in
   libgomp/atomic.c.  */

#include "libatomic_i.h"

static int atomic_lock;

void
libat_lock_n (void *ptr __attribute__((unused)),
	      size_t n __attribute__((unused)))
{
  while (__sync_lock_test_and_set (&atomic_lock, 1))
    ;  /* Spin.  */
}

void
libat_unlock_n (void *ptr __attribute__((unused)),
		size_t n __attribute__((unused)))
{
  __sync_lock_release (&atomic_lock);
}

void
libat_lock_1 (void *ptr)
{
  libat_lock_n (ptr, 1);
}

void
libat_unlock_1 (void *ptr)
{
  libat_unlock_n (ptr, 1);
}
