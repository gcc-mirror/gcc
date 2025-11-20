/* Mutex implementation for libgomp plugins.

   Copyright (C) 2025 Free Software Foundation, Inc.

   Contributed by BayLibre

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

/* This is a minimal implementation of the gomp_mutex_t spinlocks, but
   without all the dependencies used by the config/linux/mutex implementation.

   At the time of writing, this is only used by simple_alloc which has
   short-lived locks and should be fine with these.  The actual locks are in
   a header file, so only the fallback "slow" functions are needed here.  */

#include "config.h"
#include <unistd.h>
#include "libgomp.h"

#ifndef HAVE_SYNC_BUILTINS
#error "HAVE_SYNC_BUILTINS is required to build this"
#endif

void
gomp_mutex_lock_slow (gomp_mutex_t *mutex, int oldval)
{
  while (oldval == 1)
    {
      usleep (1);
      oldval = __atomic_exchange_n (mutex, 1, __ATOMIC_ACQUIRE);
    }
}

void
gomp_mutex_unlock_slow (gomp_mutex_t *mutex)
{
  GOMP_PLUGIN_fatal ("gomp_mutex_unlock_slow should be unreachable");
}
