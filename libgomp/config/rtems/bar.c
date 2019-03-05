/* Copyright (C) 2005-2019 Free Software Foundation, Inc.
   Contributed by Sebastian Huber <sebastian.huber@embedded-brains.de>.

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

/* This is the RTEMS implementation of a barrier synchronization
   mechanism for libgomp.  It is identical to the Linux implementation, except
   that the futex API is slightly different.  This type is private to the
   library.  */

#include "libgomp.h"
#include "bar.h"
#include <limits.h>

static gomp_barrier_t *
generation_to_barrier (int *addr)
{
  return (gomp_barrier_t *)
	 ((char *) addr - __builtin_offsetof (gomp_barrier_t, generation));
}

static void
futex_wait (int *addr, int val)
{
  gomp_barrier_t *bar = generation_to_barrier (addr);
  _Futex_Wait (&bar->futex, addr, val);
}

static void
futex_wake (int *addr, int count)
{
  gomp_barrier_t *bar = generation_to_barrier (addr);
  _Futex_Wake (&bar->futex, count);
}

static int
do_spin (int *addr, int val)
{
  unsigned long long i, count = gomp_spin_count_var;

  if (__builtin_expect (gomp_managed_threads > gomp_available_cpus, 0))
    count = gomp_throttled_spin_count_var;
  for (i = 0; i < count; i++)
    if (__builtin_expect (__atomic_load_n (addr, MEMMODEL_RELAXED) != val, 0))
      return 0;
  return 1;
}

static void
do_wait (int *addr, int val)
{
  if (do_spin (addr, val))
    futex_wait (addr, val);
}

#define GOMP_WAIT_H 1
#include "../linux/bar.c"
