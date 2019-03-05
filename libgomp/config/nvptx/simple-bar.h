/* Copyright (C) 2015-2019 Free Software Foundation, Inc.
   Contributed by Alexander Monakov <amonakov@ispras.ru>

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

/* This is a simplified barrier that is suitable for thread pool
   synchronizaton.  Only a subset of full barrier API (bar.h) is exposed.
   Here in the NVPTX-specific implementation, we expect that thread pool
   corresponds to a PTX CTA (thread block).  */

#ifndef GOMP_SIMPLE_BARRIER_H
#define GOMP_SIMPLE_BARRIER_H 1

typedef struct
{
  unsigned count;
} gomp_simple_barrier_t;

static inline void
gomp_simple_barrier_init (gomp_simple_barrier_t *bar, unsigned count)
{
  bar->count = count * 32;
}

/* Unused on NVPTX.
static inline void
gomp_simple_barrier_reinit (gomp_simple_barrier_t *bar, unsigned count)
{
  bar->count = count * 32;
}
*/

static inline void
gomp_simple_barrier_destroy (gomp_simple_barrier_t *bar)
{
}

static inline void
gomp_simple_barrier_wait (gomp_simple_barrier_t *bar)
{
  asm volatile ("bar.sync 0, %0;" : : "r" (bar->count) : "memory");
}

static inline void
gomp_simple_barrier_wait_last (gomp_simple_barrier_t *bar)
{
  asm volatile ("bar.arrive 0, %0;" : : "r" (bar->count) : "memory");
}

#endif /* GOMP_SIMPLE_BARRIER_H */
