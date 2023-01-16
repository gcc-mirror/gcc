/* Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
   synchronizaton.  Only a subset of full barrier API (bar.h) is exposed.  */

#ifndef GOMP_SIMPLE_BARRIER_H
#define GOMP_SIMPLE_BARRIER_H 1

#include "bar.h"

typedef struct
{
  gomp_barrier_t bar;
} gomp_simple_barrier_t;

static inline void
gomp_simple_barrier_init (gomp_simple_barrier_t *bar, unsigned count)
{
  gomp_barrier_init (&bar->bar, count);
}

static inline void
gomp_simple_barrier_reinit (gomp_simple_barrier_t *bar, unsigned count)
{
  gomp_barrier_reinit (&bar->bar, count);
}

static inline void
gomp_simple_barrier_destroy (gomp_simple_barrier_t *bar)
{
  gomp_barrier_destroy (&bar->bar);
}

static inline void
gomp_simple_barrier_wait (gomp_simple_barrier_t *bar)
{
  gomp_barrier_wait (&bar->bar);
}

static inline void
gomp_simple_barrier_wait_last (gomp_simple_barrier_t *bar)
{
  gomp_barrier_wait_last (&bar->bar);
}

#endif /* GOMP_SIMPLE_BARRIER_H */
