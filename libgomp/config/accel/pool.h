/* Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

/* This is the NVPTX implementation of the thread pool management
   for libgomp.  This type is private to the library.  */

#ifndef GOMP_POOL_H
#define GOMP_POOL_H 1

#include "libgomp.h"

/* Get the thread pool.  */

static inline struct gomp_thread_pool *
gomp_get_thread_pool (struct gomp_thread *thr, unsigned nthreads)
{
  /* NVPTX is running with a fixed pool of pre-started threads.  */
  return thr->thread_pool;
}

static inline void
gomp_release_thread_pool (struct gomp_thread_pool *pool)
{
  /* Do nothing.  */
}

#endif /* GOMP_POOL_H */
