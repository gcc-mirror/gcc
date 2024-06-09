/* Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

/* This is the NVPTX implementation of doacross spinning.  */

#ifndef GOMP_DOACROSS_H
#define GOMP_DOACROSS_H 1

#include "libgomp.h"

static int zero;

static inline int
cpu_relax (void)
{
  int r;
  /* Here we need a long-latency operation to make the current warp yield.
     We could use ld.cv, uncached load from system (host) memory, but that
     would require allocating locked memory in the plugin.  Alternatively,
     we can use ld.cg, which evicts from L1 and caches in L2.  */
  asm volatile ("ld.cg.s32 %0, [%1];" : "=r" (r) : "i" (&zero) : "memory");
  return r;
}

static inline void doacross_spin (unsigned long *addr, unsigned long expected,
				  unsigned long cur)
{
  /* Prevent compiler from optimizing based on bounds of containing object.  */
  asm ("" : "+r" (addr));
  do
    {
      int i = cpu_relax ();
      cur = addr[i];
    }
  while (cur <= expected);
}

#endif /* GOMP_DOACROSS_H */
