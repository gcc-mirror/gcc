/* Copyright (C) 2015-2019 Free Software Foundation, Inc.
   Contributed by Mentor Embedded.

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

/* This is the AMD GCN implementation of doacross spinning.  */

#ifndef GOMP_DOACROSS_H
#define GOMP_DOACROSS_H 1

#include "libgomp.h"

static inline int
cpu_relax (void)
{
  /* This can be implemented as just a memory barrier, but a sleep seems
     like it should allow the wavefront to yield (maybe?)
     Use the shortest possible sleep time of 1*64 cycles.  */
  asm volatile ("s_sleep\t1" ::: "memory");
  return 0;
}

static inline void doacross_spin (unsigned long *addr, unsigned long expected,
				  unsigned long cur)
{
  /* Prevent compiler from optimizing based on bounds of containing object.  */
  asm ("" : "+r" (addr));
  do
    {
       /* An alternative implementation might use s_setprio to lower the
	  priority temporarily, and then restore it after.  */
      int i = cpu_relax ();
      cur = addr[i];
    }
  while (cur <= expected);
}

#endif /* GOMP_DOACROSS_H */
