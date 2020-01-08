/* Copyright (C) 2015-2020 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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

/* This is a generic implementation of doacross spinning.  */

#ifndef GOMP_DOACROSS_H
#define GOMP_DOACROSS_H 1

#include "libgomp.h"
#include <errno.h>

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility push(hidden)
#endif

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

static inline void doacross_spin (unsigned long *addr, unsigned long expected,
				  unsigned long cur)
{
  /* FIXME: back off depending on how large expected - cur is.  */
  do
    {
      cpu_relax ();
      cur = __atomic_load_n (addr, MEMMODEL_RELAXED);
      if (expected < cur)
	return;
    }
  while (1);
}

#ifdef HAVE_ATTRIBUTE_VISIBILITY
# pragma GCC visibility pop
#endif

#endif /* GOMP_DOACROSS_H */
