/* Low-level functions for atomic operations.  ix86 version, x >= 4.
   Copyright (C) 1997 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef _ATOMICITY_H
#define _ATOMICITY_H	1

#ifdef _GLIBCPP_HAVE_INTTYPES_H
#include <inttypes.h>
#else
typedef unsigned int	uint32_t;
typedef int 		int32_t;
#endif


static inline int
__attribute__ ((unused))
exchange_and_add (volatile uint32_t *mem, int val)
{
  register int result;
  __asm__ __volatile__ ("lock; xaddl %0,%2"
			: "=r" (result) : "0" (val), "m" (*mem) : "memory");
  return result;
}

static inline void
__attribute__ ((unused))
atomic_add (volatile uint32_t *mem, int val)
{
  __asm__ __volatile__ ("lock; addl %0,%1"
			: : "ir" (val), "m" (*mem) : "memory");
}

static inline char
__attribute__ ((unused))
compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  char ret;
  long int readval;

  __asm__ __volatile__ ("lock; cmpxchgl %3, %1; sete %0"
                        : "=q" (ret), "=m" (*p), "=a" (readval)
                        : "r" (newval), "m" (*p), "a" (oldval));
  return ret;
}

#endif /* atomicity.h */
