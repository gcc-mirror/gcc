/* Low-level functions for atomic operations.  Sparc64 version.
   Copyright (C) 1999 Free Software Foundation, Inc.
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
  uint32_t tmp1, tmp2;

  __asm__ __volatile__("1:	lduw	[%2], %0\n\t"
		       "	add	%0, %3, %1\n\t"
		       "	cas	[%2], %0, %1\n\t"
		       "	sub	%0, %1, %0\n\t"
		       "	brnz,pn	%0, 1b\n\t"
		       "	 nop"
		       : "=&r" (tmp1), "=&r" (tmp2)
		       : "r" (mem), "r" (val)
		       : "memory");
  return tmp2;
}

static inline void
__attribute__ ((unused))
atomic_add (volatile uint32_t *mem, int val)
{
  uint32_t tmp1, tmp2;

  __asm__ __volatile__("1:	lduw	[%2], %0\n\t"
		       "	add	%0, %3, %1\n\t"
		       "	cas	[%2], %0, %1\n\t"
		       "	sub	%0, %1, %0\n\t"
		       "	brnz,pn	%0, 1b\n\t"
		       "	 nop"
		       : "=&r" (tmp1), "=&r" (tmp2)
		       : "r" (mem), "r" (val)
		       : "memory");
}

static inline int
__attribute__ ((unused))
compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  register long int tmp, tmp2;

  __asm__ __volatile__("1:	ldx	[%4], %0\n\t"
		       "	mov	%2, %1\n\t"
		       "	cmp	%0, %3\n\t"
		       "	bne,a,pn %%xcc, 2f\n\t"
		       "	 mov	0, %0\n\t"
		       "	casx	[%4], %0, %1\n\t"
		       "	sub	%0, %1, %0\n\t"
		       "	brnz,pn	%0, 1b\n\t"
		       "	 mov	1, %0\n\t"
		       "2:"
		       : "=&r" (tmp), "=&r" (tmp2)
		       : "r" (newval), "r" (oldval), "r" (p)
		       : "memory");
  return tmp;
}

#endif /* atomicity.h */
