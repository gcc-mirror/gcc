/* Low-level functions for atomic operations.  Sparc32 version.
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


static int
__attribute__ ((unused))
exchange_and_add (volatile uint32_t *mem, int val)
{
  static unsigned char lock;
  int result, tmp;

  __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
		       "	cmp	%0, 0\n\t"
		       "	bne	1b\n\t"
		       "	 nop"
		       : "=&r" (tmp)
		       : "r" (&lock)
		       : "memory");
  result = *mem;
  *mem += val;
  __asm__ __volatile__("stb	%%g0, [%0]"
		       : /* no outputs */
		       : "r" (&lock)
		       : "memory");
  return result;
}

static void
__attribute__ ((unused))
atomic_add (volatile uint32_t *mem, int val)
{
  static unsigned char lock;
  int tmp;

  __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
		       "	cmp	%0, 0\n\t"
		       "	bne	1b\n\t"
		       "	 nop"
		       : "=&r" (tmp)
		       : "r" (&lock)
		       : "memory");
  *mem += val;
  __asm__ __volatile__("stb	%%g0, [%0]"
		       : /* no outputs */
		       : "r" (&lock)
		       : "memory");
}

static int
__attribute__ ((unused))
compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  static unsigned char lock;
  int ret, tmp;

  __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
		       "	cmp	%0, 0\n\t"
		       "	bne	1b\n\t"
		       "	 nop"
		       : "=&r" (tmp)
		       : "r" (&lock)
		       : "memory");
  if (*p != oldval)
    ret = 0;
  else
    {
      *p = newval;
      ret = 1;
    }
  __asm__ __volatile__("stb	%%g0, [%0]"
		       : /* no outputs */
		       : "r" (&lock)
		       : "memory");

  return ret;
}

#endif /* atomicity.h */
