/* Low-level functions for atomic operations.  PowerPC version.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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


#if BROKEN_PPC_ASM_CR0
# define __ATOMICITY_INLINE /* nothing */
#else
# define __ATOMICITY_INLINE inline
#endif

static __ATOMICITY_INLINE int
__attribute__ ((unused))
exchange_and_add (volatile uint32_t *mem, int val)
{
  int tmp, result;
  __asm__ ("\
0:	lwarx	%0,0,%2
	add%I3	%1,%0,%3
	stwcx.	%1,0,%2
	bne-	0b
" : "=&b"(result), "=&r"(tmp) : "r" (mem), "Ir"(val) : "cr0", "memory");
  return result;
}

static __ATOMICITY_INLINE void
__attribute__ ((unused))
atomic_add (volatile uint32_t *mem, int val)
{
  int tmp;
  __asm__ ("\
0:	lwarx	%0,0,%1
	add%I2	%0,%0,%2
	stwcx.	%0,0,%1
	bne-	0b
" : "=&b"(tmp) : "r" (mem), "Ir"(val) : "cr0", "memory");
}

static __ATOMICITY_INLINE int
__attribute__ ((unused))
compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  int result;
  __asm__ ("\
0:	lwarx	%0,0,%1
	sub%I2c.	%0,%0,%2
	cntlzw	%0,%0
	bne-	1f
	stwcx.	%3,0,%1
	bne-	0b
1:
" : "=&b"(result) : "r"(p), "Ir"(oldval), "r"(newval) : "cr0", "memory");
  return result >> 5;
}

static __ATOMICITY_INLINE long int
__attribute__ ((unused))
always_swap (volatile long int *p, long int newval)
{
  long int result;
  __asm__ ("\
0:	lwarx	%0,0,%1
	stwcx.	%2,0,%1
	bne-	0b
" : "=&r"(result) : "r"(p), "r"(newval) : "cr0", "memory");
  return result;
}

static __ATOMICITY_INLINE int
__attribute__ ((unused))
test_and_set (volatile long int *p, long int newval)
{
  int result;
  __asm__ ("\
0:	lwarx	%0,0,%1
	cmpwi	%0,0
	bne-	1f
	stwcx.	%2,0,%1
	bne-	0b
1:
" : "=&r"(result) : "r"(p), "r"(newval) : "cr0", "memory");
  return result;
}

#endif /* atomicity.h */
