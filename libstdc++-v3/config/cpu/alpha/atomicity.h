/* Low-level functions for atomic operations.  Alpha version.
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
  register int result, tmp;

  __asm__ __volatile__ (
	"/* Inline exchange & add */\n"
	"1:\t"
	"ldl_l	%0,%3\n\t"
	"addl	%0,%4,%1\n\t"
	"stl_c	%1,%2\n\t"
	"beq	%1,2f\n"
	".subsection 1\n"
	"2:\t"
	"br	1b\n"
	".previous\n\t"
	"mb\n\t"
	"/* End exchange & add */"
	: "=&r"(result), "=&r"(tmp), "=m"(*mem)
	: "m" (*mem), "r"(val));

  return result;
}

static inline void
__attribute__ ((unused))
atomic_add (volatile uint32_t *mem, int val)
{
  register int result;

  __asm__ __volatile__ (
	"/* Inline exchange & add */\n"
	"1:\t"
	"ldl_l	%0,%2\n\t"
	"addl	%0,%3,%0\n\t"
	"stl_c	%0,%1\n\t"
	"beq	%0,2f\n\t"
	".subsection 1\n"
	"2:\t"
	"br	1b\n"
	".previous\n\t"
	"mb\n\t"
	"/* End exchange & add */"
	: "=&r"(result), "=m"(*mem)
	: "m" (*mem), "r"(val));
}

static inline long
__attribute__ ((unused))
compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  long int ret;

  __asm__ __volatile__ (
	"/* Inline compare & swap */\n"
	"1:\t"
	"ldq_l	%0,%4\n\t"
	"cmpeq	%0,%2,%0\n\t"
	"beq	%0,3f\n\t"
	"mov	%3,%0\n\t"
	"stq_c	%0,%1\n\t"
	"beq	%0,2f\n\t"
	".subsection 1\n"
	"2:\t"
	"br	1b\n"
	".previous\n\t"
	"3:\t"
	"mb\n\t"
	"/* End compare & swap */"
	: "=&r"(ret), "=m"(*p)
	: "r"(oldval), "r"(newval), "m"(*p));

  return ret;
}

#endif /* atomicity.h */






