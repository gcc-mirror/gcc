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

#ifndef _BITS_ATOMICITY_H
#define _BITS_ATOMICITY_H	1

typedef long _Atomic_word;

static inline _Atomic_word
__attribute__ ((unused))
__exchange_and_add (volatile _Atomic_word *__mem, int __val)
{
  _Atomic_word __tmp1, __tmp2;

  __asm__ __volatile__("1:	lduw	[%2], %0\n\t"
		       "	add	%0, %3, %1\n\t"
		       "	cas	[%2], %0, %1\n\t"
		       "	sub	%0, %1, %0\n\t"
		       "	brnz,pn	%0, 1b\n\t"
		       "	 nop"
		       : "=&r" (__tmp1), "=&r" (__tmp2)
		       : "r" (__mem), "r" (__val)
		       : "memory");
  return __tmp2;
}

static inline void
__attribute__ ((unused))
__atomic_add (volatile _Atomic_word* __mem, int __val)
{
  _Atomic_word __tmp1, __tmp2;

  __asm__ __volatile__("1:	lduw	[%2], %0\n\t"
		       "	add	%0, %3, %1\n\t"
		       "	cas	[%2], %0, %1\n\t"
		       "	sub	%0, %1, %0\n\t"
		       "	brnz,pn	%0, 1b\n\t"
		       "	 nop"
		       : "=&r" (__tmp1), "=&r" (__tmp2)
		       : "r" (__mem), "r" (__val)
		       : "memory");
}

static inline int
__attribute__ ((unused))
__compare_and_swap (volatile long *__p, long __oldval, long __newval)
{
  register int __tmp, 
  register long __tmp2;

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
		       : "=&r" (__tmp), "=&r" (__tmp2)
		       : "r" (__newval), "r" (__oldval), "r" (__p)
		       : "memory");
  return __tmp;
}

#endif /* atomicity.h */
