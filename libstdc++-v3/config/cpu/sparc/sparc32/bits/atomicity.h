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

#ifndef _BITS_ATOMICITY_H
#define _BITS_ATOMICITY_H	1

typedef int _Atomic_word;

static int
__attribute__ ((unused))
__exchange_and_add (volatile _Atomic_word* __mem, int __val)
{
  static unsigned char __lock;
  _Atomic_word __result, __tmp;

  __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
		       "	cmp	%0, 0\n\t"
		       "	bne	1b\n\t"
		       "	 nop"
		       : "=&r" (__tmp)
		       : "r" (&__lock)
		       : "memory");
  __result = *__mem;
  *__mem += __val;
  __asm__ __volatile__("stb	%%g0, [%0]"
		       : /* no outputs */
		       : "r" (&__lock)
		       : "memory");
  return __result;
}

static void
__attribute__ ((unused))
__atomic_add (volatile _Atomic_word* __mem, int __val)
{
  static unsigned char __lock;
  _Atomic_word __tmp;

  __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
		       "	cmp	%0, 0\n\t"
		       "	bne	1b\n\t"
		       "	 nop"
		       : "=&r" (__tmp)
		       : "r" (&__lock)
		       : "memory");
  *__mem += __val;
  __asm__ __volatile__("stb	%%g0, [%0]"
		       : /* no outputs */
		       : "r" (&__lock)
		       : "memory");
}

static int
__attribute__ ((unused))
__compare_and_swap (volatile long *__p, long __oldval, long __newval)
{
  static unsigned char __lock;
  long __ret, __tmp;

  __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
		       "	cmp	%0, 0\n\t"
		       "	bne	1b\n\t"
		       "	 nop"
		       : "=&r" (__tmp)
		       : "r" (&__lock)
		       : "memory");
  if (*__p != __oldval)
    __ret = 0;
  else
    {
      *__p = __newval;
      __ret = 1;
    }
  __asm__ __volatile__("stb	%%g0, [%0]"
		       : /* no outputs */
		       : "r" (&__lock)
		       : "memory");

  return __ret;
}

#endif /* atomicity.h */
