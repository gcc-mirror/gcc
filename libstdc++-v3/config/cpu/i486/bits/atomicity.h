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

#ifndef _BITS_ATOMICITY_H
#define _BITS_ATOMICITY_H	1

typedef int _Atomic_word;

static inline _Atomic_word 
__attribute__ ((unused))
__exchange_and_add (volatile _Atomic_word *__mem, int __val)
{
  register _Atomic_word __result;
  __asm__ __volatile__ ("lock; xaddl %0,%2"
			: "=r" (__result) 
                        : "0" (__val), "m" (*__mem) 
                        : "memory");
  return __result;
}

static inline void
__attribute__ ((unused))
__atomic_add (volatile _Atomic_word* __mem, int __val)
{
  __asm__ __volatile__ ("lock; addl %0,%1"
			: : "ir" (__val), "m" (*__mem) : "memory");
}

static inline char
__attribute__ ((unused))
__compare_and_swap (volatile long* __p, long __oldval, long __newval)
{
  char __ret;
  long __readval;

  __asm__ __volatile__ ("lock; cmpxchgl %3, %1; sete %0"
                        : "=q" (__ret), "=m" (*__p), "=a" (__readval)
                        : "r" (__newval), "m" (*__p), "a" (__oldval));
  return __ret;
}

#endif /* atomicity.h */
