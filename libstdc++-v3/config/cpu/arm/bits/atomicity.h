/* Low-level functions for atomic operations.  ARM version.
   Copyright (C) 2000 Free Software Foundation, Inc.
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
#define _BITS_ATOMICITY_H    1

typedef int _Atomic_word;

static inline _Atomic_word
__attribute__ ((__unused__))
__exchange_and_add (volatile _Atomic_word* __mem, int __val)
{
  _Atomic_word __tmp, __tmp2, __result;
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %0,[%3] \n\t"
	"add     %1,%0,%4 \n\t"
	"swp     %2,%1,[%3] \n\t"
	"cmp     %0,%2 \n\t"
	"swpne   %1,%2,[%3] \n\t"
	"bne     0b \n\t"
	""
	: "=&r"(__result), "=&r"(__tmp), "=&r"(__tmp2) 
	: "r" (__mem), "r"(__val) 
	: "cc", "memory");
  return __result;
}

static inline void
__attribute__ ((__unused__))
__atomic_add (volatile _Atomic_word *__mem, int __val)
{
  _Atomic_word __tmp, __tmp2, __tmp3;
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %0,[%3] \n\t"
	"add     %1,%0,%4 \n\t"
	"swp     %2,%1,[%3] \n\t"
	"cmp     %0,%2 \n\t"
	"swpne   %1,%2,[%3] \n\t"
	"bne     0b \n\t"
	""
	: "=&r"(__tmp), "=&r"(__tmp2), "=&r"(__tmp3) 
	: "r" (__mem), "r"(__val) 
	: "cc", "memory");
}

static inline int
__attribute__ ((__unused__))
__compare_and_swap (volatile long *__p, long __oldval, long __newval)
{
  int __result;
  long __tmp;
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %1,[%2] \n\t"
	"mov     %0,#0 \n\t"
	"cmp     %1,%4 \n\t"
	"bne     1f \n\t"
	"swp     %0,%3,[%2] \n\t"
	"cmp     %1,%0 \n\t"
	"swpne   %1,%0,[%2] \n\t"
	"bne     0b \n\t"
	"mov     %0,#1 \n"
	"1:\n\t"
	""
	: "=&r"(__result), "=&r"(__tmp) 
	: "r" (__p), "r" (__newval), "r" (__oldval) 
	: "cc", "memory");
  return __result;
}

static inline long
__attribute__ ((__unused__))
__always_swap (volatile long *__p, long __newval)
{
  long __result;
  __asm__ __volatile__ (
	"\n\t"
	"swp     %0,%2,[%1] \n\t"
	""
	: "=&r"(__result)
	: "r"(__p), "r"(__newval)
	: "memory");
  return __result;
}

static inline int
__attribute__ ((__unused__))
__test_and_set (volatile long *__p, long __newval)
{
  int __result;
  long __tmp;
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %0,[%2] \n\t"
	"cmp     %0,#0 \n\t"
	"bne     1f \n\t"
	"swp     %1,%3,[%2] \n\t"
	"cmp     %0,%1 \n\t"
	"swpne   %0,%1,[%2] \n\t"
	"bne     0b \n"
	"1:\n\t"
	""
	: "=&r"(__result), "=r" (__tmp) 
	: "r"(__p), "r"(__newval) 
	: "cc", "memory");
  return __result;
}

#endif /* atomicity.h */
