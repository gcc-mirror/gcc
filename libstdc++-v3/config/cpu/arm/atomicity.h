// Low-level functions for atomic operations: ARM version  -*- C++ -*-

// Copyright (C) 2000, 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _BITS_ATOMICITY_H
#define _BITS_ATOMICITY_H    1

typedef int _Atomic_word;

static inline _Atomic_word
__attribute__ ((__unused__))
__exchange_and_add (volatile _Atomic_word* __mem, int __val)
{
  _Atomic_word __tmp, __tmp2, __result;
#ifdef __thumb__
  /* Since this function is inlined, we can't be sure of the alignment.  */
  __asm__ __volatile__ (
	"ldr     %0, 4f \n\t"
	"bx      %0 \n\t"
	".align 0 \n"
	"4:\t"
	".word   0f \n\t"
	".code 32 \n"
	"0:\t"
	"ldr     %0, [%3] \n\t"
	"add     %1, %0, %4 \n\t"
	"swp     %2, %1, [%3] \n\t"
        "cmp     %0, %2 \n\t"
        "swpne   %1, %2, [%3] \n\t"
        "bne     0b \n\t"
	"ldr     %1, 1f \n\t"
	"bx      %1 \n"
	"1:\t"
	".word   2f \n\t"
	".code 16 \n"
	"2:\n"
	: "=&l"(__result), "=&r"(__tmp), "=&r"(__tmp2) 
	: "r" (__mem), "r"(__val) 
	: "cc", "memory");
#else
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %0, [%3] \n\t"
	"add     %1, %0, %4 \n\t"
	"swp     %2, %1, [%3] \n\t"
	"cmp     %0, %2 \n\t"
	"swpne   %1, %2, [%3] \n\t"
	"bne     0b \n\t"
	""
	: "=&r"(__result), "=&r"(__tmp), "=&r"(__tmp2) 
	: "r" (__mem), "r"(__val) 
	: "cc", "memory");
#endif
  return __result;
}

static inline void
__attribute__ ((__unused__))
__atomic_add (volatile _Atomic_word *__mem, int __val)
{
  _Atomic_word __tmp, __tmp2, __tmp3;
#ifdef __thumb__
  /* Since this function is inlined, we can't be sure of the alignment.  */
  __asm__ __volatile__ (
	"ldr     %0, 4f \n\t"
	"bx      %0 \n\t"
	".align 0\n"
	"4:\t"
	".word   0f \n\t"
	".code 32 \n"
	"0:\t"
	"ldr     %0, [%3] \n\t"
	"add     %1, %0, %4 \n\t"
        "swp     %2, %1, [%3] \n\t"
        "cmp     %0, %2 \n\t"
        "swpne   %1, %2,[%3] \n\t"
        "bne     0b \n\t"
	"ldr     %1, 1f \n\t"
	"bx      %1 \n"
	"1:\t"
	".word   2f \n\t"
	".code 16 \n"
	"2:\n"
	: "=&l"(__tmp), "=&r"(__tmp2), "=&r"(__tmp3) 
	: "r" (__mem), "r"(__val) 
	: "cc", "memory");
#else
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %0, [%3] \n\t"
	"add     %1, %0, %4 \n\t"
	"swp     %2, %1, [%3] \n\t"
	"cmp     %0, %2 \n\t"
	"swpne   %1, %2, [%3] \n\t"
	"bne     0b \n\t"
	""
	: "=&r"(__tmp), "=&r"(__tmp2), "=&r"(__tmp3) 
	: "r" (__mem), "r"(__val) 
	: "cc", "memory");
#endif
}

static inline long
__attribute__ ((__unused__))
__always_swap (volatile long *__p, long __newval)
{
  long __result;
#ifdef __thumb__
  long __tmp;
  /* Since this function is inlined, we can't be sure of the alignment.  */
  __asm__ __volatile__ (
	"ldr     %0, 4f \n\t"
	"bx      %0 \n\t"
	".align 0 \n"
	"4:\t"
	".word   0f \n\t"
	".code 32\n"
	"0:\t"
	"swp     %0, %3, [%2] \n\t"
	"ldr     %1, 1f \n\t"
	"bx      %1 \n"
	"1:\t"
	".word   2f \n\t"
	".code 16 \n"
	"2:\n"
	: "=&l"(__result), "=&r"(__tmp)
	: "r"(__p), "r"(__newval)
	: "memory");
#else
  __asm__ __volatile__ (
	"\n\t"
	"swp     %0, %2, [%1] \n\t"
	""
	: "=&r"(__result)
	: "r"(__p), "r"(__newval)
	: "memory");
#endif
  return __result;
}

static inline int
__attribute__ ((__unused__))
__test_and_set (volatile long *__p, long __newval)
{
  int __result;
  long __tmp;
#ifdef __thumb__
  /* Since this function is inlined, we can't be sure of the alignment.  */
  __asm__ __volatile__ (
	"ldr     %0, 4f \n\t"
	"bx      %0 \n\t"
	".align 0 \n"
	"4:\t"
	".word   0f \n\t"
	".code 32 \n"
	"0:\t"
	"ldr     %0, [%2] \n\t"
        "cmp     %0, #0 \n\t"
        "bne     1f \n\t"
        "swp     %1, %3, [%2] \n\t"
        "cmp     %0, %1 \n\t"
        "swpne   %0, %1, [%2]\n\t"
        "bne     0b \n"
	"1:\t"
	"ldr     %1, 2f \n\t"
	"bx      %1 \n"
	"2:\t"
	".word   3f \n\t"
	".code 16 \n"
	"3:"
	: "=&l"(__result), "=r" (__tmp) 
	: "r"(__p), "r"(__newval) 
	: "cc", "memory");
#else
  __asm__ __volatile__ (
	"\n"
	"0:\t"
	"ldr     %0, [%2] \n\t"
	"cmp     %0, #0 \n\t"
	"bne     1f \n\t"
	"swp     %1, %3, [%2] \n\t"
	"cmp     %0, %1 \n\t"
	"swpne   %0, %1, [%2] \n\t"
	"bne     0b \n"
	"1:\n\t"
	""
	: "=&r"(__result), "=r" (__tmp) 
	: "r"(__p), "r"(__newval) 
	: "cc", "memory");
#endif
  return __result;
}

#endif /* atomicity.h */
