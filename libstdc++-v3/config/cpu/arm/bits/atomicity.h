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
__attribute__ ((unused))
__exchange_and_add (volatile _Atomic_word* __mem, int __val)
{
  _Atomic_word __tmp, __tmp2, __result;
  __asm__ ("\
0:      ldr     %0,[%3]
        add     %1,%0,%4
        swp     %2,%1,[%3]
        cmp     %0,%2
        swpne   %1,%2,[%3]
        bne     0b
" : "=&r"(__result), "=&r"(__tmp), "=&r"(__tmp2) 
  : "r" (__mem), "r"(__val) 
  : "cc", "memory");
  return __result;
}

static inline void
__attribute__ ((unused))
__atomic_add (volatile _Atomic_word *__mem, int __val)
{
  _Atomic_word __tmp, __tmp2, __tmp3;
  __asm__ ("\
0:      ldr     %0,[%3]
        add     %1,%0,%4
        swp     %2,%1,[%3]
        cmp     %0,%2
        swpne   %1,%2,[%3]
        bne     0b
" : "=&r"(__tmp), "=&r"(__tmp2), "=&r"(__tmp3) 
  : "r" (__mem), "r"(__val) 
  : "cc", "memory");
}

static inline int
__attribute__ ((unused))
__compare_and_swap (volatile long *__p, long __oldval, long __newval)
{
  int __result;
  long __tmp;
  __asm__ ("\
0:      ldr     %1,[%2]
        mov     %0,#0
        cmp     %1,%4
        bne     1f
        swp     %0,%3,[%2]
        cmp     %1,%0
        swpne   %1,%0,[%2]
        bne     0b
        mov     %0,#1
1:
" : "=&r"(__result), "=&r"(__tmp) 
  : "r" (__p), "r" (__newval), "r" (__oldval) 
  : "cc", "memory");
  return __result;
}

static inline long
__attribute__ ((unused))
__always_swap (volatile long *__p, long __newval)
{
  long __result;
  __asm__ ("\
        swp     %0,%2,[%1]
" : "=&r"(__result) : "r"(__p), "r"(__newval) : "memory");
  return __result;
}

static inline int
__attribute__ ((unused))
__test_and_set (volatile long *__p, long __newval)
{
  int __result;
  long __tmp;
  __asm__ ("\
0:      ldr     %0,[%2]
        cmp     %0,#0
        bne     1f
        swp     %1,%3,[%2]
        cmp     %0,%1
        swpne   %0,%1,[%2]
        bne     0b
1:
" : "=&r"(__result), "=r" (__tmp) 
  : "r"(__p), "r"(__newval) 
  : "cc", "memory");
  return __result;
}

#endif /* atomicity.h */
