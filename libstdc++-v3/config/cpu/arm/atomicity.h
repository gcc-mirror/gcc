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

#ifndef _ATOMICITY_H
#define _ATOMICITY_H    1

#ifdef _GLIBCPP_HAVE_INTTYPES_H
#include <inttypes.h>
#else
typedef unsigned int    uint32_t;
typedef int             int32_t;
#endif

static inline int
__attribute__ ((unused))
exchange_and_add (volatile uint32_t *mem, int val)
{
  int tmp, tmp2, result;
  __asm__ ("\
0:      ldr     %0,[%3]
        add     %1,%0,%4
        swp     %2,%1,[%3]
        cmp     %0,%2
        swpne   %1,%2,[%3]
        bne     0b
" : "=&r"(result), "=&r"(tmp), "=&r"(tmp2) : "r" (mem), "r"(val) : "cc", "memory");
  return result;
}

static inline void
__attribute__ ((unused))
atomic_add (volatile uint32_t *mem, int val)
{
  int tmp, tmp2, tmp3;
  __asm__ ("\
0:      ldr     %0,[%3]
        add     %1,%0,%4
        swp     %2,%1,[%3]
        cmp     %0,%2
        swpne   %1,%2,[%3]
        bne     0b
" : "=&r"(tmp), "=&r"(tmp2), "=&r"(tmp3) : "r" (mem), "r"(val) : "cc", "memory");
}

static inline int
__attribute__ ((unused))
compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  int result, tmp;
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
" : "=&r"(result), "=&r"(tmp) : "r" (p), "r" (newval), "r" (oldval) : "cc", "memory");
}

static inline long int
__attribute__ ((unused))
always_swap (volatile long int *p, long int newval)
{
  long int result;
  __asm__ ("\
        swp     %0,%2,[%1]
" : "=&r"(result) : "r"(p), "r"(newval) : "memory");
  return result;
}

static inline int
__attribute__ ((unused))
test_and_set (volatile long int *p, long int newval)
{
  int result, tmp, tmp2, tmp3;
  __asm__ ("\
0:      ldr     %0,[%2]
        cmp     %0,#0
        bne     1f
        swp     %1,%3,[%2]
        cmp     %0,%1
        swpne   %0,%1,[%2]
        bne     0b
1:
" : "=&r"(result), "=r" (tmp) : "r"(p), "r"(newval) : "cc", "memory");
  return result;
}

#endif /* atomicity.h */
