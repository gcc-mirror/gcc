/* Low-level functions for atomic operations.  AIX version.
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
#define _BITS_ATOMICITY_H       1

/* Should this be type long so 64-bit word in 64-bit mode?  */
typedef int _Atomic_word;

#include <sys/atomic_op.h>

static inline int
__attribute__ ((unused))
__exchange_and_add (atomic_p __mem, int __val)
{
  return fetch_and_add (__mem, __val);
}

static inline void
__attribute__ ((unused))
__atomic_add (atomic_p __mem, int __val)
{
  (void) fetch_and_add (__mem, __val);
}

static inline int
__attribute__ ((unused))
__compare_and_swap (atomic_l __p, long int __oldval, long int __newval)
{
  return compare_and_swaplp (__p, &__oldval, __newval);
}

static inline long
__attribute__ ((unused))
__always_swap (atomic_l __p, long int __newval)
{
  long __val = *__p;

  while (! compare_and_swaplp (__p, &__val, __newval))
    /* EMPTY */;

  return __val;
}

static inline int
__attribute__ ((unused))
__test_and_set (atomic_l __p, long int __newval)
{
  long __val = 0;

  (void) compare_and_swaplp (__p, &__val, __newval);

  return __val;
}

#endif /* atomicity.h */
