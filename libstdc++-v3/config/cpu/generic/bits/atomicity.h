/* Low-level functions for atomic operations.  Stub version.
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
__exchange_and_add (_Atomic_word* __mem, int __val)
{
  _Atomic_word __result = *__mem;
  *__mem += __val;
  return __result;
}

static inline void
__attribute__ ((unused))
__atomic_add (_Atomic_word* __mem, int __val)
{
  *__mem += __val;
}

static inline int
__attribute__ ((unused))
__compare_and_swap (long *__p, long __oldval, long __newval)
{
  if (*__p != __oldval)
    return 0;

  *__p = __newval;
  return 1;
}

#endif /* atomicity.h */
