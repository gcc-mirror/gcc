/* Low-level functions for atomic operations.  IRIX version.
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
#define _BITS_ATOMICITY_H	1

#include <mutex.h>

typedef unsigned long _Atomic_word;

static inline _Atomic_word
__attribute__ ((__unused__))
__exchange_and_add (_Atomic_word* __mem, int __val)
{
  return test_then_add (__mem, __val);
}


static inline void
__attribute__ ((unused))
__atomic_add (_Atomic_word* __mem, int __val)
{
  test_then_add (__mem, __val);
}

#endif /* atomicity.h */
